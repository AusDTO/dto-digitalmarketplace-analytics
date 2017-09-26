
# User input to prompt for the authorisation token.
# if a token is already set, then it's simple returneda
prompt_auth <- function() {
  readline(prompt="Auth auth key: ")
}

auth <- function(header) {
  return(header)
}

datesSinceLaunch <- function() {data.frame(dates = seq(as.Date("2016-08-29"),Sys.Date(),by="day"))}

domains <- c("Software engineering and Development",
             "User research and Design",
             "Agile delivery and Governance",
             "Content and Publishing",
             "Strategy and Policy",
             "Change, Training and Transformation",
             "Cyber security",
             "Support and Operations",
             "Data science",
             "Emerging technologies",
             "Marketing, Communications and Engagement")

# API interface functions

# gets content from an API
fetchFromAPI <- function(getURL,auth) {
  getdata<-GET(url=getURL, add_headers(Authorization=auth))
  raw <- fromJSON(content(getdata,type="text"))
  # Assume return is a list of 2 length. One of these is content, the other has links
  # just want the one with content, so reject the one that contains raw[[]]$self
  if (is.null(raw[[1]]$self)) {
    return(raw[[1]])
  }
  return(raw[[2]])
}

# recursive version of fetch, to retrieve all 'next' URLs as a list
fetchAllFromAPI <- function(getURL,auth,x) {
  getdata<-GET(url=getURL, add_headers(Authorization=auth))
  raw <- fromJSON(content(getdata,type="text"))
  i <- 1
  j <- 2
  if (!is.null(raw[[1]]$self)) {
    i <- 2
    j <- 1
  }
  x[[length(x)+1]] <- raw[[i]]
  if (!is.null(raw[[j]]$`next`)) {
    nextURL <- raw[[j]]$`next`
    print(nextURL)
    x <- fetchAllFromAPI(nextURL,auth,x)
  }
  x
}

# extracts an email out of a list
email <- function(x) {
  x$email
}

# extracts an name out of a list
name <- function(x) {
  x$name
}

# writes a data frame to a CSV file with label YYYYMMDD-suffix.csv
writey <- function(x,suffix,includeHeader=TRUE, quote=TRUE, sep=",") {
  write.table(x,file=paste("data\\",substr(as.POSIXct(Sys.time()),1,10),"-",suffix,".csv",sep=""),
              sep=sep,quote=quote,row.names=FALSE,col.names=includeHeader)
}

format_mil <- function(x,digits=2) {
  return (paste("$",round(x/1000000,digits=digits),"M",sep=""))
}

# writes a dataframe to a Google Sheet
# ws: handle to the Google sheet
# df: dataframe to write
# This will overwrite the contents of 'LATEST' worksheet, and write the same contents to
# a new worksheet with the current date (does this first)
write_gs <- function(gs, df, latest_only = FALSE) {
    # write new worksheet with the current date 
    if (!latest_only) {
      gs <- gs %>% 
            gs_ws_new(ws_title = substr(as.POSIXct(Sys.time()),1,10), input = updatedBriefs,
              trim = TRUE, verbose = FALSE)
    }
    # update the 'LATEST' worksheet
    gs %>% gs_edit_cells(ws = "LATEST", input = df, anchor = "A1", byrow = TRUE)
}

write_seller_emails <- function(sellers, seller_users) {
  emails <- unique(c(sellers$contact_email,seller_users$email_address))
  writey(emails,"seller_contact_emails")
}

# Symbol 	Meaning 	Example
# %d 	day as a number (0-31) 	01-31
# %a  abbreviated weekday 	Mon
# %A 	unabbreviated weekday Monday
# %m 	month (00-12) 	00-12
# %b  abbreviated month 	Jan
# %B 	unabbreviated month January
# %y  2-digit year 07
# %Y 	4-digit year 	2007

date_to_string <- function(date, format="%d/%m/%Y") {
  format(date,format)
}

string_to_date <- function(string, format="%d/%m/%Y") {
  as.Date(string, format) 
}

fill_logicals <- function(df) {
  cols <- sapply(df,class)=="logical"
  df[,cols] <- apply(df[,cols],c(1,2),function(x) {if(is.na(x)) {return(FALSE)}else{return(x)}})
  return(df)
}

# equivalent to the Quote Whitespace function in PERL
qw <- function(x) unlist(strsplit(x, "[[:space:]]+"))

# translates 1 vector to another based on position
# vec is a vector of values in 'from'
translate <- function(from,to,vec) {
  to[match(vec,from)]
}

translate_specialist_role <- function(vec) {
  roles   <- qw("developer              userResearcher         
                interactionDesigner    qualityAssurance       agileCoach            
                serviceDesigner        businessAnalyst        technicalLeadArchitect
                webDevops              productManager         deliveryManager")
  domains <- c("Software engineering and Development",
               "User research and Design",
               "User research and Design",
               "Software engineering and Development",
               "Change, Training and Transformation",
               "User research and Design",
               "Agile delivery and Governance",
               "Software engineering and Development",
               "Software engineering and Development",
               "Agile delivery and Governance",
               "Agile delivery and Governance")
  translate(roles,domains,vec)
}

translate_type <- function(v) {
  types <- c("digital-outcome","digital-professionals")
  t_types <- c("Outcome","Specialist")
  translate(types,t_types,v)
}

translate_open_to <- function(v) {
  openTo <- c("allSellers","someSellers","oneSeller")
  t_openTo <- c("All","Some","One")
  translate(openTo,t_openTo,v)
}

translate_state_names <- function(v) {
  from <- c("QLD","NSW","Vic","ACT","SA","WA","Tas","NT",
            "Queensland","New South Wales","Victoria","Australian Capital Territory",
            "South Australia","Western Australia","West Australia","Tasmania","Northern Territory")
  to   <- c("QLD","NSW","Vic","ACT","SA","WA","Tas","NT",
            "QLD","NSW","Vic","ACT","SA","WA","WA","Tas","NT")
  translate(from,to,v)
}