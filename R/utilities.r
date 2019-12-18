# configuration
prod_api_url <- Sys.getenv("prod_api_url")
rc_api_url   <- Sys.getenv("rc_api_url")

domains <- c("Software engineering and Development",
             "User research and Design",
             "Agile delivery and Governance",
             "Content and Publishing",
             "Strategy and Policy",
             #"Change, Training and Transformation",
             "Cyber security",
             "Support and Operations",
             "Data science",
             "Emerging technologies",
             "Marketing, Communications and Engagement",
             "Change and Transformation",
             "Training, Learning and Development")
thresholds <- tibble(aoe=domains,
                     aoe_max_price = c(1760,1906,1767,1472,2491,2200,1384,1894,1925,2068,2420,2420))
domains_regex <- x <- paste0("(",
                             paste(domains,collapse =")|(")
                             ,")|(Change, Training and Transformation)")

frequent_roles <- tibble(
                    search = c("agile coach","business analyst","content designer","data analyst","delivery manager",
                             "interaction designer","project manager","scrum master",
                             "service design",
                              "solution architect","user research", "assurance",
                             "enterprise architect",
                             "business architect",
                             "enterprise solutions architect",
                             "solution architect", "programm manager"),
                    role_name = c("Agile Coach","Business Analyst","Content Designer","Data Analyst","Delivery Manager",
                              "Interaction Designer","Project Manager","Scrum Master",
                              "Service Designer",
                              "Solution Architect","User Research", "Assurance Specialist",
                              "Enterprise Architect", "Business Architect", "Enterprise Solutions Architect",
                              "Solution Architect", "Program Manager")
                  )

log_filename     <- "MKT_reporting_log.csv"

# this can vary if scripts are run from different directories - especially likely for RMD
# tests to see if the current WD is /R or /scratch
rel_path_data    <- function() {
  if (str_detect(getwd(),"R|(scratch)$")) {
    return("/../../data/")
  }
  "/../data/"
}

# gets the relative path to the directory with reference files
rel_path_reference    <- function() {
  if (str_detect(getwd(),"R|(scratch)$")) {
    return("/../../reference_data/")
  }
  "/../reference_data/"
}

rel_path_reports <- "/reports/"
attachments_introduced <- as.Date("2017-04-04")
dmp_sons         <- c("SON3413842","SON3364729")

standard_objects <- c("users","buyers","sellers","case_studies","briefResponses",
                      "apps","assessments","contracts","feedback","briefs")

dta_palette <- tibble(
  colour  = c("print_grey","web_grey","dark_blue","mid_blue","light_blue"),
  hexcode = c("#414141","#313131","#007099","#22A0CB","#45C2F0")
)

dta_colours <- function(colour = "dark_blue") {
  dta_palette$hexcode[which(dta_palette$colour == colour)]
}

prod_api <- function(name) {
  return(paste0(prod_api_url,name))
}

# User input to prompt for the authorisation token.
# if a token is already set, then it's simple returned
# DEPRECATED - using keyring now
#prompt_auth <- function() {
#  line <- readline(prompt="Auth auth key: Bearer ")
#  return(paste("Bearer",line))
#}

#auth <- function(header) {
#  return(header)
#}

# fetch the 'header' ie the auth token 
header <- function() {
  return(paste("Bearer",key_get("dmp_api", keyring = "mkt")))
}

# setup keyring values
# Call when installing project on a new device
setup_keyring <- function() {
  keyring_create("mkt") 
  key_set("dmp_api", keyring = "mkt")
  key_set("dmp_x_api", keyring = "mkt")
  key_set("jira", keyring = "mkt")
  key_set("jira-login", keyring = "mkt")
  key_set("slack-support-webhook", keyring = "mkt")
  key_set("slack-marketplace-webhook", keyring = "mkt")
  key_set("slack-dmp-team-webhook", keyring = "mkt")
}

# creates a data frame with each day, week or month since launch 
# defaults to day
datesSinceLaunch <- function(unit="day") {
  initial_date <- as.Date("2016-08-29")
  if (unit == "month") {
    initial_date <- as.Date("2016-09-01")
  }
  data.frame(dates = seq(initial_date,Sys.Date(),by=unit)) %>%
    mutate(dates = floor_date(dates,unit))
}

# Slack notifications to the #marketplace-support channel
send_slack_notification_support <- function(message) {
  POST(url = key_get("slack-support-webhook",keyring="mkt"),
       body = paste0('{"text":"',message,'"}'),
       encode = "json")
}

# Slack notifications to the #marketplace channel
send_slack_notification_marketplace <- function(message) {
  POST(url = key_get("slack-marketplace-webhook",keyring="mkt"),
       body = paste0('{"text":"',message,'"}'),
       encode = "json")
}

# Slack notifications to the #marketplace channel
send_slack_notification_marketplace_team <- function(message) {
  POST(url = key_get("slack-dmp-team-webhook",keyring="mkt"),
       body = paste0('{"text":"',message,'"}'),
       encode = "json")
}

# API interface functions

# gets content from an API
fetchFromAPI <- function(getURL,auth,returnRaw=FALSE, api_version = 1) {
  if (api_version == 1) {
    getdata<-GET(url=getURL, add_headers(Authorization=auth))
  } else {
    getdata<-GET(url=getURL, add_headers(`X-Api-Key`=auth))
  }
  raw <- fromJSON(content(getdata,type="text",encoding="UTF-8"))
  # Assume return is a list of 2 length. One of these is content, the other has links
  # just want the one with content, so reject the one that contains raw[[]]$self
  if (returnRaw) {
    return(raw)
  }
  if (is.null(raw[[1]]$self)) {
    return(raw[[1]])
  }
  return(raw[[2]])
}

# attempts to fetch up to 'tries' times before failing
fetchFromAPI_withexceptions <- function(getURL,auth,tries=3,abandon=TRUE) {
  for (i in 1:tries) {
    tryCatch(
      r <- fetchFromAPI(getURL,auth,TRUE),
      error = function(e) {
        print("API fetch failed, trying again")
        print(e)
      }
    )
    if (exists("r")){
      break
    }
  }
  if (exists("r")) {
    return(r)
  }
  if (abandon) {
    print("Too many failures, abandoning after ",tries," tries")
    stop()
  } else {
    return(NA)
  }
}

# recursive version of fetch, to retrieve all 'next' URLs as a list
fetchAllFromAPI <- function(getURL,auth,x) {
  raw <- fetchFromAPI_withexceptions(getURL,auth)
  #getdata<-GET(url=getURL, add_headers(Authorization=auth))
  #raw <- fromJSON(content(getdata,type="text"))
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

# writes a data frame to a CSV file with label YYYYMMDD-suffix.csv
writey <- function(x,suffix,includeHeader=TRUE, quote=TRUE, sep=",", normalise = TRUE) {
  # 
  if (class(x)[1] == "character") {
    includeHeader = FALSE
  }
  
  if (normalise) {
    x <- normalise_dataframe(x)
  }
  
  write.table(x,file=paste(getwd(),rel_path_data(),substr(as.POSIXct(Sys.time()),1,10),"-",suffix,".csv",sep=""),
              sep=sep,quote=quote,row.names=FALSE,col.names=includeHeader)
}

# reads an older data file from the data directory. 
#change the re_path_data if reading from another directory, or from a different relative location 
ready <- function(filename,rel_path=rel_path_data()) {
  read.csv(paste0(getwd(),rel_path,filename),stringsAsFactors = F)
}

# Read from the reference data files
read_reference <- function(filename, rel_path=rel_path_reference()) {
  read.csv(paste0(getwd(), rel_path, filename), stringsAsFactors = F)
}

save_reference <- function(x, filename, rel_path=rel_path_reference()) {
  write.csv(x, 
            file             = paste0(getwd(), rel_path, filename),
            row.names        = FALSE)
}

# writes a data frame to the reporting directory so thy can be accessed through the mkt-reporting
# web server
writer <- function(x,name) {
  write.table(x,
              file=paste0(getwd(),rel_path_reports,name,".csv"),
              sep=",",
              quote=TRUE,
              row.names=FALSE,
              col.names=TRUE)
}

# save each raw extract file as they're downloaded to avoid re-running the fetch-all functions that day 
# new files overwrite previous versions.
temp_save <- function(obj,obj_name) {
  save(obj,file = paste0(getwd(),rel_path_data(),obj_name,"-raw.rdata"))
}

savey     <- function(obj_name, ...) {
  save(..., file = paste0(getwd(),rel_path_data(),obj_name,"-raw.rdata"))
}

format_mil <- function(x,digits=2,order="M",dollars="$") {
  if (order=="M") {
    return (paste(dollars,round(x/1000000,digits=digits),"M",sep=""))
  } else {
    return (paste(dollars,round(x/1000,digits=digits),"K",sep=""))
  }
}

read_from_google_sheet <- function(sheet_name,worksheet_name="") {
  sheet <- gs_title(sheet_name)
  if (worksheet_name=="") {
    worksheets <- gs_ws_ls(sheet)
    if (length(worksheets) > 1) {
      stop("Need to specify a worksheet name, choose from the following: ",worksheets)
    }
    gs_read(sheet,ws = worksheets[1])
  } else {
    gs_read(sheet,ws = worksheet_name)
  }
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

# parses date/time strings strictly in the format "2017-06-05T00:31:40.363794+00:00"
# Will convert to TZ="Australia/Canberra"
# *assumes the timestamp is UTC*
# returns POSIXct
parse_utc_timestamp_datetime <- function(c) {
  x <- parse_date_time(substr(c,1,19),"Y m d H M S")
  attributes(x)$tzone <- "Australia/Canberra"
  return(x)
}

# parses date/time strings strictly in the format "2017-06-05T00:31:40.363794+00:00"
# Will convert to TZ="Australia/Canberra"
# *assumes the timestamp has TZ UTC*
# returns Date
parse_utc_timestamp_date <- function(c) {
  return(date(parse_utc_timestamp_datetime(c)))
}

date_to_string <- function(date, format="%d/%b/%Y") {
  format(date,format)
}

string_to_date <- function(string, format="%d/%m/%Y") {
  as.Date(string, format) 
}

# convenience function to display the current date in dd MMM YYYY format
today <- function() {
  date_to_string(Sys.Date(), format = "%d %b %Y")
}

fill_logicals <- function(df) {
  cols <- sapply(df,class)=="logical"
  df[,cols] <- apply(df[,cols],c(1,2),function(x) {if(is.na(x)) {return(FALSE)}else{return(x)}})
  return(df)
}

# equivalent to the Quote Whitespace function in PERL
qw <- function(x) unlist(strsplit(x, "[[:space:]]+"))

qnl <- function(x) {
  y <- unlist(str_split(x, "[\n]"))
  y[nchar(y) > 0]
}

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
               "Change and Transformation",
               "User research and Design",
               "Agile delivery and Governance",
               "Software engineering and Development",
               "Software engineering and Development",
               "Agile delivery and Governance",
               "Agile delivery and Governance")
  translate(roles,domains,vec)
}

# translate the domain codes used in briefs in the "sellerCategory" field 
translate_domain_code <- function(vec) {
  codes <- as.character(c(6,3,4,7,1,8,10,11,13,9,14,15))
  translate(codes,domains,vec)
}

translate_type <- function(v) {
  types <- c("digital-outcome","digital-professionals","training","rfx","atm", "specialist","training2")
  t_types <- c("Outcome","Specialist","Training","RFX","ATM","Specialist","Training2")
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
            "South Australia","Western Australia","West Australia","Tasmania","Northern Territory",
            "VIC")
  to   <- c("QLD","NSW","Vic","ACT","SA","WA","Tas","NT",
            "QLD","NSW","Vic","ACT","SA","WA","WA","Tas","NT", "Vic")
  # compare <- data.frame(from = from, to = to)
  translate(str_to_lower(from),to,str_to_lower(str_squish(v)))
}

#test if a vector of email addresses are all valid, using a simple regex
valid_email_address <- function(emails) {
  return(sum(!grepl("^[a-zA-Z_\\.]+@[a-zA-Z_\\.]+$",emails)) == 0)
}

# lists all objects in memory that have a timestamp
timestamps <- function(objects) {
  ts <- sapply(objects,function(x) {
    y <- attr(get(x),"timestamp")
    if (!is.null(y)) {
      return(paste0(y))
    }
    return(NA)
  })
  ts <- ts[names(ts) %in% standard_objects]
  #ts <- ts[!is.na(ts)]
  print(tibble(object=names(ts),ts=ts))
}

domains_from_emails <- function(emails) {
  split <- str_split(emails,"@")
  sapply(split,function(x) x[2])
}

# compare contents in 2 vectors
compare_vectors <- function(a,b,diffOnly=TRUE,print=TRUE) {
  if (print) {
    if (diffOnly) {
      common <- sum(a %in% b)
      cat(paste0("Number of common entries: ",common,"\n"))
    } else {
      cat("Common elements\n")
      print(a[a %in% b])
    }
    cat("\nIn vector 1 only\n")
    print(a[!a %in% b])
    cat("\nIn vector 2 only\n")
    print(b[!b %in% a])
  } else {
    return(list(
      common        = a[a %in% b],
      vector_a_only = a[!a %in% b],
      vector_b_only = b[!b %in% a]
    ))
  }
}

# Converts double quotes to singles for columns of class character
# Double quote marks can cause issues with data written out as csv
convert_quotes <- function(df) {
  for (i in 1:ncol(df)) {
    if (class(df[,i]) == "character") {
      df[,i] <- gsub("\"","'",df[,i])
    }
  }  
  return(df)
}

# simple wrapper for kable extra 
kables <- function(x) {
  kable(x) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
}

# prepare a table of available data files 
get_data_files <- function(rel_path = rel_path_data()) {
  filenames = list.files(path=paste0(getwd(),rel_path))
  files <- data.frame(str_match(filenames,"(\\d{4}\\-\\d{2}\\-\\d{2})\\-(.*)\\.csv"),stringsAsFactors=F)
  names(files) <- c("filename","date","type")
  files <- files %>% filter(!is.na(filename))
  files$date <- as.Date(files$date)
  return(files)
}

# reloads the latest contracts file that is at least 'days' old
last_contracts_file <- function(days=7) {
  files <- get_data_files() %>% 
    filter(type=="contracts") %>%
    filter(date <= Sys.Date() - days)
  filename <- files[which(files$date == max(files$date)),]$filename
  return(filename)
}

# apply some simple regularisation rules to the column names of a data frame
cleanup_df_names <- function(df) {
  n <- names(df)
  n <- str_to_lower(n)
  n <- gsub("[\\(\\)]",".",n)
  n <- gsub("\\s+","_",n)
  names(df) <- n
  return(df)
}

# convenience function for pasting a tab delimited file into R
readClip <- function(header=T,delimiter="\t") {
  read.table("clipboard",header = header,sep = delimiter,stringsAsFactors = F)
}

# writes a data frame to the clipboard
# Collapses the dataframe into a tab delimited format, which can be pasted straight into Excel
# WARNING - it's a bit primitive at the moment. Won't work so will if there are embedded tabs
writeClip <- function(df) {
  lines <- apply(df,1,function(x) {paste(x,collapse = "\t")})
  writeClipboard(lines)
}

# clear warnings
clear_warnings <- function() {
  assign("last.warning", NULL, envir = baseenv())
}

# cleanup any columns of type character to replace quotation marks as they can cause issues with csv files
normalise_dataframe <- function(df) {
  if (class(df) == "data.frame") {
    char_columns <- which(sapply(df, class) == "character")
    for (i in char_columns) {
      df[,i] <- str_replace_all(df[,i],'"', "'")
    }
  } else {
    if (class(df) == "character") {
      df <- str_replace_all(df,'"', "'")
    }
  }
  df
}

# standard normalisation of aoe names to facilitate matching between datasets
normalise_aoe <- function(aoes) {
  aoes <- str_to_lower(aoes)
  aoes <- str_replace_all(aoes,"\\,|_"," ")
  #aoes <- str_replace_all(aoes,"_"," ")
  str_squish(aoes)
}

session_numbers <- function() {
  sessions <- read.csv(paste0(getwd(),rel_path_data(),"session_numbers.csv"))
  sessions$month <- string_to_date(sessions$month)
  sessions
}

# convenience function for converting a date string to a date
ad <- function(dateasstring = Sys.Date()) {
  if (class(dateasstring) == "Date") {
    return(dateasstring)
  }
  as.Date(dateasstring)
}

# helper function for Knitr files to load the latest version of the
# files
latest_data_file <- function() {
  dfiles <- list.files(paste0(getwd(), 
                              rel_path_data()), 
                       pattern = "^20\\d{2}\\-\\d{2}\\-\\d{2}\\.Rdata$")
  tibble(dfiles, date = as.Date(str_sub(dfiles, 1,10))) %>% 
    filter(date == max(date)) %>%  
    pull(dfiles)
}

# wrapper for dplyr::count to also calculate percentages for each count
count_percentages <- function(..., digits = 0) {
  dplyr::count(...) %>% 
    mutate(perc = round(n * 100 / sum(n), digits = digits)) %>% 
    adorn_totals()
}

# calculates the total edit distance between two character strings
# cv = 2 srting vector, eg c("foo","bar")
# eg. to compare two columns in a data.frame
# results <- apply(a.data.frame[,c("colX", "colY")], 1, edit_distance)
# 
edit_distance <- function(cv) {
  c1 <- str_squish(cv[1])
  c2 <- str_squish(cv[2])
  sum(
    drop(
      attr(
        adist(c1, 
              c2, 
              counts = TRUE, 
              partial = FALSE, 
              ignore.case = TRUE), 
        "counts"
      )
    )
  )  
}

# Compares two character vectors of the same length, returning the edit
# distance between the two
# Convenience function for function edit_distance
edit_dist <- function(c1, c2) {
  apply(data.frame(c1, c2), 1, edit_distance)
}
