#
# produce a list of sellers to add to the panel on Austender
# s = sellers data.frame
##### DEPRECATED ######
# Use generate_austender_update() instead
#
update_upload_list <- function(s) {
  on_panel <- ((nchar(s$assessed_aoe) > 0) & s$dmp_framework)
  # filter down to all sellers that should be on the panel
  s      <- s[on_panel,]

  # load the list of earlier batches
  gs_sheet <- gs_title("Austender Uploads (and original assessments sheet)")
  ws       <- gs_sheet %>% gs_read(ws = "Austender Batches")
  
  s <- s[!(s$abn %in% ws$abn),]
  if (dim(s)[1] == 0) {
    return("No updates required")
  }
  
  # Create new batch and write back to the spreadsheet
  s$batch <- max(ws$batch) + 1
  s$date       <- date_to_string(Sys.Date())
  s$abn_exempt <- "no"
  updated <- rbind(s[,names(ws)],ws)
  updated <- updated[order(updated$batch),]
}


# function to inspect all live briefs that are open to selected or to 1
# returns the sellers that have been invited, as well as information about
# the seller's status
# #### DEPRECATED ######
# This won't work with the new RFX and ATM approaches
invited_sellers <- function(briefs,briefResponses) {
  br <- briefs %>% filter(status == "live",openTo %in% c("Some","One"))
  if (dim(br)[1]==0) {
    return(data.frame(id="No open to 1/selected briefs"))
  }
  # fetch the seller details for an individual brief
  fetchInvitedSellers <- function(id) {
    #bQuery <- paste("https://dm-api.apps.platform.digital.gov.au/api/briefs",as.character(id),sep="/")
    bQuery <- prod_api(paste0("briefs/",as.character(id)))
    bRaw   <- fetchFromAPI(bQuery,header)
    emails = bRaw$sellerEmailList
    if (bRaw$sellerSelector=="oneSeller") {
      emails = bRaw$sellerEmail
    }
    if (length(emails)==0) {
      emails <- "**missing**@**missing**"
    }
    if (!valid_email_address(emails)) {
      emails <- "**invalid**@**invalid**"
    }
    df        <- data.frame(id=id,emails=emails,stringsAsFactors = FALSE)
    df$closes <- as.Date(bRaw$dates$closing_date)
    if (is.null(bRaw$areaOfExpertise)) {
      df$aoe <- NA
    } else {
      df$aoe    <- bRaw$areaOfExpertise
    }
    return(df)
  }  

  invitees                   <- bind_rows(lapply(br$id,fetchInvitedSellers))
  invitees$email_domain      <- matrix(unlist(strsplit(invitees$emails,"@")),ncol=2,byrow=TRUE)[,2]
  invitees$seller_registered <- invitees$email_domain %in% sellers$email_domain
  invitees$user_registered   <- invitees$emails %in% users$email_address
  invitees$is_contact        <- invitees$emails %in% sellers$contact_email
  aoes                       <- sellers[,c("code","email_domain","assessed_aoe")]
  invitees                   <- merge(invitees,aoes,by.x="email_domain",by.y="email_domain")
  invitees$assessed          <- FALSE
  for (i in 1:dim(invitees)[1]) {
    invitees[i,"assessed"] <- grepl(invitees[i,"aoe"],invitees[i,"assessed_aoe"])
  }
  invitees[is.na(invitees$assessed),"assessed"] <- 
    nchar(invitees[is.na(invitees$assessed),"assessed_aoe"]) > 2
  invitees$assessed_aoe      <- NULL
  x <- paste(invitees$id,invitees$code,sep="|")
  y <- paste(briefResponses$id,briefResponses$supplierId,sep="|")
  invitees$has_applied       <- x %in% y
  return(invitees[order(invitees$closes),])
}



# List of primary contacts for sellers on the Marketplace
# 
#generate_seller_primary_contact_list <- function(sellers,users) {
  
#}

# extract the sellers that are approved to join the Digital Marketplace panel
sellers_approved_for_dmp <- function(sellers) {
  sellers %>%
    filter(dmp_framework) %>%
    filter(nchar(assessed_aoe)>0)
}

# filter buyers to remove DTA test accounts and/or deactivated users
filter_buyers <- function(buyers,includeDTA = FALSE, includeDeactivated = FALSE) {
  b <- buyers
  if (!includeDTA) {
    b <- b %>%
      filter(!grepl("\\+",email_address))
  }
  if (!includeDeactivated) {
    b <- b %>% filter(active)
  }
  return(b)  
}

  
# filters teh full CNS file to the last amendment only
# Note that the 'month' is calculated as the date the contract was
# originally published
filter_cns_to_latest_amendment <- function(cns) {
  ids <- cns %>% 
    group_by(CN.ID) %>%
    summarise(last_amendment = max(Amendment)) %>%
    mutate(unique.id = paste(CN.ID,last_amendment,sep="."))
  
  cns %>% 
    filter(unique.id %in% ids$unique.id) %>% 
    mutate(month = floor_date(Publish.Date,unit="month"))
}


update_jira_tickets <- function() {
  if (!exists("j_tickets")) {
    j_dates   <- str_sub(list.files(path=paste0(getwd(),rel_path_data()),pattern="j_tickets"),1,10)
    latest    <- max(as.Date(j_dates))
    print(paste("loading the latest J Tickets file - dated: ",latest))
    c_classes <- c( "character","character","character","factor","character","character",
                    "character","character","character","character","Date")
    j_tickets <- read.csv(file       = paste0(getwd(),rel_path_data(),latest,"-j_tickets.csv"),
                          colClasses = c_classes)
    j_tickets$created         <- parse_date_time(j_tickets$created,"Y m d H M S")
    #j_tickets$updated         <- parse_date_time(j_tickets$updated,"Y m d H M S")
    j_tickets$resolution_date <- parse_date_time(j_tickets$resolution_date,"Y m d H M S")
  }
  
  # is the latest updated date more than a week old? if so, do a full refresh
  last <- max(j_tickets$updated)
  if (last + days(7) <= Sys.Date()) {
    j_tickets <- extract_jira_tickets()
    writey(j_tickets,"j_tickets")
  }
  return(j_tickets)
}

# loads the local copy of the agencies file. Checks if there are any missing agencies
# If no missing agencies, returns the agencies data frame
# If missing agencies, calls update_agencies() to manually add the details of those agencies
load_update_agencies <- function(users) {
  a             <- read_reference("agencies.csv")
  buyer_domains <- users %>% 
    filter(role == "buyer") %>% 
    pull(email_domain) %>% 
    unique()
  if (sum(!buyer_domains %in% a$email_domain)) {
    print("Missing buyers in the Agencies file. Add details in the editor for these agencies:")
    print(buyer_domains[!buyer_domains %in% a$email_domain])
    update_agencies()
  } else {
    a
  }
}

# loads the last extracts file from file. If the last response is more than 7 days ago, 
# will reextract all records
load_update_brief_responses <- function(briefs, force_update = FALSE) {
  latest <- list.files(path = paste0(getwd(), rel_path_data()), 
                       pattern = "briefResponses") %>% 
    str_sub(1,10) %>% 
    as.Date() %>% 
    max(na.rm = TRUE)
  bR <- ready(paste0(latest,"-briefResponses.csv")) %>% 
    mutate(applicationDate = as.Date(applicationDate))
  last_response <- max(bR$applicationDate, na.rm = TRUE)
  if (as.integer(Sys.Date() - last_response) > 7 | force_update) {
    print(paste("Last brief response was",last_response, "- extracting the latest records"))
    extract_brief_responses() %>% process_brief_responses(briefs)
  } else {
    bR
  }
}

# loads the last extracts file from file. If the last response is more than 7 days ago, 
# will reextract all records
load_update_applications <- function(force_update = FALSE) {
  latest <- list.files(path = paste0(getwd(), rel_path_data()), 
                       pattern = "apps") %>% 
    str_sub(1,10) %>% 
    as.Date() %>% 
    max(na.rm = TRUE)
  a <- ready(paste0(latest,"-apps.csv")) %>% 
    mutate(last_update_time = as.Date(last_update_time))
  last_update <- max(a$last_update_time, na.rm = TRUE)
  if (force_update | as.integer(Sys.Date() - last_update) > 7) {
    print(paste("Last application update was",last_update, "- extracting the latest records"))
    extract_applications()
  } else {
    a
  }
}

update_agencies <- function() {
  ag        <- read_reference("agencies.csv") 
  ag_update <- edit(ag)
  x         <- readline("Update the Agencies reference file? Y/N")
  if (x %in% c("Y", "y", "Yes", "yes")) {
    print("Saving agencies file ...")
    save_reference(ag_update, "agencies.csv")
    ag_update
  } else {
    print("Abandoning any updates ...")
    ag
  }
}

# loads the latest briefs file in /data to use as a reference for buyer information 
# in the briefs data frame
load_last_briefs <- function() {
  latest <- list.files(path = paste0(getwd(), rel_path_data()), 
                      pattern = "briefs") %>% 
    str_sub(1,10) %>% 
    as.Date() %>% 
    max(na.rm = TRUE)
  print(paste("Loading the last briefs file, dated",latest))
  ready(paste0(latest, "-briefs.csv"))
}

#update_summary_stats <- function() {
#  generate_summary_stats_table(summary_stats)
#  savey("DMP_Stats", summary_stats)
#  summary_stats
#}
