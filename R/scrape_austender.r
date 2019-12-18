# Functions to scrape data from Austender


# download weekly contract notice summary files from Austender

# Get a list of the available weekly Austender summary files from the website
extract_available_contract_summary_notices <- function() {
  url_origin <- "https://www.tenders.gov.au/"
  getdata<-GET(url=paste0(url_origin,"Reports/CnWeeklyExportList"))  
  cont  <- content(getdata,as="text")
  all_lines <- strsplit(cont,"<a")[[1]]
  lines      <- all_lines[which(str_detect(all_lines,"CnWeeklyExportDownload"))]
  rel_urls   <- str_extract(lines,"Reports[^\\\"]+")
  full_urls  <- paste0(url_origin,rel_urls)
  date_range <- str_extract(lines,"\\d{2}\\-\\w{3}\\-1[6789] to \\d{2}\\-\\w{3}\\-1[6789]")
  start_date <- dmy(str_extract_all(date_range,"\\d{2}\\-\\w{3}\\-1[6789]",simplify=TRUE)[,1])
  end_date   <- dmy(str_extract_all(date_range,"\\d{2}\\-\\w{3}\\-1[6789]",simplify=TRUE)[,2])
  tibble(url=full_urls,date_range=date_range,start_date=start_date,end_date=end_date)
}

# available_files is the data.frame generated by extract_available_contract_summary_notices
# filter this list accordingly if you don't wan to download all of them
extract_contract_summary_notices <- function(available_files) {
  ## load the files from the download_list
  dfs <- vector("list",length=nrow(available_files))
  for (i in seq_along(dfs)) {
    #for (i in 1:2) {
    print(available_files$date_range[i])
    df <- read.xlsx(available_files$url[i], startRow = 3) %>% 
      mutate(
        Publish.Date = convertToDateTime(Publish.Date),
        Amendment.Publish.Date = convertToDateTime(Amendment.Publish.Date),
        Start.Date   = convertToDate(Start.Date),
        End.Date     = convertToDate(End.Date)
      )
    df[,33:36] <- available_files[i,]
    dfs[[i]] <- df
  }
  new_cns <- bind_rows(dfs)
  save(new_cns,file=paste0("RawCNSData.",as.character(Sys.Date()),".Rdata"))
  
  
  # split the CN.ID into CN.DI and amendment number
  new_cns <- new_cns %>%
    separate(CN.ID, into=c("CN.ID","Amendment"), sep="\\-A", convert=TRUE, fill = "right")
  
  new_cns$Amendment[is.na(new_cns$Amendment)] <- 0
  
  # convert some of the columns
  new_cns$published    <- new_cns$Publish.Date
  new_cns[new_cns$Amendment>0,]$published <- new_cns[new_cns$Amendment>0,]$Amendment.Publish.Date
  new_cns <- arrange(new_cns,published)
  new_cns <- new_cns %>% 
    mutate(unique.id = paste0(CN.ID,".",Amendment),
           Record.Published = Publish.Date)
  new_cns[!is.na(new_cns$Amendment.Publish.Date),]$Record.Published <- new_cns[!is.na(new_cns$Amendment.Publish.Date),]$Amendment.Publish.Date
  return(new_cns)
}

# Takes an existing DF of contract notices and identifies if there are any new files available
# before returning an updated set of contract notices
update_contract_summary_notices <- function(cns,saveOnCompletion = TRUE) {
  current_end_dates <- unique(cns$end_date)
  available_files   <- extract_available_contract_summary_notices()
  to_download       <- available_files %>% 
    filter(!end_date %in% current_end_dates)
  if (nrow(to_download) > 0) {
    new_cns <- extract_contract_summary_notices(to_download)
  } else {
    cat("No additional files available\n")
    return(cns)
  }
  cns <- bind_rows(cns,new_cns)
  if (saveOnCompletion) {
    save_reference(cns,"CNS-raw.rdata")
  }
  return(cns)
}

# recalculate variance between contracts
# determines the value_amendment, min_amendment and max_amendment
# for each amendment 
# Needs to be done on the full set of contract notices, so call this after
# update_contract_summary_notices()
post_process_cns <- function(cns) {
  
  # if these are already set, remove them so they can be recalculated 
  if ("min_amendment" %in% names(cns)) {
    cns <- select(cns,-min_amendment, -max_amendment)
  }
  
  amendments <- cns %>% 
    group_by(CN.ID) %>% 
    summarise(min_amendment = min(Amendment),
              max_amendment = max(Amendment))
  
  cns %>% 
    arrange(CN.ID, Amendment) %>% 
    left_join(amendments, by = "CN.ID") %>% 
    mutate(value_amendment = Value - lag(Value)) %>% 
    mutate(value_amendment = case_when(
      Amendment == min_amendment ~ Value,
      TRUE                       ~ value_amendment
    ))
} 

load_austender_cns <- function() {
  load(paste0(getwd(),rel_path_reference(),"CNS-raw.rdata"))
  cns <- obj
  rm(obj)
  return(cns)
}

# download information about the panels published on Austender
scrape_list_of_panels <- function() {
  # download the full list of panels, including closed ones
  panel_query <- "https://www.tenders.gov.au/?event=public.SON.searchDownload&download=true&atmtype=archived%2Cclosed%2Cpublished%2Cproposed&keywordtypesearch=AllWord&postcode=&valuefrom=&datestart=&keyword=&contractto=&suppliername=&valueto=&category=&agencyuuid=&panelarrangement=1&orderBy=Relevance&type=sonSearchEvent&agencyreporttype=&portfoliouuid=&dateend=&participantstatus=&publishfrom=&supplierabn=&datetype=Publish%20Date&agencyrefid=&multiagencyaccess=&contractfrom=&atmid=&agencyStatus=&numagencystatus=%2D1&publishto=&sonid=&multype=archived%2Cclosed%2Cpublished"
  getdata<-GET(url=panel_query)  
  cont <- content(getdata,as="text")
  x <- strsplit(cont,"\\r\\n")[[1]]
  header_line <- which(grepl("SON ID\\tTitle",x))
  headings    <- strsplit(x[header_line],"\\t")[[1]]
  headings    <- gsub("\\s+","\\.",headings)
  y           <- x[(header_line+1):length(x)]
  z           <- strsplit(y,"\\t")
  df          <- data.frame(do.call(rbind,z),stringsAsFactors = FALSE)
  names(df)   <- headings
  for (i in c(1:3,5,8,9)) {
    df[,i] <- gsub('[\\"\\=]+','',df[,i])
  }
  df$Publish.Date        <- string_to_date(df$Publish.Date,"%d-%b-%y")
  df$Contract.Start.Date <- string_to_date(df$Contract.Start.Date,"%d-%b-%y")
  df$Contract.End.Date   <- string_to_date(df$Contract.End.Date,"%d-%b-%y")
  # filter out panels that have closed
  df %>% filter(Contract.End.Date > Sys.Date())
}

# still under development
scrape_individual_panel <- function(sonid) {
  son_query <- paste0("https://www.tenders.gov.au/?event=public.advancedsearch.CNSONRedirect&type=sonSearchEvent&keyword=&KeywordTypeSearch=AllWord&SONID=",
                      sonid,
                      "&dateType=Publish+Date&dateStart=&dateEnd=&MultiAgencyAccess=&panelArrangement=&supplierName=&supplierABN=&ATMID=&AgencyRefId=")
  getdata<-GET(url=son_query)  
  cont <- content(getdata,as="text")
  panel_url <- paste0("https://www.tenders.gov.au/",
                      str_match_all(cont,"\\?event=public\\.\\SON\\.view[^\"]+")[[1]][1,1]
                      )
  cont    <- content(GET(url=panel_url),as="text")
  #extract the sellers

  
}

#summarise_cns_by_panels
#cns_temp <- cns %>% 
#  filter(grepl("SON",SON.ID),Amendment==0) %>%
#  group_by(SON.ID) %>% 
#  summarise(earliest = min(Publish.Date),
#            latest   = max(Publish.Date),
#            number   = n(),
#            total_value = sum(Value),
#            unique_sellers = length(unique(Supplier.ABN)))
#p <- panels %>% left_join(cns_temp,by=c("SON ID"="SON.ID"))
