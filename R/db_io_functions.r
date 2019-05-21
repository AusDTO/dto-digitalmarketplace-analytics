# Connections
library(odbc)
db_connect_local <- function() {
  con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "localhost\\SQLEXPRESS;Database=master;Trusted_Connection=True;", 
<<<<<<< HEAD
                      UID = "sa", 
                      #PWD = rstudioapi::askForPassword("Database password"),
                      PWD = Sys.getenv("local_db_pwd"),
=======
                      UID = "sa", PWD = rstudioapi::askForPassword("Database password"), 
>>>>>>> 3b731893c393a03c37e1d31edf78981f67d83ac8
                      Port = 1433)
  return(con)
}

## Tables
# table with the latest contract notice records, only for DMP and DSPP panels 
db_dmp_contracts_table          <- "mkt.austender.dmp_contracts"
# table with all of the Austender Contract Notice Export records since July 2016
db_contract_notice_export_table <- "mkt.austender.contract_notices"

# READ functions

### Read all marketplace contracts from DB
# Reads from the mkt.austender.dmp_contracts table
# Default is to return the latest set of values
# 'con'    = connection object
# 'latest' = if true, returns the latest set of values in the table
# 'date'   = is the target date if latest == FALSE
#     If there's no entry for the target date, the next available later date will be returned
# 'all'    = if TRUE, returns all records
db_read_dmp_contracts <- function(con, latest = TRUE, target_date = "2018-12-31", all = FALSE) {

  if (all) {
    c <- dbReadTable(con,db_dmp_contracts_table) %>%
      mutate(
        Publish.Date = as.Date(Publish.Date),
        Start.Date   = as.Date(Start.Date),
        End.Date     = as.Date(End.Date),
        extract_date = as.Date(extract_date)
      )
    return(c)
  }
  
  read_query <- paste0(
      "select * from [",db_dmp_contracts_table,"] ",
      "where extract_date = ")
  if (latest) {
    read_query <- paste0(read_query,"(select max(extract_date) from [",db_dmp_contracts_table,"])")
  } else {
    read_query <- paste0(read_query,"(select min(extract_date) from [",db_dmp_contracts_table,
                        "] where extract_date >= '",target_date,"')")
  }

  sr <- dbSendQuery(con,read_query)
  c  <- dbFetch(sr) %>%
    # convert the date columns to Date class
    mutate(
      Publish.Date = as.Date(Publish.Date),
      Start.Date   = as.Date(Start.Date),
      End.Date     = as.Date(End.Date),
      extract_date = as.Date(extract_date)
    )
  dbClearResult(sr)

  return(c)
}

<<<<<<< HEAD
# reads a summary of by day
db_read_dmp_contracts_summary <- function(con) {
  read_query <- paste0(
            "select sum(Value), count(Value), extract_date from [",
            db_dmp_contracts_table,
            "]
            group by extract_date
            order by extract_date desc")
  sr <- dbSendQuery(con,read_query)
  c  <- dbFetch(sr)
  dbClearResult(sr)
  names(c) <- c("total_value", "count", "extract_date")
    # convert the date columns to Date class
  c %>% mutate(extract_date = as.Date(extract_date))
}

=======
>>>>>>> 3b731893c393a03c37e1d31edf78981f67d83ac8
# Appends the database with contracts extracted today
db_append_dmp_contracts <- function(con,contracts) {
  
  # chck the contracts date attibute is today's date
  if (as.Date(attributes(contracts)$timestamp, tz = "Australia/Sydney") != Sys.Date()) {
    stop("contracts timestamp attribute doesn't match today's date")
  }
  
  c_ <- contracts %>%
    select(Agency:Value) %>%
    mutate(extract_date = Sys.Date())
  
  sr             <- dbSendQuery(con,paste0("select distinct extract_date from [",db_dmp_contracts_table,"]"))  
  extract_dates  <- dbFetch(sr) %>%
    mutate(extract_date = as.Date(extract_date))
  dbClearResult(sr)
    
  if (Sys.Date() %in% extract_dates$extract_date) {
    stop("Contracts with today's date are already loaded")
  }
  
  dbWriteTable(con,db_dmp_contracts_table,c_,append = TRUE)
}


# Admin functions

## Setup and initialise dmp_contracts table
# sets up a new table - at the moment they're stored in string format
db_initialise_dmp_contracts <- function(con) {

  read_table <- function(filename) {
    c_              <- read.csv(file=paste0(getwd(),rel_path_data(),filename),stringsAsFactors = F) %>%
      select(Agency:Value)
    c_$extract_date <- str_match(filename,"\\d{4}\\-\\d{2}\\-\\d{2}")[1,1]
    print(c_[1,"extract_date"])
    return(c_)
  }
  
  # check the table is not already created
  if (sum(str_detect(dbListTables(con),db_dmp_contracts_table)) > 0) {
    stop("Database is already created. Drop it manually before initialising")
  }
  
  c_files <- list.files(paste0(getwd(),rel_path_data())) %>% 
    as.tibble() %>% 
    filter(str_detect(value,"\\-[Cc]ontracts\\.+csv"))
  
  tables <- lapply(c_files$value,read_table)
  all    <- bind_rows(tables) %>%
    mutate(
      Publish.Date = as.Date(Publish.Date),
      Start.Date   = as.Date(Start.Date),
      End.Date     = as.Date(End.Date),
      extract_date = as.Date(extract_date)
    )
  
  dbWriteTable(con,db_dmp_contracts_table,all)
}

# initialise a local copy of the contract notice summary files available at
# https://www.tenders.gov.au/?event=public.reports.listCNWeeklyExport
db_initialise_contract_notices <- function(con,cns) {
  dbWriteTable(con,db_contract_notice_export_table,cns)
}
<<<<<<< HEAD


=======
>>>>>>> 3b731893c393a03c37e1d31edf78981f67d83ac8
