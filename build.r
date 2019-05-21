library(tidyverse)
library(plotly)
library(jsonlite)
library(httr)
library(googlesheets)
library(knitr)
library(RColorBrewer)
library(DBI)
library(lubridate)
library(skimr)
#library(kable)
library(kableExtra)
library(keyring)

# Source the various R files
assets <- list.files(path = "R", pattern = "\\.r$", full.names = TRUE)
created_assets <- lapply(assets, source)
rm(assets, created_assets)

<<<<<<< HEAD
#if (!exists("header")) {
#  header <- prompt_auth()
#}
#if (!exists("con")) {
#  con <- db_connect_local()
#}
#if (!exists("cns")) {
#  cns <- load_austender_cns()
#}
=======
if (!exists("header")) {
  header <- prompt_auth()
}
if (!exists("con")) {
  con <- db_connect_local()
}
if (!exists("cns")) {
  cns <- load_austender_cns()
}
>>>>>>> 3b731893c393a03c37e1d31edf78981f67d83ac8

timestamp      <- Sys.time()

users          <- extract_users(header(),TRUE) # FALSE = don't include DMP team logins
buyers         <- process_buyers(users, FALSE) # TRUE - update the buyer spreadsheet
contributors   <- process_contributors(users)
    s_list     <- extract_sellers(header()) 
sellers        <- s_list$sellers
case_studies   <- s_list$case_studies
seller_domains <- s_list$seller_domains
    s_list     <- NULL
briefsExtract  <- extract_briefs(header,FALSE)    
briefs         <- process_briefs(briefsExtract,buyers,FALSE)
mkt_briefs     <- update_brief_details(briefs)
briefResponses <- extract_brief_responses(header()) %>% process_brief_responses(briefs)
apps           <- extract_applications(header())
assessments    <- extract_assessments(header())
all_sellers    <- process_sellers_and_applications(sellers,apps,users)
seller_exceptions <- extract_exceptions() # sellers that have been removed 
contracts      <- extract_austender() %>% process_contracts(sellers,seller_exceptions)
feedback       <- extract_feedback(header())
agency_summary <- process_agency_summary(buyers,briefs,contracts)
<<<<<<< HEAD
#cns            <- update_contract_summary_notices(cns)
panels         <- extract_panel_listing()
j_tickets      <- update_jira_tickets()
generate_austender_update(sellers)
=======
cns            <- update_contract_summary_notices(cns)
panels         <- extract_panel_listing()
#sellerMailList <- process_seller_email_list(sellers,contributors,apps)
>>>>>>> 3b731893c393a03c37e1d31edf78981f67d83ac8

# write useful observations to a log  
log_current_observations(timestamp)

# save the extracted data to local drive
save_data()
# update database tables
<<<<<<< HEAD
#save_to_db()
=======
save_to_db()
>>>>>>> 3b731893c393a03c37e1d31edf78981f67d83ac8



# generate the standard Knitr reports
standard_reports()

## ad-hoc reports - these are not intended to run if you 'source' this script
if (FALSE) {
  rmarkdown::render("R\\monthly_report.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  rmarkdown::render("R\\dto_dashboard.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  rmarkdown::render("R\\snapshot.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  rmarkdown::render("R\\transactions.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  rmarkdown::render("R\\pricing_analysis.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  rmarkdown::render("R\\key_facts.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  rmarkdown::render("R\\monthly_performance.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  rmarkdown::render("R\\seller_extract.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  rmarkdown::render("R\\pricing_by_role.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  rmarkdown::render("scratch\\2018-10-15 - maxrates vs bids.rmd",output_dir=paste0(getwd(),"\\reports\\"))
}

# Use to reload data rather than extract from scratch if restarting a session. 
# Run this directly, it'll be ignored if you 'source' this script 
if (!exists("users")) {
  load(paste0(getwd(),rel_path_data(),substr(as.POSIXct(Sys.time()),1,10),".Rdata"))
  cns       <- load_austender_cns()
  #j_tickets <- update_jira_tickets()
}

timestamps(ls())
keyring_lock("mkt")
