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
library(openxlsx)
library(janitor)

# Source the various R files
assets <- list.files(path = "R", pattern = "\\.r$", full.names = TRUE)
created_assets <- lapply(assets, source)
rm(assets, created_assets)

if (!exists("con")) {
  con <- db_connect_local()
}
if (!exists("cns")) {
  cns <- load_austender_cns()
}

timestamp      <- Sys.time()

users          <- extract_users(header(),TRUE) # FALSE = don't include DMP team logins
agencies       <- gs_title("ref-domains") %>% gs_read(ws = "agencies")
buyers         <- process_buyers(users, agencies, FALSE) # TRUE - update the buyer spreadsheet
contributors   <- process_contributors(users)
    s_list     <- extract_sellers(header()) 
sellers        <- s_list$sellers
#case_studies   <- s_list$case_studies #deprecated
seller_domains <- s_list$seller_domains
    s_list     <- NULL
dmp_sellers    <- extract_vendors_from_panel("SON3413842")
briefsExtract  <- extract_briefs(header, return_all = TRUE)    
briefs         <- process_briefs(briefsExtract, buyers, FALSE)
#mkt_briefs     <- update_brief_details(briefs)
#briefResponses <- extract_brief_responses(header()) %>% process_brief_responses(briefs)
#apps           <- extract_applications(header())
###assessments    <- extract_assessments(header()) no longer in use
all_sellers    <- process_sellers_and_applications(sellers,apps,users)
seller_exceptions <- extract_exceptions() # sellers that have been removed 
contracts      <- extract_austender() %>% process_contracts(sellers,seller_exceptions, agencies)
feedback       <- extract_feedback(header())
agency_summary <- process_agency_summary(buyers,briefs,contracts)
cns            <- update_contract_summary_notices(cns) %>% post_process_cns()
#panels         <- extract_panel_listing()
j_tickets      <- update_jira_tickets()
seller_activity <- generate_last_activity(sellers, apps, users, j_tickets, briefResponses, contracts, dmp_sellers)
generate_austender_update(sellers)

# write useful observations to a log  
log_current_observations(timestamp)

# save the extracted data to local drive
save_data()
# update database tables
save_to_db()
# update the marketplace stats file
load(paste0(getwd(),rel_path_data(),"DMP_Stats-raw.Rdata"))
marketplace_stats <- generate_summary_stats_table(marketplace_stats)
save(marketplace_stats, file=paste0(getwd(),rel_path_data(),"DMP_Stats-raw.Rdata"))

# generate the standard Knitr reports
standard_reports()
agency_reports()


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
  rmarkdown::render("R\\digital_marketplace_summary.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  rmarkdown::render("R\\agency_report.rmd",output_dir=paste0(getwd(),"\\reports\\"),
                    params = list(agency = "dta.gov.au"))
  rmarkdown::render("scratch\\2018-10-15 - maxrates vs bids.rmd",output_dir=paste0(getwd(),"\\reports\\"))
}

# Use to reload data rather than extract from scratch if restarting a session. 
# Run this directly, it'll be ignored if you 'source' this script 
if (!exists("users")) {
  load(paste0(getwd(),rel_path_data(),latest_data_file()))
  #load(paste0(getwd(),rel_path_data(),as.character(ad()),".Rdata"))
  cns       <- load_austender_cns() %>% post_process_cns
  #j_tickets <- update_jira_tickets()
}

timestamps(ls())
keyring_lock("mkt")
