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

# Source the various R files
assets <- list.files(path = "R", pattern = "\\.r$", full.names = TRUE)
created_assets <- lapply(assets, source)
rm(assets, created_assets)

if (!exists("header")) {
  header <- prompt_auth()
}
if (!exists("cns")) {
  cns <- load_austender_cns()
}

timestamp      <- Sys.time()

users          <- extract_users(header,TRUE) # FALSE = don't include DMP team logins
buyers         <- process_buyers(users, FALSE) # TRUE - update the buyer spreadsheet
contributors   <- process_contributors(users)
    s_list     <- extract_sellers(header) 
sellers        <- s_list$sellers
case_studies   <- s_list$case_studies
    s_list     <- NULL
briefsExtract  <- extract_briefs(header,FALSE)
briefs         <- process_briefs(briefsExtract,buyers,FALSE)
mkt_briefs     <- update_brief_details(briefs)
briefResponses <- extract_brief_responses(header) %>% process_brief_responses(briefs)
apps           <- extract_applications(header)
assessments    <- extract_assessments(header)
all_sellers    <- process_sellers_and_applications(sellers,apps,users)
contracts      <- extract_austender() %>% process_contracts(sellers)
feedback       <- extract_feedback(header)
agency_summary <- process_agency_summary(buyers,briefs,contracts)
cns            <- update_contract_summary_notices(cns)
#sellerMailList <- process_seller_email_list(sellers,contributors,apps)

# write useful observations to a log  
log_current_observations(timestamp)

# save the extracted data to local drive
save_data()

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
  rmarkdown::render("scratch\\2018-10-15 - maxrates vs bids.rmd",output_dir=paste0(getwd(),"\\reports\\"))
}

# Use to reload data rather than extract from scratch if restarting a session. 
# Run this directly, it'll be ignored if you 'source' this script 
if (!exists("users")) {
  load(paste0(getwd(),rel_path_data(),substr(as.POSIXct(Sys.time()),1,10),".Rdata"))
}

timestamps(ls())
