################################# INITIALISE ENV
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
library(kableExtra)
library(keyring)
library(openxlsx)
library(janitor)

# Source the various R files
assets <- list.files(path = "R", pattern = "\\.r$", full.names = TRUE)
created_assets <- lapply(assets, source)
rm(assets, created_assets)

if (!exists("cns")) {
  cns <- load_austender_cns() %>% post_process_cns
}

############################## Extract the key data files

users          <- extract_users(header(),TRUE) # FALSE = don't include DMP team logins

agencies       <- load_update_agencies(users) # loads agencies and checks for new agencies
# agencies       <- update_agencies(users) # run to do a manual update

buyers         <- process_buyers(users, agencies, FALSE) # TRUE - update the buyer spreadsheet

contributors   <- process_contributors(users)

    s_list     <- extract_sellers(header()) 
sellers        <- s_list$sellers
#seller_domains <- s_list$seller_domains # not greatly useful anymore
    s_list     <- NULL

dmp_sellers    <- extract_vendors_from_panel("SON3413842") # set of sellers on the panel

briefsExtract  <- extract_briefs(header, return_all = TRUE)  
briefs_summary <- extract_briefs_summary() # loaded but not actually used
briefs         <- load_last_briefs() %>% process_briefs(briefsExtract, buyers)

# load the latest extract of briefs responses, and update if more than 7 days old
briefResponses <- load_update_brief_responses(briefs, force_update = FALSE)

# load the latest extract of applications, and update if more than 7 days old
apps           <- load_update_applications(force_update = FALSE)

seller_exceptions <- extract_exceptions() # sellers that have been removed 

contracts      <- extract_austender() %>% 
  process_contracts(sellers,seller_exceptions, agencies)

feedback       <- extract_feedback(header())

# agency_summary <- process_agency_summary(buyers,briefs,contracts)

cns            <- update_contract_summary_notices(cns) %>% post_process_cns()

j_tickets      <- update_jira_tickets()

seller_activity <- generate_last_activity(sellers, apps, users, j_tickets, briefResponses, contracts, dmp_sellers)

generate_austender_update(sellers)

########################################## SAVE THE DATA
# save the extracted data to local drive
save_data()

# update & save the marketplace stats file
load(paste0(getwd(),rel_path_reference(),"DMP_Stats-raw.Rdata"))
marketplace_stats <- generate_summary_stats_table(marketplace_stats)
save(marketplace_stats, file=paste0(getwd(),rel_path_reference(),"DMP_Stats-raw.Rdata"))

########################################## REPORTS
# generate the standard Knitr reports
standard_reports()
agency_reports()


########################################## ad-hoc reports
if (FALSE) {
  rmarkdown::render("R/monthly_report.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/dto_dashboard.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/snapshot.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/transactions.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/pricing_analysis.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/key_facts.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/monthly_performance.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/seller_extract.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/pricing_by_role.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/digital_marketplace_summary.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/agency_report.rmd",output_dir=paste0(getwd(),"/reports/"),
                    params = list(agency = "dta.gov.au"))
  rmarkdown::render("scratch/2018-10-15 - maxrates vs bids.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("scratch/2019-12-11 ANAO data request.rmd",output_dir=paste0(getwd(),"/reports/"))
}

####################################### RELOAD scripts
# Use to reload data rather than extract from scratch if restarting a session. 
# Run this directly, it'll be ignored if you 'source' this script 
if (!exists("users")) {
  load(paste0(getwd(),rel_path_data(),latest_data_file()))
  cns       <- load_austender_cns() %>% post_process_cns
}

timestamps(ls())
keyring_lock("mkt")
