library(dplyr)
library(plotly)
library(jsonlite)
library(httr)
library(googlesheets)
library(knitr)
library(RColorBrewer)

# Source the various R files
assets <- list.files(path = "R", pattern = "\\.r$", full.names = TRUE)
created_assets <- lapply(assets, source)
rm(assets, created_assets)

if (!exists("header")) {
  header <- prompt_auth()
}

users          <- extract_users(header,TRUE) # FALSE = don't include DMP team logins
buyers         <- process_buyers(users, FALSE) # TRUE - update the buyer spreadsheet
contributors   <- process_contributors(users)
sellers        <- extract_sellers(header) #%>% process_sellers()
briefsExtract  <- extract_briefs(header,FALSE)
briefs         <- process_briefs(briefsExtract,buyers,FALSE)
briefResponses <- extract_brief_responses(header) %>% process_brief_responses(briefs)
apps           <- extract_applications(header)
assessments    <- extract_assessments(header)
all_sellers    <- process_sellers_and_applications(sellers,apps,users)
contracts      <- extract_austender() %>% process_contracts(sellers)
sellerMailList <- process_seller_email_list(sellers,contributors,apps)

save_data()

standard_reports()

## ad-hoc reports
if (FALSE) {
  rmarkdown::render("R\\monthly_report.rmd",output_dir=paste0(getwd(),"\\reports\\"))
}

# Use to reload data rather than extract from scratch if restarting a session. 
# Run this directly, it'll be ignored if you 'source' this script 
if (!exists("users")) {
  load(paste0(getwd(),"/data/",substr(as.POSIXct(Sys.time()),1,10),".Rdata"))
}

