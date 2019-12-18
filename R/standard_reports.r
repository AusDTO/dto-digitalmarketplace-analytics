standard_reports <- function() {
  dir <- getwd()
  rmarkdown::render("R/snapshot.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/transactions.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/digital_marketplace_summary.rmd",output_dir=paste0(getwd(),"/reports/"))
  rmarkdown::render("R/monthly_performance.rmd",output_dir=paste0(getwd(),"/reports/"))
}

agency_reports <- function() {
  reports_to_run <- tibble(
    agency           = c("dta.gov.au,digital.gov.au",
                         "defence.gov.au,dst.defence.gov.au",
                         "homeaffairs.gov.au,border.gov.au",
                         "humanservices.gov.au"),
    agency_name      = c("Digital Transformation Agency",
                         "Dept of Defence and Related Agencies",
                         "Home Affairs",
                         "Dept of Human Services"),
    agency_aust_name = c("Digital Transformation Agency",
                         "Department of Defence,Australian Signals Directorate",
                         "Department of Home Affairs",
                         "Department of Human Services"),
    report_name      = c("DTA",
                         "defence",
                         "home_affairs",
                         "human_services")
  )
  for (i in 1:nrow(reports_to_run)) {
    rmarkdown::render("R/agency_report.rmd",
                      output_dir=paste0(getwd(),"/reports/"),
                      output_file=paste0("Agency_Report_",
                                         reports_to_run[i,]$report_name,
                                         ".html"),
                      params = list(
                        agency           = reports_to_run[i,]$agency,
                        agency_name      = reports_to_run[i,]$agency_name,
                        agency_aust_name = reports_to_run[i,]$agency_aust_name
                      )
    )
  }  
}
