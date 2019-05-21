standard_reports <- function() {
  dir <- getwd()
  rmarkdown::render("R\\snapshot.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  rmarkdown::render("R\\transactions.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  #rmarkdown::render("R\\key_facts.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  #rmarkdown::render("R\\pricing_analysis.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  #rmarkdown::render("R\\monthly_performance.rmd",output_dir=paste0(getwd(),"\\reports\\"))
}