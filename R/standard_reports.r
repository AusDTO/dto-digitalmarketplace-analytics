standard_reports <- function() {
  dir <- getwd()
  rmarkdown::render("R\\snapshot.rmd",output_dir=paste0(getwd(),"\\reports\\"))
  rmarkdown::render("R\\transactions.rmd",output_dir=paste0(getwd(),"\\reports\\"))
}