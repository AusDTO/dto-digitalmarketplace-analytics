# saves all of the main data.frames locally

save_data <- function() {
  # as Rdata
  objects_list <- ls(envir=.GlobalEnv)
  objects_list <- objects_list[objects_list 
                               %in% 
                               qw("apps users sellers buyers briefResponses briefs 
                                   assessments contracts feedback all_sellers case_studies
                                  agency_summary")
                               ]
  save(list = objects_list,
       file = paste0(getwd(),rel_path_data,substr(as.POSIXct(Sys.time()),1,10),".Rdata"))
  save_individual <- function(obj_name) {
    get(obj_name,envir=.GlobalEnv) %>% writey(obj_name)
  }
  x <- sapply(objects_list,save_individual)
  
  # save some of the common files to the reports directory also
  writer(feedback,"feedback")
  writer(briefs,"briefs")
  contracts %>% 
    select(Agency:Value,code,name,is_recruiter,indigenous,sme_by_employees) %>%
    writer("contracts")
  writer(agency_summary,"agency-summary")
  
}



