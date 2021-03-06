# saves all of the main data.frames locally

save_data <- function() {
  # as Rdata
  objects_list <- ls(envir=.GlobalEnv)
  objects_list <- objects_list[objects_list 
                               %in% 
                               qw("apps users sellers buyers briefResponses briefs 
                                   assessments contracts feedback agencies
                                   j_tickets")
                               ]
  save(list = objects_list,
       file = paste0(getwd(),rel_path_data(),substr(as.POSIXct(Sys.time()),1,10),".Rdata"))
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
  #writer(agency_summary,"agency-summary")
  writer(generate_seller_list(sellers),"sellers")
  #writer(mkt_briefs,"marketplace_briefs")
  #writer(panels,"list_of_sons")
  writer(seller_activity, "seller_activity")
}

save_to_db <- function() {
  if (!exists("con")|!dbIsValid(con)) {
    con <- db_connect_local()
  }
  db_append_dmp_contracts(con,contracts)
}



