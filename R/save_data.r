# saves all of the main data.frames locally

save_data <- function() {
  # as Rdata
  save(apps,users,sellers,buyers,briefResponses,briefs,assessments,contracts,
       file=paste0("data\\",substr(as.POSIXct(Sys.time()),1,10),".Rdata"))
  # as csv
  writey(briefResponses,"BriefResponses")
  writey(buyers,"Buyers")
  writey(briefs,"Briefs")
  writey(sellers,"Sellers")
  writey(apps,"Applications")
  writey(all_sellers,"All_Sellers")
  writey(contracts,"Contracts")
  writey(sellerMailList,"SellerMailingList",includeHeader=FALSE)
}