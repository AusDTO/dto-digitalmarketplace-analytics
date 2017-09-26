#
# produce a list of sellers to add to the panel on Austender
# s = sellers data.frame
update_upload_list <- function(s) {
  on_panel <- ((nchar(s$assessed_aoe) > 0) & s$dmp_framework)
  # filter down to all sellers that should be on the panel
  s      <- s[on_panel,]

  # load the list of earlier batches
  gs_sheet <- gs_title("DMP Seller Reviews Worksheet")
  ws       <- gs_sheet %>% gs_read(ws = "Austender Batches")
  
  s <- s[!(s$abn %in% ws$abn),]
  if (dim(s)[1] == 0) {
    return("No updates required")
  }
  
  # Create new batch and write back to the spreadsheet
  s$batch <- max(ws$batch) + 1
  s$date       <- Sys.Date()
  s$abn_exempt <- "no"
  updated <- rbind(s[,names(ws)],ws)
  updated <- updated[order(updated$batch),]
}


generate_regional_postcode_list <- function() {
  postcodes <- read.csv("regional_postcodes.csv",stringsAsFactors = FALSE)
  postcodes$X <- NULL
  names(postcodes) <- c("postcode","regional")
  # remove the many duplicates
  postcodes <- postcodes[!duplicated(postcodes$postcode),]

  #add the zero's for NT postcodes
  postcodes[grepl("^\\d{3}$",postcodes$postcode),"postcode"] <- paste("0",postcodes[grepl("^\\d{3}$",postcodes$postcode),"postcode"],sep="")

  # and any corrections
  postcodes[which(postcodes$postcode=="0872"),"regional"] <- "Yes"
  not_recognised <- as.character(c(4052, 6113, 6201, 6202, 6203, 6205, 2301, 2407, 4813, 3693, 6444, 6752, 4857, 4219, 6085, 2091))
  postcodes <- postcodes %>% filter(!is.na(postcode),!(postcode %in% not_recognised))
  #postcodes[!postcodes$postcode %in% not_recognised,]

  writey(postcodes,"regions_by_postcode")
}