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
  s$date       <- date_to_string(Sys.Date())
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

# function to inspect all live briefs that are open to selected or to 1
# returns the sellers that have been invited, as well as information about
# the seller's status
invited_sellers <- function(briefs,briefResponses) {
  br <- briefs %>% filter(status == "live",openTo %in% c("Some","One"))
  if (dim(br)[1]==0) {
    return(data.frame(id="No open to 1/selected briefs"))
  }
  # fetch the seller details for an individual brief
  fetchInvitedSellers <- function(id) {
    #bQuery <- paste("https://dm-api.apps.platform.digital.gov.au/api/briefs",as.character(id),sep="/")
    bQuery <- prod_api(paste0("briefs/",as.character(id)))
    bRaw   <- fetchFromAPI(bQuery,header)
    emails = bRaw$sellerEmailList
    if (bRaw$sellerSelector=="oneSeller") {
      emails = bRaw$sellerEmail
    }
    if (length(emails)==0) {
      emails <- "**missing**@**missing**"
    }
    if (!valid_email_address(emails)) {
      emails <- "**invalid**@**invalid**"
    }
    df        <- data.frame(id=id,emails=emails,stringsAsFactors = FALSE)
    df$closes <- as.Date(bRaw$dates$closing_date)
    if (is.null(bRaw$areaOfExpertise)) {
      df$aoe <- NA
    } else {
      df$aoe    <- bRaw$areaOfExpertise
    }
    return(df)
  }  

  invitees                   <- bind_rows(lapply(br$id,fetchInvitedSellers))
  invitees$email_domain      <- matrix(unlist(strsplit(invitees$emails,"@")),ncol=2,byrow=TRUE)[,2]
  invitees$seller_registered <- invitees$email_domain %in% sellers$email_domain
  invitees$user_registered   <- invitees$emails %in% users$email_address
  invitees$is_contact        <- invitees$emails %in% sellers$contact_email
  aoes                       <- sellers[,c("code","email_domain","assessed_aoe")]
  invitees                   <- merge(invitees,aoes,by.x="email_domain",by.y="email_domain")
  invitees$assessed          <- FALSE
  for (i in 1:dim(invitees)[1]) {
    invitees[i,"assessed"] <- grepl(invitees[i,"aoe"],invitees[i,"assessed_aoe"])
  }
  invitees[is.na(invitees$assessed),"assessed"] <- 
    nchar(invitees[is.na(invitees$assessed),"assessed_aoe"]) > 2
  invitees$assessed_aoe      <- NULL
  x <- paste(invitees$id,invitees$code,sep="|")
  y <- paste(briefResponses$id,briefResponses$supplierId,sep="|")
  invitees$has_applied       <- x %in% y
  return(invitees[order(invitees$closes),])
}

# generate a list of all DMP sellers that can be shared with buyers
create_seller_list <- function(sellers) {
  s <- sellers %>% 
    filter(dmp_framework,!product_only) %>%
    select(code, name, abn, assessed_aoe, unassessed_aoe,sme_by_employees,indigenous) %>%
    mutate(aoe      = paste(assessed_aoe,unassessed_aoe,sep="|"),
           on_panel = (nchar(assessed_aoe)>1))
  for (i in domains) {
    #strip the commas
    d <- gsub(",","",i)
    d <- gsub("\\s+","_",d)
    s[,d] <- str_detect(s$aoe,i)
  }
  select(s,-assessed_aoe,-unassessed_aoe,-aoe)
}

# extract the sellers that are approved to join the Digital Marketplace panel
sellers_approved_for_dmp <- function(sellers) {
  sellers %>%
    filter(dmp_framework) %>%
    filter(nchar(assessed_aoe)>0)
}

# filter to the 
filter_buyers <- function(buyers,includeDTA = FALSE, includeDeactivated = FALSE) {
  b <- buyers
  if (!includeDTA) {
    b <- b %>%
      filter(!grepl("\\+",email_address))
  }
  if (!includeDeactivated) {
    b <- b %>% filter(active)
  }
  return(b)  
}

