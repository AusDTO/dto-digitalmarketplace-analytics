#
# produce a list of sellers to add to the panel on Austender
# s = sellers data.frame
update_upload_list <- function(s) {
  on_panel <- ((nchar(s$assessed_aoe) > 0) & s$dmp_framework)
  # filter down to all sellers that should be on the panel
  s      <- s[on_panel,]

  # load the list of earlier batches
  gs_sheet <- gs_title("Austender Uploads (and original assessments sheet)")
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
generate_seller_list <- function(sellers) {
  s <- sellers %>% 
    filter(dmp_framework) %>%
    select(name, code, abn, assessed_aoe, unassessed_aoe,sme_by_employees,indigenous,product_only,contact_email) %>%
    mutate(aoe      = paste(assessed_aoe,unassessed_aoe,sep="|"),
           URL = paste0("https://marketplace.service.gov.au/supplier/",code))
           #on_panel = (nchar(assessed_aoe)>1))
  for (i in domains) {
    #strip the commas
    d <- gsub(",","",i)
    d <- gsub("\\s+","_",d)
    s[,d] <- str_detect(s$aoe,i)
  }
  s <- select(s,-assessed_aoe,-unassessed_aoe,-aoe,-code)
  names(s) <-
    c("Seller Name"                            , "ABN"                                    ,
      "SME"                                    , "Indigenous"                             ,
      "Product Only"                           , "Contact Email"                          ,
      "URL"                                    , "Software engineering and Development"   ,
      "User research and Design"               , "Agile delivery and Governance"          ,
      "Content and Publishing"                 , "Strategy and Policy"                    ,
      "Cyber security"                         , "Support and Operations"                 ,
      "Data science"                           , "Emerging technologies"                  ,
      "Marketing Communications and Engagement", "Change and Transformation"              ,
      "Training Learning and Development")   
  return(s)
}

# generates a list of all contacts associated with all sellers on the Marketplace
generate_seller_contact_list <- function(sellers,users) {
  l <- users %>%
    filter(!is.na(seller_id),active) %>% #active users with a seller ID
    select(seller_id,email_address) %>%
    right_join(sellers,by=c("seller_id"="code"))
  unique(c(str_to_lower(l$email_address),
           str_to_lower(l$contact_email),
           str_to_lower(l$auth_rep_email)))
}  

# List of primary contacts for sellers on the Marketplace
# 
#generate_seller_primary_contact_list <- function(sellers,users) {
  
#}

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

# generates a summary of contracts by sellers
generate_contracts_by_seller <- function(contracts,sellers) {
  c <- contracts %>%
    group_by(Supplier.ABN) %>%
    summarise(contracts = n(),
              value     = sum(Value))
  sellers %>%
    select(code,name,abn) %>%
    left_join(c,by=c("abn"="Supplier.ABN")) %>%
    replace_na(list(contracts=0,value=0))
}
  
# Generate contact lists for sellers with documents that are near expiry or have expired
# current notification email goes to both expired and near expired, so just combining the lists
generate_contacts_for_expired_docs <- function(sellers,users) {
  s_expiries <- sellers %>%
    filter(dmp_framework) %>%
    replace_na(list(work_comp_exp = as.Date("2099-12-31"))) %>%
    mutate(rel_date_liability = as.numeric(liability_exp - Sys.Date()),
           rel_date_work_comp = as.numeric(work_comp_exp - Sys.Date()), 
           near_exp_docs      = between(rel_date_liability,0,30)|between(rel_date_work_comp,0,30),
           expired_docs       = rel_date_liability < 0|rel_date_work_comp < 0) %>%
    select(code,name,liability_exp,work_comp_exp,rel_date_liability:expired_docs) %>%
    replace_na(list(near_exp_docs = FALSE,expired_docs = FALSE)) %>% ## There's one seller without docs, but is okay
    mutate(batch = code %% 4 + 1) # split sellers into 4 batches so these are not all sent at once
  
  s_warn <- s_expiries %>%
    filter(near_exp_docs | expired_docs)
  
  contacts <- data.frame(batch = integer(0),email = character(0))
  
  for (i in 1:4) {
    contacts <- rbind(contacts,
                      data.frame(batch = i,
                                 email = generate_seller_contact_list(s_warn[s_warn$batch==i,],users)))
  }
  return(contacts)
}
