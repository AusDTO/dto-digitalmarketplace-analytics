## Generate functions
# Set of functions used to generate various data objects

#--------------------------------------------------------------------------------------------------------------------------------------------

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

#--------------------------------------------------------------------------------------------------------------------------------------------
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

#----------------------------------------------------------------------------------------------------------------
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


#----------------------------------------------------------------------------------------------------------------
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

#----------------------------------------------------------------------------------------------------------------
# Generate contact lists for sellers with documents that are near expiry or have expired
# current notification email goes to both expired and near expired, so just combining the lists
#
# For manual preparation, should this be needed. Notifications for this are now generated automatically
# by the DMP platform
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


#--------------------------------------------------------------------------------------------------------------------
# Generate a list of sellers who need to be added to Austender. 
# Writes the files to the reporting directory
# Note that execution will be interrupted if there are any exceptions. This is only to draw attention to the 
# exception.
generate_austender_update <- function(sellers) {
  dmp_s  <- extract_vendors_from_panel("SON3413842")
  s      <- sellers %>%
    filter(dmp_framework, nchar(assessed_aoe) > 1)
  s_except <- dmp_s %>%
    filter(!ABN %in% s$abn)
  if (nrow(s_except) > 0) {
    print("DMP sellers are on Austender who are not in the current seller list")
    View(s_except)
    writey(s_except,"Austender Exceptions")
    readline("Hit enter to continue ...")
  }
  s      <- s %>%
    filter(!abn %in% dmp_s$ABN) %>%
    mutate(abn_exempt = "no") %>%
    select(name, address_line, suburb, postal_code, state, country, abn_exempt, abn)
  states  <- s$state
  s$state <- translate_state_names(s$state)
  if (sum(is.na(s$state)) > 0) {
    print("The following state names can't be translated. (assorted_functions.r:247)")
    print(states[is.na(s$state)])
    readline("Hit enter to continue ...")
  }
  s$state[is.na(s$state)] <- states[is.na(s$state)]
  writer(s,"sellers-to-add-to-austender")
}
