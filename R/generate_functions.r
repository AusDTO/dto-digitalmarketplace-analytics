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
#generate_seller_list <- function(sellers) {
#  s <- sellers %>% 
#    filter(dmp_framework) %>%
#    select(name, code, abn, assessed_aoe, unassessed_aoe,sme_by_employees,indigenous,contact_email) %>% #product_only,
#    mutate(aoe      = paste(assessed_aoe,unassessed_aoe,sep="|"),
#           URL = paste0("https://marketplace.service.gov.au/supplier/",code))
#           #on_panel = (nchar(assessed_aoe)>1))
#  for (i in domains) {
#    #strip the commas
#    d <- gsub(",","",i)
#    d <- gsub("\\s+","_",d)
#    s[,d] <- str_detect(s$aoe,i)
#  }
#  s <- select(s,-assessed_aoe,-unassessed_aoe,-aoe,-code)
#  names(s) <-
#    c("Seller Name"                            , "ABN"                                    ,
#      "SME"                                    , "Indigenous"                             ,
#      "Product Only"                           , "Contact Email"                          ,
#      "URL"                                    , "Software engineering and Development"   ,
#      "User research and Design"               , "Agile delivery and Governance"          ,
#      "Content and Publishing"                 , "Strategy and Policy"                    ,
#      "Cyber security"                         , "Support and Operations"                 ,
#      "Data science"                           , "Emerging technologies"                  ,
#      "Marketing Communications and Engagement", "Change and Transformation"              ,
#      "Training Learning and Development")   
#  return(s)
#}

# revised seller list (5/Aug19) - now shows only categories the seller is 
# assessed it. Also needed to add the new categories
generate_seller_list <- function(sellers) {

  s_assess <- sellers %>%
    select(code, assessed_aoe) %>% 
    mutate(assessed_aoe = str_split(assessed_aoe,"\\|")) %>% 
    unnest(assessed_aoe) %>% 
    filter(assessed_aoe != "") %>% 
    mutate(status = "Approved")
    
  s <- sellers %>% 
      filter(dmp_framework) %>%
      select(name, code, abn, sme_by_employees,indigenous, joined, recruiter, contact_email) %>%
      left_join(s_assess, by = "code") %>% 
      replace_na(list(assessed_aoe = "")) %>% 
      spread(assessed_aoe, status, fill = "") %>% 
      select(-V1) %>% # empty extra column added for sellers without any assessed aoe
      mutate(joined = date_to_string(joined))
}

#----------------------------------------------------------------------------------------------------------------
# generates a list of all contacts associated with all sellers on the Marketplace
generate_seller_contact_list <- function(s,u) {
  l <- u %>%
    filter(!is.na(seller_id),active) %>% #active users with a seller ID
    select(seller_id,email_address) %>%
    right_join(s,by=c("seller_id"="code"))
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

# generate a list of last activity dates for a set of sellers
generate_last_activity <- function(sellers, apps, users, j_tickets, briefResponses, contracts, dmp_sellers) {
  last_edits <- apps %>% 
    filter(type == "edit", !is.na(submitted_at_date)) %>% 
    group_by(supplier_code) %>% 
    summarise(last_edit = max(submitted_at_date))
  
  last_logins <- users %>% 
    filter(role == "supplier") %>% 
    group_by(seller_id) %>% 
    summarise(last_login = max(logged_in_at))
  
  last_br <- briefResponses %>% 
    group_by(supplierId) %>% 
    summarise(responses     = n(),
              last_response = max(applicationDate))
  
  last_assessment <- j_tickets %>% 
    filter(type == "Domain Assessment") %>% 
    group_by(seller_id) %>% 
    summarise(last_domain_assessment_request = as.Date(max(created))) %>% 
    mutate(seller_id = as.integer(seller_id))
  
  last_contract <- contracts %>% 
    group_by(code) %>% 
    summarise(contract_count = n(),
              last_contract  = max(Publish.Date))
  
  s <- sellers %>% 
    mutate(signed_agreement_date = dmy(signed_agreement_date),
           currently_on_panel    = abn %in% dmp_sellers$ABN,
           eligible_panel_member = if_else(nchar(assessed_aoe) > 0, "YES", "NO"),
           eligible_to_respond   = if_else(
                                     (liability_exp >= Sys.Date()) & 
                                     (work_comp_exp >= Sys.Date()) & 
                                     (eligible_panel_member == "YES") &
                                     (signed_agreement_date >= as.Date("2019-07-01")),
                                     "YES",
                                     "NO"
                                   )) %>%  
    select(code, name, joined, eligible_to_respond, currently_on_panel, eligible_panel_member, liability_exp, work_comp_exp, signed_agreement_date) %>%
    left_join(last_edits,  by = c("code" = "supplier_code")) %>% 
    left_join(last_logins, by = c("code" = "seller_id")) %>% 
    left_join(last_br    , by = c("code" = "supplierId")) %>% 
    left_join(last_assessment, by = c("code" = "seller_id")) %>%   
    left_join(last_contract, by = "code") %>% 
    arrange(code)
  
  #date_cols <- sapply(s, class)
  #s$latest_activity <- as.Date(
  #                       apply(s[,which(date_cols == "Date")], 1, function(x) max(x, na.rm = TRUE)), 
  #                       origin = "1970-01-01"
  #                     )
  s$latest_activity <- as.Date(
                         apply(s[,c(3,8,9,10,12,13,15)], 1, function(x) max(x, na.rm = TRUE)), 
                         origin = "1970-01-01"
                       )
  s
}

###############################
# Generate summary seller stats
# reads the seller and contracts log files to produce a historical record
# of seller stats
# call with 'current = null' for a full refresh
generate_summary_stats_table <- function(current = summary_stats) {

  log_files   <- tibble(files = list.files(path = paste0(getwd(),rel_path_data()),
                                           pattern = "^\\d{4}\\-\\d{2}\\-\\d{2}")) %>% 
    mutate(date = as.Date(str_sub(files, 1, 10))) 
  
  if (!is.null(current)) {
    log_files <- filter(log_files, !date %in% current$date)
  }
  
  extract_seller_summaries <- function(log_name) {
    s <- read.csv(paste0(getwd(),rel_path_data(),log_name), stringsAsFactors = FALSE)
    s_table <- tibble(date = str_sub(log_name, 1, 10)) %>% 
      mutate(`Registered sellers`      = nrow(s),
             `Registered sellers SMEs` = sum(s$sme_by_employees),
             `Percent registered SMEs` = round(`Registered sellers SMEs` / `Registered sellers` * 100, digits = 1),
             `Panel members`           = sum(nchar(s$assessed_aoe) > 1),
             `Panel members SMEs`      = sum(nchar(s$assessed_aoe) > 1 & s$sme_by_employees),
             `Percent panelists SMEs`  = round(`Panel members SMEs`/`Panel members` * 100, digits = 1))
  }
  
  seller_logs <- log_files %>% filter(str_detect(files, "\\d{2}\\-[Ss]ellers\\.csv")) %>% pull(files)
  # check if any files to update
  if (length(seller_logs) == 0) {
    return(current)
  }
  s_summaries <- bind_rows(lapply(seller_logs, extract_seller_summaries))
  s_summaries$date <- as.Date(s_summaries$date)

  contract_logs <- log_files %>% filter(str_detect(files, "\\d{2}\\-[Cc]ontracts\\.csv")) %>% pull(files)
  extract_contract_summaries <- function(log_name) {
    c <- read.csv(paste0(getwd(),rel_path_data(),log_name), stringsAsFactors = FALSE)
    c_table <- tibble(date = str_sub(log_name, 1, 10)) %>% 
      mutate(`Total spend`      = sum(c$Value),
             `Total spend SMEs` = sum(filter(c, sme_by_employees)$Value),
             `Percent SME spend`= round(`Total spend SMEs`/`Total spend`*100, digits = 1))
  }
  c_summaries <- bind_rows(lapply(contract_logs, extract_contract_summaries))
  c_summaries$date <- as.Date(c_summaries$date)
  
  summary <- s_summaries %>% 
    full_join(c_summaries, by = "date")
  
  if (!is.null(current)) {
    bind_rows(current, summary)
  } else {
    summary
  }
}

generate_name_differences_to_panel <- function(sellers) {
  dmp_s  <- extract_vendors_from_panel("SON3413842")
  diffs <- dmp_s %>% 
    left_join(sellers, by = c("ABN" = "abn")) %>% 
    select(ABN, Name, name, code) %>% 
    filter(Name != name) %>% 
    rename(dmp_name = name,
           aus_name = Name) %>% 
    mutate(url = paste0("https://abr.business.gov.au/Search/ResultsActive?SearchText=",ABN))
}
