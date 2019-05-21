# process functions

# processes the users table to extract the buyers, get agency data and update the buyers spreadsheet 
# still to add:
# - inline updates to the reference domains, rather than raise an error
# - add the date the buyer will go, or went inactive
process_buyers <- function(u,update = FALSE) {
  b             <- u[u$role=="buyer",]
  missing_domains <- "foo"
  while(length(missing_domains) > 0) {
    agencies      <- gs_title("ref-domains") %>% gs_read(ws = "agencies")
    # check if all agency domains names are present in the agencies df
    e_domains       <- unique(b$email_domain)
    missing_domains <- e_domains[!e_domains %in% agencies$email_domain]
    if (length(missing_domains) > 0) {
      print(paste0("Missing domains ",missing_domains))
      readline("Enter details in the ref-domains sheet and hit enter to continue")
    }
  }
  b            <- merge(b,agencies,by.x="email_domain",by.y="email_domain",all.x = TRUE)
  # legacy missing check - leaving in place, but should not be triggered any more
  missing      <- unique(b[is.na(b$category),]$email_domain)
  b[b$category=="Commonwealth"&!is.na(b$D10),"category"] <- "Commonwealth-D10"
  if (length(missing) > 0) {
    print("Missing agency domains - add the following details and run again")
    print(missing)
    stop()
  }
  # filter the inactive buyers
  # b <- b[b$active,]
  attr(b,"timestamp") <- Sys.time()
  if (update) {
    write_new_sheet("RegisteredBuyers",b)  
  }
  return(b)
}

process_sellers <- function(sellers) {
  write_new_sheet("MarketplaceSellers",sellers, latest_only = TRUE)
  return(sellers)
}

process_briefs <- function(b,buy,update_gs=TRUE) {
  
  # fetch the buyer details for an individual brief
  fetchBuyer <- function(i,df) {
    #print(which(df$id==i))
    row = which(df$id==i)
    #bQuery <- paste("https://dm-api.apps.platform.digital.gov.au/api/briefs",as.character(i),sep="/")
    bQuery <- prod_api(paste0("briefs/",as.character(i)))
    bRaw   <- fetchFromAPI(bQuery,header())
    #print(bRaw$users)
    return(c(bRaw$users$id[1],bRaw$users$name[1],bRaw$users$emailAddress[1]))
  }
  
  # adds a column with the name of the role where this is one of the frequent_roles
  add_role_name_to_brief <- function(briefs) {
    # calc edit distances for a role name
    edit_distances <- function()
    
    distances <- lapply(frequent_roles$search,edit_distances)
  }
  
  #b$published <- date_to_string(b$published)
  #b$created   <- date_to_string(b$created) 
  sheet_name  <- 'allBriefs'
  #briefPresentationFilter <- c("id","status","title","organisation","openTo",
  #                             "type","areaOfExpertise","budgetRange","contractLength",
  #                             "created","published","duration","frameworkFramework","phase",
  #                             "buyerID","buyerName",	"buyerEmail")
  ## Update the briefs Google Sheet
  briefsSheet <- gs_title(sheet_name)
  # identify the last updated worksheet
  ws          <- briefsSheet %>% gs_ws_ls()
  temp        <- as.Date(ws)
  temp        <- temp[!is.na(temp)]
  lastUpdate  <- max(temp)
  # read the last worksheet
  lastBriefs  <- briefsSheet %>% gs_read(ws = as.character(lastUpdate))
  sc          <- c("id","buyerID","buyerName","buyerEmail")
  lastBriefs  <- lastBriefs[,sc]
  # merge the additional columns
  updatedBriefs <- merge(b,lastBriefs,by.x="id",by.y="id",all.x=TRUE)
  
  # some seriously ugly code here, but it works ...
  if (sum(is.na(updatedBriefs$buyerID) > 0)) {
    x <- lapply(updatedBriefs[is.na(updatedBriefs$buyerID),"id"],fetchBuyer,df = updatedBriefs)
    buyer_users              <- matrix(data=unlist(x),ncol=3,byrow=TRUE)
    updatedBriefs[is.na(updatedBriefs$buyerID),"buyerID"]       <- as.numeric(buyer_users[,1])
    updatedBriefs[is.na(updatedBriefs$buyerName),"buyerName"]   <- buyer_users[,2]
    updatedBriefs[is.na(updatedBriefs$buyerEmail),"buyerEmail"] <- buyer_users[,3]
    #updatedBriefs[is.na(updatedBriefs$updated),"updated"] <- updatedBriefs[is.na(updatedBriefs$updated),"updatedAt"]
  }
  buy <- buy[,c("id","email_domain","category","agencyName","reports","entities")]
  updatedBriefs <- left_join(updatedBriefs,buy,by=c("buyerID" = "id"))
  attr(updatedBriefs,"timestamp") <- Sys.time()
    # write back to the spreadsheet
  if (update_gs) {
    write_new_sheet(sheet_name,updatedBriefs,is_verbose = TRUE)
  }
  return(updatedBriefs)
}

# merges briefs and responses. 
process_brief_responses <- function(responses,briefs, update_gs=FALSE) {
  df <- merge(briefs,responses,by.x="id",by.y="briefId",all.x=TRUE)
  attr(df,"timestamp") <- Sys.time()
  if (update_gs) {
    #write_new_sheet("BriefResponses",df,TRUE)
  }
  return(df)
}

process_contributors <- function(users) {
  return (users[users$active&!is.na(users$seller_id),])
}

# function to write a new sheet with today's date to a Google sheet
# if the worksheet already exists, the older version will be updated
# The LATEST sheet will also be updated
# MAKE SURE THE NEW DATA IS SAME OR GREATER SIZE ##
write_new_sheet <- function(sheet_name,contents,latest_only=FALSE,is_verbose=FALSE) {
  gs_sheet <- gs_title(sheet_name)
  if (!latest_only) {
    # identify the last updated worksheet
    ws          <- gs_sheet %>% gs_ws_ls()
    temp        <- as.Date(ws[grepl("\\d{4}\\-\\d{2}\\-\\d{2}",ws)])
    #temp        <- temp[!is.na(temp)]
    lastUpdate  <- max(temp)
    today       <- Sys.Date()
    if (today == lastUpdate) {
      gs_sheet %>% 
        gs_edit_cells(ws = as.character(Sys.Date()), 
                      input = contents, anchor = "A1", 
                      byrow = TRUE, verbose=is_verbose)
    } else {
      gs_sheet %>% 
        gs_ws_new(ws_title = as.character(Sys.Date()), input = contents,
                  trim = TRUE, verbose = is_verbose)
    }
  }
  gs_sheet %>% gs_edit_cells(ws = "LATEST", input = contents, anchor = "A1", byrow = TRUE)
}

# version 1 - for use just post upgrade to M2.0
# returns a list of seller contacts for EDMs
# if dmp_framework == TRUE, returns seller contacts on dmp_framework, else, all other seller contacts
process_seller_email_list_XXX <- function(sellers,contributors,dmp_framework) {
  sell <- sellers[
              sellers$dmp_framework==dmp_framework&!sellers$product_only,
              c("code","name","contact_email")
              ]
  sell <- merge(sell,contributors[!is.na(contributors$seller_id),],by.x="code",by.y="seller_id",all.x=TRUE)
  return(
    unique(
      c(
        sell$contact_email,
        sell[!is.na(sell$email_address),"email_address"]
        )
      )
    )
}

# version 2 - 
# use sellers, users, applications
# list includes all users for DMP framework sellers, except if product only
# Also includes legacy sellers whose profiles are in submitted or reverted status
# 6/2/18 - stop including DSPP sellers who are still upgrading
process_seller_email_list <- function(s,u,a) {
  # only active users
  u <- u[u$active,]
  # identify the legacy sellers still in submitted or reverted state
  #codes <- a[!is.na(a$supplier_code)&a$status %in% c("reverted","submitted")&a$type=="upgrade",]$supplier_code
  #upgrading <- s[s$code %in% codes,c("code","name","contact_email")]
  active    <- s[
                  s$dmp_framework==TRUE&!s$product_only,
                  c("code","name","contact_email")
                ]
  #all  <- rbind(upgrading,active)
  all <- active
  sell <- merge(all,u[!is.na(u$seller_id),],by.x="code",by.y="seller_id",all.x=TRUE)
  return(
    unique(
      c(
        sell$contact_email,
        sell[!is.na(sell$email_address),"email_address"]
      )
    )
  )
}

# combines sellers and applicants into a single DF
process_sellers_and_applications <- function(s,a,u) {
  s_columns <- c("name","code","abn","application_id","assessed_aoe","unassessed_aoe","legacy_aoe","creation_time","website",
            "contact_email","product_only","dmp_framework")
  s         <- s[,s_columns]
  names(s)  <- paste("seller",s_columns,sep="_")
  # remove the edits from the applications
  a <- a[a$type != "edit",]
  # reduce the applications down to key columns
  a_columns <- c("id","name","abn","supplier_code","status","contact_email","is_recruiter","created_at","submitted_at")
  a         <- a[,a_columns]
  # change status from saved to reverted if they've submitted before
  a[(a$status=="saved")&!is.na(a$submitted_at),"status"] <- "reverted"
  names(a)  <- paste("app",a_columns,sep="_")
  # find the user who (might have) created the application
  u_ref        <- users[!is.na(users$application_id),c("application_id","email_address")]
  u_ref        <- u_ref[!duplicated(u_ref$application_id),]
  names(u_ref) <- c("app_id","app_email_address")
  a            <- merge(a,u_ref,by.x="app_id",by.y="app_id",all.x=TRUE)
  # new sellers don't have an application ID in the seller catalogue, so use ABN
  all <- merge(s,a,by.x="seller_name",by.y="app_name",all = TRUE)
  #all[!is.na(all$app_email_address),]$app_domain <- 
  #  matrix(
  #    unlist(
  #      strsplit(all[!is.na(all$app_email_address),"app_email_address"],"@")
  #    )
  #    ,ncol=2,byrow=TRUE
  #  )[,2]
  
  return(all)
}

# merge contracts and seller details
process_contracts <- function(contracts,sellers,exceptions) {

  total <- sum(contracts$Value)
  
  s <- bind_rows(sellers,exceptions)
  
  contracts <- contracts %>%
    left_join(s,by=c("Supplier.ABN" = "abn"))
  
  attr(contracts,"timestamp") <- Sys.time()

  # check for duplicates  
  if (total != sum(contracts$Value)) {
    cat(paste(total,"versus",sum(contracts$Value)),"\n")
    cat("Duplicated contract notices: ")
    contracts %>% filter(duplicated(CN.ID)) %>% select(CN.ID) %>% print()
    stop("Value mismatch in contracts. Suspected duplicate seller")
  }
    
  return(contracts)
}

# generates a list of contact emails for a given set of sellers
# use a filtered list of sellers, but the full set of users
process_seller_contact_list <- function(sellers,users) {
  l <- users %>%
    filter(!is.na(seller_id),active) %>% #active users with a seller ID
    select(seller_id,email_address) %>%
    right_join(sellers,by=c("seller_id"="code"))
  unique(c(str_to_lower(l$email_address),
           str_to_lower(l$contact_email),
           str_to_lower(l$auth_rep_email)))
}  

# produce a summary file of agencies and their activity
process_agency_summary <- function(buyers,briefs,contracts,
                                   date_from = as.Date("2016-01-01"),date_to = Sys.Date()) {
  # filter the briefs & contracts data to the date range
  briefs    <- briefs    %>% filter(published >= date_from,published <= date_to)
  contracts <- contracts %>% filter(Publish.Date >= date_from,Publish.Date <= date_to)
  
  # optionally map the values in v to the mapped value in mappings
  # mappings has columns 
  map_values <- function(v,mappings) {
    for(i in 1:nrow(mappings)) {
      v[which(v == mappings[i,]$from)] <- mappings[i,]$to
    }
    v
  }
  
  ref_d         <- gs_title("ref-domains")
  ref_domain    <- ref_d  %>% gs_read(ws = "agencies")
  exceptions    <- ref_d  %>% gs_read(ws = "exceptions")
  name_map      <- ref_domain %>%
    filter(!is.na(austender_name)) %>%
    select(agencyName,austender_name)
  names(name_map) <- c("from","to")
  ref_domain[is.na(ref_domain)] <- ""
  
  b <- buyers
  b$email_domain <- map_values(b$email_domain,exceptions)
  b$agencyName   <- map_values(b$agencyName,exceptions)
  b$sector       <- map_values(b$sector,exceptions)
  agencies <- b %>%
    group_by(agencyName,sector,entities,email_domain,state) %>%
    summarise(no_registered_buyers = n(),
              first_joined = min(created_at))
  
  br <- briefs
  br$email_domain  <- map_values(br$email_domain,exceptions) 
  b_summ <- br %>%
    group_by(email_domain) %>%
    summarise(briefs_published = n(),
              first_published  = min(published),
              last_published   = max(published))
  agencies <- agencies %>%
    left_join(b_summ, by="email_domain")
  agencies$briefs_published[is.na(agencies$briefs_published)] <- 0
  agencies$state[is.na(agencies$state)] <- ""
  agencies$agencyName <- map_values(agencies$agencyName,name_map)
  
  c        <-  contracts
  c$Agency <-  map_values(c$Agency,exceptions)
  agency_contracts <- c %>% 
    group_by(Agency) %>%
    summarise(no_of_contracts = n(),
              total_value     = sum(Value),
              max_value       = max(Value),
              median_value    = median(Value),
              mean_value      = mean(Value),
              min_value       = min(Value),
              earliest_contract = min(Publish.Date),
              latest_contract   = max(Publish.Date)) %>%
    arrange(desc(no_of_contracts))
  
  agency_summary <- full_join(agencies,agency_contracts,by=c("agencyName"="Agency"))
  x <- agency_summary[,11:16]
  x[is.na(x)] <- 0
  agency_summary[,11:16] <- x
  attr(agency_summary,"timestamp") <- Sys.Date()
  return(agency_summary)
}


process_jira_tickets <- function(j_tickets) {
  jt <- j_tickets %>%
    filter(type != "Supplier Assessment Step") %>% # don't know what these are, but don't look useful
    # some edit records don't have seller_id - so recover them from the summary field
    mutate(summary_id = str_match(summary, "\\(\\#(\\d+)\\)")[,2]) %>%
    mutate(seller_id = case_when(
                                  type == "Domain Assessment" & is.na(seller_id) ~ summary_id,
                                  TRUE                                           ~ seller_id
                                )) %>%
    select(-summary_id)
}


