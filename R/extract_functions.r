# extract scripts - various functions for reading from the DMP APIs
# all return simple tables for further analysis

#-------------------------------------------------------------------------------------------
# extract all registered users, and remove those that are inactive
# this includes both buyers and sellers
extract_users <- function(header,include_dta_users = FALSE, cache = NULL) {
  user_column_names <- c("id","seller_id","application_id","name","email_address","active",
                         "role","locked","created_at","logged_in_at","frameworks")
  
  # reprocess users into a data frame without embedded lists
  process_users <- function(df) {
    extract_framework <- function(x) {
      if (is.null(nrow(x))) {return(NA)}
      if (nrow(x)==0) {
        return(NA)
      }
      if (nrow(x) > 1) {
        print("user with > 1 framework")
      }
      return(x[1,]$framework_id)
    }
    # exclude 'applicants' for now
    users           <- df[df$role %in% c("buyer","supplier","admin","applicant"),]
    if (!is.null(users$frameworks)) {
      users$frameworks <- sapply(users$frameworks,extract_framework)
    }
    # this can be entirely NAs, which will cause an error
    if (is.vector(users$supplier)) {
      users$seller_id <- NA
    } else {
      users$seller_id <- users$supplier$code
    }
    return(users[!is.na(users$id),names(users) %in% user_column_names])
  }

  #query
  #users_query  <- "https://dm-api.apps.platform.digital.gov.au/api/users"
  if (is.null(cache)) {
    users_query  <- prod_api("users?simple=TRUE")
    users_raw    <- fetchAllFromAPI(users_query,header(),list())
    temp_save(users_raw,"Users")
  } else {
    users_raw <- cache
  }
  users        <- bind_rows(lapply(users_raw,process_users))
  # and some post-processing
  users$email_domain <- str_split(users$email_address,"@",simplify=TRUE)[,2]
  users[is.na(users$frameworks),]$frameworks <- 7
  # filters
  users <- users %>% filter(frameworks == 7)
  #  filter(active) %>%
    
  #users$created_at   <- as.Date(users$created_at)
  users$created_at   <- parse_utc_timestamp_date(users$created_at)
  #users$logged_in_at <- as.Date(users$logged_in_at)
  users$logged_in_at <- parse_utc_timestamp_date(users$logged_in_at)
  #names(users) <- c("active","created_at","email_address","user_id","logged_in_at","user_name","role")
  attr(users,"timestamp") <- Sys.time()
  if (include_dta_users) {
    return(users)
  }
  # DTA users excluded by presence of a '+' 
  return(users[!grepl("\\+",users$email_address),]) 
}

#-------------------------------------------------------------------------------------------
extract_sellers <- function(header,include_deleted = FALSE, cache = NULL) {
  seller_columns <- c("code","name","abn","joined","website",
                      "contact_email","auth_rep","auth_rep_email","email_domain","application_id",
                      "assessed_aoe","unassessed_aoe","legacy_aoe",
                      "product_only","dmp_framework","no_of_case_studies","number_of_employees",
                      "sme_by_employees","sme_badge",
                      "is_recruiter","recruiter","regional","start_up","female_owned","indigenous",
                      "address_line","suburb","state","country","postal_code",
                      "gov_exp_federal","gov_exp_international","gov_exp_local","gov_exp_state",
                      "gov_exp_no_experience","creation_time","liability_exp","work_comp_exp",
                      paste0("maxPrice.",gsub(" ","_",domains)),
                      "signed_agreement_date")
  
  extract_domains_table <- function(x) {
    ids <- x$code
    domains <- x$domains$all
    for (i in seq_along(ids)) {
      if (nrow(domains[[i]]) > 0) {
        domains[[i]]$code <- ids[i]
      }
    }
    
    bind_rows(domains) %>%
      select(code,domain_name,price_status,status) %>%
      rename(aoe = domain_name)
  }
  
  process_raw_sellers <- function(df) {
    df <- df[!is.na(df$status),]        # filter out the ORAMS sellers
    if (!include_deleted) { 
      df <- df[df$status != "deleted",]
    }  
    # pricing comes out of the JSON as a set of 1 column data frames
    process_pricing <- function(x) {
      for (i in 1:dim(x)[2]) {
        x[,i] <- as.numeric(x[,i]$maxPrice)
      }
      names(x) <- paste0("maxPrice.",gsub(" ","_",names(x)))
      x
    }
    
    frameworks <- function(x) {
      #if frameworks is a data frame with any rows, then the seller is in the new framework
      if (is.data.frame(x)) {
        return(dim(x)[1] > 0)
      }
      return(FALSE)
    }
    
    df$abn            <- gsub("\\s+","",df$abn)
    df$assessed_aoe   <- unlist(lapply(df$domains$assessed,function(x) {paste(x,collapse="|")}))
    df$unassessed_aoe <- unlist(lapply(df$domains$unassessed,function(x) {paste(x,collapse="|")}))
    df$legacy_aoe     <- unlist(lapply(df$domains$legacy,function(x) {paste(x,collapse="|")}))
    df$auth_rep       <- df$representative
    df$auth_rep_email <- df$email
    df$email_domain   <- domains_from_emails(df$auth_rep_email)
    df$product_only   <- df$assessed_aoe=="" & df$unassessed_aoe==""
    df[is.na(df$number_of_employees),"number_of_employees"] <- "unknown"
    # this is SME determined by the declared number of employees
    df$sme_by_employees <- !df$number_of_employees %in% c("unknown","200+")
    df$dmp_framework  <- unlist(lapply(df$frameworks,frameworks))
    #df$contact_email  <- unlist(lapply(df$contacts,function(x) {x$email}))
    df$contact_email  <- df$contact_email
    #df$email_domain   <- matrix(unlist(strsplit(df$contact_email,"@")),ncol=2,byrow=TRUE)[,2]
    sts               <- df$seller_type
    # this is the SME status that the seller has indicated themselves. Not all sellers
    # who are SMEs have selected the badge
    names(sts)[which(colnames(sts)=="sme")] <- 'sme_badge'
    df                <- cbind(df,sts)
    names(df$government_experience) <- paste("gov_exp_",names(df$government_experience),sep="")
    df                <- cbind(df,df$government_experience)
    df                <- cbind(df,df$address)
    df$joined         <- as.Date(df$creation_time)
    df$liability_exp  <- as.Date(df$documents$liability$expiry)
    df$work_comp_exp  <- as.Date(df$documents$workers$expiry)
    df$is_recruiter   <- as.logical(df$is_recruiter)
    df$no_of_case_studies <- sapply(df$case_study_ids,length)
    if (!is.null(df$pricing)) {
      if (dim(df$pricing)[2] > 0) {
        p  <- process_pricing(df$pricing)
        df <- cbind(df,p)
      }
    }
    df$signed_agreement_date <- sapply(df$signed_agreements,function(x) {
      date_to_string(max(parse_utc_timestamp_date(x$signed_at)))
    })
    df <- df[,names(df) %in% seller_columns]
    # might want to write some other lines to extract the addresses, contacts etc
    df
  }
   
  #sellers_query <- "https://dm-api.apps.platform.digital.gov.au/api/suppliers"
  if (is.null(cache)) {
    sellers_query  <- prod_api("suppliers")
    sellers_raw    <- fetchAllFromAPI(sellers_query,header,list())
    temp_save(sellers_raw,"Sellers")
  } else {
    sellers_raw <- cache
  }
  case_studies   <- extract_case_studies(sellers_raw)
  seller_domains <- bind_rows(lapply(sellers_raw,extract_domains_table))
  sellers        <- bind_rows(lapply(sellers_raw,process_raw_sellers))
  sellers        <- fill_logicals(sellers)
  attr(sellers,"timestamp") <- Sys.time()
  attr(seller_domains,"timestamp") <- Sys.time()
  attr(case_studies,"timestamp") <- Sys.time()

  # temp fudge to remove seller 1574, which has a duplicate ABN
  sellers <- sellers %>% filter(code != 1574)
  
  # check for duplicated sellers
  if (length(unique(sellers$abn)) != length(sellers$abn)) {
    s <- sellers %>% 
      filter(duplicated(abn)) %>% 
      select(abn) %>%
      left_join(select(sellers,abn,name),by = "abn")
    print.data.frame(s)
    stop("Duplicate sellers in the catalogue")
  }

  return(list(sellers=sellers,case_studies=case_studies,seller_domains=seller_domains))
}

#-------------------------------------------------------------------------------------------
extract_briefs  <- function(header, return_all = FALSE, cache = NULL) {
  
  briefShortFilter <- c("id","status","title","organisation","location","sellerSelector","sellerCategory",
                        "specialistRole","areaOfExpertise","budgetRange","contractLength","publishedAt",
                        "createdAt","duration","close","frameworkFramework","lot","updatedAt",
                        "phase","essentials","nicetohaves","invitedSellerEmails","invitedSellerIDs")
  briefPresentationFilter <- c("id","status","title","organisation","location","openTo",
                               "type","specialistRole","areaOfExpertise","budgetRange","contractLength",
                               "created","published","close","duration","invitedSellerEmails",
                               "invitedSellerIDs", #"frameworkFramework",
                               "updatedAt","phase","essentials","nicetohaves")
  
  # legacy version, extracts from the sellerEmailList and/or sellerEmail columns
  extract_invited_seller_emails <- function(br) {
    # Check the sellerEmailList columm exists in this set of data 
    if ("sellerEmailList" %in% names(br)) {
      s_list <- br$sellerEmailList
    } else {
      s_list <- rep(list(""),nrow(br))
    }
    
    # check the sellerEmail column exists, and if so, take values in this column in preference
    if ("sellerEmail" %in% names(br)) {
      s_list <- lapply(seq_along(s_list),
                        function(x,s_list,emails) {
                          if(!is.na(emails[x])) {
                            return(emails[x])
                          }
                          s_list[[x]]
                        },
                        s_list=s_list,
                        emails=br$sellerEmail)
    }
    s_vec <- sapply(s_list,function(x) paste0(x,collapse="|"))
    return(s_vec)
  }  
  
  # new version, extracts the list of sellers invited
  extract_invited_seller_ids <- function(br) {
    if ("sellers" %in% names(br)) {
      s <- br$sellers
    } else {
      return(rep("",nrow(br)))
    }
    
    # somehow the sellers column can be empty
    if (ncol(s) ==0) {
      return(rep("",nrow(br)))
    }
    
    # if there's only 1 seller, the data structure is different...
    if (ncol(s) == 1) {
      seller_id  <- names(s)[1]
      seller_ids <- apply(s,1,function(x) {x})
      seller_ids[!is.na(seller_ids)] <- seller_id
      seller_ids[is.na(seller_ids)]  <- ""
      return(seller_ids)
    }
    
    #matrix of sellers by brief
    m     <- t(apply(s,1,function(x) {sapply(x,is.na)}))
    s_ids <- attr(m,"dimnames")[[2]]
    m     <- data.frame(m)
    names(m) <- s_ids
    se    <- m %>% 
      mutate(id = br$id) %>%
      gather(key="seller_id",value="invited",-id) %>%
      mutate(invited = !invited) %>%
      filter(invited) %>%
      group_by(id) %>%
      summarise(invited_sellers = paste(seller_id,collapse = "|")) %>%
      right_join(select(br,id),by="id") %>%
      replace_na(list(invited_sellers=""))
    
    return(se$invited_sellers)
  }

  # cleans up the briefs data.frame
  processBriefs <- function(briefs) {
    # some sets of briefs won't have a publishedAt date, which will break a few lines down 
    if (is.null(briefs$publishedAt)) {
      briefs$publishedAt <- NA
    }
    briefs$invitedSellerEmails <- extract_invited_seller_emails(briefs)
    briefs$invitedSellerIDs    <- extract_invited_seller_ids(briefs)
    briefs$duration  <- briefs$dates$application_open_weeks
    if (is.null(briefs$location)) {
      briefs$location = ""
    } else {
      briefs$location  <- sapply(briefs$location, function(x) paste(x, collapse = "|"))
    }
    briefs$close     <- string_to_date(briefs$dates$closing_date,format="%Y-%m-%d")
    if (is.null(briefs$essentialRequirements)) {
      briefs$essentials <- 0
    } else {
      briefs$essentials  <- sapply(briefs$essentialRequirements,length)
    }
    # for RFX/ATM, criteria are now in 'evaluationCriteria'
    if (!is.null(briefs$evaluationCriteria)) {
      evals <- sapply(briefs$evaluationCriteria, function(x) {if (is.null(x)) {NA} else {nrow(x)}})
      briefs[!is.na(evals),]$essentials <- evals[!is.na(evals)]
    }
    #
    if (is.null(briefs$niceToHaveRequirements)) {
      briefs$nicetohaves <- 0
    } else {
      briefs$nicetohaves <- sapply(briefs$niceToHaveRequirements,length)
    }
    briefs           <- briefs[,names(briefs) %in% briefShortFilter]
    briefs$created   <- parse_utc_timestamp_date(briefs$createdAt)
    return(briefs)
  }
  
  postProcessBriefs <- function(briefs) {
    briefs           <- briefs[order(briefs$id),]
    briefs$index     <- 1:nrow(briefs)
    briefs$openTo    <- as.factor(translate_open_to(briefs$sellerSelector)) 
    briefs$type      <- as.factor(translate_type(briefs$lot))
    # convert for the legacy categories for briefs prior to April/2017
    briefs[is.na(briefs$areaOfExpertise),"areaOfExpertise"] <-
      translate_specialist_role(briefs[is.na(briefs$areaOfExpertise),"specialistRole"])
    # manage the category selection for the new RFX briefs
    briefs$sellerCategory  <- translate_domain_code(briefs$sellerCategory)
    briefs[briefs$type == "RFX",]$areaOfExpertise <- briefs[briefs$type == "RFX",]$sellerCategory
    #
    briefs$areaOfExpertise <- gsub("\\,"," ",briefs$areaOfExpertise)
    briefs$areaOfExpertise <- as.factor(briefs$areaOfExpertise)
    briefs$published <- parse_utc_timestamp_date(briefs$publishedAt)
    briefs$updatedAt <- parse_utc_timestamp_date(briefs$updatedAt)
    # there are a few NAs early on
    briefs[is.na(briefs$close),"close"] <- briefs[is.na(briefs$close),"published"] + 14
    return(briefs[,briefPresentationFilter])
  }
  
  ### Query starts here
  #brief_query   <- "https://dm-api.apps.platform.digital.gov.au/api/briefs"
  if (is.null(cache)) {
    brief_query   <- prod_api("briefs")
    briefsRawList <- fetchAllFromAPI(brief_query,header(),list())
    temp_save(briefsRawList,"Briefs")
  } else {
    briefsRawList <- cache
  }
  #briefsRaw    <- combineBriefs(data.frame(),briefsRawList)
  briefsRaw     <- bind_rows(lapply(briefsRawList,processBriefs))
  briefsRaw     <- postProcessBriefs(briefsRaw)

  attr(briefsRaw,"timestamp") <- Sys.time()
  
  #Down-filter and post-process
  # filter out the drafts and withdrawn briefs
  if (!return_all) {
    #postProcessBriefs(briefsRaw[briefsRaw$status %in% c("closed","live"),])
    return(briefsRaw[briefsRaw$status %in% c("closed","live"),])
  } 
  
  return(briefsRaw)
}

#-------------------------------------------------------------------------------------------
extract_brief_responses <- function(header, cache = NULL) {
  
  removeBlankLines <- function(x) {
    x <- gsub("\r","",x)
    x <- gsub("\n\n","\n",x)
    return(gsub("\"","\'",x))
  }
  
  # converts a list of responses to essential/nice to have requirements,
  # concatenating them into a single text value
  convertLogic <- function(x) {
    label <- ""
    if (length(x) > 0) {
      for (i in 1:length(x)) {
        if (is.na(x[i])) {
          x[i] <- ""
        }
        if (x[i] == TRUE) {
          label <- paste(label,"TRUE")
        } else {
          if (x[i] == FALSE) {
            label <- paste(label,"FALSE")
          } else {
            label <- paste(label,i,"::-\n",removeBlankLines(as.character(x[i])),"\n")
          }
        }
      }
    }
    return(label)
  }
  
  reformat_criteria <- function(crit) {
    join_responses <- function(v) {
      crit <- sapply(seq_along(v), 
                     function(x,v) {
                       paste0(x,"::-\n",v[x],"\n")
                     },
                     v=v)
      return(paste(crit, collapse = ""))
    }
    
    crit2 <- crit %>% 
      gather(key = "criteria", value = "response", -id, na.rm = TRUE) %>%
      group_by(id) %>%
      summarise(responses = join_responses(response))
    
    return(crit2)
  }
  
  processBriefResponses <- function(df) {
    # get rid of the links column, it's not useful
    df$links                  <- NULL
    # convert the list of logicals to a vector of labels
    df$essentialRequirements  <- unlist(lapply(df$essentialRequirements,convertLogic))
    df$niceToHaveRequirements <- unlist(lapply(df$niceToHaveRequirements,convertLogic))
    if (!is.null(df$criteria)) {
      crit                   <- df$criteria
      crit$id                <- df$id
      criteria               <- reformat_criteria(crit)
      df$criteria            <- NULL
      df <- df %>%
        left_join(criteria,by = "id")
    }
    df$applicationDate        <- as.Date(df$createdAt)
    if ("attachedDocumentURL" %in% names(df)) {
      df$attachments            <- sapply(df$attachedDocumentURL,length)
      #df$attachments            <- sapply(df$attachedDocumentURL,function(x) {paste(x,collapse="|")})
    } else {
      df$attachments            <- 0
    }
    return(df)
  }
  
  #responsesquery        <- "https://dm-api.apps.platform.digital.gov.au/api/brief-responses"
  if (is.null(cache)) {
    responsesquery        <- prod_api("brief-responses")
    bapps                 <- fetchAllFromAPI(responsesquery,header(),list())
    temp_save(bapps,"BriefResponses")
  } else {
    bapps <- cache
  }
  briefResponses        <- bind_rows(lapply(bapps,processBriefResponses))
  #names(briefResponses) <- c("availability","briefId","createdAt","dayRate",
  #                           "essentialRequirements","applicationId","niceToHaveRequirements",
  #                           "respondToEmailAddress","supplierId","supplierName","applicationDate")       
  #re-order columns
  briefResponses        <- briefResponses[,c("briefId","id","applicationDate","supplierCode","supplierName",
                                             "specialistName",
                                             "availability","dayRate","essentialRequirements","niceToHaveRequirements",
                                             "responses","respondToEmailAddress","attachments",
                                             "createdAt")]
  names(briefResponses) <- c("briefId","applicationId","applicationDate","supplierId","supplierName","specialistName",
                             "availability","dayRate","essentialRequirements","niceToHaveRequirements","responses",
                             "respondToEmailAddress","attachments",
                             "createdAt")
  briefResponses$dayRate <- as.numeric(briefResponses$dayRate)
  attr(briefResponses,"timestamp") <- Sys.time()
    return(briefResponses)
}


#-------------------------------------------------------------------------------------------
# extract applications
extract_applications <- function(header, cache = NULL) {

  logicals <- c("agreed_to_master_agreement","regional","sme","indigenous","start_up","female_owned",
                "lgbtqi_owned","nfp_social_enterprise","disability",
#                "federal_government_experience","local_government_experience","state_government_experience",
                "conflicts_of_interest","insurance_claims","investigations","legal_proceedings","structual_changes")
  
  process_applications <- function(df) {
    
    # pricing comes out of the JSON as a set of 1 column data frames
    process_pricing <- function(x) {
      for (i in 1:dim(x)[2]) {
        x[,i] <- as.numeric(x[,i]$maxPrice)
      }
      names(x) <- paste0("maxPrice.",gsub(" ","_",names(x)))
      x
    }
    
    # extracts the list of services in x$services
    extract_services <- function(x) {
      domains             <- x$services
      domains$no_services <- rowSums(domains,na.rm=TRUE) == 0 
      domains$id <- x$id
      
      domains %>% 
        gather(key="aoe",value="nominated",-id,na.rm=TRUE) %>%
        filter(nominated) %>%
        mutate(aoe = case_when(
                       aoe == "no_services" ~ "",
                       TRUE                 ~ aoe
                     )) %>%
        group_by(id) %>%
        summarise(services = paste(aoe,collapse="|"))
    }

    signed_at <- function(x) {
      if (length(x) > 0) {
      #  return(x[dim(x)[1],]$signed_at)
        return(paste(unique(x$version),collapse="|"))
      }
      return("none")
    }
    df$federal_government_experience <- NULL
    df$local_government_experience   <- NULL
    df$state_government_experience   <- NULL
    df$sme               <- NULL
    if (is.null(df$supplier_code)) {
      df$existing          <- FALSE
    } else {
      df$existing          <- !is.na(df$supplier_code)
    }
    df$links             <- NULL
    df$createdAt         <- NULL
    df$created_at_date   <- as.Date(df[,"created_at"])
    df$created_at        <- as.POSIXct(df$created_at,format="%Y-%m-%dT%H:%M:%S",tz="UTC")
    #df$signed_agreements <- as.Date(unlist(lapply(df[,"signed_agreements"],signed_at)))
    df$signed_agreements <- unlist(lapply(df[,"signed_agreements"],signed_at))
    df$supplier          <- NULL
    df$abn               <- gsub("\\s+","",df$abn)
    df$application       <- NULL
    #df                   <- cbind(df,df$address[,c("address_line","country","postal_code","state","suburb")])
    df$addresses         <- NULL
    df$assessed_domains  <- NULL
    df$awards            <- NULL
    df$boards            <- NULL
    #df$number_case_studies <- unlist(lapply(df$case_studies,length))
    df$case_studies      <- NULL
    df$case_study_ids    <- NULL
    df$certifications    <- NULL
    df$day               <- NULL
    df$month             <- NULL
    df$year              <- NULL
    df$disclosures       <- NULL
    df$documents         <- NULL
    df$government_experience <- NULL
    df$products          <- NULL
    df$recruiter_info    <- NULL
    df$regional          <- NULL
    df <- cbind(df,df$seller_type)
    df$seller_type       <- NULL
    df$seller_types      <- NULL
    #df$services[is.na(df$services)] <- FALSE
    #df <- cbind(df,df$services)
    #df$services          <- apply(df$services,1,sum)
    #df <- cbind(df,df$steps)
    df$steps             <- NULL
    df                   <- cbind(df,df$address[,c("address_line","country","postal_code","state","suburb")])
    df$links             <- NULL # this can be returned in the 'address' data frame
                                 # its an embedded data frame so screws with bind_rows
    df$address           <- NULL
    df$creation_time     <- as.Date(df$creation_time)
    df$extra_links       <- NULL
    df$extraLinks        <- NULL
    df$lastUpdateTime    <- NULL
    df$last_update_time  <- as.Date(df$last_update_time)
    df$long_name         <- NULL
    df$longName          <- NULL
    df$references        <- NULL
    if (!is.null(df$submitted_at)) {
      df$submitted_at_date  <- as.Date(df$submitted_at)
      df$submitted_at       <- as.POSIXct(df$submitted_at,format="%Y-%m-%dT%H:%M:%S", tz="UTC")
    }
    df$active_candidates <- NULL
    df$database_size     <- NULL
    df$margin            <- NULL
    df$markup            <- NULL
    df$address_id        <- NULL
    df$expiry_dates      <- NULL
    df$conflicts_of_interest <- NULL
    df$insurance_claims      <- NULL
    df$investigations        <- NULL
    df$legal_proceedings     <- NULL
    df$structual_changes     <- NULL
    df$conflicts_of_interest_details <- NULL
    df$insurance_claims_details      <- NULL
    df$investigations_details        <- NULL
    df$legal_proceedings_details     <- NULL
    df$structual_changes_details     <- NULL
    df$travel            <- NULL
    #df$pricing           <- NULL
    if (!is.null(df$pricing)) {
      if (dim(df$pricing)[2] > 0) {
        p  <- process_pricing(df$pricing)
        df <- cbind(df,p)
      }
    }
    df$pricing <- NULL
    if(!is.null(df$is_recruiter)) {
      df$is_recruiter   <- as.logical(df$is_recruiter)
    }
    df[!is.na(df$submitted_at)&df$status=="saved","status"] <- "reverted"
    df[,names(df)[str_detect(names(df),"\\-")]]             <- NULL
    
    # leave to the end as the left_join doesn't like it until df is a simple data.frame
    extracted_services                                      <- extract_services(df)
    df$services                                             <- NULL
    df <- df %>% left_join(extracted_services,by="id")
    return(df)
  }
  
  # additional processing across the whole data frame - e.g. because dply doesn't handle
  # POSIXlt
  post_process_applications <- function(df) {
    df$submitted_at   <- as.POSIXct(df$submitted_at,format="%Y-%m-%dT%H:%M:%S", tz="UTC")
    df$created_at     <- as.POSIXct(df$created_at,format="%Y-%m-%dT%H:%M:%S",tz="UTC")
    # remove the NAs by changing to FALSE - removing as now done in process function
    x <- df[,names(df) %in% logicals]
    x[is.na(x)] <- FALSE
    df[,names(df) %in% logicals]  <- x
    return(df)
  }
  
  #apps_query    <- "https://dm-api.apps.platform.digital.gov.au/api/applications"
  if (is.null(cache)) {
    apps_query    <- prod_api("applications?per_page=200")
    apps_raw_list <- fetchAllFromAPI(apps_query,header(),list())
    temp_save(apps_raw_list,"Applications")
  } else {
    apps_raw_list <- cache
  }
  apps          <- bind_rows(lapply(apps_raw_list,process_applications))
  attr(apps,"timestamp") <- Sys.time()
  return(post_process_applications(apps))
  
}

#-------------------------------------------------------------------------------------------
# input is the raw sellers list from /suppliers API
extract_seller_prices <- function(rs) {
  # input the prices data frame for an individual seller
  pull_rates <- function(index,pricesList,suppId) {
    prices <- pricesList[[index]]
    prices <- cbind(prices,prices$serviceRole)
    prices$supplier_id <- suppId[index]
    return(prices[,c("daily_rate","supplier_id","role")])
  }
  extract_from_batch <- function(li) {
    return(bind_rows(lapply(1:length(li$prices),pull_rates,prices = li$prices,suppId = li$code)))
  }
  
  bind_rows(lapply(rs,extract_from_batch))
}


#-------------------------------------------------------------------------------------------
# just extracts basic details about domain assessment requests
extract_assessments <- function(header) {
  
  #ass_query    <- "https://dm-api.apps.platform.digital.gov.au/api/assessments"
  ass_query    <- prod_api("assessments")
  getdata<-GET(url=ass_query, add_headers(Authorization=header()))
  raw <- fromJSON(content(getdata,type="text"))
  if (length(raw) > 1) {
    stop("Check, has pagination been applied")
  }
  
  assessments        <- data.frame(seller_id = raw[[1]]$supplier_domain$supplier$code)
  assessments$name   <- raw[[1]]$supplier_domain$supplier$name
  assessments$status <- raw[[1]]$supplier_domain$status
  assessments$active <- raw[[1]]$active
  assessments$domain <- raw[[1]]$supplier_domain$domain$name
  assessments$created_at <- as.Date(raw[[1]]$created_at)
  assessments <- assessments[order(assessments$created_at),]
  assessments$cumulative <- 1:dim(assessments)[1]
  assessments$class      <- sapply(1:dim(assessments)[1],function(x,a) {
                                     if (!a[x,"active"]) {
                                       return("rejected")
                                     }
                                     if (a[x,"status"]=="unassessed") {
                                       return("unassessed")
                                     }
                                     return("passed")
                                   },a=assessments)
  assessments$resubmission <- duplicated(assessments[,c("seller_id","domain")])
  #assessments        <- assessments[assessments$status=="unassessed",]
  attr(assessments,"timestamp") <- Sys.time()
    return(assessments)
  
}

#-------------------------------------------------------------------------------------------
# extracts all contract records from Austender for the given SONs
# sons is a vector of standing offer numbers to extract
extract_austender <- function(sons = c("SON3413842","SON3364729")) {
  fetch_contracts <- function(son) {
    url <- paste("https://www.tenders.gov.au/?event=public.reports.CN.Published.download&decorator=CSV&agencyStatus=0&categorySearchCode=&valueEnd=&dateType=Publish%20Date&SON_ID=",
                 son,
                 "&dateStart=&AgencyUUID=&supplier_name=&ABN=&confidentiality=&dateEnd=&valueStart=",sep="")
    getdata<-GET(url=url)  
    cont <- content(getdata,as="text")
    cat(".")
    return(cont)
  }
  extract_son <- function(son) {
    cont <- fetch_contracts(son)
    x <- strsplit(cont,"\\r\\n")[[1]]
    header_line <- which(grepl("Agency\\tCN ID",x))
    headings    <- strsplit(x[header_line],"\\t")[[1]]
    headings    <- gsub("\\s+","\\.",headings)
    y <- x[(header_line+1):length(x)]
    z <- strsplit(y,"\\t")
    df <- data.frame(do.call(rbind,z),stringsAsFactors = FALSE)
    headings[12] <- "Value"
    names(df) <- headings
    df$Value  <- as.numeric(gsub("[\\$\\,]+","",df$Value))
    df$Publish.Date <- string_to_date(df$Publish.Date,"%d-%b-%y")
    df$Start.Date <- string_to_date(df$Start.Date,"%d-%b-%y")
    df$End.Date <- string_to_date(df$End.Date,"%d-%b-%y")
    for (i in 1:8) {
      df[,i] <- gsub('[\\"\\=]+','',df[,i])
    }
    return(df)
  }
  x <- bind_rows(lapply(sons,extract_son))
  attr(x,"timestamp") <- Sys.time()
  return(x)
}

#-------------------------------------------------------------------------------------------
# extracts a list of panels from Austender - returns the list of all currently open panels
extract_panel_listing <- function() {
  fetch_panels <- function() {
    url <- "https://www.tenders.gov.au/?event=public.SON.searchDownload&download=true&atmtype=archived%2Cclosed%2Cpublished%2Cproposed&keywordtypesearch=AllWord&postcode=&valuefrom=&datestart=01%2DJul%2D2017&keyword=&contractto=&suppliername=&valueto=&category=&agencyuuid=&panelarrangement=&orderBy=Relevance&agencyreporttype=&type=sonSearchEvent&portfoliouuid=&participantstatus=&dateend=&publishfrom=&supplierabn=&datetype=current&agencyrefid=&multiagencyaccess=&contractfrom=&atmid=&agencyStatus=&numagencystatus=%2D1&publishto=&sonid=&multype=archived%2Cclosed%2Cpublished"
    getdata<-GET(url=url)  
    cont <- content(getdata,as="text")
    cat(".")
    return(cont)
  }
  cont <- fetch_panels()
  x <- strsplit(cont,"\\r\\n")[[1]]
  header_line <- which(str_detect(x,"^SON ID\\tTitle"))
  headings    <- strsplit(x[header_line],"\\t")[[1]]
  headings    <- gsub("\\s+","\\.",headings)
  y <- x[(header_line+1):length(x)]
  z <- strsplit(y,"\\t")
  df <- data.frame(do.call(rbind,z),stringsAsFactors = FALSE)
  names(df) <- headings
  df$Publish.Date          <- string_to_date(df$Publish.Date,"%d-%b-%y")
  df$Contract.Start.Date   <- string_to_date(df$Contract.Start.Date,"%d-%b-%y")
  df$Contract.End.Date     <- string_to_date(df$Contract.End.Date,"%d-%b-%y")
  for (i in c(1,2,3,5,8,9)) {
    df[,i] <- gsub('[\\"\\=]+','',df[,i])
  }
  return(df)
}

#-------------------------------------------------------------------------------------------
# fetches a list of sellers for a given SON.ID from Austender
extract_vendors_from_panel <- function(son) {

  fetch_panel_listing <- function(son) {
    url <- paste0("https://www.tenders.gov.au/?event=public.advancedsearch.CNSONRedirect&type=sonSearchEvent&keyword=&KeywordTypeSearch=AllWord&SONID=",
                  son,
                  "&dateType=Publish+Date&dateStart=&dateEnd=&MultiAgencyAccess=&panelArrangement=&supplierName=&supplierABN=&ATMID=&AgencyRefId="  )
    getdata<-GET(url=url)  
    cont <- content(getdata,as="text")
    #cat(".")
    return(cont)
  }
  
  fetch_panel_page <- function(url) {
    url <- paste0("https://www.tenders.gov.au/",url)
    getdata<-GET(url=url)  
    cont <- content(getdata,as="text")
    cat(".")
    return(cont)
  }
  
  pl         <- fetch_panel_listing(son)
  pl_split   <- str_split(pl,"\\r\\n")[[1]]
  deets_line <- pl_split[str_detect(pl_split,"Full Details")]
  url        <- str_match(deets_line,"/\\?event=[^\"]+")[1,1]
  pp         <- fetch_panel_page(url)
  x          <- str_split(pp,"(#Suppliers)|(#Agency)")[[1]][2]
  y          <- str_match_all(x,"<td>[^<]*</td>")[[1]]
  z          <- str_remove_all(y,"</?td>")
  p_sellers  <- data.frame(matrix(z,ncol=4,byrow=TRUE),stringsAsFactors = FALSE)
  names(p_sellers) <- c("Name","ABN","State","Postcode")
  p_sellers$ABN    <- str_remove_all(p_sellers$ABN,"[^\\d]+")
  p_sellers$Name   <- gsub('"',"'",p_sellers$Name)
  return(p_sellers)
}

#-------------------------------------------------------------------------------------------
extract_sellers_from_panels <- function(sons) {
  fetch_individual_son <- function(son) {
    extract_vendors_from_panel(son) %>%
      mutate(`SON.ID` = son) %>%
      select(SON.ID,Name,ABN,State,Postcode)
  }
  bind_rows(lapply(sons,fetch_individual_son))
}

#-------------------------------------------------------------------------------------------
extract_feedback <- function(header) {
  
  # function to de-duplicate the feedback
  de_dupe <- function(df) {
    # use the duration figures as a unique id for each bit of feedback
    x <- df %>% group_by(user,difficulty,object_id,object_type,
                          timeToComplete,date) %>%
      summarise(comment = paste0(comment,collapse="")) %>%
      arrange(object_id,date)
    return(x)
  }
  
  process_feedback <- function(df) {
    if (!is.data.frame(df)) {
      # catches the pagination 'list'
      return(NULL)
    }
    df       <- cbind(df,df$data)
    df$data  <- NULL
    df$links <- NULL
    df$date  <- as.Date(df$createdAt)
    df[is.na(df$comment),"comment"] <- ""
    #return(df)
    #df <- de_dupe(df)
    df$cx <- translate(c("easy","ok","difficult"),c(2,1,0),df$difficulty)
    df$comment <- gsub("\"","\'",df$comment)
    return(df)
  }
  
  #feed_query  <- paste0(prod_api_url,"audit-events?audit-type=feedback")
  feed_query  <- prod_api("audit-events?audit-type=feedback")
  feed_raw    <- fetchAllFromAPI(feed_query,header(),list())
  temp_save(feed_raw,"Feedback")
  #getdata<-GET(url=feed_query, add_headers(Authorization=header))
  #raw <- fromJSON(content(getdata,type="text"))
  feed        <- bind_rows(lapply(feed_raw,process_feedback))
  attr(feed,"timestamp") <- Sys.time()
  return(feed)
}

#-------------------------------------------------------------------------------------------
extract_single_brief_attachments <- function(brief_id) {
  b_query <- prod_api(paste0("brief-responses?brief_id=",brief_id))
  b_raw   <- fetchAllFromAPI(b_query,header(),list())[[1]]
  return(unlist(b_raw$attachedDocumentURL))
}

#-------------------------------------------------------------------------------------------
# uses 'sellers_raw' which is the raw JSON from within the extract_sellers function
# this is a list of pages with 10 sellers each
extract_case_studies <- function(sellers_raw) {
  #  [1] "approach"         "client"           "created_at"       "id"               "links"           
  #  [6] "opportunity"      "outcome"          "project_links"    "referee_contact"  "referee_email"   
  #  [11] "referee_name"     "referee_position" "roles"            "service"          "supplier_code"   
  #  [16] "timeframe"        "title"           
  
  collapse_cs  <- function(x) {paste("- ",x,collapse="\n- ")}
  
  process_cs   <- function(css)  {
    if (is.null(css)|length(css)==0) {
      NULL
    } else {
      t <- 
        tibble(case_study_id = css$id,
               seller_id     = css$supplier_code,
               title         = css$title,
               client        = css$client,
               timeframe     = css$timeframe,
               aoe           = css$service,
               responsible_for = css$roles,
               challenge     = css$opportunity,
               approach      = css$approach,
               outcomes      = sapply(css$outcome,collapse_cs),
               links         = sapply(css$project_links,collapse_cs),
               status        = css$status
        )
    }
  }
  
  process_page <- function(page) {
    cat(".")
    by_page <- lapply(page$case_studies,process_cs)
    by_page <- by_page[!sapply(by_page,is.null)]    # remove the nulls
    bind_rows(by_page)
  }
  
  cs <- bind_rows(lapply(sellers_raw,process_page))
  cs <- apply(cs,2,function(x) {gsub('"',"'",x)})
  df <- data.frame(cs,stringsAsFactors = FALSE)
  df$seller_id     <- as.integer(df$seller_id)
  df$case_study_id <- as.integer(df$case_study_id)
  attr(df,"timestamp") <- Sys.time()
  return(df)
}

#-------------------------------------------------------------------------------------------
# extract the case studies for submitted applications
extract_case_studies_from_applications <- function(apps_raw) {
  #  [1] "approach"         "client"           "created_at"       "id"               "links"           
  #  [6] "opportunity"      "outcome"          "project_links"    "referee_contact"  "referee_email"   
  #  [11] "referee_name"     "referee_position" "roles"            "service"          "supplier_code"   
  #  [16] "timeframe"        "title"           
  
  collapse_cs  <- function(x) {paste("- ",x,collapse="\n- ")}
  
  process_cs   <- function(css)  {
    cat(".")
    if (is.null(css)|length(css)==0) {
      NULL
    } else {
      cat(css$supplier_code)
      t <- 
        tibble(
               title         = css$title,
               client        = css$client,
               timeframe     = css$timeframe,
               aoe           = css$service,
               responsible_for = css$roles,
               challenge     = css$opportunity,
               approach      = css$approach,
               outcomes      = sapply(css$outcome,collapse_cs)
        )
      if (length(css$project_links) > 0) {
        t$links <- sapply(css$project_links,collapse_cs)
      }
      t
    }
  }
  
  # this will be called once per application - is a list of 0 or more case studies 
  process_seller_case_studies <- function(seller_case_studies_list,id) {
    print(dim(seller_case_studies_list))
    print(id)
    # process - apps_raw[[]]$case_studies[[x]]
    #bind_rows(lapply(seller_case_studies_list,process_cs))
  }
  
  
  process_page <- function(page) {
    page <- page[page$type=="new",]
    page <- page[page$status=="submitted",]
    if (nrow(page)==0) {
      return(NULL)
    }
    cs <- page$case_studies
    x <- list(length(cs))
    for (i in 1:length(cs)) {
      x[[i]] <- process_seller_case_studies(cs[[i]],cs$id[i])
    }
    #return(bind_rows(x))
 }
  
  cs <- bind_rows(lapply(apps_raw,process_page)) # 25 pages of 200 applications
  
  #cs <- apply(cs,2,function(x) {gsub('"',"'",x)})
  #df <- data.frame(cs,stringsAsFactors = FALSE)
  #df$seller_id     <- as.integer(df$seller_id)
  #df$case_study_id <- as.integer(df$case_study_id)
  #attr(df,"timestamp") <- Sys.time()
  #return(df)
}

#-------------------------------------------------------------------------------------------
# extract the q&a from briefs
# Input is data.frame(id = <list of brief IDs>,...)
extract_brief_qa <- function(b,header) {
  extract_qa_for_a_single_brief <- function(brief_id,header) {
    print(brief_id)
    b_query <- prod_api(paste0("briefs/",brief_id))
    b_raw   <- fetchFromAPI_withexceptions(b_query,header())[[1]]
    qa      <- b_raw$clarificationQuestions
    # in case QA doesn't appear in the JSON
    if (is.null(qa)) {
      return(NULL)
    }
    # no QA returns as an empty list
    if (class(qa) != "data.frame") {
      return(NULL)
    }
    qa %>%
      mutate(qa_published = parse_utc_timestamp_datetime(qa$publishedAt),
             id           = brief_id) %>%
      select(id,qa_published,question,answer) %>%
      return()
  }
  
  qas <- bind_rows(lapply(b$id,extract_qa_for_a_single_brief,header=header))
  qas[,3:4] <- sapply(qas[,3:4],function(x) {gsub('"',"'",x)})
  return(qas)
}

#-------------------------------------------------------------------------------------------
extract_exceptions <- function() {
  # classes to convert the CSV into. This matches the classes for data.frame "sellers"
  # ie should match the return from sapply(sellers,class)
  c_classes <- c("character","integer","integer","character","character","logical",
                 "character","character","character","character","character","character",
                 "character","character","character","character","logical","logical",
                 "logical","logical","logical","logical","logical","logical","logical",
                 "logical","logical","logical","logical","character","character","character",
                 "character","character","Date","Date","Date","integer","numeric","numeric",
                 "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                 "numeric","numeric","character","logical")
  read.csv(paste0(getwd(),rel_path_data(),"seller_exceptions.csv"),colClasses = c_classes)
}

#-------------------------------------------------------------------------------------------
extract_work_orders <- function(header) {}

#-------------------------------------------------------------------------------------------
# extracts all JIRA tickets and returns as a list without further processing
#
# Currently using 'GET' with embedded search string. 
# 
extract_jira_tickets <- function(cache = NULL) {
  
  # extract relevant details from each GET return. Converts into a regular data.frame
  # so they can be merged into a single data.frame with all tickets
  extract_jira_ticket_details <- function(raw) {
    # these come out as a list, and some values can be missing
    # fills missing values and returns as a vector
    process_labels <- function(labels) {
      sapply(labels,function(x) {
        if (length(x) == 0) {
          return("unknown")
        }
        return(paste(x,collapse="|"))
      }) 
    }
    
    raw                    <- raw[[5]]
    tickets                <- data.frame(ticket_ids = raw$key, 
                                         summary    = raw$fields$summary,
                                         stringsAsFactors = FALSE)
    tickets$seller_id      <- raw$fields$customfield_11000
    tickets$application_id <- raw$fields$customfield_11100
    tickets$type           <- raw$fields$issuetype$name
    tickets$aoe       <- process_labels(raw$fields$labels)
    tickets$status    <- raw$fields$status$name
    tickets$description <- gsub('"',"'",raw$fields$description)
    tickets$created   <- parse_date_time(substr(raw$fields$created,1,19),"Y m d H M S", tz = "Australia/Canberra")
    tickets$updated   <- parse_date_time(substr(raw$fields$updated,1,19),"Y m d H M S", tz = "Australia/Canberra")
    tickets$resolution_date <- parse_date_time(substr(raw$fields$resolutiondate,1,19),"Y m d H M S", tz = "Australia/Canberra")
    return(tickets)
  }
  
  # extract the update history for a set of tickets
  extract_jira_history <- function(raw) {
    process_history_record <- function(hist) {
      changes         <- bind_rows(hist$items)
      #changes$author  <- hist$author$name
      changes$created <- parse_date_time(substr(hist$created,1,19),"Y m d H M S", tz = "Australia/Canberra")
    }
    histories <- raw$issues$changelog$histories
    bind_rows(lapply(histories,process_history_record))
  }
  
  single_api_call <- function(offset,j_token,url_get) {
    url_offset <- paste0(url_get,"&startAt=",offset)
    print(url_offset)
    raw <- GET(url_offset,add_headers(Authorization=j_token))
    fromJSON(content(raw,type="text",encoding="UTF-8"))
  }
  
  if (is.null(cache)) {
    url_get          <- paste0(Sys.getenv("jira_api"),
                               "/rest/api/2/search",
                               "?jql=project=MARADMIN&maxResults=100",
                               "&&expand=changelog")
    j_token        <- paste("Basic",
                            base64_enc(paste0(Sys.getenv("jira_login"),
                                              key_get("jira",keyring = "mkt"))))
    tick_fetch_get <- GET(url_get,add_headers(Authorization=j_token))
    raw            <- fromJSON(content(tick_fetch_get,type="text",encoding="UTF-8"))
  
    total_tickets  <- raw$total      # The number of tickets available
    max_results    <- raw$maxResults # max results returned by the API. Should = 100, but may be limited
    req_fetches    <- ceiling(total_tickets/max_results)
    offsets        <- (seq(req_fetches)-1) * max_results
  
    raw_jira       <- lapply(offsets,
                           single_api_call,
                           j_token = j_token,
                           url_get = url_get)
  
    #save the raw data
    temp_save(raw_jira,"JIRA")
  } else {
    raw_jira <- cache
  }
  
  j_tickets               <- bind_rows(lapply(raw_jira,extract_jira_ticket_details))
  j_tickets$month_created <- floor_date(j_tickets$created,unit="month")
  j_tickets$type          <- as.factor(j_tickets$type)
  return(j_tickets)
}

