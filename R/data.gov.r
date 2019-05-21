# functions to extract data and produce clean files for potential upload to data.gov.au


# function to extract the public information from briefs, for publishing on data.gov.au
# 
extract_brief_details <- function(briefs) {

  # columns that can be published
  clean_record <- function(raw) {
    columns <- names(raw)
    scalars <- columns[columns %in% scalar_columns]
    df <- tibble(id = raw$id)
    for (c in scalars) {
      #print(c)
      #print(length(raw[[c]]))
      df[,c] <- raw[[c]]
    }
    vectors <- columns[columns %in% vector_columns]
    for (c in vectors) {
      df[,c] <- as.character(toJSON(raw[[c]])) %>% str_replace_all('"',"'")
    }
    
    ### still to add - code to format raw data in data.frame format - e.g. questions & dates
    
    return(df)
  }
  
  scalar_columns <- qw("additionalRelevantInformation additionalTerms
                      applicationsClosedAt areaOfExpertise backgroundInformation budgetRange 
                      createdAt culturalWeighting contractLength
                      endUsers  existingTeam 
                      frameworkName id  numberOfSuppliers 
                      organisation outcome phase priceCriteria priceWeighting publishedAt 
                      requirementsLength securityClearance sellerSelector startDate status 
                      summary technicalWeighting title updatedAt workAlreadyDone 
                      workingArrangements workplaceAddress")
  
  vector_columns <- qw("culturalFitCriteria
                      essentialRequirements  
                      niceToHaveRequirements
                      evaluationType
                      location
                       successCriteria") 
  df_columns     <- qw("clarificationQuestions dates")
  
  # Ordering and selection of columns, and the final name to be used
  columns <- tibble(
    columns = qw("id
	  	title		organisation		summary    type   sellerSelector		publishedAt		applicationsClosedAt		status

		  startDate    contractLength		location		budgetRange

		  backgroundInformation		outcome		endUsers		workAlreadyDone		existingTeam    additionalRelevantInformation		
      phase

		  workplaceAddress		workingArrangements		securityClearance

      additionalTerms

		  essentialRequirements		niceToHaveRequirements

      numberOfSuppliers    successCriteria		culturalFitCriteria    areaOfExpertise
      priceCriteria		evaluationType		
      technicalWeighting  priceWeighting		culturalWeighting updatedAt"),
    column_names = qw("Opportunity_ID  Title   Organisation Summary Opportunity_type Open_To Published Closed  Status
                     
                     Earliest_start_date  Contract_length Location Budget_range
                     
                     Why_the_work_is_being_done Key_problem_to_solve  Users_and_their_needs Work_already_done
                     Work_will_be_done_with Additional_relevant_information Project_phase
                     
                     Workplace_address  Working_arrangements  Security_clearance
                     
                     Additional_terms
                     
                     Essential_skills_and_experience  Nice_to_have_skills_and_experience
                     
                     Number_of_sellers_evaluated  Proposal_criteria Cultural_fit_criteria Area_of_expertise
                      Payment_approach  
                     Assessment_methods Technical_competence_weighting  Cultural_fit_weighting  Price_weighting
                    
                    last_updated")
  )
    
  # extracts a single brief with id = id
  extract_single_brief <- function(id) {
    cat(id,"\n")
    bQuery <- prod_api(paste("briefs",as.character(id),sep="/"))
    bRaw   <- fetchFromAPI(bQuery,header())
    #df     <- data.frame(t(unlist(bRaw)),stringsAsFactors = FALSE)
    #names(df) <- attr(df,"dimnames")
    return(bRaw)
  }
  
  b          <- select(briefs,id,type) %>% mutate(type=as.character(type))
  briefIds   <- briefs$id
  briefs_raw <- lapply(briefIds,extract_single_brief)
  df <- bind_rows(lapply(briefs_raw,clean_record))
  
  ## cleanup steps
  cleanups <- function(column) {
    if (class(column) == "character") {
      column <- gsub('"',"'",column)
      column <- str_replace_all(column,"[^\\s]+@[^\\s+]+","<<email_address>>")
    }
    return(column)
  }
  
  b_ <- df %>%
    mutate(applicationsClosedAt = date_to_string(parse_utc_timestamp_date(applicationsClosedAt)),
           createdAt            = date_to_string(parse_utc_timestamp_date(createdAt)),
           publishedAt          = date_to_string(parse_utc_timestamp_date(publishedAt)),
           updatedAt            = date_to_string(parse_utc_timestamp_date(updatedAt))) %>%
    left_join(b,by="id") # %>%
    #select(one_of(columns$columns))
  b_[is.na(b_$applicationsClosedAt),]$applicationsClosedAt <-   b_[is.na(b_$applicationsClosedAt),]$publishedAt
  b_ <- as.data.frame(sapply(b_,cleanups),stringsAsFactors = F)
  columns   <- columns %>% filter(columns %in% names(b_))
  b_        <- select(b_,one_of(columns$columns))
  names(b_) <- columns$column_names 
  b_[is.na(b_)] <- ""
  
  return(b_)
}  

# reads the last briefs summary file and looks for any briefs that have been updated
# since the summary file (marketplace_briefs.csv) was last updated, or are new. 
# also removes any briefs that are withdrawn
update_brief_details <- function(briefs) {

  # read the briefs already extracted
  b_last <- read.csv(file=paste0(getwd(),rel_path_reports,"marketplace_briefs.csv"),
                     stringsAsFactors = FALSE,
                     colClasses="character")
  
  b_last_ids <- b_last %>%
    select(Opportunity_ID,last_updated) %>%
    mutate(last_updated = string_to_date(last_updated))
  
  # which briefs have been updated or published since the last update 
  b_updated <- briefs %>% 
    filter(updatedAt >= max(b_last_ids$last_updated)) %>%
    extract_brief_details()

  # combine the new and old sets of briefs
  b <- b_last %>% 
    filter(!Opportunity_ID %in% b_updated$Opportunity_ID) %>%
    bind_rows(b_updated) %>%
    filter(Opportunity_ID %in% briefs$id)

  return(b)
}

