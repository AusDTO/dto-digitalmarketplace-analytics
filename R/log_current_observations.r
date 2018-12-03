# logs useful observations from the current data extract
log_current_observations <- function(timestamp = Sys.time()) {
  initial_size <- 100 # initialise a large data frame - gets trimmed at the end
  log <- data.frame(timestamp = timestamp,
                    class     = character(initial_size),
                    key       = character(initial_size),
                    value     = character(initial_size),
                    stringsAsFactors = FALSE)
  
  # adds a record to the first empty log record
  add_entry <- function(log,class,key,value) {
    #print(paste(class,key,value))
    log[which(log$class=="")[1],2:4] <- c(class,key,as.character(value))
    return(log)
  }
  
  # adds multiple entries from a data frame
  add_entries <- function(log,df) {
    for (i in 1:dim(df)[1]) {
      log <- add_entry(log,df[i,1],df[i,2],df[i,3])
    }
    return(log)
  }
  
  # log current new seller assessment numbers
  log <- add_entry(log,"integer","new_seller_applications|submitted",sum(apps$status=="submitted"))
  log <- add_entry(log,"integer","new_seller_applications|saved",sum(apps$status=="saved"))
  log <- add_entry(log,"integer","new_seller_applications|reverted",sum(apps$status=="reverted"))
  
  # number of buyers, sellers, briefs
  log <- add_entry(log,"integer","brief_count",dim(briefs)[1])
  log <- add_entry(log,"integer","seller_count",dim(sellers)[1])
  x <- buyers %>% filter(active,!grepl("\\+",email_address)) %>% dim
  log <- add_entry(log,"integer","buyer_count",x[1])

  # areas of expertise - add counts of assessed and unassessed domains
  aoes <- sapply(domains,function(x,s) {
                           sum(grepl(x,s$assessed_aoe))
                         },s = sellers)
  temp <- data.frame(class="integer",
                     domains=paste0("assessed_domains|",domains),
                     count=aoes,
                     stringsAsFactors=FALSE)
  log <- add_entries(log,temp)
  
  aoes_un <- sapply(domains,function(x,s) {
                              sum(grepl(x,s$unassessed_aoe))
                            },s = sellers)
  temp <- data.frame(class="integer",
                     domains=paste0("unassessed_domains|",domains),
                     count=aoes_un,
                     stringsAsFactors=FALSE)
  log <- add_entries(log,temp)

  
  # if initiating a new file 
  #write.table(log[1:which(log$class=="")[1]-1,],file=paste0(rel_path_data,log_filename),sep=",",append=FALSE,quote=TRUE,row.names=FALSE,col.names=TRUE)
  # append the log file
  write.table(log[1:which(log$class=="")[1]-1,],file=paste0(getwd(),rel_path_data(),log_filename),sep=",",append=TRUE,quote=TRUE,row.names=FALSE,col.names=FALSE)
  #return(log[1:which(log$class=="")[1]-1,]) 
}