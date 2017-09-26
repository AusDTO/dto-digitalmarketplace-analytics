#plotly_plots

# plots for buyers
#   plots a graph showing since launch:
#     total number of registered buyers
#     total number of active buyers (has logged in within past 3 months)
#     total number of agencies (that have registered buyers)
palette <- c("#EC9898","#6F65A6","#4592C0","#F4B034","#E0E31C","#231F20","#BFBFBF","#9F9F9F",
             "#F3B122","#DF7D67","#A1C4C9","#B5D7A6","#CEE2F3","#9EC4E8","#76A470","#6BBF63",
             "#DF7D67","#4491C2")

plot_buyer_counts <- function(b,publish=FALSE) {
  counts <- data.frame(datesSinceLaunch())
  names(counts) <- "dates"
  # count of total buyer
  counts$total.buyers <- sapply(counts$dates,function(x,b) {sum(b$created_at<=x)}, b=b)
  # count of active buyers
  # change to logged_in_at to when the buyer will/went inactive
  b$inactive_at <- as.Date(sapply(1:nrow(b),
                           function(x,b) {
                             if(is.na(b[x,]$logged_in_at)) {
                               b[x,]$created_at + 90
                             } else {b[x,]$logged_in_at + 90}
                           },b=b),origin="1970-01-01")
  counts$active.buyers <- sapply(counts$dates,function(x,b) {
                                  sum(b$created_at<=x) - sum((b$inactive_at)<x)}, b=b)
  counts$agencies <- sapply(counts$dates,function(x,b) {length(unique(b[b$created_at<=x,]$email_domain))},b=b)
  plot_ly(counts,x = ~dates, y = ~total.buyers, name = "Buyers",hoverinfo= 'text', 
               text = paste(counts$total.buyers, "buyers"),
               type='scatter',mode='lines') %>%
      add_trace(y=~active.buyers,type='scatter',mode='lines',text=paste(counts$active.buyers, "active buyers"),name = "Active Buyers") %>%
      add_trace(y=~agencies,type='scatter',mode='lines',text=paste(counts$agencies, "agencies"),name="Agencies") %>%
    layout(title = 'Registered Buyers and Agencies',
           xaxis = list(title = "",
                        showgrid = TRUE),
           yaxis = list(title = "",
                        showgrid = TRUE))
  #return(counts)
}

plot_stacked_agency_counts <- function(b,publish=FALSE) {
  counts <- data.frame("dates" = datesSinceLaunch())
  names(counts) <- c("dates")
  counts$days   <- 1:dim(counts)[1]
  counts$comm <- sapply(counts$dates,
                        function(x,b) 
                        {
                          length(unique(b[b$created_at<=x&b$category%in%c("Commonwealth","Commonwealth-D8"),]$email_domain))
                        },b=b)
  counts$state <- sapply(counts$dates,
                        function(x,b) 
                        {
                          length(unique(b[b$created_at<=x&b$category%in%c("State"),]$email_domain))
                        },b=b)
  counts$local <- sapply(counts$dates,
                        function(x,b) 
                        {
                          length(unique(b[b$created_at<=x&b$category%in%c("Local"),]$email_domain))
                        },b=b)
  counts$other <- sapply(counts$dates,
                        function(x,b) 
                        {
                          length(unique(b[b$created_at<=x&b$category%in%c("Education","Corporate","Enterprise"),]$email_domain))
                        },b=b)
  
  # count of total buyer
  plot_ly(counts, x= ~days, y = ~comm, name = 'Commonwealth', type='scatter', mode= 'none', 
          fill = 'tozeroy', hoverinfo= 'text',text = ~paste(counts$comm,"Commonwealth")) %>%
    add_trace(y = ~state, name='State', text = paste(counts$state,"State")) %>%
    add_trace(y = ~local, name='Local', text = paste(counts$local,"Local")) %>%
    add_trace(y = ~other, name='Other', text = paste(counts$other,"Other")) %>%
    layout(title = 'Agencies registered on the Marketplace',
           xaxis = list(title = "",
                        showgrid = FALSE),
           yaxis = list(title = "Agencies",
                        showgrid = FALSE))
  
}

plot_buyer_count_by_agency_category <- function(buyers,publish=FALSE) {
  agencies <- buyers %>%
                group_by(category) %>% 
                summarise(count=length(category))  
  plot_ly(agencies,labels=~category, values=~count,
          #rotation = "130",
          textposition = 'outside',
          textinfo = 'label+percent+value',
          marker=list(colors = brewer.pal(7,"Blues"))) %>%
    add_pie(hole=0.4) %>%
    layout(title = "Buyers by Agency type",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  #return(agencies)
}

plot_agency_count_by_category <- function(buyers,publish=FALSE) {
  agencies <- buyers %>%
    group_by(category) %>% 
    summarise(count=length(unique(email_domain)))  
  plot_ly(agencies,labels=~category, values=~count,
          #rotation = "130",
          textposition = 'outside',
          textinfo = 'label+percent+value',
          marker=list(colors = brewer.pal(7,"Blues"))) %>%
    add_pie(hole=0.4) %>%
    layout(title = "Agencies By Type",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  #return(agencies)
}

plot_contracts_sme_by_value <- function(smes,palette) {
  plot_ly(smes,labels=~SME, values=~byvalue, marker = list(colors = palette[c(9,19)]),
          textposition="inside",
          textinfo="label+value+percent",
          rotation = "102",
          showlegend = FALSE,
          outsidetextfont=list(color = palette[2],size="16"),
          insidetextfont=list(color = '#FFFFFF',size="16")) %>%
  add_pie(hole=0.4) %>%
    layout(title = "Contracts to SME by value",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}

plot_contracts_sme_by_count <- function(smes,palette) {
  plot_ly(smes,labels=~SME, values=~bycount, marker = list(colors = palette[c(9,19)]),
          textposition="inside",
          textinfo="label+value+percent",
          rotation = "136",
          showlegend = FALSE,
          outsidetextfont=list(color = palette[2],size="16"),
          insidetextfont=list(color = '#FFFFFF',size="16")) %>%
    add_pie(hole=0.4) %>%
    layout(title = "Contracts to SME by count",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}

# use briefResponse
# plots the number of applications by each seller
plot_application_counts_by_seller <- function(bR) {
  counts <- bR %>%
    group_by(supplierId, supplierName) %>%
    summarise(count = length(applicationDate)) %>%
    arrange(desc(count))
  counts$index <- 1:dim(counts)[1]
  plot_ly(counts,x=~index, y=~count, type='scatter',
          text = paste("Applications: ",counts$count,
                       "<br />Seller ID: ",counts$supplierId,
                       "<br />",counts$supplierName),
          mode='markers') %>%
    layout(title = 'Number of applications by each seller to date',
           xaxis=list(title=""))
}

# use 'briefs'
plot_open_to_by_month <- function(briefs) {
  briefs$month <- strftime(briefs$published, "%m")
  months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  briefs$month <- months[as.integer(briefs$month)]
  by_open <- briefs %>%
    group_by(month) %>%
    summarise(open_to_1 = sum(openTo=="oneSeller"),
              open_to_selected = sum(openTo=="someSellers"),
              open_to_all      = sum(openTo=="allSellers"),
              date             = min(published)) %>%
    arrange(date)
  by_open$order <- 1:dim(by_open)[1]
  plot_ly(by_open, x=~order, y=~NaN, type='bar') %>%
    add_trace(y =~ open_to_1, name="Open to 1") %>%
    add_trace(y =~ open_to_selected, name="Open to Selected") %>%
    add_trace(y=~open_to_all, name="Open to All") %>%
    layout(yaxis = list(title = 'Count'), barmode = 'group')
}

#use briefs
plot_outcome_spec_by_month <- function(briefs) {
  briefs$month <- strftime(briefs$published, "%m")
  months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  briefs$month <- months[as.integer(briefs$month)]
  by_type <- briefs %>%
    group_by(month) %>%
    summarise(specialists = sum(type=="digital-professionals"),
              outcomes    = sum(type=="digital-outcome"),
              date        = min(published)) %>%
    arrange(date)
  by_type$order <- 1:dim(by_type)[1]
  plot_ly(by_type, x=~order, y=~NaN, type='bar') %>%
    add_trace(y =~ specialists, name="Specialists") %>%
    add_trace(y =~ outcomes, name="Outcomes") %>%
    layout(yaxis = list(title = 'Count'), barmode = 'group')
}

plot_briefs_published <- function(b) {
  b <- b[order(b$published),]
  plot_ly(y=~1:dim(b)[1],x=~b$published,mode="lines",type="scatter",
          color=~b$frameworkFramework,
          colors=c("#EC9898","#6F65A6")) %>%     
    layout(title = 'Briefs published by date',
           xaxis = list(title = "",showgrid = TRUE),
           yaxis = list(title = "briefs published",showgrid = TRUE))
}

plot_bids_by_specialist_brief <- function(bR) {
  bR$dayRate <- as.numeric(bR$dayRate)
  bR <- bR[bR$openTo=="All"
           &bR$type=="Specialist"
           &bR$dayRate<5000
           &bR$status=="closed",]
  ids <- unique(bR$id)
  index <- 1:length(ids)
  bR$index <- translate(ids,index,bR$id)
  avDR <- bR %>% group_by(index) %>% summarise(average = mean(dayRate))
  plot_ly(bR,y=~dayRate,x=~index,type="scatter",mode="markers",
          color=bR$frameworkFramework,
          colors=c("#EC9898","#6F65A6"),
          text = paste("ID:",bR$id,"<br />",bR$title)) %>%
    #add_trace(data=avDR,y=~average,mode="lines",name="average per brief") %>%
    layout(title = 'Seller bids',
           xaxis = list(title = "Brief",
                        showgrid = TRUE),
           yaxis = list(title = "$ per day",
                        showgrid = TRUE))
}

plot_distribution_of_specialist_bids <- function(bR) {
  bR$dayRate <- as.numeric(bR$dayRate)
  bR <- bR[bR$openTo=="All"
           &bR$type=="Specialist"
           &bR$dayRate<5000
           &bR$status=="closed",]
  ids <- unique(bR$id)
  index <- 1:length(ids)
  bR$index <- translate(ids,index,bR$id)
  densDMP  <- density(bR[bR$frameworkFramework=="dm",]$dayRate)
  densDMP$y <- densDMP$y * 40/max(densDMP$y)
  densDSP  <- density(bR[bR$frameworkFramework=="dsp",]$dayRate)
  densDSP$y <- densDSP$y * 90/max(densDSP$y)
  plot_ly(alpha = 0.6) %>%
    add_histogram(data=bR[bR$frameworkFramework=="dsp",],x=~dayRate,name="DSPP") %>%
    add_histogram(data=bR[bR$frameworkFramework=="dm",],x=~dayRate,name="DMP") %>%
    add_trace(x=densDSP$x,y=densDSP$y,name="DSPP smoothed", type="scatter",mode="lines") %>%
    add_trace(x=densDMP$x,y=densDMP$y,name="DMP smoothed", type="scatter",mode="lines")
}