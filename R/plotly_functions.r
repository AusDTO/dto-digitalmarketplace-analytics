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

# updated version for monthly reports
plot_agency_count_by_category_2 <- function(agencies,publish=FALSE) {
  #agencies$entities <- factor(agencies$entities,levels=cats)
  plot_ly(agencies,labels=~entities, values=~count,
          #rotation = "140",
          textposition = 'outside',
          textinfo = 'label+percent+value',
          #sort=FALSE,
          marker=list(colors = brewer.pal(5,"Dark2"),
                      line = list(color = '#FFFFFF', width = 1))) %>%
    add_pie(hole=0.4) %>%
    layout(title = "Agencies By Type",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  #return(agencies)
}

# plots a distribution of day rates for specialist briefs based on seller responses
plot_violins_of_day_rates <- function(bR) {
  # violin plots by area of expertise
  vals <- briefResponses %>% filter(!is.na(areaOfExpertise),
                                    !is.na(dayRate),
                                    dayRate > 450,dayRate < 4000)
  add_density_plot <- function(p,bR,i,color) {
    #  dens <- density(bR$dayRate,na.rm=TRUE,from=(min(bR$dayRate)-100))
    dens <- density(bR$dayRate,na.rm=TRUE,from=350,to=3000)
    up   <- dens$y/max(dens$y) + (i*3 + 2)
    down <- (i*3 + 2) - dens$y/max(dens$y)
    n    <- bR[1,"areaOfExpertise"]
    p <- p %>% add_lines(x=dens$x,y=up,mode="line",color=color,name=n)
    p %>% add_lines(x=dens$x,y=down,mode="line",color=color,
                    fill = "tonexty",
                    showlegend=FALSE)
  }
  
  resps <- vals %>% group_by(areaOfExpertise) %>% 
    summarise(number_of_briefs = length(unique(id)), 
              number_of_responses = length(applicationId))
  
  areas <- data.frame(areaOfExpertise=unique(vals$areaOfExpertise))
  doms  <- data.frame(areaOfExpertise= domains)
  resps <- merge(resps,doms,all.y=TRUE)
  resps[is.na(resps)] <- 0
  names(resps) <- c("Area of Expertise","Number of Briefs","Number of Responses")
  all_dens <- density(vals$dayRate,na.rm=TRUE,from=350,to=2750)
  up       <- all_dens$y/max(all_dens$y) + 2
  down     <- 2 - all_dens$y/max(all_dens$y)
  p <- plot_ly(x=all_dens$x,y=up,
               type="scatter",
               mode="lines",
               name="All areas",
               color=brewer.pal(10,"Paired")[1])
  p <- p %>% add_lines(x=all_dens$x,y=down,mode="line",
                       color=brewer.pal(12,"Paired")[1],
                       fill = "tonexty",
                       showlegend=FALSE)
  areas <- as.character(resps[resps$`Number of Responses`>10,]$`Area of Expertise`)
  for (i in 1:length(areas)) {
    #  if (resps[resps$areaOfExpertise==areas[i],]$number_of_responses > 9) {
    p <- add_density_plot(p,
                          vals[vals$areaOfExpertise==areas[i],],
                          i,
                          brewer.pal(12,"Paired")[i+1])
    #  }
  }
  p <- p %>% layout(xaxis=list(range=c(0,3000),
                               title="Day rate trend ($ incl GST)", showaxis=FALSE),
                    yaxis=list(showticklabels=FALSE,
                               showgrid=FALSE,
                               showline=FALSE),
                    #legend = list(orientation = 'h')
                    showlegend=FALSE)
  #title="Distribution of Day Rates for Specialist Briefs")
  # annotations
  y_s   <- c(2,1:length(areas) * 3 + 2) + 0.6
  areas <- c("All areas",areas)
  p <- p %>% add_annotations(  
    x = rep(2500,length(areas)),
    y = y_s,
    showarrow=FALSE,
    text = areas
  )
  p
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

plot_briefs_published_by_type <- function(b) {
  b <- b[order(b$published),] %>%
    arrange(published) %>%
    mutate(one=1,total=cumsum(one))      %>%
    group_by(type)     %>%
    mutate(one=1, published_by_type = cumsum(one))
  b_o <- b[b$type=="Outcome",]
  b_s <- b[b$type=="Specialist",]
  # calculate a polynomial trend line from DMP launch up to the last month
  end_date   <- floor_date(Sys.Date(),"month")
  #end_date   <- Sys.Date()
  start_date <- as.Date("2017-06-30")
  months     <- interval(start_date,end_date) %/% months(1)
  x_a        <- sum(b$published <= start_date)
  x_b        <- sum(b$published <= end_date)
  perc       <- 10^(log10(x_b/x_a)/months) - 1
  months     <- data.frame(months = seq(start_date,ceiling_date(Sys.Date(),"month"),by="months"))
  months$count <- 0:(nrow(months)-1)
  months$estimate <- x_a * (1+perc) ^ months$count
  ##   
  plot_ly(data=b,y=~total,x=~published,mode="lines",type="scatter",
          #color=brewer.pal(3,"Dark2")[1])
          colors=brewer.pal(3,"Dark2"),
          name="Total") %>% 
    add_trace(x=b_o$published,y=b_o$published_by_type, name="Outcomes") %>%
    add_trace(x=b_s$published,y=b_s$published_by_type, name="Specialists") %>%
    add_trace(x=months$months,y=months$estimate,
              name=paste("Growth at",round(perc*100),"%"),
              line=list(dash='dot')) %>%
    layout(title = 'Briefs published by date',
           xaxis = list(title = "",showgrid = TRUE),
           yaxis = list(title = "briefs published",showgrid = TRUE))
}

plot_brief_type_as_percentage <- function(briefs) {
  b <- briefs %>%
    #mutate(total = 1:n()) %>%
    arrange(published) %>%
    group_by(published,type) %>%
    summarise(total_by_type_per_day=n())
  b$total_per_day <- cumsum(b$total_by_type_per_day)
  b <- b %>%
    group_by(type) %>%
    mutate(published_by_type = cumsum(total_by_type_per_day),
           cumpercent        = 100*published_by_type/total_per_day)
  
  plot_ly(data=b,y=~cumpercent,x=~published,mode="lines",type="scatter",
          color=~b$type,
          colors=c("#EC9898","#6F65A6")) %>%     
    layout(title = 'Ratio of brief types over time',
           xaxis = list(title = "",showgrid = TRUE),
           yaxis = list(title = "percentage",showgrid = TRUE))
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
          text = paste("ID:",bR$id,"<br />",bR$title),
          marker = list(size=3)) %>%
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

plot_agencies_publishing_per_period <- function(briefs) {
  x <- briefs %>%
    group_by(month=floor_date(published,"month"),entities) %>%
    summarise(count=length(unique(agencyName))) %>%
    spread(entities,count,fill=0) %>%
    gather(key="entities",value="count",CCE:State,factor_key = TRUE) %>%
    arrange(month)
  x <- as.data.frame(x) # plotly doesn't seem to like tibbles :-(
  plot_ly(data=x,x=~month,y=~count,mode="lines",type="scatter",
          color=~entities,colors=brewer.pal(5,"Set1")) %>%
    layout(title = 'Agencies publishing briefs per month',
           xaxis = list(title = "",
                        showgrid = TRUE),
           yaxis = list(title = "Number of agencies",
                        showgrid = TRUE))
}
