# Question 2 A-------------
library(igraph)
library(zoo)
library(lubridate)

all_months <- unique(funding.events.dt[, Deal_Date_Year_Month])
#Sort all months
all_months <- all_months[order(all_months)]

month.coreness <- c()
cummulative_edge_list <- c()
cummulative.month.egdeList.dt <- NULL
for(month_index in 1:length(all_months) ){
  month.funding.events.dt <- funding.events.dt[Deal_Date_Year_Month == all_months[month_index], ]
  month.egdeList <- lapply(1:nrow(month.funding.events.dt), function(row_index)
  {
    row <- month.funding.events.dt[row_index, ]
    investors_csv <- row[, Investors]
    investors_csv <- gsub(", Inc", " Inc", investors_csv, fixed = TRUE)
    investors <- unlist(strsplit(investors_csv, ",", fixed = TRUE))
    if(length(investors) > 1){
      combinations <- combn(investors, 2, simplify = TRUE, FUN = trimws)
    }
  })
  
  listOfEdges   <- unlist(month.egdeList)
  if(!is.null(listOfEdges)){
    current.edge.list.dt <- data.table(matrix(listOfEdges, ncol = 2, byrow = TRUE))
    current.edge.list.dt <- current.edge.list.dt[, .(latest_date = all_months[month_index]), .(V1, V2)]
    if(is.null(cummulative.month.egdeList.dt)){
      cummulative.month.egdeList.dt <- current.edge.list.dt
    }
    else{
      cummulative.month.egdeList.dt <- merge(cummulative.month.egdeList.dt, current.edge.list.dt, all = TRUE)
    }
  }
  if(nrow(cummulative.month.egdeList.dt) > 0){
    cummulative.month.affiliation_graph <- graph.data.frame(cummulative.month.egdeList.dt, directed = FALSE)
    month.coreness <- c(month.coreness, mean(coreness(cummulative.month.affiliation_graph)))
  }
  else{
    month.coreness <- c(month.coreness, '0')
  }
}

nondecay.month.coreness <- month.coreness
plot(factor(all_months),nondecay.month.coreness, xlab="Month", ylab="Coreness", main = "Mean network coreness over the months")
lines(factor(all_months), nondecay.month.coreness, xlim=range(all_months), ylim=range(nondecay.month.coreness), pch=16)



# Question 2 B-------------
all_months <- unique(funding.events.dt[, Deal_Date_Year_Month])
#Sort all months
all_months <- all_months[order(all_months)]

month.coreness <- c()
cummulative_edge_list <- c()
cummulative.month.egdeList.dt <- NULL
decay_in_months <- 120
for(month_index in 1:length(all_months) ){
  month.funding.events.dt <- funding.events.dt[Deal_Date_Year_Month == all_months[month_index], ]
  month.egdeList <- lapply(1:nrow(month.funding.events.dt), function(row_index)
  {
    row <- month.funding.events.dt[row_index, ]
    investors_csv <- row[, Investors]
    investors_csv <- gsub(", Inc", " Inc", investors_csv, fixed = TRUE)
    investors <- unlist(strsplit(investors_csv, ",", fixed = TRUE))
    if(length(investors) > 1){
      combinations <- combn(investors, 2, simplify = TRUE, FUN = trimws)
    }
  })
  
  listOfEdges   <- unlist(month.egdeList)
  if(!is.null(listOfEdges)){
    current.edge.list.dt <- data.table(matrix(listOfEdges, ncol = 2, byrow = TRUE))
    current.edge.list.dt <- current.edge.list.dt[, .(latest_date = all_months[month_index]), .(V1, V2)]
    if(is.null(cummulative.month.egdeList.dt)){
      cummulative.month.egdeList.dt <- current.edge.list.dt
    }
    else{
      cummulative.month.egdeList.dt <- merge(cummulative.month.egdeList.dt, current.edge.list.dt, all = TRUE)
    }
  }
  if(nrow(cummulative.month.egdeList.dt) > 0){
    currentDate <- ymd(paste(all_months[month_index], "-01"))
    current_year <- year(currentDate)
    current_month <- month(currentDate)
    #The month date is set 01 and number of months between two dates is calculated.
    #Consider the decay
    cummulative.month.egdeList.dt <- cummulative.month.egdeList.dt[
      ( 
        (current_year - year(ymd(paste(latest_date, "-01")))) * 12 + 
          (current_month - month(ymd(paste(latest_date, "-01"))))) < decay_in_months, 
      ]
    if(nrow(cummulative.month.egdeList.dt) > 0){
      cummulative.month.affiliation_graph <- graph.data.frame(cummulative.month.egdeList.dt, directed = FALSE)
      month.coreness <- c(month.coreness, mean(coreness(cummulative.month.affiliation_graph)))
    }else{
      month.coreness <- c(month.coreness, '0')
    }
  }
  else{
    month.coreness <- c(month.coreness, '0')
  }
}

decay.month.coreness <- month.coreness
library(ggplot2)
plot(factor(all_months),decay.month.coreness, xlab="Month", ylab="Coreness", 
     main = "Mean network coreness over the months, with decay over 10 years considered")
lines(factor(all_months), decay.month.coreness, xlim=range(all_months), ylim=range(decay.month.coreness), pch=16)



# Question 3 ----------------------------------
#Find the clusters.
all_components <- components(cummulative.month.affiliation_graph)
gorder(cummulative.month.affiliation_graph)
