# Creating edge list -----------------------
edgeList <- lapply(1:nrow(funding.events.dt), function(row_index)
{
  row <- funding.events.dt[row_index, ]
  investors_csv <- row[, Investors]
  investors_csv <- gsub(", Inc", " Inc", investors_csv, fixed = TRUE)
  investors <- unlist(strsplit(investors_csv, ",", fixed = TRUE))
  if(length(investors) > 1){
    combinations <- combn(investors, 2, simplify = TRUE, FUN = trimws)
  }
}
)
edgeList.dt <- data.table(matrix(unlist(edgeList), ncol = 2, byrow = TRUE))
rm(edgeList)
edgeList.dt <- edgeList.dt[, .(Weight = .N), .(V1, V2)]
nrow(edgeList.dt)

# Create iGraph object from edge list dt -----------------
library(igraph)
affiliation_graph <- graph.data.frame(edgeList.dt, directed = FALSE)
#plot(affiliation_graph)

# Question 1 ---------------------
closeness_centralities <- closeness(affiliation_graph)
max_closeness_node <- which.max(closeness_centralities)
closeness(affiliation_graph, vids = max_closeness_node)

shortestPaths <- data.table(shortest.paths(affiliation_graph))
#Replace Inf shortest path by number of vertices
sum(shortestPaths == Inf)
shortestPaths[shortestPaths==Inf] <- gorder(affiliation_graph)
average_mean_short_paths <- shortestPaths[ , lapply(.SD, mean)]
which.min(average_mean_short_paths)

average_mean_short_paths[ , sum_of_avg_paths := sum(.SD)]
sum_of_avg_paths <- average_mean_short_paths[, sum_of_avg_paths]
average_mean_short_paths[ , sum_of_avg_paths := NULL]
number_of_nodes <- length(colnames(average_mean_short_paths))
mean_shortest_distance <- sum_of_avg_paths/number_of_nodes
mean_shortest_distance

vc_degrees <- degree(affiliation_graph)
betweenness_values <- betweenness(graph = affiliation_graph)
# Question 4 -------------------
vc_outcomes <- fread("Venture_capital_firm_outcomes.csv", nrow = 2000)
vc_outcomes <- vc_outcomes[,.(Number_of_Investments = sum(successful_investments), 
                              Size_Deals = sum(monetary_size_deals_year_usdmn), Out_of_Business = out_of_business),
                           .(firm_name, year)]

{ # Clean the venture capital names
  vc_outcomes <- vc_outcomes[, firm_name := gsub(", Inc.", " Inc", firm_name, fixed = TRUE) ]
  vc_outcomes <- vc_outcomes[, firm_name := gsub(", Inc", " Inc", firm_name, fixed = TRUE) ]
  vc_outcomes <- vc_outcomes[, firm_name := gsub(",Inc.", " Inc", firm_name, fixed = TRUE) ]
  vc_outcomes <- vc_outcomes[, firm_name := gsub(",Inc", " Inc", firm_name, fixed = TRUE) ]
  vc_outcomes <- vc_outcomes[, firm_name := gsub(", Limited.", " Limited", firm_name, fixed = TRUE) ]
  vc_outcomes <- vc_outcomes[, firm_name := gsub(", Ltd.", " Ltd", firm_name, fixed = TRUE) ]
  vc_outcomes <- vc_outcomes[, firm_name := gsub(", Ltd", " Ltd", firm_name, fixed = TRUE) ]
  vc_outcomes <- vc_outcomes[, firm_name := gsub(", LTD.", " Ltd", firm_name, fixed = TRUE) ]
  vc_outcomes <- vc_outcomes[, firm_name := gsub(", LTD.", " Ltd", firm_name, fixed = TRUE) ]
  vc_outcomes <- vc_outcomes[, firm_name := gsub(", LTD", " Ltd", firm_name, fixed = TRUE) ]
  vc_outcomes <- vc_outcomes[, firm_name := gsub(", LLC.", " LLC", firm_name, fixed = TRUE) ]
  vc_outcomes <- vc_outcomes[, firm_name := gsub(", LLC", " LLC", firm_name, fixed = TRUE) ]
}

setkey(x = vc_outcomes)
lapply(names(closeness_centralities), function(vc_name)
{
  closness_value <- closeness_centralities[vc_name]
  degree_value <- vc_degrees[vc_name]
  betweenness_value <- betweenness_values[vc_name]
  vc_outcomes <- vc_outcomes[firm_name == vc_name, c("Closensess_Centrality", "Degree_Value", "Betweenness_values") 
                             := list(closness_value, degree_value, betweenness_value), ]
})


# Part A ------------------------------------------------------------------
cc <- vc_outcomes[!is.na(Closensess_Centrality), .(Closensess_Centrality, Number_of_Investments)]
plot(log(cc[, Closensess_Centrality]), xlim = c(-15.645, -15.638), cc[, Number_of_Investments], 
     xlab="Log of closeness centrality ", ylab="Number of Investments", pch=19)

degrees <- vc_outcomes[!is.na(Degree_Value), .(Degree_Value, Number_of_Investments)]
plot(log(degrees[, Degree_Value]), degrees[, Number_of_Investments], 
     xlab="Log of Degree of venture capital vertex", ylab="Number of Investments", pch=19)

betweennesses <- vc_outcomes[!is.na(Betweenness_values), .(Betweenness_values, Number_of_Investments)]
plot(log(betweennesses[, Betweenness_values]), betweennesses[, Number_of_Investments], 
     xlab="Log of Betweenness of venture capital vertex", ylab="Number of Investments", pch=19)

# Part B ------------------------------------------------------------------

cc_success <- vc_outcomes[!is.na(Closensess_Centrality) & Out_of_Business == 0, Closensess_Centrality] # returns the density data 
cc_out_of_b <- vc_outcomes[!is.na(Closensess_Centrality) & Out_of_Business == 1, Closensess_Centrality] 
plot(density(sqrt(cc_success)), xlim = c(0.000399, 0.000403), 
     main = "Kernel Density Plot of Square root Closeness Centrality") # plots the results
lines(density(sqrt(cc_out_of_b)), col = "red")
legend("topright", c("Not out of Business","Out of Business"), fill=c("black", "red"))

cc_success <- vc_outcomes[!is.na(Degree_Value) & Out_of_Business == 0, Degree_Value] # returns the density data 
cc_out_of_b <- vc_outcomes[!is.na(Degree_Value) & Out_of_Business == 1, Degree_Value] 
plot(density(cc_success), main = "Kernel Density Plot of Degree Centrality") # plots the results
lines(density(cc_out_of_b), col = "red")
legend("topright", c("Not out of Business","Out of Business"), fill=c("black", "red"))

cc_success <- vc_outcomes[!is.na(Betweenness_values) & Out_of_Business == 0, Betweenness_values] # returns the density data 
cc_out_of_b <- vc_outcomes[!is.na(Betweenness_values) & Out_of_Business == 1, Betweenness_values] 
plot(density(log(cc_success)), main = "Kernel Density Plot of Log of Betweenness Centrality") # plots the results
lines(density(log(cc_out_of_b)), col = "red")
legend("topright", c("Not out of Business","Out of Business"), fill=c("black", "red"))

