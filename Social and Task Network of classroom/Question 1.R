setwd("C:/US Drive/MSBA/Social Network Analytics/Assignments/Assignment 1/")
library(igraph)
dataset <- read.csv("classroom_social_and_task_network.csv")
#dataset <- dataset[sample(nrow(dataset)),]
head(dataset, n = 10)

#Graph with all the vertices.
graph_all_vertices <- graph_from_data_frame(d = dataset)

#
createGraphSubset <- function(type_of_tie, directed = FALSE){
  tie_dataframe <- subset(dataset, dataset[type_of_tie] != 0, )
  graphSubset <- graph_from_data_frame(d = tie_dataframe, vertices = V(graph_all_vertices)$name, 
                                       directed = directed)
  return(graphSubset)
}

type_social_tie = 'social_tie'
type_task_tie = 'task_tie'

social_ties_graph <- createGraphSubset(type_of_tie = type_social_tie, directed = TRUE)
task_ties_graph <- createGraphSubset(type_of_tie = type_task_tie, directed = TRUE)

# Question 1 --------------------------------------------------------------
measures <- c("Indgree", "Outdegree", "Closeness", "Betweenness", "Page Rank")
Corelations <- c()
(indegree_social = degree(graph = social_ties_graph, mode = c("in")))
(indegree_task = degree(graph = task_ties_graph, mode = c("in")))
Corelations <- c(Corelations, cor(x = indegree_task, y = indegree_social))

outdegree_social = degree(graph = social_ties_graph, mode = c("out"))
outdegree_task = degree(graph = task_ties_graph, mode = c("out"))
Corelations <- c(Corelations, cor(x = outdegree_task, y = outdegree_social))

closeness_social = closeness(graph = social_ties_graph)
closeness_task = closeness(graph = task_ties_graph)
Corelations <- c(Corelations, cor(x = closeness_task, y = closeness_social))

betweenness_social = betweenness(graph = social_ties_graph)
betweenness_task = betweenness(graph = task_ties_graph)
Corelations <- c(Corelations, cor(x = betweenness_task, y = betweenness_social))

pageRank_social = page_rank(graph = social_ties_graph)
pageRank_task = page_rank(graph = task_ties_graph)
Corelations <- c(Corelations, cor(x = pageRank_social$vector, y = pageRank_task$vector))

measures_cors_df <- data.frame(measures, Corelations)

#measures_vertex_df <- data.frame(c(1:22), indegree_social, outdegree_social, closeness_social, betweenness_social, pageRank_social)

# Creating a combined graph -----------

social_ties_graph <- createGraphSubset(type_of_tie = type_social_tie, directed = TRUE)
task_ties_graph <- createGraphSubset(type_of_tie = type_task_tie, directed = TRUE)

setEdgeAttributes <- function(graphSubset, type_of_tie, mean_weight_value){
  tie_dataframe <- subset(dataset, dataset[type_of_tie] != 0, )
  for(weight_value_index in c(1:length(tie_dataframe[,type_of_tie]))){
    weight_value <- tie_dataframe[weight_value_index, type_of_tie]
    edge_color <- 'red'
    if(type_of_tie == type_social_tie){
      edge_color <- 'blue'
    }
    graphSubset <- set_edge_attr(graphSubset, "weight", index = E(graphSubset)[weight_value_index],value= weight_value)
    graphSubset <- set_edge_attr(graphSubset, "color", index = E(graphSubset)[weight_value_index],value= edge_color)
    graphSubset <- set_edge_attr(graphSubset, "type_of_tie", index = E(graphSubset)[weight_value_index],value= type_of_tie)
  }
  return(graphSubset)
}

social_ties_graph <- setEdgeAttributes(social_ties_graph, type_social_tie, mean_social_tie)
layout <-layout_as_star(social_ties_graph)
plot(social_ties_graph, layout = layout)

task_ties_graph <- setEdgeAttributes(task_ties_graph, type_task_tie, mean_task_tie)
layout <-layout_as_star(task_ties_graph)
plot(task_ties_graph, layout = layout)

create_combined_graph <- function(social_ties_graph, task_ties_graph){
  combined_graph <- social_ties_graph
  for (task_edge_index in c(1:length(E(task_ties_graph)))) {
    task_edge <- E(task_ties_graph)[task_edge_index]
    fromVertex <- ends(task_ties_graph, task_edge)[1,1]
    toVertex <- ends(task_ties_graph, task_edge)[1,2]
    if(combined_graph[fromVertex, toVertex] == 0){
      combined_graph <- add.edges(combined_graph, c(fromVertex, toVertex), attr = edge.attributes(task_ties_graph, task_edge))
    }
    else{
      present_edge <- combined_graph[fromVertex, toVertex]
      combined_graph <- set_edge_attr(combined_graph, "color", index = present_edge, value= 'green')
      combined_graph <- set_edge_attr(combined_graph, "type_of_tie", index = present_edge, value= 'both')
    }
  }
  return(combined_graph)
}

combined_graph <- create_combined_graph(social_ties_graph, task_ties_graph)
layout <-layout_as_star(combined_graph)
plot(combined_graph, layout = layout)
