# Create Clean Undirected Graph & Question 2--------------------------------------------------------------
setwd("C:/US Drive/MSBA/Social Network Analytics/Assignments/Assignment 1/")
library(igraph)
dataset <- read.csv("classroom_social_and_task_network.csv")
#dataset <- dataset[sample(nrow(dataset)),]
head(dataset, n = 10)

{
  egos <- c()
  alters <- c()
  social_values <- c()
  task_values <- c()
  ties_strength <- c() 
  reverse_present <- c()
  for (row in 1:nrow(dataset)) {
    ego <- dataset[row, "ego"]
    alter  <- dataset[row, "alter"]
    social_value <- dataset[row, "social_tie"]
    task_value <- dataset[row, "task_tie"]
    
    if(social_value != 0 || task_value != 0){
      
      rev_relation <- dataset[which((dataset$ego == alter) & (dataset$alter == ego)), ]
      rev_social_value <- rev_relation[,"social_tie"]
      rev_task_value <- rev_relation[,"task_tie"]
      
      debug <- FALSE
      if(ego == 21 & alter == 19){
        print('found')
        debug = TRUE
        print(rev_relation)
      }
      
      if(rev_social_value > social_value){
        social_value <- rev_social_value
      }
      if(rev_task_value > task_value){
        task_value <- rev_task_value
      }
      
      is_reverse_present <- FALSE
      for(value in which(egos==alter)){
        if(alters[value] == ego){
          print("Reverse")
          print(ego)
          print(alter)
          is_reverse_present <- TRUE
          reverse_present[value] <- "YES"
        }
      }
      if(!is_reverse_present)
      {
        egos <- c(egos, ego)
        alters <- c(alters, alter)
        social_values <- c(social_values, social_value)
        task_values <- c(task_values, task_value)
        
        reverse_present <- c(reverse_present, "NO")
      }
    }
  }
}
clean_undirected_graph_df <- data.frame(egos, alters, social_values, task_values, reverse_present)

(mean_social_tie <- mean(clean_undirected_graph_df[which(clean_undirected_graph_df$social_values !=0), 'social_values']))
(mean_task_tie <- mean(clean_undirected_graph_df[which(clean_undirected_graph_df$task_values !=0), 'task_values']))

#Setting the strength of each edge based on it's mean
{
  ties_strength_mean <- c() 
  for (row in 1:length(clean_undirected_graph_df)) {
    ego <- clean_undirected_graph_df[row, "egos"]
    alter  <- clean_undirected_graph_df[row, "alters"]
    social_value <- clean_undirected_graph_df[row, "social_values"]
    task_value <- clean_undirected_graph_df[row, "social_values"]
    
    if(social_value != 0 || task_value != 0){
      
      is_strong <- FALSE
      if(social_value > mean_social_tie){
         is_strong <- TRUE
      }
      if(task_value > mean_task_tie){
        is_strong <- TRUE
      }
      if(is_strong){
        ties_strength_mean <- c(ties_strength_mean, 'S')
      }
      else{
        ties_strength_mean <- c(ties_strength_mean, 'W')
      }
      
    }
  }
}

(median_social_tie <- median(clean_undirected_graph_df[which(clean_undirected_graph_df$social_values !=0), 'social_values']))
(median_task_tie <- median(clean_undirected_graph_df[which(clean_undirected_graph_df$task_values !=0), 'task_values']))

{
  ties_strength_median <- c() 
  for (row in 1:length(clean_undirected_graph_df)) {
    ego <- clean_undirected_graph_df[row, "egos"]
    alter  <- clean_undirected_graph_df[row, "alters"]
    social_value <- clean_undirected_graph_df[row, "social_values"]
    task_value <- clean_undirected_graph_df[row, "social_values"]
    
    if(social_value != 0 || task_value != 0){
      
      is_strong <- FALSE
      if(social_value > median_social_tie){
        is_strong <- TRUE
      }
      if(task_value > median_task_tie){
        is_strong <- TRUE
      }
      if(is_strong){
        ties_strength_median <- c(ties_strength_median, 'S')
      }
      else{
        ties_strength_median <- c(ties_strength_median, 'W')
      }
      
    }
  }
}

clean_undirected_graph_df <- data.frame(egos, alters, social_values, task_values, ties_strength_mean, ties_strength_median, reverse_present)

clean_undirected_graph <- graph_from_data_frame(d = clean_undirected_graph_df, directed = FALSE)
plot(clean_undirected_graph, edge.label = clean_undirected_graph_df$ties_strength_mean)

social_clean_undirected_graph_df <- clean_undirected_graph_df[which(clean_undirected_graph_df$social_values != 0),]
social_clean_undirected_graph <- graph_from_data_frame(d = social_clean_undirected_graph_df, directed = FALSE)
plot(social_clean_undirected_graph, edge.label = social_clean_undirected_graph_df$social_values)

task_clean_undirected_graph_df <- clean_undirected_graph_df[which(clean_undirected_graph_df$task_values != 0),]
task_clean_undirected_graph <- graph_from_data_frame(d = task_clean_undirected_graph_df, directed = FALSE)
plot(task_clean_undirected_graph, edge.label = task_clean_undirected_graph_df$task_values)


# Question 3 -------------------
social_clean_undirected_graph_df$social_betweenness <- edge_betweenness(social_clean_undirected_graph, directed = FALSE)
task_clean_undirected_graph_df$task_betweenness <- edge_betweenness(task_clean_undirected_graph, directed = FALSE)

# Question 5 --------
g <- erdos.renyi.game(5, 1)
degree(g)
plot(g)

g <- make_star(5, mode = c("undirected"), center = 1)
plot(g)
