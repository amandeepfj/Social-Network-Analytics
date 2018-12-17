#This file loads the tables in environment!
library(RMySQL)
library(data.table)
library(igraph)

#if you get error here run below command in MySQL Workbench
#ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY 'root';
mydb <- dbConnect(RMySQL::MySQL(), 
                  user='aman', 
                  password='amandeep', 
                  dbname='footballnetwork', 
                  host='mysqlinstance.clgkqw3dkhqv.us-east-2.rds.amazonaws.com')

row_limit <- -1
rs <- dbSendQuery(mydb, "select * from club")
club <- fetch(rs, n=row_limit)
dbClearResult(rs)
club <- data.table(club)

rs <- dbSendQuery(mydb, "select * from clubseason")
clubseason <- fetch(rs, n=row_limit)
dbClearResult(rs)
clubseason <- data.table(clubseason)

rs <- dbSendQuery(mydb, "select * from countries")
countries <- fetch(rs, n=row_limit)
dbClearResult(rs)
countries <- data.table(countries)

rs <- dbSendQuery(mydb, "select * from leagues")
leagues <- fetch(rs, n=row_limit)
dbClearResult(rs)
leagues <- data.table(leagues)

rs <- dbSendQuery(mydb, "select * from season")
season <- fetch(rs, n=row_limit)
dbClearResult(rs)
season <- data.table(season)
season$inflation_rate <- c(0.02, 0.03, 0.04, 0.02)

rs <- dbSendQuery(mydb, "select * from player")
player <- fetch(rs, n=row_limit)
dbClearResult(rs)
player <- data.table(player)

rs <- dbSendQuery(mydb, "select * from playerclubseason")
playerclubseason <- fetch(rs, n=row_limit)
dbClearResult(rs)
playerclubseason <- data.table(playerclubseason)

# shutdown connection
dbDisconnect(mydb)       #mydb is connection name.


# Create objects for perspective players --------------
# Create players edge list
player_edges <- merge(playerclubseason, playerclubseason, by = c("idS", "idClub"), 
                      allow.cartesian = TRUE)[, .(idP.x, idP.y, idS, idClub, playerValue.x, playerValue.y)]
player_edges <- player_edges[idP.x != idP.y]

#create players as undirected edges.
player_edges <- player_edges[, .(sanity_check_number = .N, 
                                 player1_value = max(playerValue.x), player2_value = max(playerValue.y)), 
                             by=list(player1 = pmin(idP.x, idP.y), player2 = pmax(idP.x, idP.y), idS, idClub)]

#get rid of NA and -1's as we already grouped and assigned max values.
player_edges <- player_edges[(!is.na(player1_value)) & (player1_value != -1) & 
                               (!is.na(player2_value)) & (player2_value != -1), ]

#This plot should be a line at 2.
#plot(player_edges[, sanity_check_number])

player_edges <- merge(player_edges, season, by = "idS")
# how to calculate weights?
player_edges[, weights := (player1_value + player2_value) * (1 + inflation_rate * (15 - idS))/100000]



players_graph <- graph_from_data_frame(d = player_edges[, .(player1, player2)], directed = FALSE)
E(players_graph)$weight <- player_edges[, weights]


# Remove unwanted objects from environment ---------------------------
rm(list = setdiff(ls(), c("club", "clubseason", "countries", "leagues", "playerclubseason", "player", "season", 
                          "players_graph", "player_edges")))


