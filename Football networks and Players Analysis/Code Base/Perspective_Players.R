#Use above created graph for network analysis
#players_graph and player_edges already loaded in memory!
load("Load_Data.R")

players_graph
player_edges
 

# playerclubseason[which(playerclubseason$idS == 2),]

# unique(playerclubseason$idClub)

func_season <- function(i){
  playeredge <- setDT(playerclubseason[, 1:3])
  
  playeredge <- playeredge[which(playeredge$idS==i),]
  
  player_connection <- playeredge[, as.data.table(t(combn(idP, 2))), by= c('idClub','idS')]
  
  player_connection <- unique(player_connection)
  
  # player_connection <- player_connection[-which(player_connection$V1 == player_connection$V2),]
  
  colnames(player_connection) = c('idClub','season', 'player1', 'player2')
  
  playeredge_i <- player_connection
  
  return(playeredge_i)
}

# create and save all season tables
for (x in 1:15) {
  df <- func_season(x)
  assign(paste("season",x, sep="_"),df)
}

season_1


# combine all seasons
playeredge_all <- data.table()
for (j in seq(unique(playerclubseason$idS))) {
  playeredge_temp <- func_season(j)
  playeredge_all <- rbind(playeredge_all, playeredge_temp)
}

# merging data to edgelist

# playeredge <- right_join(player, playeredge, by = c("idP"="player1")) 
# 
# playeredge <- as.data.table(playeredge)

playeredge_all <- as.data.table(left_join(playeredge, playerclubseason[,c('idP','idS','playerValue')], by = c('player1'='idP','idS')))

playeredge_all <- unique(playeredge_all)

playeredge_all <-  as.data.table(left_join(playeredge_all, playerclubseason[,c('idP','idS','playerValue')], by = c('player2'='idP','idS')))

playeredge_all <- unique(playeredge_all)

playeredge_all <- playeredge_all[-which(playeredge_all$playerValue == -1) ,]

playeredge_all <- playeredge_all[-which(is.na(playeredge_all$playerValue)),]
# create weight



# playerclubseason[playerclubseason$idP == 1,]
# playeredge[playeredge$idP ==1 ,]
# player[player$idP==4,]

names(playeredge_all)[names(playeredge_all) == "status"] = 'weights'

#inflation 0.02
playeredge_all[, weights := (playerValue.x + playerValue.y) * (1 + 0.02 * (15 - idS))/100000] # how to calculate weights?

names(playeredge_all)[names(playeredge_all) == "weights"] = 'weights'

playeredge_all[player1 == 1,]
# assign weights for connection in each season

func_weight <- function(h){
  playeredge_h <- playeredge_all[which(playeredge_all$idS == h),]
  
  g_h <- graph.data.frame(playeredge_h[,2:3])
  
  is.weighted(g_h)
  
  g_h <- set_edge_attr(g_h,'weight', value= playeredge_h$weights)
  
  is.weighted(g_h)
  
  return(g_h)
}

# create and save all season_weight tables
for (h in 1:15) {
  df <- func_weight(h)
  assign(paste("g",h, sep="_"),df)
}


# plot the connection 
# change _1 for different season



# calculate centralities
# change_1 for different season
season_1$degree <- degree(g_1, v = V(g_1), mode = "all")
season_1$close <- closeness(g_1, vids = V(g_1), mode = "total")
season_1$between <- betweenness(g_1, v = V(g_1), directed = FALSE)
season_1$pg <- page_rank(g_1, algo = c("prpack", "arpack", "power"), vids = V(g_1))

# Are able to see the improvement of each players according to different seasons

# finding according to existing data without any analysis

playername_club <- left_join(playerclubseason[,1:4],player[,c(1,3:4)], by = 'idP' )

playername_club <- data.table(playername_club) 

playername_club <- playername_club[which(playername_club$playerValue!='NA'),]

setorder(playername_club,-playerValue)

head(playername_club,20)

top20 <- head(playername_club[, .(.N, max(playerValue)), by = .(idP, lastName, firstName)], 20)

tail20 <- tail(playername_club[, .(.N, max(playerValue)), by = .(idP, lastName, firstName)], 20)

colnames(top20) <- c('idP'  ,  'lastName' , 'firstName' , 'count_year', 'playerValue')

# color specific node 
as.numeric(V(g_8)["88103"])

V(g_8)$color[5391] <- "red"

V(g_5)[name==28003]

playeredge_all$color = "orange"

playeredge_all$color[playeredge_all$playerValue.x >100000]='red'

median(playeredge_all$playerValue.x)

plot(g_8, edge.arrow.size = .2, vertex.label=NA, vertex.size = 8, vertex.color=playeredge_all$color) 

playerclubseason[which(playerclubseason$idS==8&playerclubseason$idClub==23611),]

playeredge_all[which(playeredge_all$player1==28003&playeredge_all$idS==8),]

playeredge_all[which(playeredge_all$idS==8&playeredge_all$idClub==23611),]

playeredge_all[which(playeredge_all$idS==8&playeredge_all$player1==597),]

playerclubseason[which(playerclubseason$idClub==23611),]

# eliminate weight

G_playeredge <- playeredge_all[,1:3]

G_playeredge <- G_playeredge[which(G_playeredge$idS==8),]

G_playeredge <- graph.data.frame(G_playeredge)

#plot(G_playeredge, edge.arrow.size = .2, vertex.label=NA, vertex.size = 


