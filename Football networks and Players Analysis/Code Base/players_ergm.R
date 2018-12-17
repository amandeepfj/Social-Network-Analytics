setwd("C:/US Drive/MSBA/Social Network Analytics/Assignments/Group Project")
library(data.table)
player_edges <- fread("playeredge_all.csv")
ergm_player_edges <- player_edges[idS==5]

this_year <- 2018
player_1_country <- merge(ergm_player_edges, player, by.y = "idP", 
                          by.x = "player1")[, .(player1, player1_country = idC, player1_age = (this_year - as.numeric(birthDate)))]
player_1_country <- player_1_country[, .(player1_country = min(player1_country), player1_age = max(player1_age)), by = (player1)]

player_2_country <- merge(ergm_player_edges, player, by.y = "idP", 
                          by.x = "player2")[, .(player2, player2_country = idC, player2_age = (this_year - as.numeric(birthDate)))]
player_2_country <- player_2_country[, .(player2_country = min(player2_country), player2_age = max(player2_age)), by = (player2)]

ergm_player_edges <- merge(ergm_player_edges, player_1_country, by = "player1")
ergm_player_edges <- merge(ergm_player_edges, player_2_country, by = "player2")

ergm_player_edges[, player1 := as.character(player1)]
ergm_player_edges[, player2 := as.character(player2)]

age_diff <- ergm_player_edges[, abs(player1_age - player2_age)]

same_country <- ergm_player_edges[, (player1_country == player2_country)]
just_edges <- ergm_player_edges[, .(player1, player2)] 
library(network)
players_n <- network(just_edges, vertex.attr = list(ergm_player_edges[,weights]), 
                     vertex.attrnames = c("weights"), directed = FALSE)

vertices_in_network <- get.node.attr(nw = players_n, attrname = "vertex.names")

setkey(player, idP)
playe_countries <- player[as.integer(vertices_in_network), idC]
players_n %v% 'countryID' <- playe_countries
length(players_n %v% 'countryID')
table(players_n %v% 'countryID')

playe_ages <- player[as.integer(vertices_in_network), this_year - as.numeric(birthDate)]
playe_ages[playe_ages <= 20] <- 1
playe_ages[(playe_ages > 20) & (playe_ages <= 28)] <- 2
playe_ages[(playe_ages > 28) & (playe_ages <= 38)] <- 3
playe_ages[playe_ages > 38] <- 4
players_n %v% 'age' <- playe_ages
length(players_n %v% 'age')
table(players_n %v% 'age')


playingPosition <- player[as.integer(vertices_in_network), playingPosition]
players_n %v% 'playingPosition' <- playingPosition
length(players_n %v% 'playingPosition')
table(players_n %v% 'playingPosition')


library(ergm)
ergm_2 <- ergm(players_n ~ edges + triangle + nodematch("countryID") + nodematch("age", diff = T) 
               + nodematch("playingPosition", diff = T))
summary(ergm_2)
# test the gof.ergm function
gofflo <- gof(ergm_2)
gofflo

# Plot all three on the same page
# with nice margins
par(mfrow=c(1,3))
par(oma=c(0.5,2,1,0.5))
plot(gofflo)

# And now the log-odds
plot(gofflo, plotlogodds=TRUE)


#flo.03.gof.model <- ?gof(players_n ~ ergm_2)

