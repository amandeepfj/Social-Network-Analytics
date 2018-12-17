# list of all players
playercareer <- setorder(playerclubseason[, .(idP, idClub, idS)], idP)
playerlist <- unique(playerclubseason$idP)


# use value as criterion to pick top teams
# ranking is uncomparable among different leagues
clubval <- clubseason[value>0][,avg_value:=mean(value),by=idClub]
clubval <- setorder(unique(clubval,by=c("idClub")),-avg_value)
topclubs<-clubval[1:20,]
topclubs <- merge(topclubs,club[,c("idClub","nameClub")],by = "idClub")
topclubs <- setorder(topclubs,-avg_value)

# create the transfer list
transfer <- data.frame(NULL)
temp <- lapply(playerlist, function(p){
  path_temp <- setorder(playerclubseason[idP==3],idS)
  path_temp <- path_temp$idClub
  i <- 1
  while (i < length(path_temp)) {
    if(path_temp[i] != path_temp[i+1]){
      # use value increase as weight
      # val<-(clubval[idClub==path[i+1]]$avg_value-clubval[idClub==path[i]]$avg_value)/1000000
      val <- (clubval[idClub==path_temp[i+1]]$avg_value)/1000000
      transfer <<- rbind(transfer, list(path_temp[i],path_temp[i+1],val))
    }
    i<-i+1
  }
})
colnames(transfer) <- c("from","to","val_increase")
transfer<-as.data.table(unique(transfer))



