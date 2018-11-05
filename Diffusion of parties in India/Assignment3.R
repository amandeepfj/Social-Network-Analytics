setwd("/home/rstudio/Social Network Analysis/Assignment 3/")
rm(list = ls(all = TRUE))
# Data Loading and Creation -----------------------------

start_time <- Sys.time()
library(data.table)
library(igraph)
library(zoo)
library(lubridate)

dist_info <- fread("district_information.csv")
unique_dist_state <- unique(dist_info[, .(district, state)])

#We need this to group by election year later
dist_info <- dist_info[, election_year := year]

#This is to keep track of normal, drought, and floods
rain_info <- fread("rain_information.csv")
rain_info <- rain_info[, normal := 0]
rain_info <- rain_info[(spi >= -1) & (spi <= 1), normal := 1]
rain_info <- rain_info[, drought := 0]
rain_info <- rain_info[spi < -1, drought := 1]
rain_info <- rain_info[, flood := 0]
rain_info <- rain_info[spi > 1, flood  := 1]

#right join dist_info with rain_info
setkey(dist_info, district, year)
setkey(rain_info, district, year)
dist_rain_info <- dist_info[rain_info, .(district, state, year, election_year, rain, spi, normal, drought, flood,
                                         new_parties, total_parties, political_concentration)]
setkey(dist_rain_info, district, year)
#sort by district and year
dist_rain_info <- dist_rain_info[order(district, year),]

#Update election year column
current_election_yr <- NA
current_election_dist_name <- NA
state_name <- NA
for(row_index in nrow(dist_rain_info):1){
  #for(row_index in 1000:1){
  row <- dist_rain_info[row_index, ]
  temp_dist_name <- row[, district]
  if(!is.na(row[, election_year])){
    current_election_yr <- row[, election_year]
    state_name <- row[, state]
    current_election_dist_name <- temp_dist_name
  } 
  else if((!is.na(current_election_dist_name)) && (current_election_dist_name == temp_dist_name)){
    dist_rain_info <- dist_rain_info[row_index, c("election_year", "state") := list(current_election_yr, state_name)]
  }
}

dist_rain_info <- dist_rain_info[, 
                                 c("avg_rain", "avg_spi", "normal_cnt", "drought_cnt", "flood_cnt") := 
                                   list(mean(rain, na.rm = TRUE), mean(spi, na.rm = TRUE), 
                                        sum(normal), sum(drought), sum(flood)), 
                                 by = .(district, election_year)]

#Election year NA means that Rain data for the district is missing hence the election year was NA or there 
#were no elections after these years.
election_dist_rain_info <- dist_rain_info[!is.na(election_year), ]

avg_rain_each_dist_each_elect_year <- election_dist_rain_info[!is.na(new_parties), ]

border_info <- fread("border_information.csv")
#Remove reverse pairs duplicates if any
border_info <- border_info[
  border_info[, .I[1], by = list(pmin(focal_district, district), pmax(focal_district, district))]$V1, ]

border_info_graph <- graph.data.frame(border_info, directed = FALSE)
for(row_index in 1:nrow(avg_rain_each_dist_each_elect_year)){
  #for(row_index in 1:2){
  row <- avg_rain_each_dist_each_elect_year[row_index, ]
  district <- row[, district]
  temp_election_year <- row[, election_year]
  tryCatch(
    {  
      neighbors <- neighbors(border_info_graph, district)
      # temp_DT <- avg_rain_each_dist_each_elect_year[district %in% neighbors$name & election_year == temp_election_year, 
      #                   .(neighbor_avg_rain = mean(avg_rain, na.rm = TRUE), neighbor_avg_spi = mean(avg_spi, na.rm = TRUE), 
      #                     neighbor_normal_cnt = sum(normal_cnt), neighbor_drought_cnt = sum(drought_cnt), 
      #                     neighbor_flood_cnt = sum(flood_cnt), neighbor_cnt = .N)]
      # if(nrow(temp_DT) > 0){
      #   avg_rain_each_dist_each_elect_year[row_index, 
      #                                    c("neighbor_avg_rain", "neighbor_avg_spi", 
      #                                      "neighbor_normal_cnt", "neighbor_drought_cnt", "neighbor_flood_cnt", "neighbor_cnt") := 
      #                                      list(temp_DT[, neighbor_avg_rain], temp_DT[, neighbor_avg_spi],
      #                                           temp_DT[, neighbor_normal_cnt], temp_DT[, neighbor_drought_cnt], 
      #                                           temp_DT[, neighbor_flood_cnt], temp_DT[, neighbor_cnt])]
      # }
      # 
      avg_rain_each_dist_each_elect_year[district %in% neighbors$name & election_year == temp_election_year,
                                         c("neighbor_avg_rain", "neighbor_avg_spi",
                                           "neighbor_normal_cnt", "neighbor_drought_cnt", "neighbor_flood_cnt", "neighbor_cnt") :=
                                           list(mean(avg_rain, na.rm = TRUE), mean(avg_spi, na.rm = TRUE),
                                                sum(normal_cnt), sum(drought_cnt), sum(flood_cnt), .N)]
    },
    error=function(cond) {
      print(paste(cond, " District Name = ", district))
    })
}


#sort by district and year
avg_rain_each_dist_each_elect_year <- avg_rain_each_dist_each_elect_year[order(district, year),]

cols_to_lag <- c("avg_rain","avg_spi", "normal_cnt", "drought_cnt", "flood_cnt",
                 "neighbor_avg_rain", "neighbor_avg_spi",
                 "neighbor_normal_cnt", "neighbor_drought_cnt", "neighbor_flood_cnt", "election_year")
lagColNames <- paste("lag", cols_to_lag, sep="_")
avg_rain_each_dist_each_elect_year <-
  avg_rain_each_dist_each_elect_year[, c(lagColNames) :=  shift(.SD), by = district, .SDcols = cols_to_lag]
#Another way to lag
#avg_rain_each_dist_each_elect_year[, (lagColNames):=lapply(.SD, function(x) c(NA, x[-.N])), by = district,.SDcols=cols_to_lag]

avg_rain_each_dist_each_elect_year <- avg_rain_each_dist_each_elect_year[, years_unto_election := 
                                                                           (normal_cnt + drought_cnt + flood_cnt)]

setcolorder(avg_rain_each_dist_each_elect_year,
            c(
              "district",
              "state",
              "election_year",
              "years_unto_election",
              "avg_rain", "avg_spi",
              "normal_cnt", "drought_cnt", "flood_cnt",
              "neighbor_avg_rain", "neighbor_avg_spi", "neighbor_cnt",
              "neighbor_normal_cnt", "neighbor_drought_cnt", "neighbor_flood_cnt",
              lagColNames
            ))

cleanDT <- avg_rain_each_dist_each_elect_year


# NEEDED FOR QUESTION 5
new_parties_by_candidate <- fread("new_parties_in_each_district_by_candidate.csv")
unique_election_years <- unique(cleanDT[, election_year])
unique_election_years <- unique_election_years[order(unique_election_years)]
for(row_index in 1:nrow(cleanDT)){
  row <- cleanDT[row_index, ]
  temp_district <- row[, district]
  temp_election_year <- row[, election_year]
  temp_prev_election_year <- row[, lag_election_year]
  tryCatch(
    {
      if(!is.na(temp_prev_election_year)){
        new_parties <- new_parties_by_candidate[temp_election_year == year & temp_district == district,][, party_name]
        neighbors <- neighbors(border_info_graph, district)
        index_in_unique_yrs <- match(c(temp_prev_election_year),unique_election_years)
        prev_election_years <- unique_election_years[1:index_in_unique_yrs]
        from_neighbors <- 0
        for(prev_election_year in prev_election_years){
          for(new_party in new_parties){
            in_neighbors <- new_parties_by_candidate[party_name == new_party & prev_election_year == year 
                                                     & district %in% neighbors$name, ]
            if(nrow(in_neighbors) > 0){
              from_neighbors <- from_neighbors + 1
              new_parties <- new_parties[-match(c(new_party),new_parties)]
              break
            }
          }
        }
        cleanDT <- cleanDT[row_index, from_neighbors_cnt := from_neighbors]
      }
    },
    error=function(cond) {
      print(paste(cond, " District Name = ", district))
    })
}
cleanDT <- cleanDT[is.na(from_neighbors_cnt), from_neighbors_cnt := 0]

end_time <- Sys.time()
print(end_time - start_time)


#cleaning env variables
to.del <- setdiff(ls(), c("cleanDT", "border_info", "dist_info", "dist_rain_info",
                          "election_dist_rain_info", "rain_info", "unique_dist_state", "to.del", 
                          "border_info_graph", "new_parties_by_candidate"))
rm(list = to.del)
rm(to.del)

# Question 1 A --------------------

library(ggplot2)
ggplot(cleanDT, aes(new_parties, avg_rain)) + geom_point(aes(colour = factor(election_year)))
ggplot(cleanDT, aes(new_parties, avg_spi)) + geom_point(aes(colour = factor(election_year)))

# Question 1 B ----------------------

library(plm)
model1b_rain <- plm(avg_rain ~ lag_avg_rain + lag_neighbor_avg_rain + factor(years_unto_election) + factor(election_year), 
                    cleanDT, effect = "twoways", model = "within", index = "district")
summary(model1b_rain)

model1b_spi <- plm(avg_spi ~ lag_avg_spi + lag_neighbor_avg_spi + factor(years_unto_election) + factor(election_year), 
                   cleanDT, effect = "twoways", model = "within", index = "district")
summary(model1b_spi)

# Question 1 C -----------------------
library(pglm)
cleanDT <- cleanDT[, extreme_weather := (flood_cnt + drought_cnt)]
model1c_rain <- plm(extreme_weather ~ lag_avg_rain + lag_neighbor_avg_rain + factor(years_unto_election) + factor(election_year), 
                    cleanDT, effect = "twoways", model = "within", index = "district")
summary(model1c_rain)

model1c_spi <- plm(extreme_weather ~ lag_avg_spi + lag_neighbor_avg_spi + factor(years_unto_election) + factor(election_year), 
                   cleanDT, effect = "twoways", model = "within", index = "district")
summary(model1c_spi)

# Question 2 -----------------------
library(pglm)
model2 <- pglm(new_parties ~ extreme_weather + factor(years_unto_election), 
               cleanDT, effect = "twoways", model = "within", index = c("district"), family = "poisson")
summary(model2)


# Question 3 -----------------------
cleanDT <- cleanDT[, neighbor_extreme_weather := (neighbor_flood_cnt + neighbor_drought_cnt)]
library(pglm)
model3 <- pglm(new_parties ~ extreme_weather + neighbor_extreme_weather + factor(years_unto_election), 
               cleanDT, effect = "twoways", model = "within", index = "district", family = "poisson")
summary(model3)


# Question 4 -----------------------
library(pglm)
model4 <- pglm(political_concentration ~ extreme_weather + neighbor_extreme_weather + factor(years_unto_election), 
               cleanDT, effect = "twoways", model = "within", index = "district", family = "poisson")
summary(model4)


# Question 5 -----------------------
library(pglm)
cleanDT <- cleanDT[, lag_neighbor_extreme_weather := (lag_neighbor_flood_cnt + lag_neighbor_drought_cnt)]
cleanDT <- cleanDT[, not_from_neighbor := (new_parties - from_neighbors_cnt)]
model5_a <- pglm(from_neighbors_cnt ~ extreme_weather + factor(years_unto_election) + lag_neighbor_extreme_weather, 
                 cleanDT, effect = "twoways", model = "within", index = "district", family = "poisson")
summary(model5_a)

model5_b <- pglm(not_from_neighbor ~ extreme_weather + factor(years_unto_election) + lag_neighbor_extreme_weather, 
                 cleanDT, effect = "twoways", model = "within", index = "district", family = "poisson")
summary(model5_b)

