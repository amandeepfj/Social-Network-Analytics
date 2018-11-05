rm(list = ls(all = TRUE))
setwd("C:/Users/dplewi3/Dropbox")
setwd("~/Dropbox")

library(data.table)
library(igraph)
library(reshape)
library(MASS)
library(plm)
library(pglm)
library(ggplot2)

############## Data prep

# key insight -- lists are helpful for creating mini-enviroments to work with for each district. reshape can help out with getting the characteristics of neighbors all at once by transforming the data from long to wide

districts = fread("district_information.csv", header = TRUE)
border = fread("border_information.csv", header = TRUE)
rain = fread("rain_information.csv", header = TRUE)

# first want to make border edge list undirected
border = rbindlist(list(border, data.table(cbind(border$district, border$focal_district))))


# next set up droughts and floods variables from rain info using spi
# sum over election years, accounting for non-regular intervals in between elections
years = unique(districts$year) # each election year
periods = c(list(seq(1946, 1951)), lapply(seq_along(years)[-1], function(i) seq(years[i -1] + 1, years[i]))) # list with years in each interval


# can set up a few with something like
rain[, moderately_dry := as.numeric(spi < -1)]
rain[, severely_dry := as.numeric(spi < -1.5)]
rain[, extremely_dry := as.numeric(spi < -2)]
rain[, moderately_wet := as.numeric(spi > 1)]
rain[, severely_wet := as.numeric(spi > 1.5)]
rain[, extremely_wet := as.numeric(spi > 2)]
rain[, moderately_abnormal := as.numeric(spi > 1 | spi < -1)]
rain[, severely_abnormal := as.numeric(spi > 1.5 | spi < -1.5)]
rain[, extremely_abnormal := as.numeric(spi > 2 | spi < -2)]



# creating a list with just rain in each interval
rain_elections = lapply(seq_along(periods), function(i) rain[year %in% periods[[i]]])

spicols = colnames(rain)[(ncol(rain)-8):ncol(rain)]

rain_elections = lapply(seq_along(periods), function(i) rain_elections[[i]][, (spicols) := lapply(.SD, function(x) sum(x)), .SDcols = spicols, by = district])
rain_elections = lapply(seq_along(periods), function(i) rain_elections[[i]][, spi := mean(spi), by = district])
rain_elections = lapply(seq_along(periods), function(i) rain_elections[[i]][, year := as.integer(max(periods[[i]])), by = district])
rain_elections = lapply(seq_along(periods), function(i) rain_elections[[i]][, rain := sum(rain), by = district])

spi_elections = lapply(seq_along(periods), function(i) unique(rain_elections[[i]][, list(moderately_dry = moderately_dry, severely_dry = severely_dry, extremely_dry = extremely_dry, moderately_wet = moderately_wet, severely_wet = severely_wet, extremely_wet = extremely_wet, moderately_abnormal = moderately_abnormal, severely_abnormal = severely_abnormal, extremely_abnormal = extremely_abnormal, spi = spi, year = year, rain = rain, district = district)]))

spi_elections = rbindlist(spi_elections)


setkeyv(spi_elections, c("district", "year"))
setkeyv(districts, c("district", "year"))
districts = merge(districts, spi_elections)


dwide = reshape( districts, idvar="district", timevar="year", direction="wide" )

years = unique(districts$year)
focal_vars =  c(sapply(seq_along(years), function(i) paste("spi.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("moderately_dry.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("severely_dry.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("extremely_dry.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("moderately_wet.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("severely_wet.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("extremely_wet.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("moderately_abnormal.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("severely_abnormal.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("extremely_abnormal.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("new_parties.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("rain.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("total_parties.",years[i],sep =""))
		)

alter_table = setNames(data.table(matrix(data = rep(0, nrow(dwide)*length(years)), nrow = nrow(dwide), ncol = length(focal_vars))), paste("adj_",focal_vars,sep =""))
adjs = lapply(seq_len(nrow(dwide)), function(j) border$district[ border$focal_district == dwide$district[j]])
dwide_df = as.data.frame(dwide)
# can skip this using .SDCols in data table in the loop below


for(i in seq_along(focal_vars)){
	for(j in seq_len(nrow(dwide_df))){
		alter_table[j,i] = mean(dwide_df[,colnames(dwide_df)==focal_vars[i]][dwide_df$district %in% adjs[[j]]], na.rm = TRUE)
	}
}

dwide = cbind(dwide, alter_table)

districts_adjrain = reshape(dwide, idvar= "district", varying = colnames(dwide)[2:ncol(dwide)], sep = ".", timevar = "year", direction = "long")	

########### Question 1

##### Part A
# lagging variables

# can also do this in four lines, but cols/sdcols/lapply from the data table example saves a bit of space
# it's also a bit more modular in case we want to add columns in later on
cols = c("moderately_dry", "severely_dry", "extremely_dry", "moderately_wet", "severely_wet", "extremely_wet", "moderately_abnormal", "severely_abnormal", "extremely_abnormal", "rain", "spi", "new_parties", "adj_moderately_dry", "adj_severely_dry", "adj_extremely_dry", "adj_moderately_wet", "adj_severely_wet", "adj_extremely_wet", "adj_moderately_abnormal", "adj_severely_abnormal", "adj_extremely_abnormal", "adj_new_parties", "adj_rain", "adj_spi", "total_parties", "adj_total_parties"
	)
lags = paste("l_",cols,sep="")

districts_adjrain[, (lags) := lapply(.SD, function(x) shift(x, 1)), .SDcols = cols, by = district]	

# can use loess or any other kind of scatter plot
library(ggplot2)

lowess_rain = ggplot(districts_adjrain, aes(rain, new_parties)) + geom_smooth(method = "loess", se = F) + labs(x = "Rainfall, in ml", y = "New organization") + coord_cartesian(ylim = c(1.75, 2.75)) + scale_y_continuous(breaks=seq(1.75,2.75,.25))
ggsave("lowess_rain.pdf", width = 7, height = 7, units = "in")

lowess_spi = ggplot(districts_adjrain, aes(spi, new_parties)) + geom_smooth(method = "loess", se = F) + labs(x = "Standardized Precipitation Index", y = "") + coord_cartesian(ylim = c(1.75, 2.75), xlim = c(-1.5, 1.5))
ggsave("lowess_spi.pdf", width = 7, height = 7, units = "in")


##### Part B

# is rain related to rain from previous period
summary(plm(rain ~ l_rain + l_adj_rain, data = districts_adjrain, effect = "twoways", model = "within", index = "district"))

# and spi
summary(plm(spi ~ l_spi + l_adj_spi, data = districts_adjrain, effect = "twoways", model = "within", index = "district"))

# both related 

##### Part C

# now try with drought/flood measure
summary(pglm(moderately_abnormal ~ l_moderately_abnormal + l_adj_moderately_abnormal, data = districts_adjrain, effect = "twoways", model = "within", index = "district", family = "poisson"))

# not related, so can use for exogenous variation


########### Question 2
years = sort(years)
for(i in seq_along(years)[-1]){
	districts_adjrain[year > 1951, interval := year - years[i - 1]]
}

districts_adjrain[year == 1951, interval := 5]

districts_nonmissing = districts_adjrain[!is.na(spi)]
#fwrite(districts_adjrain, "districts_adjrain.csv")

#districts_adjrain = read.csv("districts_adjrain.csv", head = TRUE)


summary(pglm(new_parties ~ moderately_abnormal + interval, data = districts_nonmissing, effect = "individual", model = "within", index = "district", family = "poisson"))

# new parties more likely when droughts or floods
summary(pglm(new_parties_caste ~ moderately_abnormal + interval, data = districts_nonmissing, effect = "individual", model = "within", index = "district", family = "poisson"))

summary(pglm(new_parties_nationalist ~ moderately_abnormal + interval, data = districts_nonmissing, effect = "individual", model = "within", index = "district", family = "poisson"))

########## Question 3

# add in neighbor effect
summary(pglm(new_parties ~ moderately_abnormal + l_adj_moderately_abnormal + interval, data = districts_nonmissing, effect = "individual", model = "within", index = "district", family = "poisson"))


# it looks like neighbors'experiencing droughts or floods is also related to more parties in the current period

########## Question 4

summary(plm(political_concentration ~ moderately_abnormal + l_adj_moderately_abnormal + interval, data = districts_adjrain, effect = "individual", model = "within", index = "district"))

########## Question 5

# candidate-level info
districts_parties = fread("new_parties_in_each_district_by_candidate.csv", header = TRUE)

# get unique parties that contest in each year
districts_parties = unique(districts_parties[,c("district", "year","party_name")])
colnames(districts_parties)[colnames(districts_parties) == "party_name"] = "party"

# subset to districts with rain data
districts_parties = districts_parties[district %in% dwide$district]

setorderv(districts_parties, c("district", "year"))

# split by district so we can make mini-tables to work on independently
dp_district = split(districts_parties, f = districts_parties$district)

# create mini-tables for each district's neighbors
dp_district_neighbor = lapply(seq_along(dp_district), function(i) districts_parties[district %in% adjs[[i]]])

# initialize a list to put parties that have appeared in neighbors into
dp_district_neighbor_any_year = list()

# each ij element will contain the parties that have appeared in district i's neighbors, before year j
for(i in seq_along(dp_district)){
	years_local = unique(dp_district[[i]]$year) # lets us flexibly iterate depending on how many years we have for each district
	dp_district_neighbor_any_year[[i]] = list() # re-initializing second level
	for(j in seq_along(years_local)){
		dp_district_neighbor_any_year[[i]][[j]] = dp_district_neighbor[[i]][year < years_local[j]] # subset to parties that have been introduced before year j
	}
}

# create an object to make the comparison with the focal district
dp_district_any_year = lapply(seq_along(dp_district), function(i) split(dp_district[[i]], f = dp_district[[i]]$year))

# checking if the parties in this year have appeared in the neighbor before
for(i in seq_along(dp_district)){
	for(j in seq_along(dp_district_any_year[[i]])){
		dp_district_any_year[[i]][[j]][, same_party_any_year := party %in% dp_district_neighbor_any_year[[i]][[j]]$party]
	}
}

dp_diffused_any_year = rbindlist(lapply(seq_along(dp_district), function(i) rbindlist(dp_district_any_year[[i]])))

# sum diffused parties for each year
dp_diffused_any_year_panel = unique(dp_diffused_any_year[, list(diffused_any_year = sum(same_party_any_year)), by = c("district", "year")])

setkeyv(dp_diffused_any_year_panel, c("district", "year"))
setkeyv(districts_adjrain, c("district", "year"))

# merge back to main data
districts_adjrain = merge(districts_adjrain, dp_diffused_any_year_panel, all.x = TRUE)

# subtract from new parties to get parties that have not diffused from neighbors
districts_adjrain[, non_diffused_any_year := new_parties - diffused_any_year]

# set up similar regression
summary(pglm(diffused_any_year ~ moderately_abnormal + interval, data = districts_adjrain, effect = "individual", model = "within", index = "district", family = "poisson"))

summary(pglm(non_diffused_any_year ~ moderately_abnormal + interval, data = districts_adjrain, effect = "individual", model = "within", index = "district", family = "poisson"))
