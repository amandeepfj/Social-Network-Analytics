setwd("C:/US Drive/MSBA/Social Network Analytics/Assignments/Assignment 6/")
#rm(list = ls())
library(data.table)
library(lubridate)

investor_firm_details <- fread("investor_firm_details.csv")
#Find Unique investor countires
unique(investor_firm_details[,Country])[order(unique(investor_firm_details[,Country]))]
investor_firm_details <- investor_firm_details[Country == "United States", ]
#Top 5 investors cities in US 
investor_firm_details[, .N, by = City][order(N, decreasing = TRUE),][1:5, City]
cities_to_consider <- c("Los Angeles")
investor_firm_details <- investor_firm_details[City %in% cities_to_consider, ]
valid_investor_ids <- investor_firm_details[, InvestorId]


formatStringToDate <- function(dates){
  dates <- as.Date(dates, format = "%d %b %y")
  #Any date after 2019-10-01 will be converted to 19XX!
  #Above date was selected from Fiscal year
  dates <- as.Date(ifelse(dates >= "2019-10-01", format(dates, "19%y-%m-%d"), format(dates)))
  return(dates)
}

deal_details <- fread("deal_detailed.csv")
deal_details[,.N, by = Fiscal_year][order(Fiscal_year)]
deal_details$Deal_Date <- formatStringToDate(deal_details$Deal_Date)

invested.in.company_successfull_deals_cnt <- deal_details[, .(Invested.In.CompanyId = CompanyId,
                                                              Invested.In.successful_deals_cnt = .N), by = .(CompanyId)]

deal_details <- deal_details[(Deal_Date >= '2000-01-01'),]

# invested.in.company_successfull_deals_cnt <- deal_details[, .(Invested.In.CompanyId = CompanyId, 
#                                                                Invested.In.successful_deals_cnt = .N), by = .(CompanyId)]
                                                              
unique(deal_details[, Deal_Type_1])[order(unique(deal_details[, Deal_Type_1]))]
deal_type_to_ignore <- c("Buyout/LBO", "Merger/Acquisition", "IPO")
deal_details <- deal_details[!(Deal_Type_1 %in% deal_type_to_ignore), ]
setnames(deal_details, c("CompanyId"), c("Invested.In.CompanyId"))

#invested.in.company_successfull_deals_cnt <- deal_details[, .(Invested.In.successful_deals_cnt = .N), by = .(Invested.In.CompanyId)]


investors <- fread("investors.csv")
new_col_names <- paste("Investor", colnames(investors), sep = ".")
setnames(investors, colnames(investors), new_col_names)
investor_company_uniques <- investors[, .N, by = .(Investor.PersonId, Investor.CompanyId)]
investor_diff_company_counts <- investor_company_uniques[, .(Investor.Diff_Company_Cnt = .N), by = Investor.PersonId ]
investors <- merge(investors, investor_diff_company_counts, by = "Investor.PersonId")
investors <- investors[(Investor.InvestorId %in% valid_investor_ids)]# & (DealId %in% deal_details[, DealId])]


investment_details <- merge(deal_details, investors, by.y = "Investor.DealId", by.x = "DealId")

invested.in <- fread("execs.csv")
new_col_names <- paste("Invested.In", colnames(invested.in), sep = ".")
setnames(invested.in, colnames(invested.in), new_col_names)
invested.in <- invested.in[, lapply(.SD, max), by = .(Invested.In.PersonId, Invested.In.CompanyId)]
invested.in <- merge(invested.in.company_successfull_deals_cnt, invested.in, by = "Invested.In.CompanyId")

investment_details <- merge(investment_details, invested.in, by.x = "Invested.In.CompanyId", by.y = "Invested.In.CompanyId")

person_ids <- c(unique(investment_details[, Investor.PersonId]), unique(investment_details[, Invested.In.PersonId]))

people <- fread("people.csv")
new_col_names <- paste("Invested.In", colnames(people), sep = ".")
setnames(people, colnames(people), new_col_names)

investment_details <- merge(investment_details, people, by = "Invested.In.PersonId")

people <- fread("people.csv")
new_col_names <- paste("Investor", colnames(people), sep = ".")
setnames(people, colnames(people), new_col_names)

investment_details <- merge(investment_details, people, by = "Investor.PersonId")

colnames(investment_details)

people <- fread("people.csv")
# Cleaning and network creation for ERGM -----------------------
library(ergm)
edges_for_network <- investment_details[,.(Invested.In.Is.MBA = max(Invested.In.Is.MBA),
                                           Invested.In.Gender = max(Invested.In.Gender),
                                           Investor.Gender = max(Investor.Gender),
                                           Investor.Is.MBA = max(Investor.Is.MBA),
                                           Invested.In.successful_deals_cnt = sum(Invested.In.successful_deals_cnt),
                                           Investor.Diff_Company_Cnt = sum(Investor.Diff_Company_Cnt)), by = .(Investor.PersonId, Invested.In.PersonId)]
edges <- edges_for_network[, .(Investor.PersonId, Invested.In.PersonId)]

invest_net <- network(edges)
vertices_in_network <- get.node.attr(nw = invest_net, attrname = "vertex.names")

setkey(people, PersonId)
genders <- people[vertices_in_network, Gender]
genders[genders == ""] <- NA
invest_net %v% 'Gender' <- genders
length(invest_net %v% 'Gender')
table(invest_net %v% 'Gender')

invest_net %v% 'has_MBA' <- people[vertices_in_network, Education %like% 'MBA']
length(invest_net %v% 'has_MBA')
table(invest_net %v% 'has_MBA')

setkey(investor_diff_company_counts, Investor.PersonId)
diff_Company_Cnt <- investor_diff_company_counts[vertices_in_network, Investor.Diff_Company_Cnt]
diff_Company_Cnt[is.na(diff_Company_Cnt)] <- 0
invest_net %v% 'diff_Company_Cnt' <- diff_Company_Cnt
length(invest_net %v% 'diff_Company_Cnt')

person_successful_deals <- invested.in[, (Invested.In.successful_deals_cnt = sum(Invested.In.successful_deals_cnt)), by = Invested.In.PersonId]
setkey(person_successful_deals, Invested.In.PersonId)
successful_deals_cnt <- person_successful_deals[vertices_in_network, V1]
successful_deals_cnt[is.na(successful_deals_cnt)] <- 0
invest_net %v% 'successful_deals_cnt' <- successful_deals_cnt
length(invest_net %v% 'successful_deals_cnt')

# Question 1 ------------------------------------------------------------------
ergm_1 <- ergm(invest_net ~ edges + triangle)
summary(ergm_1)

# Question 2 ----------------------------------------------------------------
plot(invest_net, vertex.col='Gender')
legend('topright',fill=c("green", "red"),legend=c("Male", "Female"), cex=0.75, inset = 0.05)

plot(invest_net, vertex.col='has_MBA')
legend('topright',fill=c("white", "black"),legend=c("Not MBA", "Has MBA"), cex=0.75, inset = 0.05)

ergm_2 <- ergm(invest_net ~ edges + triangle + nodematch("Gender", diff=T) + nodematch("has_MBA", diff=T), iterations = 20)
summary(ergm_2)

# Question 3 -------------------------------------------------------------
plot(invest_net, vertex.col='successful_deals_cnt')
deal_cnts <- unique(get.node.attr(nw = invest_net, attrname = "successful_deals_cnt"))
legend('topright',fill=deal_cnts,legend=paste('Successfull deal counts = ',deal_cnts), cex=0.75, inset = 0.05)

plot(invest_net, vertex.col='diff_Company_Cnt')
diff.company_count <- unique(get.node.attr(nw = invest_net, attrname = "diff_Company_Cnt"))
legend('topright',fill=deal_cnts,legend=paste('Companies = ',diff.company_count),cex=0.75, inset = 0.05)

ergm_3 <- ergm(invest_net ~ edges + triangle + nodematch("Gender", diff=T) + nodematch("has_MBA", diff=T) + 
                 nodecov("successful_deals_cnt") + nodecov("diff_Company_Cnt"), iterations = 20)
summary(ergm_3)



