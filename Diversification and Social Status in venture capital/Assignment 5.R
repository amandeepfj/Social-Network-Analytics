setwd("C:/US Drive/MSBA/Social Network Analytics/Assignments/Assignment 5")
rm(list = ls(all = TRUE))

# Data Loading and Cleaning -----------------------------
library(data.table)
library(MASS)
library(igraph)
library(scatterplot3d)
library(rgl)
library(nnet)

investor_firm <- fread("investor_firms.csv")
setkey(investor_firm, InvestorId)
investor_firm <- unique(investor_firm)

investors_deals <- fread("investors_and_deals.csv")
setkey(investors_deals, Investor_Id, Deal_Id)
investors_deals <- unique(investors_deals)

investors_deals <- merge(investor_firm , investors_deals, by.x = "InvestorId", by.y = "Investor_Id", all.y = TRUE)

startup_deals <- fread("startups_and_deals.csv")
setkey(startup_deals, DealId)
startup_deals <- unique(startup_deals)

investors_deals <- merge(startup_deals, investors_deals, by.x = "DealId", by.y = "Deal_Id")

startup_companies <- fread("startup_companies.csv")
setkey(startup_companies, CompanyID)
startup_companies <- unique(startup_companies)

detailed_deals <- merge(startup_companies, investors_deals, by.x = "CompanyID", by.y = "CompanyId")

#REMOVE INDIVIDUAL INVESTORS
detailed_deals <- detailed_deals[InvestorId != "10217-17", ]

# Question 1 ------------------------------------
detailed_deals_q1 <- detailed_deals[!is.na(successful_investments) & 
                                      !is.na(InvestorId) & !is.na(Primary_Industry_Code), ]

Investment_by_industry <- detailed_deals_q1[, .(industry_wise_count = .N), 
                    by = .(InvestorId, Primary_Industry_Code, successful_investments)]

Investment_by_industry[order(decreasing = TRUE, successful_investments), ]

Investment_by_industry_cnt <- 
  Investment_by_industry[, .(industry_count = .N), by = .(InvestorId, successful_investments)]

Total_Investments <- detailed_deals_q1[, .(investment_count = .N), by = .(InvestorId)]

investment_by_diversification <- merge(Investment_by_industry_cnt, Total_Investments, by = "InvestorId")

investment_by_diversification[order(decreasing = TRUE, investment_count), ]

investment_by_diversification[, diversification := (industry_count/investment_count), ]

mean_diverse <- mean(investment_by_diversification[, diversification])

#correcting diversification
min_diver_to_consider <- 20
investment_by_diversification[, corrected_diversification := 
                                diversification * (investment_count)/(investment_count + min_diver_to_consider) + 
                                mean_diverse * (min_diver_to_consider)/(investment_count + min_diver_to_consider), ]

#PART A
investment_by_diversification_plt <- investment_by_diversification
plot(x = investment_by_diversification_plt[, corrected_diversification], 
     y = (investment_by_diversification_plt[, successful_investments]), 
     col="green", ylab = "Log of Succesfull Investments", xlab = "Diversification")

#PART B
model3 <- glm(successful_investments ~ corrected_diversification, 
               investment_by_diversification, family = "poisson")

summary(model3)


# Question 2 ----------------------------
detailed_deals_q2 <- detailed_deals[!is.na(InvestorId), .(InvestorId, DealId, Lead_Investor)]
edge_list__investors_deals <- merge(detailed_deals_q2, detailed_deals_q2, by = "DealId", 
                         allow.cartesian = TRUE)[InvestorId.x != InvestorId.y,]
edge_list__investors_deals[, Lead_Investor.y := NULL]
setnames(edge_list__investors_deals, old = c("InvestorId.x", "InvestorId.y", "Lead_Investor.x"),
         new = c("Investor_from", "Investor_to", "lead_investor"))

max(edge_list__investors_deals[, .N, by = .(Investor_from, Investor_to)][, N])

directed_edge_list_investors <- edge_list__investors_deals[, .(lead_sum = sum(lead_investor), total_investments = .N), 
                           by = .(Investor_from, Investor_to)]

directed_edge_list_investors[, status := (lead_sum/(total_investments))]
investors_graph <- graph_from_data_frame(directed_edge_list_investors, directed = TRUE)
E(investors_graph)$weight <- directed_edge_list_investors[, status]

eigen_centrality <- eigen_centrality(investors_graph)
investors_eigen_centrality <- data.table(investorId = names(eigen_centrality(investors_graph)$vector), 
                                         eigen = eigen_centrality(investors_graph)$vector)


investors_eigen_centrality_sucess <- merge(investors_eigen_centrality, detailed_deals, by.x = "investorId", 
      by.y = "InvestorId")[, .(investorId, eigen, successful_investments)]
investors_eigen_centrality_sucess <- investors_eigen_centrality_sucess[!is.na(successful_investments), ]
investors_eigen_centrality_sucess <- investors_eigen_centrality_sucess[, .(successful_investments = mean(successful_investments))
                                                                       , by = .(investorId, eigen)]
#PART A
investors_eigen_centrality_sucess_plt <- investors_eigen_centrality_sucess
plot(x = (investors_eigen_centrality_sucess_plt[, eigen]), 
     y = (investors_eigen_centrality_sucess_plt[, successful_investments]), col="maroon", 
     ylab = "Succesfull Investments", xlab = "Eigen Value")

#PART B
model2b <- glm(successful_investments ~ (eigen), 
               investors_eigen_centrality_sucess_plt, family = "poisson")

summary(model2b)


# Question 3 ---------------------------------
interaction_terms_model <- merge(investment_by_diversification, investors_eigen_centrality_sucess, by.x = "InvestorId", by.y = "investorId")[, .(InvestorId, corrected_diversification, eigen, successful_investments.x)]
model3a <- glm(successful_investments.x ~ eigen*corrected_diversification, 
               interaction_terms_model, family = "poisson")
summary(model3a)

# set up scaled grid of (x,y) values 
diversification <- seq(min(interaction_terms_model[, corrected_diversification]), 
                       max(interaction_terms_model[, corrected_diversification]), length.out = 100)
eigen <- seq(min(interaction_terms_model[, eigen]), max(interaction_terms_model[, eigen]), length.out = 100)
values <- expand.grid(corrected_diversification=diversification, eigen=eigen)
# prediction from the model 
values$successful_investments <- predict(model3a, values, type = "response")
# regular 3d plot 
scatterplot3d(values$corrected_diversification, values$eigen, values$successful_investments)
# interactive 3d plot you can move around 
plot3d(values$corrected_diversification, values$eigen, values$successful_investments)


# Question 4 ------------------------------------------
unique(detailed_deals[, Business_Status])
ramp_up <- c("Startup", "Clinical Trials - Phase 1", "Clinical Trials - Phase 2", "Clinical Trials - Phase 3", "Clinical Trials - Phase 4",
             "Clinical Trials - General", "Pre-Clinical Trials")
detailed_deals[Business_Status %in% ramp_up, startup_state := "Ramp-up"]

generating_revenue <- c("Generating Revenue", "Generating Revenue/Not Profitable")
detailed_deals[Business_Status %in% generating_revenue, startup_state := "Generating revenue"]

profitable <- c("Profitable")
detailed_deals[Business_Status %in% profitable, startup_state := "Profitable"]

failed <- c("Bankruptcy: Liquidation", "Bankruptcy: Admin/Reorg", "Out of Business")
detailed_deals[Business_Status %in% failed, startup_state := "Failed"]
unique(detailed_deals[, startup_state])

merged_status <- merge(detailed_deals, interaction_terms_model, by = "InvestorId")
merged_status <- merged_status[!is.na(startup_state)]
model <- multinom(startup_state ~ corrected_diversification + eigen + corrected_diversification*eigen, merged_status) 

z <- summary(model)$coefficients/summary(model)$standard.errors 
(1 - pnorm(abs(z), 0, 1)) * 2 


