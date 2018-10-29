# Loading the data --------------------------------------------------------------

library(data.table)
setwd("C:/US Drive/MSBA/Social Network Analytics/Assignments/Assignment 2/")

columnsToRead <- c("Portfolio Company Name", "Stage", "Investors", "Deal Date")

one <- fread("Funding_events_7.14.csv")
library(readxl)
two <- data.table(read_excel(path = "Funding_events_7.14_page2.xlsx"))
detach("package:readxl", unload=TRUE)
two <- two[, `Deal Date` := format(as.Date(two[, `Deal Date`], "%Y-%m-%d"), "%m/%d/%Y")]

names(two)  <- names(one)
funding.events.dt <- rbind( one, two)
funding.events.dt <- funding.events.dt[,columnsToRead, with=FALSE]
funding.events.dt <- funding.events.dt[, `Deal Date` := as.Date(`Deal Date`, "%m/%d/%Y")]
funding.events.dt <- funding.events.dt[, Deal_Date_Year_Month := format(as.Date(`Deal Date`), "%Y-%m")]

{ #Clean the investors names
  funding.events.dt <- funding.events.dt[, Investors := gsub(", Inc.", " Inc", Investors, fixed = TRUE) ]
  funding.events.dt <- funding.events.dt[, Investors := gsub(", Inc", " Inc", Investors, fixed = TRUE) ]
  funding.events.dt <- funding.events.dt[, Investors := gsub(",Inc.", " Inc", Investors, fixed = TRUE) ]
  funding.events.dt <- funding.events.dt[, Investors := gsub(",Inc", " Inc", Investors, fixed = TRUE) ]
  funding.events.dt <- funding.events.dt[, Investors := gsub(", Limited.", " Limited", Investors, fixed = TRUE) ]
  funding.events.dt <- funding.events.dt[, Investors := gsub(", Ltd.", " Ltd", Investors, fixed = TRUE) ]
  funding.events.dt <- funding.events.dt[, Investors := gsub(", Ltd", " Ltd", Investors, fixed = TRUE) ]
  funding.events.dt <- funding.events.dt[, Investors := gsub(", LTD.", " Ltd", Investors, fixed = TRUE) ]
  funding.events.dt <- funding.events.dt[, Investors := gsub(", LTD.", " Ltd", Investors, fixed = TRUE) ]
  funding.events.dt <- funding.events.dt[, Investors := gsub(", LTD", " Ltd", Investors, fixed = TRUE) ]
  funding.events.dt <- funding.events.dt[, Investors := gsub(", LLC.", " LLC", Investors, fixed = TRUE) ]
  funding.events.dt <- funding.events.dt[, Investors := gsub(", LLC", " LLC", Investors, fixed = TRUE) ]
}
