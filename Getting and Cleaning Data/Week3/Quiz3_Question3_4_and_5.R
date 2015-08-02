## Question 3

gdp.url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
gdp2.url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'

setwd('Programming/R/Getting and Cleaning Data/Week3/')
download.file(gdp.url, 'fgdp.csv', method = 'libcurl')
download.file(gdp2.url, 'fedstats.csv', method = 'libcurl')

fedstats <- read.csv('fedstats.csv')
fgdp <- read.csv('fgdp.csv', skip = 4, nrows = 190)

fgdp[, "X.4"] <- as.numeric(gsub(',', '', as.character(fgdp[, "X.4"])))
##fgdp[, "X.1"] <- as.numeric(gsub(',', '', as.character(fgdp[, "X.1"])))
merged.data <- merge(fedstats, fgdp, by.y = "X", by.x = "CountryCode")
merged.logical <- is.na(merged.data[, "X.1"])
length(merged.logical) - sum(merged.logical)


## 189?

library(plyr)
arrange(merged.data, X.4)


## Question 4
library(dplyr)

temp.data <- group_by(merged.data, Income.Group)
summarize(temp.data, meanx.1 = mean(X.1))


## Question 5

gdp.groups = cut(merged.data$X.1, breaks = quantile(merged.data[, "X.1"], c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
table(gdp.groups, merged.data[, "Income.Group"])





quantile(merged.data[, "X.1"])