library(sqldf)
setwd("Programming/R/Getting and Cleaning Data/Week2/")

filename <- "getdata-data-ss06pid.csv"
acs <- read.csv(filename)

sqldf("select pwgtp1 from acs where AGEP < 50")
sqldf("select distinct AGEP from acs")
