connection <- url("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for", method = "libcurl")
htmlData <- readLines(connection)
close(connection)

setwd("/Users/dbucholt/Programming/R/Getting and Cleaning Data/Week2")
write(htmlData, "data.csv")
filename <- "data.csv"
data <- read.fwf(filename, skip = 4, widths = c(15, 4, 4, 9, 4, 9, 4, 9, 4))
sum(data[,4])
