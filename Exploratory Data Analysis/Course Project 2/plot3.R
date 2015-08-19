library(ggplot2)
## Read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Aggregate by the year to get totals and fix column names
NEI.baltimore <- NEI[NEI[, "fips"] == 24510, ]
total.emissions <- aggregate(NEI.baltimore[, "Emissions"], by = list(NEI.baltimore[, "year"], NEI.baltimore[, "type"]), FUN = sum, na.rm = TRUE)
colnames(total.emissions) <- c("Year", "Type", "TotalEmissions")

## Do this to make the scientific notation go away (our numbers aren't that big)
options(scipen = 5)

## Plot a line chart and fit a line
balt.plot <- qplot(Year, TotalEmissions, data = total.emissions, facets = . ~ Type)
balt.plot <- balt.plot + stat_smooth(method = "lm", aes(group = 1))
balt.plot

#write the file plot1.png
dev.copy(device = png, "plot3.png")
dev.off()