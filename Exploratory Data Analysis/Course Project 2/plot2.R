## Read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Aggregate by the year to get totals and fix column names
NEI.baltimore <- NEI[NEI[, "fips"] == 24510, ]
total.emissions <- aggregate(NEI.baltimore[, "Emissions"], by = list(NEI.baltimore[, "year"]), FUN = sum, na.rm = TRUE)
colnames(total.emissions) <- c("Year", "TotalEmissions")

## Do this to make the scientific notation go away (our numbers aren't that big)
options(scipen = 5)

## Plot a line chart and fit a line
plot(total.emissions[, "Year"], total.emissions[, "TotalEmissions"], type = "l", xlab = "Year", ylab = "Total Emissions", main = "Trend for Baltimore in blue")
abline(lm(total.emissions[, "TotalEmissions"] ~ total.emissions[, "Year"]), col = "blue")

#write the file plot1.png
dev.copy(device = png, "plot2.png")
dev.off()