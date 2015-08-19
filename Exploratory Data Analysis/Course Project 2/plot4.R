library(ggplot2)
## Read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Get the rows in SCC[, "EI.Sector"] that refer to coal and comb (for combustion)
coal.combustion.rows <- intersect(
                                   grep('comb', SCC[, "EI.Sector"], ignore.case = TRUE)
                                  ,grep('coal', SCC[, "EI.Sector"], ignore.case = TRUE)
                                 )
SCC.coal.combustion <- SCC[coal.combustion.rows, "SCC"]
                  
## Get the NEI rows that are in SCC.coal.combustion
NEI.coal.rows <- NEI[, "SCC"] %in% SCC.coal.combustion
               
## Aggregate by the year to get totals and fix column names
NEI.coal <- NEI[NEI.coal.rows, ]
total.emissions <- aggregate(NEI.coal[, "Emissions"], by = list(NEI.coal[, "year"]), FUN = sum, na.rm = TRUE)
colnames(total.emissions) <- c("Year", "TotalEmissions")

## Do this to make the scientific notation go away (our numbers aren't that big)
options(scipen = 5)

## Plot a line chart and fit a line
coal.plot <- qplot(Year, TotalEmissions, data = total.emissions)
coal.plot <- coal.plot + stat_smooth(method = "lm", aes(group = 1))
coal.plot

## write the file plot1.png
dev.copy(device = png, "plot4.png")
dev.off()