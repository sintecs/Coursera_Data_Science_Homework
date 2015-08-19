library(ggplot2)
## Read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Get the rows in SCC[, "EI.Sector"] that refer to mobile and on-road
## which are what I consider "motor vehicles" since I take that to mean they 
## are referring to items on the highways, not in the air
motor.vehicle.rows <- intersect(
                                 grep('mobile', SCC[, "EI.Sector"], ignore.case = TRUE)
                                ,grep('on-road', SCC[, "EI.Sector"], ignore.case = TRUE)
                               )
motor.vehicle.rows <- setdiff(
                               motor.vehicle.rows
                              ,grep('non-road', SCC[, "EI.Sector"], ignore.case = TRUE)
                             )
SCC.motor.vehicles <- SCC[motor.vehicle.rows, "SCC"]
                  
## Get the NEI rows that are in SCC.motor.vehicles
NEI.motor.vehicle.rows <- NEI[, "SCC"] %in% SCC.motor.vehicles
NEI.motor.vehicle <- NEI[NEI.motor.vehicle.rows, ]
              
## Get the NEI motor vehicle rows that are in Baltimore City
NEI.baltimore.motor.vehicle <- NEI.motor.vehicle[NEI.motor.vehicle[, "fips"] == 24510, ]

## Aggregate by the year to get totals and fix column names
total.emissions <- aggregate(
                              NEI.baltimore.motor.vehicle[, "Emissions"]
                             ,by = list(NEI.baltimore.motor.vehicle[, "year"])
                             ,FUN = sum
                             ,na.rm = TRUE
                            )
colnames(total.emissions) <- c("Year", "TotalEmissions")

## Do this to make the scientific notation go away (our numbers aren't that big)
options(scipen = 5)

## Plot a line chart and fit a line
coal.plot <- qplot(Year, TotalEmissions, data = total.emissions)
coal.plot <- coal.plot + stat_smooth(method = "lm", aes(group = 1))
coal.plot

## write the file plot1.png
dev.copy(device = png, "plot5.png")
dev.off()