## Exploratory Data Analysis
## This will take the URL provided in the assignment and download the zip file.
## It then loads the data into a dataset titled epc

## URL of the dataset
sourceDataURL <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'

## File names for data
fileList              <- c('household_power_consumption.txt')

## Temporary File for storing the downloaded zip then extract the files we want into R
temp <- tempfile()
download.file(sourceDataURL, temp, method = "libcurl")
epc.data              <- read.table(unz(temp, fileList[1]), sep = ';', skip = 66637, nrows = 2880)

unlink(temp)

## Set the column names from the code book
colnames(epc.data)    <- c('Date', 'Time', 'Global_active_power', 'Global_reactive_power', 'Voltage', 'Global_intensity', 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3')

## Make the date and time a new column with date and time together
epc.data              <- cbind(epc.data, observation.date = strptime(paste(epc.data[, "Date"], epc.data[, "Time"]), format = "%d/%m/%Y %H:%M:%S"))

#Create line chart of Global_active_power data vs. date
plot(epc.data[, "observation.date"], epc.data[, "Global_active_power"], xlab = "", ylab = "Global Active Power (kilowatts)", type = "l")

#write the file plot1.png
dev.copy(device = png, "plot2.png", width = 480, height = 480)
dev.off()
