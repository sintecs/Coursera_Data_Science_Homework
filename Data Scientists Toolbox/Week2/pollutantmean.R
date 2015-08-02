pollutantmean <- function(directory, pollutant, id = 1:332) {
   library(stringr)
   
   pollutantData <- vector(mode = "numeric", length = 0)
   
   for (i in id)
   {
      filename <- paste(directory, '/', str_pad(i, 3, side = "left", '0'), '.csv', sep = "")
      pollutantFile <- read.csv(filename)
      pollutantData <- append(pollutantData, as.vector(pollutantFile[[pollutant]]))
   }

   bad <- is.na(pollutantData)
   good <- pollutantData[!bad]
   
   mean(good)

}