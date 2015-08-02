complete <- function(directory, id = 1:332) 
{
   library(stringr)
   idVal <- vector(mode = "numeric", length = 0)
   nobs <- vector(mode = "numeric", length = 0)

   for (i in id)
   {
      filename <- paste(directory, '/', str_pad(i, 3, side = "left", '0'), '.csv', sep = "")
      pollutantFile <- read.csv(filename)
      
      isComplete <- complete.cases(pollutantFile)
      
      idVal <- append(idVal, i)
      nobs <- append(nobs, length(isComplete[isComplete == TRUE]))
   }

   completeNobs <- data.frame(id = idVal, nobs = nobs)
   
   completeNobs
}

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used
        
## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases