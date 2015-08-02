corr <- function(directory, threshold = 0)
{
   myResult <- vector(mode = "numeric", length = 0)

   for (i in (dir(directory)))
   {
      filename <- paste('./', directory, '/', i, sep = "")
      pollutantFile <- read.csv(filename)
      
      isComplete <- complete.cases(pollutantFile)
      
      if (length(isComplete[isComplete == TRUE]) > threshold)
      {
         myResult <- append(myResult, cor(pollutantFile[, "sulfate"], pollutantFile[, "nitrate"], use = "complete.obs"))
      }
   }

   myResult
}

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
## NOTE: Do not round the result!
