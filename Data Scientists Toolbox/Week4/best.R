best <- function(state, outcome)
{
 ## Read in outcome data
 outcome.data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
 ## Check that state and outcome are valid
 if (sum(toupper(outcome.data[, "State"]) == toupper(state)) <= 0)
 {
  stop ("invalid state")
 }
 if (sum(toupper(c("heart attack", "heart failure", "pneumonia")) == toupper(outcome)) <= 0)
 {
  stop ("invalid outcome")
 }
 ## Return hospital name in that state with lowest 30-day death
 ## rate
 outcome.text <- gsub("heart", "Heart", outcome)
 outcome.text <- gsub("failure", "Failure", outcome.text)
 outcome.text <- gsub("attack", "Attack", outcome.text)
 outcome.text <- gsub("pneumonia", "Pneumonia", outcome.text)
 outcome.text <- gsub(" ", ".", outcome.text)
 outcome.text <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome.text, sep = "")
 
 outcome.data <- subset(outcome.data, State == state)
 outcome.data[, outcome.text] <- as.numeric(outcome.data[, outcome.text])
 if(outcome == "pneumonia")
 {
  test <- outcome.data[order(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, outcome.data$Hospital.Name, na.last = TRUE), ] 
 }
 if(outcome == "heart attack")
 {
  test <- outcome.data[order(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcome.data$Hospital.Name, na.last = TRUE), ] 
 }
 if(outcome == "heart failure")
 {
  test <- outcome.data[order(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, outcome.data$Hospital.Name, na.last = TRUE), ] 
 }

 test[1, "Hospital.Name"]
}