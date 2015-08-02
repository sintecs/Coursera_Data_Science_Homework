rankall <- function(outcome, num = "best")
{
 ## Read in outcome data
 outcome.data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
 hospital.data <- read.csv("hospital-data.csv", stringsAsFactors = FALSE)
 ## Check that outcome is valid
 if (sum(toupper(c("heart attack", "heart failure", "pneumonia")) == toupper(outcome)) <= 0)
 {
  stop ("invalid outcome")
 }
 if (!is.numeric(num) & sum(toupper(c("BEST", "WORST")) == toupper(num)) <= 0)
 {
  stop ("invalid num")
 }
 ## Return hospital name in that state with lowest 30-day death
 ## rate
 outcome.text <- gsub("heart", "Heart", outcome)
 outcome.text <- gsub("failure", "Failure", outcome.text)
 outcome.text <- gsub("attack", "Attack", outcome.text)
 outcome.text <- gsub("pneumonia", "Pneumonia", outcome.text)
 outcome.text <- gsub(" ", ".", outcome.text)
 outcome.text <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome.text, sep = "")
 
 outcome.states <- unique(hospital.data$State)
 outcome.data[, outcome.text] <- as.numeric(outcome.data[, outcome.text])
 
 if(num == "worst")
 {
  worst.or.best = TRUE
 } else
 {
  worst.or.best = FALSE
 }
 if(num == "worst" | num == "best")
 {
  return.row <- 1
 } else
 {
  return.row <- num
 }
 
 outcome.states <- sort(outcome.states)
 hospital.value <- c()
 state.value <- c()
 
 
 for(state in outcome.states)
 {

  if(outcome == "pneumonia")
  {
   test <- outcome.data[order(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, outcome.data$Hospital.Name, na.last = TRUE, decreasing = worst.or.best), ] 
   test <- test[is.na(test$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == FALSE, ]
  }
  if(outcome == "heart attack")
  {
   test <- outcome.data[order(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcome.data$Hospital.Name, na.last = TRUE, decreasing = worst.or.best), ] 
   test <- test[is.na(test$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == FALSE, ]
  }
  if(outcome == "heart failure")
  {
   test <- outcome.data[order(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, outcome.data$Hospital.Name, na.last = TRUE, decreasing = worst.or.best), ] 
   test <- test[is.na(test$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == FALSE, ]
  }
   
   test.data <- subset(test, State == state)
   hospital.value <- c(hospital.value, test.data[return.row, "Hospital.Name"])
   state.value <- c(state.value, state)
 }
 
 result <- data.frame(hospital = hospital.value, state = state.value)
 result
}