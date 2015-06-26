
best <- function(state,outcome){
  if(outcome == "heart attack"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  if(outcome == "heart failure"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  if(outcome == "pneumonia"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  #print(outcome)
  data <- read.csv("outcome-of-care-measures.csv", na.string= "Not Available")
  #print(head(data[outcome]))
  data_state <- data[grep(state,data$State),]
  #print(head(data_state$State))
  #print(tail(data_state$State))
  data_complete <- data_state[which(complete.cases(data_state[outcome])),]
  #data_order <- data_complete[order(data_complete[,outcome],data_complete$Hospital.Name),]
  data_order <- arrange(data_complete,)
  data_order$Hospital.Name[1]
}