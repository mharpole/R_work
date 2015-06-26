## Function Which determines the best hospital in a state based on the mortality
## rates of either heart attack. heart failure, or pneumonia.
## coumn 2 contains hospital names
## column 7 contains state names
## column 11  contain mortalilty informtaion on heart attacks
## columns 17 contain mortality information on heat failure
## columns 23 contain mortality information on pneumonia
rankhospital <- function(state, outcome, num = "best"){
  ## Read outcome data
  data <-read.csv("outcome-of-care-measures.csv", na.strings = "Not Available",colClasses = "character")
  outcomes <- c("heart attack", "heart failure","pneumonia")
  #print(outcomes)
  ## check that state and outcome are valid
    #return(states)
  #return(outcomes)
  if(!(state %in% data$State)){
    
    stop("invalid state")
  }
  
  if(!(outcome %in% outcomes)){
    
    stop("invalid outcome")
  }
  if(outcome == "heart attack"){
    out <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  if(outcome == "heart failure"){
    out <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  if(outcome == "pneumonia"){
    out <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  # truncate data to only state of intrest and outcom of interest
  data <- filter(data, State==state)
  #print(head(data$State))
  data <- data[which(complete.cases(data[out])),]
  #print(head (data[17]))
  #rint(tail(data$State))
  #data <- arrange(data,outcome,Hospital.Name)
  #print(head(data))
  if(outcome == "heart attack"){
    data <- suppressWarnings(arrange(data,
                                     as.numeric(
                                       as.character(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), 
                                     as.character(Hospital.Name)))
  }
  if(outcome == "heart failure"){
    data <- suppressWarnings(arrange(data,
                                     as.numeric(
                                       as.character(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),
                                     as.character(Hospital.Name)))
    #print(head(data$State))
    #print(head(data$Hospital.Names))
  }
  if(outcome == "pneumonia"){
    data <- suppressWarnings(arrange(data,
                                     as.numeric(
                                       as.character(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),
                                     as.character(Hospital.Name)))
  }
  #print(head(data$Hospital.Name))
  #print(tail(data[out]))
  if(num=="worst"){
    #print("In the worst logic expression")
    return(data$Hospital.Name[length(data$Hospital.Name)])
  }
  if(num == "best"){
    num<-as.numeric(1)
  }
  #print(tail(data$Hospital.Name))
  as.character(data$Hospital.Name[num])
}