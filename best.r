## Function Which determines the best hospital in a state based on the mortality
## rates of either heart attack. heart failure, or pneumonia.
## coumn 2 contains hospital names
## column 7 contains state names
## column 11 or 15 contain mortalilty informtaion on heart attacks
## columns 17 or 21 contain mortality information on heat failure
## columns 23 or 27 contain mortality information on pneumonia
best <- function(state, outcome){
	# format outcome vector to parse later in the code
	 outcome <- tolower(outcome)
	 outcomes<- vector(length = 3)
	 names(outcomes)<-c("heart attack", "heart failure","pneumonia")
	 outcomes["heart attack"]<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	 outcomes["heart failure"]<- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	 outcomes["pneumonia"]<- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	 state <- toupper(state)
	  
## Read outcome data
	data <-read.csv("outcome-of-care-measures.csv",numerals = "no.loss",na.strings = "Not Avaliable")

## check that state and outcome are valid
	states <- levels(data[,7])
	#return(states)
	#return(outcomes)
	if(!(state %in% states)){
	
	stop("invalid state")
	}
	
	if(!(outcome %in% names(outcomes))){
	
	stop("invalid outcome")
	}
	# truncate data to only state of intrest and outcom of interest
  data <- data[grep(state,data$State),]
  data <- data[complete.cases(data[outcomes[outcome]]),]
  data <- data[order(data[,outcomes[outcome]]),]
	#print(head(data))
	return(as.character(data$Hospital.Name[1]))
}