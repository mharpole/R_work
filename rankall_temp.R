

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <-read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  outcomes <- c("heart attack", "heart failure","pneumonia")
  if(!(outcome %in% outcomes)){
    
    stop("invalid outcome")
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
  ## For each state, find the hospital of the given rank
  state_list<-as.character(levels(data$State))
  #print("state_list")
  #print(state_list)
  states <- vector(mode = "character")
  hospitals<-vector(mode = "character")
  
  for(i in state_list){
    #print("I is now")
    #print(i)
    states<-c(states,i)
    
    temp<-data[grep(as.character(i),data$State),]
    
    #print(head(temp))
    temp<- temp[which(complete.cases(temp[out])),]
    
    temp<- temp[order(temp[,out],temp$Hospital.Name),]
    
    if(num=="worst"){
    
      print("In the worst logic expression")
      
      num<-as.numeric(length(temp$Hospital.Name))
      if(outcome == "heart attack"){
        temp <- suppressWarnings(arrange(temp,
                                         as.numeric(
                                           as.character(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), 
                                         desc(as.character(Hospital.Name))))
      }
      if(outcome == "heart failure"){
        temp <- suppressWarnings(arrange(temp,
                                         as.numeric(
                                           as.character(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),
                                         desc(as.character(Hospital.Name))))
        #print(head(data$State))
        #print(head(data$Hospital.Names))
      }
      if(outcome == "pneumonia"){
        temp <- suppressWarnings(arrange(temp,
                                         as.numeric(
                                           as.character(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),
                                         desc(as.character(Hospital.Name))))
        hospitals<-c(hospitals,as.character(temp$Hospital.Name[num]))
      }
      #print("num")
      #print(num)
      
      }else{
      
        if(num == "best"){
      
          num<-as.numeric(1)
          
          hospitals<-c(hospitals,as.character(temp$Hospital.Name[num]))
          
          }else{
    
            hospitals<-c(hospitals,as.character(temp$Hospital.Name[num]))
          }
      }
    }
    #print(head(states))
    #print(head(hospitals))
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  output<-data.frame(hospitals,states,row.names = states)
  output
}

head(rankall("heart attack",20))
