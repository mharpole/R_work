complete <- function(directory, id = 1:332) {
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
  #generate a list of the files within the given directory
  files<-list.files(directory, full.names = TRUE)
  #print(files)
  #generate a temp list of the requested sensors' data
  tmp<-lapply(files[id],read.csv)
  #condense the data into one data frame for easier usage
  pol_data<-do.call(rbind,tmp)
  #weed out incomplete cases
  complete<-pol_data[which(complete.cases(pol_data)),]
  #set up vector to count number of complete cases per id
  temp<-vector(mode = "integer")
	for (i in id){
		id_temp<-complete[which(complete$ID==i),]
		temp<-c(temp,length(id_temp[["ID"]]))		
		        }
	#print(temp)
  output<-data.frame(id=id,nobs=temp)
  output
}