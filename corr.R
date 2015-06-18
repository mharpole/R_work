corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  #generate a list of the files within the given directory
  files<-list.files(directory, full.names = TRUE)
  #print(files)
  #generate a temp list of the  sensors data
  temp<-lapply(files,read.csv)
  output<-vector(mode="numeric")
	for (i in temp){
	complete<-i[which(complete.cases(i)),]
		if (nrow(complete)>threshold){
		output<-c(output,cor(complete[['sulfate']],complete[['nitrate']]))
		}
	}
  output
}