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
  col2<-vector()
  for (i in id){ file<-paste(directory,sprintf("/%03d.csv",i),sep="")
                 data<-read.csv(file)
                 totalsum<-sum(complete.cases(data))
                 col2<-append(col2,totalsum)
  }
  col1<-id
  completelist<-cbind(id,col2)
  dimnames(completelist)<-list(NULL,c("ID","nods"))
  return(completelist)
}
