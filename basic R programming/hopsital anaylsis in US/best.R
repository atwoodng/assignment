# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of ¡§heart attack¡¨, ¡§heart failure¡¨, or ¡§pneumonia¡¨. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.


best<-function(state,outcome){
 
  outcome2<-read.csv("hospital/outcome-of-care-measures.csv", colClasses = "character")
  outcome2[,11]<-suppressWarnings(as.numeric(outcome2[,11]))
  outcome2[,17]<-suppressWarnings(as.numeric(outcome2[,17]))
  outcome2[,23]<-suppressWarnings(as.numeric(outcome2[,23]))
  if (!state %in% outcome2[,7])stop("state error")
  if (!outcome %in% c("heart attack","heart failure","pneumonia"))stop("outcome error")

  
  outcome3<-outcome2[outcome2[,7] %in% state,]
  outcome3<-outcome3[order(outcome3[,2],na.last=T),]
  
  if(outcome == "heart attack"){ 
    outcome4<-outcome3[order(outcome3[,11],na.last=T),]
    print(outcome4[1,2])
  }
  else if(outcome =="heart failure"){
    outcome4<-outcome3[order(outcome3[,17],na.last=T),]
    print(outcome4[1,2])
  }
  else if(outcome =="pneumonia"){
    outcome4<-outcome3[order(outcome3[,23],na.last=T),]
    print(outcome4[1,2])
  }
}

