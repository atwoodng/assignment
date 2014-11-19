# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument

rankhospital<-function(state,outcome,num="best"){
  complete<-read.csv("hospital/outcome-of-care-measures.csv" ,colClasses = "character")
  coloutcome<-c(11,17,23)
  names(coloutcome)<-c("heart attack","heart failure", "pneumonia")
  pickitout<-complete[complete$State %in% state, c(coloutcome[names(coloutcome)%in% outcome],2)]
  pickitout[,1]<-suppressWarnings(as.numeric(pickitout[,1]))
  pickitout<-pickitout[order(pickitout[,1],pickitout[,2], na.last=T),]
  pickitout<-pickitout[complete.cases(pickitout),]
  if (num == "best"){num<-1}
  else if(num == "worst"){num<-nrow(pickitout)}
  print(pickitout[num, 2])
}