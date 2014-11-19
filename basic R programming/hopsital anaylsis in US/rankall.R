rankall<-function(outcome,num ="best"){
  complete<-read.csv("hospital/outcome-of-care-measures.csv" ,colClasses = "character")
  coloutcome<-c(11,17,23)
  names(coloutcome)<-c("heart attack","heart failure", "pneumonia")
  pickitout<-complete[,c(coloutcome[names(coloutcome)%in% outcome],which(names(complete)== "State"),2)]
  pickitout[,2]<-as.factor(pickitout[,2])
 pickitout[,1]<-suppressWarnings(as.numeric(pickitout[,1]))
 pickitout2<-split(pickitout,pickitout[,2])

 x<-matrix(,0,2)
 
 if (num == "best"){num2<-1}
 if (is.numeric(num)){num2<-num}
 for (i in levels(pickitout[,2])){ 
   pickitout2[[i]]<-pickitout2[[i]][order(pickitout2[[i]][,1],pickitout2[[i]][,3],na.last= T),]
   if (num == "worst"){ num2<-sum(!is.na(pickitout2[[i]][,1])) }
  x<-rbind(x,c(pickitout2[[i]][num2,3],i))
 }
 x<-as.data.frame(x)
 colnames(x)<-c("hospital","state")
 return(x)
    
}