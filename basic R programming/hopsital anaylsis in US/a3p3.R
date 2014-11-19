#Plot the 30-day mortality rates for heart attack

readcsv<-read.csv("hospital/outcome-of-care-measures.csv", colClasses = "character")
readcsv[,11]<-as.numeric(readcsv[,11])
hospitalno<-table(readcsv$State)
largedata<-hospitalno[hospitalno>19]
haha<-vector()
for(i in 1:nrow(readcsv)){
  for(x in names(largedata))
         if(as.character(x) ==readcsv$State[i])
           {haha[i]<-T} 
}
truehaha<-!is.na(haha)
readcsv2<-readcsv[truehaha,]
death<-readcsv2[,11]
state<-readcsv2$State
par(las=2)
boxplot(death~state, ylab = "30-day death rate", main = "Heart Attack", pch = 20?re)
