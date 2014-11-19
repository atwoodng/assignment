setwd("C:/Users/commander shepard/Documents/R")
##
NEI <- readRDS("course_project_2/summarySCC_PM25.rds")
SCC <- readRDS("course_project_2/Source_Classification_Code.rds")
MARYLAND<-NEI[NEI$fips == "24510",]
PM25<-split(MARYLAND$Emissions,MARYLAND$year,drop=T)
y<-vector()
for(i in 1:4){y[i]<-sum(PM25[[i]])}
names(y)<-c("1999","2002","2005","2008")
y<-y/1000
png("plot2.png",width=640,height=480)
plot(y=y,x=c(1999,2002,2005,2008),xlab="years",ylab="Emissions of PM2.5 (1000tons)", main="emission of PM2.5 on 1999,2002, 05 and 08 in the Baltimore City",ylim=c(0,3.5),axes=F)
lines(y=y,x=c(1999,2002,2005,2008))
axis(1,at=c(1999,2002,2005,2008))
axis(2)
dev.off()
