setwd("C:/Users/commander shepard/Documents/R")
library("ggplot2")
NEI <- readRDS("course_project_2/summarySCC_PM25.rds")
SCC <- readRDS("course_project_2/Source_Classification_Code.rds")
coalSCC<-grepl("[Cc]omb.*[Cc]oal",SCC$EI.Sector)
coalSCC2<-SCC$SCC[coalSCC]
NEIcoal<-NEI[NEI$SCC %in% coalSCC2,]
coalSCCname<-SCC[coalSCC,]
NEIcoalname<-merge(coalSCCname, NEIcoal, by.x="SCC",by.y="SCC")
NEIcoalname<-NEIcoalname[order(NEIcoalname$EI.Sector),]
g<-ggplot(NEIcoalname,aes(x=factor(year,level=c("1999","2002","2005","2008")),y=Emissions/1000,fill=factor(EI.Sector)))+geom_bar(stat="identity",position="stack")+labs(x="year")+labs(y="Emissions(1000Tons)")+labs(title="emission of PM2.5 on 1999,2002, 05 and 08 due to coal combination")+labs(fill ="source of PM2.5")

png("plot4.png",width = 700, height = 480)
g
dev.off()

