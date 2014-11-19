setwd("C:/Users/commander shepard/Documents/R")
library("ggplot2")
NEI <- readRDS("course_project_2/summarySCC_PM25.rds")
SCC <- readRDS("course_project_2/Source_Classification_Code.rds")
mobileSCC<-grepl("Mobile",SCC$EI.Sector)
mobileSCC2<-SCC$SCC[mobileSCC]
NEI2<-NEI[NEI$fips == "06037"|NEI$fips =="24510",]
NEImobile<-NEI2[NEI2$SCC %in% mobileSCC2,]
mobileSCCname<-SCC[mobileSCC,]
NEImobilename<-merge(mobileSCCname, NEImobile, by.x="SCC",by.y="SCC")
NEImobilename<-NEImobilename[order(NEImobilename$EI.Sector),]
superfinal<-aggregate(NEImobilename$Emissions, by=list(NEImobilename$fips,NEImobilename$year),sum)
g<-ggplot(superfinal,aes(x=factor(Group.2,level=c("1999","2002","2005","2008")),y=x/1000,fill=factor(Group.1)))+geom_bar(stat="identity",position="dodge",width=0.5)+labs(x="year")+labs(y="Emissions (1000 tons)")+labs(title="emission of PM2.5 on 1999,2002, 05 and 08 due to motor vehicle source")+scale_fill_brewer(palette="Set3",labels=c("Los Angeles County","Baltimore City"))+theme_bw()+labs(fill ="city")
png("plot6.png",width = 700, height = 480)
g
dev.off()
