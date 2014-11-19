setwd("C:/Users/commander shepard/Documents/R")
library("ggplot2")
NEI <- readRDS("course_project_2/summarySCC_PM25.rds")
SCC <- readRDS("course_project_2/Source_Classification_Code.rds")
coalSCC<-grepl("Mobile",SCC$EI.Sector)
coalSCC2<-SCC$SCC[coalSCC]
NEI2<-NEI[NEI$fips == "24510",]
NEImobile<-NEI2[NEI2$SCC %in% coalSCC2,]
coalSCCname<-SCC[coalSCC,]
NEIcoalname<-merge(coalSCCname, NEImobile, by.x="SCC",by.y="SCC")
NEIcoalname<-NEIcoalname[order(NEIcoalname$EI.Sector),]
###
g<-ggplot(NEIcoalname,aes(x=factor(year,level=c("1999","2002","2005","2008")),y=Emissions/1000,fill=factor(EI.Sector)))+geom_bar(stat="identity",position="stack")+labs(x="year")+labs(y="Emissions (1000 tons)")+labs(title="emission of PM2.5 on 1999,2002, 05 and 08 due to motor vehicle source")+scale_fill_brewer(palette="Set3")+theme_bw()+labs(fill ="source of PM2.5")
png("plot5.png",width = 700, height = 480)
g
dev.off()
