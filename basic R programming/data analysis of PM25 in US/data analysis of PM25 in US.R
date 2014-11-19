# DATA ANAYLSIS OF PM2.5 EMISSION from year 1999 to 2008 in US


#Data source
#http://www.epa.gov/ttn/chief/eiinformation.html


#set up your own working directory
setwd("F:/Users/commander shepard/Documents/R/course_project_2/")


# data import 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#------------task 1: total emission of PM2.5 of US in 1999,2002,2005 and 2008-----------------

# calculate total emission of PM2.5 of US in 1999,2002,2005 and 2008
PM25<-split(NEI$Emissions,NEI$year,drop=T)
x<-vector()
for(i in 1:4){
  x[i]<-sum(PM25[[i]])
}
names(x)<-c("1999","2002","2005","2008")
x<-x/1000

# plot the graph and export it as plot1
barplot(x,xlab="years",ylab="Emissions of PM2.5 (1000tons)", main="emission of PM2.5 on 1999,2002, 05 and 08", col=c("green","blue","gray","purple"))
dev.copy(device=png,"plot1.png")
dev.off()

#---------------end of ploting of "Emissions of PM2.5 on 1999, 2002, 05 and 08 in US"----------------------------

#---------------task 2: total emission of PM2.5 at Baltimore City, Maryland (fips == "24510")------------------------

# extract Baltimore City PM2.5 Data
MARYLAND<-NEI[NEI$fips == "24510",]
PM25<-split(MARYLAND$Emissions,MARYLAND$year,drop=T)
y<-vector()
for(i in 1:4){y[i]<-sum(PM25[[i]])}
names(y)<-c("1999","2002","2005","2008")
y<-y/1000

# directly export the graph to plot2 ( a different approaches than above)
png("plot2.png",width=640,height=480)
plot(y=y,x=c(1999,2002,2005,2008),xlab="years",ylab="Emissions of PM2.5 (1000tons)", main="emission of PM2.5 on 1999,2002, 05 and 08 in the Baltimore City",ylim=c(0,3.5),axes=F)
lines(y=y,x=c(1999,2002,2005,2008))
axis(1,at=c(1999,2002,2005,2008))
axis(2)
dev.off()

#---------------end of task 2--------------------------------------------------------------

#-----------------------task 3: comparing the emission of PM2.5 from different sources at Baltimore City, Maryland------------

library("ggplot2")

# aggregate is the fastest method in grouping and summarize the data. I can actually apply the same method above but i want to try to apply different approaches.
summary<-aggregate(MARYLAND$Emissions, by=list(MARYLAND$type,MARYLAND$year),sum)
summary$Group.1<-factor(summary$Group.1,levels=c("NON-ROAD","ON-ROAD","NONPOINT","POINT"))
png("plot3.png",width=640,height=480)
g<-ggplot(summary,aes(Group.2,x))
g+geom_line()+geom_point()+facet_wrap(~ Group.1, ncol=2)+labs(x="year")+labs(y="Emissions of PM2.5(Tons)")+labs(title="emission of PM2.5 on 1999,2002, 05 and 08 from 4 sources for Baltimore City")+scale_x_continuous(breaks=c(1999,2002,2005,2008))
dev.off()

#-----------------------end of task 3-------------------------------------------------------

#-------------task 4: emissions from coal combustion-related sources  from 1999¡V2008 in US---------------

# using regexp to search for data row collected from monitoring station near coal combustion 
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

#------------------------end of task 4------------------------------------------------------

#-------------------task 5: emissions from motor vehicle sources  from 1999¡V2008 in Baltimore City---------


#similar to task 4....
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

#----------------------end of task 5------------------------------------------------------

#----------task 6: comparison of emissions from motor vehicle sources in Baltimore City with Los Angeles County(fips == "06037")

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

#-----------------end----------------------------------------------------------------------