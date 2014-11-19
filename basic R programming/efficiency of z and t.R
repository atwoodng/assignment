is.between <- function(x, a, b) {
  x > a & x < b
}
set.seed(09101165)
##population<-rnorm(n=10000, mean=100, sd =10)
population<-runif(n=10000, min=50,max=100)
## you can use runif() instead of rnorm for generating a evenly distributed population and get a totally different result
mean_of_population<-mean(population)
var(population)
## var = 102.5493
master_of_sample<-list()
for (ss in 3:50){
  group_of_sample<-list()
  for (i in 1:1000){
    group_of_sample[[i]]<-sample(population,size=ss)
  }
  master_of_sample[[ss]]<-group_of_sample
}
Mean<-vector()
Variance<-vector()
upper_interval<-vector()
lower_interval<-vector()
upper_interval2<-vector()
lower_interval2<-vector()
within_confidence_interval<-logical()
within_confidence_interval2<-logical()
finalt<-vector()
finalz<-vector()

for (ss in 3:50){
  Mean<-vector()
  Variance<-vector()
  upper_interval<-vector()
  lower_interval<-vector()
  within_confidence_interval<-logical()
  for (i in 1:1000){Mean[i]<-mean(master_of_sample[[ss]][[i]])
                    Variance[i]<-var(master_of_sample[[ss]][[i]])
                    upper_interval[i]<-Mean[i]+qt(0.975,df=(ss-1))*(sqrt(Variance[i]/ss))
                    lower_interval[i]<-Mean[i]-qt(0.975,df=(ss-1))*(sqrt(Variance[i]/ss))
                    upper_interval2[i]<-Mean[i]+qnorm(0.975)*(sqrt(Variance[i]/ss))
                    lower_interval2[i]<-Mean[i]-qnorm(0.975)*(sqrt(Variance[i]/ss))
                    within_confidence_interval[i]<-is.between(mean_of_population,lower_interval[i],upper_interval[i])
                    within_confidence_interval2[i]<-is.between(mean_of_population,lower_interval2[i],upper_interval2[i])
 }
  finalt[ss]<-sum(within_confidence_interval)/length(within_confidence_interval)
  finalz[ss]<-sum(within_confidence_interval2)/length(within_confidence_interval2)
}

plot(y=finalt[3:50], x=3:50,ylim=c(0.8,1),type="n", ylab="population mean lying within sample's confidence interval (proportion)", xlab="sample size", 
     main="efficiency of calculating confidence interval using z and t distribution for unknown population variance" )
points(y=finalt[3:50], x=3:50,col="green",pch=3)
fort<-smooth.spline(finalt[3:50]~3:50,df=3)
lines(fort,col="green")
points(y=finalz[3:50], x=3:50,col="blue",pch=2 )
forz<-smooth.spline(finalz[3:50]~3:50,df=3)
lines(forz,col="blue")
legend("topleft",legend=c("using t distribution","using z distribution"),pch=c(3,2),col=c("green","blue"))

