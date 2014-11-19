
## generalization bound

growthfunction<-function(n,k){
  temp<-0
  for (i in 0:(k-1)){
    temp<-temp+choose(n,i)
  }
  return(temp)
}

omega<-function(N,DVC,delta){
  result<-sqrt(8/N*log(4*N**DVC/delta))
  print(result)
}


## cumulative binomial curve


inequality<-function(size,oprob, dprob){
  temp<-pbinom(size*(oprob-dprob), size, oprob)
  return(temp)
}
## interval size of binomial curve

inequality2<-function(size,oprob, dprob){
  temp1<-pbinom(size*(oprob-dprob), size, oprob)
  temp2<-pbinom(size*(oprob+dprob), size, oprob)
  return(temp2-temp1)
}






## visualize the bionominal distribution

plotprob<-function(size,oprob){
  x<-seq(0.00,oprob,0.01)
  
  summary<-data.frame()
  for(i in 1:length(x)){
    summary[i,1]<-x[i]
    summary[i,2]<-inequality(size, oprob,x[i])
  }
  plot(summary[,1],summary[,2],"l")
  
}




