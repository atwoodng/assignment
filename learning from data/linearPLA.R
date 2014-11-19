
## Load the library
library(ggplot2)

## Function to generate the data
data.generate = function(n = 10, ext = 1){ 
  # Generate the points.
  x1 = runif(n, -ext, ext)
  x2 = runif(n, -ext, ext)
  
  # Draw a random line in the area.
  point = runif(2, -ext, ext)
  point2 = runif(2, -ext, ext)
  slope = (point2[2] - point[2]) / (point2[1] - point[1])
  intercept = point[2] - slope * point[1]
  
  # Assign the dependent values.
  y = as.numeric(x1 * slope + intercept > x2) * 2 - 1
  
  # Return the values.
  data = data.frame(x1,x2,y)
  return(list(data = data,slope = slope, intercept = intercept))
}  

## Generates the data; passing the parameters with desired numbers can change the points generated as well as the range. (n = 100, ext = 2)
generated = data.generate()

## Plot the data.
qplot(x1,x2,col= as.factor(y), data = generated$data) + geom_abline(intercept = generated$intercept, slope = generated$slope)

## linear regression
linearweight<-function(x=generated[["data"]]){
  constant<-rep(1,length(x[,1]))
  x<-cbind(constant,x)
  X<-as.matrix(x[,c(1,2,3)])
  Xinversed<-pseudoinverse(X)
  Y<-as.matrix(x[,c(4)])
  w<-Xinversed%*%Y
  return(w)
}

w<-as.vector(t(linearweight()))

count<-0
PLA<-function(independent1=generated, w1=w, count1=count){
  independent1 = as.matrix(cbind(1, independent1$data[c(1,2,3)]))
  boolean<-logical()
  for(i in 1:nrow(independent1)){
    result=w1[1]*independent1[i,1]+w1[2]*independent1[i,2]+w1[3]*independent1[i,3]
    if (result>0){
      score=1}
    else
    {score=-1}
    if (score==independent1[i,4]){
      boolean<-c(boolean,T)}
    else{
      boolean<-c(boolean,F)}
  }
  if (any(boolean==F)){
    if (count1<=10000){
      value<-which(boolean==F)
      selectedvalue<-sample(value,1)
      w1[1]=w1[1]+independent1[selectedvalue,4]*independent1[selectedvalue,1]
      w1[2]=w1[2]+independent1[selectedvalue,4]*independent1[selectedvalue,2]
      w1[3]=w1[3]+independent1[selectedvalue,4]*independent1[selectedvalue,3]
      count1<-count1+1
      return(PLA(independent1, w1,count1))}
    else{return(count1)}}
  else{
    count1<-count1+1
    return(count1)
  }
}

Repeat<-function(){
  temp<-vector()
  for (i in 1:1000){
    generated = data.generate()
    w<-as.vector(t(linearweight(x=generated[["data"]])))
    count<-0
    temp<-c(temp, PLA(generated,w,count))
  }
  return(mean(temp))
}