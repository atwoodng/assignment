
## Load the library
library(ggplot2)

## Function to generate the data
data.generate = function(n = 100, ext = 1){ 
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

w<-linearweight()

error<-function(weight=w,x=generated[["data"]],number=1000){
  constant<-rep(1,length(x[,1]))
  x<-cbind(constant,x)
  X<-as.matrix(x[,c(1,2,3)])
  Y<-as.matrix(x[,c(4)])
  product<-X%*%weight
  product2<-matrix(,nrow=length(product[,1]),ncol=1)
  for(i in 1:length(product[,1])){
    if(product[i,1]>=0){
      product2[i,1]<-1
    }
    else{
      product2[i,1]<--1
    }
  }
  counterror<-0
  for(i in 1:length(product2[,1])){
    if(product2[i,1]!=Y[i,1]){
      counterror<-counterror+1
    }
  }
  return(counterror/number)
}


Repeat<-function(){
  store<-vector()
  for(i in 1:1000){
    generated <-data.generate(n=1100)
    w<-linearweight(x=generated[["data"]][c(1:100),])
    temp<-error(weight=w,x=generated[["data"]][c(101:1100),],1000)
    store<-c(store,temp)
  }
  return(mean(store))
}