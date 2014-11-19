library(corpcor)

## Function to generate the data
data.generate = function(n = 100, ext = 1){ 
  # Generate the points.
  x1 = runif(n, -ext, ext)
  x2 = runif(n, -ext, ext)
  yinital<-x1**2+x2**2-0.6
  y<-vector()
  for(i in 1:length(yinital)){
    if(yinital[i]>=0){
      y<-c(y,1)
    }else{
      y<-c(y,-1)
    }
  }
  y[1:10]<-y[1:10]*-1
  data = data.frame(x1,x2,y)
  return(data)
}  

## Generates the data; passing the parameters with desired numbers can change the points generated as well as the range. (n = 100, ext = 2)
generated = data.generate()



## linear regression
linearweight<-function(x=generated){
  constant<-rep(1,length(x[,1]))
  x<-cbind(constant,x)
  X<-as.matrix(x[,c(1,2,3)])
  Xinversed<-pseudoinverse(X)
  Y<-as.matrix(x[,c(4)])
  w<-Xinversed%*%Y
  return(w)
}

w<-linearweight()

error<-function(weight=w,x=generated,number=100){
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
    generated <-data.generate(n=100)
    w<-linearweight(x=generated)
    temp<-error(weight=w,x=generated,100)
    store<-c(store,temp)
  }
  return(mean(store))
}