## caluclate the optimized weight using linear regression

library("corpcor")
library("MASS")

## linear regression without regularizer 
linearweight<-function(x,y){  

  X<-as.matrix(x)
  Xinversed<-pseudoinverse(X)
  Y<-as.matrix(y)
  w<-Xinversed%*%Y
  return(w)
}

## linear regression with regularization
modifed.linearweight<-function(x,y,lambda){  
  
  X<-as.matrix(x)
  Xinversed<-ginv(t(X)%*%X+lambda*diag(ncol(X)))%*%t(X)
  Y<-as.matrix(y)
  w<-Xinversed%*%Y
  return(w)
}







## calculate the E-in or E-out of linear classifcation for linear regression
##input: w = weight, x = transformed/raw input, y = output
##output: percentage of error 

error.measure<-function(weight=w,x,y){
  X<-as.matrix(x)
  Y<-as.matrix(y)
  product<-X%*%weight
  temp.result<-matrix(,nrow=length(product[,1]),ncol=1)
  for(i in 1:length(product[,1])){
    if(product[i,1]>=0){
      temp.result[i,1]<-1
    }
    else{
      temp.result[i,1]<--1
    }
  }
  counterror<-0
  for(i in 1:length(temp.result[,1])){
    if(temp.result[i,1]!=Y[i,1]){
      counterror<-counterror+1
    }
  }
  return(counterror/nrow(Y))
}