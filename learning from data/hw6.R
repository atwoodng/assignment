##q2
## estimate the difference between Eout and Ein

library("foreign")
getwd()
URL<-"http://work.caltech.edu/data/in.dta"
URL2<-"http://work.caltech.edu/data/out.dta"
download.file(URL,destfile="C:\\Users\\ipad\\Documents\\learning from data\\in.dta",method="auto")
download.file(URL2,destfile="C:\\Users\\ipad\\Documents\\learning from data\\out.dta",method="auto")

training.set<-read.table("C:\\Users\\ipad\\Documents\\learning from data\\in.dta")
test.set<-read.table("C:\\Users\\ipad\\Documents\\learning from data\\out.dta")


## transforming the data £p(x1, x2) into (1, x1, x2, x1^2, x2^2, x1*x2, | x1 ??? x2| , | x1 + x2|)
transformation<-function(x){
  transformed.data<-matrix()
  col1<-rep(1,nrow(x))
  col2<-x[,1]
  col3<-x[,2]
  col4<-col2**2
  col5<-col3**2
  col6<-col2*col3
  col7<-sqrt((col2-col3)**2)
  col8<-sqrt((col2+col3)**2)
  transformed.data<-cbind(col1,col2,col3,col4,col5,col6,col7,col8)
  transformed.data<-as.matrix(transformed.data)
  return(transformed.data)
}

transformed.data<-transformation(training.set)

## import linear regression function
source("C:\\Users\\ipad\\Documents\\learning from data\\linearweight.R")

optimized.weight<-linearweight(transformed.data,training.set[,3])


E.in<-error.measure(optimized.weight,transformed.data,training.set[,3] )

##result = 0.0285

##-----------------end of calculation of E-in--------
## calculation of E-out---------------
transformed.data2<-transformation(test.set)
E.out<-error.measure(optimized.weight,transformed.data2,test.set[,3] )


## calculating E-in with regularized linear regression function with lambda equal to 10**-3

regularized.weight<-modifed.linearweight(transformed.data,training.set[,3],10**-3)
E.in<-error.measure(regularized.weight,transformed.data,training.set[,3])
E.out<-error.measure(regularized.weight,transformed.data2,test.set[,3] )


## calculating E-in with regularized linear regression function with lambda equal to 10**3

regularized.weight<-modifed.linearweight(transformed.data,training.set[,3],10**3)
E.in<-error.measure(regularized.weight,transformed.data,training.set[,3])
E.out<-error.measure(regularized.weight,transformed.data2,test.set[,3] )


##Q10

lweight<-function(layer1,layer2){
  if(layer2==0){
    return(0)
  }else{
    my.weights<-layer1*(layer2-1)
    return(my.weights)
  }
}

overall.weight<-function(layer1=10,layer2=0,layer3=0,layer4=0,layer5=0){
  weights<-(lweight(layer1,layer2)+lweight(layer2,layer3)++lweight(layer3,layer4)+lweight(layer4,layer5))
  if(layer3==0){
    weights<-weights+layer2
  }else if (layer4==0){
    weights<-weights+layer3
  }else if (layer5==0) {
    weights<-weights+layer4
  }else if (layer5!=0){
    weights<-weights+layer5
  }
  return(weights)
}

