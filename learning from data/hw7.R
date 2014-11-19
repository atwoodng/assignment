# assignment 7 of "learning from data"

#Q1
#using the Q6 training data

total.set<-read.table("C:\\Users\\ipad\\Documents\\learning from data\\in.dta")
training.set<-total.set[1:25,]
validation.set<-total.set[26:35,]


# function for transforming the data (x1, x2) into ??(1, x1, x2, x1^2, x2^2, x1*x2, | x1 ??? x2| , | x1 + x2|)
# j = number of ?? ended (start from 0,1,2,3,4......7) (constraint)
transformation<-function(x,j){
  comp.table<-matrix()  
  col1<-rep(1,nrow(x))
  col2<-x[,1]
  col3<-x[,2]
  col4<-col2**2
  col5<-col3**2
  col6<-col2*col3
  col7<-sqrt((col2-col3)**2)
  col8<-sqrt((col2+col3)**2)
  comp.table<-col1
  for(i in 2:c(j+1)){
    comp.table<-cbind(comp.table,get(paste("col",i,sep="")))
  }
  transformed.data<-as.matrix(comp.table)
  return(transformed.data)
}

## calculate the E-val for different constraint in hypothesis polynominal 
transformed.data<-transformation(total.set,6)

## import linear regression function
source("C:\\Users\\ipad\\Documents\\learning from data\\linearweight.R")

optimized.weight<-linearweight(transformed.data[1:25,],training.set[,3])


E.val<-error.measure(optimized.weight,transformed.data[26:35,],validation.set[,3])

# k=3 E.val=0.08, k=4, E.val=0.5 k=5, E.val=0.08, k=6, E.val=0, k=7 E.val=0.1

#---------------------------------------------------------------------------------------------

#Q2 

test.set<-read.table("C:\\Users\\ipad\\Documents\\learning from data\\out.dta")

# calcuating the E-out for hypothesis with different constraint in its polynominal 
# i = constraint
# train.set<- column 1 = x1, column 2= x2, column 3=y
Hyp.Eout<-function(train.set,test.set,i){
  transformed.train.data<-transformation(train.set,i)
  transformed.test.data<-transformation(test.set,i)
  optimized.weight<-linearweight(transformed.train.data,train.set[,3])
  E.val<-error.measure(optimized.weight,transformed.test.data,test.set[,3])
  print(paste("k=",as.character(i)))
  print(E.val)
}
  
for(i in 3:7){Hyp.Eout(training.set,test.set,i)}

# result
# [1] "k= 3"
# [1] 0.42
# [1] "k= 4"
# [1] 0.416
# [1] "k= 5"
# [1] 0.188
# [1] "k= 6"
# [1] 0.084
# [1] "k= 7"
# [1] 0.072

#--------------------------------------------------------------------------------

#q3

# plz refer back to q1

transformed.data<-transformation(total.set,3)
optimized.weight<-linearweight(transformed.data[26:35,],validation.set[,3])
E.val<-error.measure(optimized.weight,transformed.data[1:25,],training.set[,3])

#k=3 Eval=0.28, k=4 Eval=0.36, k=5 Eval = 0.2 , k=6 Eval=0.08, k=7, Eval= 0.12

#-----------------------------------------------------------------------------

#q4

for(i in 3:7){Hyp.Eout(validation.set,test.set,i)}

# result
# [1] "k= 3"
# [1] 0.396
# [1] "k= 4"
# [1] 0.388
# [1] "k= 5"
# [1] 0.284
# [1] "k= 6"
# [1] 0.192
# [1] "k= 7"
# [1] 0.196


#---------------------------------------------------------------------------------

#q6

temp<-vector()
for(i in 1:10000){
  e1<-runif(1,0,1)
  e2<-runif(1,0,1)
  e<-min(e1,e2)
  temp<-c(temp,e)
}
hist(temp,breaks=100)

sum(temp)/length(temp)

#fast method for q7

# for h(x)= total E = 1.5/3 

calucation<-function(p){
  total=1**2+(-2/(p-1))**2+(2/(p+1))**2
  print(total)
}


#--------------------------------------------------------------------------------------
#8
source("C:\\Users\\ipad\\Documents\\learning from data\\PLA.r")

# for quadratic programming 
library("LowRankQP", lib.loc="C:/Program Files/R/R-3.1.1/library")

# testing of quadratic programming
X <- matrix(c(1,0,0,1,2,2,1, 2,0,1,3,0), ncol=3, byrow = TRUE)
Y <- c(-1,-1,1,1)
N <- 4
Vmat<-(Y*X[,-1])%*%t((Y*X[,-1]))
dvec<-rep(-1,4)
Amat<-matrix(Y,nrow=1)
bvec<-0
uvec<-rep(10000000,4)
result<-LowRankQP(Vmat,dvec,Amat,bvec,uvec,method="LU")
w<-t(X[,-1])%*%((result$alpha)*Y)
#------------success!-----------------------------------------------------------

#implementation of hard margin SVM
#X = matrix with first column = 1
#Y=element vector
library("LowRankQP", lib.loc="C:/Program Files/R/R-3.1.1/library")

SVM<-function(X,Y){
  N<-length(Y)
  Vmat<-(Y*X[,-1])%*%t((Y*X[,-1]))
  dvec<-rep(-1,N)
  Amat<-matrix(Y,nrow=1)
  bvec<-0
  uvec<-rep(10000000,N)
  result<-LowRankQP(Vmat,dvec,Amat,bvec,uvec,method="LU",verbose=F)
  w<-t(X[,-1])%*%((result$alpha)*Y)
  beta<-result$beta
  overall<-list(w,beta)
  return (overall)
}

#.......
SVMtest<-function(test.setx,test.sety,overall){
  temp<-vector()
  temp<-sign((test.setx[,-1]%*%overall[[1]])+as.vector(overall[[2]]))
  print(temp)
  counter=0
  for (i in 1:length(temp)){
    if(test.sety[i] != temp[i]){
      counter<-counter+1
    }
  }
  return(counter)
}




generated = data.generate(n=500)
complete = as.matrix(cbind(1, generated$data[c(1,2,3)]))
training.set<-complete[1:10,]
testing.set<-complete[11:500,]
testing.setx<-testing.set[,1:3]
testing.sety<-testing.set[,4]
SVMw<-SVM(training.set[,1:3],training.set[,4])
SVMtest(testing.setx,testing.sety,SVMw)





