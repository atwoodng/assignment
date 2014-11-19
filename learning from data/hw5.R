## gradient desecent 

## equation for Error 

error<-function(u,v){
  result<-(u*exp(v)-2*v*exp(-u))**2
  return(result)
}

##gardient of  (u*exp(v)-2v*exp(-u))**2

errore<-expression((u * exp(v) - 2 * v * exp(-u))^2)
errorexp<-deriv(errore,c("u","v"),func=T)

##set up a function for calculation of du
du<-function(u,v){
  result<-errorexp(u,v)
  pdu<-unname(attr(result, "gradient")[,1])
  return(pdu)
}

dv<-function(u,v){
  result<-errorexp(u,v)
  pdv<-unname(attr(result, "gradient")[,2])
  return(pdv)
}


question5<-function(a=1,b=1){
  counter<-1
  Er<-error(a,b)
  while(Er>=10**-14){
    tempa<-a
    tempb<-b
    a<-a-0.1*du(tempa,tempb)
    b<-b-0.1*dv(tempa,tempb)
    counter<-counter+1
    Er<-error(a,b)
    print(counter)
    print(Er)
  }
  print("done!")
  print(a)
  print(b)
}

question7<-function(a=1,b=1){
  counter<-1
  turn<-1
  Er<-error(a,b)
  while(turn<15){
    tempa<-a
    tempb<-b
    a<-a-0.1*du(a,b)
    b<-b-0.1*dv(a,b)
    counter<-counter+1
    Er<-error(a,b)
    turn<-turn+1
    print(counter)
    print(Er)
  }
  print("done!")
  print(a)
  print(b)
}

## question 8: generating a logistic regression simulation

data.generate <- function(n = 10000, ext = 1){ 
  # Generate the points.
  constant = rep(1,n)
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
  data = data.frame(constant,x1,x2,y)
  return(data)
}	

data<-data.generate()
data<-as.matrix(data)
##create logistic functicon which x is matrix and w is column vector

theta<-function(s){
  product<-exp(s)/(1+exp(s))
  return(product)
  }

logistic<-function(x,w){
  x<-as.matrix(x)
  w<-as.matrix(w)
  s<-x%*%w
  product<-theta(s)  
}

#error measurement

error<-function(x,y,w){
  x<-as.matrix(x)
  y<-as.matrix(y)
  w<-as.matrix(w)
  total<-0
  for(i in 1:nrow(y)){
    total<-total+log(1+exp(-y[i]*x[i,]%*%w))
  }
  total<-total/nrow(y)
  return(total)
}



gradient<-function(x,y,w,n){
  x<-as.matrix(x)
  y<-as.matrix(y)
  w<-as.matrix(w)
  result<--y[n]*(x[n,])/(1+exp(y[n]*x[n,]%*%w))
  return(unname(result))
}



## 
minerror<-function(x,y,w){
  x<-as.matrix(x)
  y<-as.matrix(y)
  w<-as.matrix(w)
  difference=1
  iteration<-0
  while(difference>=0.01){
    oldtemp<-w
    random<-as.integer(runif(nrow(y),1,nrow(x)))
    overall<-0
    for(i in 1:nrow(y)){
      slope<-gradient(x,y,w,random[i])
      w[1,]<-w[1,]-0.01*slope[1]
      w[2,]<-w[2,]-0.01*slope[2]
      w[3,]<-w[3,]-0.01*slope[3]     
    }
    difference<-sqrt((oldtemp[1,]-w[1,])**2+(oldtemp[2,]-w[2,])**2+(oldtemp[3,]-w[3,])**2)
    iteration<-iteration+1
    print(iteration)

  }
  return(w)



error(data[101:1000,1:3],data[101:1000,4],c( -4.600145,-1.328112, -7.287354))