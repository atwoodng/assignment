
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

## Initializing PLA
independent = as.matrix(cbind(1, generated$data[c(1,2,3)]))
w = c(0,0,0)

## PLA
# col1 = 1
# col2 = x1
# col3 = x2
# col4 = y

PLA<-function(independent1=independent, w1=w){
  boolean<-logical()
  boolean<-rep(F,nrow(independent1))
  count1<-0 
  while(any(boolean==F)& count1<200 ){
    
    selectedvalue<-(which(boolean==F))[1]
    w1[1]=w1[1]+independent1[selectedvalue,4]*independent1[selectedvalue,1]
    w1[2]=w1[2]+independent1[selectedvalue,4]*independent1[selectedvalue,2]
    w1[3]=w1[3]+independent1[selectedvalue,4]*independent1[selectedvalue,3]
    count1<-count1+1
    
    for(i in 1:nrow(independent1)){
      result=w1[1]*independent1[i,1]+w1[2]*independent1[i,2]+w1[3]*independent1[i,3]
      if (result>0){
        score=1
      }
      else
      {score=-1}
      if (score==independent1[i,4]){
        boolean[i]<-T
      }
    }
  }
  return(w1)
}


#____________________

duplicate<-function(time=100){
  allfinal<-vector()
  for (i in 1:time){
  generated <- data.generate()
  independent <- as.matrix(cbind(1, generated$data[c(1,2,3)]))
  w <- c(0,0,0)
  finalcount<-PLA(independent,w)
  allfinal<-c(allfinal,finalcount)
  }
  return (mean(allfinal))
}


