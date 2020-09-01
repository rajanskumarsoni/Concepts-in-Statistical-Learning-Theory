

complexity =c(4,14,26)
library(splines)
Q1_fun_01 <- function(x) {
  y = 0.3*cos(3*pi*x) - 0.4* cos(4*pi*x) - 10/(x**2 + 1)
  return(y) 
}

Q1_fun_02<- function(x){
  
  y = 0.4*log(x**4+log(x-0.7)+exp(3*x))
  return(y)
}

generate_y <-function(x, func,sig){
  if(func == 1){
    y = Q1_fun_01(x) + rnorm(1, 0, sig)
  }else{
    y = Q1_fun_02(x) + rnorm(1, 0, sig)
  }
  return(y)
}
# x_sample = sort(runif(number_of_sample, min=-10, max=10))
generate_samples <- function(x_sample,func,sig){
  my.array <- array(0, dim=c(length(x_sample)))
  
  for (i in 1 : length(x_sample)) {
    my.array[i] = generate_y(x_sample[i], func, sig)
  }
  
  return(my.array)
}


number_of_sample = 1000
number_of_simulations = 100
sig = 2 #sigma value for noise
x_test1 <-sort(runif(number_of_sample, min=-10, max=10))
y_real1 <-Q1_fun_01(x_test1)
y_test1 <- generate_samples(x_test1, 1,2)
x_test2 <-sort(runif(number_of_sample, min=1, max=10))
y_real2 <-Q1_fun_02(x_test2)
y_test2 <- generate_samples(x_test2, 2,2)



##############plot for model fitting function 1


plot(x_test1, y_test1,
     main="Q1_fun_01",col ='yellow',
     ylab="hat(y)", cex.lab = 1.5)
model11 <- lm(y_test1 ~ poly(x_test1,4))
model12 <- lm(y_test1 ~ poly(x_test1,14))
model13 <- lm(y_test1 ~ poly(x_test1,26))

lines(x_test1, predict(model11, data.frame(x=x_test1)), col='black')
lines(x_test1, predict(model12, data.frame(x=x_test1)), col='green')
lines(x_test1, predict(model13, data.frame(x=x_test1)), col='blue')
legend("bottomright",c("degree 4","degree 14", "degree 26"), col=c("black","green","blue"),lwd=3)

###################plot for model fitting function 2
plot(x_test2, y_test2,
     main="Q1_fun_02",col ='yellow',
     ylab="hat(y)", cex.lab = 1.5)
model21 <- lm(y_test2 ~ poly(x_test2,4))
model22 <- lm(y_test2 ~ poly(x_test2,14))
model23 <- lm(y_test2 ~ poly(x_test2,26))

lines(x_test2, predict(model21, data.frame(x=x_test2)), col='black')
lines(x_test2, predict(model22, data.frame(x=x_test2)), col='green')
lines(x_test2, predict(model23, data.frame(x=x_test2)), col='blue')
legend("bottomright",c("degree 4","degree 14", "degree 26"), col=c("black","green","blue"),lwd=3)

######################################################################Plots for bias variance tradeoff with complexity for function1

allbias1 = matrix(0,1,length(complexity))
allvariance1 = matrix(0,1,length(complexity))
for(com in 1:length(complexity)){
  
  
  predictions = matrix(0,number_of_simulations,number_of_sample)
  
  for (simulation in 1:number_of_simulations) {
    
      x_train <-sort(runif(number_of_sample, min=-10, max=10));
      y_train <- generate_samples(x_train, 1,sig);
    
      model <- lm(y_train ~ poly(x_train,complexity[com]));
    
      pred =  predict(model, data.frame(x=x_test1));
    #print(typeof(p))
    #print(class(pred));
    
    
      predictions[simulation,]<-pred;
    
  }
  l = colMeans(predictions);
   
  biasums = 0
  for(j in 1:number_of_sample){
        biasums = biasums + abs(l[j]-y_real1[j]);
  }
      
    
  bias = (biasums/number_of_sample);
    
    
    #finding variance
  varsum = 0;
  for(n in 1:number_of_sample){
      
      sums = 0;
      for(k in 1:number_of_simulations){
        sums = sums + (l[n]-predictions[k,n])^2
      }
      
      varsum =varsum + sums/number_of_simulations
      
  }
  variance = varsum/number_of_sample
  
  allbias1[1,com]<-bias
  allvariance1[1,com]<-variance
  
}

plot(complexity, allvariance1[1,], ylim =c(0,max(max(allbias1),max(allvariance1))),
     main="plot for Q1_fun_01",
     ylab="model behaviour",
     type = "l",
     col="blue", cex.lab = 1.5)

lines(complexity,allbias1[1,],col='black',lwd=1)
legend("bottomright",c("bias","variance"), col=c("black","blue"), lwd=3)

######################################################################Plots for bias variance tradeoff with complexity for function2

allbias2 = matrix(0,1,length(complexity))
allvariance2 = matrix(0,1,length(complexity))
for(com in 1:length(complexity)){
  
  
  predictions = matrix(0,number_of_simulations,number_of_sample)
  
  for (simulation in 1:number_of_simulations) {
    
    x_train <-sort(runif(number_of_sample, min=1, max=10));
    y_train <- generate_samples(x_train, 2,sig);
    
    model <- lm(y_train ~ poly(x_train,complexity[com]));
    
    pred =  predict(model, data.frame(x=x_test2));
    #print(typeof(p))
    #print(class(pred));
    
    
    predictions[simulation,]<-pred;
    
  }
  l = colMeans(predictions);
  
  biasums = 0
  for(j in 1:number_of_sample){
    biasums = biasums + abs(l[j]-y_real2[j]);
  }


  bias = (biasums/number_of_sample);
  # bias = abs(l[1]-y_real2[1])
  
  
  #finding variance
  varsum = 0
  for(n in 1:number_of_sample){

    sums = 0
    for(k in 1:number_of_simulations){
      sums = sums + (l[n]-predictions[k,n])^2
    }

    varsum =varsum + sums/number_of_simulations

  }
  print(varsum)
  variance = varsum/number_of_sample
  #    sums = 0
  #    for(k in 1:number_of_simulations){
  #      sums = sums + (l[1]-predictions[k,1])^2
  #    }
  # variance = sums/number_of_simulations
  allbias2[1,com]<-bias
  allvariance2[1,com]<-variance
  
}

plot(complexity, allvariance2[1,], ylim =c(0,max(max(allbias2),max(allvariance2))),
     main="plot for Q1_fun_02",
     ylab="model behaviour",
     type = "l",
     col="blue", cex.lab = 1.5)

lines(complexity,allbias2[1,],col='black',lwd=1)
legend("bottomright",c("bias","variance"), col=c("black","blue"), lwd=3)


print(max(allvariance2))
print(l)


varsum = 0;
for(n in 1:number_of_sample){
  
  sums = 0;
  for(k in 1:number_of_simulations){
    sums = sums + (l[n]-predictions[k,n])^2
  }
  
  varsum =varsum + sums/number_of_simulations
  
}
variance = varsum/number_of_sample
print(variance)