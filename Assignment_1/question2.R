set.seed(46)
simulations = 1000
library(ggplot2)
my.array <- array(0, dim=c(simulations))
#my.array <-as.numeric(my.array)

for (i in 1 : simulations) {
  my.array[i] = mean(rexp(40,0.5))
}
data <- data.frame(my.array,size=40)
print(data)
sampleMean <- mean(my.array)
sampleVariance <- var(my.array)
print(sampleMean)
print(sampleVariance)
theoreticalVariance <- ((1/0.5)/sqrt(40))^2
print(theoreticalVariance)

means <- data.frame(my.array)
print(dim(means))
print(class(as.numeric(my.array)))



ggplot(data,aes(x=my.array,fill=size))+
  #theme_bw()+
  geom_histogram(aes(y=..density..),alpha=0.7,binwidth=.05,col="black") +
  ylim(c(0,2))+
  #stat_function(fun=dnorm,args=list(mean=2,sd=sd(my.array)))+
  geom_vline(aes(xintercept=mean(my.array),colour="red")) +
  #geom_text(aes(x=mean(my.array),label="\nsample mean",y=0.5),colour="black",angle=90, text = element_text(size=11))+
  xlab("sample means") + ylab("Density of sample mean")+
  ggtitle("plot of density of sample means and gaussian distribution")
  
cumulative_mean = list()
counts = 0
sums = 0
li = list()
for(mu in my.array){
  counts = counts +1;
  sums = sums + mu;
  li[counts]<-mu
  print(li)
  cumulative_mean[counts]<-var(li)
  
}
# g <- ggplot(data.frame(x = 1:simulations, y = cumulative_mean), aes(x = x, y = y))
# g <- g + geom_hline(yintercept = 0) + geom_line(size = 2)
# g <- g + geom_abline(intercept = sampleMean, slope = 0, color = "blue", size = 1)
# g <- g + scale_y_continuous(breaks=c(1, 1.4, 1.8, 2.2, 2.6, 3), limits=c(1, 3))
# g <- g + labs(title="Sample Mean vs Theoretical  Mean")
# g <- g + labs(x = "Simulations", y = "Sample Mean")
# print(g)

simulations_count_array = list()
real_mean = list()
for(i in 1:simulations){
  simulations_count_array[i]<-i
  real_mean[i] = 2
}


plot(simulations_count_array, cumulative_mean, ylim =c(1.5,2.5),
     main="Sample Mean vs Theoretical  Mean",
     ylab="sample mean",xlab = "simulations",
     type = "l",
     col="blue", cex.lab = 1.5)

lines(simulations_count_array,real_mean,col='black',lwd=1)
legend("bottomright",c("Theoretical  Mean","Sample mean"), col=c("black","blue"), lwd=3)


