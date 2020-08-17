library(forecast)
#library(Rdonlp2)
library(tseries)
library(alabama)
library(timeDate)



### put all data in the same list
datafinal <- list(NULL)
dataPfinal <- list(NULL)





## AMZN

K=read.csv("AMZN.csv");n=length(K$Date);
k=K$Close
K=log10(K$Close)[2:n]-log10(K$Close)[1:(n-1)]

datafinal <- c(datafinal,list(K))
dataPfinal <- c(dataPfinal,list(k))

## MSFT

K=read.csv("MSFT.csv");n=length(K$Date);
k=K$Close
K=log10(K$Close)[2:n]-log10(K$Close)[1:(n-1)]

datafinal <- c(datafinal,list(K))
dataPfinal <- c(dataPfinal,list(k))

## NEE

K=read.csv("NEE.csv");n=length(K$Date);
k=K$Close
K=log10(K$Close)[2:n]-log10(K$Close)[1:(n-1)]

datafinal <- c(datafinal,list(K))
dataPfinal <- c(dataPfinal,list(k))

## SBUX

K=read.csv("AMZN.csv");n=length(K$Date);
k=K$Close
K=log10(K$Close)[2:n]-log10(K$Close)[1:(n-1)]

datafinal <- c(datafinal,list(K))
dataPfinal <- c(dataPfinal,list(k))

## SIVB

K=read.csv("SIVB.csv");n=length(K$Date);
k=K$Close
K=log10(K$Close)[2:n]-log10(K$Close)[1:(n-1)]

datafinal <- c(datafinal,list(K))
dataPfinal <- c(dataPfinal,list(k))

## STE

K=read.csv("STE.csv");n=length(K$Date);
k=K$Close
K=log10(K$Close)[2:n]-log10(K$Close)[1:(n-1)]

datafinal <- c(datafinal,list(K))
dataPfinal <- c(dataPfinal,list(k))

## STT

K=read.csv("STT.csv");n=length(K$Date);
k=K$Close
K=log10(K$Close)[2:n]-log10(K$Close)[1:(n-1)]

datafinal <- c(datafinal,list(K))
dataPfinal <- c(dataPfinal,list(k))

## AMD

K=read.csv("AMD.csv");n=length(K$Date);
k=K$Close
K=log10(K$Close)[2:n]-log10(K$Close)[1:(n-1)]

datafinal <- c(datafinal,list(K))
dataPfinal <- c(dataPfinal,list(k))

## SYK

K=read.csv("SYK.csv");n=length(K$Date);
k=K$Close
K=log10(K$Close)[2:n]-log10(K$Close)[1:(n-1)]

datafinal <- c(datafinal,list(K))
dataPfinal <- c(dataPfinal,list(k))

## UTX

K=read.csv("AMZN.csv");n=length(K$Date);
k=K$Close
K=log10(K$Close)[2:n]-log10(K$Close)[1:(n-1)]

datafinal <- c(datafinal,list(K))
dataPfinal <- c(dataPfinal,list(k))

datafinal[[1]] <- NULL
dataPfinal[[1]] <- NULL

databackup <- datafinal
datafinal <- data.frame(datafinal)

dataname <- c("AMZN","MSFT","NEE","SBUX","SIVB","STE",
              "STT","AMD","SYK","UTX")  #change the name
colnames(datafinal) <-  dataname

garchfinding <- function(data){
  model <- list()
  for(i in 1:10){
    logdiff <- data[[i]]
    model[[i]] <- garchchosen(logdiff)
  }
  return(model)
}

garchchosen <- function(data){
  parnum <- matrix(c(1,1,2,2,1,2,1,2),nrow = 4, byrow = F)
  models <- list()
  mse <- numeric(4)
  for(i in 1:4){
    models[[i]] <- garch(data, order = parnum[i,],trace = F)
    mse[i] <- mean(models[[i]]$residuals^2,na.rm = T)
  }
  for(index in order(mse)){
    if(!is.na(sum(summary(models[[index]])$coef[,2]))){
      return(models[[index]])
    }
  }
  return("NO APPROPRIATE MODEL!!!")
}

garchmodels <- garchfinding(datafinal)

for(i in 1:10){
  print(dataname[i])
  print(summary(garchmodels[[i]]))
  print(shapiro.test(na.omit(residuals(garchmodels[[i]]))))
  print(jarque.bera.test(na.omit(residuals(garchmodels[[i]]))))
  print(skewness(na.omit(residuals(garchmodels[[i]]))))
  print(kurtosis(na.omit(residuals(garchmodels[[i]]))))
  cat("\n","\n","\n")
}


#### step 1-4 finished




par0 <- rep(1/10,10)



varCal <- function(x,data){
  vec <- x[1]*data[[1]]
  for(i in 2:10){
    vec <- vec + x[i]*data[[i]]
  }
  return(var(vec))
}
meanCal <- function(x,data){
  vec <- x[1]*data[[1]]
  for(i in 2:10){
    vec <- vec + x[i]*data[[i]]
  }
  return(mean(vec))
}

mean0 <- meanCal(par0,datafinal)
var0 <- varCal(par0,datafinal)
sqrt(var0)

optimFun <- function(x,data,var0){
  return(-meanCal(x,data))
}
constrFunIEQ <- function(x,data,var0){
  return(c(var0-varCal(x,data),x))
}
constrFunEQ <- function(x,data,var0){
  return(1-sum(x))
}
EQjacob <- function(x,data,var0){
  return(matrix(-1,nrow = 1,ncol = 10))
}
parinit <- c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.91,0.01) 
#test for your own data

OptimResult <- constrOptim.nl(par = parinit,
                              fn = optimFun,data = datafinal,var0=var0,
                              heq = constrFunEQ,
                              heq.jac = EQjacob,
                              hin = constrFunIEQ,
                              control.optim = list(maxit=100000,
                                                   reltol=1e-13) 
                  #if error: vmmin not finite... run with larger reltol
)

meanCal(x=OptimResult$par,datafinal)
sqrt(varCal(x=OptimResult$par,datafinal))




errorfinal <- data.frame(residuals(garchmodels[[1]]),
                         residuals(garchmodels[[2]]),
                         residuals(garchmodels[[3]]),
                         residuals(garchmodels[[4]]),
                         residuals(garchmodels[[5]]),
                         residuals(garchmodels[[6]]),
                         residuals(garchmodels[[7]]),
                         residuals(garchmodels[[8]]),
                         residuals(garchmodels[[9]]),
                         residuals(garchmodels[[10]]))
colnames(errorfinal) <- dataname
naindex <- NULL
for(i in seq_len(dim(errorfinal)[1])){
  if(is.na(sum(errorfinal[i,]))){
    naindex <- c(naindex,i)
  }
}
errorfinal <- errorfinal[-naindex,]

parinit_error <- c(0.09,0.11,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)

mean0_error <- meanCal(par0,errorfinal)
var0_error <- varCal(par0,errorfinal)
sqrt(var0_error)

OptimResult_error <- constrOptim.nl(par = parinit_error,
                                    fn = optimFun,data = errorfinal,var0=var0_error,
                                    heq = constrFunEQ,
                                    heq.jac = EQjacob,
                                    hin = constrFunIEQ,
)

mean1_error <- meanCal(OptimResult_error$par,errorfinal)
var1_error <- varCal(OptimResult_error$par,errorfinal)
sqrt(var1_error)








varCal_log <- function(x,data){
  vec <- x[1]*data[[1]]
  for(i in 2:10){
    vec <- vec + x[i]*data[[i]]
  }
  return(var(diff(log(vec))))
}
meanCal_log <- function(x,data,mean = T){
  vec <- x[1]*data[[1]]
  for(i in 2:10){
    vec <- vec + x[i]*data[[i]]
  }
  if(mean) {
    return(mean(diff(log(vec))))
  }
  else{
    return(vec)
  }
}

mean0_log <- meanCal_log(par0,dataPfinal)
var0_log <- varCal_log(par0,dataPfinal)
sqrt(var0_log)

optimFun_log <- function(x,data,var0){
  return(-meanCal_log(x,data))
}
constrFunIEQ_log <- function(x,data,var0){
  return(c(var0-varCal_log(x,data),x))
}

parinit_log <- c(0.01,0.01,0.91,0.01,0.01,0.01,0.01,0.01,0.01,0.01)

OptimResult_log <- constrOptim.nl(par = parinit_log,
                                  fn = optimFun_log,data = dataPfinal,var0=var0_log,
                                  heq = constrFunEQ,
                                  heq.jac = EQjacob,
                                  hin = constrFunIEQ_log,
                                  control.optim = list(maxit=100000,reltol=1e-9) 
)

meanCal_log(OptimResult_log$par,dataPfinal)
sqrt(varCal_log(OptimResult_log$par,dataPfinal))

dataseries_0.1 <- meanCal_log(par0, dataPfinal, mean=F)

series=ts(dataseries_0.1,frequency = 1)
plot(series, xlab="Time daily", ylab="Index Return")
model_0.1 <- garchchosen(dataseries_0.1)
plot(residuals(model_0.1),xlab="Index",ylab="Standard Residuals",type="l")

mean(residuals(model_0.1),na.rm = T)
sd(residuals(model_0.1),na.rm = T)

res=model_0.1$residuals
qqnorm(res)
qqline(res)
acf(res^2,na.action = na.omit)
sum(na.omit(res)^2)

print(summary(model_0.1))
print(shapiro.test(na.omit(residuals(model_0.1))))
print(jarque.bera.test(na.omit(residuals(model_0.1))))
print(skewness(na.omit(residuals(model_0.1))))
print(kurtosis(na.omit(residuals(model_0.1))))

dataseries_best <- meanCal_log(OptimResult_log$par, dataPfinal, mean = F)

model_best <- garchchosen(dataseries_best)

series=ts(dataseries_best,frequency = 1)
plot(series, xlab="Time daily", ylab="Index Return")
model_best <- garchchosen(dataseries_best)
plot(residuals(model_best),xlab="Index",ylab="Standard Residuals",type="l")

mean(residuals(model_best),na.rm = T)
sd(residuals(model_best),na.rm = T)

res=model_best$residuals
qqnorm(res)
qqline(res)
acf(res^2,na.action = na.omit)
sum(na.omit(res)^2)

print(summary(model_best))
print(shapiro.test(na.omit(residuals(model_best))))
print(jarque.bera.test(na.omit(residuals(model_best))))
print(skewness(na.omit(residuals(model_best))))
print(kurtosis(na.omit(residuals(model_best))))

require(xlsx)
install.packages("xlsx")
mean(residuals(model_best),na.rm = T)
sd(residuals(model_best),na.rm = T)


## all the original plot

## get log diff
k=ts(K,frequency = 1)
## series plot
series=ts(datafinal[1],frequency = 1)
plot(series, xlab="Time daily", ylab="Index Return")
model=garchmodels[[1]]
model$order
res=model$residuals
plot(res,xlab="Index",ylab="Standard Residuals",type="l")
## all plot
for (i in 1:10) {
  series=ts(datafinal[i],frequency = 1)
  plot(series, xlab="Time daily", ylab="Index Return")
  model=garchmodels[[i]]
  model$order
  res=model$residuals
  plot(res,xlab="Index",ylab="Standard Residuals",type="l")
}
## sse
a=c()
for (i in 1:10) {
  model=garchmodels[[i]]
  res=model$residuals
  a[i]=sum(na.omit(res)^2)
}
## qqplot
for (i in 1:10) {
  model=garchmodels[[i]]
  res=model$residuals
  qqnorm(res)
  qqline(res)
  acf(res^2,na.action = na.omit)
}

