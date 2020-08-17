install.packages("cluster")
install.packages("NbClust")
install.packages("flexclust")
install.packages("fMultivar")
install.packages("rattle")
install.packages("class")
install.packages("kknn")

# divided into several columns for each variable.


data=read.csv(file = "flight_delays_train.csv",head=T)
data$Month=factor(data$Month)
data$DayofMonth=factor(data$DayofMonth)
data$DayOfWeek=factor(data$DayOfWeek)
data$Origin=factor(data$Origin)
data$Dest=factor(data$Dest)
data$dep_delayed_15min=factor(data$dep_delayed_15min)



# choose a little set, which need to be changed in the final verison.
data=data[1:100,]

attach(data)

## devide test set and train set

set.seed(0)
train.num = sample(nrow(data),0.7*nrow(data))
train_data=data[train.num,]
test_data=data[-train.num,]

## in order to better calculate the distance 

D.sd=sd(train_data$Distance)
D.mean=mean(train_data$Distance)
train_data$Distance=scale(train_data$Distance)

test_data$Distance=(test_data$Distance-D.mean)/D.sd

dep_time.sd=sd(train_data$DepTime)
dep_time.mean=mean(train_data$DepTime)
train_data$DepTime=scale(train_data$DepTime)

test_data$DepTime=(test_data$DepTime-dep_time.mean)/dep_time.sd



### Using SVD method to calculate PCA 1 & PCA 2
continu=cbind(train_data$DepTime,train_data$Distance)
x=prcomp(continu)$rotation
train_data$PCA1=continu%*%x[,1]
train_data$PCA2=continu%*%x[,2]
continu1=cbind(test_data$DepTime,test_data$Distance)
test_data$PCA1=continu1%*%x[,1]
test_data$PCA2=continu1%*%x[,2]





## find the best parameters in the model.
### 2-fold cross validation
library("class")
library("kknn")

set.seed(0)
train.num1 = sample(nrow(train_data),0.5*nrow(train_data))
train_data01=train_data[train.num1,]
train_data02=train_data[-train.num1,]
use_data=rbind(train_data01,train_data02)[,c(9,10,11)]
jiaquan_train<-train.kknn(formula = dep_delayed_15min~., data = use_data,
                          kernel = c("rectangular", "triangular",
                                     "epanechnikov","gaussian", "rank", "optimal"),
                          distance = 2
)

plot(jiaquan_train)

##### parameter: k=10, weighting method is "rectangular"

### feature selection which is by giving codes in GitHub during the class.

#### calculate the accuracy of differrent factors

##### cost a long time and without less interpretion.



## use R function and package class

train_data1=train_data[,c(9,10,11)]
test_data1=test_data[,c(9,10,11)]
test_label=test_data[,9]

prc_test_pred <- kknn(formula = dep_delayed_15min~.,train=train_data1,test=test_data1,
                      k=10,scale=F,distance=2,kernel= "rectangular")



### second weighting which means balance, repeat the Y data in train data.
weighting_result=function(k){
  prob=prc_test_pred$prob
  p=k*prob[,2]/(prob[,1]+k*prob[,2])
  q=c(1:dim(prob)[1])
  q[p>=0.5]="Y"
  q[p<0.5]="N"
  return(q)
}
weighting_precision=function(x){
  q=weighting_result(x)
  p=sum(q==test_label)/length(test_label)
  m=sum(q=="Y"&test_label=="Y")/sum(test_label=="Y")
  return(p*m)
}
M=1:1000
X=1:1000
for (i in 1:1000) {
  X[i]=1+(i/100)
  M[i]=weighting_precision(1+(i/100))
}
plot(X,M,xlab = "voice",ylab = "accuracy*recall")



## calculating several target in the project.

prc_test=weighting_result(1)

acc=sum(prc_test==test_label)/length(test_label)

recall=sum(prc_test=="Y"&test_label=="Y")/sum(test_label=="Y")



## logistic
rm(list=ls())
data=read.csv(file = "flight_delays_train.csv",head=T)
data$Month=factor(data$Month)
data$DayofMonth=factor(data$DayofMonth)
data$DayOfWeek=factor(data$DayOfWeek)
data$Origin=factor(data$Origin)
data$Dest=factor(data$Dest)
data$dep_delayed_15min=factor(data$dep_delayed_15min)
data[1,]
data=data[,c(4,8,6,7,9)]
Dest1=c()
Origin1=c()


c=table(data$Dest)
m=quantile(c,0.6)
n=quantile(c,0.9)
for (i in 1:100000) {
  a=data$Dest[i]
  b=c[which(names(c)==a)]
  b1=as.numeric(b)
  if (isTRUE(b1>n)) {Dest1[i]=factor("Dest_b",
                                         levels = c("Dest_b","Dest_m","Dest_s"))}
  if (isTRUE(b1>m & b1<=n)) {Dest1[i]=factor("Dest_m",
                                             levels = c("Dest_b","Dest_m","Dest_s"))}
  if (isTRUE(b1<=m)) {Dest1[i]=factor("Dest_s",
                                      levels = c("Dest_b","Dest_m","Dest_s"))}
}
data$Dest1=factor(Dest1)

c=table(data$Origin)
m=quantile(c,0.6)
n=quantile(c,0.9)
for (i in 1:100000) {
  a=data$Origin[i]
  b=c[which(names(c)==a)]
  b1=as.numeric(b)
  if (isTRUE(b1>n)) {Origin1[i]=factor("Origin_b",
                                     levels = c("Origin_b","Origin_m","Origin_s"))}
  if (isTRUE(b1>m & b1<=n)) {Origin1[i]=factor("Origin_m",
                                             levels = c("Origin_b","Origin_m","Origin_s"))}
  if (isTRUE(b1<=m)) {Origin1[i]=factor("Origin_s",
                                      levels = c("Origin_b","Origin_m","Origin_s"))}
}
data$Origin1=factor(Origin1)
data[1,]
data=data[,c(1,2,6,7,5)]


set.seed(0)
train.num = sample(nrow(data),0.7*nrow(data))
train_data=data[train.num,]
test_data=data[-train.num,]
Dest1=train_data$Dest1
Origin1=train_data$Origin1

attach(train_data)

log.model = glm(dep_delayed_15min~DepTime+Distance+Dest1+Origin1, family = binomial(link = logit))
summary(log.model)

sum(fitted(log.model)>0.2)

