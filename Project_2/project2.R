data=read.table("diabetes.txt",header=T)
attach(data)

#problem 1#
str(data)
name=names(data)
#histogram#
par(mfrow=c(3,5))
par(mfrow=c(1,1))
for (i in seq_along(data)){
  if (i==6||i==8||i==11){
    next
  }
  variable=as.name(name[i])
  hist(eval(variable),xlab=name[i],main=paste("Histogram of ", name[i], sep = ""))
}
#pie chart#
table(location)
count=c(175,191)
pct <- round(count/sum(count)*100)
lbls=c("Buckingham ","Louisa ")
lbls=paste(lbls,pct,"%",sep="")
pie(count,labels=lbls,main="Location",col=rainbow(length(lbls)))
table(gender)
count=c(214,152)
pct <- round(count/sum(count)*100)
lbls=c("female ","male ")
lbls=paste(lbls,pct,"%",sep="")
pie(count,labels=lbls,main="Gender",col=rainbow(length(lbls)))
table(frame)
count=c(96,172,98)
pct <- round(count/sum(count)*100)
pct
lbls=c("large ","medium ","small ")
lbls=paste(lbls,pct,"%",sep="")
pie(count,labels=lbls,main="Frame",col=rainbow(length(lbls)))
#scatterplot matrix and pairwise correlation matrix#
pairs(data[-c(6,8,11)])

#problem 2#
model1=lm(glyhb~.,data)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

#problem 3#
boxcox(model1)
data$glyhb=glyhb^-1
model2=lm(glyhb~.,data)
summary(model2)
plot(model2)
boxcox(model2)

#problem 4#
set.seed(10)
N=nrow(data)
index=sample(1:N, size=N/2, replace=FALSE)
data.t=data[index,]
data.v=data[-index,]

#problem5#
model3=lm(glyhb~.,data.t)
summary(model3)
length(model3$coefficients)
MSE=sum((data.t$glyhb-model3$fitted.values)^2)/166
MSE
#problem 6#
library(leaps)
library(MASS)
best=regsubsets(glyhb~., data=data.t, nbest=1, nvmax=16)
sum_sub=summary(best)
sum_sub$which
n=nrow(data.t)
p.m=2:17
sse=sum_sub$rss
sse
aic=n*log(sse)+2*p.m-n*log(n)
bic=n*log(sse)+log(n)*p.m-n*log(n)
fit0=lm(glyhb~1,data=data.t)
sse1=sum(fit0$residuals^2)
p=1
c1=sse1/0.001384-(n-2*p)
aic1=n*log(sse1)+2*p-n*log(n)
bic1=n*log(sse1)+log(n)*p-n*log(n)
none=c(1,rep(0,16),sse1,0,0,c1,aic1,bic1)
res_sub=cbind(sum_sub$which,sse,sum_sub$rsq,sum_sub$adjr2,sum_sub$cp,aic, bic)
res_sub=rbind(none,res_sub)
colnames(res_sub)=c(colnames(sum_sub$which),"sse", "R^2", "R^2_a", "Cp", "aic", "bic")
res_sub
frametype=model.matrix(~data.t$frame-1)
frametype
framesmall=frametype[,3]
framesmall
model3.1=lm(glyhb~stab.glu+age+waist+ratio+framesmall,data.t)
model3.2=lm(glyhb~stab.glu+age+waist,data.t)
model3.3=lm(glyhb~stab.glu+age+waist+ratio+framesmall+time.ppn,data.t)

#problem 7#
model4=lm(glyhb~.^2,data.t)
summary(model4)
length(model4$coefficients)
MSE=sum((data.t$glyhb-model4$fitted.values)^2)/47
#problem8#
library(leaps)
library(MASS)
model.fs1=stepAIC(fit0, scope=list(upper=lm(glyhb~.^2,data=data.t), lower=~1), direction="both", k=2)
#problem9#
model.fs2=stepAIC(model3, scope=list(upper=lm(glyhb~.^2,data=data.t), lower=~1), direction="both", k=2)
#problem10#
sse.fs1=sum(model.fs1$residuals^2)
sse.fs2=sum(model.fs2$residuals^2)
bic=n*log(sse.fs1)+log(n)*length(model.fs1$coefficients)-n*log(n)
bic=n*log(sse.fs2)+log(n)*length(model.fs2$coefficients)-n*log(n)
bic
model4.1=model.fs2
model4.2=model.fs1
#problem11#
press.3.1=sum((model3.1$residuals/(1-lm.influence(model3.1)$hat))^2)
press.3.2=sum((model3.2$residuals/(1-lm.influence(model3.2)$hat))^2)
press.3.3=sum((model3.3$residuals/(1-lm.influence(model3.3)$hat))^2)
press.4.1=sum((model4.1$residuals/(1-lm.influence(model4.1)$hat))^2)
press.4.2=sum((model4.2$residuals/(1-lm.influence(model4.2)$hat))^2)
sum(model3.1$residuals^2)
sum(model3.2$residuals^2)
sum(model3.3$residuals^2)
sum(model4.1$residuals^2)
sum(model4.2$residuals^2)
#problem12#
MSPR3.1=sum((data.v$glyhb-predict(model3.1,data.v))^2)/n
MSPR3.2=sum((data.v$glyhb-predict(model3.2,data.v))^2)/n
MSPR3.3=sum((data.v$glyhb-predict(model3.3,data.v))^2)/n
MSPR4.1=sum((data.v$glyhb-predict(model4.1,data.v))^2)/n
MSPR4.2=sum((data.v$glyhb-predict(model4.2,data.v))^2)/n
press.3.1/n
press.3.2/n
press.3.3/n
press.4.1/n
press.4.2/n
#problem13#
frametype=model.matrix(~data$frame-1)
framesmall=frametype[,3]
finalmodel=lm(glyhb~stab.glu+age+waist+ratio+framesmall+time.ppn,data)
summary(finalmodel)
anova(finalmodel) 

