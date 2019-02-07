#Read data from the directory#
getwd()
data=read.table("UN.txt",header =T)

#Create a scatter plot#
Y=data$Fertility
X=data$PPgdp
plot(Y~X,main="Fertility vs PPgdp", ylab ="Fertility", xlab="PPgdp")

#Create diagnostic plots#
model=lm(Y~X)
par(mfrow=c(1,1))
plot(model)

#Find appopriate transfromation on Y#
library("MASS")
boxcox(model)
Ya=log(Y)
plot(Ya~X,main="Fertility vs PPgdp", ylab ="Fertility", xlab="PPgdp")
model1=lm(Ya~X)
plot(model1)

#Find apporiate transformation on X#
Xa=log(X)
plot(Ya~Xa,main=" Transformed Fertility vs PPgdp", ylab ="Fertility", xlab="PPgdp")
model2=lm(Ya~Xa)

plot(model2)

#Plain coding for least sqaure estimates for coefficients and R_square#
plot(Ya~Xa,main=" Transformed Fertility vs PPgdp", ylab ="Fertility", xlab="PPgdp")
n=length(Xa)
Xabar=mean(Xa)
Yabar=mean(Ya)
SXX=sum((Xa-Xabar)^2)
beta1hat=sum((Xa-Xabar)*(Ya-Yabar))/SXX
beta0hat=Yabar-Xabar*beta1hat
abline(beta0hat,beta1hat, col="red")
Yahat=beta0hat+beta1hat*Xa
SSR=sum((Yahat-Yabar)^2)
SSTO=sum((Ya-Yabar)^2)
R_square=SSR/SSTO

#Using lm() function#
plot(Ya~Xa,main=" Transformed Fertility vs PPgdp", ylab ="Fertility", xlab="PPgdp")
linear_model=lm(Ya~Xa)
linear_model$coefficients
Yahat=linear_model$fitted.values
lines(Xa,Yahat,col="red")
R_square=summary(linear_model)$r.square

#using matrix manipulation#
plot(Ya~Xa,main=" Transformed Fertility vs PPgdp", ylab ="Fertility", xlab="PPgdp")
n=length(Xa)
head(data)
Ya_matrix=matrix(c(Ya),nrow=n,ncol=1)
Xa_matrix=as.matrix(cbind(rep(1,n),matrix(c(Xa),nrow=n,ncol=1)))
betahat=solve(t(Xa_matrix)%*%(Xa_matrix))%*%t(Xa_matrix)%*%Ya_matrix
beta1hat=betahat[2,]
beta0hat=betahat[1,]
abline(beta0hat,beta1hat,col="red")
Yahat=Xa_matrix%*%betahat
Yabar=mean(Ya_matrix)
SSR=sum((Yahat-Yabar)^2)
SSTO=sum((Ya_matrix-Yabar)^2)
R_square=SSR/SSTO

#diagnostic plots#
par(mfrow=c(2,2))
plot(linear_model)

#Hypothesis testing#
summary(linear_model)

#99% confidence interval of expected Fertility of transformed data at PPgdp=log(20000)#
newdata=data.frame(Xa=log(20000))
predict(linear_model,newdata,interval = "confidence",level=0.99)

#95% confidence band#
plot(Y~X,main="Fertility vs PPgdp", ylab ="Fertility", xlab="PPgdp")
sigmahat=sqrt(sum(residuals(linear_model)^2)/df.residual(linear_model))
W=sqrt(2*qf(0.95,df1 = 2, df2 = n-2))
range=seq(from=min(X),to=max(X),length.out = 100)
upperbound=beta0hat+beta1hat*log(range)+W*sigmahat*sqrt(1/n+(log(range)-Xabar)^2/SXX)
lowerbound=beta0hat+beta1hat*log(range)-W*sigmahat*sqrt(1/n+(log(range)-Xabar)^2/SXX)
lines(exp(upperbound)~range,col="red")
lines(exp(lowerbound)~range,col="red")

#99% prediction interval of expected Fertility of transformed data at ppgdp= log(25000)#
newdata=data.frame(Xa=log(25000))
predict(linear_model,newdata,interval="prediction",leve=0.99)

