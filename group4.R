#matrix to get data
install.packages("PerformanceAnalytics")
install.packages("pdfetch")
install.packages('moments')
install.packages("corrplot")
install.packages("car")
install.packages("lmtest")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("MASS")
install.packages("fitdist")
install.packages("EnvStats")
install.packages("fastcluster")
install.packages("fitdistrplus")
library(EnvStats)
library(pdfetch)
library(moments)
library(MASS)
library(car)
library(lmtest)
library(psych)
library(corrplot)
library(PerformanceAnalytics)
library(tidyverse)
library(glue)
library(dplyr)
library(lubridate)
library(readxl)

# Index Data & Log Return
ticker = c("^GSPC")
index   = pdfetch_YAHOO(ticker, fields = "adjclose", from="2014-01-01", to="2019-01-01", interval= "1mo")
dj = read_xls("Dow Jones Technology.xls")
dj1=dj[1637:2936,3]
logdj=dj1[1:260,1]
sum=1
for(val in 1:260) {
  logdj[val,1]=100*log(dj1[sum+4,1]/dj1[sum,1]) 
  
  sum=sum+5
}
logsp   = na.omit(diff(log(index[,1]))*100)
View(logsp)
allindex= data.frame(logsp,logdj,index[-1,2])
names(allindex) = c("SP500", "Dow Jones Tech","RF")

# ETFs Data
tickers = c("JALRX","QQQ","PNQI","ROBO","IGM")
etf = pdfetch_YAHOO(tickers,fields = "adjclose",
                    from="2014-01-01", to="2019-01-01", interval= "1w")
View (etf)

#log return etfs
logrtn = na.omit(diff(log(etf))*100)
View(logrtn)


# A. Central tendency, dispersion and shape of distributions
#Method 1
etfsts <- function (r){
  mean<-mean(r)
  median<-median(r)
  quantile1<-unname(quantile(r,.25))
  quantile3<-unname(quantile(r,.75))
  std<-sd(r)
  skewness<-unname(skewness(r))
  kurtosis<-unname(kurtosis(r))
  return(c(Mean = mean, Median = median, FstQuantile = quantile1, ThrdQuantile = quantile3,
           STD = std, Skewness = skewness, Kurtosis = kurtosis))
}

Report<- sapply(logrtn,etfsts)
Report

chart.Correlation(logrtn,method="pearson")

i=1
while(i<3)
{
  hist(logrtn[,i],prob = TRUE,breaks = 20,col = "lightblue", 
       border = "pink", main = paste(i),
       xlab = "Log Return", ylab = "Density")

  fit1.t <- fitdistr(logrtn[,i], "t")
  fit1.norm<-fitdistr(logrtn[,i],"normal")
  curve(dnorm(x,mean=fit1.norm$estimate[1],sd =fit1.norm$estimate[2]),col="red",add = TRUE,lwd=1)
  curve(dnorm(x,mean=fit1.t$estimate[1],sd =fit1.t$estimate[2]),col="blue",add = TRUE,lwd=2)
}

hist(logrtn[,1],prob = TRUE,breaks = 20,col = "lightblue", 
     border = "pink", main = paste("XLK Distribution"),
     xlab = "Log Return", ylab = "Density")
fit1.t <- fitdistr(logrtn[,1], "t")
fit1.norm<-fitdistr(logrtn[,1],"normal")
curve(dnorm(x,mean=fit1.norm$estimate[1],sd =fit1.norm$estimate[2]),col="red",add = TRUE,lwd=1)
curve(dnorm(x,mean=fit1.t$estimate[1],sd =fit1.t$estimate[2]),col="blue",add = TRUE,lwd=2)
fit1.norm$loglik
fit1.t$loglik
cat("The log likelihood value of Student t distribution is 
greater compared normal distribution. Hence student t describes the fund better")

hist(logrtn[,2],prob = TRUE,breaks = 20,col = "lightblue", 
     border = "pink", main = paste("QQQ Distribution"),
     xlab = "Log Return", ylab = "Density")
fit2.t <- fitdistr(logrtn[,2], "t")
fit2.norm<-fitdistr(logrtn[,2],"normal")
curve(dnorm(x,mean=fit2.norm$estimate[1],sd =fit2.norm$estimate[2]),col="red",add = TRUE,lwd=1)
curve(dnorm(x,mean=fit2.t$estimate[1],sd =fit2.t$estimate[2]),col="blue",add = TRUE,lwd=2)
fit2.norm$loglik
fit2.t$loglik
cat("The log likelihood value of Student t distribution is 
greater compared normal distribution. Hence student t describes the fund better")

hist(logrtn[,3],prob = TRUE,breaks = 20,col = "lightblue", 
     border = "pink", main = paste("PNG Distribution"),
     xlab = "Log Return", ylab = "Density")
fit3.t <- fitdistr(logrtn[,3], "t")
fit3.norm<-fitdistr(logrtn[,3],"normal")
curve(dnorm(x,mean=fit3.norm$estimate[1],sd =fit3.norm$estimate[2]),col="red",add = TRUE,lwd=1)
curve(dnorm(x,mean=fit3.t$estimate[1],sd =fit3.t$estimate[2]),col="blue",add = TRUE,lwd=2)
fit3.norm$loglik
fit3.t$loglik
cat("The log likelihood value of Student t distribution is 
greater compared normal distribution. Hence student t describes the fund better")

hist(logrtn[,4],prob = TRUE,breaks = 20,col = "lightblue", 
     border = "pink", main = paste("ROBO Distribution"),
     xlab = "Log Return", ylab = "Density")
fit4.t <- fitdistr(logrtn[,4], "t")
fit4.norm<-fitdistr(logrtn[,4],"normal")
curve(dnorm(x,mean=fit4.norm$estimate[1],sd =fit4.norm$estimate[2]),col="red",add = TRUE,lwd=1)
curve(dnorm(x,mean=fit4.t$estimate[1],sd =fit4.t$estimate[2]),col="blue",add = TRUE,lwd=2)
fit4.norm$loglik
fit4.t$loglik
cat("The log likelihood value of Student t distribution is 
greater compared normal distribution. Hence student t describes the fund better")

hist(logrtn[,5],prob = TRUE,breaks = 20,col = "lightblue", 
     border = "pink", main = paste("IGM Distribution"),
     xlab = "Log Return", ylab = "Density")
fit5.t <- fitdistr(logrtn[,5], "t")
fit5.norm<-fitdistr(logrtn[,5],"normal")
curve(dnorm(x,mean=fit5.norm$estimate[1],sd =fit5.norm$estimate[2]),col="red",add = TRUE,lwd=1)
curve(dnorm(x,mean=fit5.t$estimate[1],sd =fit5.t$estimate[2]),col="blue",add = TRUE,lwd=2)
fit5.norm$loglik
fit5.t$loglik
cat("The log likelihood value of Student t distribution is 
greater compared normal distribution. Hence student t describes the fund better")

# B. Var/CoVar Corr Matrix for all ETFs. Compare risks and performance with index
#Correlation - Covariance Matrix opt 1 by data.frame
funds  <- data.frame(logrtn,logsp,logdj)
etfs   <- c("XLK","QQQ","PNQI","ROBO","IGM","S&P 500","Dow Jones")
names(funds) = etfs
covmatrix    = matrix(c(cov(funds)), nrow = 7, ncol = 7)
dimnames(covmatrix)     = list(etfs,etfs)
covmatrix
cor(funds)
cormat  <- cor(funds)
corrplot(cormat)


#C. Tracking error, hypothesis test for mean and variance of returns
allindex2 = xts(allindex,order.by =as.Date(rownames(allindex)))
"Tracking Error between XLK compare to SP500"
TrackingError(logrtn[,1],allindex2[,1])

"Tracking Error between QQQ compare to SP500"
TrackingError(logrtn[,2],allindex2[,1])

"Tracking Error between PNQI compare to SP500"
TrackingError(logrtn[,3],allindex2[,1])

"Tracking Error between ROBO compare to SP500"
TrackingError(logrtn[,4],allindex2[,1])

"Tracking Error between IGM compare to SP500"
TrackingError(logrtn[,5],allindex2[,1])

"Tracking Error between XLK compare to DowJones"
TrackingError(logrtn[,1],allindex2[,2])

"Tracking Error between QQQ compare to DowJones"
TrackingError(logrtn[,2],allindex2[,2])

"Tracking Error between PNQI compare to DowJones"
TrackingError(logrtn[,3],allindex2[,2])

"Tracking Error between ROBO compare to DowJones"
TrackingError(logrtn[,4],allindex2[,2])

"Tracking Error between IGM compare to DowJones"
TrackingError(logrtn[,5],allindex2[,2])

#Hypothesis
XLK <- array(logrtn[,1])
QQQ <- array(logrtn[,2])
PNQI <- array(logrtn[,3])
ROBO <- array(logrtn[,4])
IGM <- array(logrtn[,5])
GSPC <- array(allindex[,1])
DJT <- array(allindex[,2])

#hypothesis testing for mean & variance of returns between XLK, GSPC & XLK,DJT
var.test(XLK,GSPC)
var.test(XLK,DJT)
t.test(XLK,GSPC,var.test=FALSE)
t.test(XLK,DJT,var.test= FALSE)

#hypothesis testing for mean & variance of returns between QQQ, GSPC & QQQ,DJT
var.test(QQQ,GSPC)
var.test(QQQ,DJT)
t.test(QQQ,GSPC,var.test=FALSE)
t.test(QQQ,DJT,var.test= FALSE)

#hypothesis testing for mean & variance of returns between PNQI, GSPC & PNQI,DJT
var.test(PNQI,GSPC)
var.test(PNQI,DJT)
t.test(PNQI,GSPC,var.test=FALSE)
t.test(PNQI,DJT,var.test= FALSE)

#hypothesis testing for mean & variance of returns between ROBO, GSPC & ROBO,DJT
var.test(ROBO,GSPC)
var.test(ROBO,DJT)
t.test(ROBO,GSPC,var.test=FALSE)
t.test(ROBO,DJT,var.test= FALSE)

#hypothesis testing for mean & variance of returns between IGM, GSPC & IGM,DJT
var.test(IGM,GSPC)
var.test(IGM,DJT)
t.test(IGM,GSPC,var.test=FALSE)
t.test(IGM,DJT,var.test= FALSE)

#D. Regression
options(scipen=100) #remove e
rf = allindex[,3]/52


#excess return for etfs
excessxlk  = logrtn[,1] - rf
excessqqq  = logrtn[,2] - rf
excesspnqi = logrtn[,3] - rf
excessrobo = logrtn[,4] - rf
excessigm  = logrtn[,5] - rf

#excess return for index
excesssp   = allindex[,1]- rf
excessdj   = allindex[,2]- rf

excess  = cbind(excessxlk,excessqqq,excesspnqi,excessrobo,excessigm,excesssp,excessdj)
names(excess)=c("XLK","QQQ","PNQI","ROBO","IGM","SP","DowJonesTech")

#regression XLK SP500
xlksp <- lm(XLK~SP, data = excess)
summary(xlksp)
plot(y= excessxlk,x= excesssp,pch = 10, cex = 0.5, col = "red",
     ylab = "XLK",xlab = "SP500", main = "Linear Regression XLK~SP500")
abline(lm(xlksp))


#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(xlksp)
#Heteroskadasticity Test XLKSP
bptest(xlksp)

#regression QQQ SP500
qqqsp  <- lm(QQQ~SP, data = excess)
summary(qqqsp)
plot(y= excessqqq,x= excesssp,pch = 10, cex = 0.5, col = "blue",
     ylab = "QQQ",xlab = "SP500", main = "Linear Regression QQQ~SP500")
abline(lm(qqqsp))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(qqqsp)
#Heteroskadasticity Test QQQSP
bptest(qqqsp)

#regression PNQI SP500
pnqisp <- lm(PNQI~SP, data = excess)
summary(pnqisp)
plot(y= excesspnqi,x= excesssp,pch = 10, cex = 0.5, col = "green",
     ylab = "PNQI", xlab = "SP500", main = "Linear Regression PNQI~SP500")
abline(lm(pnqisp))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(pnqisp)
#Heteroskadasticity Test PNQISP
bptest(pnqisp)

#regression ROBO SP500
robosp <- lm(ROBO~SP, data = excess)
summary(robosp)
plot(y= excessrobo,x= excesssp,pch = 10, cex = 0.5, col = "purple",
     ylab = "ROBO",xlab = "SP500", main = "Linear Regression ROBO~SP500")
abline(lm(robosp))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(robosp)
#Heteroskadasticity Test ROBOSP
bptest(robosp)

#regression IGM SP500
igmsp  <- lm(IGM~SP, data = excess)
summary(igmsp)
plot(y= excessigm,x= excesssp,pch = 10, cex = 0.5, col = "orange",
     ylab = "IGM",xlab = "SP500", main = "Linear Regression IGM~SP500")
abline(lm(igmsp))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(igmsp)
#Heteroskadasticity Test IGMSP
bptest(igmsp)

#E

fama      = read.csv("F-F_Research_Data_Factors_weekly.CSV",header=TRUE)
famasmall = fama[4566:4826,]
mergedata <- data.frame(famasmall[-1,],excess)
View(mergedata)


xlkfama   <-lm(excessxlk ~ Mkt.RF + SMB + HML, data = mergedata) 
summary(xlkfama)  #FF 3 Factors XLK

qqqfama   <-lm(excessqqq ~ Mkt.RF + SMB + HML, data = mergedata) 
summary(qqqfama)  #FF 3 Factors QQQ

pnqifama   <-lm(excesspnqi ~ Mkt.RF + SMB + HML, data = mergedata) 
summary(pnqifama)  #FF 3 Factors PNQI

robofama   <-lm(excessrobo ~ Mkt.RF + SMB + HML, data = mergedata) 
summary(robofama)  #FF 3 Factors ROBO

igmfama   <-lm(excessigm ~ Mkt.RF + SMB + HML, data = mergedata) 
summary(igmfama)  #FF 3 Factors IGM

#F. 
#anova for regression
anova(xlksp)

anova(qqqsp)

anova(pnqisp)

anova(robosp)

anova(igmsp)

#anova for fama
anova(xlkfama)

anova(qqqfama)

anova(pnqifama)

anova(robofama)

anova(igmfama)