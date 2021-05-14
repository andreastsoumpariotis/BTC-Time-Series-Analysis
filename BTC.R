
### Project ###

#Load packages
library(tswge)
library(zoo)
library(xts)
library(lubridate)
library(dplyr)
library(tidyr)
library(WDI)
library(keras)
library(anomalize)
library(devtools)
library(signal)
library(MASS)
library(dplyr)
library(tidyverse)


#Read in monthly data (August 2010 - May 2021)
btc.monthly = read.csv("BTC-USD.csv")
View(btc.monthly)
str(btc.monthly)


# Methods #

#Plot monthly data
realization = plotts.wge(ts((btc.monthly$Open),start = c(btc.monthly$Date == "2014-10-01"),frequency = 1))

#Realizations, Sample Autocorrelations, and Parzen Spectral Densities
plotts.sample.wge(btc.monthly$Open)

#Examine factor table
est.ar.wge(btc.monthly$Open,p=15,type='burg')

#Cochrane-Orcutt Tests for trends
co.wge(btc.monthly$Open) #p-value=2.197806e-05
#Woodward-Bottone-Gray test for trend
wbg.boot.wge(btc.monthly$Open) #p-value=0.01754386


# Modeling Results #

#Model 1

#Identify top candidate models for data
aic5.wge(btc.monthly$Open,p=0:12,q=0:4,type='aic') #ARMA(9,4), ARMA(3,2), ARMA(6,3)

#Factor table for top models
model1 = est.arma.wge(btc.monthly$Open,p=9,q=4,factor=TRUE) #ARMA(9,4)
model2 = est.arma.wge(btc.monthly$Open,p=3,q=2,factor=TRUE) #ARMA(3,2)
model3 = est.arma.wge(btc.monthly$Open,p=6,q=3,factor=TRUE) #ARMA(6,3)
#Chosen = ARMA(3,2)

#Check 'whiteness' of residuals
model2$res
#Plot residuals
plotts.sample.wge(model2$res,lag.max=48,arlimits=TRUE)

#Ljung-Box test results for K=24 and K=48
ljung.wge(model2$res,p=3,q=2) #K=24 is the default
ljung.wge(model2$res,p=3,q=2,K=48)
#K=24: p-value is 0.4987081
#K=48:p-value is 0.6587886

#Forecasting performance
model2.forecast=fore.arma.wge(btc.monthly$Open,phi=model2$phi,theta=model2$theta,n.ahead=10,lastn=TRUE,limits=FALSE)
model2.forecast$f
#RMSE
sqrt(mean((model2.forecast$se)^2)) #10010.86
#MAD
mean(abs(model2.forecast$se)) #9133.65
#Model doesn't perform too well!


#Model 2

#Fit an ARIMA model with (1-B^12) roots
d_btc1=artrans.wge(btc.monthly$Open,phi.tr=c(rep(0,12)),plot=TRUE)
d_btc2=artrans.wge(d_btc1,phi.tr=1)
aic.wge(d_btc2,p=0:10,q=0:2)
btc.model = est.arma.wge(d_btc2,p=3,q=0)
#phi = 0.27586863, 0.07772535, 0.38366067
#var = 6021575


#Check 'whiteness' of residuals
btc.model$res
#Plot of residuals
plotts.sample.wge(btc.model$res,lag.max=48,arlimits=TRUE)


#Ljung-Box test results for K=24 and K=48
ljung.wge(btc.model$res,p=3,q=0) #K=24 is the default
ljung.wge(btc.model$res,p=3,q=0,K=48)
#K=24: p-value is 0.9459546
#K=48:p-value is 0.9689315


#Forecasting performance
btc.model.forecast=fore.arma.wge(btc.monthly$Open,phi=btc.model$phi,theta=btc.model$theta,n.ahead=10,lastn=TRUE,limits=FALSE)
btc.model.forecast$f
#RMSE
sqrt(mean((btc.model.forecast$se)^2)) #7132.217
#MAD
mean(abs(btc.model.forecast$se)) #7115.181


