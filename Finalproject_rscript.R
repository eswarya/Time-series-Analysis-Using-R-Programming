
## Forecasting the stock price of Tesla And Volkswagen 
#The main objective of this research paper is to forecast the stock prices and predit for the 
upcoming 1 year by analyzing the previous stock values, and compare both the prices trend.

## STEP-1 
##I have used both the stock value which i downloaded from the yahoo finance website,
#and it is a weekly based datas, for the peried of 2015-2010 nd i will be focusing on the close price.
#Loading the appropriate data.

library(readr)
TSLA_8_ <- read_csv("C:/Users/Eswarya/Downloads/TSLA (8).csv")
View(TSLA_8_)
te=TSLA_8_
tes=te[,c(1,5)]
dim(tes)

library(readr)
VOW3_DE_3_ <- read_csv("C:/Users/Eswarya/Downloads/VOW3.DE (3).csv")
View(VOW3_DE_3_)
vw=VOW3_DE_3_
vws=vw[,5]
dim(vws)
df=cbind(tes,vws)
colnames(df) <-c('Date',"Tesla","VW")

#######################
Step-2
##Convert data to date and numeric function for further analysis
df$Date=as.Date(df$Date)
df$Tesla <- as.numeric(df$Tesla)
df$VW <- as.numeric(df$VW)
str(df)
################
Step-3
#Convert our data to time series
dfts <- ts(df[,2:3], start=c(2013,07), end=c(2020,07), frequency=12)
################
step-4
#Plotting both the stock value
library(tidyr)
library(dplyr)
datanew <- df %>%
  select(Date, Tesla, VW) %>%
  gather(key = "Stocks", value = "value", -Date)
head(datanew, 3)
ggplot(datanew, aes(x = Date, y = value,main='Stock Price of Tesla ANd VW(2013-2020)')) + 
  geom_line(aes(color = Stocks), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

################
Step-5
##Extract the 2 stock value and assign it to a avariable

tesla <-dfts[,1]
vw <-dfts[,2]

###################
Step-6
#plot the time series seperately for each stock
par(mfrow=c(1,2))
plot(tesla,xlab="Time",ylab='Tesla Close Price',main='Stock price of Tesla(2013-2020)')
plot(vw,xlab="Time", ylab='Volkswagen Close Price',main='Stock price of Volkswagen(2013-2020)')

####################
Step-7
#Decompose
tesla.de <-decompose(tesla)
vw.de <-decompose(vw)
par(mfrow=c(1,2))
plot(tesla.de)
plot(vw.de)
####################

Step-8
##Check whether our data is stationary or not
adf.test(tesla)
adf.test(vw)
par(mfrow=c(1,2)
acf(tesla,main='ACF of Tesla Stock')
pacf(tesla, main='PACF of Tesla Stock')
acf(vw,main='ACF of VW STock')
pacf(vw,main='PACF of VW Stock')
##Our values is non- stationary so we can remove that in next step by doing difference or log.
##############
Step-9
##To check whether our data is cointegration or not.
po.test(dfts)
##Ans- The value of p is greater than 0.05 so it is not cointegrated, 
#and we cannot use linear regression model

#######
Step-10
#In order to receive consistent, reliable results, the non-stationary data needs to 
#be transformed into stationary data
adf.test(teslalog)
teslog=c(NA,diff(log(tesla)))
vwstat=c(NA,diff(log(vw)))
teslastat=c(NA,diff(teslog))

teslavwts <- ts(cbind(teslastat,vwstat,tesla,vw),start=c(2013,07), end=c(2020,07), frequency=12)
teslavwnew <-na.omit(teslavwts)
tesla.ts <-teslavwnew[,1]
vw.ts <-teslavwnew[,2]
###We have converted our data to stationary

#########
Step-11
#Perform Augmented dickey fuller test and check whether there is a unit roots
adf.test(tesla.ts)
adf.test(vw.ts)
#Ans-The value of p is less than 0.05  for   both
#so it is stationary and it doesn't have unit roots.
###################

Step-12
##Linear Regression Model by taking Tesla as dependent and VW as Independent
teslavwts[,c(1,2)]
po.test(teslavwts[,c(1,2)])

#The value of p is less than 0.05 so it is  cointegrated, 
#and we can use linear regression model

dflinear <- lm(tesla.ts~ vw.ts)
summary(dflinear)
##Ans- Based on the findings of the linear regression model, 
#there is no  relationship between the tesla and VW stock, the alpha value is greater and
#there is no significant between both stocks.

######################
Step-13
###VECTOR AUTO REGRESSIVE MODEL
library(vars)
df.var <- VAR(cbind(tesla.ts,vw.ts), p = 3, type = "trend")
df.var.teslare <-residuals(df.var)[,1]
df.var.vwre <- residuals(df.var)[,2]

########
Step-14
#Check acf and pacf
par(mfrow=c(1,2))
acf(df.var.teslare)
pacf(df.var.teslare)

#Good fit as there is no autocoorelation

par(mfrow=c(1,2))
acf(df.var.vwre)
pacf(df.var.vwre)
#Not a good fit as there is autocorrelation.

###############
Step-15
#Predicting for next 1 year
VAR.pred <- predict(df.var,n.ahead = 12)
tesla.pred <- ts(VAR.pred$fcst$tesla.ts[, 1], start=c(2020,07),frequency = 12)
vw.pred <- ts(VAR.pred$fcst$vw.ts[, 1], start=c(2020,07),frequency = 12)

#ts.plot(cbind(tesla.ts, tesla.pred),lty = 1:2, col=c('red','blue'))
ts.plot(cbind(window(tesla.ts, start = 2016), tesla.pred),lty = 1:2, col=c('red','blue'),main='Forecasted stock price of Tesla')
ts.plot(cbind(window(vw.ts, start = 2016), vw.pred),lty = 1:2, col=c('red','blue'),main='Forecasted Stock price of VW')
#################
Step-16
#Using Arima model
#Split our data to train and test data
tesla.pre <-window(tesla.ts,start=c(2013,07),end=c(2019,07))
tesla.post <- window(tesla.ts,start=c(2019,07))
vw.pre <- window(vw.ts,start=c(2013,07),end=c(2019,07))
vw.post <- window(vw.ts,start=c(2019,07))

#######
Step-17
##Check to find the order for ARIMA(p,q,d)
par(mfrow=c(1,2))
acf(tesla.pre,main='ACF of Tesla stocks to get order')
pacf(tesla.pre,main='PACF of Tesla stocks to get order')
#From the graph we can see the acf value()is 1 and the pacf value(p)is also 0.
acf(vw.pre,main='ACF of VW stocks to get order')
pacf(vw.pre,main='PACF of VW stocks to get order')


###########


#####
# FOR TESLA
modelfit <- auto.arima(tesla.pre, allowdrift=F,lambda = "auto")
best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:2) for(d in 0:1) for (j in 0:2) {
  fit.aic <- AIC(arima(tesla.pre, order = c(i, d,
                                                 j)))
  if (fit.aic < best.aic) {
    best.order <- c(i, d, j)
    best.arima <- arima(tesla.pre, order = best.order)
    best.aic <- fit.aic
  }}
best.arima   
best.order
pred <- predict(best.arima, n.ahead = 12)
    ts.plot(tesla.pre, tesla.post, lty = 1:2,col = c("red","blue"))
    Box.test(resid(best.arima), lag=20, type="Ljung-Box")
    mean(abs((tesla.post-pred$pred)/tesla.post) * 100)
   ## Not a good fit
    

#######
For Vw
    modelfitvw <- auto.arima(vw.pre, allowdrift=F,lambda = "auto")
    best.order1 <- c(0, 0, 0)
    best.aic1 <- Inf
    for (i in 0:2) for(d in 0:1) for (j in 0:2) {
      fit.aic1 <- AIC(arima(vw.pre, order = c(i, d,
                                                j)))
      if (fit.aic1 < best.aic1) {
        best.order1 <- c(i, d, j)
        best.arima1 <- arima(vw.pre, order = best.order1)
        best.aic1 <- fit.aic1
      }}
    best.arima1   
    best.order1
    pred1 <- predict(best.arima1, n.ahead = 12)
    acf(best.arima1$residuals)
    pacf(best.arima1$residuals)
    Box.test(resid(best.arima1), lag=20, type="Ljung-Box")
    mean(abs((vw.post-pred$pred)/vw.post) * 100)
    ts.plot(vw.pre, pred1$pred, lty = 1:2,col = c("red","blue"))
    ##Not a good fit

