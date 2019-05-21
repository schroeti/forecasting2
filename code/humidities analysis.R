## HUMIDITIES ANALYSIS: 

library(tibble)
library(tseries)


#---------------------------------------------------------------------------#

#1. Humedad comedor sensor


#HOLT-WINTER
hw<-hw(trhumcomsens, h=24)

checkresiduals(hw$residuals)

AIC(hw$residuals)

#ETS

ets<-ets(trhumcomsens, damped = TRUE)

checkresiduals(ets$residuals)

#ARIMA 

humidity1arima<-auto.arima(humcomsens, 
                           stepwise = FALSE,
                           lambda="auto", 
                           biasadj=TRUE,
                           allowdrift=TRUE)

checkresiduals(humidity1arima)

ggPacf(humidity1arima$residuals)

AIC(humidity1arima)
### THIS IS BEST (2,1,1)(1,0,1)[24]

plot(forecast(humidity1arima, h=24))

autoplot(humidity1arima)+ggtitle("unitroot test of ARIMA(2,1,1)(1,0,1)[24]")
###unitroot ok

gglagplot(humidity1arima$residuals,do.lines=FALSE)+
  ggtitle("lagplots for humedad comedor sensor ")

adf.test(humidity1arima$residuals)
kpss.test(humidity1arima$residuals)
#ADF and KPSS ok

### function for cross validation
far2 <- function(x, h){forecast(auto.arima(x,
                     step = F,
                     lambda="auto",
                     biasadj=T,
                     allowdrift=T),
          h=h)}

### graphic forecasts
forecast(humidity1arima, 
         h=24)%>%
  autoplot()+
  xlab("days")+
  ylab("humedad comedor sensor")

forecast(humidity1arima, 
         h=1)%>%
  autoplot()+
  xlab("days")+
  ylab("humedad comedor sensor")

forecast(humidity1arima,
         h=120)%>% 
  autoplot()+
  xlab("days")+
  ylab("humedad comedor sensor")

library(tibble)

## tables of forecasts
humidity1forecast24h<-forecast(humidity1arima, 
                               h=24)%>%
  as.data.frame()%>%
  as_tibble()

View(humidity1forecast24h)

humidity1forecast120h<-forecast(humidity1arima,
                                h=120)%>%
  as.data.frame()%>%
  as.tibble()

View(humidity1forecast120)


humidity1forecast1h<-forecast(humidity1arima,
                              h=1)%>%
  as.data.frame()%>%
  as.tibble()

View(humidity1forecast1h)

### errors with cross validation
e <- tsCV(humcomsens, far2, h=1)
autoplot(e)

sqrt(mean(e^2, na.rm=TRUE))

f <- tsCV(humcomsens, far2, h=120)
sqrt(mean(f^2, na.rm=TRUE))

g<-tsCV(humcomsens, far2, h=24)
sqrt(mean(g^2, na.rm=TRUE))



#---------------------------------------------------------------------------#

#2. Humedad exterior sensor


## HOLT-WINTER
hw<-hw(trhumextsens, h=24)
checkresiduals(hw)
hw%>%summary()

##ETS 
ets<-ets(trhumextsens, damped=TRUE)
checkresiduals(ets)
ets%>%summary()

##ARIMA
humidity2arima<-auto.arima(humextsens, 
                           stepwise = FALSE, 
                           lambda="auto", 
                           biasadj=TRUE, 
                           allowdrift=TRUE)

plot(forecast(humidity2arima,
              h=120))

summary(humidity2arima)

checkresiduals(humidity2arima)
###humidity2arima not ok 

model1<-auto.arima(humextsens, 
                   lambda="auto")

checkresiduals(model1)

AIC(model1)

model1%>%autoplot()+
  ggtitle("Unit root test of model1")
##Unit root test ok

ggPacf(model1$residuals)
##pacf ok

gglagplot(model1$residuals,do.lines=FALSE)+
  ggtitle("lagplots for humedad exterior sensor model1")

adf.test(model1$residuals)
kpss.test(model1$residuals)
## ADF and KPSS tests ok 

#graphical forecasts
model1%>%forecast(h=120)%>%
  autoplot()+
  xlab("time")+
  ylab("relative humidity")


model1%>%
  forecast(h=1)%>%
  as.tibble()%>%
  View()

model1%>%
  forecast(h=24)%>%
  as.tibble()%>%
  View()

### function for cross validation 
far3 <- function(x, h){forecast(auto.arima(x,
                                           lambda="auto"),
                                h=h)}

### evaluation with CV 
e1<- tsCV(humextsens, far3, h=1)
sqrt(mean(e1^2, na.rm=TRUE))

f1 <- tsCV(humextsens, far3, h=5)
sqrt(mean(f1^2, na.rm=TRUE))

g1<-tsCV(humextsens, far3, h=24)
sqrt(mean(g1^2, na.rm=TRUE))

#---------------------------------------------------------------------------#

#3. Temperature comedor sensor

#Seasonal differenciation
seasonaldifftemp <- diff(tempcomsens, lag=24, differences=1)
seasonaldifftemp%>%autoplot()+ggtitle("Seasonal differenced temperature comedor sensor")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))

##ETS 
ets_temp <- ets(seasonaldifftemp, model = "ZZZ", damped = NULL, 
                alpha = NULL, beta = NULL, gamma = NULL, phi = NULL,
                lambda = "auto", biasadj = TRUE, ic = c("aicc", "aic", "bic") )

ets_temp%>% autoplot()+ggtitle("ets(A,Ad,A) temperature comedor sensor")
ets_temp

#AIC ets
AIC(ets_temp)

#check residuals ets
checkresiduals(ets_temp)


##ARIMA Model
ARIMA_auto_tempcomsens <- auto.arima(tempcomsens, 
                                     stepwise = FALSE, biasadj = TRUE,
                                     allowdrift = TRUE, lambda = "auto")

#AIC Arima
AIC(ARIMA_auto_tempcomsens)

#check residuals
checkresiduals(ARIMA_auto_tempcomsens)

#Portemanteau tests
adf.test(ARIMA_auto_tempcomsens$residuals)
kpss.test(ARIMA_auto_tempcomsens$residuals)

#Unit roots
ARIMA_auto_tempcomsens%>%autoplot()+ggtitle("Unit roots ARIMA(2,1,0)(2,1,0)24")

#Pacf
ggPacf(ARIMA_auto_tempcomsens$residuals)

#lagplot
gglagplot(ARIMA_auto_tempcomsens$residuals,do.lines=FALSE)+
  ggtitle("Lagplot of ARIMA(2,1,0)(2,1,0)24")

# Forecasts ARIMA_auto_tempcomsens
ARIMA1 <- forecast(ARIMA_auto_tempcomsens, h=1)
ARIMA1
autoplot(ARIMA1)+ggtitle("Forecast ARIMA temperature h=1")+ylab("Temperature")+xlab("Days")
ARIMA24 <- forecast(ARIMA_auto_tempcomsens, h=24)
ARIMA24
autoplot(ARIMA24)+ggtitle("Forecast temperature comedor sensor with ARIMA(2,1,0)(2,1,0)24 h=24")+ylab("Temperature")+xlab("Days")
ARIMA120 <- forecast(ARIMA_auto_tempcomsens, h=120)
ARIMA120
autoplot(ARIMA120)+ggtitle("Forecast temperature comedor sensor with ARIMA(2,1,0)(2,1,0)24 h=120")+ylab("Temperature")+xlab("Days")

#Cross validation
CvARIMA <- function(x,h){forecast(auto.arima(x, stepwise = FALSE), h=h)}

ARIMAcv1 <- tsCV(tempcomsens, CvARIMA, h=1)
ARIMAcv1
ARIMAcv24 <- tsCV(tempcomsens, CvARIMA, h=24)
ARIMAcv24
ARIMAcv120 <- tsCV(tempcomsens, CvARIMA, h=5*24)
ARIMAcv120

#RMSE
RMSE1 <-  sqrt(mean(ARIMAcv1^2, na.rm=TRUE))
RMSE24 <-  sqrt(mean(ARIMAcv24^2, na.rm=TRUE))
RMSE120 <-  sqrt(mean(ARIMAcv120^2, na.rm=TRUE))
RMSE1
RMSE24
RMSE120


#---------------------------------------------------------------------------#

#4. CO2 comedor sensor

#ARIMA
ARIMA_auto_co2 <- auto.arima(co2comsens, stepwise = FALSE, biasadj = TRUE, allowdrift = TRUE, lambda = "auto")
View(ARIMA_auto_co2)

#AIC ARIMA
AIC(ARIMA_auto_co2)


#ETS
ets_co2 <- ets(co2comsens, model = "ZZZ", damped = NULL, alpha = NULL, beta = NULL, gamma = NULL, phi = NULL,lambda = "auto", biasadj = TRUE, ic = c("aicc", "aic", "bic") )
View(ets_co2)

#AIC_ETS
AIC(ets_co2)


#residuals
checkresiduals(ARIMA_auto_co2)
checkresiduals(ets_co2)


#ADF tests
adf.test(ARIMA_auto_co2$residuals)
autoplot(ARIMA_auto_co2)

#KPSSTEST
kpss.test(ARIMA_auto_co2$residuals)


#Forecasts

h1 <- forecast(ARIMA_auto_co2, h = 1)
autoplot(h1)

h24 <- forecast(ARIMA_auto_co2, h = 24)
autoplot(h24)

h120 <- forecast(ARIMA_auto_co2, h = 120)
autoplot(h120)


#Cross-Validation
CvARIMA <- function(x,h){forecast(auto.arima(co2comsens, stepwise = FALSE,
                                             biasadj = TRUE, lambda = "auto"), h=h)}

ARIMA_co2_cv1 <- tsCV(co2comsens, CvARIMA, h=120)
ARIMA_co2_cv1


ARIMA_co2_cv0 <- tsCV(co2comsens, CvARIMA, h=1)
ARIMA_co2_cv0


ARIMA_co2_cv24 <- tsCV(co2comsens, CvARIMA, h=24)
ARIMA_co2_cv24


RMSE1 <-  sqrt(mean(ARIMA_co2_cv0^2, na.rm=TRUE))
RMSE2 <-  sqrt(mean(ARIMA_co2_cv24^2, na.rm=TRUE))
RMSE3 <-  sqrt(mean(ARIMA_co2_cv1^2, na.rm=TRUE))

RMSE1
RMSE2
RMSE3


#---------------------------------------------------------------------------#

# 5.Lightning habitacion sensor

#BoxCox transformation

lightbc <- BoxCox(Lighthabsens, lambda = "auto")
autoplot(lightbc) + ggtitle("Time series using a Box-Cox transformation") + xlab("Days")


#ARIMA 

arim <-  auto.arima(lightbc, stepwise = F)
summary(arim)

checkresiduals(arim)

kpss.test(arim$residuals)

autoplot(arim) + 
  ggtitle("Unit root test")

#ETS (tried but not selected)

etsl<-ets(Lighthabsens, damped=NULL)
checkresiduals(etsl)
etsl%>%summary()


#Cross validation
far2 <- function(x, h){forecast(auto.arima(x, step = F), h=h)}

#1 hour in advance
  e_1h <- tsCV(lightbc, far2, h=1)
  
  #RMSE
  sqrt(mean(e_1h^2, na.rm=TRUE))
  


#24 hours in advance
e_24h <- tsCV(lightbc, far2, h=24)


  #RMSE
  sqrt(mean(e_24h^2, na.rm=TRUE))




#5 days in advance
e_5d <- tsCV(lightbc, far2, h=5*24)


  #RMSE
  sqrt(mean(e_5d^2, na.rm=TRUE))


  
#---------------------------------------------------------------------------#
