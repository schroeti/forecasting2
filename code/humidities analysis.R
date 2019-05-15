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







#---------------------------------------------------------------------------#

#4. CO2 comedor sensor

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
