# EXPLORATORY ANALYSIS.

library( fpp2 )
install.packages("astsa")
library( astsa )
library( xts )
install.packages("GGally")
library(GGally)
install.packages("seasonal")
library(seasonal)
library(gridExtra)

scale<-as.vector(domo.hourly$Day)
time<-ts(domo.hourly[,-c(1,2,3,4,21,22,23)], start=18, frequency=24)
View(time)
##Taking away 21,22,23 because always equal to zero
ourtime<-ts(domo.hourly[,c(5,8,10,13,25)])
View(ourtime)


##scatterplots : 
autoplot(time, facets=TRUE)+xlab("day of april 2012")+ylab(" ")+ggtitle("domo hourly data")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
##We see that some of the series seem to strongly ressemble each other. For example we have Temperature_Comedor sensor
## that looks like temperature_habitacion_sensor. 
## same with meteo_exterior_sol_sud and meteo_exterior_piranometro and meteo_exterio_est
## CO2_comedor_sensor and CO2_habitacion sensor seem strongly similar

## lighting comedor sensor and lighting habitacion sensor look strongly like each other with the same seasonnalities
## Overall a strong daily seasonnality except for humedad_exterior_sensor, and plots 4,5,6,7 and 10.

time %>% as.data.frame() %>% GGally::ggpairs()
##too big to really see what happens. 
## selection of the ones that look more alike

##the three temperatures 
time[,c(1,2,3)]%>%as.data.frame()%>%GGally::ggpairs()
## Extreme correlation (99,7%) between temperature_comedor_sensor and temperature_habitacion_sensor. Both exhibit a strong correlation with Weather_temperature too 

##co2 and humidity
time[,c(4,5,6,7)]%>%as.data.frame()%>%GGally::ggpairs()
## C02 of comedor and habitacion are strongly correlated, Humidity of comedor and habitacion strongly correlated too

##lightings
time[,c(8,9)]%>%as.data.frame()%>%GGally::ggpairs()
## Correlation of 93,7% between the lightings

##meteo 
time[,c(13,14,15,16)]%>%as.data.frame()%>%GGally::ggpairs()
## at the end the only strong correlation is between meteo_exterior_piranometro and meteo_exterior_sol_sud.

ourtime%>% as.data.frame()%>%GGally::ggpairs()
##no strong correlation in our particular variables of interest 



#1. Temperature comedor sensor

  tempcomsens<-ts(domo.hourly$Temperature_Comedor_Sensor, start=18, frequency=24)
  autoplot(tempcomsens)+ggtitle("Temperature comedor sensor")+ylab("Degree Celcius")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
  ## There seems to be a seasonality. A trend that goes up and then down. The variance looks quite stable
  ggseasonplot(tempcomsens, year.labels = FALSE, year.labels.left = FALSE, polar=TRUE, main="seasonnality of temperature in eating room")
  ggseasonplot(tempcomsens)
  ##clear seasonnality with peak at 16h and min at 6h 
  ##il faut enlever le "year " à droite. Comment faire ?
  ggsubseriesplot(tempcomsens)+ggtitle("Seasonnality per day")
  ##We see a seasonnality per day quite regular 
  
  gglagplot(tempcomsens,do.lines=FALSE)+ggtitle("lagplots for temperature comedor sensor ")
  ##strong autocorrelation for lags 1,2,3 and 4. Gets weaker and weaker
  
  ggAcf(tempcomsens)
  ##extreme autocorrelation 
  
  p1<-tempcomsens %>% decompose(type="additive") %>% autoplot () + ggtitle("seasonal decomposition of temperature comedor sensor")
  p2<-tempcomsens %>% decompose(type="multiplicative") %>% autoplot () + ggtitle("seasonal decomposition of temperature comedor sensor")
  ## Seem similar decomposition. We see the strong seasonnality. All peaks are not explained 
  ## multiplicative explains better overall as remainder seem smaller .
  
  ##x11temcomsens<-ts(domo.hourly$Temperature_Comedor_Sensor,start=c(2012,04,18), frequency=(24*365))
  
  ##seas(x11temcomsens,x11="") %>% autoplot () + ggtitle("seasonal decomposition of temperature comedor sensor")
  ## does not work. Erros. 
 
  p3<-tempcomsens%>%stl(t.window=13, s.window="periodic", robust=TRUE)%>%autoplot()+ggtitle("stl decomposition")
  ## I would still use multiplicative 
  grid.arrange(p1,p2,p3)
  
  trtempcomsens<-BoxCox(tempcomsens, lambda="auto")
  autoplot(trtempcomsens)+ggtitle("Transformed temperature comedor sensor")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
  ggAcf(trtempcomsens)
  trtempcomsens%>%stl(t.window=13, s.window="periodic", robust=TRUE)%>%autoplot()+ggtitle("stl decomposition")
  ## do not see much change here
  
#2. CO2 comedor sensor
  
  co2comsens<-ts(domo.hourly$CO2_Comedor_Sensor, start=18, frequency=24)
  autoplot(co2comsens)+ggtitle("CO2 comedor sensor")+ylab("CO2")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
  ## There seems to be a sort of cycle or weak seasonnality. There is a clear peak Variance is unstable. Overall trend is decreasing
  ggseasonplot(co2comsens)
  ##finally no seasonnality 
  ggseasonplot(co2comsens, year.labels = FALSE, year.labels.left = FALSE, polar=TRUE, main="seasonnality of Co2 in eating room")
  ggsubseriesplot(co2comsens)+ggtitle("Seasonnality per day")
  ##We see a seasonnality per day exists but there is a clear peak and changing variance visible 
  
  gglagplot(co2comsens,do.lines=FALSE)+ggtitle("lagplots for co2 comedor sensor ")
  ##some autocorrelation in the first part of the series but not sure
  
  ggAcf(co2comsens)
  ##extreme autocorrelation
  
  co2comsens %>% decompose(type="additive") %>% autoplot () + ggtitle("seasonal decomposition of co2 comedor sensor")
  co2comsens %>% decompose(type="multiplicative") %>% autoplot () + ggtitle("seasonal decomposition of co2 comedor sensor")
  ## multiplicative is much much better !!!
  
  co2comsens%>%stl(t.window=13, s.window="periodic", robust=TRUE)%>%autoplot()+ggtitle("stl decomposition")
  ##smallest remainder with multiplicative also 
  
  trco2comsens<-BoxCox(co2comsens, lambda="auto")
  autoplot(trco2comsens)+ggtitle("Transformed co2 comedor sensor")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
  ggAcf(trco2comsens)
  trco2comsens%>%stl(t.window=13, s.window="periodic", robust=TRUE)%>%autoplot()+ggtitle("stl decomposition")
  ## not much difference 
  
  
#3. Humedad comedor sensor
  
  humcomsens<-ts(domo.hourly$Humedad_Comedor_Sensor, start=18, frequency=24)
  autoplot(humcomsens)+ggtitle("Humedad comedor sensor")+ylab("Humidity")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
  ## Clear increasing trend. Variations. The variance seems quite random. Do not see a clear seasonnality
  ggseasonplot(humcomsens)
  ggseasonplot(humcomsens, year.labels = FALSE, year.labels.left = FALSE, polar=TRUE, main="seasonnality of humidity in eating room")
  ## quite stable from 0 to 10 h and then variability in all senses
  ggsubseriesplot(humcomsens)+ggtitle("Seasonnality per day")
  ##We see a seasonnality per day quite regular until hour 6 and then it is wiggly and with a varying variance
  
  gglagplot(humcomsens,do.lines=FALSE)+ggtitle("lagplots for humedad comedor sensor ")
  ##Extreme correlation. Until lag 16 !
  
  ggAcf(humcomsens)
  ##extreme autocorrelation
  
  humcomsens %>% decompose(type="additive") %>% autoplot () + ggtitle("seasonal decomposition of humidity comedor sensor")
  humcomsens %>% decompose(type="multiplicative") %>% autoplot () + ggtitle("seasonal decomposition of humidity  comedor sensor")
  ##multiplicative is better but remainder still not unstructured etc. All is not explained 
  
  humcomsens %>% stl(t.window=13, s.window="periodic", robust=TRUE)%>%autoplot()+ggtitle("stl decomposition")
  ## smallest remaindr with multiplicative
  
  trhumcomsens<-BoxCox(humcomsens, lambda="auto")
  autoplot(trhumcomsens)+ggtitle("Transformed humeded comedor sensor")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
  ggAcf(trhumcomsens)
  trhumcomsens%>%stl(t.window=13, s.window="periodic", robust=TRUE)%>%autoplot()+ggtitle("stl decomposition")
  ## not much difference, a little variance stabilization
  
  
#4. Lighting habitacion sensor
  
  Lighthabsens<-ts(domo.hourly$Lighting_Habitacion_Sensor, start=18, frequency=24)
  autoplot(Lighthabsens)+ggtitle("Lighting room sensor")+ylab("lighting")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
  ## Extreme seasonnality with wuite stable variance. Trend is stable. 
  ggseasonplot(Lighthabsens)
  ggseasonplot(Lighthabsens, year.labels = FALSE, year.labels.left = FALSE, polar=TRUE, main="seasonnality of lighting in room")
  ##extreme seasonnality with peak at hour 9 or 10. except day 27
  ggsubseriesplot(Lighthabsens)+ggtitle("Seasonnality per day")
  ##It varies each day. Not very clear plot 
  
  gglagplot(Lighthabsens,do.lines=FALSE)+ggtitle("lagplots for lighting habitacion sensor ")
  ##Correlation lag 1. Lag 10, 11, 12, 13 --- to 16 have a structure
  
  ggAcf(Lighthabsens)
  ##extreme autocorrelation
  
  Lighthabsens %>% decompose(type="additive") %>% autoplot () + ggtitle("seasonal decomposition of lighting habitacion sensor")
  Lighthabsens %>% decompose(type="multiplicative") %>% autoplot () + ggtitle("seasonal decomposition of lighting habitacion sensor")
  ##multiplicative is much better with smaller remainder. Still not unstructured enough with over and undershooting
  
  Lighthabsens%>%stl(t.window=13, s.window="periodic", robust=TRUE)%>%autoplot()+ggtitle("stl decomposition")
  ## better remainder with multiplicative still 
  
  trLighthabsebs<-BoxCox(Lighthabsens, lambda="auto")
  autoplot(trLighthabsebs)+ggtitle("Transformed lighting habitacion sensor")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
  ggAcf(trLighthabsebs)
  trLighthabsebs%>%stl(t.window=13, s.window="periodic", robust=TRUE)%>%autoplot()+ggtitle("stl decomposition")
  ## stabilises a little bit the variance but not much
  
#5. Humedad exterior sensor
  
  humextsens<-ts(domo.hourly$Humedad_Exterior_Sensor, start=18, frequency=24)
  autoplot(humextsens)+ggtitle("Humedad exterior sensor")+ylab("Humidity outside")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
  ##increasing trend, at first seems to be a kind of seasonnality then goes out ouf control. Increasing variance. 
  ggseasonplot(humextsens)
  ggseasonplot(humextsens, year.labels = FALSE, year.labels.left = FALSE, polar=TRUE, main="seasonnality of humidity outside")
  ##some kind of seasonnality but with varianility. day 28 seems to be an exception. 
  ggsubseriesplot(humextsens)+ggtitle("Seasonnality per day")
  ##We see a seasonnality per day quite regular  and a trend quite visible. 
  
  gglagplot(humextsens,do.lines=FALSE)+ggtitle("lagplots for humedad exterior sensor ")
  ##strong correlation in lags 1 to 4 and gets weaker and weaker. 
  
  ggAcf(humextsens)
  ##extreme autocorrelation
  
  humextsens %>% decompose(type="additive") %>% autoplot () + ggtitle("seasonal decomposition of humedad exterior sensor")
  humextsens %>% decompose(type="multiplicative") %>% autoplot () + ggtitle("seasonal decomposition of humedad exterior sensor")
  ## multiplicative better but still not perfect etc. 
  
  humextsens %>% stl(t.window=13, s.window="periodic", robust=TRUE)%>%autoplot()+ggtitle("stl decomposition")
  ## remainder is less structured with stl but smaller with multiplicative => would go with multiplicative 

  trhumextsens<-BoxCox(humextsens, lambda="auto")
  autoplot(trhumextsens)+ggtitle("transformed humedad exterior sensor")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
  ggAcf(trhumextsens)
  trhumextsens%>%stl(t.window=13, s.window="periodic", robust=TRUE)%>%autoplot()+ggtitle("stl decomposition")
  ##did help to stabilize variance 
  
  ##differentiatng 
  diff(trhumextsens)%>%autoplot()+ggtitle("differentiated humidity exterior sensor")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
  diff(diff(trhumextsens))%>%autoplot()+ggtitle("differentiated humidity exterior sensor")+scale_x_continuous("Day", breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
  ##now much more stationnary. 
  
  auto.arima(humextsens, step=FALSE)
  

