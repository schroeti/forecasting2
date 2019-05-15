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
library(forecast)

### GENERAL EXPLORATORY ANALYSIS

load("/Users/rsefraou/Downloads/Domo_hourly.RData")
### We first import the data set

scale<-as.vector(domo.hourly$Day)
### We have a look at the variables , and see some can be taken away as they are always equal to zero 

time<-ts(domo.hourly[,-c(1,2,3,4,21,22,23)], start=18, frequency=24)
View(time)

##Taking away 21,22,23 because always equal to zero

ourtime<-ts(domo.hourly[,c(5,8,10,13,25)])
View(ourtime)


##scatterplots : 
autoplot(time, facets=TRUE)+
  xlab("day of april 2012")+
  ylab(" ")+ggtitle("domo hourly data")+
  scale_x_continuous("Day",
                     breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) ,
                     labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))

##We see that some of the series seem to strongly ressemble each other.
## For example we have temperature_comedor_sensor that looks like temperature_habitacion_sensor. 
## same with meteo_exterior_sol_sud and meteo_exterior_piranometro and meteo_exterio_est
## CO2_comedor_sensor and CO2_habitacion sensor seem strongly similar

## lighting_comedor_sensor and lighting_habitacion_sensor look strongly like each other with the same seasonality patterns.
## Overall a strong daily seasonnality except for humedad_exterior_sensor, and plots 4,5,6,7 and 10.


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

### EXPLORATORY ANALYSIS OF OUR 5 VARIABLES OF INTEREST

#1. Temperature comedor sensor

###plot of the general behaviour
tempcomsens<-ts(domo.hourly$Temperature_Comedor_Sensor, start=18, frequency=24)

autoplot(tempcomsens)+
  ggtitle("Temperature comedor sensor")+
  ylab("Degree Celcius")+
  scale_x_continuous("Day",
                     breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) ,
                     labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))

## There seems to be a seasonality. A trend that goes up and then down.
##The variance looks quite stable

## Seasonality analysis
ggseasonplot(tempcomsens, 
             year.labels = FALSE,
             year.labels.left = FALSE, 
             polar=TRUE, 
             main="seasonnality of temperature in eating room")

ggseasonplot(tempcomsens)

## clear seasonnality with peak at 16h and min at 6h 

ggsubseriesplot(tempcomsens)+
  ggtitle("Seasonnality per day")
##We see a seasonnality per day quite regular 

## lagplot 
gglagplot(tempcomsens,do.lines=FALSE, lags=30)+
  ggtitle("lagplots for temperature comedor sensor ")
##strong autocorrelation for lags 1,2,3 and 4. Gets weaker and weaker

# ACF and PACF
ggAcf(tempcomsens)
##extreme autocorrelation 
ggPacf(tempcomsens)


# Seasonal decomposition 
p1<-tempcomsens %>%
  decompose(type="additive") %>%
  autoplot () +
  ggtitle("seasonal decomposition of temperature comedor sensor")

p2<-tempcomsens %>% 
  decompose(type="multiplicative") %>% 
  autoplot () +
  ggtitle("seasonal decomposition of temperature comedor sensor")

p3<-tempcomsens%>%
  stl(t.window=13, s.window="periodic", robust=TRUE)%>%
  autoplot()+
  ggtitle("stl decomposition")


grid.arrange(p1,p2,p3)
## I would still use stl 

# variance stabilization 
trtempcomsens<-BoxCox(tempcomsens, lambda="auto")

autoplot(trtempcomsens)+
  ggtitle("Transformed temperature comedor sensor")+
  scale_x_continuous("Day", 
                     breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , 
                     labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
ggAcf(trtempcomsens)

trtempcomsens%>%stl(t.window=13, s.window="periodic", robust=TRUE)%>%autoplot()+ggtitle("stl decomposition")
## do not see much change here









#2. CO2 comedor sensor

co2comsens<-ts(domo.hourly$CO2_Comedor_Sensor, start=18, frequency=24)
### see its behaviour through time 

autoplot(co2comsens)+
  ggtitle("CO2 comedor sensor")+
  ylab("CO2")+
  scale_x_continuous("Day",
                     breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , 
                     labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))

## There seems to be a sort of cycle or weak seasonnality. 
# There is a clear peak Variance is unstable. Overall trend is decreasing

### See seasonality 
ggseasonplot(co2comsens)
##very weak


ggseasonplot(co2comsens,
             year.labels = FALSE, 
             year.labels.left = FALSE, 
             polar=TRUE,
             main="seasonnality of Co2 in eating room")

ggsubseriesplot(co2comsens)+
  ggtitle("Seasonnality per day")
##We see a seasonnality per day exists but there is a clear peak and changing variance visible 

# lags
gglagplot(co2comsens,do.lines=FALSE, lags=30)+ggtitle("lagplots for co2 comedor sensor ")
##some autocorrelation in the first part of the series but not sure


# ACF and PACF
ggAcf(co2comsens)
##extreme autocorrelation
ggPacf(co2comsens)


# seasonal decomposition
co2comsens %>%
  decompose(type="additive") %>% 
  autoplot () +
  ggtitle("seasonal decomposition of co2 comedor sensor")

co2comsens %>% 
  decompose(type="multiplicative") %>% 
  autoplot () + 
  ggtitle("seasonal decomposition of co2 comedor sensor")

co2comsens%>%
  stl(t.window=13, 
      s.window="periodic", 
      robust=TRUE)%>%
  autoplot()+
  ggtitle("stl decomposition")

##use STL

# variance stabilization
trco2comsens<-BoxCox(co2comsens, lambda="auto")

autoplot(trco2comsens)+
  ggtitle("Transformed co2 comedor sensor")+
  scale_x_continuous("Day", 
                     breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , 
                     labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))
ggAcf(trco2comsens)
trco2comsens%>%
  stl(t.window=13,
      s.window="periodic",
      robust=TRUE)%>%
  autoplot()+
  ggtitle("stl decomposition")
## not much difference 








#3. Humedad comedor sensor

## See the behaviour through time 
humcomsens<-ts(domo.hourly$Humedad_Comedor_Sensor,
               start=18,
               frequency=24)

autoplot(humcomsens)+
  ggtitle("Humedad comedor sensor")+
  ylab("Humidity")+
  scale_x_continuous("Day",
                     breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , 
                     labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))

## Clear increasing trend. Variations. The variance seems quite random. Do not see a clear seasonnality

# seasonality analysis
ggseasonplot(humcomsens)
ggseasonplot(humcomsens, 
             year.labels = FALSE,
             year.labels.left = FALSE,
             polar=TRUE,
             main="seasonnality of humidity in eating room")

## quite stable from 0 to 10 h and then variability in all senses

ggsubseriesplot(humcomsens)+
  ggtitle("Seasonnality per day")
##We see a seasonnality per day quite regular until hour 6 and then it is wiggly and with a varying variance

#lags 
gglagplot(humcomsens,do.lines=FALSE)+ggtitle("lagplots for humedad comedor sensor ")
##Extreme correlation. Until lag 16 !

#ACF and PACF 
ggAcf(humcomsens)
##extreme autocorrelation
ggPacf(humcomsens)

##seasonal decomposition
humcomsens %>% decompose(type="additive") %>% autoplot () + ggtitle("seasonal decomposition of humidity comedor sensor")
humcomsens %>% decompose(type="multiplicative") %>% autoplot () + ggtitle("seasonal decomposition of humidity  comedor sensor")
humcomsens %>% stl(t.window=13, s.window="periodic", robust=TRUE)%>%autoplot()+ggtitle("stl decomposition")
## STL is best 


## variance stabilization
trhumcomsens<-BoxCox(humcomsens, lambda="auto")
autoplot(trhumcomsens)+
  ggtitle("Transformed humeded comedor sensor")+
  scale_x_continuous("Day", 
                     breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) ,
                     labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))

ggAcf(trhumcomsens)
trhumcomsens%>%
  stl(t.window=13,
      s.window="periodic", 
      robust=TRUE)%>%
  autoplot()+
  ggtitle("stl decomposition")
## not much difference, a little variance stabilization










#4. Lighting habitacion sensor


# See its behaviour through time
Lighthabsens<-ts(domo.hourly$Lighting_Habitacion_Sensor, start=18, frequency=24)

autoplot(Lighthabsens)+
  ggtitle("Lighting room sensor")+
  ylab("lighting")+
  scale_x_continuous("Day",
                     breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , 
                     labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))

## Extreme seasonnality with wuite stable variance. Trend is stable. 

#Seasonality analysis
ggseasonplot(Lighthabsens)

ggseasonplot(Lighthabsens,
             year.labels = FALSE,
             year.labels.left = FALSE,
             polar=TRUE, 
             main="seasonnality of lighting in room")

##extreme seasonnality with peak at hour 9 or 10. except day 27
ggsubseriesplot(Lighthabsens)+
  ggtitle("Seasonnality per day")
##It varies each day. Not very clear plot 

#lags
gglagplot(Lighthabsens,do.lines=FALSE)+
  ggtitle("lagplots for lighting habitacion sensor ")
##Correlation lag 1. Lag 10, 11, 12, 13 --- to 16 have a structure

# ACF and PACF 
ggAcf(Lighthabsens)
##extreme autocorrelation
ggPacf(Lighthabsens)


#Seasonality decomposition
Lighthabsens %>% 
  decompose(type="additive") %>%
  autoplot () + 
  ggtitle("seasonal decomposition of lighting habitacion sensor")

Lighthabsens %>% 
  decompose(type="multiplicative") %>%
  autoplot () + 
  ggtitle("seasonal decomposition of lighting habitacion sensor")

Lighthabsens%>%
  stl(t.window=13, s.window="periodic", robust=TRUE)%>%
  autoplot()+
  ggtitle("stl decomposition")
## better remainder with STL still 

#Variance stabilization
trLighthabsebs<-BoxCox(Lighthabsens, lambda="auto")

autoplot(trLighthabsebs)+
  ggtitle("Transformed lighting habitacion sensor")+
  scale_x_continuous("Day",
                     breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) , 
                     labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))

ggAcf(trLighthabsebs)
trLighthabsebs%>%
  stl(t.window=13,
      s.window="periodic", 
      robust=TRUE)%>%
  autoplot()+
  ggtitle("stl decomposition")

## stabilises a little bit the variance but not much









#5. Humedad exterior sensor

# See its behaviour through time 
humextsens<-ts(domo.hourly$Humedad_Exterior_Sensor, start=18, frequency=24)

autoplot(humextsens)+
  ggtitle("Humedad exterior sensor")+
  ylab("Humidity outside")+
  scale_x_continuous("Day",
                     breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) ,
                     labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))

##increasing trend, at first seems to be a kind of seasonnality then goes out ouf control. Increasing variance. 

#Seasonality analyis
ggseasonplot(humextsens)

ggseasonplot(humextsens,
             year.labels = FALSE,
             year.labels.left = FALSE,
             polar=TRUE,
             main="seasonnality of humidity outside")

##some kind of seasonnality but with variability day 28 seems to be an exception. 
ggsubseriesplot(humextsens)+ggtitle("Seasonnality per day")
##We see a seasonnality per day quite regular  and a trend quite visible. 

# lags
gglagplot(humextsens,do.lines=FALSE)+
  ggtitle("lagplots for humedad exterior sensor ")
##strong correlation in lags 1 to 4 and gets weaker and weaker. 

# ACF and PACF 
ggAcf(humextsens)
##extreme autocorrelation
ggPacf(humextsens)


# seasonal decomposition
humextsens %>% 
  decompose(type="additive") %>% 
  autoplot () +
  ggtitle("seasonal decomposition of humedad exterior sensor")

humextsens %>% 
  decompose(type="multiplicative") %>% 
  autoplot () +
  ggtitle("seasonal decomposition of humedad exterior sensor")

humextsens %>% 
  stl(t.window=13,
      s.window="periodic",
      robust=TRUE)%>%
  autoplot()+
  ggtitle("stl decomposition")
## remainder is less structured with stl , go with STL

#Variance stabilization
trhumextsens<-BoxCox(humextsens, lambda="auto")

autoplot(trhumextsens)+
  ggtitle("transformed humedad exterior sensor")+
  scale_x_continuous("Day",
                     breaks= c(18,19,20,21,22,23,24,25,26,27,28,29,30) ,
                     labels=c(18,19,20,21,22,23,24,25,26,27,28,29,30))

ggAcf(trhumextsens)

trhumextsens%>%
  stl(t.window=13, 
      s.window="periodic", 
      robust=TRUE)%>%
  autoplot()+
  ggtitle("stl decomposition")
##did help to stabilize variance 




