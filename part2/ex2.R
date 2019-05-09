library(fpp2)
library(astsa)
library(xts)
library(GGally)
library(seasonal)
library(gridExtra)
library(tseries)
library(hts)
library(forecast)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

options(scipen = 999)

load("Group_3.RData")

match <- read_csv("C:/Users/schro/Desktop/forecasting2/Part2/matchmod.csv", col_names = F )

#----------------------------------------------------------------------------------------#
tx <- x
txc <- tx %>%
  colnames() %>%
  as.data.frame() %>%
  slice(3:nrow(x))

match$X3 <- noquote(match$X3)

prod <- c()

for (i in 1:nrow(txc)){
  j <- which(txc[i,] == match$X3)
  prod[i] <- paste(match$X1[j], match$X3[j], sep = "")
}

for(i in 1:length(prod)){
  names(x)[2+i] = prod[i]
}


x.ts <- data.frame(matrix(nrow = 111, ncol = 30))
for(i in 3:ncol(x)){
  x.ts[i-2] <- ts(x[i],  start = c(2016,1), end = c(2018,7), frequency = 52)
}



for(i in 1:length(prod)){
  names(x.ts)[i] = prod[i]
}


prod.hts <- hts(x.ts[,1:ncol(x.ts)], characters = c(8,12))


#---------------------------------------------------------------------------------#

#Grand Total
prod.hts %>% aggts(levels=0) %>%
  autoplot(facet=TRUE) +
  ylab("Revenue in CHF")

#Total by category
prod.hts %>% aggts(levels=1) %>%
  autoplot(facet=TRUE) +
  ylab("Revenue in CHF")

#Each product
prod.hts %>% aggts(levels=2) %>%
  autoplot(facet=TRUE) +
  ylab("Revenue in CHF")



#--------------------------------------------------------------------------------#
#Forecasting methods

#bottom up approach forecast h = 17
#ARIMA(+)
prod.bu.fct.arima <- forecast(object = prod.hts, method = "bu", h = 17, fmethod = "arima")

#Total level

autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.bu.fct.arima, levels = 0), series = "Forecast")

prod.agg0 <- auto.arima(ts(aggts(prod.hts, levels = 0),
                           frequency = 52, start=start(prod.hts$bts)), lambda = T)

checkresiduals(prod.agg0)

prod.agg0.forecast <- forecast(prod.agg0, h = 17)

autoplot(prod.agg0.forecast)

#Category level

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.bu.fct.arima, levels = 1), series = "Forecast")

#Product level
autoplot(aggts(prod.hts, levels = 2), series = "Total") +
  autolayer(aggts(prod.bu.fct.arima, levels = 2), series = c(0)) +
  theme(legend.title = element_blank()) 


#ETS (not possible because frequency > 24)
prod.bu.fct.ets <- forecast(object = prod.hts, h = 17, fmethod = "ets", method = "bu")

#Total level

x.ts.ets <- x.ts %>%
  mutate(sum = rowSums(.[1:30]))


#x.ts.ets.ts <- ts(x.ts.ets[31], start = c(2016,1), frequency = 52)

#ets(x.ts.ets.ts) %>%
#forecast(h = 17) %>%
#autoplot()

autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.bu.fct.ets, levels = 0), series = "Forecast")


#RW (not good either)
prod.bu.fct.rw <- forecast(object = prod.hts, h = 17, fmethod = "rw", method = "bu")
autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.bu.fct.rw, levels = 0), series = "Forecast")

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.bu.fct.rw, levels = 1), series = "Forecast")

#------------------------------------------------------------------------------------#

#ARIMA manually to see if we get the same results, but we have the CI in addition
#If needed we'll do this product after product
#By product category
x.ts.cat <- x.ts %>%
  mutate(Group0D = rowSums(.[1:2])) %>%
  mutate(Group0S = rowSums(.[3:11])) %>%
  mutate(Group0N = rowSums(.[12:13])) %>%
  mutate(Group0C = rowSums(.[14:24])) %>% 
  mutate(Group0K = rowSums(.[25:30])) %>%
  select(31:35)

#Name has an O and not a zero 0
OD.ts <- ts(x.ts.cat$Group0D, start = c(2016,1), frequency = 52)
OS.ts <- ts(x.ts.cat$Group0S, start = c(2016,1), frequency = 52)
ON.ts <- ts(x.ts.cat$Group0N, start = c(2016,1), frequency = 52)
OC.ts <- ts(x.ts.cat$Group0C, start = c(2016,1), frequency = 52)
OK.ts <- ts(x.ts.cat$Group0K, start = c(2016,1), frequency = 52)

OD.ts.arim <- auto.arima(OD.ts, lambda = T, seasonal = T)
OS.ts.arim <- auto.arima(OS.ts, lambda = T, seasonal = T)
ON.ts.arim <- auto.arima(ON.ts, lambda = T, seasonal = T)
OC.ts.arim <- auto.arima(OC.ts, lambda = T, seasonal = T)
OK.ts.arim <- auto.arima(OK.ts, lambda = T, seasonal = T)

OD.ts.arim.f <- forecast(OD.ts.arim, h = 17)
OS.ts.arim.f <- forecast(OS.ts.arim, h = 17)
ON.ts.arim.f <- forecast(ON.ts.arim, h = 17)
OC.ts.arim.f <- forecast(OC.ts.arim, h = 17)
OK.ts.arim.f <- forecast(OK.ts.arim, h = 17)

g1 = autoplot(OD.ts.arim.f)
g2 = autoplot(OS.ts.arim.f)
g3 = autoplot(ON.ts.arim.f)
g4 = autoplot(OC.ts.arim.f)
g5 = autoplot(OK.ts.arim.f)

grid.arrange(g1, g2, g3, g4, g5)
#------------------------------------------------------------------------------------#

#top down approach tdfp forecast h = 17
#ARIMA
prod.tdfp.fct.arima <- forecast(object = prod.hts, method = "tdfp", h = 17, fmethod = "arima")

#Total level

autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdfp.fct.arima, levels = 0), series = "Forecast")


#Category level

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.tdfp.fct.arima, levels = 1), series = "Forecast")

#Product level
autoplot(aggts(prod.hts, levels = 2), series = "Total") +
  autolayer(aggts(prod.tdfp.fct.arima, levels = 2), series = c(0)) 

#ETS 
prod.tdfp.fct.ets <- forecast(object = prod.hts, h = 17, fmethod = "ets", method = "tdfp")


autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdfp.fct.ets, levels = 0), series = "Forecast")


#RW 
prod.tdfp.fct.rw <- forecast(object = prod.hts, h = 17, fmethod = "rw", method = "tdfp")
autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdfp.fct.rw, levels = 0), series = "Forecast")

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.tdfp.fct.rw, levels = 1), series = "Forecast")

#--------------------------------------------------------------------------------------#
#top down approach tdgsa forecast h = 17
#ARIMA
prod.tdgsa.fct.arima <- forecast(object = prod.hts, method = "tdgsa", h = 17, fmethod = "arima")

#Total level

autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdgsa.fct.arima, levels = 0), series = "Forecast")


#Category level

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.tdgsa.fct.arima, levels = 1), series = "Forecast")

#Product level
autoplot(aggts(prod.hts, levels = 2), series = "Total") +
  autolayer(aggts(prod.tdgsa.fct.arima, levels = 2), series = c(0)) 

#ETS 
prod.tdgsa.fct.ets <- forecast(object = prod.hts, h = 17, fmethod = "ets", method = "tdgsa")


autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdgsa.fct.ets, levels = 0), series = "Forecast")


#RW 
prod.tdgsa.fct.rw <- forecast(object = prod.hts, h = 17, fmethod = "rw", method = "tdgsa")
autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdgsa.fct.rw, levels = 0), series = "Forecast")

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.tdgsa.fct.rw, levels = 1), series = "Forecast")

#------------------------------------------------------------------------------------#

#top down approach tdgsf forecast h = 17
#ARIMA
prod.tdgsf.fct.arima <- forecast(object = prod.hts, method = "tdgsf", h = 17, fmethod = "arima")

#Total level

autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdgsf.fct.arima, levels = 0), series = "Forecast")


#Category level

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.tdgsf.fct.arima, levels = 1), series = "Forecast")

#Product level
autoplot(aggts(prod.hts, levels = 2), series = "Total") +
  autolayer(aggts(prod.tdgsf.fct.arima, levels = 2), series = c(0)) 

#ETS 
prod.tdgsf.fct.ets <- forecast(object = prod.hts, h = 17, fmethod = "ets", method = "tdgsf")


autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdgsf.fct.ets, levels = 0), series = "Forecast")



#RW 
prod.tdgsf.fct.rw <- forecast(object = prod.hts, h = 17, fmethod = "rw", method = "tdgsf")
autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdgsf.fct.rw, levels = 0), series = "Forecast")

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.tdgsf.fct.rw, levels = 1), series = "Forecast")

#------------------------------------------------------------------------------------#

#optimal reconciliation forecast h = 17
#ARIMA(+)
prod.or.fct.arima <- forecast(object = prod.hts, method = "comb", weights ="wls", h = 17, fmethod = "arima")

#Total level

autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.or.fct.arima, levels = 0), series = "Forecast")


#Category level

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.tdfp.fct.arima, levels = 1), series = "Forecast")

#Product level
autoplot(aggts(prod.hts, levels = 2), series = "Total") +
  autolayer(aggts(prod.tdfp.fct.arima, levels = 2), series = c(0)) 

#ETS 
prod.tdfp.fct.ets <- forecast(object = prod.hts, h = 17, fmethod = "ets", method = "comb", weights = "wls")


autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdfp.fct.ets, levels = 0), series = "Forecast")


#RW 
prod.tdfp.fct.rw <- forecast(object = prod.hts, h = 17, fmethod = "rw", method = "comb", weights = "wls")
autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdfp.fct.rw, levels = 0), series = "Forecast")

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.tdfp.fct.rw, levels = 1), series = "Forecast")

#------------------------------------------------------------------------------------#

#middle-out approach reconciliation forecast h = 17
#ARIMA (+)
prod.mo.fct.arima <- forecast(object = prod.hts, method = "mo", h = 17, fmethod = "arima", level = 1)

#Total level

autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.mo.fct.arima, levels = 0), series = "Forecast")


#Category level

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.mo.fct.arima, levels = 1), series = "Forecast")

#Product level
autoplot(aggts(prod.hts, levels = 2), series = "Total") +
  autolayer(aggts(prod.mo.fct.arima, levels = 2), series = c(0)) 

#ETS 
prod.mo.fct.ets <- forecast(object = prod.hts, h = 17, fmethod = "ets", method = "mo", level = 1)


autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdfp.fct.ets, levels = 0), series = "Forecast")


#RW 
prod.mo.fct.rw <- forecast(object = prod.hts, h = 17, fmethod = "rw", method = "mo", level = 1)
autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.mo.fct.rw, levels = 0), series = "Forecast")

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.mo.fct.rw, levels = 1), series = "Forecast")





