library( fpp2 )
library( astsa )
library( xts )
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
#ARIMA
prod.bu.fct.arima <- forecast(object = prod.hts, method = "bu", h = 17, fmethod = "arima")
#Total level

autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.bu.fct.arima, levels = 0), series = "Forecast")

prod.agg0 <- auto.arima(aggts(prod.hts, levels = 0))
prod.agg0.f <- forecast(prod.agg0, h = 17)

autoplot(prod.agg0.f) +
  autolayer(aggts(prod.bu.fct.arima, levels = 0), series = "hts")



#Category level
autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.bu.fct.arima, levels = 1), series = "Forecast")

prod.agg1 <- auto.arima(aggts(prod.hts, levels = 1))

prod.agg1.f <- forecast(prod.agg1, h = 17)

autoplot(prod.agg1.f) +
  autolayer(aggts(prod.bu.fct, levels = 1), series = "hts")

#Product level
autoplot(aggts(prod.hts, levels = 2), series = "Total") +
  autolayer(aggts(prod.bu.fct.arima, levels = 2), series = "Forecast")



