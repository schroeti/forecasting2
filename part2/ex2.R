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


#-----------------------------------------------------------------------------#
a1 <- ts(x[3],  start = c(2016,1), frequency = 52)
a2 <- ts(x[4],  start = c(2016,1), frequency = 52)
a3 <- ts(x[5],  start = c(2016,1), frequency = 52)
a4 <- ts(x[6],  start = c(2016,1), frequency = 52)
a5 <- ts(x[7],  start = c(2016,1), frequency = 52)
a6 <- ts(x[8],  start = c(2016,1), frequency = 52)
a7 <- ts(x[9],  start = c(2016,1), frequency = 52)
a8 <- ts(x[10],  start = c(2016,1), frequency = 52)
a9 <- ts(x[11],  start = c(2016,1), frequency = 52)
a10 <- ts(x[12],  start = c(2016,1), frequency = 52)
a11 <- ts(x[13],  start = c(2016,1), frequency = 52)
a12 <- ts(x[14],  start = c(2016,1), frequency = 52)
a13 <- ts(x[15],  start = c(2016,1), frequency = 52)
a14 <- ts(x[16],  start = c(2016,1), frequency = 52)
a15 <- ts(x[17],  start = c(2016,1), frequency = 52)
a16 <- ts(x[18],  start = c(2016,1), frequency = 52)
a17 <- ts(x[19],  start = c(2016,1), frequency = 52)
a18 <- ts(x[20],  start = c(2016,1), frequency = 52)
a19 <- ts(x[21],  start = c(2016,1), frequency = 52)
a20 <- ts(x[22],  start = c(2016,1), frequency = 52)
a21 <- ts(x[23],  start = c(2016,1), frequency = 52)
a22 <- ts(x[24],  start = c(2016,1), frequency = 52)
a23 <- ts(x[25],  start = c(2016,1), frequency = 52)
a24 <- ts(x[26],  start = c(2016,1), frequency = 52)
a25 <- ts(x[27],  start = c(2016,1), frequency = 52)
a26 <- ts(x[28],  start = c(2016,1), frequency = 52)
a27 <- ts(x[29],  start = c(2016,1), frequency = 52)
a28 <- ts(x[30],  start = c(2016,1), frequency = 52)
a29 <- ts(x[31],  start = c(2016,1), frequency = 52)
a30 <- ts(x[32],  start = c(2016,1), frequency = 52)


ts <- cbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30)

for(i in 1:length(prod)){
  colnames(ts)[i] = prod[i]
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



