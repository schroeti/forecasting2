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
library(stringr)

options(scipen = 999)

load("Group_3.RData")

match <- read_csv("matchmod.csv", col_names = F )

#----------------------------------------------------------------------------------------#

#Transformation in order to match dataset with match table and create hts
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

#----------------------------------------------------------------------------#

#TRAINING AND TEST SET
prod.tr.hts <- window(prod.hts, end = 78)#78 weeks
prod.ts.hts <- window(prod.hts, start = 79)#the remaining weeks (33)

#----------------------------------------------------------------------------#

#EXPLORATORY ANALYSIS

#Grand Total
prod.hts %>% aggts(levels=0) %>%
  autoplot(facet=TRUE) +
  ylab("Revenue in CHF") +
  ggtitle("Total level")

#Graph of groups/categories

test <- aggts(prod.hts, level=1)

cols <- sample(scales::hue_pal(h=c(15,375),
                               c=100,l=65,h.start=0,direction = 1)(NCOL(prod.hts)))
as_tibble(test) %>%
  gather(Group) %>%
  mutate(Date = rep(time(test), NCOL(test)),
         Groups = str_sub(Group,1,8)) %>%
  ggplot(aes(x=Date, y=value, group=Groups, colour=Group)) +
  geom_line() +
  facet_grid(Groups~., scales="free_y") +
  xlab("Weeks") + ylab("Product incomes") +
  ggtitle("Group level")

#Each product
prod.hts %>% aggts(levels=2) %>%
  autoplot(facet=TRUE) +
  ylab("Revenue in CHF")
#Not very readable

#--------------------------------------------------------------------------------#

#FORECASTING METHODS

#TOTAL (LEVEL = 0)
#ARIMA MODELS
prod.tr.bu.arima <- forecast(prod.tr.hts, h =33, method = "bu" , fmethod = "arima")
prod.tr.tdf.arima <- forecast(prod.tr.hts, h = 33, method = "tdfp" , fmethod = "arima")
prod.tr.tda.arima <- forecast(prod.tr.hts, h = 33, method = "tdgsa", fmethod = "arima")
prod.tr.tdp.arima <- forecast(prod.tr.hts, h = 33, method = "tdgsf", fmethod = "arima")
prod.tr.or.arima <- forecast(prod.tr.hts, h = 33, method = "comb" , weights = "wls", fmethod = "arima")
prod.tr.mo.arima <- forecast(prod.tr.hts, h = 33, method = "mo" , level = 1, fmethod = "arima")

autoplot(aggts(prod.ts.hts, levels = 0), series = "test set") + 
  autolayer(aggts(prod.tr.bu.arima, levels = 0), series = "bu")+ 
  autolayer(aggts(prod.tr.tda.arima, levels = 0), series = "tdgsa")+ 
  autolayer(aggts(prod.tr.tdp.arima, levels = 0), series = "tdgsf")+ 
  autolayer(aggts(prod.tr.tdf.arima, levels = 0), series = "tdfp")+ 
  autolayer(aggts(prod.tr.or.arima, levels = 0), series = "or")+
  autolayer(aggts(prod.tr.mo.arima, levels = 0), series = "mo")+
  ggtitle("Forecasts total (level = 0) with ARIMA ") +
  labs(x="Weeks", y="Product incomes")

#Accuracy arima
acc.tr.bu.arima0 <- t(accuracy.gts(prod.tr.bu.arima, prod.ts.hts, levels = 0))
acc.tr.tda.arima0 <- t(accuracy.gts(prod.tr.tda.arima, prod.ts.hts,levels = 0)) 
acc.tr.tdp.arima0 <- t(accuracy.gts(prod.tr.tdp.arima, prod.ts.hts,levels = 0))
acc.tr.tdf.arima0 <- t(accuracy.gts(prod.tr.tdf.arima, prod.ts.hts,levels = 0))
acc.tr.or.arima0 <- t(accuracy.gts(prod.tr.or.arima, prod.ts.hts,levels = 0))
acc.tr.mo.arima0 <- t(accuracy.gts(prod.tr.mo.arima, prod.ts.hts,levels = 0))

acc.tr.arima0 <- rbind(acc.tr.bu.arima0, acc.tr.tda.arima0, acc.tr.tdp.arima0, 
                       acc.tr.tdf.arima0, acc.tr.or.arima0, acc.tr.mo.arima0)

rownames(acc.tr.arima0) <- c("bu", "tdgsa", "tdgsf", "tdgp", "or", "mo")
knitr::kable(acc.tr.arima0, digits = 4)

### ETS models
prod.tr.bu.ets <- forecast(prod.tr.hts, h =33, method = "bu" , fmethod = "ets")
prod.tr.tdf.ets <- forecast(prod.tr.hts, h = 33, method = "tdfp" , fmethod = "ets")
prod.tr.tda.ets <- forecast(prod.tr.hts, h = 33, method = "tdgsa", fmethod = "ets")
prod.tr.tdp.ets <- forecast(prod.tr.hts, h = 33, method = "tdgsf", fmethod = "ets")
prod.tr.or.ets <- forecast(prod.tr.hts, h = 33, method = "comb" , weights = "wls", fmethod = "ets")
prod.tr.mo.ets <- forecast(prod.tr.hts, h = 33, method = "mo" , level = 1, fmethod = "ets")

autoplot(aggts(prod.ts.hts, levels = 0), series = "test set") + 
  autolayer(aggts(prod.tr.bu.ets, levels = 0), series = "bu")+ 
  autolayer(aggts(prod.tr.tda.ets, levels = 0), series = "tdgsa")+ 
  autolayer(aggts(prod.tr.tdp.ets, levels = 0), series = "tdgsf")+ 
  autolayer(aggts(prod.tr.tdf.ets, levels = 0), series = "tdfp")+ 
  autolayer(aggts(prod.tr.or.ets, levels = 0), series = "or")+
  autolayer(aggts(prod.tr.mo.ets, levels = 0), series = "mo")+
  ggtitle("Forecasts total (level = 0) with ETS") +
  labs(x="Weeks", y="Product incomes")

#Accuracy ets
acc.tr.bu.ets0 <- t(accuracy.gts(prod.tr.bu.ets, prod.ts.hts, levels = 0))
acc.tr.tda.ets0 <- t(accuracy.gts(prod.tr.tda.ets, prod.ts.hts,levels = 0)) 
acc.tr.tdp.ets0 <- t(accuracy.gts(prod.tr.tdp.ets, prod.ts.hts,levels = 0))
acc.tr.tdf.ets0 <- t(accuracy.gts(prod.tr.tdf.ets, prod.ts.hts,levels = 0))
acc.tr.or.ets0 <- t(accuracy.gts(prod.tr.or.ets, prod.ts.hts,levels = 0))
acc.tr.mo.ets0 <- t(accuracy.gts(prod.tr.mo.ets, prod.ts.hts,levels = 0))

acc.tr.ets0 <- rbind(acc.tr.bu.ets0, acc.tr.tda.ets0, acc.tr.tdp.ets0, acc.tr.tdf.ets0, 
                     acc.tr.or.ets0, acc.tr.mo.ets0)

rownames(acc.tr.ets0) <- c("bu", "tdgsa", "tdgsf", "tdgp", "or", "mo")
knitr::kable(acc.tr.ets0, digits = 4)

### RANDOM WALK MODELS
prod.tr.bu.rw <- forecast(prod.tr.hts, h =33, method = "bu" , fmethod = "rw")
prod.tr.tdf.rw <- forecast(prod.tr.hts, h = 33, method = "tdfp" , fmethod = "rw")
prod.tr.tda.rw <- forecast(prod.tr.hts, h = 33, method = "tdgsa", fmethod = "rw")
prod.tr.tdp.rw <- forecast(prod.tr.hts, h = 33, method = "tdgsf", fmethod = "rw")
prod.tr.or.rw <- forecast(prod.tr.hts, h = 33, method = "comb" , weights = "wls", fmethod = "rw")
prod.tr.mo.rw <- forecast(prod.tr.hts, h = 33, method = "mo" , level = 1, fmethod = "rw")

autoplot(aggts(prod.ts.hts, levels = 0), series = "test set") + 
  autolayer(aggts(prod.tr.bu.rw, levels = 0), series = "bu")+ 
  autolayer(aggts(prod.tr.tda.rw, levels = 0), series = "tdgsa")+ 
  autolayer(aggts(prod.tr.tdp.rw, levels = 0), series = "tdgsf")+ 
  autolayer(aggts(prod.tr.tdf.rw, levels = 0), series = "tdfp")+ 
  autolayer(aggts(prod.tr.or.rw, levels = 0), series = "or")+
  autolayer(aggts(prod.tr.mo.rw, levels = 0), series = "mo")+
  ggtitle("Forecasts total (level = 0) with Random Walk") +
  labs(x="Weeks", y="Product incomes")

#Accuracy random walk
acc.tr.bu.rw0 <- t(accuracy.gts(prod.tr.bu.rw, prod.ts.hts, levels = 0))
acc.tr.tda.rw0 <- t(accuracy.gts(prod.tr.tda.rw, prod.ts.hts,levels = 0)) 
acc.tr.tdp.rw0 <- t(accuracy.gts(prod.tr.tdp.rw, prod.ts.hts,levels = 0))
acc.tr.tdf.rw0 <- t(accuracy.gts(prod.tr.tdf.rw, prod.ts.hts,levels = 0))
acc.tr.or.rw0 <- t(accuracy.gts(prod.tr.or.rw, prod.ts.hts,levels = 0))
acc.tr.mo.rw0 <- t(accuracy.gts(prod.tr.mo.rw, prod.ts.hts,levels = 0))

acc.tr.rw0 <- rbind(acc.tr.bu.rw0, acc.tr.tda.rw0, acc.tr.tdp.rw0, acc.tr.tdf.rw0, 
                    acc.tr.or.rw0, acc.tr.mo.rw0)

rownames(acc.tr.rw0) <- c("bu", "tdgsa", "tdgsf", "tdgp", "or", "mo")
knitr::kable(acc.tr.rw0, digits = 4)

#--------------------------------------------------------------------------------#

# CATEGORY/GROUP (LEVEL = 1)

#ARIMA MODELS
#Accuracy
acc.tr.bu.arima1 <- t(accuracy.gts(prod.tr.bu.arima, prod.ts.hts, levels = 1))
acc.tr.tda.arima1 <- t(accuracy.gts(prod.tr.tda.arima, prod.ts.hts,levels = 1)) 
acc.tr.tdp.arima1 <- t(accuracy.gts(prod.tr.tdp.arima, prod.ts.hts,levels = 1))
acc.tr.tdf.arima1 <- t(accuracy.gts(prod.tr.tdf.arima, prod.ts.hts,levels = 1))
acc.tr.or.arima1 <- t(accuracy.gts(prod.tr.or.arima, prod.ts.hts,levels = 1))
acc.tr.mo.arima1 <- t(accuracy.gts(prod.tr.mo.arima, prod.ts.hts,levels = 1))

knitr::kable(acc.tr.bu.arima1, digits = 4)
knitr::kable(acc.tr.tda.arima1, digits = 4)
knitr::kable(acc.tr.tdp.arima1, digits = 4)
knitr::kable(acc.tr.tdf.arima1, digits = 4)
knitr::kable(acc.tr.or.arima1, digits = 4)
knitr::kable(acc.tr.mo.arima1, digits = 4)

#Mean of the RMSE
acc.tr.bu.arima1 <- as_tibble(acc.tr.bu.arima1)
acc.tr.tda.arima1 <- as_tibble(acc.tr.tda.arima1)
acc.tr.tdp.arima1 <- as_tibble(acc.tr.tdp.arima1)
acc.tr.tdf.arima1 <- as_tibble(acc.tr.tdf.arima1)
acc.tr.or.arima1 <- as_tibble(acc.tr.or.arima1)
acc.tr.mo.arima1 <- as_tibble(acc.tr.mo.arima1)

mRmse.bu.arima1 <- mean(acc.tr.bu.arima1$RMSE)
mRmse.tda.arima1 <- mean(acc.tr.tda.arima1$RMSE)
mRmse.tdp.arima1 <- mean(acc.tr.tdp.arima1$RMSE)
mRmse.tdf.arima1 <- mean(acc.tr.tdf.arima1$RMSE)
mRmse.or.arima1 <- mean(acc.tr.or.arima1$RMSE)
mRmse.mo.arima1 <- mean(acc.tr.mo.arima1$RMSE)

RMSE.arima1 <- rbind(mRmse.bu.arima1, mRmse.tda.arima1, mRmse.tdp.arima1,
                     mRmse.tdf.arima1, mRmse.or.arima1, mRmse.mo.arima1)

rownames(RMSE.arima1) <- c("bu", "tdgsa", "tdgp", "tdgsf", "or", "mo")
colnames(RMSE.arima1) <- "RMSE arima 1"

knitr::kable(RMSE.arima1, digits = 4)

#Mean of MAE
mae.bu.arima1 <- mean(acc.tr.bu.arima1$MAE)
mae.tda.arima1 <- mean(acc.tr.tda.arima1$MAE)
mae.tdp.arima1 <- mean(acc.tr.tdp.arima1$MAE)
mae.tdf.arima1 <- mean(acc.tr.tdf.arima1$MAE)
mae.or.arima1 <- mean(acc.tr.or.arima1$MAE)
mae.mo.arima1 <- mean(acc.tr.mo.arima1$MAE)

MAE.arima1 <- rbind(mae.bu.arima1, mae.tda.arima1, mae.tdp.arima1,
                    mae.tdf.arima1, mae.or.arima1, mae.mo.arima1)

rownames(MAE.arima1) <- c("bu", "tdgsa", "tdgp", "tdgsf", "or", "mo")
colnames(MAE.arima1) <- "MAE arima 1"

knitr::kable(MAE.arima1, digits = 4)

#ETS models
#Accuracy
acc.tr.bu.ets1 <- t(accuracy.gts(prod.tr.bu.ets, prod.ts.hts, levels = 1))
acc.tr.tda.ets1 <- t(accuracy.gts(prod.tr.tda.ets, prod.ts.hts,levels = 1)) 
acc.tr.tdp.ets1 <- t(accuracy.gts(prod.tr.tdp.ets, prod.ts.hts,levels = 1))
acc.tr.tdf.ets1 <- t(accuracy.gts(prod.tr.tdf.ets, prod.ts.hts,levels = 1))
acc.tr.or.ets1 <- t(accuracy.gts(prod.tr.or.ets, prod.ts.hts,levels = 1))
acc.tr.mo.ets1 <- t(accuracy.gts(prod.tr.mo.ets, prod.ts.hts,levels = 1))

knitr::kable(acc.tr.bu.ets1, digits = 4)
knitr::kable(acc.tr.tda.ets1, digits = 4)
knitr::kable(acc.tr.tdp.ets1, digits = 4)
knitr::kable(acc.tr.tdf.ets1, digits = 4)
knitr::kable(acc.tr.or.ets1, digits = 4)
knitr::kable(acc.tr.mo.ets1, digits = 4)

#Mean of RMSE
acc.tr.bu.ets1 <- as_tibble(acc.tr.bu.ets1)
acc.tr.tda.ets1 <- as_tibble(acc.tr.tda.ets1)
acc.tr.tdp.ets1 <- as_tibble(acc.tr.tdp.ets1)
acc.tr.tdf.ets1 <- as_tibble(acc.tr.tdf.ets1)
acc.tr.or.ets1 <- as_tibble(acc.tr.or.ets1)
acc.tr.mo.ets1 <- as_tibble(acc.tr.mo.ets1)

mRmse.bu.ets1 <- mean(acc.tr.bu.ets1$RMSE)
mRmse.tda.ets1 <- mean(acc.tr.tda.ets1$RMSE)
mRmse.tdp.ets1 <- mean(acc.tr.tdp.ets1$RMSE)
mRmse.tdf.ets1 <- mean(acc.tr.tdf.ets1$RMSE)
mRmse.or.ets1 <- mean(acc.tr.or.ets1$RMSE)
mRmse.mu.ets1 <- mean(acc.tr.mo.ets1$RMSE)

RMSE.ets1 <- rbind(mRmse.bu.ets1, mRmse.tda.ets1, mRmse.tdp.ets1,
                   mRmse.tdf.ets1, mRmse.or.ets1, mRmse.mu.ets1)

rownames(RMSE.ets1) <- c("bu", "tdgsa", "tdgp", "tdgsf", "or", "mo")
colnames(RMSE.ets1) <- "RMSE ets 1"

knitr::kable(RMSE.ets1, digits = 4)

#Mean of MAE
mae.bu.ets1 <- mean(acc.tr.bu.ets1$MAE)
mae.tda.ets1 <- mean(acc.tr.tda.ets1$MAE)
mae.tdp.ets1 <- mean(acc.tr.tdp.ets1$MAE)
mae.tdf.ets1 <- mean(acc.tr.tdf.ets1$MAE)
mae.or.ets1 <- mean(acc.tr.or.ets1$MAE)
mae.mo.ets1 <- mean(acc.tr.mo.ets1$MAE)

MAE.ets1 <- rbind(mae.bu.ets1, mae.tda.ets1, mae.tdp.ets1,
                  mae.tdf.ets1, mae.or.ets1, mae.mo.ets1)

rownames(MAE.ets1) <- c("bu", "tdgsa", "tdgp", "tdgsf", "or", "mo")
colnames(MAE.ets1) <- "MAE ets 1"

knitr::kable(MAE.ets1, digits = 4)

#RANDOM WALK MODELS
#Accuracy
acc.tr.bu.rw1 <- t(accuracy.gts(prod.tr.bu.rw, prod.ts.hts, levels = 1))
acc.tr.tda.rw1 <- t(accuracy.gts(prod.tr.tda.rw, prod.ts.hts,levels = 1)) 
acc.tr.tdp.rw1 <- t(accuracy.gts(prod.tr.tdp.rw, prod.ts.hts,levels = 1))
acc.tr.tdf.rw1 <- t(accuracy.gts(prod.tr.tdf.rw, prod.ts.hts,levels = 1))
acc.tr.or.rw1 <- t(accuracy.gts(prod.tr.or.rw, prod.ts.hts,levels = 1))
acc.tr.mo.rw1 <- t(accuracy.gts(prod.tr.mo.rw, prod.ts.hts,levels = 1))

knitr::kable(acc.tr.bu.rw1, digits = 4)
knitr::kable(acc.tr.tda.rw1, digits = 4)
knitr::kable(acc.tr.tdp.rw1, digits = 4)
knitr::kable(acc.tr.tdf.rw1, digits = 4)
knitr::kable(acc.tr.or.rw1, digits = 4)
knitr::kable(acc.tr.mo.rw1, digits = 4)

#Mean of RMSE
acc.tr.bu.rw1 <- as_tibble(acc.tr.bu.rw1)
acc.tr.tda.rw1 <- as_tibble(acc.tr.tda.rw1)
acc.tr.tdp.rw1 <- as_tibble(acc.tr.tdp.rw1)
acc.tr.tdf.rw1 <- as_tibble(acc.tr.tdf.rw1)
acc.tr.or.rw1 <- as_tibble(acc.tr.or.rw1)
acc.tr.mo.rw1 <- as_tibble(acc.tr.mo.rw1)

mRmse.bu.rw1 <- mean(acc.tr.bu.rw1$RMSE)
mRmse.tda.rw1 <- mean(acc.tr.tda.rw1$RMSE)
mRmse.tdp.rw1 <- mean(acc.tr.tdp.rw1$RMSE)
mRmse.tdf.rw1 <- mean(acc.tr.tdf.rw1$RMSE)
mRmse.or.rw1 <- mean(acc.tr.or.rw1$RMSE)
mRmse.mu.rw1 <- mean(acc.tr.mo.rw1$RMSE)

RMSE.rw1 <- rbind(mRmse.bu.rw1, mRmse.tda.rw1, mRmse.tdp.rw1,
                  mRmse.tdf.rw1, mRmse.or.rw1, mRmse.mu.rw1)

rownames(RMSE.rw1) <- c("bu", "tdgsa", "tdgp", "tdgsf", "or", "mo")
colnames(RMSE.rw1) <- "RMSE rw 1"

knitr::kable(RMSE.rw1, digits = 4)

#Mean of MAE
mae.bu.rw1 <- mean(acc.tr.bu.rw1$MAE)
mae.tda.rw1 <- mean(acc.tr.tda.rw1$MAE)
mae.tdp.rw1 <- mean(acc.tr.tdp.rw1$MAE)
mae.tdf.rw1 <- mean(acc.tr.tdf.rw1$MAE)
mae.or.rw1 <- mean(acc.tr.or.rw1$MAE)
mae.mu.rw1 <- mean(acc.tr.mo.rw1$MAE)

MAE.rw1 <- rbind(mae.bu.rw1, mae.tda.rw1, mae.tdp.rw1,
                 mae.tdf.rw1, mae.or.rw1, mae.mu.rw1)

rownames(MAE.rw1) <- c("bu", "tdgsa", "tdgp", "tdgsf", "or", "mo")
colnames(MAE.rw1) <- "MAE rw 1"

knitr::kable(MAE.rw1, digits = 4)

#--------------------------------------------------------------------------------#

#PRODUCTS (LEVEL = 2)

#ARIMA MODELS
#Accuracy
acc.tr.bu.arima2 <- t(accuracy.gts(prod.tr.bu.arima, prod.ts.hts, levels = 2))
acc.tr.tda.arima2 <- t(accuracy.gts(prod.tr.tda.arima, prod.ts.hts,levels = 2)) 
acc.tr.tdp.arima2 <- t(accuracy.gts(prod.tr.tdp.arima, prod.ts.hts,levels = 2))
acc.tr.tdf.arima2 <- t(accuracy.gts(prod.tr.tdf.arima, prod.ts.hts,levels = 2))
acc.tr.or.arima2 <- t(accuracy.gts(prod.tr.or.arima, prod.ts.hts,levels = 2))
acc.tr.mo.arima2 <- t(accuracy.gts(prod.tr.mo.arima, prod.ts.hts,levels = 2))

knitr::kable(acc.tr.bu.arima2, digits = 4)
knitr::kable(acc.tr.tda.arima2, digits = 4)
knitr::kable(acc.tr.tdp.arima2, digits = 4)
knitr::kable(acc.tr.tdf.arima2, digits = 4)
knitr::kable(acc.tr.or.arima2, digits = 4)
knitr::kable(acc.tr.mo.arima2, digits = 4)

#Mean of RMSE
acc.tr.bu.arima2 <- as_tibble(acc.tr.bu.arima2)
acc.tr.tda.arima2 <- as_tibble(acc.tr.tda.arima2)
acc.tr.tdp.arima2 <- as_tibble(acc.tr.tdp.arima2)
acc.tr.tdf.arima2 <- as_tibble(acc.tr.tdf.arima2)
acc.tr.or.arima2 <- as_tibble(acc.tr.or.arima2)
acc.tr.mo.arima2 <- as_tibble(acc.tr.mo.arima2)

mRmse.bu.arima2 <- mean(acc.tr.bu.arima2$RMSE)
mRmse.tda.arima2 <- mean(acc.tr.tda.arima2$RMSE)
mRmse.tdp.arima2 <- mean(acc.tr.tdp.arima2$RMSE)
mRmse.tdf.arima2 <- mean(acc.tr.tdf.arima2$RMSE)
mRmse.or.arima2 <- mean(acc.tr.or.arima2$RMSE)
mRmse.mo.arima2 <- mean(acc.tr.mo.arima2$RMSE)

RMSE.arima2 <- rbind(mRmse.bu.arima2, mRmse.tda.arima2, mRmse.tdp.arima2,
                     mRmse.tdf.arima2, mRmse.or.arima2, mRmse.mo.arima2)

rownames(RMSE.arima2) <- c("bu", "tdgsa", "tdgp", "tdgsf", "or", "mo")
colnames(RMSE.arima2) <- "RMSE arima 2"

knitr::kable(RMSE.arima2, digits = 4)

#Mean of MAE
mae.bu.arima2 <- mean(acc.tr.bu.arima2$MAE)
mae.tda.arima2 <- mean(acc.tr.tda.arima2$MAE)
mae.tdp.arima2 <- mean(acc.tr.tdp.arima2$MAE)
mae.tdf.arima2 <- mean(acc.tr.tdf.arima2$MAE)
mae.or.arima2 <- mean(acc.tr.or.arima2$MAE)
mae.mo.arima2 <- mean(acc.tr.mo.arima2$MAE)

MAE.arima2 <- rbind(mae.bu.arima2, mae.tda.arima2, mae.tdp.arima2,
                    mae.tdf.arima2, mae.or.arima2, mae.mo.arima2)

rownames(MAE.arima2) <- c("bu", "tdgsa", "tdgp", "tdgsf", "or", "mo")
colnames(MAE.arima2) <- "MAE arima 2"

knitr::kable(MAE.arima2, digits = 4)

#ETS models
#Accuracy
acc.tr.bu.ets2 <- t(accuracy.gts(prod.tr.bu.ets, prod.ts.hts, levels = 2))
acc.tr.tda.ets2 <- t(accuracy.gts(prod.tr.tda.ets, prod.ts.hts,levels = 2)) 
acc.tr.tdp.ets2 <- t(accuracy.gts(prod.tr.tdp.ets, prod.ts.hts,levels = 2))
acc.tr.tdf.ets2 <- t(accuracy.gts(prod.tr.tdf.ets, prod.ts.hts,levels = 2))
acc.tr.or.ets2 <- t(accuracy.gts(prod.tr.or.ets, prod.ts.hts,levels = 2))
acc.tr.mo.ets2 <- t(accuracy.gts(prod.tr.mo.ets, prod.ts.hts,levels = 2))

knitr::kable(acc.tr.bu.ets2, digits = 4)
knitr::kable(acc.tr.tda.ets2, digits = 4)
knitr::kable(acc.tr.tdp.ets2, digits = 4)
knitr::kable(acc.tr.tdf.ets2, digits = 4)
knitr::kable(acc.tr.or.ets2, digits = 4)
knitr::kable(acc.tr.mo.ets2, digits = 4)

#Mean of RMSE
acc.tr.bu.ets2 <- as_tibble(acc.tr.bu.ets2)
acc.tr.tda.ets2 <- as_tibble(acc.tr.tda.ets2)
acc.tr.tdp.ets2 <- as_tibble(acc.tr.tdp.ets2)
acc.tr.tdf.ets2 <- as_tibble(acc.tr.tdf.ets2)
acc.tr.or.ets2 <- as_tibble(acc.tr.or.ets2)
acc.tr.mo.ets2 <- as_tibble(acc.tr.mo.ets2)

mRmse.bu.ets2 <- mean(acc.tr.bu.ets2$RMSE)
mRmse.tda.ets2 <- mean(acc.tr.tda.ets2$RMSE)
mRmse.tdp.ets2 <- mean(acc.tr.tdp.ets2$RMSE)
mRmse.tdf.ets2 <- mean(acc.tr.tdf.ets2$RMSE)
mRmse.or.ets2 <- mean(acc.tr.or.ets2$RMSE)
mRmse.mu.ets2 <- mean(acc.tr.mo.ets2$RMSE)

RMSE.ets2 <- rbind(mRmse.bu.ets2, mRmse.tda.ets2, mRmse.tdp.ets2,
                   mRmse.tdf.ets2, mRmse.or.ets2, mRmse.mu.ets2)

rownames(RMSE.ets2) <- c("bu", "tdgsa", "tdgp", "tdgsf", "or", "mo")
colnames(RMSE.ets2) <- "RMSE ets 2"

knitr::kable(RMSE.ets2, digits = 4)

#Mean of MAE
mae.bu.ets2 <- mean(acc.tr.bu.ets2$MAE)
mae.tda.ets2 <- mean(acc.tr.tda.ets2$MAE)
mae.tdp.ets2 <- mean(acc.tr.tdp.ets2$MAE)
mae.tdf.ets2 <- mean(acc.tr.tdf.ets2$MAE)
mae.or.ets2 <- mean(acc.tr.or.ets2$MAE)
mae.mu.ets2 <- mean(acc.tr.mo.ets2$MAE)

MAE.ets2 <- rbind(mae.bu.ets2, mae.tda.ets2, mae.tdp.ets2,
                  mae.tdf.ets2, mae.or.ets2, mae.mu.ets2)

rownames(MAE.ets2) <- c("bu", "tdgsa", "tdgp", "tdgsf", "or", "mo")
colnames(MAE.ets2) <- "MAE ets 2"

knitr::kable(MAE.ets2, digits = 4)

#RANDOM WALK MODELS
#Accuracy
acc.tr.bu.rw2 <- t(accuracy.gts(prod.tr.bu.rw, prod.ts.hts, levels = 2))
acc.tr.tda.rw2 <- t(accuracy.gts(prod.tr.tda.rw, prod.ts.hts,levels = 2)) 
acc.tr.tdp.rw2 <- t(accuracy.gts(prod.tr.tdp.rw, prod.ts.hts,levels = 2))
acc.tr.tdf.rw2 <- t(accuracy.gts(prod.tr.tdf.rw, prod.ts.hts,levels = 2))
acc.tr.or.rw2 <- t(accuracy.gts(prod.tr.or.rw, prod.ts.hts,levels = 2))
acc.tr.mo.rw2 <- t(accuracy.gts(prod.tr.mo.rw, prod.ts.hts,levels = 2))

knitr::kable(acc.tr.bu.rw2, digits = 4)
knitr::kable(acc.tr.tda.rw2, digits = 4)
knitr::kable(acc.tr.tdp.rw2, digits = 4)
knitr::kable(acc.tr.tdf.rw2, digits = 4)
knitr::kable(acc.tr.or.rw2, digits = 4)
knitr::kable(acc.tr.mo.rw2, digits = 4)

#Mean of RMSE
acc.tr.bu.rw2 <- as_tibble(acc.tr.bu.rw2)
acc.tr.tda.rw2 <- as_tibble(acc.tr.tda.rw2)
acc.tr.tdp.rw2 <- as_tibble(acc.tr.tdp.rw2)
acc.tr.tdf.rw2 <- as_tibble(acc.tr.tdf.rw2)
acc.tr.or.rw2 <- as_tibble(acc.tr.or.rw2)
acc.tr.mo.rw2 <- as_tibble(acc.tr.mo.rw2)

mRmse.bu.rw2 <- mean(acc.tr.bu.rw2$RMSE)
mRmse.tda.rw2 <- mean(acc.tr.tda.rw2$RMSE)
mRmse.tdp.rw2 <- mean(acc.tr.tdp.rw2$RMSE)
mRmse.tdf.rw2 <- mean(acc.tr.tdf.rw2$RMSE)
mRmse.or.rw2 <- mean(acc.tr.or.rw2$RMSE)
mRmse.mu.rw2 <- mean(acc.tr.mo.rw2$RMSE)

RMSE.rw2 <- rbind(mRmse.bu.rw2, mRmse.tda.rw2, mRmse.tdp.rw2,
                  mRmse.tdf.rw2, mRmse.or.rw2, mRmse.mu.rw2)

rownames(RMSE.rw2) <- c("bu", "tdgsa", "tdgp", "tdgsf", "or", "mo")
colnames(RMSE.rw2) <- "RMSE rw 2"

knitr::kable(RMSE.rw2, digits = 4)

#Mean of MAE
mae.bu.rw2 <- mean(acc.tr.bu.rw2$MAE)
mae.tda.rw2 <- mean(acc.tr.tda.rw2$MAE)
mae.tdp.rw2 <- mean(acc.tr.tdp.rw2$MAE)
mae.tdf.rw2 <- mean(acc.tr.tdf.rw2$MAE)
mae.or.rw2 <- mean(acc.tr.or.rw2$MAE)
mae.mu.rw2 <- mean(acc.tr.mo.rw2$MAE)

MAE.rw2 <- rbind(mae.bu.rw2, mae.tda.rw2, mae.tdp.rw2,
                 mae.tdf.rw2, mae.or.rw2, mae.mu.rw2)

rownames(MAE.rw2) <- c("bu", "tdgsa", "tdgp", "tdgsf", "or", "mo")
colnames(MAE.rw2) <- "MAE rw 2"

knitr::kable(MAE.rw2, digits = 4)

#---------------------------------------------------------------------------#

#FORECASTING PART

#Random Walk
prod.tdgsa.fct.rw <- forecast(object = prod.hts, h = 17, fmethod = "rw", method = "tdgsa")
autoplot(aggts(prod.hts, levels = 0), series = "Total") +
  autolayer(aggts(prod.tdgsa.fct.rw, levels = 0), series = "Forecast")

autoplot(aggts(prod.hts, levels = 1), series = "Total") +
  autolayer(aggts(prod.tdgsa.fct.rw, levels = 1), series = "Forecast")

autoplot(aggts(prod.hts, levels = 2), series = "Total") +
  autolayer(aggts(prod.tdgsa.fct.rw, levels = 2), series = "Forecast")

prod_rw_before <- ts(aggts(prod.hts, levels = 0),
                     frequency = 52, start=start(prod.hts$bts))

prod_rw <- rwf(prod_rw_before, lambda = T, h = 17)

checkresiduals(prod_rw)

autoplot(prod_rw)

