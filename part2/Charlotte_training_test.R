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

#Code d'Alex pour cr?er la hts
options(scipen = 999)

load("Group_3.RData")

match <- read_csv("matchmod.csv", col_names = F )

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
names(x.ts) = names(x)


for(i in 1:length(prod)){
  names(x.ts)[i] = prod[i]
}


prod.hts <- hts(x.ts[,1:ncol(x.ts)], characters = c(8,12))

#####----------------------------------------------------------------------------#

#TRAINING AND TEST SET
prod.tr.hts <- window(prod.hts, end = 78)#1an et demi = 78 semaines
prod.ts.hts <- window(prod.hts, start = 79)#depuis mi 2017

#--------------------------------------------------------------------------------#
###### TOTAL (LEVEL = 0)

### ARIMA MODELS
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
###### CATEGORY/GROUP (LEVEL = 1)

### ARIMA MODELS
autoplot(aggts(prod.ts.hts, levels = 1), series = "test set") + 
  autolayer(aggts(prod.tr.bu.arima, levels = 1), series = "bu")+ 
  autolayer(aggts(prod.tr.tda.arima, levels = 1), series = "tdgsa")+ 
  autolayer(aggts(prod.tr.tdp.arima, levels = 1), series = "tdgsf")+ 
  autolayer(aggts(prod.tr.tdf.arima, levels = 1), series = "tdfp")+ 
  autolayer(aggts(prod.tr.or.arima, levels = 1), series = "or")+
  autolayer(aggts(prod.tr.mo.arima, levels = 1), series = "mo")+
  ggtitle("Forecasts categories (level = 1) with ARIMA") +
  labs(x="Weeks", y="Product incomes")

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

### ETS models
autoplot(aggts(prod.ts.hts, levels = 1), series = "test set") + 
  autolayer(aggts(prod.tr.bu.ets, levels = 1), series = "bu")+ 
  autolayer(aggts(prod.tr.tda.ets, levels = 1), series = "tdgsa")+ 
  autolayer(aggts(prod.tr.tdp.ets, levels = 1), series = "tdgsf")+ 
  autolayer(aggts(prod.tr.tdf.ets, levels = 1), series = "tdfp")+ 
  autolayer(aggts(prod.tr.or.ets, levels = 1), series = "or")+
  autolayer(aggts(prod.tr.mo.ets, levels = 1), series = "mo")+
  ggtitle("Forecasts categories (level = 1) with ETS") +
  labs(x="Weeks", y="Product incomes")

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

### RANDOM WALK MODELS
autoplot(aggts(prod.ts.hts, levels = 1), series = "test set") + 
  autolayer(aggts(prod.tr.bu.rw, levels = 1), series = "bu")+ 
  autolayer(aggts(prod.tr.tda.rw, levels = 1), series = "tdgsa")+ 
  autolayer(aggts(prod.tr.tdp.rw, levels = 1), series = "tdgsf")+ 
  autolayer(aggts(prod.tr.tdf.rw, levels = 1), series = "tdfp")+ 
  autolayer(aggts(prod.tr.or.rw, levels = 1), series = "or")+
  autolayer(aggts(prod.tr.mo.rw, levels = 1), series = "mo")+
  ggtitle("Forecasts categories (level = 1) with Random Walk") +
  labs(x="Weeks", y="Product incomes")

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

#--------------------------------------------------------------------------------#
###### PRODUCTS (LEVEL = 2)

### ARIMA MODELS
autoplot(aggts(prod.ts.hts, levels = 2), series = "test set") + 
  autolayer(aggts(prod.tr.bu.arima, levels = 2), series = "bu")+ 
  autolayer(aggts(prod.tr.tda.arima, levels = 2), series = "tdgsa")+ 
  autolayer(aggts(prod.tr.tdp.arima, levels = 2), series = "tdgsf")+ 
  autolayer(aggts(prod.tr.tdf.arima, levels = 2), series = "tdfp")+ 
  autolayer(aggts(prod.tr.or.arima, levels = 2), series = "or")+
  autolayer(aggts(prod.tr.mo.arima, levels = 2), series = "mo")+
  ggtitle("Forecasts products (level = 2) with ARIMA") +
  labs(x="Weeks", y="Product incomes")

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

### ETS models
autoplot(aggts(prod.ts.hts, levels = 2), series = "test set") + 
  autolayer(aggts(prod.tr.bu.ets, levels = 2), series = "bu")+ 
  autolayer(aggts(prod.tr.tda.ets, levels = 2), series = "tdgsa")+ 
  autolayer(aggts(prod.tr.tdp.ets, levels = 2), series = "tdgsf")+ 
  autolayer(aggts(prod.tr.tdf.ets, levels = 2), series = "tdfp")+ 
  autolayer(aggts(prod.tr.or.ets, levels = 2), series = "or")+
  autolayer(aggts(prod.tr.mo.ets, levels = 2), series = "mo")+
  ggtitle("Forecasts products (level = 2) with ETS") +
  labs(x="Weeks", y="Product incomes")

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

### RANDOM WALK MODELS
autoplot(aggts(prod.ts.hts, levels = 2), series = "test set") + 
  autolayer(aggts(prod.tr.bu.rw, levels = 2), series = "bu")+ 
  autolayer(aggts(prod.tr.tda.rw, levels = 2), series = "tdgsa")+ 
  autolayer(aggts(prod.tr.tdp.rw, levels = 2), series = "tdgsf")+ 
  autolayer(aggts(prod.tr.tdf.rw, levels = 2), series = "tdfp")+ 
  autolayer(aggts(prod.tr.or.rw, levels = 2), series = "or")+
  autolayer(aggts(prod.tr.mo.rw, levels = 2), series = "mo")+
  ggtitle("Forecasts products (level = 2) with Random Walk") +
  labs(x="Weeks", y="Product incomes")

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

###-------------------------------------------------------------------------###
prod.tr.agg <- ts(aggts(prod.tr.hts, levels = 2), frequency = 52, start=start(prod.tr.hts$bts))
prod.ts.agg <- ts(aggts(prod.ts.hts, levels = 2), frequency = 52, start=start(prod.ts.hts$bts))

test <- aggts(prod.hts, level=1)

#install.packages("stringr")
library(stringr)

#Graph of groups/categories
cols <- sample(scales::hue_pal(h=c(15,375),
                               c=100,l=65,h.start=0,direction = 1)(NCOL(prod.tr.agg)))
as_tibble(test) %>%
  gather(Group) %>%
  mutate(Date = rep(time(test), NCOL(test)),
         Groups = str_sub(Group,1,8)) %>%
  ggplot(aes(x=Date, y=value, group=Groups, colour=Group)) +
  geom_line() +
  facet_grid(Groups~., scales="free_y") +
  xlab("Weeks") + ylab("Product incomes") +
  ggtitle("Group level")
  #theme(legend.position="none")

prod.ts.fct <- aggts(prod.ts.hts, level=1)

as_tibble(prod.ts.fct) %>%
  gather(Group) %>%
  mutate(Date = rep(time(prod.ts.fct), NCOL(prod.ts.fct)),
         Groups = str_sub(Group,1,8)) %>%
  ggplot(aes(x=Date, y=value, group=Groups, colour=Group)) +
  geom_line() +
  facet_grid(Groups~., scales="free_y") +
  xlab("Weeks") + ylab("Product incomes") +
  ggtitle("Group level: forecasts") +
  scale_colour_manual(values = cols) +
  autolayer(aggts(prod.tr.bu.arima, levels = 1), series = "bu")+
  autolayer(aggts(prod.tr.tda.arima, levels = 1), series = "tdgsa")+ 
  autolayer(aggts(prod.tr.tdp.arima, levels = 1), series = "tdgsf")+ 
  autolayer(aggts(prod.tr.tdf.arima, levels = 1), series = "tdfp")+ 
  autolayer(aggts(prod.tr.or.arima, levels = 1), series = "or")+
  autolayer(aggts(prod.tr.mo.arima, levels = 1), series = "mo")

#bu
bu <- aggts(prod.tr.bu.arima, levels = 1)
bu <- bu %>% 
  as_tibble() %>%
  gather(Group) %>%
  mutate(Date = rep(time(bu), NCOL(bu)),
         Group = str_sub(Group,1,8))
 bu.plot <- bu %>% ggplot(aes(x=Date, y=value, group=Group, colour=Group)) +
  geom_line() +
  facet_grid(Group~., scales="free_y") +
  xlab("Weeks") + ylab("Product incomes") +
  ggtitle("Group level: forecasts") +
  scale_colour_manual(values = cols)
#tda
tda <- aggts(prod.tr.tda.arima, levels = 1)
tda <- tda %>% 
   as_tibble() %>%
   gather(Group) %>%
   mutate(Date = rep(time(tda), NCOL(tda)),
          Groups = str_sub(Group,1,8))
tda.plot <- tda %>% ggplot(aes(x=Date, y=value, group=Groups, colour=Group)) +
   geom_line() +
   facet_grid(Groups~., scales="free_y") +
   xlab("Weeks") + ylab("Product incomes") +
   ggtitle("Group level: forecasts arima tda") +
   scale_colour_manual(values = cols)
#tdp
tdp <- aggts(prod.tr.tdp.arima, levels = 1)
tdp <- tdp %>% 
   as_tibble() %>%
   gather(Group) %>%
   mutate(Date = rep(time(tdp), NCOL(tdp)),
          Groups = str_sub(Group,1,8))
tdp.plot <- tdp %>% ggplot(aes(x=Date, y=value, group=Groups, colour=Group)) +
   geom_line() +
   facet_grid(Groups~., scales="free_y") +
   xlab("Weeks") + ylab("Product incomes") +
   ggtitle("Group level: forecasts arima tdp") +
   scale_colour_manual(values = cols)
#tdf
tdf <- aggts(prod.tr.tdf.arima, levels = 1)
tdf <- tdf %>% 
   as_tibble() %>%
   gather(Group) %>%
   mutate(Date = rep(time(tdf), NCOL(tdf)),
          Groups = str_sub(Group,1,8))
tdf.plot <- tdf %>% ggplot(aes(x=Date, y=value, group=Groups, colour=Group)) +
   geom_line() +
   facet_grid(Groups~., scales="free_y") +
   xlab("Weeks") + ylab("Product incomes") +
   ggtitle("Group level: forecasts arima tdf") +
   scale_colour_manual(values = cols)
#mo
mo <- aggts(prod.tr.mo.arima, levels = 1)
mo <- mo %>% 
   as_tibble() %>%
   gather(Group) %>%
   mutate(Date = rep(time(mo), NCOL(mo)),
          Groups = str_sub(Group,1,8))
mo.plot <- mo %>% ggplot(aes(x=Date, y=value, group=Groups, colour=Group)) +
   geom_line() +
   facet_grid(Groups~., scales="free_y") +
   xlab("Weeks") + ylab("Product incomes") +
   ggtitle("Group level: forecasts arima mo") +
   scale_colour_manual(values = cols)

#or
or <- aggts(prod.tr.or.arima, levels = 1)
or <- or %>% 
   as_tibble() %>%
   gather(Group) %>%
   mutate(Date = rep(time(or), NCOL(or)),
          Groups = str_sub(Group,1,8))
or.plot <- or %>% ggplot(aes(x=Date, y=value, group=Groups, colour=Group)) +
   geom_line() +
   facet_grid(Groups~., scales="free_y") +
   xlab("Weeks") + ylab("Product incomes") +
   ggtitle("Group level: forecasts arima or") +
   scale_colour_manual(values = cols) 

grid.arrange(bu.plot,tda.plot,tdp.plot,tdf.plot,or.plot,mo.plot, ncol=2)

 
#---------------------------------------------------------------------------#
#Graphs of products
test1 <- aggts(prod.hts, level=2)

cols <- sample(scales::hue_pal(h=c(15,375),
                               c=100,l=65,h.start=0,direction = 1)(NCOL(test1)))
as_tibble(test1) %>%
  gather(Group) %>%
  mutate(Date = rep(time(test1), NCOL(test1)),
         Groups = str_sub(Group,1,8)) %>%
  ggplot(aes(x=Date, y=value, group=Groups, colour=Group)) +
  geom_line() +
  facet_grid(Groups~., scales="free_y") +
  xlab("Weeks") + ylab("Product incomes") +
  ggtitle("Product level") +
  scale_colour_manual(values = cols)
#theme(legend.position="none")
  
#---------------------------------------------------------------------------#

#RW
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


