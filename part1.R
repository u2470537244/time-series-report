#Preparation
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE, cache = FALSE,
  dev.args = list(pointsize = 11), fig.height=3.5, fig.width=7, fig.align='center'
)
options(digits = 3, width = 60)

rm(list=ls())

lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))


library(tidyverse)
library(fpp3)
library(tidyquant)
library(sandwich)
library(lmtest)
library(GGally)
library(kableExtra)

#define stocks
stock <- tidyquant::tq_get(c("AAPL","MSFT","TSLA","AMZN"), 
                           get = "stock.prices" ,from="2010-06-29", to="2022-06-29") %>% 
  mutate(symbol = case_when(
    symbol == "AAPL" ~ "Apple",
    symbol == "MSFT" ~ "Microsoft",
    symbol == "TSLA" ~ "Tesla",
    symbol == "AMZN" ~ "Amazon"
  )) %>% 
  mutate(date = as_date(date)) %>% 
  as_tsibble(index = date,key = symbol) %>% 
  group_by_key() %>% 
  mutate(rtn = difference(log(close))) %>% #this is the log return computed
  filter(!is.na(rtn)) %>% 
  group_by_key() %>% 
  mutate(trading_day = row_number()) 

stock %>% select(symbol,date,close,rtn)

#1b
#plot log returns and time series
stock %>% 
  autoplot(close)+facet_grid(vars(symbol), scale = "free_y")+
  theme(legend.position="none", axis.text=element_text(size=6),
        axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 6))

stock %>% 
  autoplot(rtn)+facet_grid(vars(symbol), scale = "free_y" )+
  theme(legend.position="none", axis.text=element_text(size=6),
        axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 6))

#summary statistics(mean, standard deviation, skewness, kurt)
  #mean
stock %>%
  features(rtn, list(mean = ~mean(., na.rm=TRUE), 
                     n = ~length(.))) %>%
  kbl(booktabs = TRUE)

  #standard deviation
stock %>%
  features(rtn, list(sd = ~sd(., na.rm=TRUE), 
                     n = ~length(.))) %>%
  kbl(booktabs = TRUE)

  #Skew
stock %>%
  features(rtn,list(skew =  timeSeries::colSkewness, 
                    n = ~length(.))) %>%
  mutate(skew_stat =  sqrt(n)*skew/sqrt(6), 
         p_val_skew_stat = 2*(1-pnorm(abs(skew_stat))))


stock %>%
  features(rtn,list(skew =  timeSeries::colSkewness, 
                    n = ~length(.))) %>% 
  mutate(skew_stat =  sqrt(n)*skew/sqrt(6), 
         p_val_skew_stat = 2*(1-pnorm(abs(skew_stat)))) %>% 
  kbl(booktabs = TRUE)

  #kurt
stock %>%
  features(rtn,list(kurt =  timeSeries::colKurtosis, 
                    n = ~length(.))) %>% 
  mutate(kurt = kurt +3, 
         kurt_stat =  sqrt(n)*kurt/sqrt(24), 
         p_val_kurt_stat = 2*(1-pnorm(abs(kurt_stat))))


stock %>%
  features(rtn,list(kurt =  timeSeries::colKurtosis, 
                    n = ~length(.))) %>% 
  mutate(kurt = kurt+3,
         kurt_stat =  sqrt(n)*kurt/sqrt(24), 
         p_val_kurt_stat = 2*(1-pnorm(abs(kurt_stat)))) %>% 
  kbl(booktabs = TRUE)

#1c
  #ACF for log returns
stock %>% update_tsibble(index = trading_day) %>% ACF(rtn) %>% 
  autoplot()+
  ggtitle("ACF for log returns")+
  theme(legend.position="none", axis.text=element_text(size=6),
        axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 6))

  #PACF for log returns
stock %>% update_tsibble(index = trading_day) %>% PACF(rtn) %>% 
  autoplot()+
  ggtitle("PACF for log returns")+
  theme(legend.position="none", axis.text=element_text(size=6),
        axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 6))
  
#1d
  #ACF for squared log returns
stock %>% update_tsibble(index = trading_day) %>% ACF(rtn^2) %>% 
  autoplot()+
  ggtitle("ACF for squared log returns")+
  theme(legend.position="none", axis.text=element_text(size=6),
        axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 6))

  #PACF for squared log returns
stock %>% update_tsibble(index = trading_day) %>% PACF(rtn^2) %>%
  autoplot()+
  ggtitle("PACF for squared log returns")+
  theme(legend.position="none", axis.text=element_text(size=6),
        axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 6))
