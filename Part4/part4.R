rm(list = ls())

lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(paste0('package:', pkgs), character.only = T, unload = T, force = T))

library(tidyverse)
library(fpp3)
library(tidyquant)
#these are data from part 2
p<- c(1,2,3,2,0,0,1,2,0,0,2,0,2,2,0,3)
q<- c(0,0,0,2,0,0,0,2,0,3,3,0,3,2,0,1)
names_stocks<- c("BTC-USD", "HOOD", "AMD",  "PLTR", "RKLB",
           "IONQ",    "ACHR", "NU",   "SE",   "KWEB",
           "GLD",     "CCJ",  "CRSP", "NEE",  "MELI", "SOL-USD")
num_stocks<-length(names_stocks)
#define stocks
stock <- tidyquant::tq_get(
  c("BTC-USD", "HOOD", "AMD",  "PLTR", "RKLB",
    "IONQ",    "ACHR", "NU",   "SE",   "KWEB",
    "GLD",     "CCJ",  "CRSP", "NEE",  "MELI", "SOL-USD"),
  get  = "stock.prices",
  from = "2020-01-01",
  to   = "2024-12-31"
) %>%
  mutate(date = as_date(date)) %>%
  as_tsibble(index = date, key = symbol) %>%
  group_by_key() %>%                  
  mutate(rtn = difference(log(close))) %>%
  filter(!is.na(rtn)) %>%
  mutate(trading_day = row_number())
stock %>% select(symbol,date,close,rtn)
#4a
#the residuals and test for the presence of ARCH effects
result1 <- list()

for (i in 1:num_stocks){
  result1[[i]] <- stock %>% 
    filter(symbol == names_stocks[i]) %>%
    update_tsibble(index = trading_day) %>% 
    model(ARMA = ARIMA(rtn ~ 1 + pdq(p[i], 0, q[i]))) %>% 
    augment() %>% 
    features(.resid, list(T = length, ~stat_arch_lm(., lag = 10))) %>% 
    mutate(p=p[i],q=q[i],
           stat_arch_lm = T * stat_arch_lm,
           pval_arch_lm = 1 - pchisq(stat_arch_lm, df = 10)
           )
  #print(result1[[i]])
}
arch_result <- bind_rows(result1)
print(arch_result)

#the squared residuals
result2 <- list()
for (i in 1:num_stocks){
  result2[[i]] <- stock %>% 
    filter(symbol == names_stocks[i]) %>%
    update_tsibble(index = trading_day) %>% 
    model(ARMA = ARIMA(rtn ~ 1 + pdq(p[i], 0, q[i]))) %>% 
    augment() %>% 
    features(.resid^2, list(~box_pierce(.,lag = 10, dof = max(10-p[i]-q[i],1)))) %>% 
    mutate(symbol = names_stocks[i])
  #print(result2[[i]])
}
arch_result2 <- bind_rows(result2)
print(arch_result2)