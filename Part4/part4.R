rm(list = ls())

lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(paste0('package:', pkgs), character.only = T, unload = T, force = T))

library(tidyverse)
library(fpp3)
library(tidyquant)
#ARMA data in part 2
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

stock %>% select(symbol, p, q, rtn) %>% head()
#these are data from part 2

pq_table <- tibble(symbol = names_stocks, p = p, q = q)
stock <- stock %>% left_join(pq_table, by = "symbol")

stock %>% select(symbol,date,close,rtn)
#4a
#the residuals and test for the presence of ARCH effects
result1 <- list()

for (i in 1:num_stocks){
  result1[[i]] <- stock %>% 
    filter(symbol == names_stocks[i]) %>%
    update_tsibble(index = trading_day) %>% 
    model(ARMA = ARIMA(rtn ~ 1 + pdq(p[i], 0, q[i]))) %>% #see which ARMA model in part 2
    augment() %>% 
    features(.resid, list(T = length, ~stat_arch_lm(., lag = 10))) %>% 
    mutate(p=p[i],q=q[i],
           stat_arch_lm = T * stat_arch_lm,
           pval_arch_lm = 1 - pchisq(stat_arch_lm, df = 10),
           has_arch = pval_arch_lm < 0.05,
           symbol = names_stocks[i])
  #print(result1[[i]])
}
arch_result1 <- bind_rows(result1)
print(arch_result1)

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

#4b
arch_resultB <- arch_result1 %>%
  select(symbol, p, q, stat_arch_lm, pval_arch_lm, has_arch)
print(arch_resultB)

stocks_with_arch <- arch_resultB %>%
  filter(has_arch == TRUE) %>%
  pull(symbol)
for (stk in stocks_with_arch) {
  IXICrtn <- stock %>%
    filter(symbol == stk) %>%
    pull(rtn)
  IXICrtn <- 100*IXICrtn
  
  #find id
  idx <- which(names_stocks == stk)
  
  #do Garch in norm
  spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                   garchOrder=c(1,0)),
                               mean.model=list(armaOrder=c(p[idx],q[idx]), 
                                               include.mean=TRUE))
  
  rugarch::ugarchfit(data = IXICrtn, spec = spec)
  
  spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                   garchOrder=c(1,1)),
                               mean.model=list(armaOrder=c(p[idx],q[idx]), 
                                               include.mean=TRUE))
  
  rugarch::ugarchfit(data = IXICrtn, spec = spec)
  
  spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                   garchOrder=c(2,1)),
                               mean.model=list(armaOrder=c(p[idx],q[idx]), 
                                               include.mean=TRUE))
  rugarch::ugarchfit(data = IXICrtn, spec = spec)
  #diagnostic check
  #Correlogram of standardized residuals in R
  fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)
  fit@fit$z %>% ts() %>% as_tsibble() %>% 
    ACF(lag_max = 20) %>% autoplot()
  #Correlogram of squared standardized residuals
  fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)
  fit@fit$z^2 %>% ts() %>% as_tsibble() %>% 
    ACF(lag_max = 20) %>% autoplot()
}

#4c for std model
for (stk in stocks_with_arch) {
  IXICrtn <- stock %>%
    filter(symbol == stk) %>%
    pull(rtn)
  IXICrtn <- 100*IXICrtn
  
  #find id
  idx <- which(names_stocks == stk)
  
  #do Garch in std
  spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                   garchOrder=c(1,0)),
                               mean.model=list(armaOrder=c(p[idx],q[idx]), 
                                               include.mean=TRUE),
                               distribution.model = "std")
  
  rugarch::ugarchfit(data = IXICrtn, spec = spec)
  fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)

  qqnorm(fit@fit$z,main="",col="red")
  qqline(fit@fit$z,col="blue")
  
  spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                   garchOrder=c(1,1)),
                               mean.model=list(armaOrder=c(p[idx],q[idx]), 
                                               include.mean=TRUE),
                               distribution.model = "std")
  
  rugarch::ugarchfit(data = IXICrtn, spec = spec)
  fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)

  qqnorm(fit@fit$z,main="",col="red")
  qqline(fit@fit$z,col="blue")
  
  spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                   garchOrder=c(2,1)),
                               mean.model=list(armaOrder=c(p[idx],q[idx]), 
                                               include.mean=TRUE),
                               distribution.model = "std")
  rugarch::ugarchfit(data = IXICrtn, spec = spec)
  fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)

  qqnorm(fit@fit$z,main="",col="red")
  qqline(fit@fit$z,col="blue")
}
