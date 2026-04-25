rm(list = ls())

lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(paste0('package:', pkgs), character.only = T, unload = T, force = T))

library(tidyverse)
library(fpp3)
library(tidyquant)
library(kableExtra)
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
#create directory to save pictures
if(!dir.exists("4b")) {
  dir.create("4b")
}

for (stk in stocks_with_arch) {
  IXICrtn <- stock %>%
    filter(symbol == stk) %>%
    pull(rtn)
  IXICrtn <- 100*IXICrtn
  
  #find id
  idx <- which(names_stocks == stk)
  
  #do Garch in norm(remember to state choosing GARCH(1,1) model)
  spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                   garchOrder=c(1,1)),
                               mean.model=list(armaOrder=c(p[idx],q[idx]), 
                                               include.mean=TRUE))
  
  rugarch::ugarchfit(data = IXICrtn, spec = spec)
  
  fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)
  #diagnostic check
  #Correlogram of standardized residuals (ACF for fit)
  png(filename = paste0("4b/", stk, "_ACF.png"), width = 800, height = 600)
  print(fit@fit$z %>% ts() %>% as_tsibble() %>% 
          ACF(lag_max = 20) %>% autoplot(
            title = paste(stk, "- ACF of standardized residuals")
          ))
  dev.off()
  #Correlogram of squared standardized residuals (ACF for fit^2)
  png(filename = paste0("4b/", stk, "_ACF_sq.png"), width = 800, height = 600)
  print(fit@fit$z^2 %>% ts() %>% as_tsibble() %>% 
          ACF(lag_max = 20) %>% autoplot(
            title = paste(stk, "- ACF of squared standardized residuals")
          ))
  dev.off()
  #QQ plot for fit
  png(filename = paste0("4b/", stk, "_QQ.png"), width = 800, height = 600)
  qqnorm(fit@fit$z,main=paste(stk, "- Q-Q plot (normal distribution) for GARCH(1,1)"),col="red")
  qqline(fit@fit$z,col="blue")
  dev.off()
}

#4c for std model
#create directory to save pictures
if(!dir.exists("4c")) {
  dir.create("4c")
}

for (stk in stocks_with_arch) {
  IXICrtn <- stock %>%
    filter(symbol == stk) %>%
    pull(rtn)
  IXICrtn <- 100*IXICrtn
  
  #find id
  idx <- which(names_stocks == stk)
  
  #do Garch in std(choose the same model GARCH(1,1) for t-distribution)
  spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                   garchOrder=c(1,1)),
                               mean.model=list(armaOrder=c(p[idx],q[idx]), 
                                               include.mean=TRUE),
                               distribution.model = "std")
  
  
  fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)
  #Correlogram of standardized residuals (ACF for fit)
  png(filename = paste0("4c/", stk, "_ACF_t.png"), width = 800, height = 600)
  print(fit@fit$z %>% ts() %>% as_tsibble() %>% 
          ACF(lag_max = 20) %>% autoplot(
            title = paste(stk, "- ACF of standardized residuals (t-distribution)")
          ))
  dev.off()
  #Correlogram of squared standardized residuals (ACF for fit^2)
  png(filename = paste0("4c/", stk, "_ACF_sq_t.png"), width = 800, height = 600)
  print(fit@fit$z^2 %>% ts() %>% as_tsibble() %>% 
          ACF(lag_max = 20) %>% autoplot(
            title = paste(stk, "- ACF of squared standardized residuals (t-distribution)")
          ))
  dev.off()
  #QQ plot for fit
  png(filename = paste0("4c/", stk, "_ACF_QQ_t.png"), width = 800, height = 600)
  qqnorm(fit@fit$z,main=paste(stk, "- Q-Q plot (t-distribution) for GARCH(1,1)"),col="red")
  qqline(fit@fit$z,col="blue")
  dev.off()
}

#4d
if(!dir.exists("4d")) {
  dir.create("4d")
}
for(stk in stocks_with_arch){
  
  #preparation
  IXICrtn <- stock %>%
    filter(symbol == stk) %>%
    pull(rtn)
  IXICrtn <- 100*IXICrtn
  
  #find id
  idx <- which(names_stocks == stk)
  
  spec <- rugarch::ugarchspec(variance.model=list(model="sGARCH",
                                                  garchOrder=c(1,1)),
                              mean.model=list(armaOrder=c(p[idx],q[idx]),
                                              include.mean=TRUE))
  fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)
  rugarch::ugarchforecast(fit,n.ahead = 1)
  rec_forc <- c()
  rtn_forc <- c()
  #change the number of a training set initial for how many days
  #with volatility and log return
  .init = 1000
  for (i in .init:(length(IXICrtn)-1)){
    fit <- rugarch::ugarchfit(data = IXICrtn[1:i], spec = spec)
    forc <- rugarch::ugarchforecast(fit,n.ahead = 1)
    rec_forc[i-.init+1] <- forc@forecast$sigmaFor
    rtn_forc[i-.init+1] <- forc@forecast$seriesFor
  }
  png(filename = paste0("4d/", stk, "_volatility_g11.png"), width = 800, height = 600)
  
  #generate forecasts for volatility
  print(tibble(index = 1:(length(rec_forc)),
               rec_forc = rec_forc,
               abs_IXICrtn = abs(IXICrtn[(.init+1):length(IXICrtn)])) %>%
          as_tsibble(index = index) %>%
          pivot_longer(-index) %>%
          autoplot()+
          labs(title = paste(stk, "- 1-step Forecast for Volatility,Garch(1,1)"))
  )
  dev.off()
  png(filename = paste0("4d/", stk, "_rtn_g11.png"), width = 800, height = 600)
  
  #generate forecasts for log returns
  print(tibble(index = 1:(length(rtn_forc)),
               rtn_forc = rtn_forc,
               IXICrtn = IXICrtn[(.init+1):length(IXICrtn)]) %>%
          as_tsibble(index = index) %>%
          pivot_longer(-index) %>%
          autoplot()+
          labs(title = paste(stk, "- 1-step Forecast for Log Returns,Garch(1,1)"))
  )
  dev.off()
  
  
  #for Garch(2,1) 1 step
  spec <- rugarch::ugarchspec(variance.model=list(model="sGARCH",
                                                  garchOrder=c(2,1)),
                              mean.model=list(armaOrder=c(p[idx],q[idx]),
                                              include.mean=TRUE))
  fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)
  rugarch::ugarchforecast(fit,n.ahead = 1)
  rec_forc <- c()
  rtn_forc <- c()
  for (i in .init:(length(IXICrtn)-1)){
    fit <- rugarch::ugarchfit(data = IXICrtn[1:i], spec = spec)
    forc <- rugarch::ugarchforecast(fit,n.ahead = 1)
    rec_forc[i-.init+1] <- forc@forecast$sigmaFor
    rtn_forc[i-.init+1] <- forc@forecast$seriesFor
  }
  #generate forecasts for volatility
  png(filename = paste0("4d/", stk, "_volatility_g21.png"), width = 800, height = 600)
  
  print(tibble(index = 1:(length(rec_forc)),
               rec_forc = rec_forc,
               abs_IXICrtn = abs(IXICrtn[(.init+1):length(IXICrtn)])) %>%
          as_tsibble(index = index) %>%
          pivot_longer(-index) %>%
          autoplot()+
          labs(title = paste(stk, "- 1-step Forecast for Volatility,Garch(2,1)"))
  )
  dev.off()
  #generate forecasts for log returns
  png(filename = paste0("4d/", stk, "_rtn_g21.png"), width = 800, height = 600)
  
  print(tibble(index = 1:(length(rtn_forc)),
               rtn_forc = rtn_forc,
               IXICrtn = IXICrtn[(.init+1):length(IXICrtn)]) %>%
          as_tsibble(index = index) %>%
          pivot_longer(-index) %>%
          autoplot()+
          labs(title = paste(stk, "- 1-step Forecast for Log Returns,Garch(2,1)"))
  )
  dev.off()
}

#4e
#choose one stock as an example
stk="AMD"
#preparation
IXICrtn <- stock %>%
  filter(symbol == stk) %>%
  pull(rtn)
IXICrtn <- 100*IXICrtn
if(!dir.exists("4e")) {
  dir.create("4e")
}
#find id
idx <- which(names_stocks == stk)

#do prediction in two models for 1 or 2 steps
.init = 1000

spec11 <- rugarch::ugarchspec(variance.model=list(model="sGARCH",
                                                  garchOrder=c(1,1)),
                              mean.model=list(armaOrder=c(p[idx],q[idx]),
                                              include.mean=TRUE))
fit11 <- rugarch::ugarchfit(data = IXICrtn[1:.init], spec = spec11)
spec21 <- rugarch::ugarchspec(variance.model=list(model="sGARCH",
                                                  garchOrder=c(2,1)),
                              mean.model=list(armaOrder=c(p[idx],q[idx]),
                                              include.mean=TRUE))
fit21 <- rugarch::ugarchfit(data = IXICrtn[1:.init], spec = spec21)

step2_ga11 <- rugarch::ugarchforecast(fit11,n.ahead = 2)
step2_ga21 <- rugarch::ugarchforecast(fit21,n.ahead = 2)
results_4e<-tibble(
  Model = c("GARCH(1,1)","GARCH(1,1)", "GARCH(2,1)","GARCH(2,1)"),
  Step_num = c("1-step", "2-step","1-step", "2-step"),
  Return = c(step2_ga11@forecast$seriesFor[1], step2_ga11@forecast$seriesFor[2],
             step2_ga21@forecast$seriesFor[1], step2_ga21@forecast$seriesFor[2]),
  Volatility = c(step2_ga11@forecast$sigmaFor[1], step2_ga11@forecast$sigmaFor[2],
                 step2_ga21@forecast$sigmaFor[1], step2_ga21@forecast$sigmaFor[2])
)
table_4e<-results_4e%>%
  kbl(booktabs=TRUE,
      caption = paste("1-step and 2-step forecasts for ",stk," (GARCH(1,1) vs GARCH(2,1))")
  )

#save file
write_csv(results_4e, paste0("4e/", stk, "_prediction_table.csv"))
print(table_4e)
#4f
if(!dir.exists("4f")) {
  dir.create("4f")
}
results_4f <- tibble()
for (stk in stocks_with_arch){
  IXICrtn <- stock %>%
    filter(symbol == stk) %>%
    pull(rtn)
  IXICrtn <- 100*IXICrtn
  
  #find id
  idx <- which(names_stocks == stk)
  
  #check if it's different from zero
  spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                   garchOrder=c(1,1)),
                               mean.model=list(armaOrder=c(p[idx],q[idx]), 
                                               include.mean=TRUE))
  
  fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)
  coef_table <- fit@fit$matcoef
  mu_pvalue <- coef_table["mu", 4]
  statistically_difference <- mu_pvalue < 0.05
  
  #check leverage effects
  spec_e <-  rugarch::ugarchspec(variance.model=list(model="eGARCH", 
                                                     garchOrder=c(1,1)),
                                 mean.model=list(armaOrder=c(p[idx],q[idx]), 
                                                 include.mean=TRUE))
  
  fit_e <- rugarch::ugarchfit(data = IXICrtn, spec = spec_e)
  gamma1_p <- fit_e@fit$matcoef["gamma1", 4]
  if(is.null(gamma1_p))
    next  #skip which doesnot have gamma1
  gamma1_val <- fit_e@fit$matcoef["gamma1", 1]
  leverage_effect <- gamma1_p < 0.05 & gamma1_val < 0
  
  #check risk premia
  spec_m <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                     garchOrder=c(1,1)),
                                 mean.model=list(armaOrder=c(p[idx],q[idx]),
                                                 include.mean=TRUE,
                                                 archm=TRUE))
  
  fit_m <- rugarch::ugarchfit(data = IXICrtn, spec = spec_m)
  archm_p <- fit_m@fit$matcoef["archm", 4]
  has_risk_premia <- archm_p < 0.05
  #build table
  results_4f <- bind_rows(results_4f, tibble(
    Stock = stk,
    Mean_p = round(mu_pvalue, 4),
    Stat_diff = statistically_difference,
    
    Leverage_p = round(gamma1_p, 4),
    Leverage_value = round(gamma1_val, 4),
    has_Leverage = leverage_effect,
    
    RiskPremia_p = round(archm_p, 4),
    has_RiskPremia = has_risk_premia,
  ))
  
}

table_4f<-results_4f %>%
        kbl(caption = "Table 4f: Hypothesis Tests for Mean, Leverage, and Risk Premia")
#save file
write_csv(results_4f, "4f/tests.csv")
print(results_4f)
#only have 7 rows because other series don't converge in e-Garch,