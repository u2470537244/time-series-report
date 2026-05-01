rm(list = ls())

lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(paste0('package:', pkgs), character.only = T, unload = T, force = T))

library(tidyverse)
library(fpp3)
library(tidyquant)
library(kableExtra)
library(rugarch)

#ARMA orders chosen in Part II for each stock

p<- c(1,2,3,2,0,0,1,2,0,0,2,0,2,2,0,3)
q<- c(0,0,0,2,0,0,0,2,0,3,3,0,3,2,0,1)
names_stocks<- c("BTC-USD", "HOOD", "AMD","PLTR", "RKLB",
                 "IONQ", "ACHR", "NU", "SE","KWEB",
                 "GLD", "CCJ",  "CRSP", "NEE", "MELI", "SOL-USD")
num_stocks<-length(names_stocks)
pq_table <- tibble(symbol = names_stocks, p = p, q = q)

#Get prices, build log returns

stock <- tq_get(names_stocks, get = "stock.prices", from = "2020-01-01", to ="2024-12-31") %>%
  mutate(date = as_date(date)) %>%
  as_tsibble(index = date, key = symbol) %>%
  group_by_key() %>%
  mutate(rtn = difference(log(close))) %>%
  filter(!is.na(rtn)) %>%
  mutate(trading_day = row_number()) %>%
  ungroup() %>%
  left_join(pq_table, by = "symbol")

#4a
#ARCH-LM test on ARMA residuals + Box-Pierce on r^2

arch_lm_list <- list()

for (i in 1:num_stocks) {
  arch_lm_list[[i]] <- stock %>%
    filter(symbol == names_stocks[i]) %>%
    update_tsibble(index = trading_day) %>%
    model(ARMA = ARIMA(rtn ~ 1 + pdq(p[i], 0, q[i]))) %>%
    augment() %>%
    features(.resid, list(T = length, ~stat_arch_lm(., lag = 10))) %>%
    mutate(symbol = names_stocks[i],p = p[i],q = q[i], stat_arch_lm = T * stat_arch_lm,
           pval_arch_lm = 1 - pchisq(stat_arch_lm, df = 10),
           has_arch = pval_arch_lm < 0.05)
}
arch_lm_results <- bind_rows(arch_lm_list) %>%
  select(symbol, p, q, stat_arch_lm, pval_arch_lm, has_arch)
print(arch_lm_results)

#Box-Pierce on squared residuals

bp_sq_list <- list()

for (i in 1:num_stocks) {
  bp_sq_list[[i]] <- stock %>%
    filter(symbol == names_stocks[i]) %>%
    update_tsibble(index = trading_day) %>%
    model(ARMA = ARIMA(rtn ~ 1 + pdq(p[i], 0, q[i]))) %>%
    augment() %>%
    features(.resid^2, list(~box_pierce(., lag = 10, dof = max(10 - p[i] - q[i], 1)))) %>%
    mutate(symbol = names_stocks[i])
}

bp_sq_results <- bind_rows(bp_sq_list)
print(bp_sq_results)

stocks_with_arch <- arch_lm_results %>%
  filter(has_arch) %>%
  pull(symbol)

cat("Stocks with ARCH effects:\n")
print(stocks_with_arch)

#4b
# GARCH(1,1) with Normal errors

for (stk in stocks_with_arch) {
  
  rtn_pct <- stock %>%
    filter(symbol == stk) %>%
    pull(rtn) * 100
  
  idx <- which(names_stocks == stk)
  
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(p[idx], q[idx]),
                          include.mean = TRUE),
    distribution.model = "norm"
  )
  
  fit <- tryCatch(ugarchfit(data = rtn_pct, spec = spec),
                  error = function(e) NULL)
  if (is.null(fit)) { cat(stk, "- GARCH(1,1) Normal failed\n"); next }
  
  z <- fit@fit$z
  
  # ACF of standardized residuals
  print(z %>% ts() %>% as_tsibble() %>%
          ACF(lag_max = 20) %>%
          autoplot() +
          labs(title = paste(stk, "ACF of standardized residuals")))
  
  # ACF of squared standardized residuals
  print((z^2) %>% ts() %>% as_tsibble() %>%
          ACF(lag_max = 20) %>%
          autoplot() +
          labs(title = paste(stk, "ACF of squared standardized residuals (Normal)")))
  
  qqnorm(z, main = paste(stk, "Q-Q plot, GARCH(1,1) Normal"), col = "red")
  qqline(z, col = "blue")
}


#4c for std model

for (stk in stocks_with_arch) {
  IXICrtn <- stock %>%
    filter(symbol == stk) %>%
    pull(rtn)
  IXICrtn <- 100*IXICrtn
  
  idx <- which(names_stocks == stk)
  #do Garch in std(choose the same model GARCH(1,1) for t-distribution)
  spec <- rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                   garchOrder=c(1,1)),
                               mean.model=list(armaOrder=c(p[idx],q[idx]), 
                                               include.mean=TRUE),
                               distribution.model = "std")
  fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)

  print(fit@fit$z %>% ts() %>% as_tsibble() %>% 
    ACF(lag_max = 20) %>% autoplot(
      title = paste(stk, "- ACF of standardized residuals (t-distribution)")
    ))

  print(fit@fit$z^2 %>% ts() %>% as_tsibble() %>% 
    ACF(lag_max = 20) %>% autoplot(
      title = paste(stk, "- ACF of squared standardized residuals (t-distribution)")
    ))
  qqnorm(fit@fit$z,main=paste(stk, "- Q-Q plot (t-distribution) for GARCH(1,1)"),col="red")
  qqline(fit@fit$z,col="blue")
}

#4d

# Helper to keep the loop short and readable.
expanding_forecast <- function(rtn_pct, spec, init = 1000) {
  N <- length(rtn_pct)
  sigma_fc <- numeric(N - init)
  rtn_fc <- numeric(N - init)
  
  for (i in init:(N - 1)) {
    fit <- tryCatch(ugarchfit(data = rtn_pct[1:i], spec = spec, solver = "hybrid"), error = function(e) NULL)
    if (is.null(fit)) {
      sigma_fc[i - init + 1] <- NA
      rtn_fc[i - init + 1] <- NA
      next
    }
    fc <- ugarchforecast(fit, n.ahead = 1)
    sigma_fc[i - init + 1] <- fc@forecast$sigmaFor
    rtn_fc[i - init + 1] <- fc@forecast$seriesFor
  }
  list(sigma = sigma_fc, rtn = rtn_fc)
}

.init <- 1000

for (stk in stocks_with_arch) {
  
  rtn_pct <- stock %>% filter(symbol == stk) %>% pull(rtn) * 100
  idx <- which(names_stocks == stk)
  
  realised <- rtn_pct[(.init + 1):length(rtn_pct)]
  
  # GARCH(1,1)
  spec11 <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(p[idx], q[idx]), include.mean = TRUE))
  
  fc11 <- expanding_forecast(rtn_pct, spec11, init = .init)
  
  print(tibble(index = seq_along(fc11$sigma),
               sigma_fc = fc11$sigma,
               abs_realised = abs(realised)) %>%
          as_tsibble(index = index) %>%
          pivot_longer(-index) %>%
          autoplot() + labs(title = paste(stk, "1-step Volatility Forecast, GARCH(1,1)")))
  
  print(tibble(index = seq_along(fc11$rtn),
               rtn_fc = fc11$rtn,
               realised = realised) %>%
          as_tsibble(index = index) %>%
          pivot_longer(-index) %>%
          autoplot() + labs(title = paste(stk, "1-step Log Return Forecast, GARCH(1,1)")))
  
  # GARCH(2,1)
  spec21 <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(2, 1)),
    mean.model = list(armaOrder = c(p[idx], q[idx]), include.mean = TRUE))
  fc21 <- expanding_forecast(rtn_pct, spec21, init = .init)
  
  print(tibble(index = seq_along(fc21$sigma),
               sigma_fc = fc21$sigma,
               abs_realised = abs(realised)) %>%
          as_tsibble(index = index) %>%
          pivot_longer(-index) %>%
          autoplot() + labs(title = paste(stk, "1-step Volatility Forecast, GARCH(2,1)")))
  
  print(tibble(index = seq_along(fc21$rtn),
               rtn_fc = fc21$rtn,
               realised = realised) %>%
          as_tsibble(index = index) %>%
          pivot_longer(-index) %>%
          autoplot() + labs(title = paste(stk, "1-step Log Return Forecast, GARCH(2,1)")))
}

#4e
#choose one stock as an example

stk <- "AMD"
rtn_pct <- stock %>% filter(symbol == stk) %>% pull(rtn) * 100
idx <- which(names_stocks == stk)
.init <- 1000

spec11 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p[idx], q[idx]), include.mean = TRUE))

spec21 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2, 1)),
  mean.model = list(armaOrder = c(p[idx], q[idx]), include.mean = TRUE))

fit11 <- ugarchfit(data = rtn_pct[1:.init], spec = spec11)
fit21 <- ugarchfit(data = rtn_pct[1:.init], spec = spec21)

fcst11 <- ugarchforecast(fit11, n.ahead = 2)
fcst21 <- ugarchforecast(fit21, n.ahead = 2)

forecast_table <- tibble(
  Model = c("GARCH(1,1)", "GARCH(1,1)", "GARCH(2,1)", "GARCH(2,1)"),
  Step = c("1-step", "2-step", "1-step", "2-step"),
  Return = c(fcst11@forecast$seriesFor[1], fcst11@forecast$seriesFor[2],
                 fcst21@forecast$seriesFor[1], fcst21@forecast$seriesFor[2]),
  Volatility = c(fcst11@forecast$sigmaFor[1], fcst11@forecast$sigmaFor[2],
                 fcst21@forecast$sigmaFor[1], fcst21@forecast$sigmaFor[2])
)

print(forecast_table %>%
        kbl(booktabs = TRUE,
            caption = paste("1- and 2-step forecasts for", stk,
                            "(GARCH(1,1) vs GARCH(2,1))")))

#4f 
#Hypothesis tests

results_4f <- tibble()

for (stk in stocks_with_arch) {
  
  rtn_pct <- stock %>% filter(symbol == stk) %>% pull(rtn) * 100
  idx <- which(names_stocks == stk)
  #mean different from zero
  
  spec_mean <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(p[idx], q[idx]), include.mean = TRUE))
  
  fit_mean <- tryCatch(ugarchfit(data = rtn_pct, spec = spec_mean), error = function(e) NULL)
  
  if (is.null(fit_mean)) next
  mu_pvalue <- fit_mean@fit$matcoef["mu", 4]
  mu_significant <- mu_pvalue < 0.05
  
  #leverage effect via eGARCH
  spec_eg <- ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(p[idx], q[idx]), include.mean = TRUE))
  
  fit_eg <- tryCatch(ugarchfit(data = rtn_pct, spec = spec_eg), error = function(e) NULL)
  
  if (is.null(fit_eg) ||
      !("gamma1" %in% rownames(fit_eg@fit$matcoef))) {
    # eGARCH did not converge or gamma1 missing - record NA and move on
    gamma1_p <- NA_real_
    gamma1_val <- NA_real_
    leverage <- NA
  } else {
    gamma1_p <- fit_eg@fit$matcoef["gamma1", 4]
    gamma1_val <- fit_eg@fit$matcoef["gamma1", 1]
    leverage <- (gamma1_p < 0.05) & (gamma1_val < 0)
  }
  
  #risk premia via GARCH-in-mean
  spec_m <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(p[idx], q[idx]), include.mean = TRUE, archm = TRUE))
  
  fit_m <- tryCatch(ugarchfit(data = rtn_pct, spec = spec_m), error = function(e) NULL)
  if (is.null(fit_m) ||
      !("archm" %in% rownames(fit_m@fit$matcoef))) {
    archm_p <- NA_real_
    risk_premia <- NA
  } else {
    archm_p <- fit_m@fit$matcoef["archm", 4]
    risk_premia <- archm_p < 0.05
  }
  
  results_4f <- bind_rows(results_4f, tibble(
    Stock = stk,
    Mean_p = round(mu_pvalue, 4),
    Mean_diff_zero = mu_significant,
    Leverage_p = round(gamma1_p,   4),
    Leverage_val = round(gamma1_val, 4),
    has_Leverage = leverage,
    RiskPremia_p = round(archm_p, 4),
    has_RiskPremia = risk_premia
  ))
}

print(results_4f %>%
        kbl(booktabs = TRUE,
            caption = "Mean, Leverage, and Risk Premia tests"))