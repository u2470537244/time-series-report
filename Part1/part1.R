knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE, cache = FALSE,
  dev.args = list(pointsize = 11), fig.height = 3.5, fig.width = 7, fig.align = 'center'
)
options(digits = 3, width = 60)

rm(list = ls())

lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(paste0('package:', pkgs), character.only = T, unload = T, force = T))

library(tidyverse)
library(fpp3)
library(tidyquant)
library(sandwich)
library(lmtest)
library(GGally)
library(kableExtra)

#define stocks
stock <- tidyquant::tq_get(
  c("BTC-USD","HOOD","AMD", "PLTR", "RKLB",
    "IONQ","ACHR", "NU", "SE","KWEB",
    "GLD","CCJ","CRSP", "NEE","MELI", "SOL-USD"),
  get =  "stock.prices",
  from = "2019-01-01",
  to = "2024-12-31"
) %>%
  mutate(date = as_date(date)) %>%
  as_tsibble(index = date, key = symbol) %>%
  group_by_key() %>%                  
  mutate(rtn = difference(log(close))) %>%
  filter(!is.na(rtn)) %>%
  mutate(trading_day = row_number())

stock %>% select(symbol, date, close, rtn)

#1b
#PRICES, LOG RETURNS & SUMMARY STATISTICS
stock %>% 
  autoplot(close)+facet_grid(vars(symbol), scale = "free_y")+
  theme(legend.position="none", axis.text=element_text(size=6),
        axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 6))

stock %>% 
  autoplot(rtn)+facet_grid(vars(symbol), scale = "free_y" )+
  theme(legend.position="none", axis.text=element_text(size=6),
        axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 6))

#summary statistics (mean, standard deviation, skewness, kurt)
  #mean and SD combined in one table
stock %>%
  features(rtn, list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(.,   na.rm = TRUE),
    n = ~length(.)
  )) %>%
  kbl(booktabs = TRUE)

# Skewness with test statistic

stock %>%
  features(rtn, list(skew = timeSeries::colSkewness, n = ~length(.))) %>%
  mutate(
    skew_stat = sqrt(n) * skew / sqrt(6),
    p_val_skew_stat = 2 * (1 - pnorm(abs(skew_stat)))
  ) %>%
  kbl(booktabs = TRUE)

# Kurtosis with test statistic

stock %>%
  features(rtn, list(kurt = timeSeries::colKurtosis,
                     n = ~length(.))) %>%
  mutate(
    kurt = kurt + 3,                        
    kurt_stat = sqrt(n) * (kurt - 3) / sqrt(24),
    p_val_kurt_stat = 2 * (1 - pnorm(abs(kurt_stat)))
  ) %>%
  kbl(booktabs = TRUE)

# Jarque-Bera tes
# Joint test of H0: skew = 0 AND kurt = 3
feat_normal <- function(x) {
  n <- length(x)
  kurt <- sum(((x - mean(x)) / sd(x))^4) / n
  skew <- sum(((x - mean(x)) / sd(x))^3) / n
  JB <- (n / 6) * (skew^2) + (n / 24) * ((kurt - 3)^2)
  pval_JB <- 1 - pchisq(JB, df = 2)
  c(kurt = kurt, skew = skew, JB = JB, pval_JB = pval_JB)
}

stock %>%
  features(rtn, feat_normal) %>%
  kbl(booktabs = TRUE)

#1c
  # ACF for log returns
stock %>%
  update_tsibble(index = trading_day) %>%
  ACF(rtn) %>%
  autoplot() +
  ggtitle("ACF for log returns") +
  theme(legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6))

  # PACF of log returns
stock %>%
  update_tsibble(index = trading_day) %>%
  PACF(rtn) %>%
  autoplot() +
  ggtitle("PACF for log returns") +
  theme(legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6))

  # Ljung-Box and Box-Pierce tests on log returns
stock %>%
  features(rtn, list(
    ~ljung_box(., lag = 10),
    ~box_pierce(., lag = 10)
  )) %>%
  kbl(booktabs = TRUE)
  
#1d
  #ACF for squared log returns
stock %>%
  update_tsibble(index = trading_day) %>%
  ACF(rtn^2) %>%
  autoplot() +
  ggtitle("ACF for squared log returns") +
  theme(legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6))

  #PACF for squared log returns
stock %>%
  update_tsibble(index = trading_day) %>%
  PACF(rtn^2) %>%
  autoplot() +
  ggtitle("PACF for squared log returns") +
  theme(legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6))

# Ljung-Box and Box-Pierce tests on squared log returns
stock %>%
  features(rtn^2, list(
    ~ljung_box(., lag = 10),
    ~box_pierce(., lag = 10)
  )) %>%
  kbl(booktabs = TRUE)
