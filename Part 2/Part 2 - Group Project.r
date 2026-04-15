library(tidyverse)
library(tidyquant)
library(tsibble)    
library(fabletools)   
library(sandwich)
library(lmtest)
library(forecast)
library(lubridate)
library(dplyr)   

# In order to start we need to import the data and compute the log returns

tickers <- c(
  "BTC-USD", "HOOD", "AMD",  "PLTR", "RKLB",
  "IONQ",    "ACHR", "NU",   "SE",   "KWEB",
  "GLD",     "CCJ",  "CRSP", "NEE",  "MELI", "SOL-USD"
  )

prices <- tq_get(tickers, from = "2015-01-01", to = "2024-12-31")

returns <- prices %>%
  group_by(symbol) %>%
  arrange(date) %>%
  transmute(
    date,
    adjusted,
    rtn = log(adjusted) - log(lag(adjusted))
  ) %>%
  na.omit() %>%
  ungroup()

#Within the basket, not all the stocks have the same length of returns:
#For example, Hood had its IPO in 2021, or cryptos also trade during the weekend

# DESCRIPTIVE STATISTICS

desc_stats <- returns %>%
  group_by(symbol) %>%
  summarise(
    Start     = min(date),
    End       = max(date),
    N         = n(),
    Mean      = round(mean(rtn) * 252, 4),    
    Std_Dev   = round(sd(rtn) * sqrt(252), 4),
    Min       = round(min(rtn), 4),
    Max       = round(max(rtn), 4),
    Skewness  = round(skewness(rtn), 3),
    Kurtosis  = round(kurtosis(rtn), 3)
  )

print(desc_stats)

# PART (a) – ARMA ANALYSIS

tickers_list <- unique(returns$symbol)

results <- map_dfr(tickers_list, function(tk) {
  
  rtn <- returns %>%
    filter(symbol == tk) %>%
    pull(rtn) %>%
    ts()
  
  # Hyndman-Khandakar algorithm, We set d to 0 since returns are already 
  # stationary, unlike prices.
  auto_fit <- auto.arima(rtn, d = 0, stepwise = FALSE, approximation = FALSE)
  
  # Two simpler benchmarks
  ar1 <- Arima(rtn, order = c(1, 0, 0))
  ma1 <- Arima(rtn, order = c(0, 0, 1))
  
  # White noise check on residuals
  lb <- Box.test(residuals(auto_fit), lag = 10, type = "Ljung-Box")
  
  data.frame(
    Ticker     = tk,
    Best_Model = paste0("ARMA(", auto_fit$arma[1], ",", auto_fit$arma[2], ")"),
    AIC_Auto   = round(AIC(auto_fit), 2),
    AIC_AR1    = round(AIC(ar1), 2),
    AIC_MA1    = round(AIC(ma1), 2),
    LB_pvalue  = round(lb$p.value, 4),
    WhiteNoise = ifelse(lb$p.value > 0.05, "Yes", "No")
  )
})

print(results)

# PART (b) – MARTINGALE TEST

# For each ticker, Wald test: rtn ~ 1 vs rtn ~ L1 + L2
martingale_results <- map_dfr(tickers_list, function(tk) {
  
  df <- returns %>%
    filter(symbol == tk) %>%
    mutate(L1 = lag(rtn, 1), L2 = lag(rtn, 2)) %>%
    na.omit()
  
  lm_r <- lm(rtn ~ 1,       data = df)
  lm_u <- lm(rtn ~ L1 + L2, data = df)
  
  wt <- waldtest(lm_r, lm_u, vcov = vcovHC)
  
  data.frame(
    Ticker  = tk,
    F_stat  = round(wt$F[2], 4),
    p_value = round(wt$`Pr(>F)`[2], 4),
    Martingale = ifelse(wt$`Pr(>F)`[2] > 0.05, "Yes", "No")
  )
})

print(martingale_results)

# PART (c) – RW1: COWLES-JONES + RUNS TEST

results_rw1 <- map_dfr(tickers_list, function(tk) {
  
  tk_data <- returns %>% filter(symbol == tk)
  T_obs   <- nrow(tk_data)
  pi_hat  <- mean(tk_data$rtn > 0)
  
# Cowles-Jones- We check flags whether the current sign equals the previous one
  tk_test <- tk_data %>%
    mutate(sign   = ifelse(rtn > 0, 1, 0),
           is_seq = ifelse(sign == lag(sign), 1, 0)) %>%
    na.omit()
  # counts the number of sequences/reversals and the CJ ratio
  n_s    <- sum(tk_test$is_seq)
  n_r    <- T_obs - n_s
  cj_hat <- n_s / n_r
  
  # theoretical probability of a sequence
  pi_s        <- pi_hat^2 + (1 - pi_hat)^2
  expected_cj <- pi_s / (1 - pi_s)
  
  #analytical variance of the CJ ratio under the null
  #accounts for the fact that π^\hat is estimated from the data
  term1       <- pi_s * (1 - pi_s)
  term2       <- 2 * (pi_hat^3 + (1 - pi_hat)^3 - pi_s^2)
  denom       <- T_obs * (1 - pi_s)^4
  var_cj      <- (term1 + term2) / denom
  
  #Builds the z-statistic
  cj_z        <- (cj_hat - expected_cj) / sqrt(var_cj)
  cj_p        <- 2 * (1 - pnorm(abs(cj_z)))
  
  # Runs Test
  runs <- tk_data %>%
    mutate(sign = if_else(rtn > 0, 1, 0)) %>%
    summarise(
      #A run is an unbroken sequence of the same sign
      N_runs     = sum(sign != lag(sign, default = first(sign))) + 1,
      
      # The expected number/theoretical variance of runs under 
      # the null of independence
      E_N_runs   = 2 * T_obs * pi_hat * (1 - pi_hat),
      Var_N_runs = 4 * T_obs * pi_hat * (1 - pi_hat) * (1 - 3 * pi_hat * (1 - pi_hat)),
      z_stat     = (N_runs + 0.5 - E_N_runs) / sqrt(Var_N_runs),
      p_value    = 2 * (1 - pnorm(abs(z_stat)))
    )
  
  # Output
  data.frame(
    Ticker    = tk,
    CJ_ratio  = round(cj_hat, 4),
    CJ_pvalue = round(cj_p, 4),
    CJ_RW1    = ifelse(cj_p  > 0.05, "Fail to Reject", "Reject"),
    RT_pvalue = round(runs$p_value, 4),
    RT_RW1    = ifelse(runs$p_value > 0.05, "Fail to Reject", "Reject")
  )
})

print(results_rw1)

# PART (d) 

# Day-of-the-Week Effect
# Same methodology as martingale test 
# Restricted:   rtn ~ 1
# Unrestricted: rtn ~ weekday dummies
# Wald test with robust standard errors (vcovHC)

dow_results <- map_dfr(tickers_list, function(tk) {
  
  df <- returns %>%
    filter(symbol == tk) %>%
    mutate(weekday = wday(date, label = TRUE, week_start = 1))

  lm_r <- lm(rtn ~ 1,       data = df)   
  lm_u <- lm(rtn ~ weekday, data = df)  
  
  wt <- waldtest(lm_r, lm_u, vcov = vcovHC)
  
  data.frame(
    Ticker  = tk,
    F_stat  = round(wt$F[2], 4),
    p_value = round(wt$`Pr(>F)`[2], 4),
    DOW     = ifelse(wt$`Pr(>F)`[2] < 0.05, "Effect Present", "No Effect")
  )
})

print(dow_results)


# January Effect
# Restricted:   rtn ~ 1
# Unrestricted: rtn ~ is_january dummy
# Wald test with robust standard errors

jan_results <- map_dfr(tickers_list, function(tk) {
  
  df <- returns %>%
    filter(symbol == tk) %>%
    mutate(is_jan = factor(ifelse(month(date) == 1, "Jan", "Other")))
  
  lm_r <- lm(rtn ~ 1,      data = df)
  lm_u <- lm(rtn ~ is_jan, data = df)
  
  wt <- waldtest(lm_r, lm_u, vcov = vcovHC)
  
  data.frame(
    Ticker    = tk,
    Mean_Jan  = round(mean(df$rtn[df$is_jan == "Jan"]) * 100, 4),
    Mean_Rest = round(mean(df$rtn[df$is_jan == "Other"]) * 100, 4),
    F_stat    = round(wt$F[2], 4),
    p_value   = round(wt$`Pr(>F)`[2], 4),
    January   = ifelse(wt$`Pr(>F)`[2] < 0.05, "Effect Present", "No Effect")
  )
})

print(jan_results)
# WINNER-LOSER EFFECT
# To test the Winner-Loser effect we will enlarge the dataset chosing 
# among different sectors:

#Every line will be a different sector
wl_tickers <- c(
  # Finance & Banking
  "AXP", "BAC", "BLK", "C", "GS", "JPM", "MS", "WFC",
  # Healthcare & Pharma
  "ABT", "BMY", "CVS", "JNJ", "LLY", "MRK", "PFE", "UNH",
  # Energy & Oil
  "BP", "COP", "CVX", "EOG", "PSX", "SLB", "VLO", "XOM",
  # Consumer & Retail
  "COST", "KO", "MCD", "NKE", "PG", "SBUX", "TGT", "WMT",
  # Industrials
  "BA", "CAT", "DE", "GE", "HON", "LMT", "MMM", "RTX",
  # Real Estate
  "AMT", "O", "PLD", "SPG",
  # Materials & Mining
  "AA", "BHP", "FCX", "NEM",
  # Utilities
  "DUK", "SO",
  # Crypto
  "BTC-USD", "SOL-USD",
  # Tech & AI
  "AMD", "IONQ", "PLTR", "RKLB",
  # Fintech & Neobanks
  "ACHR", "HOOD", "NU",
  # Emerging Markets & ETFs
  "KWEB", "MELI", "SE",
  # Other
  "CCJ", "CRSP", "GLD", "NEE"
)

# Parameters
# Top 10 Winners and Bottom 10 Losers
num <- 10

# 12 Quarters (3 years) formation + 12 quarters test
per <- 12        

tq_get(wl_tickers, from = "2015-01-01", to = "2024-12-31") %>%
  as_tsibble(key = symbol, index = date) %>%
  group_by_key() %>%
  mutate(ret = difference(log(close))) %>%
  drop_na() %>%
  index_by(Quarter = tsibble::yearquarter(date)) %>%
  dplyr::summarise(ret = sum(ret)) -> ret_raw

# S&P 500 as market benchmark
tq_get("^GSPC", from = "2015-01-01", to = "2024-12-31") %>%
  as_tsibble(index = date) %>%
  mutate(ret_mkt = difference(log(close))) %>%
  drop_na() %>%
  index_by(Quarter = tsibble::yearquarter(date)) %>%
  dplyr::summarise(ret_mkt = sum(ret_mkt)) -> ret_mkt

#Calculate excess returns
ret_raw %>%
  left_join(ret_mkt, by = "Quarter") %>%
  mutate(excess_ret = ret - ret_mkt) -> excess_ret

# Label formation and test periods
excess_ret %>%
  group_by_key() %>%
  mutate(
    q_idx       = row_number(),
    block_id    = ceiling(q_idx / per),
    pair_id     = ceiling(block_id / 2),
    period_type = ifelse(block_id %% 2 == 1, "Formation", "Test")
  ) %>%
  ungroup() -> labeled_ts

# Rank stocks in formation period → Winner / Loser
labeled_ts %>%
  filter(period_type == "Formation") %>%
  update_tsibble(key = c(symbol, pair_id)) %>%
  features(ret, list(total_form_ret = sum)) %>%
  group_by(pair_id) %>%
  mutate(rank = min_rank(total_form_ret)) %>%
  mutate(category = case_when(
    rank <= num                    ~ "Loser",
    rank > (max(rank) - num)       ~ "Winner",
    TRUE                           ~ "Neither Winner nor Loser"
  )) %>%
  filter(category != "Neither Winner nor Loser") %>%
  select(pair_id, symbol, category) -> portfolios

# Calculate ACAR in test period
labeled_ts %>%
  filter(period_type == "Test") %>%
  left_join(portfolios, by = c("pair_id", "symbol")) %>%
  filter(!is.na(category)) %>%
  group_by(symbol, pair_id) %>%
  mutate(time_step = row_number()) %>%
  update_tsibble(index = time_step, key = c(symbol, pair_id, category)) %>%
  group_by(category) %>%
  summarise(mean_excess = mean(excess_ret)) %>%
  mutate(ACAR = cumsum(mean_excess)) -> final_results


final_results %>%
  bind_rows(
    tibble(category = c("Winner", "Loser"),
           time_step = 0, mean_excess = 0, ACAR = 0)
  ) %>%
  ggplot(aes(x = time_step, y = ACAR, color = category)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  scale_color_manual(values = c("Winner" = "royalblue", "Loser" = "firebrick")) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_x_continuous(breaks = 0:per) +
  labs(
    title    = "Winner-Loser Effect",
    subtitle = "Analysis based on 3-year Formation and 3-year Test windows",
    x        = "Quarters After Portfolio Formation",
    y        = "Average Cumulative Abnormal Return (ACAR)",
    color    = "Portfolio"
  ) +
  theme_minimal()

# PART (e) – RW3: LJUNG-BOX ON RAW RETURNS

rw3_results <- map_dfr(tickers_list, function(tk) {
  rtn <- returns %>% filter(symbol == tk) %>% pull(rtn)
  lb  <- Box.test(rtn, lag = 10, type = "Ljung-Box")
  
  data.frame(
    Ticker   = tk,
    ACF_lag1 = round(acf(rtn, lag.max = 1, plot = FALSE)$acf[2], 4),
    LB_stat  = round(lb$statistic, 4),
    p_value  = round(lb$p.value, 4),
    RW3      = ifelse(lb$p.value > 0.05, "Fail to Reject", "Reject")
  )
})

print(rw3_results)