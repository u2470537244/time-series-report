#Part 3 event study analysis code. Following slides and CLM chapter.
rm(list = ls())

library(tidyverse)
library(tidyquant)
library(broom)
library(scales)
library(kableExtra)
library(lubridate)

set.seed(123)

TICKERS <- c("AAPL", "JPM", "JNJ", "XOM")
MARKET <- "SPY"

SECTOR <- tibble(
  symbol = TICKERS,
  sector = c("Information Technology", "Financials", "Health Care", "Energy")
)

#Event-study parameters 
PARAMS <- list(
  L1 = 250,
  tau_lo = -20,
  tau_hi =  20
)
PARAMS$L2 <- PARAMS$tau_hi - PARAMS$tau_lo + 1

#News classification threshold
SURPRISE_THR <- 0.025


read_eps_block <- function(sym, dates, est, act) {
  tibble(symbol = sym,
         ann_date = as.Date(dates),
         eps_e = est,
         eps_a = act)
}

eps_calendar <- bind_rows(
  read_eps_block(
    "AAPL",
    c("2020-01-28","2020-04-30","2020-07-30","2020-10-29",
      "2021-01-27","2021-04-28","2021-07-27","2021-10-28",
      "2022-01-27","2022-04-28","2022-07-28","2022-10-27",
      "2023-02-02","2023-05-04","2023-08-03","2023-11-02",
      "2024-02-01","2024-05-02","2024-08-01","2024-10-31",
      "2025-01-30","2025-05-01","2025-07-31","2025-10-30"),
    c(1.14, 0.51, 0.52, 0.70, 1.41, 0.99, 1.01, 1.24,
      1.89, 1.43, 1.16, 1.27, 1.94, 1.43, 1.19, 1.39,
      2.10, 1.50, 1.35, 1.60, 2.35, 1.62, 1.42, 1.74),
    c(1.25, 0.64, 0.65, 0.73, 1.68, 1.40, 1.30, 1.24,
      2.10, 1.52, 1.20, 1.29, 1.88, 1.52, 1.26, 1.46,
      2.18, 1.53, 1.40, 1.64, 2.40, 1.65, 1.57, 1.85)
  ),
  read_eps_block(
    "JPM",
    c("2020-01-14","2020-04-14","2020-07-14","2020-10-13",
      "2021-01-14","2021-04-14","2021-07-13","2021-10-13",
      "2022-01-14","2022-04-13","2022-07-14","2022-10-14",
      "2023-01-13","2023-04-14","2023-07-14","2023-10-13",
      "2024-01-12","2024-04-12","2024-07-12","2024-10-11",
      "2025-01-15","2025-04-11","2025-07-15","2025-10-14"),
    c(2.36, 1.84, 1.04, 2.23, 2.62, 3.10, 3.21, 3.00,
      3.01, 2.69, 2.88, 2.87, 3.10, 3.42, 3.97, 3.96,
      3.32, 4.11, 4.19, 4.01, 4.09, 4.61, 4.48, 4.84),
    c(2.57, 0.78, 1.38, 2.92, 3.79, 4.50, 3.78, 3.74,
      3.33, 2.63, 2.76, 3.12, 3.57, 4.10, 4.75, 4.33,
      3.97, 4.44, 4.40, 4.37, 4.81, 5.07, 5.24, 5.07)
  ),
  read_eps_block(
    "JNJ",
    c("2020-01-22","2020-04-14","2020-07-16","2020-10-13",
      "2021-01-26","2021-04-20","2021-07-21","2021-10-19",
      "2022-01-25","2022-04-19","2022-07-19","2022-10-18",
      "2023-01-24","2023-04-18","2023-07-20","2023-10-17",
      "2024-01-23","2024-04-16","2024-07-17","2024-10-15",
      "2025-01-22","2025-04-15","2025-07-16","2025-10-14"),
    c(1.87, 2.00, 1.49, 1.98, 1.82, 2.34, 2.30, 2.36,
      2.13, 2.58, 2.54, 2.49, 2.23, 2.50, 2.62, 2.52,
      2.28, 2.64, 2.71, 2.43, 2.00, 2.59, 2.66, 2.41),
    c(1.88, 2.30, 1.67, 2.20, 1.86, 2.59, 2.48, 2.60,
      2.13, 2.67, 2.59, 2.55, 2.35, 2.68, 2.80, 2.66,
      2.29, 2.71, 2.82, 2.42, 2.04, 2.77, 2.77, 2.80)
  ),
  read_eps_block(
    "XOM",
    c("2020-01-31","2020-05-01","2020-07-31","2020-10-30",
      "2021-02-02","2021-04-30","2021-07-30","2021-10-29",
      "2022-02-01","2022-04-29","2022-07-29","2022-10-28",
      "2023-01-31","2023-04-28","2023-07-28","2023-10-27",
      "2024-02-02","2024-04-26","2024-08-02","2024-11-01",
      "2025-01-31","2025-05-02","2025-08-01","2025-10-31"),
    c( 0.43,  0.04, -0.61, -0.25,  0.02,  0.59,  0.99,  1.55,
       1.92,  2.25,  3.84,  3.74,  3.31,  2.59,  2.04,  2.36,
       2.21,  2.20,  2.01,  1.88,  1.55,  1.74,  1.51,  1.85),
    c( 0.41, -0.14, -0.70, -0.18,  0.03,  0.65,  1.10,  1.58,
       2.05,  2.07,  4.14,  4.45,  3.40,  2.83,  1.94,  2.27,
       2.48,  2.06,  2.14,  1.92,  1.67,  1.88,  1.64,  1.88)
  )
)

#Compute the surprise component and label good/neutral/bad news
classify_news <- function(eps_e, eps_a, thr) {
  surp <- ifelse(eps_e != 0,
                 (eps_a - eps_e) / abs(eps_e),
                 eps_a - eps_e)
  label <- case_when(surp >  thr ~ "Good News",
                     surp < -thr ~ "Bad News",
                     TRUE        ~ "No News")
  tibble(surprise = surp,
         news = factor(label,
                           levels = c("Good News", "No News", "Bad News")))
}

eps_calendar <- eps_calendar %>%
  bind_cols(classify_news(eps_calendar$eps_e, eps_calendar$eps_a,
                          SURPRISE_THR)) %>%
  mutate(event_id = sprintf("%s_%s", symbol, format(ann_date, "%Y%m%d")))

cat(sprintf("\nEarnings calendar: %d events across %d firms.\n",
            nrow(eps_calendar), length(TICKERS)))

news_breakdown <- eps_calendar %>%
  count(news) %>%
  mutate(share = sprintf("%.1f%%", n / sum(n) * 100))
cat("\nNews classification +/-2.5% threshold from slides\n")
print(news_breakdown)


#Compute the log returns
fetch_returns <- function(symbols, start, end) {
  tq_get(symbols, from = start, to = end) %>%
    as_tibble() %>%
    arrange(symbol, date) %>%
    group_by(symbol) %>%
    transmute(date, r = log(adjusted) - log(dplyr::lag(adjusted))) %>%
    ungroup() %>%
    drop_na(r)
}

ret_long <- fetch_returns(
  c(TICKERS, MARKET),
  start = min(eps_calendar$ann_date) - 365 * 1.5,
  end = max(eps_calendar$ann_date) + 60
)

cat(sprintf("\nPrice history: %d (symbol, day) observations from %s to %s.\n",
            nrow(ret_long),
            as.character(min(ret_long$date)),
            as.character(max(ret_long$date))))

trading_days <- sort(unique(ret_long$date))


#Map an announcement date to a tau-window of trading days
window_for <- function(ann_date, days, lo, hi) {
  i0 <- which(days >= ann_date)[1]
  if (is.na(i0)) return(NULL)
  idx <- (i0 + lo):(i0 + hi)
  idx <- idx[idx >= 1 & idx <= length(days)]
  tibble(date = days[idx], tau = idx - i0)
}

#Long panel: one row per (event, tau) goes across estimation + event windwos.
panel_window <- eps_calendar %>%
  rowwise() %>%
  mutate(panel = list(window_for(ann_date, trading_days, PARAMS$tau_lo - PARAMS$L1,
                                 PARAMS$tau_hi))) %>%
  ungroup() %>%
  unnest(panel)

#Bring in the firm's return
firm_returns <- ret_long %>%
  filter(symbol %in% TICKERS) %>%
  rename(r_firm = r)

mkt_returns <- ret_long %>%
  filter(symbol == MARKET) %>%
  select(date, r_mkt = r)

event_panel <- panel_window %>%
  left_join(firm_returns, by = c("symbol", "date")) %>%
  left_join(mkt_returns,  by = "date")


#Drop events that lack a complete estimation or event window
window_quality <- event_panel %>%
  group_by(event_id) %>%
  summarise(
    n_est = sum(between(tau, PARAMS$tau_lo - PARAMS$L1, PARAMS$tau_lo - 1) &
                !is.na(r_firm) & !is.na(r_mkt)),
    n_evt = sum(between(tau, PARAMS$tau_lo, PARAMS$tau_hi) &
                !is.na(r_firm) & !is.na(r_mkt)),
    .groups = "drop"
  )

complete_events <- window_quality %>%
  filter(n_est >= PARAMS$L1 - 5,
         n_evt >= PARAMS$L2 - 1)

event_panel <- event_panel %>% semi_join(complete_events, by = "event_id")
n_kept <- length(unique(event_panel$event_id))

#fit a market model per event
mm_one_event <- function(df) {
  est <- df %>% filter(between(tau,PARAMS$tau_lo - PARAMS$L1, PARAMS$tau_lo - 1))
  m <- lm(r_firm ~ r_mkt, data = est)
  cf <- tidy(m)
  tibble(
    a_hat = cf$estimate[1],
    b_hat = cf$estimate[2],
    sig2_e = glance(m)$sigma^2,
    mu_m = mean(est$r_mkt),
    ss_m = sum((est$r_mkt - mean(est$r_mkt))^2)
  )
}

#One row per event with the OLS fit components
mkt_model_fits <- event_panel %>%
  group_by(event_id, symbol, ann_date, news, surprise) %>%
  nest() %>%
  mutate(fit = map(data, mm_one_event)) %>%
  select(-data) %>%
  unnest(fit) %>%
  ungroup()


#Calculate the abnormal returns over the event window
ar_panel <- event_panel %>%
  filter(between(tau, PARAMS$tau_lo, PARAMS$tau_hi)) %>%
  inner_join(mkt_model_fits,
             by = c("event_id", "symbol", "ann_date", "news", "surprise")) %>%
  mutate(
    ar = r_firm - a_hat - b_hat * r_mkt,
    var_ar = sig2_e * (1 + 1/PARAMS$L1 + (r_mkt - mu_m)^2 / ss_m),
    z_ar = ar / sqrt(var_ar),
    p_ar = 2 * pt(-abs(z_ar), df = PARAMS$L1 - 2)
  ) %>%
  group_by(event_id) %>%
  arrange(tau, .by_group = TRUE) %>%
  mutate(car = cumsum(ar)) %>%
  ungroup()


#Example of an event
demo_id <- mkt_model_fits %>%
  filter(news == "Good News") %>%
  arrange(ann_date) %>%
  slice(1) %>%
  pull(event_id)

demo_event <- ar_panel %>% filter(event_id == demo_id)
demo_fit <- mkt_model_fits %>% filter(event_id == demo_id)

cat(sprintf("\nExample event: %s on %s\n",
            demo_fit$symbol, as.character(demo_fit$ann_date)))
cat(sprintf("alpha_hat = %.4f beta_hat = %.4f\n\n",
            demo_fit$a_hat, demo_fit$b_hat))

demo_event %>%
  transmute(
    date = format(date, "%b %d"),
    tau,
    ar = sprintf("%+.3f%%", ar * 100),
    se_ar = sprintf("%.3f%%",  sqrt(var_ar) * 100),
    sar = sprintf("%.2f",    z_ar),
    pval_sar = sprintf("%.3f",    p_ar),
    car = sprintf("%+.3f%%", car * 100)
  ) %>%
  print(n = Inf)

demo_summary <- demo_event %>%
  summarise(
    CAR = sum(ar),
    sig_CAR = sqrt(sum(var_ar)),
    SCAR = CAR / sig_CAR,
    p_value = 2 * pt(-abs(SCAR), df = PARAMS$L1 - 2)
  )
cat("\nSingle-event summary:\n")
print(demo_summary)


#Cross sectional aggregation
car_per_event <- ar_panel %>%
  group_by(event_id, symbol, news) %>%
  summarise(CAR = sum(ar),
            varCAR = sum(var_ar),
            SCAR = CAR / sqrt(varCAR),
            .groups = "drop")

caar_full_window <- car_per_event %>%
  group_by(news) %>%
  summarise(
    N = n(),
    CAAR = mean(CAR),
    var_bar  = mean(varCAR) / N,
    J1  = CAAR / sqrt(var_bar),
    SCAR_bar = mean(SCAR),
    J2 = sqrt(N * (PARAMS$L1 - 4) / (PARAMS$L1 - 2)) * SCAR_bar,
    p_J1 = 2 * pnorm(-abs(J1)),
    p_J2 = 2 * pnorm(-abs(J2)),
    .groups = "drop"
  )

cat("\nCAAR over the full event window [-20, +20]\n")
print(caar_full_window)


#Test for the subwindow
test_subwindow <- function(panel, lo, hi, label) {
  panel %>%
    filter(between(tau, lo, hi)) %>%
    group_by(event_id, news) %>%
    summarise(CAR = sum(ar), varCAR = sum(var_ar),
              .groups = "drop") %>%
    group_by(news) %>%
    summarise(window = label,
              N = n(),
              CAAR = mean(CAR),
              J1 = CAAR / sqrt(mean(varCAR) / N),
              p_J1 = 2 * pnorm(-abs(J1)),
              .groups = "drop")
}

subwindow_results <- bind_rows(
  test_subwindow(ar_panel, -20, -1, "[-20, -1]"),
  test_subwindow(ar_panel, 0, 0, "[ 0,  0]"),
  test_subwindow(ar_panel, 1, 20, "[+1, +20]")
) %>%
  arrange(news, window)

cat("\nSub-window CAAR and J1\n")
print(subwindow_results)


#Ar at tau = 0 and tau = +1 following slides. 
ar_pointwise <- ar_panel %>%
  filter(tau %in% c(0, 1)) %>%
  group_by(news, tau) %>%
  summarise(
    N = n(),
    AR = mean(ar),
    SE = sqrt(mean(var_ar) / n()),
    J1 = AR / SE,
    p_value = 2 * pnorm(-abs(J1)),
    .groups = "drop"
  ) %>%
  arrange(news, tau)

cat("\nAR at tau = 0 and tau = +1\n")
print(ar_pointwise)


#Plots for output in the report. 
caar_pooled <- ar_panel %>%
  group_by(news, tau) %>%
  summarise(AAR = mean(ar),
            var_AAR = mean(var_ar) / n(),
            .groups = "drop") %>%
  group_by(news) %>%
  arrange(tau, .by_group = TRUE) %>%
  mutate(CAAR = cumsum(AAR),
         var_CAAR = cumsum(var_AAR),
         se_CAAR = sqrt(var_CAAR)) %>%
  ungroup()

p_caar_pooled <- ggplot(caar_pooled,aes(x = tau, y = CAAR, colour = news, linetype = news)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_colour_manual(values = c("Good News" = "darkgreen",
                                 "No News" = "grey40",
                                 "Bad News" = "firebrick")) +
  scale_linetype_manual(values = c("Good News" = "solid",
                                   "No News" = "dashed",
                                   "Bad News" = "solid")) +
  labs(x  = "Event Time",
       y = "CAAR",
       colour = NULL,
       linetype = NULL,
       title = "Cumulative Average Abnormal Returns Around Earnings Announcements",
       subtitle = "Market model on SPY, estimation window L1 = 250") +
  theme_bw() + theme(legend.position = "bottom")

caar_byfirm <- ar_panel %>%
  group_by(symbol, news, tau) %>%
  summarise(AAR = mean(ar), .groups = "drop") %>%
  group_by(symbol, news) %>%
  arrange(tau, .by_group = TRUE) %>%
  mutate(CAAR = cumsum(AAR)) %>%
  ungroup()

p_caar_byfirm <- ggplot(caar_byfirm,aes(x = tau, y = CAAR, colour = news, linetype = news)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
  facet_wrap(~ symbol, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_colour_manual(values = c("Good News" = "darkgreen",
                                 "No News" = "grey40",
                                 "Bad News" = "firebrick")) +
  scale_linetype_manual(values = c("Good News" = "solid",
                                   "No News" = "dashed",
                                   "Bad News" = "solid")) +
  labs(x = "Event Time",
       y = "CAAR",
       colour = NULL,
       linetype = NULL,
       title = "CAAR by Stock and News Category") +
  theme_bw() + theme(legend.position = "bottom")

print(p_caar_pooled)
print(p_caar_byfirm)


#Constant mean return model
cmr_one_event <- function(df) {
  est <- df %>% filter(between(tau, PARAMS$tau_lo - PARAMS$L1, PARAMS$tau_lo - 1))
  tibble(mu_i  = mean(est$r_firm), si2_i = var(est$r_firm))
}

cmr_fits <- event_panel %>%
  group_by(event_id, symbol, ann_date, news) %>%
  nest() %>%
  mutate(fit = map(data, cmr_one_event)) %>%
  select(-data) %>%
  unnest(fit) %>%
  ungroup()

ar_panel_cmr <- event_panel %>%
  filter(between(tau, PARAMS$tau_lo, PARAMS$tau_hi)) %>%
  inner_join(cmr_fits, by = c("event_id", "symbol", "ann_date", "news")) %>%
  mutate(ar = r_firm - mu_i,
         var_ar = si2_i * (1 + 1/PARAMS$L1))

caar_cmr <- ar_panel_cmr %>%
  group_by(event_id, news) %>%
  summarise(CAR = sum(ar), varCAR = sum(var_ar), .groups = "drop") %>%
  group_by(news) %>%
  summarise(N = n(),
            CAAR = mean(CAR),
            J1 = CAAR / sqrt(mean(varCAR) / N),
            p_J1 = 2 * pnorm(-abs(J1)),
            model = "Constant-Mean-Return",
            .groups = "drop")

caar_mkt_compact <- caar_full_window %>%
  select(news, N, CAAR, J1, p_J1) %>%
  mutate(model = "Market Model")

model_compare <- bind_rows(caar_mkt_compact, caar_cmr) %>%
  arrange(news, model)

cat("\nMarket model vs. constant-mean-return model\n")
print(model_compare)


#Save the plots to import into overleaf
out_dir <- "Part3/output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "CAAR_pooled.png"), p_caar_pooled,
       width = 8,  height = 5, dpi = 300)
ggsave(file.path(out_dir, "CAAR_byfirm.png"), p_caar_byfirm,
       width = 10, height = 7, dpi = 300)

cat(sprintf("\nResults written to %s/\n", out_dir))
