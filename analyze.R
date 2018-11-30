'''
pat@7Leven.net

This script is to analyze the portfolios for the 
Rambler Investment Fund. (RIF)

Please do not touch this script if you do not know
what you are doing. I will have a settings file that 
can adjust this script without damaging it. 

symbols <- c("AMZN", "LVMUY", "CELH",
             "XOM", "TLT", "IAU",
"GSK", "GD", "LMT", 
"NOC", "RTN", "PANW", 
"TCEHY", "WDC")

'''
#set.seed(7777777)
#777777
library(tidyquant)
library(tidyverse)
library(timeSeries)
library(timetk)
library(broom)
library(highcharter)
library(purrr)
library(PerformanceAnalytics)

# Generating portfolio & metrics


roth_symbols <- c("FDN", "QQQ", "MCD", "CVS", "TMUS")
roth_weights <- c(.248, .167, .37, .079, .136)
spy <- c("spy")
spyw <- c(1)
num_months <- 120
num_sims <- 1500
symbols <- roth_symbols
w <- roth_weights

prices <- 
  getSymbols(symbols, src = 'yahoo', 
             from = "2008-01-01",
             to = "2018-10-01",
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)

asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

portfolio_returns_tq_rebalanced_yearly <- 
  asset_returns_long %>%
  tq_portfolio(assets_col  = asset, 
               returns_col = returns,
               weights     = w,
               col_rename  = "returns",
               rebalance_on = "years")

mean_port_return <- 
  mean(portfolio_returns_tq_rebalanced_yearly$returns)

stddev_port_return <- 
  sd(portfolio_returns_tq_rebalanced_yearly$returns)

simulated_monthly_returns <- rnorm(num_months, 
                                   mean_port_return, 
                                   stddev_port_return)

simulation_cumprod <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>% 
    `colnames<-`("returns") %>%
    mutate(growth = cumprod(returns)) %>% 
    select(growth)
}

simulated_returns_add_1 <- 
  tibble(c(1, 1 + simulated_monthly_returns)) %>% 
  `colnames<-`("returns")

simulated_growth <- 
  simulated_returns_add_1 %>%
  mutate(growth1 = accumulate(returns, function(x, y) x * y),
         growth2 = accumulate(returns, `*`),
         growth3 = cumprod(returns)) %>% 
  select(-returns)

cagr <- 
  ((simulated_growth$growth1[nrow(simulated_growth)]^
      (1/10)) - 1) * 100

cagr <- round(cagr, 2)

# Running monte-carlo analysis
starts <- 
  rep(1, num_sims) %>%
  set_names(paste("sim", 1:num_sims, sep = ""))

monte_carlo_sims <- 
  map_dfc(starts, 
          simulation_cumprod, 
          N = num_months,
          mean = mean_port_return,
          stdev = stddev_port_return)

ending_balances <- as.numeric(monte_carlo_sims[120,])

hist(ending_balances)

tail(monte_carlo_sims %>%  select(growth1, growth30,
                                    growth90, growth120), 3)

monte_carlo_sims <- 
  monte_carlo_sims %>% 
  mutate(month = seq(1:nrow(.))) %>% 
  select(month, everything()) %>% 
  `colnames<-`(c("month", names(starts))) %>% 
  mutate_all(funs(round(., 2))) 

monte_carlo_sims %>% 
  gather(sim, growth, -month) %>% 
  group_by(sim) %>% 
  ggplot(aes(x = month, y = growth, color = sim)) + 
  geom_line() +
  theme(legend.position="none")

sim_summary <- 
  monte_carlo_sims %>% 
  gather(sim, growth, -month) %>% 
  group_by(sim) %>% 
  summarise(final = last(growth)) %>% 
  summarise(
    max = max(final), 
    min = min(final),
    median = median(final))

sim_summary

mc_gathered <- 
  monte_carlo_sims %>% 
  gather(sim, growth, -month) %>% 
  group_by(sim)

mc_max_med_min <- 
  mc_gathered %>%
  filter(
    last(growth) == sim_summary$max || 
      last(growth) == sim_summary$median ||
      last(growth) == sim_summary$min) %>% 
  group_by(sim)


# Sources:
# https://rviews.rstudio.com/2018/06/05/monte-carlo/
# https://rviews.rstudio.com/2018/06/13/monte-carlo-part-two/

