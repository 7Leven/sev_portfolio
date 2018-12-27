# Load package PerformanceAnalytics 
library(PerformanceAnalytics)
library(plotly)
library(ggplot2)
library(RColorBrewer)
'''
Normalized equity weights of RIF:
eq_weights <- c(.175, .05, .025, .05, .025,
                .225, .075, .05, .05, .05,
                .05, .05, .075, .025, .025)

Raw Weights:
'''
# Print the first and last six rows of prices
pal <- c("forestgreen", "darkred")
rif_weights <- c(.07, .02, .01, .02, .01,
                .09, .03, .02, .02, .02,
                 .02, .02, .03, .01, .01, .6)
spy_weight <- c(1)
RIF_symbols <- c("AMZN", "LVMUY", "CELH", "XOM",
                  "GBTC", "TLT", "IAU", "GSK", "GD",
                  "LMT", "NOC", "RTN", "PANW", "TCEHY",
                  "WDC", 'BIL')
spy <- c("SPY")
symbols <- spy
start <- "2018-07-01"
end <- "2018-12-05"

rif_prices <- 
  getSymbols(RIF_symbols, src = 'yahoo', 
             from = start,
             to = end,
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(RIF_symbols)
spy_prices <- 
  getSymbols(spy, src = 'yahoo', 
             from = start,
             to = end,
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(spy)
# Create the variable returns using Return.calculate()  
RIF_returns <- Return.calculate(rif_prices)
SPY_returns <- Return.calculate(spy_prices)

# Print the first six rows of returns. Note that the first observation is NA, because there is no prior price.


# Remove the first row of returns
RIF_returns <- RIF_returns[-1,]
SPY_returns <- SPY_returns[-1,]
# Create the weights


# Creating RIF's portfolio v S&P 500 rebalanced yearly.
RIF_port <- Return.portfolio(R = RIF_returns, weights = rif_weights,
                             rebalance_on = "years", verbose = TRUE)
SPY_port <- Return.portfolio(R = SPY_returns, weights = spy_weight,
                             rebalance_on = "years", verbose = TRUE)


# Plot the time-series
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
chart.CumReturns(RIF_port$returns, verbose = TRUE) # plotting RIF returns
chart.CumReturns(SPY_port$returns, verbose = TRUE) # plotting SPY returns

