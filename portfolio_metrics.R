library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(timetk)
library(broom)
library(tibble)
library(highcharter)
library(purrr)
library(tidyquant)
library(tidyverse)
#library(plotly)
'''
Author: 7Leven
Written for the Rambler Investment Fund - 2018
Purpose: Written to gather descriptive stats about portfolios input.
Notes from last update:
All of this script is functional. 
Need to write a function that calculates returns and variance.
12/29/18
'''
######################      MISC: CONFIG       #########################################

setwd("C:\\Users\\patdjm7\\Documents\\RIF\\sev_portfolio\\sev_portfolio") # sets the working directory
start_date <- "2015-01-01" #Format year-mo-da
end_date <- substr(Sys.time(), 0, 10) #gets current date
port_weights <- paste("portfolios_",end_date,".csv", sep="")
weights <- read.csv(port_weights)
weights$X <- NULL
tickers <- c("LVMUY", "CELH",                                        #CONSUMER DISCRETIONARY
             "AMZN", "PANW", "WDC", "TSLA","MU",                     #TECH
             "GSK", "MRK",                                           #HEALTHCARE
             "GD", "LMT", "NOC", "RTN", "BA", "MDR",                 #INDUSTRIALS
             "XOM", "PXD",                                           #ENERGY
             "CL", "NGG", "VZ", "BAX", "FMS",                        #DEFENSIVE
             "SHY",                                                  #FIXED INCOME
             "IAU",                                                  #COMMODITIES
             "UUP", "NLY",                                           #ALTERNATIVE
             "MINT")                                                 #CASH

######################STEP ONE: Gathering Data#########################################

portfolioPrices <- NULL #creates the portfolioPrices variable
for (ticker in tickers){ #binds all the portfolio data together into one dataframe
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(ticker, src='yahoo',
                                      from = start_date,
                                      to = end_date,
                                      auto.assign =  FALSE,
                                      adjusted = TRUE)[,4])
} #basic wrangling
######################STEP TWO: WRANGLING DATA#########################################
#Delete all dates with no prices -- removing data errors
portfolioPrices <- na.omit(portfolioPrices)
#Rename Columns -- making the data frame look prettier
colnames(portfolioPrices) <- tickers
prices_monthly <- to.monthly(portfolioPrices, indexAt = "last", OHLC=FALSE)
prices_weekly <- to.weekly(portfolioPrices, indexAt = "last", OHLC=FALSE)

returns_monthly <- na.omit(Return.calculate(prices_monthly, method="log"))
returns_weekly <- na.omit(Return.calculate(prices_weekly, method="log"))
#Calculate Returns: Daily RoC
#portfolioReturns <- CalculateReturns(portfolioPrices)
#portfolioReturns <- na.omit(portfolioReturns) #removes null values
# The line below may not be necessary. If errors uncomment
#portfolioReturns <- as.timeSeries(portfolioReturns)

port_returns <- list()
port_var <- list()
num_ports <- NROW(weights)
pcount = 1
while(pcount <= num_ports){
  portfolio_returns_rebal_monthly <- Return.portfolio(returns_monthly, 
                                                      weights = as.numeric(weights[pcount,]),
                                                      rebalance_on = "years",
                                                      method = "compound")
  portfolio_returns_rebal_weekly <- Return.portfolio(returns_weekly, 
                                                      weights = as.numeric(weights[pcount,]),
                                                      rebalance_on = "years",
                                                      method ="compound")
  
  #monthly_cagr <- round(Return.annualized(portfolio_returns_rebal_monthly), 4)
  #rolling_monthly_var <- na.omit(rollapply(portfolio_returns_rebal_monthly, FUN = StdDev, width = 12)*sqrt(12))
  weekly_cagr <- round(Return.annualized(portfolio_returns_rebal_weekly), 4)
  rolling_weekly_var <-na.omit(rollapply(portfolio_returns_rebal_weekly, FUN = StdDev, width = 4)*sqrt(252))
  port_returns <- c(port_returns, weekly_cagr)
  port_var <- c(port_var, round(tail(rolling_weekly_var,1),4))
  pcount = pcount +1 
}
ret_var <- cbind(port_returns, port_var)
colnames(ret_var) <- c("CAGR", "STDev")
portfolios <- cbind(as.data.frame(ret_var), weights)
write.csv(portfolios, paste("RIF_PORTS_WEEKLY_DOWN_", end_date,".csv", sep=""))
