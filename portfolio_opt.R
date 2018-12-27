library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
#library(plotly)
'''
Author: 7Leven
Written for the Rambler Investment Fund - 2018
Purpose: Optimize RIFs portfolio according to constraints and goals.
Notes from last update:
All of this script is functional. Need to write a function that calculates returns and variance.
12/27/18
'''
######################      MISC: CONFIG       #########################################

setwd("C:\\Users\\patdj\\Documents\\RIF\\data") # sets the working directory
start_date <- "2016-01-01" #Format year-mo-da
end_date <- substr(Sys.time(), 0, 10) #gets current date
tar_return <- .1 #target return - 10%
tar_risk <- .05 #target risk variance - 5%
alp <- .05 #alpha
risk_free <- .0275 #risk free rate, treasury
risk_aversion <- 4 #risk aversion coefficient

tickers <- c("LVMUY", "CELH",                                        #CONSUMER DISCRETIONARY
             "AMZN", "PANW", "WDC", "SNAP", "TSLA","MU",             #TECH
             "GSK", "MRK",                                           #HEALTHCARE
             "GD", "LMT", "NOC", "RTN", "BA", "MDR",                 #INDUSTRIALS
             "XOM", "PXD", "ARCH",                                   #ENERGY
             "CL", "NGG", "VZ", "BAX", "FMS",                        #DEFENSIVE
             "TLT",                                                  #FIXED INCOME
             "IAU",                                                  #COMMODITIES
             "UUP", "NLY",                                           #ALTERNATIVE
             "MINT")                                                 #CASH


weights <- c("minW[1:24]=.0015", "maxW[1:24]=.5",
             "minsumW[1:24]=.4", "minsumW[25:29]=.6",
             "maxsumW[1:2]=.02", "maxsumW[3:8]=.05",
             "maxsumW[9:10]=.1", "maxsumW[11:16]=.06",
             "maxsumW[17:19]=.05", "maxsumW[20:24]=.12",
             "minsumW[25]=.3", "minsumW[26]=.1",
             "minsumW[27:28]=.0015","maxsumW[27:28]=.02", 
             "minsumW[29]=.18","maxsumW[29]=.18")

######################STEP ONE: Gathering Data#########################################

prices <- getSymbols(tickers, src = 'yahoo',
                     from = start_date,
                     to = end_date,
                     auto.assign = TRUE, warnings = FALSE)

portfolioPrices <- NULL #creates the portfolioPrices variable
for (ticker in tickers){ #binds all the portfolio data together into one dataframe
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(ticker, auto.assign =  FALSE)[,4])
} # basic wrangling 

######################STEP TWO: WRANGLING DATA#########################################
#Delete all dates with no prices -- removing data errors
portfolioPrices <- na.omit(portfolioPrices)
#Rename Columns -- making the data frame look prettier
colnames(portfolioPrices) <- tickers

#Calculate Returns: Daily RoC
portfolioReturns <- CalculateReturns(portfolioPrices)
portfolioReturns <- na.omit(portfolioReturns) #removes null values
# The line below may not be necessary. If errors uncomment
portfolioReturns <- as.timeSeries(portfolioReturns) #converts the dataframe to a timeseries

######################STEP THREE: CONSTRUCTION #########################################

meanReturns <- colMeans(portfolioReturns) #calculates the average return of each ticker
covMat <- cov(portfolioReturns) #covariance matrix

portSpec <- portfolioSpec( #this is where the risk aversion and other parameters are input
  model = list(type = "MV", optimize = "minRisk",
               estimator = "covEstimator", tailRisk = list(),
               params = list(alpha = alp, a = risk_aversion)),
  portfolio = list(weights = NULL, targetReturn = tar_return,
                   targetRisk = tar_risk, riskFreeRate = risk_free, nFrontierPoints = 60,
                   status = 0),
  optim = list(solver = "solveRquadprog", objective = NULL,
               params = list(meq = 2), control = list(), trace = FALSE))

######################STEP FOUR: PORTFOLIO GENERATION#########################################

#calculates efficient frontier, minimum variance portfolio, effiicent portfolio, minrisk
effFrontier <- portfolioFrontier(portfolioReturns, spec = portSpec, constraints = weights)
efmvPort <- minvariancePortfolio(portfolioReturns, spec = portSpec, constraints = weights)
efficientPort <- efficientPortfolio(portfolioReturns, spec = portSpec, constraints = weights)
minRiskPort <- minriskPortfolio(portfolioReturns, spec = portSpec, constraints = weights)
#the above functions work, but with new constraints added in, I need more memory to get more results
frontierWeights <- getWeights(effFrontier) # get allocations for each instrument for each point on the efficient frontier
frontierReturns <- getTargetReturn(effFrontier) #generates the return
frontier <- cbind(frontierWeights, frontierReturns) #formats return and weights into a dataframe
