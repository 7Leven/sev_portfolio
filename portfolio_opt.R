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
All of this script is functional. 
Need to write a function that calculates returns and variance.
12/27/18
'''
######################      MISC: CONFIG       #########################################

setwd("C:\\Users\\patdjm7\\Documents\\RIF\\sev_portfolio\\sev_portfolio") # sets the working directory
start_date <- "2007-01-01" #Format year-mo-da
end_date <- substr(Sys.time(), 0, 10) #gets current date
tar_return <- .1 #target return - 10%
tar_risk <- .05 #target risk variance - 5%
alp <- .05 #alpha
risk_free <- .0275 #risk free rate, treasury
risk_aversion <- 4 #risk aversion coefficient

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


weights <- c("minW[1:22]=.0015", "maxW[1:22]=.05",          # Min-Max Weights all tickers
             "minsumW[1:22]=.4", "minsumW[22:27]=.6",       # 40% min Equities, 60% min else         
             "maxsumW[1:2]=.02", "maxsumW[3:7]=.05",        # 2% max consumer disc. 5% max tech
             "maxsumW[8:9]=.1", "maxsumW[10:15]=.06",      # 10% max health. 6% max industr.
             "maxsumW[16:17]=.05", "maxsumW[18:22]=.12",    # 5% max energy. 12% max defensive
             "minW[18:22]=.015",                            # 1.5% min individual defensive
             "minsumW[23]=.3", "minsumW[24]=.1",            # 30% minimum fix-inc. 10% min commodities
             "minW[25:26]=.005",                            # Individual min weights for alt.
             "minsumW[25:26]=.015","maxsumW[25:26]=.02",    # 1.5-2% range alternatives
             "minsumW[27]=.18","maxsumW[27]=.18")           # 18% cash 

######################STEP ONE: Gathering Data#########################################

portfolioPrices <- NULL #creates the portfolioPrices variable
for (ticker in tickers){ #binds all the portfolio data together into one dataframe
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(ticker, src='yahoo',
                                      from = start_date,
                                      to = end_date,
                                      auto.assign =  FALSE,
                                      adjusted = TRUE)[,4])
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
#colames(frontier)[29] <- "CASH" #Replaces MINT with CASH. Better formatting. Make sure 29 is actually where mint is.
port_weights <- frontier[,0:29]
write.csv(port_weights, paste("portfolios_", end_date,".csv", sep=""))
