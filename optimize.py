# -*- coding: utf-8 -*-
"""
Created on Tue Nov  6 20:12:10 2018

Portfolio Optimization, efficient frontier
This script is used to create severl portfolios for various objectives.
This script alone should not be used for portfolio selection, further analysis
is needed.

Developed for the Rambler Investment Fund.
Open sourced by 7Leven.

@author: 7Leven
"""

import pandas as pd
import pandas_datareader.data as web
import datetime
import time
import pypfopt
from pypfopt.efficient_frontier import EfficientFrontier
from pypfopt import expected_returns
from pypfopt import risk_models
import settings

start_time = time.time()
start_date = datetime.datetime(settings.start_date[0],
                               settings.start_date[1],
                               settings.start_date[2])
end_date = datetime.datetime(settings.end_date[0],
                             settings.end_date[1],
                             settings.end_date[2])

price_data = web.DataReader(settings.portfolio, 
                            'yahoo',
                            start_date,
                            end_date)['Adj Close']
price_data.index.rename('date', True)

returns = expected_returns.mean_historical_return(price_data)
cov = risk_models.sample_cov(price_data)

ef = EfficientFrontier(returns, cov, weight_bounds = (.15, .4))

elapsed = round(time.time() - start_time, 2)

print("Portfolio optimized..")
print("Completed in {} seconds..".format(elapsed))
