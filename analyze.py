# -*- coding: utf-8 -*-
"""
Created on Fri Nov  9 17:52:21 2018
This script analyzes portfolios input into the model.
Runs Monte Carlo Analysis, Walk Forward Analysis, Monkey Analysis

Created for the Rambler Investment Fund

@author: 7Leven
"""

import pandas as pd
import pandas_datareader.data as web
import time
import settings
from scipy import *
import datetime
from pypfopt import expected_returns
import matplotlib.pyplot as plt

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
daily_returns = price_data.pct_change()
returns = expected_returns.mean_historical_return(daily_returns)
