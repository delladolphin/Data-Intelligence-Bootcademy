#%%

# To optimize a users portfolio using the Efficient Frontier & Python.
# Import the python libraries

import pandas as pd
import numpy as np
from datetime import datetime
from matplotlib import pyplot as plt

plt.style.use('fivethirtyeight')

df = pd.DataFrame()
df = pd.read_excel("AMTD.xlsx", parse_dates=True, index_col="date")


#%%

def remove_whitespace(df):
    if isinstance(df):
        return df.strip()
    else:
        return df

remove_whitespace(df)
#%%

df.shape

#%%

df.info()
#6 - AMINAGP HK Equity and  8 - AMINCAA HK Equity have only 684 non-null. 5 - AMRSGRF HK Equity has only 189 non-null.
#Others are similar - about 2600 entries.

#%%

#df_obj = df.select_dtypes(['object'])
#df[df_obj.columns] = df_obj.apply(lambda x: x.str.strip())
#df.applymap(lambda x: x.strip() if isinstance(x, str) else x)


#%%

df.values

#%%

df.describe()

#%%

df.head

#%%

df.tail

#%%

df.dtypes

#%%

df.dropna(axis="index",how="all")

#%%

df.plot(figsize=(15,10))

#%%

df.std()

#%%

#!pip install PyPortfolioOpt

#%%

#from pypfopt.efficient_frontier import EfficientFrontier
from pypfopt import risk_models
from pypfopt import expected_returns
from pypfopt import plotting

#%%

from pypfopt.risk_models import CovarianceShrinkage
S = CovarianceShrinkage(df).ledoit_wolf()
S

#%%

plotting.plot_covariance(S, plot_correlation=True);

#%%

#Calculate the expected returns and the annualised sample covariance matrix of daily asset returns.

from pypfopt.expected_returns import mean_historical_return
mu = mean_historical_return(df)
mu

#%%

mu.plot.barh(figsize=(10,6))

#%%

#Optimize for maximal Sharpe ratio.
ef = EfficientFrontier(mu, S)
weights = ef.max_sharpe() #Maximize the Sharpe ratio, and get the raw weights
cleaned_weights = ef.clean_weights()
print(cleaned_weights) #Note the weights may have some rounding error, meaning they may not add up exactly to 1 but should be close
ef.portfolio_performance(verbose=True)

#%%

!pip install pulp

#%%

#Input amount of your portfolio
tpv = input("Amount of your portfolio:")

#%%

# Get discrete allocation of each MPF - how many shares of each asset you should purchase.
from pypfopt.discrete_allocation import DiscreteAllocation, get_latest_prices
latest_prices = get_latest_prices(df)

cleaned_weights = ef.clean_weights()
weights = cleaned_weights
da = DiscreteAllocation(weights, latest_prices, total_portfolio_value=tpv)
allocation, leftover = da.lp_portfolio()
print("Discrete allocation:", allocation)
print("Funds remaining: ${:.2f}".format(leftover))

#%%

pd.Series(weights).plot.barh()

#%%

#anomaly detection - benchmark abnormal situations, find

#%%


from pypfopt import plotting



#%%



#%%



#%%



#%%



#%%



#%%



#%%



#%%



#%%



#%%


