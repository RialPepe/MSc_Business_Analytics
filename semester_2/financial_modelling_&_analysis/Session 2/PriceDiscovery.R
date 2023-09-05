# Financial Modelling and Analysis, 2023

# This code is designed to estimate the Information Share between two financial indices using the Gonzalo and Granger (1995) method. 
# Gonzalo and Granger (1995) method:
# The Gonzalo and Granger method is based on a Vector Error Correction Model (VECM) and cointegration analysis. 
# In this approach, the Information Share is estimated by calculating the contribution of each market 
# to the adjustment process of the cointegrating relationship between the markets. 
# The Information Share is determined by dividing the estimated cointegrating coefficients for each market 
# by the sum of the coefficients for both markets. 
# This approach assumes that the markets are cointegrated and that there is a long-run relationship between the prices in the different markets.

# The code takes the following steps

# Load required libraries
library(quantmod)
library(xts)
library (urca)

# Define date range
start_date ="2020-01-01"
end_date = "2021-01-01"

# Download data from Yahoo Finance
ticker1 ="^GSPC"  # S&P 500 Index
ticker2 = "^IXIC"  # Nasdaq Composite Index

data1 = getSymbols(ticker1, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
data2 = getSymbols(ticker2, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)

# Calculate daily returns
returns1 = diff(log(Ad(data1)))
returns2 = diff(log(Ad(data2)))

# Remove NA values
returns1 = returns1[!is.na(returns1)]
returns2 = returns2[!is.na(returns2)]

# Perform cointegration test
# This test checks whether there is a long-run relationship between the two return series.
coint_test =ca.jo(cbind(returns1, returns2), type = "trace", ecdet = "none", K = 2)

# Estimate the VECM
# estimates the coefficients of the cointegrating relationship
vecm = cajorls(coint_test)

# Estimate the Gonzalo and Granger Information Share
alpha = vecm$rlm$coef[1, 1]
beta = vecm$rlm$coef[2, 1]
information_share <- (beta / (alpha + beta))

print(paste("Gonzalo and Granger Information Share:", information_share))


# The Information Share obtained from the Gonzalo and Granger (1995) method measures the relative contribution of each index to the price discovery 
# process. The value of the Information Share ranges between 0 and 1. 
# A value closer to 1 indicates that the second index (Nasdaq Composite Index) plays a more significant role in the price discovery process, 
# while a value closer to 0 indicates that the first index (S&P 500 Index) is more dominant in the price discovery process. 
# A value of 0.5 would indicate that both indices contribute equally to the price discovery process.


# Hasbrouck (1995) method
# The assumptions: Gonzalo and Granger assume that the markets are cointegrated, while Hasbrouck does not require this assumption.
# Hasbrouck (1995) method:
# The Hasbrouck method, on the other hand, is based on a Vector Autoregressive (VAR) model and the concept of permanent and transitory 
# price components. In this approach, the Information Share is estimated by analyzing the relative contribution of each market to
# the permanent price component (i.e., the portion of the price change that is not expected to be reversed).
# The Information Share is determined by decomposing the variance of the permanent price component and calculating the proportion 
# of the total variance that is attributed to each market.

library (vars)

returns = cbind(returns1, returns2)

# Fit VAR model
var_model = VAR(returns, p = 1)

# Get the residuals for each stock
resid1 = var_model$varresult[[1]]$resid
resid2 = var_model$varresult[[2]]$resid


# Calculate residual covariance matrix
residual_cov = cov(resid1, resid2)

# Calculate total variance
total_variance = sum(diag(residual_cov))

# Calculate Hasbrouck Information Share
information_share = residual_cov / total_variance

# Print the results
cat("Hasbrouck Information Share:", information_share, "\n")
cat("Total Variance:", total_variance, "\n")



# More sophisticated
# Estimate impulse response functions
irf = irf(var_model, n.ahead = 10, ortho = FALSE)

# Calculate variance decomposition
vd = fevd(var_model, n.ahead = 10)

# Compute Hasbrouck Information Share
total_variance = sum(diag(vd[[1]][[10]]))
information_share = vd[[1]][[10]] / total_variance

# Print results
print(information_share)

