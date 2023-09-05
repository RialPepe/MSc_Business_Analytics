library(quantmod)
library(pastecs)
library(fredr)
library(jsonlite)
library(rugarch)
library(vars)
library(ggplot2)



setwd("C:/Users/pepec/Desktop/estudios/master/semester 2/Financial modeling/Individual_Project2") 


### Importing the data and calculating returns

## Cryptocurrencies----------------------------------------------------------------------------------
# Bitcoin ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("BTC-USD", from = '2020-10-03',
           to = "2023-05-01",warnings = FALSE,
           auto.assign = TRUE) # getting Bitcoin prices

Bitcoin <- `BTC-USD`

chart_Series(Bitcoin$`BTC-USD.Close`)

sum(is.na(Bitcoin))

bitcoin.xts = Bitcoin$`BTC-USD.Close`
bitcoin.zoo = as.zoo(bitcoin.xts)

return1 = diff (log(bitcoin.xts)) # Log return calculation
return1 = return1 [-1] # removing the first empty observation, received after return calculation
summary (return1)
stat.return1 = stat.desc(return1)

plot(return1, main = "Bitcoin daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph



# ETH ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("ETH-USD", from = '2020-10-03',
           to = "2023-05-01",warnings = FALSE,
           auto.assign = TRUE)

Ethereum <- `ETH-USD`

chart_Series(Ethereum$`ETH-USD.Close`)

sum(is.na(Ethereum))

ethereum.xts = Ethereum$`ETH-USD.Close`
ethereum.zoo = as.zoo(ethereum.xts)


return2 = diff (log(ethereum.xts)) 
return2 = return2 [-1] 
summary (return2)
stat.return2 = stat.desc(return2)

plot(return2, main = "Ethereum daily returns", xlab = "year", type = "l", ylab = "log return") 



# Binance ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("BNB-USD", from = '2020-10-03',
           to = "2023-05-01",warnings = FALSE,
           auto.assign = TRUE) 

Binance <- `BNB-USD`

chart_Series(Binance$`BNB-USD.Close`)

sum(is.na(Binance))

binance.xts = Binance$`BNB-USD.Close`
binance.zoo = as.zoo(binance.xts)

return3 = diff (log(binance.xts)) 
return3 = return3 [-1] 
summary (return3)
stat.return3 = stat.desc(return3)

plot(return3, main = "BNB-USD daily returns", xlab = "year", type = "l", ylab = "log return") 



# UNI ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("UNI-USD", from = '2020-10-03',
           to = "2023-05-01",warnings = FALSE,
           auto.assign = TRUE)

Uniswap <- `UNI-USD`

chart_Series(Uniswap$`UNI-USD.Close`)

sum(is.na(Uniswap))

Uniswap.xts = Uniswap$`UNI-USD.Close`
Uniswap.zoo = as.zoo(Uniswap.xts)

return4 = diff (log(Uniswap.xts)) 
return4 = return4 [-1] 
summary (return4)
stat.return4 = stat.desc(return4)

plot(return4, main = "Uniswap daily returns", xlab = "year", type = "l", ylab = "log return") 



# AAVE ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("AAVE-USD", from = '2020-10-03',
           to = "2023-05-01",warnings = FALSE,
           auto.assign = TRUE) 

AAVE <- `AAVE-USD`

chart_Series(AAVE$`AAVE-USD.Close`)

sum(is.na(AAVE))

AAVE.xts <- AAVE$`AAVE-USD.Close`
AAVE.zoo <- as.zoo(AAVE.xts)

return5 <- diff(log(AAVE.xts)) 
return5 <- return5[-1] 
summary(return5)
stat.return5 <- stat.desc(return5)

plot(return5, main = "AAVE daily returns", xlab = "year", type = "l", ylab = "log return") 



## Traditional assets ----------------------------------------------------------------------------------
# Apple (AAPL) ----------------------------------------------------------------------
# Download stock prices for Apple (AAPL)
getSymbols("AAPL", from = "2020-10-03", to = "2023-05-01")

# Retrieve the closing prices
AAPL <- Cl(AAPL)

# Calculate the daily returns
return6 <- diff(log(AAPL))
return6 <- return6[-1]

# Summary statistics of the returns
summary(return6)

# Plot the returns
plot(return6, main = "AAPL daily returns", xlab = "Year", type = "l", ylab = "Log return")



# Microsoft (MSFT) ----------------------------------------------------------------------
# Download stock prices for Microsoft (MSFT)
getSymbols("MSFT", from = "2020-10-03", to = "2023-05-01")

# Retrieve the closing prices
MSFT <- Cl(MSFT)

# Calculate the daily returns
return7 <- diff(log(MSFT))
return7 <- return7[-1]

# Summary statistics of the returns
summary(return7)

# Plot the returns
plot(return7, main = "MSFT daily returns", xlab = "Year", type = "l", ylab = "Log return")



# Tesla (TSLA) ----------------------------------------------------------------------
# Download stock prices for Tesla (TSLA)
getSymbols("TSLA", from = "2020-10-03", to = "2023-05-01")

# Retrieve the closing prices
TSLA <- Cl(TSLA)

# Calculate the daily returns
return8 <- diff(log(TSLA))
return8 <- return8[-1]

# Summary statistics of the returns
summary(return8)

# Plot the returns
plot(return8, main = "TSLA daily returns", xlab = "Year", type = "l", ylab = "Log return")



# NVIDIA (NVDA) ----------------------------------------------------------------------
# Download stock prices for NVIDIA (NVDA)
getSymbols("NVDA", from = "2020-10-03", to = "2023-05-01")

# Retrieve the closing prices
NVDA <- Cl(NVDA)

# Calculate the daily returns
return9 <- diff(log(NVDA))
return9 <- return9[-1]

# Summary statistics of the returns
summary(return9)

# Plot the returns
plot(return9, main = "NVDA daily returns", xlab = "Year", type = "l", ylab = "Log return")



# Intel (INTC) ----------------------------------------------------------------------
# Download stock prices for Intel (INTC)
getSymbols("INTC", from = "2020-10-03", to = "2023-05-01")

# Retrieve the closing prices
INTC <- Cl(INTC)

# Calculate the daily returns
return10 <- diff(log(INTC))
return10 <- return10[-1]

# Summary statistics of the returns
summary(return10)

# Plot the returns
plot(return10, main = "INTC daily returns", xlab = "Year", type = "l", ylab = "Log return")


#putting all the returns together
assets_returns_together.xts <- na.omit(cbind(return1, return2, return3, return4, return5, 
                              return6, return7, return8, return9, return10))



# Calculate correlations between asset returns
correlations <- cor(assets_returns_together.xts)

# Print the correlation matrix
print(correlations)


#causality
# Combine the variables into a matrix
data <- na.omit(cbind(bitcoin.xts, ethereum.xts, binance.xts, AAVE.xts, Uniswap.xts, AAPL, MSFT, TSLA, NVDA, INTC))

# Estimate a VAR model with lag order selection
var_model <- VAR(data, p = 3, type = "both")

# Test for Granger causality
causality_test <- causality(var_model)

# Print the test results
print(causality_test)



#volatility
# Define the ARMA-GARCH specification
arma.garch.norm <- ugarchspec(
  mean.model = list(armaOrder = c(1, 0)),
  variance.model = list(garchOrder = c(1, 1))
)

# Estimate the GARCH model
BTC.garch.norm <- ugarchfit(data = assets_returns_together.xts$BTC.USD.Close, spec = arma.garch.norm)
ETH.garch.norm <- ugarchfit(data = assets_returns_together.xts$ETH.USD.Close, spec = arma.garch.norm)
BNB.garch.norm <- ugarchfit(data = assets_returns_together.xts$BNB.USD.Close, spec = arma.garch.norm)
UNI.garch.norm <- ugarchfit(data = assets_returns_together.xts$UNI.USD.Close, spec = arma.garch.norm)
AAVE.garch.norm <- ugarchfit(data = assets_returns_together.xts$AAVE.USD.Close, spec = arma.garch.norm)
AAPL.garch.norm <- ugarchfit(data = assets_returns_together.xts$AAPL.Close, spec = arma.garch.norm)
MSFT.garch.norm <- ugarchfit(data = assets_returns_together.xts$MSFT.Close, spec = arma.garch.norm)
TSLA.garch.norm <- ugarchfit(data = assets_returns_together.xts$TSLA.Close, spec = arma.garch.norm)
NVDA.garch.norm <- ugarchfit(data = assets_returns_together.xts$NVDA.Close, spec = arma.garch.norm)
INTC.garch.norm <- ugarchfit(data = assets_returns_together.xts$INTC.Close, spec = arma.garch.norm)



## PCA 
pca = prcomp(assets_returns_together.xts)# Performs a principal components analysis on the given data matrix and returns the results as an object of class prcomp.
summary(pca) # return the statistics


##Factor analysis
# Define the time range for the data
start_date <- as.Date("2020-10-03")
end_date <- as.Date("2023-05-01")

# Import inflation data
getSymbols("CPIAUCNS", src = "FRED", from = start_date, to = end_date)
inflation_data <- CPIAUCNS[1:29]

# Import industrial production index data
getSymbols("INDPRO", src = "FRED", from = start_date, to = end_date)
industrial_production_data <- INDPRO[1:29]

# Import stock market index data (e.g., S&P 500)
getSymbols("^GSPC", from = start_date, to = end_date)
SP500_data <- to.monthly(GSPC$GSPC.Close, indexAt = "firstof", OHLC = FALSE)[2:30]

# Import interest rate data (e.g., 10-year Treasury bond yield)
getSymbols("DGS10", src = "FRED", from = start_date, to = end_date)
treasury_data <- to.monthly(DGS10, indexAt = "firstof", OHLC = FALSE)[2:30]

# Import unemployment rate data
getSymbols("UNRATE", src = "FRED", from = start_date, to = end_date)
unemployment_data <- UNRATE[1:29]

factors_together = cbind(inflation_data, industrial_production_data, SP500_data,
                         treasury_data, unemployment_data)


# Calculate the returns of the economic factors

# CPI
CPI_returns <- diff(log(inflation_data))[-1]

# Industrial Production
IP_returns <- diff(log(industrial_production_data))[-1]

# Stock market index (S&P 500)
SP500_returns <- diff(log(SP500_data))[-1]

# Interest rate (10-year Treasury bond yield)
interest_rate_returns <- diff(log(treasury_data))[-1]

# Unemployment rate
unemployment_returns <- diff(log(unemployment_data))[-1]

# Combine the returns of the economic factors 
factors_returns_together <- cbind(CPI_returns, IP_returns, SP500_returns,
                                  interest_rate_returns, unemployment_returns)

# Combine the returns of assets and economic factors
all_returns_together <- na.omit(cbind(assets_returns_together.xts, factors_returns_together))

# Create the design matrix and response vector
design_matrix <- as.matrix(all_returns_together[, -(1:ncol(assets_returns_together.xts))])
response_vector <- as.matrix(all_returns_together[, 1:ncol(assets_returns_together.xts)])

# Fit the multivariate regression model
model <- lm(response_vector ~ design_matrix)

# Analyze the model
summary(model)


#prediction of the prices with monte carlo simulation
# Set the number of Monte Carlo simulations
num_simulations <- 1000

# Set the number of future periods to predict
num_periods <- 30

# Get the last available price of the asset
last_price <- tail(Bitcoin$`BTC-USD.Close`, 1)

# Create an empty matrix to store the simulated prices
simulated_prices <- matrix(nrow = num_periods, ncol = num_simulations)

# Perform Monte Carlo simulation
for (i in 1:num_simulations) {
  # Set the initial price as the last available price
  current_price <- last_price
  
  # Simulate future price movements
  for (j in 1:num_periods) {
    # Generate a random log return for the asset
    random_return <- rnorm(1, mean = 0, sd = sd(all_returns_together))
    
    # Calculate the new price based on the log return
    new_price <- current_price * exp(random_return)
    
    # Update the current price for the next iteration
    current_price <- new_price
    
    # Store the simulated price in the matrix
    simulated_prices[j, i] <- new_price
  }
}

# Plot the simulated prices
plot(simulated_prices[, 1], type = "l", main = "Simulated Prices for Bitcoin",
     xlab = "Period", ylab = "Price")

