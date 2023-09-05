## MSc Business Analitics
##Financial Modelling and Analysis


setwd("~/R/Trinity/Session1") # setting a working directory to access cvs data file


# Calculate the present value of a series of cash flows
cash_flows = c(5000, 6000, 7000, 8000)
discount_rate  = 0.05
present_value = sum(cash_flows / (1 + discount_rate )^(1:length(cash_flows)))
present_value


#Calculate the future value of an investment
initial_investment = 10000
interest_rate = 0.04
years = 10

future_value = initial_investment * (1 + interest_rate)^years
future_value



# NPV
library (FinancialMath)

# NPV(cf0,cf,times,i,plot=FALSE)
# cf0 cash flow at period 0
# cf vector of cash flows
# times vector of the times for each cash flow
# i interest rate per period
# plot tells whether or not to plot the time diagram of the cash flows

Savings = c(50000, 60000, 75000, 90000, 90000)
i=0.08
cf0 = 300000
times = c(1,2,3,4,5)
NPV(cf0=cf0, cf=Savings,times=times,i=i,plot=TRUE)






data.csv = read.csv("BitcoinData.csv")

# The base R function read.csv() will, by default, convert any character variable to a factor. 
# This is often not what you want, and can be overridden by passing the option stringsAsFactors = FALSE to read.

# read_csv() will always read variables containing text as character variables.
# read_csv is significantly faster for large .csv files


# Load the readr package
library(readr)

# Import data from a CSV file

data_csv = read_csv("BitcoinData.csv") 

# Load the readxl package
library(readxl)

# Import data from an Excel file
file_path = "~/R/Trinity2023/BitcoinData.xlsx"
data_excel <- read_excel(file_path, sheet = 1) # Specify the sheet number or name



# Import stock price data from Yahoo Finance

# Load the quantmod package
#Way 1

library(quantmod)
ticker = "AAPL" # Apple Inc.
tickers = c("VXX", "AMZN", "ABNB", "EFV") # or several
start_date = as.Date("2020-01-01")
end_date = as.Date("2023-03-27")

getSymbols(ticker, src = "yahoo", from = start_date, to = end_date) #get one
getSymbols(tickers, src = "yahoo", from = start_date, to = end_date) #get several

# The data will be saved in the R environment with the name 'AAPL'


# Way 2

library(tidyquant)

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
#The list of the YahooFInance symbols you can get from
# https://www.cboe.com/us/equities/market_statistics/listed_symbols/

# example
getSymbols("AMZN", from = '2017-01-01',
           to = "2023-03-01",warnings = FALSE,
           auto.assign = TRUE) # getting Amazon prices

chart_Series(AMZN) #plotting the series

# you can download multiple stock prices
tickers = c("VXX", "AMZN", "ABNB", "EFV") # Series B S&P 500 VIX Short-Term Futures, Amazon, VanEck Bitcoin Strategy
getSymbols(tickers, from = '2021-01-01',
           to = "2023-03-25",warnings = FALSE,
           auto.assign = TRUE)



# Way N3 # more options 

#install.packages('yfR')
# Github (dev version)
#require(devtools)
#devtools::install_github('msperlin/yfR')

library(yfR)
#  options 
my_ticker = 'VXX' # a code of a stock, as the example Facebook
first_date = Sys.Date() - 365 #First day
last_date = Sys.Date() #last day at the moment of running

# get  data
df_yf = yf_get(tickers = my_ticker, 
               first_date = first_date,
               last_date = last_date)


#crypto
library (crypto2)
# return new listings from the last 30 days
new_quotes = crypto_global_quotes(which="latest", quote=FALSE)
new_quotes2 = crypto_global_quotes(which="latest", quote=TRUE, convert="BTC,USD")
# return all global quotes since January 2023
quotes_2023 = crypto_global_quotes(which="historical", quote=TRUE,
                                   start_date = "20230101", end_date="20230327", interval="daily")
# report in two different currencies
listings_2023_USDBTC =  crypto_global_quotes(which="historical", quote=TRUE,
                                             start_date = "20230101", end_date="20230327", interval="daily", convert="USD,BTC")

# Retrieving market history for ALL crypto currencies
all_coins = crypto_history(limit = 2)
one_coin = crypto_history(limit = 1)

# Retrieving market history since 2020 for ALL crypto currencies
all_coins = crypto_history(start_date = '20200101',limit=10) #taking only top10 

# Retrieve 2015 history for all 2015 crypto currencies
coin_list_2015 = crypto_list(only_active=TRUE) %>%
  dplyr::filter(first_historical_data<="2015-12-31",
                last_historical_data>="2015-01-01")
coins_2015 = crypto_history(coin_list = coin_list_2015,
                            start_date = "20150101", end_date="20151231", limit=20, interval="90d")


coin_info = crypto_info(limit=3)










##Economic data

#World development indicators https://data.worldbank.org/
# Loading the WDI package
library(WDI)

# Search all indicators with the term "GDP"
listOfIndicators = WDIsearch("GDP")

# List the first 5 indicators
listOfIndicators[1:5,]

# For example, it would be interesting to evaluate the total amount of stocks traded in percentage of GDP (CM.MKT.TRAD.GD.ZS) 
# for 4 countries (Ireland - IRL; France - FR; Canada - CA; USA - US; China - CN; ) from 2000 to 2022. 
# This could be obtained by using the function WDI() with the following inputs:

listOfIndicators [which(listOfIndicators$indicator== "CM.MKT.TRAD.GD.ZS"), ]

indicator = "CM.MKT.TRAD.GD.ZS"
country = c("IRL", "FR", "CA", "US", "CN")
start = 2000
end = 2022

stockTraded <- WDI(indicator = indicator, country = country, start = start, end = end)

head(stockTraded)



#World bank data
library(wbstats)
str(wb_cachelist, max.level = 1) #a snapshot of available countries, indicators, and other relevant informatio

new_cache <- wb_cache() #to update cach


# Search available data 
CPI_ind = wb_search("CPI")
head(CPI_ind)

#search where source organization is Bloomberg
blmbrg_vars =  wb_search("Bloomberg", fields = "source_org")

# 'poverty' OR 'unemployment' OR 'employment'
povemply_inds =  wb_search(pattern = "poverty|unemployment|employment")

# contains "gdp" and NOT "trade"
gdp_no_trade_inds = wb_search("^(?=.*gdp)(?!.*trade).*", perl = TRUE)


# Population, total
pop_data <- wb_data("SP.POP.TOTL", start_date = 2000, end_date = 2021)



# you can mix different ids and they are case insensitive
# you can even use SpOnGeBoB CaSe if that's the kind of thing you're into
# iso3c, iso2c, country, region_iso3c, admin_region_iso3c, admin_region, income_level
example_geos = c("ABW","AF", "albania", "SSF", "eca", "South Asia", "HiGh InCoMe")
pop_data = wb_data("SP.POP.TOTL", country = example_geos,
                    start_date = 2012, end_date = 2022)


pop_data

#several indicators
my_indicators = c("pop" = "SP.POP.TOTL",
                  "gdp" = "NY.GDP.MKTP.CD")

pop_gdp <- wb_data(my_indicators, start_date = 2010, end_date = 2022)




# data manipulation
# Load the dplyr package
library(dplyr)

# Example dataset
data = data.frame(
  date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
  price = c(100, 105, 110, 115),
  volume = c(5000, 5500, 5200, 5800)
)

# Select specific columns
data_selected = data %>%
  select(date, price)

# Filter rows based on a condition
data_filtered = data %>%
  filter(volume > 5000)

# Arrange rows by a specific column
data_arranged = data %>%
  arrange(desc(price)) # Sort by price in descending order

# Add a new column using existing columns
data_mutated = data %>%
  mutate(price_change = price - lag(price))

# Summarize data by applying aggregation functions
data_summarized <- data %>%
  summarize(mean_price = mean(price), total_volume = sum(volume))



# Load the tidyr package
library(tidyr)

# Example dataset
data_wide = data.frame(
  date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
  price_AAPL = c(100, 105, 110),
  price_MSFT = c(150, 155, 160)
)

# Convert wide format to long format
data_long = data_wide %>%
  gather(key = "stock", value = "price", -date)

# Convert long format to wide format
data_wide_again = data_long %>%
  spread(key = "stock", value = "price")



# Load the data.table package
library(data.table)

# Convert the data frame to a data.table object
data_DT = as.data.table(data)

# Select specific columns
data_selected_DT = data_DT[, .(date, price)]

# Filter rows based on a condition
data_filtered_DT = data_DT[volume > 5000]

# Arrange rows by a specific column
data_arranged_DT = data_DT[order(-price)] # Sort by price in descending order

# Add a new column using existing columns
data_mutated_DT = data_DT[, price_change := price - shift(price)]

# Summarize data by applying aggregation functions
data_summarized_DT <- data_DT[, .(mean_price = mean(price), total_volume = sum(volume))]





#Missing data
# Check for missing data in the entire dataset
missing_data = is.na(data)

# Check for missing data in a specific column
missing_data_column = is.na(data$price)

# Calculate the percentage of missing data in each column
missing_data_percent = colMeans(is.na(data)) * 100



data2 = data
data2$price[3] = NA
data2$volume[2] = NA

# Remove rows with any missing data
data_no_missing = na.omit(data2)

# Remove rows with missing data in a specific column
data_no_missing_column = data2[!is.na(data2$price), ]




# Impute missing values with the mean
data_mean_imputed = data2
data_mean_imputed$price[is.na(data2$price)] = mean(data2$price, na.rm = TRUE)

# Impute missing values with the median
data_median_imputed = data2
data_median_imputed$price[is.na(data2$price)] = median(data2$price, na.rm = TRUE)




# Load the zoo package
library(zoo)

# Impute missing values using linear interpolation
data_interpolated = data2
data_interpolated$price = na.approx(data2$price)



data2 = data
data2$price[3] = NA
data2$volume[2] = NA
# Load the DMwR package
library(DMwR2)

# Impute missing values using k-Nearest Neighbors (kNN) imputation
data_knn_imputed = knnImputation(data2, k = 2) # k is the number of nearest neighbors to consider




data2 = data
data2$price[3] = NA
data2$volume[2] = NA
# Load the mice package
library(mice)

# Perform multiple imputation using MICE
mice_imputed <- mice(data2, m = 2) # m is the number of imputed datasets to create

# Extract the first imputed dataset
data_mice_imputed <- complete(mice_imputed, 1)






# Create a boxplot to visualize outliers
boxplot(data$price, main = "Price Boxplot", ylab = "Price")

# Identify outliers using the boxplot function
boxplot_stats$ =  boxplot(data$price, plot = FALSE)
outliers = boxplot_stats$out




# Calculate z-scores
data$z_scores = scale(data$price)

# Define a threshold for outliers (e.g., 1 standard deviations away from the mean)
threshold = 1

# Identify outliers based on z-scores
outliers = data[abs(z_scores) > threshold, ]




# Load the DescTools package
library(DescTools)

# Winsorize the data at the 5th and 95th percentiles
data_winsorized <- Winsorize(data$price, probs = c(0.05, 0.95))




# Define the 5th and 95th percentiles as the trimming thresholds
lower_threshold = quantile(data$price, 0.05)
upper_threshold = quantile(data$price, 0.95)

# Trim the data based on the defined thresholds
data_trimmed = data[data$price >= lower_threshold & data$price <= upper_threshold, ]




# Log transformation
data_log_transformed = log(data$price)

# Square root transformation
data_sqrt_transformed = sqrt(data$price)

# Box-Cox transformation (find the optimal transformation parameter)
library(MASS)
boxcox_res = boxcox(price ~ 1, data = data)
lambda =  boxcox_res$x[which.max(boxcox_res$y)]

# Define a function to apply the Box-Cox transformation with the optimal lambda
boxcox_transform = function(x, lambda) {
  if (lambda == 0) {
    return(log(x))
  } else {
    return((x^lambda - 1) / lambda)
  }
}

# Apply the Box-Cox transformation to the entire dataset
data_boxcox_transformed = boxcox_transform(data$price, lambda)



## Return

require (xts) # attache the package to convert our dataframes into an xts (time-series) object for simplicity in calculations

Bitcoin = read.csv("BitcoinData.csv")# import data
Bitcoin = Bitcoin[, -1] # clean a bit
Bitcoin$date = as.Date(Bitcoin$date,format="%Y-%m-%d", tz = "") # converting a date column into date format 
Bitcoin.xts = xts(Bitcoin[,-1], order.by=Bitcoin[,1]) # converting a data frame into xts object (time-series object)

plot (Bitcoin.xts$price)

#return calculation

#log return
return = diff (log(Bitcoin.xts$price)) # Log return calculation
return = return [-1] # removing the first empty observation, received after return calculation
summary (return)
plot(return$price)

# it will be the same as :
library(PerformanceAnalytics)
return2 = Return.calculate(prices.xts, method="log")

return3 = diff (prices.xts) # differences
# the sane as 
return4 = Return.calculate(prices.xts, method="difference")


#daily gain/loss (%)
return5 = diff(prices.xts)/prices.xts[-length(prices.xts)]





## simulation
# Simulate the future price of a stock with a current price of $100, an annualized return of 10%, an annualized volatility of 20%, 
# and a simulation period of 1 year. Use Monte Carlo simulation with 10,000 scenarios to estimate the expected future price and the probability 
# of the stock price being above $120 at the end of the year.

# Load necessary libraries
library(tidyverse)

# Set parameters
current_price = 100
annual_return =  0.1
annual_volatility = 0.2
simulation_period = 1
num_simulations = 10000

# Define function to simulate stock prices
simulate_stock_prices = function(current_price, annual_return, annual_volatility, simulation_period, num_simulations) {
  # Calculate daily return and daily volatility
  daily_return = annual_return / 252
  daily_volatility = annual_volatility / sqrt(252)
  
  # Generate random daily returns using normal distribution
  random_daily_returns = matrix(rnorm(num_simulations * 252, mean = daily_return, sd = daily_volatility), nrow = num_simulations, ncol = 252)
  
  # Calculate simulated stock prices
  simulated_prices = t(apply(1 + random_daily_returns, 1, cumprod)) * current_price
  return(simulated_prices)
}

# Run the simulation
simulated_prices = simulate_stock_prices(current_price, annual_return, annual_volatility, simulation_period, num_simulations)

# Calculate expected future price
expected_future_price = mean(simulated_prices[, 252])

# Calculate probability of the stock price being above $120 at the end of the year
prob_above_120 = mean(simulated_prices[, 252] > 120)

# Print results
cat("Expected future price:", expected_future_price, "\n")
cat("Probability of stock price above $120:", prob_above_120 * 100, "%\n")

# The code is set up to simulate stock prices for 252 days (1 year of trading days) and 10,000 simulations. 
# This generates a 10,000 x 252 matrix, with each row representing a unique simulation of stock prices over the course of a year.
# Thus, for instance, the expression simulated_prices[, 252] selects the last day's prices for all 10,000 simulations. 
# This is because simulated_prices is a matrix where rows represent simulations and columns represent days. 
# So we calculate the average stock price at the end of the year (day 252) across all 10,000 simulations and then compare with 120






# an R code that downloads stock prices from Yahoo Finance, calculates the necessary parameters for discretization of GBM, 
# and then simulates stock prices for the next week

# Load necessary libraries
# We load the 'tidyquant' and 'dplyr' libraries, which are used to handle financial data and data manipulation, respectively.
library(tidyquant)
library(dplyr)


# Set the ticker symbol and the date range
# We define the stock ticker symbol (AAPL for Apple Inc.), and the start and end dates for the historical data.
ticker = "AAPL"
start_date = "2022-01-01"
end_date = "2022-12-31"

# Download stock prices
prices = tq_get(ticker, from = start_date, to = end_date)

# Calculate  log returns using adjusted prices
daily_returns = prices %>%
  dplyr::select(date, adjusted) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, type = "log")


# Calculate return and volatility in the simplest way
daily_return = mean(daily_returns$monthly.returns)
daily_volatility = sd(daily_returns$monthly.returns)



# GBM Discretization parameters
# We define several parameters for the GBM Discretization:
# 'current_price': the most recent adjusted closing price.
# 'time_step': the time step used in the discretization, which we set to 1/252 (assuming 252 trading days per year; it is different for a crypto market - it is open whole year).
# 'num_simulations': the number of simulations to run (1000 in this case).
# 'forecast_period': the number of days to forecast into the future (5 trading days for next week).

current_price = tail(prices$adjusted, 1)
time_step = 1/252
num_simulations = 1000
forecast_period = 5  # 5 trading days for next week



# Discretization of GBM simulation
# This function simulates stock prices using the discretization of GBM. 
# It generates random  returns using the 'rnorm()' function with the mean and standard deviation adjusted for the time step. 
# The random daily returns are then used to compute the stock price paths using the cumulative product of (1 + random_daily_returns).
# Finally, it returns the simulated stock price paths.

simulate_stock_prices = function(current_price, daily_return, daily_volatility, time_step, num_simulations, forecast_period) {
  random_daily_returns = matrix(rnorm(num_simulations * forecast_period, mean = daily_return * time_step, sd = daily_volatility * sqrt(time_step)), nrow = num_simulations)
  price_paths = current_price * t(apply(1 + random_daily_returns, 1, cumprod))
  return(price_paths)
}


# Simulate stock prices for next week
# We call the 'simulate_stock_prices()' function with the previously defined parameters to generate the simulated stock prices for the next week.
simulated_prices = simulate_stock_prices(current_price, daily_return, daily_volatility, time_step, num_simulations, forecast_period)



# Calculate the expected stock price for the end of next week
expected_future_price = mean(simulated_prices[, forecast_period])
print(expected_future_price)


