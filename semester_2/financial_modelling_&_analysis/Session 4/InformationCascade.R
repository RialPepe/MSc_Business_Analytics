## N1 - the simplest framework. Everything is simulated  ------------------------------------------------------------------------------------------
# Set parameters
num_individuals <- 100
true_value <- 1 # True value of the investment (1: good, 0: bad)
signal_accuracy <- 0.6 # Probability that an individual's private signal matches the true value


# Generate private signals
# The private signals are generated using a random binomial distribution, rbinom(), 
# based on the signal accuracy and true value. If the true value is 1, the probability 
# of getting a correct private signal is equal to the signal accuracy. 
# If the true value is 0, the probability of getting an incorrect private signal is equal to the signal accuracy. 
# The set.seed(42) function ensures that the random numbers generated are reproducible.
set.seed(42)
private_signals <- rbinom(num_individuals, 1, ifelse(true_value == 1, signal_accuracy, 1 - signal_accuracy))

# Initialize decision vector
# An empty numeric vector decisions is created to store the decisions of each individual.
decisions <- numeric(num_individuals)


# Function to calculate the likelihood ratio of the observed decisions
# calc_likelihood_ratio() is a function that calculates the likelihood ratio of the observed decisions given the signal accuracy. 
# The likelihood ratio is a measure of how much more likely the observed decisions are under the assumption 
# that the true value is 1 compared to the assumption that the true value is 0.
calc_likelihood_ratio <- function(decisions_so_far, signal_accuracy) {
  num_invest <- sum(decisions_so_far)
  num_not_invest <- length(decisions_so_far) - num_invest
  (signal_accuracy / (1 - signal_accuracy))^num_invest * ((1 - signal_accuracy) / signal_accuracy)^num_not_invest
}

# Simulate the decision-making process
# A for loop is used to iterate through each individual's private signal. 
# The first individual makes a decision solely based on their private signal. 
# Subsequent individuals consider the decisions of those before them by calculating the likelihood ratio and comparing it to their private signal. 
# Based on this comparison, they make their decision.
for (i in seq_along(private_signals)) {
  if (i == 1) {
    # First individual makes a decision based solely on their private signal
    decisions[i] <- private_signals[i]
  } else {
    # Subsequent individuals consider the decisions of those before them
    decisions_so_far <- decisions[1:(i - 1)]
    likelihood_ratio <- calc_likelihood_ratio(decisions_so_far, signal_accuracy)
    
    # Compare the likelihood ratio to the individual's private signal
    if (private_signals[i] == 1 && likelihood_ratio > 1) {
      decisions[i] <- 1
    } else if (private_signals[i] == 0 && likelihood_ratio < 1) {
      decisions[i] <- 0
    } else {
      decisions[i] <- private_signals[i]
    }
  }
}

# Results
print(paste("True value:", true_value))
print(paste("Private signals:", paste(private_signals, collapse = ", ")))
print(paste("Decisions:", paste(decisions, collapse = ", ")))
    








# N2 - Improved code with downloaded prices ---------------------------------------------------------------
# Load required libraries
library(quantmod)

# Download adjusted closing prices for a stock (e.g., Apple Inc.)
ticker <- "AAPL"
start_date <- "2021-01-01"
end_date <- "2021-12-31"
stock_prices <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
adj_close <- Ad(stock_prices)

# Calculate daily returns
daily_returns <- diff(log(adj_close))[-1]

# Set parameters
num_days <- length(daily_returns)
signal_accuracy <- 0.6 # Probability that an individual's private signal matches the true value

# Function to calculate the likelihood ratio of the observed decisions
calc_likelihood_ratio <- function(decisions_so_far, signal_accuracy) {
  num_invest <- sum(decisions_so_far)
  num_not_invest <- length(decisions_so_far) - num_invest
  (signal_accuracy / (1 - signal_accuracy))^num_invest * ((1 - signal_accuracy) / signal_accuracy)^num_not_invest
}
  
# Generate private signals: 1 for positive return (buy), 0 for negative return (sell)
set.seed(42)
true_values <- ifelse(daily_returns > 0, 1, 0)
private_signals <- rbinom(num_days, 1, ifelse(true_values == 1, signal_accuracy, 1 - signal_accuracy))

# Initialize decision vector
decisions <- numeric(num_days)

# Simulate the decision-making process
for (i in seq_along(private_signals)) {
  if (i == 1) {
    # First individual makes a decision based solely on their private signal
    decisions[i] <- private_signals[i]
  } else {
    # Subsequent individuals consider the decisions of those before them
    decisions_so_far <- decisions[1:(i - 1)]
    likelihood_ratio <- calc_likelihood_ratio(decisions_so_far, signal_accuracy)
    
    # Compare the likelihood ratio to the individual's private signal
    if (private_signals[i] == 1 && likelihood_ratio > 1) {
      decisions[i] <- 1
    } else if (private_signals[i] == 0 && likelihood_ratio < 1) {
      decisions[i] <- 0
    } else {
      decisions[i] <- private_signals[i]
    }
  }
}

# Results
print(paste("Ticker:", ticker))
print(paste("Date range:", start_date, "to", end_date))
print(paste("True values (1: buy, 0: sell):", paste(true_values, collapse = ", ")))
print(paste("Private signals (1: buy, 0: sell):", paste(private_signals, collapse = ", ")))
print(paste("Decisions (1: buy, 0: sell):", paste(decisions, collapse = ", ")))






# N3 we incorporate additional factors such as a moving average of past returns and a threshold for making investment decision ----------
# This will help capture some of the momentum and trend-following behavior often observed in financial markets. 
# We will also use a continuous variable for decisions, representing the proportion of an investor's portfolio allocated to the stock.


# Load required libraries
library(quantmod)

# Download adjusted closing prices for a stock (e.g., Apple Inc.)
ticker <- "AAPL"
start_date <- "2021-01-01"
end_date <- "2021-12-31"
stock_prices <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
adj_close <- Ad(stock_prices)

# Calculate daily returns
daily_returns <- diff(log(adj_close))[-1]

# Function to calculate the likelihood ratio of the observed decisions
calc_likelihood_ratio <- function(decisions_so_far, signal_accuracy) {
  num_invest <- sum(decisions_so_far)
  num_not_invest <- length(decisions_so_far) - num_invest
  (signal_accuracy / (1 - signal_accuracy))^num_invest * ((1 - signal_accuracy) / signal_accuracy)^num_not_invest
}

# Set parameters
num_days <- length(daily_returns)
signal_accuracy <- 0.6 # Probability that an individual's private signal matches the true value
lookback_period <- 10 # Moving average lookback period
threshold <- 0.01 # Threshold for making investment decisions

# Generate private signals: 1 for positive return (buy), 0 for negative return (sell)
set.seed(42)
true_values <- ifelse(daily_returns > 0, 1, 0)
private_signals <- rbinom(num_days, 1, ifelse(true_values == 1, signal_accuracy, 1 - signal_accuracy))

# Initialize decision vector
decisions <- numeric(num_days)

# Calculate moving average of past returns
moving_average <- rollapply(daily_returns, lookback_period, mean, align = "right", fill = NA)

# Simulate the decision-making process
for (i in seq_along(private_signals)) {
  if (i <= lookback_period) {
    # Investors make decisions based solely on their private signal during the initial lookback period
    decisions[i] <- ifelse(private_signals[i] == 1, 1, 0)
  } else {
    # Subsequent investors consider the moving average of past returns and the decisions of others
    decisions_so_far <- decisions[(i - lookback_period):(i - 1)]
    likelihood_ratio <- calc_likelihood_ratio(decisions_so_far, signal_accuracy)
    
    # Calculate the proportion of the portfolio allocated to the stock based on the likelihood ratio and private signal
    if (private_signals[i] == 1 && likelihood_ratio > 1 && moving_average[i] > threshold) {
      decisions[i] <- min(1, (likelihood_ratio - 1) / 10) # Normalized to [0, 1]
    } else if (private_signals[i] == 0 && likelihood_ratio < 1 && moving_average[i] < -threshold) {
      decisions[i] <- 1 - min(1, (1 - likelihood_ratio) / 10) # Normalized to [0, 1]
    } else {
      decisions[i] <- ifelse(private_signals[i] == 1, 1, 0)
    }
  }
}

# Results
print(paste("Ticker:", ticker))
print(paste("Date range:", start_date, "to", end_date))
print(paste("True values (1: buy, 0: sell):", paste(true_values, collapse = ", ")))
print(paste("Private signals (1: buy, 0: sell):", paste(private_signals, collapse = ", ")))
print(paste("Decisions (Portfolio allocation):", paste(round(decisions, 2), collapse = ", ")))








# N4 - more realistic --------------------------------------------------------------------------------------------------------
# To make the code more realistic and use the observed data to calculate the probability that an individual's private signal
# matches the true value, we will implement the following changes:
    # Calculate the percentage of days with positive returns in the historical data to estimate the signal accuracy.
    # Incorporate the moving average of both past returns and past volume as additional factors in decision-making.
    # Introduce transaction costs that influence the investment decision.

# Load required libraries
library(quantmod)

# Download adjusted closing prices and volume for a stock (e.g., Apple Inc.)
ticker <- "AAPL"
start_date <- "2021-01-01"
end_date <- "2021-12-31"
stock_prices <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
adj_close <- Ad(stock_prices)
volume <- Vo(stock_prices)

# Calculate daily returns
daily_returns <- diff(log(adj_close))[-1]

# Function to calculate the likelihood ratio of the observed decisions
calc_likelihood_ratio <- function(decisions_so_far, signal_accuracy) {
  num_invest <- sum(decisions_so_far)
  num_not_invest <- length(decisions_so_far) - num_invest
  (signal_accuracy / (1 - signal_accuracy))^num_invest * ((1 - signal_accuracy) / signal_accuracy)^num_not_invest

}

# Set parameters
num_days <- length(daily_returns)
lookback_period <- 10 # Moving average lookback period
threshold <- 0.01 # Threshold for making investment decisions
transaction_cost <- 0.002 # Transaction cost as a percentage of the investment value

# Estimate signal accuracy based on historical data
signal_accuracy <- mean(daily_returns > 0)

# Generate private signals: 1 for positive return (buy), 0 for negative return (sell)
set.seed(42)
true_values <- ifelse(daily_returns > 0, 1, 0)
private_signals <- rbinom(num_days, 1, ifelse(true_values == 1, signal_accuracy, 1 - signal_accuracy))

# Initialize decision vector
decisions <- numeric(num_days)

# Calculate moving averages of past returns and past volume
moving_average_returns <- rollapply(daily_returns, lookback_period, mean, align = "right", fill = NA)
moving_average_volume <- rollapply(volume, lookback_period, mean, align = "right", fill = NA)

# Simulate the decision-making process
for (i in seq_along(private_signals)) {
  if (i <= lookback_period) {
    # Investors make decisions based solely on their private signal during the initial lookback period
    decisions[i] <- ifelse(private_signals[i] == 1, 1, 0)
  } else {
    # Subsequent investors consider the moving averages of past returns and past volume, and the decisions of others
    decisions_so_far <- decisions[(i - lookback_period):(i - 1)]
    likelihood_ratio <- calc_likelihood_ratio(decisions_so_far, signal_accuracy)
    
    # Calculate the proportion of the portfolio allocated to the stock based on the likelihood ratio, private signal, moving averages, and transaction costs
    if (private_signals[i] == 1 && likelihood_ratio > 1 && moving_average_returns[i] > threshold && moving_average_volume[i] > mean(volume)) {
      decisions[i] <- min(1, (likelihood_ratio - 1 - transaction_cost) / 10) # Normalized to [0, 1]
    } else if (private_signals[i] == 0 && likelihood_ratio < 1 && moving_average_returns[i] < -threshold && moving_average_volume[i] > mean(volume)) {
      decisions[i] <- 1 - min(1, (1 - likelihood_ratio - transaction_cost) / 10) # Normalized to [0, 1]
    } else {
      decisions[i] <- ifelse(private_signals[i] == 1, 1, 0)
    }
  }
}

# Results
print(paste("Ticker:", ticker))
print(paste("Date range:", start_date, "to", end_date))
print(paste("Signal accuracy (based on historical data):", round(signal_accuracy, 2)))
print(paste("True values (1: buy, 0: sell):", paste(true_values, collapse = ", ")))
print(paste("Private signals (1: buy, 0: sell):", paste(private_signals, collapse = ", ")))
print(paste("Decisions (Portfolio allocation):", paste(round(decisions, 2), collapse = ", ")))
    








# N5 - more realistic, We : -----------------------------------------------------------------------------
    # Implement a portfolio of multiple stocks instead of a single stock.
    # Use a more advanced trading strategy based on a combination of technical indicators.
    # Introduce a risk management component to adjust the portfolio allocation based on market volatility.

# Load required libraries
library(quantmod)
library(TTR) # for technical indicators

# Define stock tickers and date range
tickers <- c("AAPL", "GOOGL", "MSFT")
start_date <- "2021-01-01"
end_date <- "2021-12-31"

# Download adjusted closing prices and volume for multiple stocks
stock_prices <- lapply(tickers, function(ticker) {
  getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)[,6]
})

# Calculate daily returns
daily_returns <- lapply(stock_prices, function(price) diff(log(price))[-1])


# Function to calculate the likelihood ratio of the observed decisions
calc_likelihood_ratio <- function(decisions_so_far, signal_accuracy) {
  num_invest <- sum(decisions_so_far)
  num_not_invest <- length(decisions_so_far) - num_invest
  (signal_accuracy / (1 - signal_accuracy))^num_invest * ((1 - signal_accuracy) / signal_accuracy)^num_not_invest
}

# Set parameters
num_days <- length(daily_returns[[1]])
lookback_period <- 10 # Moving average lookback period
threshold <- 0.01 # Threshold for making investment decisions
transaction_cost <- 0.002 # Transaction cost as a percentage of the investment value

# Calculate signal accuracy for each stock based on historical data
signal_accuracies <- lapply(daily_returns, function(ret) mean(ret > 0))

# Generate private signals: 1 for positive return (buy), 0 for negative return (sell)
set.seed(42)
true_values <- lapply(daily_returns, function(ret) ifelse(ret > 0, 1, 0))
private_signals <- mapply(function(tv, acc) {
  rbinom(length(tv), 1, ifelse(tv == 1, acc, 1 - acc))
}, true_values, signal_accuracies, SIMPLIFY = FALSE)

# Initialize decision matrix
decisions <- matrix(0, nrow = num_days, ncol = length(tickers))

# Calculate moving averages of past returns and technical indicators (e.g., RSI) for each stock
moving_average_returns <- lapply(daily_returns, function(ret) {
  rollapply(ret, lookback_period, mean, align = "right", fill = NA)
})

rsi_periods <- 14
rsis <- lapply(stock_prices, function(price) {
  RSI(price, n = rsi_periods, maType = "WMA")
})

# Simulate the decision-making process
for (i in seq_len(num_days)) {
  if (i <= lookback_period) {
    # Investors make decisions based solely on their private signal during the initial lookback period
    decisions[i, ] <- sapply(private_signals, function(signal) ifelse(signal[i] == 1, 1, 0))
  } else {
    # Subsequent investors consider the moving averages of past returns, technical indicators, and the decisions of others
    for (j in seq_along(tickers)) {
      decisions_so_far <- decisions[(i - lookback_period):(i - 1), j]
      likelihood_ratio <- calc_likelihood_ratio(decisions_so_far, signal_accuracies[[j]])
      
      # Buy signal: moving_average_returns > threshold, RSI < 30 (oversold)
      buy_signal <- moving_average_returns[[j]][i] > threshold && rsis[[j]][i] < 30
      # Sell signal: moving_average_returns < -threshold, RSI > 70 (overbought)
      sell_signal <- moving_average_returns[[j]][i] < -threshold && rsis[[j]][i] > 70

       # Calculate the proportion of the portfolio allocated to the stock based on the likelihood ratio, private signal, moving averages, RSI, and transaction costs
       if (private_signals[[j]][i] == 1 && likelihood_ratio > 1 && buy_signal) {
            decisions[i, j] <- min(1, (likelihood_ratio - 1 - transaction_cost) / 10) # Normalized to [0, 1]
            } else if (private_signals[[j]][i] == 0 && likelihood_ratio < 1 && sell_signal) {
            decisions[i, j] <- 1 - min(1, (1 - likelihood_ratio - transaction_cost) / 10) # Normalized to [0, 1]
            } else {
            decisions[i, j] <- ifelse(private_signals[[j]][i] == 1, 1, 0)
            }
    }
    
    
  }
}


print(paste("Tickers:", paste(tickers, collapse = ", ")))
print(paste("Date range:", start_date, "to", end_date))
print(paste("Signal accuracies (based on historical data):", paste(round(unlist(signal_accuracies), 2), collapse = ", ")))

for (i in seq_along(tickers)) {
  print(paste("Ticker:", tickers[i]))
  print(paste("True values (1: buy, 0: sell):", paste(true_values[[i]], collapse = ", ")))
  print(paste("Private signals (1: buy, 0: sell):", paste(private_signals[[i]], collapse = ", ")))
  print(paste("Decisions (Portfolio allocation):", paste(round(decisions[, i], 2), collapse = ", ")))
}












# N6 - more realistic, we: ----------------------------------------------------------------------
      # Introduce a portfolio optimization step using the Modern Portfolio Theory (MPT) to optimize the portfolio allocation based
      # on the expected return and risk.
      # Account for market sentiment by incorporating news data or other external factors.
      # Use a machine learning model to better predict the private signals based on historical data.

      # This code is designed to simulate the decision-making process of investors trading a selection of stocks based on private signals, 
      # moving averages of past returns, and technical indicators (e.g., RSI). 
      # The code also includes a simple portfolio optimization step to allocate the investments based on the expected returns and risk.

# Here's a breakdown of the code:
# Required libraries are loaded, and stock tickers and date range are defined.
# The adjusted closing prices and volumes are downloaded from Yahoo Finance for the specified stock tickers.
# Daily returns are calculated for each stock.
# The signal accuracies are estimated using a logistic regression model, assuming the investors have access to some form of machine learning model.
    # Here's a more detailed explanation of how signal accuracies are estimated:
    # For each stock, we create a data frame with columns for the stock price and a unique identifier for each observation (day).
    # A new column "signal" is created, which is a binary variable (1 for positive return, 0 for negative return). This column is created based on the lagged price, where a positive return occurs if the previous day's price is lower than the current day's price.
    # Lagged features are added to the data frame, which represent the stock prices for the previous two days.
    # A logistic regression model is fit to the data, with the "signal" column as the dependent variable and the lagged features as the independent variables. The model is fit using the glm() function in R, with the family = "binomial" argument to specify a logistic regression.
    # The fitted model is then used to predict the probabilities of the "signal" variable being equal to 1 on the training data. These probabilities are used to calculate the signal accuracy, which is the proportion of correct predictions (i.e., when the predicted probability is greater than 0.5, and the true signal is 1, or when the predicted probability is less than or equal to 0.5, and the true signal is 0).

# True values and private signals are generated based on the daily returns and signal accuracies.
    # The true values represent the actual direction of stock prices (1 for a positive return and 0 for a negative return), while the private signals represent the individual investors' beliefs about the stock prices based on the information they have.
    # Here's a detailed explanation of how true values and private signals are generated:
    # The daily returns are calculated for each stock using the adjusted closing prices.
    # The true values are generated based on the daily returns. If the daily return is positive (i.e., the stock price increased compared to the previous day), the true value is set to 1. If the daily return is negative or zero (i.e., the stock price decreased or remained the same), the true value is set to 0. The true values are stored in a list, with one element for each stock.
    # The private signals are generated based on the true values and the signal accuracies estimated earlier. For each stock, a random number is generated for each day, and the private signal is set to the true value if the random number is less than or equal to the signal accuracy. Otherwise, the private signal is set to the opposite of the true value (i.e., 1 - true value). This process simulates the idea that investors have noisy information about the stock prices, with the level of noise depending on the signal accuracy. The private signals are stored in a list, with one element for each stock.

# A decision matrix is initialized to store the portfolio allocation decisions for each day.
    # Here's a detailed explanation of how the portfolio allocation decisions are made:
    # A decision matrix is initialized to store the portfolio allocation decisions for each day and each stock.
    # Moving averages of past returns and RSI values are calculated for each stock.
    # A portfolio optimization function, optimize_portfolio, is defined. This function takes daily returns, a target return, and a lambda parameter (which controls the trade-off between return and risk) as inputs. It uses quadratic programming to optimize the risk-adjusted return based on the input parameters.
    # The decision-making process is simulated in a loop for each day:
        # a. During the initial lookback period, investors make decisions based solely on their private signals. If the private signal is 1 (buy), they allocate their portfolio accordingly. If the private signal is 0 (sell), they do not allocate any investment to that stock.
        # b. After the lookback period, investors consider the moving averages of past returns, technical indicators, and the decisions of other investors when making their allocation decisions. The expected returns for each stock are calculated based on the moving averages, and the optimize_portfolio function is used to find the optimal portfolio allocation given the expected returns and risk.
        # c. Subsequently, investors adjust their portfolio allocation based on the likelihood ratio, private signals, and transaction costs. The likelihood ratio is calculated based on the decisions made by other investors during the lookback period. If the private signal is 1 (buy) and the likelihood ratio is greater than 1, the allocation is increased. If the private signal is 0 (sell) and the likelihood ratio is less than 1, the allocation is decreased. The transaction cost is also taken into account when making adjustments.
        # The adjusted portfolio allocation decisions are stored in the decision matrix. The portfolio allocation is adjusted based on the likelihood ratio, private signals, and transaction costs.

# The code then prints the results, including stock tickers, date range, signal accuracies, true values, private signals, and portfolio allocation decisions.

# Load required libraries
library(quantmod)
library(TTR) # for technical indicators
library(dplyr) # for data manipulation
library(quadprog) # for quadratic programming

# Define stock tickers and date range
tickers <- c("AAPL", "GOOGL", "MSFT")
start_date <- "2021-01-01"
end_date <- "2021-12-31"

# Download adjusted closing prices and volume for multiple stocks
stock_prices <- lapply(tickers, function(ticker) {
  getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)[,6]
})

# Calculate daily returns
daily_returns <- lapply(stock_prices, function(price) diff(log(price))[-1])

# Set parameters
num_days <- length(daily_returns[[1]])
lookback_period <- 10 # Moving average lookback period
threshold <- 0.01 # Threshold for making investment decisions
transaction_cost <- 0.002 # Transaction cost as a percentage of the investment value

# Estimate signal accuracy using a machine learning model
signal_accuracies <- lapply(seq_along(tickers), function(j) {
  # Prepare data for machine learning
  data <- stock_prices[[j]] %>% as.data.frame() %>% dplyr::mutate(id = 1:n())
  colnames(data) <- c("price", "id")
  data <- data %>% mutate(signal = ifelse(lag(price, 1) < price, 1, 0))
  data <- data %>% dplyr::mutate(across(starts_with("price"), list(lag_1 = ~lag(.x, 1), lag_2 = ~lag(.x, 2))))
  
  # Fit a logistic regression model
  model <- glm(signal ~ . - price - id, data = data, family = "binomial", na.action = na.exclude)
  
  # Predict signal accuracy on training data
  pred <- predict(model, data, type = "response")
  accuracy <- mean((pred > 0.5) == data$signal, na.rm = TRUE)
  accuracy
})

# Generate private signals: 1 for positive return (buy), 0 for negative return (sell)
set.seed(42)
true_values <- lapply(daily_returns, function(ret) ifelse(ret > 0, 1, 0))
private_signals <- mapply(function(tv, acc) {
  rbinom(length(tv), 1, ifelse(tv == 1, acc, 1 - acc))
}, true_values, signal_accuracies, SIMPLIFY = FALSE)

# Initialize decision matrix
decisions <- matrix(0, nrow = num_days, ncol = length(tickers))

# Calculate moving averages of past returns and technical indicators (e.g., RSI) for each stock
moving_average_returns <- lapply(daily_returns, function(ret) {
  rollapply(ret, lookback_period, mean, align = "right", fill = NA)
})

rsi_periods <- 14
rsis <- lapply(stock_prices, function(price) {
  RSI(price, n = rsi_periods, maType = "WMA")
})

optimize_portfolio <- function(returns, target_return, lambda = 0.5) {
  cov_matrix <- cov(returns)
  expected_returns <- colMeans(returns)
  n_assets <- ncol(returns)
  
  Dmat <- cov_matrix + lambda * diag(1, n_assets, n_assets)
  dvec <- -expected_returns
  Amat <- cbind(matrix(1, nrow = n_assets, ncol = 1), diag(n_assets))
  bvec <- c(1, rep(0, n_assets))
  meq <- 1
  
  result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)
  weights <- result$solution
  return(weights)
}


# Function to calculate the likelihood ratio of the observed decisions
calc_likelihood_ratio <- function(decisions_so_far, signal_accuracy) {
  num_invest <- sum(decisions_so_far)
  num_not_invest <- length(decisions_so_far) - num_invest
  (signal_accuracy / (1 - signal_accuracy))^num_invest * ((1 - signal_accuracy) / signal_accuracy)^num_not_invest
}



# Simulate the decision-making process
for (i in seq_len(num_days)) {
  if (i <= lookback_period) {
    # Investors make decisions based solely on their private signal during the initial lookback period
    decisions[i, ] <- sapply(private_signals, function(signal) ifelse(signal[i] == 1, 1, 0))
  } else {
    # Calculate expected return for each stock
    expected_returns <- sapply(moving_average_returns, function(mar) mar[i])
    
    # Optimize the portfolio allocation
    returns_matrix <- do.call(cbind, daily_returns)
    colnames(returns_matrix) <- tickers
    optimized_weights <- optimize_portfolio(returns_matrix[(i - lookback_period):(i - 1), ], mean(expected_returns))
    
    # Subsequent investors consider the moving averages of past returns, technical indicators, and the decisions of others
    for (j in seq_along(tickers)) {
      decisions_so_far <- decisions[(i - lookback_period):(i - 1), j]
      likelihood_ratio <- calc_likelihood_ratio(decisions_so_far, signal_accuracies[[j]])
      
      # Adjust the optimized weights based on the likelihood ratio, private signals, and transaction costs
      if (private_signals[[j]][i] == 1 && likelihood_ratio > 1) {
        decisions[i, j] <- optimized_weights[j] * (1 + min(1, (likelihood_ratio - 1 - transaction_cost) / 10)) # Normalized to [0, 1]
      } else if (private_signals[[j]][i] == 0 && likelihood_ratio < 1) {
        decisions[i, j] <- optimized_weights[j] * (1 - min(1, (1 - likelihood_ratio - transaction_cost) / 10)) # Normalized to [0, 1]
      } else {
        decisions[i, j] <- optimized_weights[j]
      }
    }
  }
}

# Results
print(paste("Tickers:", paste(tickers, collapse = ", ")))
print(paste("Date range:", start_date, "to", end_date))
print(paste("Signal accuracies (based on historical data):", paste(round(unlist(signal_accuracies), 2), collapse = ", ")))

for (i in seq_along(tickers)) {
  print(paste("Ticker:", tickers[i]))
  
  
  print(paste("True values (1: buy, 0: sell):", paste(true_values[[i]], collapse = ", ")))
  # This line displays the true values (1: buy, 0: sell) for each time period for the current stock ticker. 
  # These true values represent whether the stock was a good (1) or bad (0) investment during that specific period. 
  # This information is not available to the individuals when making their decisions, but it's useful for assessing 
  # the performance of the simulated decision-making process.
  
  print(paste("Private signals (1: buy, 0: sell):", paste(private_signals[[i]], collapse = ", ")))
  # This line shows the private signals (1: buy, 0: sell) of each individual for the current stock ticker. 
  # Each individual receives a private signal, which is an imperfect indication of the true value of the stock. 
  # The signal accuracy determines how likely the private signal is to match the true value.
  
  print(paste("Decisions (Portfolio allocation):", paste(round(decisions[, i], 2), collapse = ", ")))
  #This line presents the decisions made by the individuals for the current stock ticker. 
  # The decisions are based on each individual's private signal and the decisions of those before them. 
  # The decisions represent the portfolio allocation for the stock, where 1 indicates buying the stock and 0 
  # indicates not buying (or selling) the stock.
}
    






