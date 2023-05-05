library(forecast)
library(zoo)

# Finishing last weeks class
Amtrak.data <- read.csv("Amtrak data.csv")

# create time series object using ts()
# ts() takes three arguments: start, end, and freq.
# with monthly data, the frequency of periods per season is 12 (per year).
# arguments start and end are (season number, period number) pairs.
# here start is Jan 1991: start = c(1991, 1); end is Mar 2004: end = c(2004, 3).
ridership.ts <- ts(Amtrak.data$Ridership,
                   start = c(1991, 1), end = c(2004, 3), freq = 12)

nValid <- 36
nTrain <- length(ridership.ts) - nValid

# partition the data
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1),
                   end = c(1991, nTrain + nValid))

# plot Training data
plot(train.ts)

# Generate benchmarks
naive_forecast <- mean(train.ts)
seasonal_naive_fit <- ts(Amtrak.data$Ridership[(1):(nTrain-12)], start = c(1991, 12+1),
                         end = c(1991, nTrain), frequency = 12)
seasonal_naive <- ts(Amtrak.data$Ridership[(nTrain - 12 + 1):(nTrain + nValid-12)], start = c(1991, nTrain + 1),
                     end = c(1991, nTrain + nValid), frequency = 12)

accuracy(rep(naive_forecast,36), valid.ts)
accuracy(seasonal_naive, valid.ts)

# 1. moving average on training ----
ma.trailing <- rollmean(train.ts, k = 12, align = "right")

# obtain the last moving average in the training period
last.ma <- tail(ma.trailing, 1)

# create forecast based on last MA
ma.trailing.pred <-
  ts(
    rep(last.ma, nValid),
    start = c(1991, nTrain + 1),
    end = c(1991, nTrain + nValid),
    freq = 12
  )

# Calculate the accuracy of trailing MA
accuracy(ma.trailing.pred, valid.ts)

# plot the series
plot(
  train.ts,
  ylim = c(1300, 2600),
  ylab = "Ridership",
  xlab = "Time",
  bty = "l",
  xaxt = "n",
  xlim = c(1991, 2006.25),
  main = ""
)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ma.trailing, lwd = 2, col = "blue")
lines(ma.trailing.pred,
      lwd = 2,
      col = "blue",
      lty = 2)
lines(valid.ts, col = "red")

# Simple exponential smoothing model
ses <- ets(train.ts, model = c("MNN"))
# auto_arima_model <- auto.arima(train.ts)

# Predictions
ses_pred <- forecast(ses, h = 36)
# arima_pred <- forecast(auto_arima_model, h = 36)

# in-sample predictive power
accuracy(ses$fitted, train.ts)
# accuracy(auto_arima_model$fitted, train.ts)

# Out-of-sample predictive power
accuracy(ses_pred, valid.ts)
# accuracy(arima_pred, valid.ts)

plot(
  valid.ts,
  ylab = "Ridership",
  xlab = "Time",
  bty = "l",
  xaxt = "n",
  main = "",
  lty = 1,
  lwd = 2,
  ylim = c(1600,2400)
)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

# Naive Forecasts:
lines(seasonal_naive, col = "orange", lty = 3, lwd = 3)
abline(h = mean(valid.ts), col = "darkgreen", lty =3, lwd = 3)
# Moving average
lines(ma.trailing.pred, col = "red", lty = 2)
# Simple Exponential Smoothing
lines(ses_pred$mean, col = "blue", lty = 2)

# Let's try our linear model
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
train.lm.trend.season.pred <-
  forecast(train.lm.trend.season, h = 36, level = 0)
lines(train.lm.trend.season.pred$mean,
      lwd = 1,
      col = "purple")

# Naive Benchmarks to beat
naive_benchmark_accuracy <- rbind(accuracy(rep(naive_forecast,123), train.ts),
      accuracy(rep(naive_forecast,36), valid.ts),
      accuracy(seasonal_naive_fit, window(train.ts, start = c(1991, 12 + 1),
                                          end = c(1991, 123))),
      accuracy(seasonal_naive, valid.ts)
      )
rownames(naive_benchmark_accuracy) <- c("simple naive fit", "simple naive forecast", "seasonal naive fit", "seasonal naive forecast")
naive_benchmark_accuracy

# Forecast accuracies
forecast_accuracy <-
  rbind(accuracy(ma.trailing, train.ts),
        accuracy(ma.trailing.pred, valid.ts),
        accuracy(ses$fitted, train.ts),
        accuracy(ses_pred$mean, valid.ts),
        accuracy(train.lm.trend.season$fitted.values, train.ts),
        accuracy(train.lm.trend.season.pred$mean, valid.ts)
)

rownames(forecast_accuracy) <- c("Trail MA fit",
                                        "Trail MA forecast",
                                        "SES fit",
                                        "SES forecast",
                                        "LM fit",
                                        "LM forecast")
forecast_accuracy

# now lets compare simple and Holt-Winters smoothing
# Simple exponential smoothing model
ses_hw <- ets(train.ts, model = c("ZZZ"))

# Predictions
ses_hw_pred <- forecast(ses_hw, h = 36)

# Out-of-sample predictive power
accuracy(ses_hw_pred, valid.ts)
# accuracy(arima_pred, valid.ts)

lines(ses_hw_pred$mean, lwd =3)

plot(
  train.ts,
  ylim = c(1300, 2300),
  ylab = "Ridership",
  xlab = "Time",
  bty = "l",
  xaxt = "n",
  xlim = c(1991, 2006.25),
  main = ""
)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

lines(valid.ts, col = "black")
abline(v = 2001.25, col = "red", lty = 2)
lines(ses_hw$fitted, col = "blue", lty =3)
lines(ses_hw_pred$mean, col = "blue", lty =2)


# Finally, lets change our timeline to a more recent training set
new_train.ts <- window(ridership.ts, start = c(1991, 61), end = c(1991, nTrain))
new_ses_hw <- ets(new_train.ts, model = c("ZZZ"))
new_lm <- tslm(train.ts ~ trend + I(trend^2) + season)


# Predictions
new_ses_hw_pred <- forecast(new_ses_hw, h = 36)
new_lm_pred <- forecast(new_lm, h = 36)
# Out-of-sample predictive power
accuracy(new_ses_hw_pred, valid.ts)
accuracy(new_lm_pred, valid.ts)

plot(
  valid.ts,
  ylab = "Ridership",
  xlab = "Time",
  bty = "l",
  xaxt = "n",
  main = "",
  lty = 1,
  lwd = 2,
  ylim = c(1600,2400)
)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

# Naive Forecasts:
lines(seasonal_naive, col = "orange", lty = 3, lwd = 3)

# HW Exponential Smoothing
lines(ses_hw_pred$mean, col = "blue", lty =2, lwd = 3)
lines(new_ses_hw_pred$mean, col = "red", lty = 2)
lines(new_lm_pred$mean, col = "blue", lty = 2)

naive_benchmark_accuracy
accuracy(ses_hw_pred, valid.ts)
