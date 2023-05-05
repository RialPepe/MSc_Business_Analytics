#### Figure 16.1

library(forecast)
Amtrak.data <- read.csv("Amtrak data.csv")

# create time series object using ts()
# ts() takes three arguments: start, end, and freq.
# with monthly data, the frequency of periods per season is 12 (per year).
# arguments start and end are (season number, period number) pairs.
# here start is Jan 1991: start = c(1991, 1); end is Mar 2004: end = c(2004, 3).
ridership.ts <- ts(data = Amtrak.data$Ridership,
                   start = c(1991, 1), end = c(2004, 3), freq = 12)

plot(1:159, Amtrak.data[,2])
# plot the series
plot(ridership.ts)
plot(ridership.ts, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300), main = "ridership over time")

# Add the Moving average
lines(ma(ridership.ts, order = 24, centre = TRUE), col = "red")

# Zoom to three years
ridership.ts.3yrs <- window(ridership.ts, start = c(1997, 1), end = c(1999, 12))
season_naive <-  window(ridership.ts, start = c(1996, 1), end = c(1998, 12))
plot(ridership.ts.3yrs, ylim = c(1300,2200))
lines(season_naive, col = "green")

# Naive benchmark - trend
trend_benchmark <- ma(ridership.ts.3yrs, order = 4, centre = TRUE)
lines(trend_benchmark, col = "blue")
# lines(ma(ridership.ts, order = 12, centre = TRUE), col = "blue")

# Seasonal naive
seasonal_naive <- ts(Amtrak.data[61:96,2],
                     start = c(1997, 1),
                     end = c(1999, 12),
                     freq = 12)
lines(seasonal_naive, col = "red")

# Decompose the data into constituents
ridershipComp <- decompose(ridership.ts)
plot(ridershipComp)

# Lets partition the data
par(mfrow = c(1, 1))
nValid <- 36
nTrain <- length(ridership.ts) - nValid

# partition the data
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1),
                   end = c(1991, nTrain + nValid))
length(ridership.ts)
length(train.ts)
length(valid.ts)

train.lm <- tslm(train.ts ~ trend)
train.lm.pred <- forecast(train.lm, h = nValid, level = 0)
accuracy(train.lm)

error <- train.lm.pred$mean - valid.ts
sqrt(mean(error^2))
mean(abs(error))

par(mfrow = c(2, 1))
plot(train.lm.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
plot(train.lm.pred$residuals, ylim = c(-420, 500),  ylab = "Forecast Errors",
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts - train.lm.pred$mean, lwd = 1)



# Test for predictability
train_arima <- arima(train.ts, order = c(1,0,0))
train_arima






# Exponential
par(mfrow = c(2, 1))
plot(train.lm.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
plot(train.lm.pred$residuals, ylim = c(-420, 500),  ylab = "Forecast Errors",
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts - train.lm.pred$mean, lwd = 1)

train.lm2 <- tslm(train.ts ~ trend, lambda = 0)
train.lm.pred2 <- forecast(train.lm2, h = nValid)
accuracy(train.lm2)

error <- train.lm.pred2$mean - valid.ts
sqrt(mean(error^2))
mean(abs(error))

par(mfrow = c(2, 1))
plot(train.lm.pred2, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.pred2$fitted, lwd = 2, col = "blue")
lines(valid.ts)
plot(train.lm.pred2$residuals, ylim = c(-420, 500),  ylab = "Forecast Errors",
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts - train.lm.pred2$mean, lwd = 1)


# Polynomial
train.lm3 <- tslm(train.ts ~ trend + I(trend^2))
train.lm.pred3 <- forecast(train.lm3, h = nValid, level = 0)
accuracy(train.lm3)

error <- train.lm.pred3$mean - valid.ts
sqrt(mean(error^2))
mean(abs(error))

par(mfrow = c(2, 1))
plot(train.lm.pred3, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.pred3$fitted, lwd = 2, col = "blue")
lines(valid.ts)
plot(train.lm.pred3$residuals, ylim = c(-420, 500),  ylab = "Forecast Errors",
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts - train.lm.pred3$mean, lwd = 1)

# Seasonality

train.lm.season <- tslm(train.ts ~ season)
summary(train.lm.season)
accuracy(train.lm.season)
train.lm.pred4 <- forecast(train.lm.season, h = nValid, level = 0)
error <- train.lm.pred4$mean - valid.ts
sqrt(mean(error^2))
mean(abs(error))


plot(train.lm.pred4, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.pred4$fitted, lwd = 2, col = "blue")
lines(valid.ts)
plot(train.lm.pred4$residuals, ylim = c(-420, 500),  ylab = "Forecast Errors",
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts - train.lm.pred4$mean, lwd = 1)

#### Table 17.5

train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.lm.trend.season)
accuracy(train.lm.trend.season)
train.lm.pred5 <- forecast(train.lm.trend.season, h = nValid, level = 0)
error <- train.lm.pred5$mean - valid.ts
sqrt(mean(error^2))
mean(abs(error))


plot(train.lm.pred5, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.pred5$fitted, lwd = 2, col = "blue")
lines(valid.ts)
plot(train.lm.pred5$residuals, ylim = c(-420, 500),  ylab = "Forecast Errors",
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts - train.lm.pred5$mean, lwd = 1)

data <- read.csv("Sept11Travel.csv")

# Sept 11 Air Travel----
data <- read.csv(file = "Sept11Travel.csv")
plot(data$Air.RPM..000s.)
air.ts <-  ts(data$Air.RPM..000s., start = c(1990, 1), end = c(2004, 4), freq = 12)
plot(air.ts)
Acf(air.ts)
Pacf(air.ts)
Amtrak.data <- read.csv("Amtrak.csv")
# create time series
ridership.ts <- ts(
  Amtrak.data$Ridership,
  start = c(1991, 1),
  end = c(2004, 3),
  freq = 12
)

ma.centered <- ma(air.ts,
                  order = 12)
ma.centered
ma.centered
ma.trailing <- rollmean(air.ts,
                        k = 12,
                        align = "right")

ma.trailing

# generate a plot ----
par(mfrow = c(1,1))
plot(
  air.ts,
  # xlim = c(0, 60),
  ylab = "Air",
  xlab = "Time",
  bty = "o",
  xaxt = "n",
  # xlim = c(1991, 2004.25),
  main = ""
)
axis(1,
     at = seq(1991, 2004.25, 1),
     labels = format(seq(1991, 2004.25, 1)))
lines(ma.centered, lwd = 2, col = "blue")
lines(ma.trailing, lwd = 2, lty = 2)
legend(
  "topleft",
  c(
    "Ridership",
    "Centered Moving Average",
    "Trailing Moving Average"
  ),
  lty = c(1, 1, 2),
  lwd = c(1, 2, 2),
  bty = "n"
)

# partition the data
nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <-
  window(ridership.ts,
         start = c(1991, 1),
         end = c(1991, nTrain))
valid.ts <- window(
  ridership.ts,
  start = c(1991, nTrain + 1),
  end = c(1991, nTrain + nValid)
)
# moving average on training
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

# models
ses <- ets(train.ts)
auto_arima_model <- auto.arima(train.ts)

# Predictions
ses_pred <- forecast(ses, h = 48)
arima_pred <- forecast(auto_arima_model, h = 48)

# in-sample predictive power
accuracy(ses$fitted, train.ts)
accuracy(auto_arima_model$fitted, train.ts)

# Out-of-sample predictive power
accuracy(ses_pred, valid.ts)
accuracy(arima_pred, valid.ts)

mean(valid.ts)
plot(
  valid.ts,
  ylab = "Ridership",
  xlab = "Time",
  bty = "l",
  xaxt = "n",
  main = "",
  flty = 2,
  lwd = 2
)
lines(ses$fitted, col = "blue")
lines(auto_arima_model$fitted, col = "red")

plot(
  valid.ts,
  ylab = "Ridership",
  xlab = "Time",
  bty = "l",
  xaxt = "n",
  main = "",
  ylim = c(40000000,70000000),
  flty = 2,
  lwd = 2
)
lines(ses_pred$lower[,2], col = "purple")
lines(ses_pred$upper[,2], col = "purple")
lines(ses_pred$mean, col = "blue", lty = 2)
lines(arima_pred$mean, col = "red", lty = 2)

summary(ses)
plot(
  train.ts,
  ylab = "Ridership",
  xlab = "Time",
  bty = "l",
  # xaxt = "n",
  main = "",
  flty = 2,
  lwd = 2
)
lines(ses$fitted, col = "blue")



ses.pred <- forecast(ses, h = nValid, level = 0)
plot(
  ses.pred,
  ylim = c(-250, 2200),
  ylab = "Ridership",
  xlab = "Time",
  bty = "l",
  xaxt = "n",
  xlim = c(1991, 2006.25),
  main = "",
  flty = 2
)
train.lm.trend.season.pred <-
  forecast(train.lm.trend.season, h = 36, level = 0)
lines(train.lm.trend.season.pred$fitted,
      lwd = 2,
      col = "blue")
lines(train.lm.trend.season.pred$mean,
      lwd = 1,
      col = "purple")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ses.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
lines(train.ts)
lines(train.lm.trend.season.pred$mean + ses.pred$mean, col = "red")
# train.lm.trend.season.pred$mean + ses.pred$mean

accuracy(train.lm.trend.season.pred$mean + ses.pred$mean, valid.ts)
accuracy(train.lm.trend.season.pred$mean, valid.ts)

train.lm.trend.season.pred <-
  forecast(train.lm.trend.season, h = 12, level = 0)
train.lm.trend.season.pred$mean
residuals.ts <- train.lm.trend.season$residuals
ses <- ets(residuals.ts, model = "ANN", alpha = 0.2)
ses.pred <- forecast(ses, h = 12, level = 0)
ses.pred$mean


plot(
  ses.pred,
  ylim = c(-250, 2400),
  ylab = "Ridership",
  xlab = "Time",
  bty = "l",
  xaxt = "n",
  xlim = c(1991, 2006.25),
  main = "",
  flty = 2
)
combined_forecast <- ses.pred$mean + train.lm.trend.season.pred$mean
lines(combined_forecast, col = "red")
lines(valid.ts)
lines(train.ts)

ses <- ets(residuals.ts, model = "ZZZ", alpha = 0.2)
ses.pred <- forecast(ses, h = nValid, level = 0)
