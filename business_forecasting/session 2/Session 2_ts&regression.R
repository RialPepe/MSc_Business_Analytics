#### Figure 16.1

library(forecast)
Amtrak.data <- read.csv("Amtrak data.csv")

# create time series object using ts()
# ts() takes three arguments: start, end, and freq.
# with monthly data, the frequency of periods per season is 12 (per year).
# arguments start and end are (season number, period number) pairs.
# here start is Jan 1991: start = c(1991, 1); end is Mar 2004: end = c(2004, 3).
ridership.ts <- ts(Amtrak.data$Ridership,
                   start = c(1991, 1), end = c(2004, 3), freq = 12)

# plot the series
plot(ridership.ts, xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))
plot(ridership.ts)

# Add the Moving average
lines(ma(ridership.ts, order = 12, centre = TRUE), col = "blue")

# Zoom to three years
ridership.ts.3yrs <- window(ridership.ts, start = c(1997, 1), end = c(1999, 12))
plot(ridership.ts.3yrs, ylim = c(1300,2200))

# Naive benchmark
lines(ma(ridership.ts, order = 12, centre = TRUE), col = "blue")

# Seasonal naive
seasonal_naive <- ts(Amtrak.data[61:96,2],
                     start = c(1997, 1), end = c(1999, 12), freq = 12)
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


