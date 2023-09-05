rm(list=ls(all=TRUE))

data<-read.csv("bicup2006.csv")
data

View(data)

bicup.ts <- ts(data$DEMAND)

plot(bicup.ts)

library(dplyr)
library(lubridate)

data$DATETIME<-parse_date_time(paste(data$DATE,data$TIME),orders = "dmy HM")
data

df<-data[c("DATETIME","DEMAND")]
df



train.ts = window(bicup.ts, start=c(1), end=c(882))
autoplot(train.ts)                    


daily <- ts(df$DEMAND, start = c(1,1), frequency = 63)
autoplot(daily, facets = FALSE) + xlab("DAYS") + ylab("DEMAND") + ggtitle("Time Series for the daily demand")


weekly<- ts(df$DEMAND, start = c(1,1), frequency = 441)
autoplot(weekly, facets = FALSE) + xlab("WEEKS") + ylab("DEMAND")

decompose_weekly_add = decompose(weekly, "additive")
autoplot(decompose_weekly_add)


nValid<-633
ntest<-189
nTrain <- length(weekly) - nValid
train.ts <- window(weekly, start = c(1, 1), end = c(1, nTrain))
valid.ts <- window(weekly, start = c(1, nTrain+1), end = c(1, nTrain+nValid-ntest))
test.ts<-window(weekly, start = c(1, nTrain+nValid-ntest+1), end = c(1, nTrain+nValid))
plot(train.ts)
plot(valid.ts)
plot(test.ts)




aver.pred <- meanf(train.ts, h=441)
autoplot(aver.pred)

snaive.pred <- snaive(train.ts, h = 441)
autoplot(snaive.pred)

accuracy(snaive.pred,valid.ts)





plot(train.ts, ylim = c(1, 150), ylab = "Demand", xlab = "Weekly", bty = "l",
     xaxt = "n", xlim = c(1,4), main = "")
#axis(1, at = seq(1, 3.9, 1/7), labels = rep(substr(days, 1, 2), 3))
lines(snaive.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(valid.ts, col = "grey20", lty = 3)
lines(c(4, 4), c(0, 150), col='green')
lines(c(3, 3), c(0, 150), col='green')
text(2, 150, "Training")
text(3.5, 150, "Validation")




train.lm1 <- tslm(train.ts ~ season)
train.lm.pred1 <- forecast(train.lm1, h = 441)

accuracy(train.lm.pred1,valid.ts)



train.lm2 <- tslm(train.ts ~trend+season)
train.lm.pred2 <- forecast(train.lm2, h = 633)

accuracy(train.lm.pred2,valid.ts)
plot(train.lm.pred2)

bicup.hw<- ets(train.ts, model = "ZZZ")
hw.pred<-forecast(bicup.hw,h=633)
lines(valid.ts)


accuracy(hw.pred, valid.ts)

plot(hw.pred)

test.pred<-snaive(train.ts,h=630)



plot(train.ts, ylim = c(1, 150), ylab = "Demand", xlab = "Weekly", bty = "l",
     xaxt = "n", xlim = c(1,5), main = "")
#axis(1, at = seq(1, 3.9, 1/7), labels = rep(substr(days, 1, 2), 3))
lines(test.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(valid.ts, col = "grey20", lty = 3)
lines(c(4, 4), c(0, 150), col='green')
lines(c(3, 3), c(0, 150), col='green')
text(2, 150, "Training")
text(3.5, 150, "Validation")
text(4.4,150,"test")




