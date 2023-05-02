install.packages("forecast")
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

# Zoom to three years
ridership.ts.3yrs <- window(ridership.ts, start = c(1997, 1), end = c(1999, 12))
plot(ridership.ts.3yrs)

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
