library(dplyr)
library(ggplot2)
library(fBasics)
library(moments)
require(xts) # attache the package to convert our dataframes into an xts (time-series) object for simplicity in calculations
require(zoo)
library(tidyquant)
library(moments)
library(stats)
library(lawstat) 
library(PerformanceAnalytics)
library(readr)
library(nortest)
library (ADGofTest)
library(rugarch)




setwd("C:/Users/pepec/Desktop/estudios/master/semester 2/Financial modeling/Session 4") 


# Time series

ts1 <- read.csv("Gold.csv")
ts2 <- read.csv("Silver.csv")
ts3 <- read.csv("10Y-Notes.csv")
ts4 <- read.csv("30Y-Bond.csv")
ts5 <- read.csv("Binance.csv")
ts6 <- read.csv("Bitcoin.csv")
ts7 <- read.csv("Blackberry.csv")
ts8 <- read.csv("Intel.csv")
ts9 <- read.csv("Uber.csv")
ts10 <- read.csv("Walmart.csv")

# Returns for each time serie

# Gold -------------------------------------------------------------------------------------

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("GC=F", from = '2017-12-31',
           to = "2023-01-01",warnings = FALSE,
           auto.assign = TRUE) # getting Gold prices

Gold <- `GC=F`

chart_Series(Gold$`GC=F.Close`)

gold.xts = Gold$`GC=F.Close`

return1 = diff (log(gold.xts)) # Log return calculation
return1 = return1 [-1] # removing the first empty observation, received after return calculation
summary (return1)

plot(return1, main = "Gold daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

# Silver ----------------------------------------------------------------------------------------

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("SI=F", from = '2017-12-31',
           to = "2023-01-01",warnings = FALSE,
           auto.assign = TRUE) # getting SP600 prices

Silver <- `SI=F`

chart_Series(Silver$`SI=F.Close`)

silver.xts = Silver$`SI=F.Close`

return2 = diff (log(silver.xts)) # Log return calculation
return2 = return2 [-1] # removing the first empty observation, received after return calculation
summary (return2)

plot(return2, main = "Silver daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

# 10 years note ----------------------------------------------------------------

#getting $T.Notes prices
T.Notes_data <- read_csv("10Y-Notes.csv")
T.Notes_price <- T.Notes_data %>%
  mutate(Time = as.Date(format(as.Date(Date, format = "%m/%d/%Y"), "%Y-%m-%d"))) %>%
  rename(PriceUSD = "Close/Last") %>%
  select(Time, PriceUSD) %>%
  filter(Time >= "2020-06-11" & Time <= "2023-05-03") %>%
  arrange(Time)

T.Notes_price_xts <- xts(T.Notes_price$PriceUSD, order.by = T.Notes_price$Time)


return3 = diff (log(T.Notes_price_xts)) # Log return calculation
return3 = return3 [-1] # removing the first empty observation, received after return calculation
summary (return3)

plot(return3, main = "10Y notes daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

# 5 years bonds ---------------------------------------------------------------

T.Bond_data <- read.csv("30Y-Bond.csv")
T.Bond_price <- T.Bond_data %>%
  mutate(Time = as.Date(format(as.Date(Date, format = "%m/%d/%Y"), "%Y-%m-%d"))) %>%
  rename(PriceUSD = Close.Last) %>%
  select(Time, PriceUSD) %>%
  arrange(Time)

T.Bond_price_xts <- xts(T.Bond_price$PriceUSD, order.by = T.Bond_price$Time)


return4 = diff (log(T.Bond_price_xts)) # Log return calculation
return4 = return4 [-1] # removing the first empty observation, received after return calculation
summary (return4)

plot(return4, main = "5 year bond daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

# Binance ----------------------------------------------------------------------

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("BNB-USD", from = '2017-12-31',
           to = "2023-01-01",warnings = FALSE,
           auto.assign = TRUE) # getting SP600 prices

Binance <- `BNB-USD`

chart_Series(Binance$`BNB-USD.Close`)

binance.xts = Binance$`BNB-USD.Close`

return5 = diff (log(binance.xts)) # Log return calculation
return5 = return5 [-1] # removing the first empty observation, received after return calculation
summary (return5)

plot(return5, main = "BNB-USD daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

# Bitcoin ----------------------------------------------------------------------

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("BTC-USD", from = '2017-12-31',
           to = "2023-01-01",warnings = FALSE,
           auto.assign = TRUE) # getting SP600 prices

Bitcoin <- `BTC-USD`

chart_Series(Bitcoin$`BTC-USD.Close`)

bitcoin.xts = Bitcoin$`BTC-USD.Close`

return6 = diff (log(bitcoin.xts)) # Log return calculation
return6 = return6 [-1] # removing the first empty observation, received after return calculation
summary (return6)

plot(return6, main = "Bitcoin daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

# Blackberry -------------------------------------------------------------------

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("BB", from = '2017-12-31',
           to = "2023-01-01",warnings = FALSE,
           auto.assign = TRUE) # getting SP600 prices

chart_Series(BB$BB.Close)

bb.xts = BB$BB.Close

return7 = diff (log(bb.xts)) # Log return calculation
return7 = return7 [-1] # removing the first empty observation, received after return calculation
summary (return7)

plot(return7, main = "BlackBerry daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

# Intel ------------------------------------------------------------------------

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("INTC", from = '2017-12-31',
           to = "2023-01-01",warnings = FALSE,
           auto.assign = TRUE) # getting SP600 prices

chart_Series(INTC$INTC.Close)

intel.xts = INTC$INTC.Close

return8 = diff (log(intel.xts)) # Log return calculation
return8 = return8 [-1] # removing the first empty observation, received after return calculation
summary (return8)

plot(return8, main = "Intel daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

# Uber -------------------------------------------------------------------------

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("UBER", from = '2017-12-31',
           to = "2023-01-01",warnings = FALSE,
           auto.assign = TRUE) # getting SP600 prices

chart_Series(UBER$UBER.Close)

uber.xts = UBER$UBER.Close

return9 = diff (log(uber.xts)) # Log return calculation
return9 = return9 [-1] # removing the first empty observation, received after return calculation
summary (return9)

plot(return9, main = "Uber daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

# Walmart ----------------------------------------------------------------------

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("WMT", from = '2017-12-31',
           to = "2023-01-01",warnings = FALSE,
           auto.assign = TRUE) # getting SP600 prices

chart_Series(WMT$WMT.Close)

walmart.xts = WMT$WMT.Close

return10 = diff (log(walmart.xts)) # Log return calculation
return10 = return10 [-1] # removing the first empty observation, received after return calculation
summary (return10)

plot(return10, main = "Walmart daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph


# Fortify return

return1_2 = fortify.zoo(return1)
return2_2 = fortify.zoo(return2)
return3_2 = fortify.zoo(return3)
return4_2 = fortify.zoo(return4)
return5_2 = fortify.zoo(return5)
return6_2 = fortify.zoo(return6)
return7_2 = fortify.zoo(return7)
return8_2 = fortify.zoo(return8)
return9_2 = fortify.zoo(return9)
return10_2 = fortify.zoo(return10)



### Distributions


## Location

# calculate mean of the return
mean (return1, na.rm = TRUE) #gold
mean (return2, na.rm = TRUE) #silver
mean (return3, na.rm = TRUE) #t-notes
mean (return4, na.rm = TRUE) #bond
mean (return5, na.rm = TRUE) #binance
mean (return6, na.rm = TRUE) #bitcoin
mean (return7, na.rm = TRUE) #BB
mean (return8, na.rm = TRUE) #Intel
mean (return9, na.rm = TRUE) #Uber
mean (return10, na.rm = TRUE) #Walmart


# calculate trimmed mean (10 per cent is trimmed) or the return
mean (return1, trim=0.1, na.rm = TRUE) 
floor (0.1*nrow(return1))

mean (return2, trim=0.1, na.rm = TRUE) 
floor (0.1*nrow(return2))

mean (return3, trim=0.1, na.rm = TRUE) 
floor (0.1*nrow(return3))

mean (return4, trim=0.1, na.rm = TRUE) 
floor (0.1*nrow(return4))

mean (return5, trim=0.1, na.rm = TRUE) 
floor (0.1*nrow(return5))
mean (return6, trim=0.1, na.rm = TRUE) 
floor (0.1*nrow(return6))

mean (return7, trim=0.1, na.rm = TRUE) 
floor (0.1*nrow(return7))

mean (return8, trim=0.1, na.rm = TRUE) 
floor (0.1*nrow(return8))

mean (return9, trim=0.1, na.rm = TRUE) 
floor (0.1*nrow(return9))

mean (return10, trim=0.1, na.rm = TRUE) 
floor (0.1*nrow(return10))


# calculate trimmed mean (20 per cent is trimmed) or the return
mean (return1, trim=0.2, na.rm = TRUE) 
floor (0.2*nrow(return1)) 

mean (return2, trim=0.2, na.rm = TRUE) 
floor (0.2*nrow(return2)) 

mean (return3, trim=0.2, na.rm = TRUE) 
floor (0.2*nrow(return3)) 

mean (return4, trim=0.2, na.rm = TRUE) 
floor (0.2*nrow(return4)) 

mean (return5, trim=0.2, na.rm = TRUE) 
floor (0.2*nrow(return5)) 

mean (return6, trim=0.2, na.rm = TRUE) 
floor (0.2*nrow(return6)) 

mean (return7, trim=0.2, na.rm = TRUE) 
floor (0.2*nrow(return7)) 

mean (return8, trim=0.2, na.rm = TRUE) 
floor (0.2*nrow(return8)) 

mean (return9, trim=0.2, na.rm = TRUE) 
floor (0.2*nrow(return9)) 

mean (return10, trim=0.2, na.rm = TRUE) 
floor (0.2*nrow(return10)) 


# calculate the median value of the return
median(return1, na.rm = TRUE) 
median(return2, na.rm = TRUE) 
median(return3, na.rm = TRUE) 
median(return4, na.rm = TRUE) 
median(return5, na.rm = TRUE) 
median(return6, na.rm = TRUE) 
median(return7, na.rm = TRUE) 
median(return8, na.rm = TRUE) 
median(return9, na.rm = TRUE) 
median(return10, na.rm = TRUE) 


## Estimates of Variability

# Mean absolute deviation
mean(abs(return1), na.rm = TRUE) 
mean(abs(return2), na.rm = TRUE) 
mean(abs(return3), na.rm = TRUE) 
mean(abs(return4), na.rm = TRUE) 
mean(abs(return5), na.rm = TRUE) 
mean(abs(return6), na.rm = TRUE) 
mean(abs(return7), na.rm = TRUE) 
mean(abs(return8), na.rm = TRUE) 
mean(abs(return9), na.rm = TRUE) 
mean(abs(return10), na.rm = TRUE) 

# Variance
var(return1, na.rm = TRUE) 
var(return2, na.rm = TRUE) 
var(return3, na.rm = TRUE) 
var(return4, na.rm = TRUE) 
var(return5, na.rm = TRUE) 
var(return6, na.rm = TRUE) 
var(return7, na.rm = TRUE) 
var(return8, na.rm = TRUE) 
var(return9, na.rm = TRUE) 
var(return10, na.rm = TRUE) 

# Standard deviation
sd(return1, na.rm = TRUE) 
sd(return2, na.rm = TRUE) 
sd(return3, na.rm = TRUE) 
sd(return4, na.rm = TRUE) 
sd(return5, na.rm = TRUE) 
sd(return6, na.rm = TRUE) 
sd(return7, na.rm = TRUE) 
sd(return8, na.rm = TRUE) 
sd(return9, na.rm = TRUE) 
sd(return10, na.rm = TRUE) 

# Median Absolute Deviation
mad (return1, na.rm = TRUE) 
mad (return2, na.rm = TRUE) 
mad (return3, na.rm = TRUE) 
mad (return4, na.rm = TRUE) 
mad (return5, na.rm = TRUE) 
mad (return6, na.rm = TRUE) 
mad (return7, na.rm = TRUE) 
mad (return8, na.rm = TRUE) 
mad (return9, na.rm = TRUE) 
mad (return10, na.rm = TRUE) 


## Estimates Based on Percentiles

# values in the 0, 25%, 50%, 75%, 100% of the distribution
quantile (return1, na.rm = TRUE) 
quantile (return2, na.rm = TRUE) 
quantile (return3, na.rm = TRUE) 
quantile (return4, na.rm = TRUE) 
quantile (return5, na.rm = TRUE) 
quantile (return6, na.rm = TRUE) 
quantile (return7, na.rm = TRUE) 
quantile (return8, na.rm = TRUE) 
quantile (return9, na.rm = TRUE) 
quantile (return10, na.rm = TRUE) 

# values in the 5th and 95th quantiles
quantile (return1, probs=c(0.05, 0.95), na.rm = TRUE) 
quantile (return2, probs=c(0.05, 0.95), na.rm = TRUE) 
quantile (return3, probs=c(0.05, 0.95), na.rm = TRUE) 
quantile (return4, probs=c(0.05, 0.95), na.rm = TRUE) 
quantile (return5, probs=c(0.05, 0.95), na.rm = TRUE) 
quantile (return6, probs=c(0.05, 0.95), na.rm = TRUE) 
quantile (return7, probs=c(0.05, 0.95), na.rm = TRUE) 
quantile (return8, probs=c(0.05, 0.95), na.rm = TRUE) 
quantile (return9, probs=c(0.05, 0.95), na.rm = TRUE) 
quantile (return10, probs=c(0.05, 0.95), na.rm = TRUE) 


tauseq = seq(.1,.95,.1) # generate sequence of numbers that will be used as quantiles

quantile (return1, tauseq, na.rm = TRUE)
quantile (return2, tauseq, na.rm = TRUE)
quantile (return3, tauseq, na.rm = TRUE)
quantile (return4, tauseq, na.rm = TRUE)
quantile (return5, tauseq, na.rm = TRUE)
quantile (return6, tauseq, na.rm = TRUE)
quantile (return7, tauseq, na.rm = TRUE)
quantile (return8, tauseq, na.rm = TRUE)
quantile (return9, tauseq, na.rm = TRUE)
quantile (return10, tauseq, na.rm = TRUE)

# IQRx = Qx(0,75) - Qx(0.25)
IQR (return1, na.rm = TRUE) 
IQR (return2, na.rm = TRUE) 
IQR (return3, na.rm = TRUE) 
IQR (return4, na.rm = TRUE) 
IQR (return5, na.rm = TRUE) 
IQR (return6, na.rm = TRUE) 
IQR (return7, na.rm = TRUE) 
IQR (return8, na.rm = TRUE) 
IQR (return9, na.rm = TRUE) 
IQR (return10, na.rm = TRUE) 


# boxplot
boxplot(return1, horizontal=TRUE, main = "Return gold")
boxplot(return2, horizontal=TRUE, main = "Return silver")
boxplot(return3, horizontal=TRUE, main = "Return t-notes")
boxplot(return4, horizontal=TRUE, main = "Return t-bond")
boxplot(return5, horizontal=TRUE, main = "Return binance")
boxplot(return6, horizontal=TRUE, main = "Return bitcoin")
boxplot(return7, horizontal=TRUE, main = "Return blackberry")
boxplot(return8, horizontal=TRUE, main = "Return intel")
boxplot(return9, horizontal=TRUE, main = "Return uber")
boxplot(return10, horizontal=TRUE, main = "Return walmart")

# bild a histograme of the return
hist(return1)
hist(return2)
hist(return3)
hist(return4)
hist(return5)
hist(return6)
hist(return7)
hist(return8)
hist(return9)
hist(return10)


# estimate kernel density & plot the estimated density 
return1.density = density(return1, na.rm = TRUE) 
plot (return1.density) 

return2.density = density(return2, na.rm = TRUE) 
plot (return2.density, main = "Silver Return") 

return3.density = density(return3, na.rm = TRUE) 
plot (return3.density) 

return4.density = density(return4, na.rm = TRUE) 
plot (return4.density) 

return5.density = density(return5, na.rm = TRUE) 
plot (return5.density) 

return6.density = density(return6, na.rm = TRUE) 
plot (return6.density) 

return7.density = density(return7, na.rm = TRUE) 
plot (return7.density) 

return8.density = density(return8, na.rm = TRUE) 
plot (return8.density) 

return9.density = density(return9, na.rm = TRUE) 
plot (return9.density) 

return10.density = density(return10, na.rm = TRUE) 
plot (return10.density) 


#QQ
qqnorm(return1)
qqline(return1)

qqnorm(return2)
qqline(return2)

qqnorm(return3)
qqline(return3)

qqnorm(return4)
qqline(return4)

qqnorm(return5)
qqline(return5)

qqnorm(return6)
qqline(return6)

qqnorm(return7)
qqline(return7)

qqnorm(return8)
qqline(return8)

qqnorm(return9)
qqline(return9)

qqnorm(return10)
qqline(return10)


# Half-Normal Plots with label 2 & º0 of the most outlying cases

#halfnorm(abs(return1), main = "return", ylab = "Sorted data")
#halfnorm(abs(return1),nlab = 10, main = "return", ylab = "Sorted data") 

#halfnorm(abs(return2), main = "return", ylab = "Sorted data")
#halfnorm(abs(return2),nlab = 10, main = "return", ylab = "Sorted data") 

# halfnorm(abs(return3), main = "return", ylab = "Sorted data")
# halfnorm(abs(return3),nlab = 10, main = "return", ylab = "Sorted data") 
# 
# halfnorm(abs(return4), main = "return", ylab = "Sorted data")
# halfnorm(abs(return4),nlab = 10, main = "return", ylab = "Sorted data") 
# 
# halfnorm(abs(return5), main = "return", ylab = "Sorted data")
# halfnorm(abs(return5),nlab = 10, main = "return", ylab = "Sorted data") 
# 
# halfnorm(abs(return6), main = "return", ylab = "Sorted data")
# halfnorm(abs(return6),nlab = 10, main = "return", ylab = "Sorted data") 
# 
# halfnorm(abs(return7), main = "return", ylab = "Sorted data")
# halfnorm(abs(return7),nlab = 10, main = "return", ylab = "Sorted data") 
# 
# halfnorm(abs(return8), main = "return", ylab = "Sorted data")
# halfnorm(abs(return8),nlab = 10, main = "return", ylab = "Sorted data") 
# 
# halfnorm(abs(return9), main = "return", ylab = "Sorted data")
# halfnorm(abs(return9),nlab = 10, main = "return", ylab = "Sorted data") 
# 
# halfnorm(abs(return10), main = "return", ylab = "Sorted data")
# halfnorm(abs(return10),nlab = 10, main = "return", ylab = "Sorted data")



# Shapiro-Wilk -> Is the data a normal distribution? Yes > 0,05, No < 0,05

shapiro.test(return1_2$`GC=F.Close`) # No
shapiro.test(return2_2$`SI=F.Close`) # No
shapiro.test(return3_2$return3) # No
shapiro.test(return4_2$return4) # No
shapiro.test(return5_2$`BNB-USD.Close`)  # No
shapiro.test(return6_2$`BTC-USD.Close`) # No
shapiro.test(return7_2$BB.Close) # No
shapiro.test(return8_2$INTC.Close) # No
shapiro.test(return9_2$UBER.Close) # No
shapiro.test(return10_2$WMT.Close) # No


# Kolmogorov-Smirnov -> Is the data part of a normal distribution? Yes > 0,05, No < 0,05

ks.test(return1_2$`GC=F.Close`,"pnorm") # No
ks.test(return2_2$`SI=F.Close`,"pnorm") # No
ks.test(return3_2$return3,"pnorm") # No
ks.test(return4_2$return4,"pnorm") # No
ks.test(return5_2$`BNB-USD.Close`,"pnorm") # No
ks.test(return6_2$`BTC-USD.Close`,"pnorm") # No
ks.test(return7_2$BB.Close,"pnorm") # No
ks.test(return8_2$INTC.Close,"pnorm") # No
ks.test(return9_2$UBER.Close,"pnorm") # No
ks.test(return10_2$WMT.Close,"pnorm") # No



# Jarque-Bera -> data have the skewness and kurtosis matching a normal distribution? Yes > 0,05, No < 0,05

jarqueberaTest(return1_2$`GC=F.Close`) # No
jarqueberaTest(return2_2$`SI=F.Close`) # No
jarqueberaTest(return3_2$return3) # No
jarqueberaTest(return4_2$return4) # No
jarqueberaTest(return5_2$`BNB-USD.Close`) # No
jarqueberaTest(return6_2$`BTC-USD.Close`) # No
jarqueberaTest(return7_2$BB.Close) # No
jarqueberaTest(return8_2$INTC.Close) # No
jarqueberaTest(return9_2$UBER.Close) # No
jarqueberaTest(return10_2$WMT.Close) # No



# D´Agostino ->  is the shape of the distribution similar to the shape of the normal distribution? Yes > 0,05, No < 0,05

agostino.test(return1, alternative = "two.sided") # No
agostino.test(return2, alternative = "two.sided") # No
agostino.test(return3, alternative = "two.sided") # No
agostino.test(return4, alternative = "two.sided") # No
agostino.test(return5, alternative = "two.sided") # No
agostino.test(return6, alternative = "two.sided") # No
agostino.test(return7, alternative = "two.sided") # No
agostino.test(return8, alternative = "two.sided") # No
agostino.test(return9, alternative = "two.sided") # No
agostino.test(return10, alternative = "two.sided") # No



# Anscombe-Glynn test of kurtosis -> is a statistical test used to assess whether a dataset has a normal distribution or exhibits significant deviations in the shape of its distribution from normality.

anscombe.test (return1, alternative = "two.sided" ) #No
anscombe.test (return2, alternative = "two.sided" ) #No
anscombe.test (return3, alternative = "two.sided" ) #No
anscombe.test (return4, alternative = "two.sided" ) #No
anscombe.test (return5, alternative = "two.sided" ) #No
anscombe.test (return6, alternative = "two.sided" ) #No
anscombe.test (return7, alternative = "two.sided" ) #No
anscombe.test (return8, alternative = "two.sided" ) #No
anscombe.test (return9, alternative = "two.sided" ) #No
anscombe.test (return10, alternative = "two.sided" ) #No



#Bonett-seier test of kurtosis -> is a statistical test used to determine whether a dataset's kurtosis significantly differs from that of a normal distribution

bonett.test (return1, alternative = "two.sided" ) #No
bonett.test (return2, alternative = "two.sided" ) #No
bonett.test (return3, alternative = "two.sided" ) #No
bonett.test (return4, alternative = "two.sided" ) #No
bonett.test (return5, alternative = "two.sided" ) #No
bonett.test (return6, alternative = "two.sided" ) #No
bonett.test (return7, alternative = "two.sided" ) #No
bonett.test (return8, alternative = "two.sided" ) #No
bonett.test (return9, alternative = "two.sided" ) #No
bonett.test (return10, alternative = "two.sided" ) #No



#Anderson-Darling goodness of fit test ->  is a statistical test used to determine whether a given sample of data comes from a particular distribution
ad.test (return1, plnorm) #No
ad.test (return2, plnorm) #No
ad.test (return3, plnorm) #No
ad.test (return4, plnorm) #No
ad.test (return5, plnorm) #No
ad.test (return6, plnorm) #No
ad.test (return7, plnorm) #No
ad.test (return8, plnorm) #No
ad.test (return9, plnorm) #No
ad.test (return10, plnorm) #No



#Two sample t-test for the difference in the mean of pairs of assets

# show the first 6 rows
head (Gold$`GC=F.Close`) 
head (Silver$`SI=F.Close`) 
head (T.Notes_price$PriceUSD) 
head (T.Bond_price$PriceUSD) 
head (Binance$`BNB-USD.Close`) 
head (Bitcoin$`BTC-USD.Close`) 
head (BB$BB.Close) 
head (INTC$INTC.Close) 
head (UBER$UBER.Close) 
head (WMT$WMT.Close) 


# merging 2 pairs of assets
SiGo =  merge(Silver$`SI=F.Close`,Gold$`GC=F.Close`, all=FALSE) 
BiBi =  merge(Binance$`BNB-USD.Close`,Bitcoin$`BTC-USD.Close`, all=FALSE) 
TnTb =  merge(T.Notes_price_xts,T.Bond_price_xts, all=FALSE)
WaUb =  merge(WMT$WMT.Close,UBER$UBER.Close, all=FALSE) 
Inbb =  merge(INTC$INTC.Close,BB$BB.Close, all=FALSE) 

TnSi =  merge(T.Notes_price_xts,Silver$`SI=F.Close`, all=FALSE)
SiBi = merge(Silver$`SI=F.Close`,Binance$`BNB-USD.Close`, all=FALSE)
SiWa = merge(Silver$`SI=F.Close`,WMT$WMT.Close, all=FALSE)
BiTn = merge(Binance$`BNB-USD.Close`,T.Notes_price_xts, all=FALSE)
BiWa = merge(Binance$`BNB-USD.Close`,WMT$WMT.Close, all=FALSE)
TnWa = merge(T.Notes_price_xts,WMT$WMT.Close, all=FALSE)

# Calculate log return
# the package to calculate return automaticaly 
return.SiGo = Return.calculate( SiGo , method = "log")
return.BiBi = Return.calculate( BiBi , method = "log")
return.TnTb = Return.calculate( TnTb , method = "log")
return.WaUb = Return.calculate( WaUb , method = "log")
return.Inbb = Return.calculate( Inbb , method = "log")

return.TnSi = Return.calculate( TnSi , method = "log")
return.SiBi = Return.calculate( SiBi , method = "log")
return.SiWa = Return.calculate( SiWa , method = "log")
return.BiTn = Return.calculate( BiTn , method = "log")
return.BiWa = Return.calculate( BiWa , method = "log")
return.TnWa = Return.calculate( TnWa , method = "log")

plot (return.SiGo)
plot (return.BiBi)
plot (return.TnTb)
plot (return.WaUb)
plot (return.Inbb)

plot (return.TnSi)
plot (return.SiBi)
plot (return.SiWa)
plot (return.BiTn)
plot (return.BiWa)
plot (return.TnWa)


#convert into dataframes for further usage

# convert the  xts object into a dataframe
return.SiGo.df = fortify(return.SiGo) 
return.SiGo.df = return.SiGo.df [-1,]

return.BiBi.df = fortify(return.BiBi) 
return.BiBi.df = return.BiBi.df [-1,]

return.TnTb.df = fortify(return.TnTb) 
return.TnTb.df = return.TnTb.df [-1,]

return.WaUb.df = fortify(return.WaUb) 
return.WaUb.df = return.WaUb.df [-1,]

return.Inbb.df = fortify(return.Inbb) 
return.Inbb.df = return.Inbb.df [-1,]



return.TnSi.df = fortify(return.TnSi) 
return.TnSi.df = return.TnSi.df [-1,]

return.SiBi.df = fortify(return.SiBi)
return.SiBi.df = return.SiBi.df [-1,]

return.SiWa.df = fortify(return.SiWa)
return.SiWa.df = return.SiWa.df [-1,]

return.BiTn.df = fortify(return.BiTn)
return.BiTn.df = return.BiTn.df [-1,]

return.BiWa.df = fortify(return.BiWa)
return.BiWa.df = return.BiWa.df [-1,]

return.TnWa.df = fortify(return.TnWa)
return.TnWa.df = return.TnWa.df [-1,]



## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
A = return.SiGo.df$SI.F.Close
B = return.SiGo.df$GC.F.Close
C = return.BiBi.df$BNB.USD.Close
D = return.BiBi.df$BTC.USD.Close
E = return.TnTb.df$T.Notes_price_xts
G = return.TnTb.df$T.Bond_price_xts
H = return.WaUb.df$WMT.Close
I = return.WaUb.df$UBER.Close
J = return.Inbb.df$INTC.Close
K = return.Inbb.df$BB.Close

l = return.TnSi.df$T.Notes_price_xts
m = return.TnSi.df$SI.F.Close
n = return.SiBi.df$SI.F.Close
o = return.SiBi.df$BNB.USD.Close
p = return.SiWa.df$SI.F.Close
q = return.SiWa.df$WMT.Close
r = return.BiTn.df$T.Notes_price_xts
s = return.BiTn.df$BNB.USD.Close
t = return.BiWa.df$WMT.Close
u = return.BiWa.df$BNB.USD.Close
v = return.TnWa.df$T.Notes_price_xts
w = return.TnWa.df$WMT.Close

#Two sample t-test for the difference in the mean of pairs of assets
t.test(A,B, alternative = "two.sided", var.equal=TRUE)
t.test(C,D, alternative = "two.sided", var.equal=TRUE)
t.test(E,G, alternative = "two.sided", var.equal=TRUE)
t.test(H,I, alternative = "two.sided", var.equal=TRUE)
t.test(J,K, alternative = "two.sided", var.equal=TRUE)


#volatility

# Define the ARMA-GARCH specification
arma.garch.norm <- ugarchspec(
  mean.model = list(armaOrder = c(1, 0)),
  variance.model = list(garchOrder = c(1, 1))
)

#estimating missing values with linear interpolation
clean_return1<- na.approx(return1)
clean_return2<- na.approx(return2)
clean_return3<- na.approx(return3)
clean_return4<- na.approx(return4)
clean_return5<- na.approx(return5)
clean_return6<- na.approx(return6)
clean_return7<- na.approx(return7)
clean_return8<- na.approx(return8)
clean_return9<- na.approx(return9)
clean_return10<- na.approx(return10)

# Estimate the GARCH model
gold.garch.norm <- ugarchfit(data = clean_return1, spec = arma.garch.norm)
silver.garch.norm <- ugarchfit(data = clean_return1, spec = arma.garch.norm)
tnotes.garch.norm <- ugarchfit(data = clean_return1, spec = arma.garch.norm)
bond.garch.norm <- ugarchfit(data = clean_return1, spec = arma.garch.norm)
binance.garch.norm <- ugarchfit(data = clean_return1, spec = arma.garch.norm)
bitcoin.garch.norm <- ugarchfit(data = clean_return1, spec = arma.garch.norm)
bb.garch.norm <- ugarchfit(data = clean_return1, spec = arma.garch.norm)
intel.garch.norm <- ugarchfit(data = clean_return1, spec = arma.garch.norm)
uber.garch.norm <- ugarchfit(data = clean_return1, spec = arma.garch.norm)
walmart.garch.norm <- ugarchfit(data = clean_return1, spec = arma.garch.norm)


# Show the estimated model
show(gold.garch.norm)
show(silver.garch.norm)
show(tnotes.garch.norm)
show(bond.garch.norm)
show(binance.garch.norm)
show(bitcoin.garch.norm)
show(bb.garch.norm)
show(intel.garch.norm)
show(uber.garch.norm)
show(walmart.garch.norm)


# Correlation 

#Pearson's product moment correlation coefficient t-test

cor.test (l,m, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(l,m)

cor.test (n,o, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(n,o)

cor.test (p,q, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(p,q)

cor.test (r,s, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(r,s)

cor.test (t,u, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(t,u)

cor.test (v,w, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(v,w)

#spearman rank correlation test
cor.test(l,m, method="spearman",alternative="two.sided", exact = FALSE)
cor.test(n,o, method="spearman",alternative="two.sided", exact = FALSE)
cor.test(p,q, method="spearman",alternative="two.sided", exact = FALSE)
cor.test(r,s, method="spearman",alternative="two.sided", exact = FALSE)
cor.test(t,u, method="spearman",alternative="two.sided", exact = FALSE)
cor.test(v,w, method="spearman",alternative="two.sided", exact = FALSE)

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(l,m,method="kendal",alternative="two.sided")
cor.test(n,o,method="kendal",alternative="two.sided")
cor.test(p,q,method="kendal",alternative="two.sided")
cor.test(r,s,method="kendal",alternative="two.sided")
cor.test(t,u,method="kendal",alternative="two.sided")
cor.test(v,w,method="kendal",alternative="two.sided")
