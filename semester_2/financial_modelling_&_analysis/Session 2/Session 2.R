## MSc Business Analitics
##Financial Modelling and Analysis

setwd("~/R/Trinity/Session2") # setting a working directory to access cvs data file

require (xts) # attache the package to convert our dataframes into an xts (time-series) object for simplicity in calculations
require(zoo)

library(tidyquant)

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("^SP600", from = '2010-12-31',
           to = "2023-01-01",warnings = FALSE,
           auto.assign = TRUE) # getting SP600 prices

chart_Series(SP600$SP600.Close) #plotting the series

data.xts = SP600$SP600.Close

return = diff (log(data.xts)) # Log return calculation
return = return [-1] # removing the first empty observation, received after return calculation
summary (return)

plot(return, main = "S&P 600 daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph


### Distributions

## Location
mean (return) # calculate mean of the return
mean (return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(return) # calculate the median value of the return

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(return)

# Variance
var(return)

# Standard deviation
sd(return)

# Median Absolute Deviation
mad (return)



# Estimates Based on Percentiles
quantile (return)
quantile (return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (return, tauseq)

IQR (return) # IQRx = Qx(0,75) - Qx(0.25)



# probability calculation : The binomial probability function
1/prod(40:36) # prod(40:36) = 40*39*38*37*36

#the lines below return thesame results
prod(5:1)/prod(40:36)
1/choose(40,5)

# The binomial probability function - picking up stocks
# You have a pool of stocks having returns either above 5% or below 5%. 
# The probability of selecting a stock with above 5% returns is 0.10. 
# You are going to pick up 5 stocks. Assuming binomial distribution, what is the probability of picking 2 stocks with above 5% returns?
p = 0.1
n = 5
x = 2

(factorial(n)/(factorial(n-x)*factorial(x)))*p^x*(1-p)^(n-x) # manual calculation as in formula
dbinom(2, size=5, prob=0.1) # R function for binominal distribution


# boxplot
return2 = diff (log(SP600$SP600.Close)) # return as a vector
boxplot(return2, horizontal=TRUE, main="SP 600 Return")


hist(return) # bild a histograme of the return

# kernel density
return.density = density(return) # estimate kernel density
plot (return.density) # plot the estimated density

#QQ
qqnorm(return)
qqline(return)


# Half-Normal Plots
library (faraway)
halfnorm(abs(return), main = "return", ylab = "Sorted data") # label only 2 the most outlying cases
halfnorm(abs(return),nlab = 10, main = "return", ylab = "Sorted data") # label 10 the most outlying cases


## Tests for normality

## Shapiro-Wilk test 
return2 = fortify.zoo(return)
shapiro.test(return2$SP600.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

#skewness
library(e1071) # the library to calculate skewness
skewness (return2$SP600.Close)
kurtosis(return2$SP600.Close)



# D'AGOSTINO TEST OF SKEWNESS
library (moments)
agostino.test(return, alternative = "two.sided") #	D'Agostino skewness test

#	D'Agostino skewness test

#data:  return
#skew = -1.3934, z = -15.8385, p-value < 2.2e-16
#alternative hypothesis: data have a skewness

# since p-value is lover than 0.05 we can reject H0, implying that the return SP600 is skewed 




# Anscombe-Glynn test of kurtosis

anscombe.test (return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

# 	Anscombe-Glynn kurtosis test

#data:  return
#kurt = 20.697, z = 17.829, p-value < 2.2e-16
#alternative hypothesis: kurtosis is not equal to 3
# since p-value is lover than 0.05 we can reject H0, implying that the return SP600 xhibit excess kurtosis relatively to to the normal distribution 


#Bonett-seier test of kurtosis
bonett.test (return, alternative = "two.sided" )

#Bonett-Seier test for Geary kurtosis

#data:  return
#tau = 0.0091766, z = 34.8911977, p-value < 2.2e-16
#alternative hypothesis: kurtosis is not equal to sqrt(2/pi)

# since p vallue is lover than 0.05, therefore we reject the null hypothesis, 
#the data exhibits excess Geary's measure of kurtosis relative to the normal distribution.



# Shapiro-Wilk test
shapiro.test (return2$SP600.Close) # input data should be a vector or a data frame column. That why we use return2

#	Shapiro-Wilk normality test

#data:  return2
#W = 0.82678, p-value < 2.2e-16

# since p value is lower than 0.05 we reject the null hypothesis that
# the data are from the normal distribution


# Kolmogorov-Smirnov
library (fBasics)
ks.test(return2, "pnorm")

# One-sample Kolmogorov-Smirnov test

#data:  return2
#D = 0.47516, p-value < 2.2e-16
#alternative hypothesis: two-sided 

# p-value is lower than 0.05, therefore reject the null hypothesis that the data are from the normal distribution

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(return2$SP600.Close)

#Title:
#Jarque - Bera Normalality Test
# since p value is lower than 0.05, we reject the null hypothesis that the data are from the normal distribution.

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (return2$SP600.Close, plnorm)



## Multivariate analysis
## I test correlation between SP600 (from teh previouse examples) and Bitcoin

##Data preparation ########################################################################################################

# import a time series of different frequency
Bitcoin = read.csv("BitcoinData.csv")
Bitcoin = Bitcoin[, -1]
Bitcoin$date = as.Date(Bitcoin$date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
Bitcoin.xts = xts(Bitcoin[,-1], order.by=Bitcoin[,1])  # converting a data frame into xts onject (time-series object)



plot (Bitcoin.xts$price)


# transforming 7 day week of Bitcoin observations into 5 day week observation (cutting weekends off)
# analyze the code yourself

weekdays.cryptos = data.frame ( # declare a new dataframe
  Date=as.Date(character()),
  price =numeric(),
  stringsAsFactors=FALSE
)

# transforming to weekdays
r = !(weekdays(as.Date(Bitcoin$date)) %in% c('Saturday','Sunday'))

i=1
j=1
while (i<=nrow(Bitcoin))  
{
  
  if (r[i]=='TRUE') {weekdays.cryptos [j,] = Bitcoin [i,]; j=j+1 } 
  
  i=i+1
  
  
} 
# end of transformation to 5 day week


weekdays.cryptos.xts = xts(weekdays.cryptos[,-1], order.by=weekdays.cryptos[,1]) # converting into xts object

head (SP600$SP600.Close) # show the first 6 rows
head (weekdays.cryptos.xts) #show the first 6 rows

tail (SP600$SP600.Close) #show the last 6 rows
tail (weekdays.cryptos.xts) #show the last 6 rows


together =  merge(SP600$SP600.Close,weekdays.cryptos.xts, all=FALSE) # merging 2 dataframes


# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
return.together = Return.calculate( together , method = "log")
plot (return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
return.together.df = fortify(return.together) # convert the  xts object into a dataframe
return.together.df = return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= return.together.df$SP600.Close
Y= return.together.df$weekdays.cryptos.xts


# Correlation 
require (fBasics) # attache the required package


#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)
# The correlation between x and y is reported as 0.14. 
# Since the p-value is lower than the critical value of 0.05 we reject the null hypothesis 
# of zero correlation. The function also reports the 95% confidence interval as 0.1036790 to 0.1736413. It doesnt crosses zero. 


#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")
#The Spearman rank correlation between x and y is .1. 

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")


#Two sample t-test for the difference in sample means
t.test(X,Y, alternative = "two.sided", var.equal=TRUE)



set.seed("119933") # to ensure the same result of random data generation
par(mfrow=c(1,2)) # to build one  figures in one row and two columns  
x1 = rlnorm(20,meanlog=1,sdlog=2) # generate random values from the Log Normal Distribution, mean of the distribution on the log scale
hist(x1) # build the histograms 

x2 = rlnorm(20,meanlog=3,sdlog=2)
hist(x2)

boxplot(list(x1,x2),main="(a) no transformation")
boxplot(list(log(x1),log(x2)),main="(b) log transformation")
t.test(x1,x2,equal.var=F)
t.test(log(x1),log(x2))



## Causality ##

library(yfR)
#  options 
my_ticker = c("VXX", "AMZN", "ABNB") 
first_date = Sys.Date() - 365*2 #First day
last_date = Sys.Date() #last day at the moment of running

# get  data
df_yf = yf_get(tickers = my_ticker, 
               first_date = first_date,
               last_date = last_date)

unique.tikers = unique(df_yf$ticker)

ABNB= df_yf[df_yf$ticker==unique.tikers[1], ] #"ABNB"
AMZN= df_yf[df_yf$ticker==unique.tikers[2], ] # "AMZN"
VXX= df_yf[df_yf$ticker==unique.tikers[3], ] # "VXX"


library (lmtest)
grangertest(AMZN$price_open ~ VXX$price_adjusted, order = 1)

grangertest(AMZN$price_open ~ ABNB$price_close, order = 1)

grangertest(AMZN$price_open ~ ABNB$volume, order = 1)

# The null hypothesis for the Granger causality test is that X does not Granger-cause Y.
# If the p-value is less than or equal to alpha, you reject the null hypothesis and conclude that X Granger-causes Y (i.e., past values of X help predict Y).
# If the p-value is greater than alpha, you fail to reject the null hypothesis and conclude that there is not enough evidence to suggest that X Granger-causes Y.



#causality based on VAR
library(vars)
data = cbind(return.together.df$SP600.Close, return.together.df$weekdays.cryptos.xts) # Combine the time series into a matrix
# Set the maximum number of lags to consider
max_lags = 10

# Calculate selection criteria for different lag orders
lag_selection = VARselect(data, lag.max = max_lags, type = "both")

# View the selection criteria
print(lag_selection)

# Extract the optimal lag order based on the AIC or BIC criterion
optimal_lag_aic = lag_selection$selection["AIC(n)"]
optimal_lag_bic = lag_selection$selection["SC(n)"]

# Estimate the VAR model with the optimal lag order
model_aic = VAR(data, p = optimal_lag_aic)
model_bic = VAR(data, p = optimal_lag_bic)

#To test for causality within a VAR model, you can use the causality() function:
causal_test = causality(model_aic, cause = "y1")




## Causality based on DAG
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("graph")
BiocManager::install("RBGL")
BiocManager::install("Rgraphviz")

library(pcalg)
library(graph)


# Load the required packages
library(pcalg)
library(ggm)

# Set the seed for reproducibility
set.seed(42)

# Number of observations
n = 1000

# Simulate data for six variables
X1 = rnorm(n)
X2 =  0.6 * X1 + rnorm(n, sd=0.8)
X3 = 0.7 * X1 - 0.3 * X2 + rnorm(n, sd=0.9)
X4 = 0.5 * X2 + rnorm(n, sd=1.0)
X5 = -0.8 * X3 + rnorm(n, sd=1.1)
X6 = 0.4 * X4 + 0.4 * X5 + rnorm(n, sd=1.2)

# Combine variables into a data frame
data  = data.frame(X1, X2, X3, X4, X5, X6)

# Compute the covariance matrix
cov_matrix = cor(data)

# Estimate the DAG structure using the PC algorithm
suffStat = list(C = cov_matrix, n = n)
dag = pc(suffStat, indepTest = gaussCItest, alpha = 0.05, labels = colnames(data))

# Plot the DAG
plot(dag, main="Estimated DAG")






library(yfR)
#  options 
my_ticker = c("VXX", "AMZN", "ABNB") 
first_date = Sys.Date() - 365 #First day
last_date = Sys.Date() #last day at the moment of running

# get  data
df_yf = yf_get(tickers = my_ticker, 
               first_date = first_date,
               last_date = last_date)

unique.tikers = unique(df_yf$ticker)

stock1= df_yf[df_yf$ticker==unique.tikers[1], ] # ABNB
stock2= df_yf[df_yf$ticker==unique.tikers[2], ] # AMZN
stock3= df_yf[df_yf$ticker==unique.tikers[3], ] # VXX

data = cbind(stock1$price_close, stock2$price_close, stock3$price_close, stock1$volume, stock2$volume, stock3$volume)
colnames(data) = c("stockPrice1", "stockPrice2", "stockPrice3", "stockVol1", "stockVol2", "stockVol3")

# Estimate the DAG:
suffStat = list(C = cor(data), n = nrow(data))
dag = pc(suffStat, indepTest = gaussCItest, alpha = 0.05, labels = colnames(data))


# Plot the estimated DAG:
library(Rgraphviz)
plot(dag)








## Spillover
# Install the required packages if not already installed
if (!requireNamespace("quantmod", quietly = TRUE)) {
  install.packages("quantmod")
}

if (!requireNamespace("Spillover", quietly = TRUE)) {
  install.packages("Spillover")
}

# Load the required packages
library(quantmod)
library(Spillover)

# Define the stock indices and their respective symbols
indices <- list(
  "S&P 500" = "^GSPC",
  "Nikkei 225" = "^N225",
  "DAX" = "^GDAXI"
)

# Download historical data for the stock indices
data = NULL
for (symbol in indices) {
  prices = getSymbols(symbol, auto.assign = FALSE, from = "2020-01-01")
  if (is.null(data)) {
    data = Ad(prices)
  } else {
    data = cbind(data, Ad(prices))
  }
}
# Calculate the daily returns
returns = na.omit(ROC(data, type = "discrete"))

## Estimate VAR
library(vars)

# Select the optimal lag order for the VAR model
lag_selection =  VARselect(returns, lag.max = 10, type = "both")
optimal_lag = lag_selection$selection[1]

# Estimate a VAR model
var_model <- VAR(returns, p = optimal_lag)
summary (var_model)


# Estimate the spillover index
spillover_index <- G.spillover(var_model, n.ahead = 10)

# Print the spillover index
print(spillover_index)

# Diagonal elements: The diagonal elements (20.26% for GSPC, 18.50% for N225, and 19.29% for GDAXI) represent the proportion of the total forecast error variance in each index due to its own innovations or shocks. These values indicate how much of the variability in each index is explained by its own past values.
# Off-diagonal elements: The off-diagonal elements show the pairwise spillover effects between the indices. For example, the value of 3.40% in the first row and second column indicates that 3.40% of the total forecast error variance in the N225 index is due to shocks from the GSPC index. Similarly, the value of 7.42% in the second row and first column indicates that 7.42% of the total forecast error variance in the GSPC index is due to shocks from the N225 index.
# C. from others: The "from others" row shows the proportion of the total forecast error variance in each index that is due to shocks from other indices. For example, 13.07% of the total forecast error variance in the GSPC index is due to shocks from the N225 and GDAXI indices.
# C. to others (spillover): The "to others" row shows the proportion of the total forecast error variance in each index that is transmitted to other indices. For example, the GSPC index sends 17.31% of its total forecast error variance to the N225 and GDAXI indices.
# C. to others including own: The "to others including own" row shows the proportion of the total forecast error variance in all indices that is due to spillovers. In this case, the total spillover index is 100%, indicating the overall level of interdependence among the three indices.


dy_results <- dynamic.spillover(data=returns, width=200, remove.own = FALSE)
plot(dy_results$from)
plot(dy_results$to)









## Advanced and optional - Box-Cox
## Example : Risk-free returns-Strength of the Box-Cox transformation for variance stabilization
library("car")  ##  for bcPower
library("Ecdat")   ##  for Capm

data(Capm)

alpha = seq(-.1,1,by =.01)
result_sq = 0*alpha
result_abs = result_sq

n_rf = length(Capm$rf)

for (i in 1:length(alpha))
{
  result_sq[i] =cor(Capm$rf[-n_rf],abs(diff(bcPower(Capm$rf,alpha[i])))^2 )
  result_abs[i] =cor(Capm$rf[-n_rf],abs(diff(bcPower(Capm$rf,alpha[i]))) )
}

par(mfrow=c(1,1))

plot(alpha,result_abs,lty=1,type="l",xlab=expression(alpha),
     ylab="correlation", lwd=2)
lines(alpha,result_sq,lty=5,lwd=2,col="red")
abline(h=0)
abline(v=spline(result_sq,alpha,xout=0)$y,lty=5,col="red")
abline(v=spline(result_abs,alpha,xout=0)$y)
legend(.3,-.02,c("absolute changes","squared changes"),lwd=2,lty=c(1,5),col=c("black","red"))






## Time-series ######################################################
library(Ecdat)
data(Mishkin,package="Ecdat")
head(Mishkin) # pai1 = one-month inflation rate (in percent, annual rate) 

y = as.ts(Mishkin[,1], start=1950, frequency=12)  # creat variable y as a time-series that includes Mishkin[,1] observations starting from 1950, monhly
y = ts(as.vector(Mishkin[,1]), start=1950, frequency=12)  # another way of doin it

par(mfrow=c(2,1))
plot(y,ylab="Inflation Rate",type="l",xlab="Year",cex.lab=1.5, cex.axis=1.5,cex.main=1.3,main="(a)") # plotting initial time-series
plot(diff(y),ylab="Change in Rate",type="l",xlab="Year",cex.lab=1.5, cex.axis=1.5,cex.main=1.2,main="(b)") # plotting first diff series


#AirPassengers
data(AirPassengers) # monthly total international airline passengers
z = as.ts(AirPassengers, start=1949, frequency=12) 
plot(z,type="b",ylab="Passengers",cex.axis=1.5,cex.lab=1.5,cex=1.5,lwd=2)


#Box.test
y = as.vector(Mishkin[,1]) 
par(mfrow=c(1,2))
acf(y,cex.axis=1.5,cex.lab=1.5,cex.main=1.2,main="(a)") # autocorrelation of the raw data
acf(diff(y),cex.axis=1.5,cex.lab=1.5,cex.main=1.2,main="(b)") #autocorrelation of the first diff data
Box.test(diff(y), lag=10, type="Ljung-Box")




#examples of the time-series with different Phi
set.seed(8716)
e = rnorm(200)
x1 = x2 = x3 = x4 = e

for (t in 2:200){ # we simulate 4 differetn time-series with different Phis
  x1[t] = 0.98*x1[t-1]+e[t]
  x2[t] = -0.6*x2[t-1]+e[t]
  x3[t] = 1.00*x3[t-1]+e[t]
  x4[t] = 1.01*x4[t-1]+e[t]
}

par(mfrow=c(2,2),cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
plot(x1,type="l",xlab="Time (t)",ylab=expression(Y[t]), main=expression(paste(phi," = 0.98")))
plot(x2,type="l",xlab="Time (t)",ylab=expression(Y[t]), main=expression(paste(phi == - 0.6)))
plot(x3,type="l",xlab="Time (t)",ylab=expression(Y[t]), main=expression(paste(phi," = 1")))
plot(x4,type="l",xlab="Time (t)",ylab=expression(Y[t]), main=expression(paste(phi," = 1.01")))




#Daily log returns for BMW stock-ACF plots and AR fit
require(evir) # the package needed for data

data(bmw, package = "evir")
Box.test(bmw, lag = 5, type = "Ljung-Box")

fitAR1 = arima(bmw, order = c(1,0,0))
print(fitAR1)

Box.test(residuals(fitAR1), lag = 5, type = "Ljung-Box", fitdf = 1)

Box.test(residuals(fitAR1), lag = 10, type = "Ljung-Box", fitdf = 1)
Box.test(residuals(fitAR1), lag = 15, type = "Ljung-Box", fitdf = 1)
Box.test(residuals(fitAR1), lag = 20, type = "Ljung-Box", fitdf = 1)


library(xts)
data(bmw,package="evir")
BMW = xts(bmw, attr(bmw,"times"))

layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
par(cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
plot(BMW,main="(a)", minor.ticks = FALSE)
acf(bmw,lag.max=20,main="(b)")
qqnorm(bmw,main="(c)") ; qqline(bmw)


# Example : Inflation rate-AR(1) fit and checking residuals
data(Mishkin, package = "Ecdat")
y = as.vector(Mishkin[,1]) 
fit = arima(y, order = c(1,0,0))
Box.test(fit$resid, type = "Ljung", lag = 24, fitdf = 1)

par(mfrow=c(1,2))
acf(y,main="Inflation rate")
acf(fit$resid,main="Residuals from AR(1)")


## GARCH model
# X return SP600
# Y return bitcoin


library (tseries)
bitcoin.garch.1 <- garch(Y, order =c(1,1))
summary (bitcoin.garch.1)


library (rugarch)
bitcoin.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
bitcoin.garch.2 = ugarchfit(bitcoin.garch.spec, Y)
summary(bitcoin.garch.2)
plot (bitcoin.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

# BMW 
library(rugarch)
data(bmw, package="evir")
arma.garch.norm = ugarchspec(mean.model=list(armaOrder=c(1,0)), variance.model=list(garchOrder=c(1,1)))
bmw.garch.norm = ugarchfit(data=bmw, spec=arma.garch.norm)

show(bmw.garch.norm)
length (bmw)

plot (bmw.garch.norm)

#testing distribution
library(MASS)
e = residuals(bmw.garch.norm, standardize=TRUE)
fitdistr(e,"t") #Maximum-likelihood fitting of univariate distributions, allowing parameters to be held fixed if desired.

#student distribution
arma.garch.t = ugarchspec(mean.model=list(armaOrder=c(1,0)),variance.model=list(garchOrder=c(1,1)), distribution.model = "std")
bmw.garch.t = ugarchfit(data=bmw,spec=arma.garch.t)
show(bmw.garch.t)


# ARMA-APARCH-t
arma.aparch.t = ugarchspec(mean.model=list(armaOrder=c(1,0)), 
                          variance.model=list(model="apARCH",
                          garchOrder=c(1,1)))

bmw.aparch.t = ugarchfit(data=bmw, spec=arma.aparch.t)
show(bmw.aparch.t)
plot (bmw.aparch.t)






