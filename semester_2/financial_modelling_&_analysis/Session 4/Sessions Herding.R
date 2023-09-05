## Roman Matkovskyy
## MSc Business Analytics 

 

setwd("C:/Users/pepec/Desktop/estudios/master/semester 2/Financial modeling/Session 4") 


###########################################################################################################


library (xts)
library (zoo)
library (PerformanceAnalytics)
require (sandwich)
require(lmtest)


## load the data

close.prices = read.csv("close.prices.csv", header = TRUE) #importing the data
close.prices$Index = as.POSIXct(close.prices$Index,format="%d/%m/%Y %H", tz = "") # converting the first column into date format


if (any(is.na(close.prices[,1]))) {
  # Remove rows with missing values in the first column
  close.prices <- na.omit(close.prices)
  
}


close.prices.xts <- xts(close.prices[,-1], order.by=close.prices[,1]) # convert the object into xts object (time series object)
close.prices.zoo  <- as.zoo(close.prices.xts) # convert the time series object into zoo object

############################################################################################################
## the model
############################################################################################################

## calculate the return

return = Return.calculate( close.prices.xts , method = "log") # automatically calculate return



library(pastecs)
descriptive.stat.return = stat.desc(return) # descriptive statistics



# a function to create CSAD and Rm
exchange.herd = function(return) 
{
  n=ncol(return)
  Rm = rowMeans(return)
  temp_dif =abs(return-Rm)
  temp_sum = rowSums(temp_dif)
  CSAD = temp_sum / ncol(return)
  CSAD = cbind (CSAD, Rm)
  return (CSAD)
}

 
f = exchange.herd(return) # calling the function "exchange.herd" that calculates CSAD and Rm
head (f) # show the first 6 rows

CSAD.df = fortify.zoo(f) # converting f into a dataframe (to simplify further calculations)
CSAD.df$Rm2 = CSAD.df$Rm^2 # calculating Rm^2
CSAD.df = CSAD.df[-c(1),] # removing the first row with NAs
head (CSAD.df) # show the first 6 rows
tail (CSAD.df) # show the last 6 rows


# reassign my columns as Y and Xs to look better in the regression model
y = CSAD.df$CSAD  # reassign my columns as Y and Xs to look better in the regression model
x1 = abs (CSAD.df$Rm)
x2 = CSAD.df$Rm2


#Linear model
linearMod <- lm(y~x1+x2)  # build linear regression model on full data
print(linearMod)
summary(linearMod)






## For curious students advanced estimatimation 
#Newey-West Heteroscedasticity and Autocorrelation consistent (HAC) estimators
coeftest(linearMod,vcov=NeweyWest(linearMod,verbose=T))


## For curious students - if you wish you can take a look at the Time varying regression models 
## the package tvReg

# estimate TV Linear Regression
require (tvReg)
tvlm.fit = tvLM(y~x1+x2, bw = NULL  ) #bw=0.149 
head (tvlm.fit$coefficients)
plot(tvlm.fit$coefficients[,1], type="l")
plot(tvlm.fit$coefficients[,2], type="l")
plot(tvlm.fit$coefficients[,3], type="l")





# Bayesian models
library (brms)
hourly = cbind(y, x1, x2)
model = brm(formula = y ~ x1+x2, 
             data    = hourly,
             seed    = 123)
summary(model)


# Markow regime-switching model
library (MSwM)

nstates <- 2 # a number of states
msEuro = msmFit(linearMod, k = nstates, sw = rep(TRUE, 4)) # estimation; linearMod is an object from a linear estimation
summary(msEuro) #show the 
plotProb(msEuro ,which=1)


#Quantile regression
library (quantreg)
taus<-seq(from = .1, to = .9, by = .1) 
coef0 <- rq( y ~ x1+x2, tau=taus)
summary (coef0)
