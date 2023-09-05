library(quantmod)
library(pastecs)
library(tvReg)
library (brms)
library (MSwM)
library (quantreg)


setwd("C:/Users/pepec/Desktop/estudios/master/semester 2/Financial modeling/Group_Project") 


### Importing the data and calculating returns

## Top market cap tokens ----------------------------------------------------------------------------------
# Bitcoin ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("BTC-USD", from = '2020-10-03',
           to = "2023-05-01",warnings = FALSE,
           auto.assign = TRUE) # getting Bitcoin prices

Bitcoin <- `BTC-USD`

chart_Series(Bitcoin$`BTC-USD.Close`)

sum(is.na(Bitcoin))

bitcoin.xts = Bitcoin$`BTC-USD.Close`
bitcoin.zoo = as.zoo(bitcoin.xts)

return1 = diff (log(bitcoin.xts)) # Log return calculation
return1 = return1 [-1] # removing the first empty observation, received after return calculation
summary (return1)
stat.return1 = stat.desc(return1)

plot(return1, main = "Bitcoin daily returns", xlab = "year", type = "l", ylab = "log return") # plot the graph



# ETH ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("ETH-USD", from = '2020-10-03',
           to = "2023-05-01",warnings = FALSE,
           auto.assign = TRUE)

Ethereum <- `ETH-USD`

chart_Series(Ethereum$`ETH-USD.Close`)

sum(is.na(Ethereum))

ethereum.xts = Ethereum$`ETH-USD.Close`
ethereum.zoo = as.zoo(ethereum.xts)


return2 = diff (log(ethereum.xts)) 
return2 = return2 [-1] 
summary (return2)
stat.return2 = stat.desc(return2)

plot(return2, main = "Ethereum daily returns", xlab = "year", type = "l", ylab = "log return") 



# Binance ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("BNB-USD", from = '2020-10-03',
           to = "2023-05-01",warnings = FALSE,
           auto.assign = TRUE) 

Binance <- `BNB-USD`

chart_Series(Binance$`BNB-USD.Close`)

sum(is.na(Binance))

binance.xts = Binance$`BNB-USD.Close`
binance.zoo = as.zoo(binance.xts)

return3 = diff (log(binance.xts)) 
return3 = return3 [-1] 
summary (return3)
stat.return3 = stat.desc(return3)

plot(return3, main = "BNB-USD daily returns", xlab = "year", type = "l", ylab = "log return") 


## Top DeFitokens ----------------------------------------------------------------------------------
# UNI ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("UNI-USD", from = '2020-10-03',
           to = "2023-05-01",warnings = FALSE,
           auto.assign = TRUE)

Uniswap <- `UNI-USD`

chart_Series(Uniswap$`UNI-USD.Close`)

sum(is.na(Uniswap))

Uniswap.xts = Uniswap$`UNI-USD.Close`
Uniswap.zoo = as.zoo(Uniswap.xts)

return4 = diff (log(Uniswap.xts)) 
return4 = return4 [-1] 
summary (return4)
stat.return4 = stat.desc(return4)

plot(return4, main = "Uniswap daily returns", xlab = "year", type = "l", ylab = "log return") 



# AAVE ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("AAVE-USD", from = '2020-10-03',
           to = "2023-05-01",warnings = FALSE,
           auto.assign = TRUE) 

AAVE <- `AAVE-USD`

chart_Series(AAVE$`AAVE-USD.Close`)

sum(is.na(AAVE))

AAVE.xts <- AAVE$`AAVE-USD.Close`
AAVE.zoo <- as.zoo(AAVE.xts)

return5 <- diff(log(AAVE.xts)) 
return5 <- return5[-1] 
summary(return5)
stat.return5 <- stat.desc(return5)

plot(return5, main = "AAVE daily returns", xlab = "year", type = "l", ylab = "log return") 


## Top layer 1/2 blockchain tokens ----------------------------------------------------------------------------------
# SOL ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("SOL-USD", from = '2020-10-03',
           to = "2023-05-01",warnings = FALSE,
           auto.assign = TRUE) 

Solana <- `SOL-USD`

chart_Series(Solana$`SOL-USD.Close`)

sum(is.na(Solana))

Solana.xts = Solana$`SOL-USD.Close`
Solana.zoo = as.zoo(Solana.xts)

return6 = diff (log(Solana.xts))
return6 = return6 [-1] 
summary (return6)
stat.return6 = stat.desc(return6)

plot(return6, main = "Solana daily returns", xlab = "year", type = "l", ylab = "log return") 



# DOT ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("DOT-USD", from = '2020-10-03',
           to = "2023-05-01",warnings = FALSE,
           auto.assign = TRUE) 

Polkadot <- `DOT-USD`

chart_Series(Polkadot$`DOT-USD.Close`)

sum(is.na(Polkadot))

Polkadot.xts = Polkadot$`DOT-USD.Close`
Polkadot.zoo = as.zoo(Polkadot.xts)

return7 = diff(log(Polkadot.xts)) 
return7 = return7[-1] 
summary(return7)
stat.return7 = stat.desc(return7)

plot(return7, main = "Polkadot daily returns", xlab = "year", type = "l", ylab = "log return") 


## Top CEX tokens ----------------------------------------------------------------------------------
# HT ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("HT-USD", from = '2020-10-03',
           to = "2023-05-01", warnings = FALSE,
           auto.assign = TRUE) 

HT <- `HT-USD`

chart_Series(HT$`HT-USD.Close`)

sum(is.na(HT))

HT.xts <- HT$`HT-USD.Close`
HT.zoo <- as.zoo(HT.xts)

return8 <- diff(log(HT.xts)) 
return8 <- return8[-1]

summary(return8)
stat.return8 <- stat.desc(return8)

plot(return8, main = "HT daily returns", xlab = "year", type = "l", ylab = "log return")



# OKB ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("OKB-USD", from = '2020-10-03',
           to = "2023-05-01", warnings = FALSE,
           auto.assign = TRUE) 

OKB <- `OKB-USD`

chart_Series(OKB$`OKB-USD.Close`)

sum(is.na(OKB))

OKB.xts <- OKB$`OKB-USD.Close`
OKB.zoo <- as.zoo(OKB.xts)

return9 <- diff(log(OKB.xts)) 
return9 <- return9[-1]

summary(return9)
stat.return9 <- stat.desc(return9)

plot(return9, main = "OKB daily returns", xlab = "year", type = "l", ylab = "log return")



## Top Metaverse protocol tokens ----------------------------------------------------------------------------------
# MANA ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("MANA-USD", from = '2020-10-03',
           to = "2023-05-01", warnings = FALSE,
           auto.assign = TRUE) 

MANA <- `MANA-USD`

chart_Series(MANA$`MANA-USD.Close`)

sum(is.na(MANA))

MANA.xts <- MANA$`MANA-USD.Close`
MANA.zoo <- as.zoo(MANA.xts)

return10 <- diff(log(MANA.xts))
return10 <- return10[-1] 

summary(return10)
stat.return10 <- stat.desc(return10)

plot(return10, main = "MANA daily returns", xlab = "year", type = "l", ylab = "log return")



# SAND ----------------------------------------------------------------------
options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)
getSymbols("SAND-USD", from = '2020-10-03', 
           to = "2023-05-01", warnings = FALSE, 
           auto.assign = TRUE)

SAND <- `SAND-USD`

sum(is.na(SAND))

SAND.xts <- SAND$`SAND-USD.Close`
SAND.zoo <- as.zoo(SAND.xts)

return11 <- diff(log(SAND.xts))
return11 <- return11[-1]

plot(return11, main = "SAND daily returns", xlab = "year", type = "l", ylab = "log return")



#putting all the returns together
returns_together.xts <- cbind(return1, return2, return3, return4, return5, 
                              return6, return7, return8, return9, return10, return11)

exchange.herd = function(return) # a function to create CSAD and Rm
{
  n=ncol(return)
  Rm = rowMeans(return)
  temp_dif =abs(return-Rm)
  temp_sum = rowSums(temp_dif)
  CSAD = temp_sum / ncol(return)
  CSAD = cbind (CSAD, Rm)
  return (CSAD)
}

f = exchange.herd(returns_together.xts) # calling the function "exchange.herd" that calculates CSAD and Rm
head (f)


CSAD.df = fortify.zoo(f) # converting f into a dataframe (to simplify further calculations)
CSAD.df$Rm2 = CSAD.df$Rm^2 # calculating Rm^2
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

#Newey-West Heteroscedasticity and Autocorrelation consistent (HAC) estimators
coeftest(linearMod,vcov=NeweyWest(linearMod,verbose=T))


# estimate TV Linear Regression
tvlm.fit = tvLM(y~x1+x2, bw = NULL  ) #bw =  0.1538297 
head (tvlm.fit$coefficients, 800)
plot(tvlm.fit$coefficients[,1], type="l")
plot(tvlm.fit$coefficients[,2], type="l")
plot(tvlm.fit$coefficients[,3], type="l")


# # Bayesian models
# daily = cbind(y, x1, x2)
# model = brm(formula = y ~ x1+x2, 
#             data = daily,
#             seed    = 123)
# summary(model)


# Markow regime-switching model
nstates <- 2 # a number of states
msEuro = msmFit(linearMod, k = nstates, sw = rep(TRUE, 4)) # estimation; linearMod is an object from a linear estimation
summary(msEuro) #show the 
plotProb(msEuro ,which=1)


#Quantile regression
taus<-seq(from = .1, to = .9, by = .1) 
coef0 <- rq( y ~ x1+x2, tau=taus)
summary (coef0)
