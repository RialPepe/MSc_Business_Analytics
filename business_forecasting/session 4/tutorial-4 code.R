rm(list=ls(all=TRUE))


#loading data
appliance.df <- read.csv("ApplianceShipments.csv")


#exploring data
appliance.df
View(appliance.df)
summary(appliance.df)


#creating time series
shipments.ts<-ts(appliance.df$Shipments,start = c(1985,1),end=c(1989,4),frequency=4)

shipments.ts


#visualising 
plot(shipments.ts)

plot(shipments.ts, type="l", xlab=('Year'), ylab=('Shipments'), main=('Appliance Shipments'),ylim=c(3500,5500))



#partitioning data
nValid <- 4

nTrain <- length(shipments.ts) - nValid


train.ts <- window(shipments.ts, start = c(1985, 1), end = c(1985, nTrain))

valid.ts <- window(shipments.ts, start = c(1985, nTrain + 1),
                   
                   end = c(1985, nTrain + nValid))


plot(train.ts,ylim=c(3500,5500))



#building a model
train.lm. <- tslm(train.ts ~ trend + season)
train.lm.

#prediction
ypred<-forecast(train.lm.,h=4)
ypred
#accuracy
accuracy(ypred,valid.ts)


#holtz winter model
hw.lm<-ets(train.ts,model='ZAA')
hw.lm

#predictions
ypred1<-forecast(hw.lm,h=4)
ypred1

#accuracy
accuracy(ypred1,valid.ts)




#17.6

#LOADING DATA
SouvenirSales <- read.csv("SouvenirSales.csv")

#data exploration
View(SouvenirSales)

#creating a time series 
s.sales.ts <- ts(SouvenirSales$Sales,start=c(1995,1),end=c(2001,12),frequency=12)

s.sales.ts

#plotting data
plot(s.sales.ts,)

plot(s.sales.ts,log="y")

lines(ma(Sales.ts,order=12,centre = TRUE))


#splitting data
nvalid<-12
ntrain<-length(s.sales.ts)-nvalid

train.ts <- window(s.sales.ts, start = c(1995,1), end = c(1995, ntrain))

train.ts

plot(train.ts, main = ("Training Set of Souvenir Sales"), ylab=("Number of Sales"), xlab=("Year"), col = "purple")



valid.tslabels <- window(s.sales.ts, start = c(1995, ntrain+1), end = c(1995, ntrain+nvalid), main = ("Validation Set of Souvenir Sales"), ylab = ("Number of Sales"), xlab = ("Year"))

valid.ts <- window(s.sales.ts, start = c(1995, ntrain+1), end = c(1995, ntrain+nvalid), main = ("Validation Set of Souvenir Sales"), ylab=("Number of Sales"), xlab=("Year"))

valid.ts

plot(valid.tslabels)


#creating a  model
sales.lm<-tslm(train.ts~trend+season)
sales.lm
summary(sales.lm)


#exponential model
sales.lm1<-tslm(log(train.ts)~trend+season)
sales.lm1
summary(sales.lm1)
ypreds<-forecast(sales.lm1,h=14)
ypreds
exp(9.744667)
