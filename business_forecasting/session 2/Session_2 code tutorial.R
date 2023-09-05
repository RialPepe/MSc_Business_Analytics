rm(list=ls(all=TRUE))

#16.3
sales.df<-read.csv('DepartmentStoreSales.csv')
View(sales.df)
summary(sales.df)

sales.ts<-ts(sales.df$Sales,start=c(2000,1),end=c(2005,4), frequency = 4)
sales.ts

plot(sales.ts)
plot(sales.ts, xlab=('Year'), ylab=('Sales'), main=('Store Sales'),ylim=c(45000,105000))


#16.4
ApplianceShipments <- read.csv("ApplianceShipments.csv")

View(ApplianceShipments)



shipments.ts<-ts(ApplianceShipments$Shipments,start = c(1985,1),end=c(1989,4),frequency=4)

shipments.ts

plot(shipments.ts)

plot(shipments.ts, xlab=('Year'), ylab=('Shipments'), main=('Appliance Shipments'))



#16.5
canadian_hours <- read.csv("CanadianWorkHours.csv")

canadian_hours

canadian_hours.ts <- ts(canadian_hours$Hours, start = 1966, end = 2000, frequency = 1)

canadian_hours.ts

plot(canadian_hours.ts)


#16.6
SouvenirSales <- read.csv("SouvenirSales.csv")

View(SouvenirSales)



s.sales.ts<-ts(SouvenirSales$Sales,start=c(1995,1),end=c(2001,12),frequency=12)

s.sales.ts



plot(s.sales.ts)

plot(s.sales.ts, xlab=('Year'), ylab=('Sales'), main=('Store Sales'))



plot(s.sales.ts,log='xy')


nvalid<-12
ntrain<-length(s.sales.ts)-nvalid
ntrain

train.ts<-window(s.sales.ts,start=c(1995,1),end=c(1995,ntrain))
train.ts
valid.ts<-window(s.sales.ts,start=c(1995,ntrain+1),end=c(1995,ntrain+nvalid))
valid.ts
