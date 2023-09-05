rm(list=ls(all=TRUE))


#loading libraries
library(caret)
library(neuralnet)
library(forecast)


#loading data
data<-read.csv('ToyotaCorolla.csv')

summary(data)
View(data)

#dummies
data$Fuel_Type_CNG<-1*(data$Fuel_Type=='CNG')
data$Fuel_Type_Diesel<-1*(data$Fuel_Type=='Diesel')


#partioning
set.seed(123)
train.index<-sample(row.names(data),0.6*dim(data)[1])
valid.index<-setdiff(row.names(data),train.index)
train.df<-data[train.index,]
valid.df<-data[valid.index,]

dim(train.df)
dim(valid.df)


#normalise
norm.values<-preProcess(train.df,method='range')
train.norm.df<-predict(norm.values,train.df)
valid.norm.df<-predict(norm.values, valid.df)


#creating a model
model <- neuralnet(Price ~ Age_08_04 
                   
                   + KM 
                   
                   + Fuel_Type_CNG 
                   
                   + Fuel_Type_Diesel 
                   
                   + HP 
                   
                   + Automatic 
                   
                   + Doors 
                   
                   + Quarterly_Tax 
                   
                   + Mfr_Guarantee 
                   
                   + Guarantee_Period 
                   
                   + Airco 
                   
                   + Automatic_airco 
                   
                   + CD_Player 
                   
                   + Powered_Windows 
                   
                   + Sport_Model 
                   
                   + Tow_Bar, 
                   
                   data = train.norm.df, linear.output = T,hidden = 2)

plot(model)


#predictions
training.prediction<-compute(model,train.norm.df)
validation.prediction<-compute(model,valid.norm.df)


#performance
RMSE(training.prediction$net.result,train.norm.df$Price)
RMSE(validation.prediction$net.result,valid.norm.df$Price)



#creating a model
model <- neuralnet(Price ~ Age_08_04 
                   
                   + KM 
                   
                   + Fuel_Type_CNG 
                   
                   + Fuel_Type_Diesel 
                   
                   + HP 
                   
                   + Automatic 
                   
                   + Doors 
                   
                   + Quarterly_Tax 
                   
                   + Mfr_Guarantee 
                   
                   + Guarantee_Period 
                   
                   + Airco 
                   
                   + Automatic_airco 
                   
                   + CD_Player 
                   
                   + Powered_Windows 
                   
                   + Sport_Model 
                   
                   + Tow_Bar, 
                   
                   data = train.norm.df, linear.output = T,hidden = 5)

plot(model)


#predictions
training.prediction<-compute(model,train.norm.df)
validation.prediction<-compute(model,valid.norm.df)


#performance
RMSE(training.prediction$net.result,train.norm.df$Price)
RMSE(validation.prediction$net.result,valid.norm.df$Price)


#creating a model
model <- neuralnet(Price ~ Age_08_04 
                   
                   + KM 
                   
                   + Fuel_Type_CNG 
                   
                   + Fuel_Type_Diesel 
                   
                   + HP 
                   
                   + Automatic 
                   
                   + Doors 
                   
                   + Quarterly_Tax 
                   
                   + Mfr_Guarantee 
                   
                   + Guarantee_Period 
                   
                   + Airco 
                   
                   + Automatic_airco 
                   
                   + CD_Player 
                   
                   + Powered_Windows 
                   
                   + Sport_Model 
                   
                   + Tow_Bar, 
                   
                   data = train.norm.df, linear.output = T,hidden = c(5,5,2))

plot(model)


#predictions
training.prediction<-compute(model,train.norm.df)
validation.prediction<-compute(model,valid.norm.df)


#performance
RMSE(training.prediction$net.result,train.norm.df$Price)
RMSE(validation.prediction$net.result,valid.norm.df$Price)


