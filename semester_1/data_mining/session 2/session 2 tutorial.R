rm(list=ls(all=TRUE))

#loading libraries
library(class)
library(caret)
library(e1071)


#reading data
library(readr)

universal.df <- read.csv('UniversalBank.csv')

View(universal.df)


#partition the data
set.seed(1)
#coge una muestra del 60% del total de la base de datos
train.index <- sample(row.names(universal.df), 0.6*dim(universal.df)[1])
#el resto de los datos (setdiff coge las diferencias entre la base de datos al completo y train.index)
valid.index <- setdiff(row.names(universal.df), train.index)  
train.df <- universal.df[train.index, -c(1, 5)]
valid.df <- universal.df[valid.index, -c(1, 5)]
t(t(names(train.df)))


#new customer information
new.cust <- data.frame(Age = 40,                
                       Experience = 10,    
                       Income = 84,  
                       Family = 2,          
                       CCAvg = 2,          
                       Education = 2,        
                       Mortgage = 0,          
                       Securities.Account = 0,
                       CD.Account = 0,
                       Online = 1,            
                       CreditCard = 1)


# normalize the data
train.norm.df <- train.df[,-8]
valid.norm.df <- valid.df[,-8]


new.cust.norm <- new.cust
norm.values <- preProcess(train.df[, -8], method=c("center", "scale"))
train.norm.df <- predict(norm.values, train.df[, -8])
valid.norm.df <- predict(norm.values, valid.df[, -8])
new.cust.norm <- predict(norm.values, new.cust.norm)



#model
knn.pred <- class::knn(train = train.norm.df,
                       test = new.cust.norm,
                       cl = train.df$Personal.Loan, k = 1)


knn.pred


library(e1071)
# optimal k
accuracy.df <- data.frame(k = seq(1, 15, 1), overallaccuracy = rep(0, 15))
for(i in 1:15) {
  knn.pred <- class::knn(train = train.norm.df,
                         test = valid.norm.df,
                         cl = train.df$Personal.Loan, k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred,
                                       as.factor(valid.df$Personal.Loan))$overall[1]}
 
accuracy.df

which(accuracy.df[,2] == max(accuracy.df[,2]))



knn.pred <- class::knn(train = train.norm.df,
                       test = valid.norm.df,
                       cl = train.df$Personal.Loan, k = 3)
confusionMatrix(knn.pred, as.factor(valid.df$Personal.Loan), positive = "1")




knn.pred <- class::knn(train = train.norm.df,
                       test = new.cust.norm,
                       cl = train.df$Personal.Loan, k = 3)
knn.pred





# 3-way partition
set.seed(1)  
train.index <- sample(row.names(universal.df), 0.5*dim(universal.df)[1])
valid.index <- sample(setdiff(row.names(universal.df), train.index),
                      0.3*dim(universal.df)[1])
test.index <-  setdiff(row.names(universal.df), c(train.index, valid.index))
train.df <- universal.df[train.index, -c(1, 5)]
valid.df <- universal.df[valid.index, -c(1, 5)]
test.df <- universal.df[test.index, -c(1, 5)]

# normalization
train.norm.df <- train.df[,-8]
valid.norm.df <- valid.df[,-8]
test.norm.df <- test.df[,-8]
norm.values <- preProcess(train.df[, -8], method=c("center", "scale"))
train.norm.df <- predict(norm.values, train.df[, -8])
valid.norm.df <- predict(norm.values, valid.df[, -8])
test.norm.df <- predict(norm.values, test.df[, -8])

# predictions on train
knn.predt <- class::knn(train = train.norm.df,
                        test = train.norm.df,
                        cl = train.df$Personal.Loan, k = 3)

confusionMatrix(knn.predt, as.factor(train.df$Personal.Loan), positive = "1")




# predictions on validation
knn.predv <- class::knn(train = train.norm.df,
                        test = valid.norm.df,
                        cl = train.df$Personal.Loan, k = 3)

confusionMatrix(knn.predv, as.factor(valid.df$Personal.Loan), positive = "1")

# predictions on test
knn.predtt <- class::knn(train = train.norm.df,
                         test = test.norm.df,
                         cl = train.df$Personal.Loan, k = 3)
confusionMatrix(knn.predtt, as.factor(test.df$Personal.Loan), positive = "1")








#10.1

bank.df<-read.csv('banks.csv')
View(bank.df)


#model
logit<-glm(Financial.Condition~TotExp.Assets+TotLns.Lses.Assets,data=bank.df,family='binomial')
logit
summary(logit)



logit1 <- c(1, 0.11, 0.6) %*% logit$coefficients
odds<-exp(logit1)
prob<-odds/(1+odds)
prob



p=0.2
odds=p/(1-p)
odds
log(odds)
exp(8.371)
