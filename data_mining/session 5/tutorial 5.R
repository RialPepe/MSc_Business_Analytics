rm(list=ls(all=TRUE))


#loading libraries
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(rpart.plot))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(Matrix))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(gains))



#loading data
ebay.df <- read.csv("eBayAuctions.csv")

#data exploration
head(ebay.df)
View(ebay.df)
summary(ebay.df)
lapply(ebay.df, class)


#convet variable "Duration" to categorical type
ebay.df$Duration <- as.factor(ebay.df$Duration)
ebay.df$Competitive. <- as.factor(ebay.df$Competitive.)



#partitioning data
set.seed(1)  
train.index <- sample(c(1:dim(ebay.df)[1]), dim(ebay.df)[1]*0.6)  
valid.index <- setdiff(c(1:dim(ebay.df)[1]), train.index)  
train.df <- ebay.df[train.index, ]
valid.df <- ebay.df[valid.index, ]


train.df
valid.df


#building model
tr<-rpart(Competitive.~.,data=train.df,minbucket=50, maxdepth=7)
tr
prp(tr)
prunefit<-prune(tr,cp = tr$cptable[which.min(tr$cptable[,"xerror"]),"CP"])

#variable importance
t(t(tr$variable.importance))


# select vars: 
t(t(names(ebay.df)))
selected.var <- c(1, 2, 3, 4, 7, 8)
tr <- rpart(Competitive. ~ ., data = train.df[, selected.var], 
            minbucket = 50, maxdepth = 7, model=TRUE)
#pruned tree
pfit<- prune(tr, cp = tr$cptable[which.min(tr$cptable[,"xerror"]),"CP"])
prp(tr)

#set of rules
tr



x <- train.df$sellerRating
y <- train.df$OpenPrice

plot(x, y, xlab="sellerRating", ylab="OpenPrice", main="OpenPrice vs sellerRating",
     col = ifelse(train.df$Competitive. == "1", "red", "blue"))



logx <- log(train.df$sellerRating)
logy <- log(train.df$OpenPrice)

plot(logx, logy, xlab="log(sellerRating)", ylab="log(OpenPrice)", 
     main="log(OpenPrice) vs log(sellerRating)",
     col = ifelse(train.df$Competitive. == "1", "red", "blue"))



pred <- predict(tr, valid.df[, selected.var])
head(pred[,2])
#confusion matrix
confusionMatrix(factor(1*(pred[,2]>0.5)), valid.df$Competitive., positive = "1")


pred <- predict(tr, valid.df[, selected.var])
gain <- gains(as.numeric(factor(valid.df$Competitive.)), 
              as.numeric(factor((1*(pred[,2]>0.5)))))
gain


plot(c(0,gain$cume.pct.of.total*sum(as.numeric(valid.df$Competitive.))) 
     ~ c(0,gain$cume.obs), xlab="# cases", ylab = "Cumulative",
     main="Lift chart for validation data", type = "l")
lines(c(0,sum(as.numeric(valid.df$Competitive.))) ~ c(0, dim(valid.df)[1]),lty=1)

heights <- gain$mean.resp/mean(as.numeric(valid.df$Competitive.))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9),
                     xlab = "Percentile", ylab = "Mean Response", 
                     main = "Decile-wise chart for validation data")







#question-2
rm(list=ls(all=TRUE))

delays.df <- read.csv("FlightDelays.csv")
t(t(names(delays.df)))  # Look at your selected column names.
delays.df <- delays.df[, -c(3, 6, 7, 12)]  # Select your variables.

View(delays.df)

lapply(delays.df, class)

delays.df$DAY_WEEK <- as.factor(delays.df$DAY_WEEK)
delays.df$CRS_DEP_TIME <- cut(delays.df$CRS_DEP_TIME/100, 8)
unique(delays.df$CRS_DEP_TIME)


set.seed(201)  
train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)  
valid.index <- setdiff(c(1:dim(delays.df)[1]), train.index)  
train.df <- delays.df[train.index, ]
valid.df <- delays.df[valid.index, ]


delays.ct <- rpart(Flight.Status ~ ., 
                   data = train.df[,-8], 
                   method = "class",
                   maxdepth = 8, 
                   cp = 0.001, model = TRUE)
prp(delays.ct)
t(t(delays.ct$variable.importance))

#pruned tree
pfit<- prune(delays.ct, 
             cp = delays.ct$cptable[which.min(delays.ct$cptable[,"xerror"]),"CP"])
prp(pfit)



delays.ct <- rpart(Flight.Status ~ ., 
                   data = train.df[,-c(6,8)], 
                   method = "class",
                   maxdepth = 8, 
                   cp = 0.001, model = TRUE)
prp(delays.ct)
t(t(delays.ct$variable.importance))

pfit<- prune(delays.ct, 
             cp = delays.ct$cptable[which.min(delays.ct$cptable[,"xerror"]),"CP"])

prp(pfit)
t(t(delays.ct$variable.importance))



# 9.2.b

#We cannot use this tree, because we must know the Weather. The redundantinformation is the day of week (Monday) and arrival airport (EWR). The tree requires knowing whether the weather was inclement or not. We may not know the weather in advance

#9.2.c.i

#In the best-pruned tree we get a single terminal node labeled "ontime." Therefore any new flight will be classified as being "on time".

# 9.2.c.ii.
#This is equivalent to the na?ve rule, which is the majority rule. In this dataset most of the flights arrived on time, and therefore the na?ve rule is to classify a new flight as arriving on time.

#9.2.c.iii

#CARRIER=CO,DH,MQ,RU,UA, CARRIER=DL,OH,US, Binned CRS_DEP_TIME=(17.5,19.4] and Binned CRS_DEP_TIME=(5.98,7.91].