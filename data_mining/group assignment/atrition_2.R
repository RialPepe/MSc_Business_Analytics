library(tidyverse)
library(viridis)
library(caret)
library(randomForest)
library(e1071)
library(rpart)
library(xgboost)
library(ggplot2)
library(scales)
library(cowplot)
library(magrittr)
library(ggpubr)
library(RColorBrewer)
library(ggrepel)
library(forcats)
library(reshape2)
library(caTools)
library(fastDummies)
library(neuralnet)

df <- read.csv("C:/Users/pepec/Desktop/estudios/master/semester_1/data_mining/group assignment/attrition.csv")
View(df)

norm_df <- df

set.seed(13)

norm_df <- dummy_cols(norm_df, select_columns = c("BusinessTravel", "Attrition", "Gender", "MaritalStatus", "OverTime"), remove_first_dummy = TRUE)

norm_df$Department_Research <- (norm_df$Department == "Research & Development")*1

norm_df$Department_Sales <- (norm_df$Department == "Sales")*1


# Delete unecessary columns
cols <- c("BusinessTravel","Over18", "EmployeeNumber", "EmployeeCount", "StandardHours", "EducationField", "JobRole", "Attrition", "Department", "Gender", "MaritalStatus", "OverTime", "HourlyRate","DailyRate", "MonthlyRate", "JobLevel", "PercentSalaryHike", "Age", "YearsInCurrentRole", "YearsWithCurrManager")

norm_df[cols] <- NULL

train.index <- sample(row.names(norm_df), 0.8 * dim(norm_df)[1])
valid.index <- setdiff(row.names(norm_df), train.index)
train.df <- norm_df[train.index, ]
valid.df <- norm_df[valid.index, ]

norm.values <- preProcess(train.df, method = "range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

res <- cor(train.norm.df)
round(res, 2)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

View(train.norm.df)

#########################################################################################

#1 Linear regression model

model_linear <- lm( Attrition_Yes ~ . , 
                    data = train.norm.df)

summary(model_linear)

#########################################################################################

#2 Neuronal net model

model <- neuralnet( Attrition_Yes ~ DistanceFromHome + Education + EnvironmentSatisfaction 
                    +  JobInvolvement + JobSatisfaction + MonthlyIncome + NumCompaniesWorked 
                    + PerformanceRating +  RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears 
                    + TrainingTimesLastYear + WorkLifeBalance +YearsAtCompany  
                    + YearsSinceLastPromotion  +  BusinessTravel_Travel_Frequently  
                    + BusinessTravel_Travel_Rarely + Department_Sales + Department_Research + OverTime_Yes + MaritalStatus_Single
                    + MaritalStatus_Married + Gender_Male, 
                    data = train.norm.df, linear.output = T, hidden = 1)

plot(model)

# Predictions

train.prediction <- compute(model, train.norm.df)
valid.prediction <- compute(model, valid.norm.df)

# Performance

RMSE(train.prediction$net.result, train.norm.df$Attrition_Yes)
RMSE(valid.prediction$net.result, valid.norm.df$Attrition_Yes)

#########################################################################################

#3 Tree model
tree_df <-df
names(tree_df)
set.seed(13)

tree_df$BusinessTravel = factor(tree_df$BusinessTravel,
                                    levels = c('Travel_Frequently', 'Travel_Rarely', 'Non-Travel'),
                                    labels = c(1, 2, 3))



# Changing the datatype from integer to factors from the ordinal variables.
cols <- c("Education", "EnvironmentSatisfaction", "JobInvolvement", "JobLevel",
          "JobSatisfaction", "PerformanceRating", "RelationshipSatisfaction", 
          "StockOptionLevel", "TrainingTimesLastYear", "WorkLifeBalance")

tree_df[cols] <- lapply(tree_df[cols], factor)

# Delete unecessary columns
cols <- c("Over18", "EmployeeNumber", "EmployeeCount", "StandardHours", "EducationField", "JobRole", "HourlyRate","DailyRate", "MonthlyRate", "JobLevel", "PercentSalaryHike", "Age", "YearsInCurrentRole", "YearsWithCurrManager")

tree_df[cols] <- NULL

train.index <- sample(row.names(tree_df), 0.8 * dim(tree_df)[1])
valid.index <- setdiff(row.names(tree_df), train.index)
train.df <- tree_df[train.index, ]
valid.df <- tree_df[valid.index, ]


rpart.tree <- rpart(Attrition ~ ., data=train.df)
plot(rpart.tree, uniform=TRUE)
text(rpart.tree, all=TRUE, use.n=TRUE)
title("Training Set's Classification Tree")


var_imp <- data.frame(rpart.tree$variable.importance)
var_imp$features <- rownames(var_imp)
var_imp <- var_imp[, c(2, 1)]
var_imp$importance <- round(var_imp$rpart.tree.variable.importance, 2)
var_imp$rpart.tree.variable.importance <- NULL

predictions <- predict(rpart.tree, valid.df, type="class")
conf_df <- data.frame(table(valid.df$Attrition, predictions))


prune.rpart.tree <- prune(rpart.tree, cp=0.02) # pruning the tree
plot(prune.rpart.tree, uniform=TRUE)
text(prune.rpart.tree, all=TRUE, use.n=TRUE)

#########################################################################################

#4 XGBoost model
View(train.norm.df)
X_train = data.matrix(train.norm.df[,-18])                  # independent variables for train
y_train = train.norm.df[,18]                                # dependent variables for train
X_test = data.matrix(valid.norm.df[,-18])                    # independent variables for test
y_test = valid.norm.df[,18]                                   # dependent variables for test

# convert the train and test data into xgboost matrix type.
xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = 2
)

xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 500,
  verbose = 1
)
xgb_model

importance_matrix <- xgb.importance(
  feature_names = colnames(xgb_train), 
  model = xgb_model
)
importance_matrix
xgb.plot.importance(importance_matrix)

xgb_preds <- predict(xgb_model, as.matrix(X_test), reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- c("No", "Yes")
xgb_preds

xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- c("No", "Yes")[y_test + 1]
xgb_preds

accuracy <- sum(xgb_preds$PredictedClass == xgb_preds$ActualClass) / nrow(xgb_preds)
accuracy

confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))

cm <- confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))
cfm <- as_tibble(cm$table)



#######################################################################################################

df <- read.csv("attrition.csv")
View(df)

norm_df <- df


set.seed(13)

norm_df <- dummy_cols(norm_df, select_columns = c("BusinessTravel", "Attrition", "Gender", "MaritalStatus", "OverTime"), remove_first_dummy = TRUE)

norm_df$Department_Research <- (norm_df$Department == "Research & Development")*1

norm_df$Department_Sales <- (norm_df$Department == "Sales")*1


# Delete unecessary columns
cols <- c("BusinessTravel","Over18", "EmployeeNumber", "EmployeeCount", "StandardHours", "EducationField", "JobRole", "Attrition", "Department", "Gender", "MaritalStatus", "OverTime")

norm_df[cols] <- NULL

train.index <- sample(row.names(norm_df), 0.8 * dim(norm_df)[1])
valid.index <- setdiff(row.names(norm_df), train.index)
train.df <- norm_df[train.index, ]
valid.df <- norm_df[valid.index, ]

norm.values <- preProcess(train.df, method = "range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

res <- cor(train.norm.df)
round(res, 2)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)


###########################################################################################################3

library(e1071)

train_svm = train.norm.df                         
valid_svm = valid.norm.df   
accu <- factor(valid_svm$Attrition_Yes)

classifier = svm(formula = Attrition_Yes ~ .,
                 data = train_svm,
                 type = 'C-classification',
                 kernel = 'linear')

y_pred = predict(classifier,  valid_svm)
table(y_pred, valid_svm[, 18], dnn=c("Prediction", "Actual"))   
confusionMatrix(y_pred, accu)


