setwd("~/R/Trinity/Session4/BlackFriday") 

blackFriday = read.csv("BlackFriday.csv", header = TRUE)
summary(blackFriday)

str(blackFriday)

sum (blackFriday$Gender=="M")
sum(blackFriday$Gender=="F")
sum(blackFriday$Age =="46-50")
sum(blackFriday$City_Category=="A")

levels(blackFriday$Age)

blackFriday$Gender = as.factor(blackFriday$Gender)
str(blackFriday)

blackFriday$Age = as.factor(blackFriday$Age)
blackFriday$Occupation = as.factor(blackFriday$Occupation)
blackFriday$City_Category = as.factor(blackFriday$City_Category)
blackFriday$Marital_Status = as.factor(blackFriday$Marital_Status)
blackFriday$Stay_In_Current_City_Years = as.factor(blackFriday$Stay_In_Current_City_Years)

str(blackFriday)

boxplot(blackFriday$Purchase)

ready.BlackFriday = blackFriday [, -c(1, 2, 6, 9:11)]
str(ready.BlackFriday)

colSums(is.na(ready.BlackFriday))

linear.model = lm (Purchase ~ ., data = ready.BlackFriday)
summary(linear.model)

ready.BlackFriday2 = ready.BlackFriday[, -c(3)]

logistic.model = glm(Marital_Status ~ ., data = ready.BlackFriday2, family = "binomial")
summary(logistic.model)

coefficients = t (coef(logistic.model))

temp.row = ready.BlackFriday2[5,]
to.predict = data.frame( matrix( ncol = ncol(coefficients), nrow = 1, 0))
colnames(to.predict) = colnames(coefficients)

to.predict$GenderM = 1
to.predict$`Age55+`= 1
to.predict$`Stay_In_Current_City_Years4+`=1
to.predict$Purchase = 7969


model.response1 = exp(
  coefficients[1]+
    coefficients[2]*to.predict[2]+
    coefficients[3]*to.predict[3]+
    coefficients[4]*to.predict[4]+
    coefficients[5]*to.predict[5]+
    coefficients[6]*to.predict[6]+
    coefficients[7]*to.predict[7]+
    coefficients[8]*to.predict[8]+
    coefficients[9]*to.predict[9]+
    coefficients[10]*to.predict[10]+
    coefficients[11]*to.predict[11]+
    coefficients[12]*to.predict[12]+
    coefficients[13]*to.predict[13])/
  (1+exp((
    coefficients[1]+
      coefficients[2]*to.predict[2]+
      coefficients[3]*to.predict[3]+
      coefficients[4]*to.predict[4]+
      coefficients[5]*to.predict[5]+
      coefficients[6]*to.predict[6]+
      coefficients[7]*to.predict[7]+
      coefficients[8]*to.predict[8]+
      coefficients[9]*to.predict[9]+
      coefficients[10]*to.predict[10]+
      coefficients[11]*to.predict[11]+
      coefficients[12]*to.predict[12]+
      coefficients[13]*to.predict[13])))

model.response1

model.response2 = predict(logistic.model, newdata = temp.row, type = "response")
model.response2

to.predict = data.frame( matrix( ncol = ncol(coefficients), nrow = 1, 0))
colnames(to.predict) = colnames(coefficients)

to.predict$GenderM = 0
to.predict$`Age26-35`= 1
to.predict$Stay_In_Current_City_Years3=1
to.predict$Purchase = 200

temp.row$Gender ="F"
temp.row$Age = "26-35"
temp.row$Stay_In_Current_City_Years="3"
temp.row$Purchase = 200


model.response3 = predict(logistic.model, newdata = temp.row, type = "response")
model.response3

