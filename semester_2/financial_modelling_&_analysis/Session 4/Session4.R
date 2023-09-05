## REGRESSIONS

setwd("C:/Users/pepec/Desktop/estudios/master/semester 2/Financial modeling/Session 4") #  # settig a working directory to access cvs data file

## changes in the 10-year Treasury constant maturity rate and changes in the
# Moody's seasoned corporate AAA bond yield

dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )
par(mfrow=c(1,1))
pdf("cm10aaa.pdf",width=6,height=5)
plot(cm10_dif,aaa_dif,xlab="change in 10YR T rate", ylab="change in AAA rate")
graphics.off()
options(digits = 3)
summary(lm(aaa_dif ~ cm10_dif))



## Excess returns on the food sector and the market portfolio
data(Capm,package="Ecdat")
attach(Capm)
rfood2 = rfood/100
rmrf2 = rmrf/100
pdf("capm_regression_xyplot.pdf",width=6,height=5) # export graphs as pdf
plot(rmrf2,rfood2,ylab="Food industry excess return",
     xlab="Market excess return")
graphics.off()
options(digits = 3)
summary(lm(rfood2~rmrf2))


##Multiple linear regression with interest rates
summary(lm(aaa_dif ~ cm10_dif + cm30_dif + ff_dif))
plot(as.data.frame(cbind(aaa_dif,cm10_dif,cm30_dif,ff_dif)))



## Anova
anova(lm(aaa_dif ~ cm10_dif + cm30_dif + ff_dif))
anova(lm(aaa_dif~ff_dif+cm30_dif+cm10_dif))


## Testing models
fit1 = lm(aaa_dif ~ cm10_dif)
fit2 = lm(aaa_dif~cm10_dif+cm30_dif)
fit3 = lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)
anova(fit1, fit3)
anova(fit2, fit3)


##Weekly interest rates-Model selection by AIC and BIC
library(leaps)
subsets = regsubsets(aaa_dif~., data=as.data.frame(cbind(cm10_dif,cm30_dif,ff_dif)),nbest=1) # calculate 3 indexes: BIC, CP and R2(adj) 
b = summary(subsets)
b
# ploting the indexes
par(mfrow=c(1,3),lab=c(2,5,3),pch=19)
plot(1:3,b$bic,type="b",xlab="number of variables", ylab="BIC",cex=2.5)
plot(1:3,b$cp,type="b",xlab="number of variables",  ylab="Cp",cex=2.5)
plot(1:3,b$adjr2,type="b",xlab="number of variables", ylab="adjusted R2")

# AIC and BIC
# regression with one variable
sat.lm1 <- lm(aaa_dif ~ cm10_dif) # estimate the regression with 1 varible
sat.lm2 <- lm(aaa_dif~cm10_dif+cm30_dif) # estimate the regression with 2 varibles
sat.lm3 <- lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)# estimate the regression with 3 varibles

# AIC and BIC for one variable regression
# step-by-step
sat.n <- length(aaa_dif) # number of observations
sat.sse1 <- sum(resid(sat.lm1) ^2) # the sum of squared residuals
AIC.selfmade <- sat.n + sat.n*log(2*pi) + sat.n * log(sat.sse1 / sat.n) + 2 * (2+1)
AIC.selfmade
#AIC functin
AIC(sat.lm1, k=2)


BIC.selfmade <- sat.n + sat.n * log(2*pi) + sat.n*log(sat.sse1/sat.n) + log(sat.n) * (2+1)
BIC.selfmade
#BIC functions
AIC (sat.lm1, k=log(sat.n))
BIC (sat.lm1)

#AIC and BIC for 2 variable regression
AIC(sat.lm2, k=2)
BIC (sat.lm2)

#AIC and BIC for  3 variable model
AIC(sat.lm3, k=2)
BIC (sat.lm3)


## Troubleshooting ========
# Leverages example
library(robust)
library(faraway)
set.seed(99) # to insure replicability
x = 1:11 # create a variable x with values from 1 to 11
x[11] = 50 # replace the 11th element with 50 (anomaly)
y=1+x+rnorm(11) # generate y that depends on x
y2 = y 
y2[11] = y[11]-45
x2 = x
x2[11] = 5.5
cexx = c(rep(21,10),19) # create cexx with 10 values of 21, and the last element is 19

# hatvalues () calculates hatvalues

par(mfrow=c(2,2),lwd=1,pch=19)
plot(hatvalues(lm(y~x)),ylab="leverage",main="(a)",ylim=c(0,1))
plot(hatvalues(lm(y2~x)),ylab="leverage",main="(b)",ylim=c(0,1))
plot(hatvalues(lm(y~x2)),ylab="leverage",main="(c)",ylim=c(0,1))
plot(x2,hatvalues(lm(y~x2)),xlab="x",ylab="leverage", main="(d)",ylim=c(0,1))


# Residuals example - Externally studentized residuals - the function -  rstudent()
par(mfrow=c(2,3),lwd=1,pch=19)
plot(rstudent(lm(y~x)),ylab="studentized residual",main="Dataset (a)")
plot(rstudent(lm(y2~x)),ylab="studentized residual",main="Dataset (b)")
plot(rstudent(lm(y~x2)),ylab="studentized residual",main="Dataset (c)")
plot(residuals(lm(y~x)),ylab="residual",main="Dataset (a)")
plot(residuals(lm(y2~x)),ylab="residual",main="Dataset (b)")
plot(residuals(lm(y~x2)),ylab="residual",main="Dataset (c)")



# Cook's distance
par(mfrow=c(2,3),cex.axis=1,cex.lab=1,lwd=1,pch=19)
plot(sqrt(cooks.distance(lm(y~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (a)",ylim=c(0,11))
plot(sqrt(cooks.distance(lm(y2~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (b)",ylim=c(0,11))
plot(sqrt(cooks.distance(lm(y~x2))),ylab=("square root Cook's D"),cex=1,main="Dataset (c)",ylim=c(0,11))
halfnorm( sqrt(cooks.distance(lm(y~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (a)",xlim=c(0,1.85))
halfnorm(sqrt(cooks.distance(lm(y2~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (b)", xlim=c(0,1.85))
halfnorm(sqrt(cooks.distance(lm(y~x2))),ylab=("square root Cook's D"),cex=1,main="Dataset (c)", xlim=c(0,1.85))



#Checking Model Assumptions: Nonlinearity======================================
#generate data
n = 80
set.seed("2020")
e = matrix(runif(12*n),nrow=n) %*% rep(1,12)
e = abs(e)^4
e= e/mean(e) 
x1 = runif(n)
x1 = sort(x1) 
x2 = rbeta(n,6,.5)

y =( 8*x2 + x1 + 5*x1^3) + ( 4* x2 + x1 + 7*x1^3) * e 

par(mfrow=c(1,2))
plot(x1,y,xlab=expression(x[1]))
plot(x2,y,xlab=expression(x[2]))


fit = lm(y~x1+x2)
rstudent = rstudent(fit)
par(mfrow=c(1,2))
qqnorm(rstudent,datax=T,main="Normal QQ Plot")
hist(rstudent,12)




# Simulated bond prices
bondprices = read.table("bondprices.txt", header = TRUE) # load the txt file

attach(bondprices) #Attach Set of R Objects to Search Path. This means that the database is searched by R when evaluating a variable, so objects in the database can be accessed by simply giving their names.
fit = nls(price ~ 1000 * exp(-r * maturity), start = list(r = 0.04)) # fit a nonlinear model (Nonlinear Least Squares), for simulation a starting point is taken r=0.04

summary(fit) # show the estimates


par(mfrow=c(1,1)) # set or query graphical parameters - here one graph; for details help (par)
plot(maturity,price,pch="*",cex = 2) # building the the graph using observations

grid = seq(0, 20, length=201)
price_grid = 1000*exp(-0.0585*grid)
lines(grid,price_grid, lwd = 2, col = "red")
legend("topright",c("price","predicted price"),pch=c("*",NA), col = c("black","red"), lty=c(NA,1),pt.cex=c(2,1))

detach(bondprices)



##Example: Estimating default probabilities

DefaultData = read.table("DefaultData.txt",header=T) # 1 to 16 scale credit rating (Aaa = 1, . . . , B3 = 16)
attach(DefaultData) # to refer to the column names as variables 

freq2=freq/100 # freq is the column from DefaultData

y = log(freq2[freq2>0]) # we cannot take log (0)

fit_bow = lm(y ~ rating[freq>0])

fit_nls = nls(freq2 ~ exp(b1+b2*rating), start=list(b1=-5,b2=.5))
summary (fit_nls)

fit_tbs = nls(sqrt(freq2) ~ exp(b1/2+b2*rating/2), start=list(b1=-6,b2=.5))
summary (fit_tbs)

sum_nls = summary(fit_nls)
coef_nls = as.numeric(sum_nls$coef[1:2])
sum_tbs = summary(fit_tbs)
coef_tbs = as.numeric(sum_tbs$coef[1:2])
rate_grid = seq(1,16,by=.01)
coef_bow = fit_bow$coefficients

par(mfrow=c(1,2)) # figure left
plot(rating,freq2,ylim=c(-.0001,.13),pch="*",ylab="frequency",cex=1.5)
lines(rate_grid,exp( coef_nls[1]+coef_nls[2]*rate_grid))

legend("topleft",c("exponential","data"),lty=c(1,NA),
       pch=c("","*"),pt.cex=c(1, 1.5))
plot(rate_grid, (coef_bow[1]+rate_grid*coef_bow[2]),
     type="l",ylim=c(-14.15,1),xlab="rating",ylab="log(default probability)")
lines(rate_grid,( coef_nls[1]+coef_nls[2]*rate_grid) ,lty=2,col="red")
lines(rate_grid,( coef_tbs[1]+coef_tbs[2]*rate_grid) ,lty=6,col="blue")
points(rating,log(freq2+1e-6))
legend("topleft",c("BOW","nonlinear","tbs","data"),lty=c(1,2,6,NA),
       pch=c("","","","o"),col=c("black","red","blue"))


par(mfrow=c(1,2)) # 
fitted_nls = -sum_nls$resid+freq2
plot(fitted_nls,abs(sum_nls$resid),xlab="fitted values", ylab="absolute residual")
fit_loess  = loess(abs(sum_nls$resid)~ fitted_nls,span=1,deg=1)
ord_nls = order(fitted_nls)
lines(fitted_nls[ord_nls],fit_loess$fit[ord_nls])
qqnorm(sum_nls$resid,datax=T,main="",ylab="sample quantiles", xlab="theoretical quantiles")
qqline(sum_nls$resid,datax=T)





## Example:  transformation=========================================================
initech = read.csv("initech.csv")

plot(salary ~ years, data = initech, col = "grey", pch = 45, cex = 2, main = "Salaries at Initech, By Seniority") # years on the horizontal 
plot(years ~salary  , data = initech, col = "grey", pch = 20, cex = 1.5, main = "Salaries at Initech, By Seniority") # years on the vertical 
# col = color )
# pch controls the shape of points - you get 25 symbols to choose from, as well as alphabetic characters. When pch is 21:25, the points also get a background color which is set using bg
# cex controls the size of the points


initech_fit = lm(salary ~ years, data = initech) #We first fit a simple linear model.
summary(initech_fit)

plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5, main = "Salaries at Initech, By Seniority")
abline(initech_fit, col = "darkorange", lwd = 2)

par(mfrow = c(1, 2)) # figures will in in one row two columns

plot(fitted(initech_fit), resid(initech_fit), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(initech_fit), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(initech_fit), col = "dodgerblue", lwd = 2)


initech_fit_log = lm(log(salary) ~ years, data = initech)
plot(log(salary) ~ years, data = initech, col = "grey", pch = 20, cex = 1.5, main = "Salaries at Initech, By Seniority")
abline(initech_fit_log, col = "darkorange", lwd = 2)

plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5, main = "Salaries at Initech, By Seniority")
curve(exp(initech_fit_log$coef[1] + initech_fit_log$coef[2] * x), from = 0, to = 30, add = TRUE, col = "darkorange", lwd = 2)



par(mfrow = c(1, 2))
plot(fitted(initech_fit_log), resid(initech_fit_log), col = "grey", pch = 20, xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(initech_fit_log), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(initech_fit_log), col = "dodgerblue", lwd = 2)

# RMSE
sqrt(mean(resid(initech_fit) ^ 2))
## [1] 27080
sqrt(mean(resid(initech_fit_log) ^ 2))
## [1] 0.193

sqrt(mean((initech$salary - fitted(initech_fit)) ^ 2))
## [1] 27080
sqrt(mean((initech$salary - exp(fitted(initech_fit_log))) ^ 2))
## [1] 24280

summary(initech_fit_log)
summary (initech_fit)



# Box-Cox transformation
library(MASS)
library(faraway)
savings # show the data

savings_model = lm(sr ~ ., data = savings) # estimate a general regression, sr is y, all the rest variables are Xs
boxcox(savings_model, plotit = TRUE) # determin the value of lambda
boxcox(savings_model, plotit = TRUE, lambda = seq(0.5, 1.5, by = 0.1)) # close up look at the best selcted values from the previouse step

plot(fitted(savings_model), resid(savings_model), col = "dodgerblue", pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

#studentized Breusch-Pagan test
library(lmtest)
bptest(savings_model)

#Shapiro-Wilk normality test
shapiro.test(resid(savings_model))



##Example Box-cox transformation - the second dataset
gala_model = lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
plot(fitted(gala_model), resid(gala_model), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

boxcox(gala_model, lambda = seq(-0.25, 0.75, by = 0.05), plotit = TRUE)

# transformation
gala_model_cox = lm((((Species ^ 0.3) - 1) / 0.3) ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
plot(fitted(gala_model_cox), resid(gala_model_cox), col = "dodgerblue", pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

#initech
boxcox(initech_fit)



## use trafo
library (trafo)
library(Ecdat) # for data
data(University) # dataset University

linMod <- lm(nassets ~ stfees, data = University)
summary (linMod)

plot(fitted(linMod), resid(linMod), col = "dodgerblue",     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)


plot(nassets ~ stfees, data = University, col = "grey", pch = 20, cex = 1.5,
     main = "how study fees (stfees) raise the universities net assets (nassets)")
abline(linMod, col = "darkorange", lwd = 2)


assumptions(linMod)

linMod_trafo <- trafo_lm(linMod)
diagnostics(linMod_trafo)
plot (linMod_trafo)


linMod_trafo2 <- trafo_lm(object = linMod, trafo = "logshiftopt", method = "skew")





### Binary Regression ===================================

# Who gets a credit card?
library("AER")
library("faraway")
library(MASS)

data("CreditCard")
CreditCard_clean = CreditCard[CreditCard$age>18,] # we dont include possibile mistakes and individuals who have less than 18 y.

attach(CreditCard_clean)
names(CreditCard) # show the variables

# figures
par(mfrow=c(3,3)) 
hist(reports,main="reports")
hist(income, main="income")
hist(share, main="share")
hist(age, main="age")
owner2 = c( sum((owner=="yes")),sum((owner=="no")))
hist(as.numeric(owner), main="owner",breaks=2,xlab=" no   yes",axes=F,ylab="")
h=hist(dependents,main="dependents",breaks=(0:7)-.5)
hist(months,main="months")
hist(log(share),main="log(share)")
hist(log(reports+1),main="log(reports+1)")

fit1= glm(card~log(reports+1)+income+log(share)+age+owner+dependents+months,
          family="binomial",data=CreditCard_clean) # Fitting Generalized Linear Models

summary(fit1)
stepAIC(fit1) # Choose a model by AIC in a Stepwise Algorithm

# For convenience the variables are mean-centered
log_reports_c = log(reports+1)
log_reports_c = log_reports_c - mean(log_reports_c)
income_c = income - mean(income)
log_share_c = log(share) - mean(log(share))
dependents_c = dependents - mean(dependents)

glm_fit02 = glm(card~log_reports_c+income_c+log_share_c+dependents_c, family="binomial",data=CreditCard_clean)
summary(glm_fit02)

income_grid = seq(min(income),max(income),.01)
share_grid = exp(seq(min(log(share)),max(log(share)),.01))
share_grid2 = log(share_grid) - mean(log(share))
reports_grid = 0:14
reports_grid2 = log(reports_grid+1) - mean(log(reports+1))

par(mfrow=c(2,2))
plot(reports_grid, plogis(9.5238 -2.8953 * reports_grid2 ),type="b",
     lwd=2,xlab="reports",ylab="P(accept)",ylim=c(0,1))
plot(income_grid, plogis(9.5238 + 0.8717 *(income_grid-mean(income)) ),type="l",
     cex=2,lwd=2,xlab="income",ylab="P(accept)",ylim=c(0,1))
plot(log(share_grid), plogis(9.5238  + 3.3102  * share_grid2 ),type="l",
     cex=2,lwd=2,xlab="log(share)",ylab="P(accept)",ylim=c(0,1))
plot(0:6,plogis(9.5238 - .5506*((0:6)-mean(dependents)) ),type="b",
     lwd=2,xlab="dependents",ylab="P(accept)",ylim=c(0,1))


par(mfrow=c(2,2))
plot(log(share),as.numeric(card)-1,ylab="card",main="(a)" )
plot(log(share),reports,main="(b)" )
plot(log(share),income,main="(c)" )
plot(log(share),majorcards,main="(d)" )

detach(CreditCard_clean)

