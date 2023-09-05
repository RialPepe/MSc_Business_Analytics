rm(list=ls(all=TRUE))

Toyota.df <- read.csv("ToyotaCorolla.csv")

summary(Toyota.df)

View(Toyota.df)
unique(Toyota.df$Gears)
str(Toyota.df)

  Toyota.df$Gears <- as.factor(Toyota.df$Gears)

str(Toyota.df)


View(Toyota.df)

fuel_type.cat<-model.matrix(~factor(Toyota.df$Fuel_Type))



library(fastDummies)

data<-dummy_cols(Toyota.df$Gears)


cor(na.omit(Toyota.df[,-c(1,2,5,6,8,10:12,14:16,19:39)]))


plot(cor(na.omit(Toyota.df[,-c(1,2,5,6,8,10:12,14:16,19:39)])))



wine.df<-read.csv("Wine.csv")
str(wine.df)


pcs.cor<-prcomp(wine.df[-1])

summary(pcs.cor)


wine.df[-1]
