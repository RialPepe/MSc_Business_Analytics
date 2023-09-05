
library(NPL)
library(tm)
library(SnowballC)
library(wordcloud)
library(syuzhet)
library(corpus)
library(dplyr)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(fastDummies))
suppressPackageStartupMessages(library(neuralnet))
library(readr)


#Divide the datasheet

df_test <- read_csv("glassdoor_reviews.csv")
companies <- c('EY','PwC','Deloitte','KPMG')
data_kpmg <- df_test %>% filter(firm%in%companies)
cities <- data_kpmg %>%
  group_by(location) %>%
  count(location)

View(cities)
cities_big_review <- c()

for (i in 1:746) {
  
  if(cities[i, 2] > 100){
    
    cities_big_review <- c(cities_big_review, cities$location[i])
    
  }
}

cities_big_review[30] <- 0


df_test2 <- read_csv("glassdoor_reviews.csv")
companies <- c('KPMG')
data_kpmg <- df_test %>% filter(firm%in%companies)
last_data_kpmg <- data_kpmg %>% arrange(desc(date_review))
data_per_city <- last_data_kpmg %>% filter(location%in%cities_big_review)
data_text <-  data_per_city[, -c(1,2,3,4,6,7,8,9,10,11,12,13,14,15)]

View(data_text)

#---------------------------------------------------------

data_city_emotion <- c("City")
rm(emotional_data)
emotional_data <-c()
City <- c()
Anger<- c()
Anticipation <- c()
Disgust <- c()
Fear <- c()
Joy <- c()
Sadness <- c()
Surprise <- c()
Trust <- c()

for (i in 1:29) {
  
  
  data_cities <- data_text %>% filter(location == cities_big_review[i])
  
  city_emotion <- "City"
  num_reviews <- nrow(data_cities)
  
  for (j in 1:100) {
    
    
    city_emotion <- c(city_emotion, data_cities$headline[j], data_cities$pros[j], data_cities$cons[j])
    
  }
  
  d <- get_nrc_sentiment(city_emotion, language = "english")
  
  #Transformation and cleaning for Plotting 
  td<-data.frame(t(d))
  td_new <- data.frame(rowSums(td[2:20]))
  names(td_new)[1] <- "count"
  td_new <- cbind("sentiment" = rownames(td_new), td_new)
  rownames(td_new) <- NULL
  td_new2<-td_new[1:8,]
  
  City <- c(City, cities_big_review[i])
  Anger<- c(Anger, td_new2$count[1])
  Anticipation <- c(Anticipation, td_new2$count[2])
  Disgust <- c(Disgust, td_new2$count[3])
  Fear <- c(Fear, td_new2$count[4])
  Joy <- c(Joy, td_new2$count[5])
  Sadness <- c(Sadness, td_new2$count[6])
  Surprise <- c(Surprise, td_new2$count[7])
  Trust <- c(Trust, td_new2$count[8])
  
  
}

Emotion <- data.frame(City, Anger,Anticipation,Disgust,Fear,
                      Joy,Sadness,Surprise,Trust)

write.csv2(Emotion, "Emotion_tableau.csv", row.names=FALSE)


#Plot Emotion Classification 
quickplot(sentiment, data=td_new2, weight=count, geom="bar", 
          fill=sentiment, ylab="count")+ggtitle("Survey sentiments")
.
