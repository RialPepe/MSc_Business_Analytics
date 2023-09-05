
library(NPL)
library(tm)
library(syuzhet)
library(dplyr)
suppressPackageStartupMessages(library(ggplot2))
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

for (i in 1:2017) {
  
  if(cities[i, 2] > 100){
    
    cities_big_review <- c(cities_big_review, cities$location[i])
    
  }
}


big4_cities <- c("Buenos Aires","Cairo", "Dublin, Dublin","Hong Kong","London, England, England","Madrid", "New York, NY",
                 "Paris","Rio de Janeiro","Rome", "Singapore", "Tel Aviv-Yafo","Tokyo", "Toronto, ON")

df_test2 <- read_csv("glassdoor_reviews.csv")
companies <- c('EY','PwC','Deloitte','KPMG')
data_kpmg <- df_test %>% filter(firm%in%companies)
last_data_kpmg <- data_kpmg %>% arrange(desc(date_review))
data_per_city <- last_data_kpmg %>% filter(location%in%big4_cities)
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

  
  city_emotion <- "/"
  
  for (j in 1:20000) {
    
    
    city_emotion <- c(city_emotion, data_text$headline[j], data_text$pros[j], data_text$cons[j])
    
  }
  
  d <- get_nrc_sentiment(city_emotion, language = "english")
  
  #Transformation and cleaning for Plotting 
  td<-data.frame(t(d))
  td_new <- data.frame(rowSums(td[2:20]))
  names(td_new)[1] <- "count"
  td_new <- cbind("sentiment" = rownames(td_new), td_new)
  rownames(td_new) <- NULL
  td_new2<-td_new[1:8,]
  
  quickplot(sentiment, data=td_new2, weight=count, geom="bar", 
            fill=sentiment, ylab="count")+ggtitle("Survey sentiments", big4_cities[i])
  
  
  City <- c(City, big4_cities[i])
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



