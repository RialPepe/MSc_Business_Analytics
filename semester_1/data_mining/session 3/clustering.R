rm(list=ls(all=TRUE))

#loading libraries
library(FNN)

library(caret)

library(pROC)

library(RColorBrewer)


#loading data
Airline.df <- read.csv("EastWestAirlinesCluster.csv")


#data exploration
head(Airline.df)

summary(Airline.df)

View (Airline.df)


#normalising the data
airlines.df.norm <- sapply(Airline.df[-1], scale)
airlines.df.norm


#clustering
d.norm <- dist(airlines.df.norm, method = "euclidean")
cluster1<-hclust(d.norm,method='ward.D')
plot(cluster1, hang= -3, ann= TRUE)
clust_types<-cutree(cluster1,k=2)


#normalisation test 
d <- dist(Airline.df, method = "euclidean")

hc2 <- hclust(d, method = "ward.D")

plot(hc2, hang = -1, ann = FALSE)


#getting centroids
centers<-aggregate(.~clust_types,data=Airline.df[-1], FUN= mean)
centers


#clsutering 2
set.seed(2)
sample_data<-sample(airlines.df.norm,length(airlines.df.norm)*0.95)
d_norms<-dist(sample_data,method='euclidean')
cluster2<-hclust(d_norms,method='ward.D')
plot(cluster2, hang= -3, ann= TRUE)


set.seed(1)
reomve.ind <- sample(1:length(clust_types), length(clust_types)*0.05)
d.norm <- dist(airlines.df.norm[-reomve.ind,], method = "euclidean")
hc2 <- hclust(d.norm, method = "ward.D")
plot(hc2, hang = -1, ann = FALSE)


#kmeans clustering
km<-kmeans(airlines.df.norm,2)
table(clust_types,km$cluster)
