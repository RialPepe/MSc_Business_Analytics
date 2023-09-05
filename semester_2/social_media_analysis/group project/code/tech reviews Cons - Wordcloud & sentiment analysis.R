#Install the neccesary libraries: 
library(readr)
library(magrittr)
library(dplyr)
library(tm)
library(wordcloud)
library(tidytext)
library(ggplot2)
library(syuzhet)


#Loading the data:
data <- read_csv("C:/Users/pepec/Desktop/estudios/master/semester 2/social media analysis/group project/glassdoor_reviews.csv")


#filtering the data by companies:
tech_data <- data %>%
  filter(firm == c("Google", "Facebook", "Apple", "Microsoft"))

cons <- tech_data$cons
cons_source <- VectorSource(tech_data$cons)
cons_corpus <- VCorpus(cons_source)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"),"company", "google", "facebook", "apple", "microsoft",
                                          "pwc","kpmg","deloitte",
                                          "corporation","enterprise","employee",
                                          "work","employ","current","year",
                                          "associ","senior", "good", "great", "working",
                                          "get", "one", "will", "also", "life", "can",
                                          "within", "many", "much", "long", "well", "may", 
                                          "really", "etc"))
  return(corpus)
}

clean_corp <- clean_corpus(cons_corpus)

cons_tdm <- TermDocumentMatrix(clean_corp)

cons_m <- as.matrix(cons_tdm)

term_frequency <- rowSums(cons_m)

term_frequency <- sort(term_frequency, decreasing=TRUE)

word_freqs <- data.frame(term=names(term_frequency), num=term_frequency)

set.seed(123)
my_colors <- c("#33a02c", "#1f78b4", "#e31a1c")
wordcloud(word_freqs$term,word_freqs$num, max.words = 100, random.order = FALSE, 
          colors = my_colors[findInterval(word_freqs$num, c(0, quantile(word_freqs$num, c(0.5, 0.9)))) + 1],
          scale = c(3, 0.7), rot.per = 0.15, shape = "circle", 
          par.settings = list(mar = c(0, 0, 0, 0)))

# Get the 20 most frequent terms
top_terms <- head(term_frequency, n = 10, sort = TRUE)
top_freq_df <- data.frame(word = names(top_terms), freq = top_terms)

# Create a bar chart using ggplot2
ggplot(top_freq_df, aes(x = reorder(word, -freq) , y = freq)) +
  geom_bar(stat = "identity", fill = "#1f78b4", color = "black", size = 0.9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 10, face = "bold")) +
  ggtitle("Word Frequencies") +
  xlab("Words") +
  ylab("Frequency")

#Word Association 
associations <- findAssocs(cons_tdm, terms = c("management", "people", "time", "hard", "hours",
                                               "like", "lot", "managers","sometimes", "balance"), corlimit = 0.2)
associations

# Get sentiment scores using afinn package
afinn_vector <- get_sentiment(cons, method="afinn")
summary(afinn_vector)