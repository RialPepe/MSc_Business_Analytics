#Session 2 - Basic R text analysis Scripts

paste(c('B', 'U', '7147'), collapse='|')
paste(c('B', 'U', '7147'), collapse='')
paste0('B', 'U', '7147')

x=substr("Red fox jumped over the sleeping rabbit",10, 20)

strsplit("Red fox jumped over the sleeping rabbit", " ")


# regex - grep

y=c('red fox', 'yellow fox', 'red rabbit')
grepl(y, pattern = '^r.*fox$')

library(dplyr)
library(tidyverse)
library(tidytext)
y %>% grepl(pattern = '^r.*fox$')




# Document organization

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text_df<- text %>% tibble(line=1:4)

text_df <- tibble(line = 1:4, text = text)



token1<-text_df %>% unnest_tokens(word, text)
token2<-text_df %>% unnest_tokens(word, text, token = "ngrams", n=2)

table(token1$line)


#cleaning and clearing a file- HG wells example
library(gutenbergr)
library(urltools)
library(Rcpp)
data("stop_words")
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)


# tf-idf exercise

library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

count(unnest_tokens(austen_books(), word, text),book, word, sort=TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words


library(ggplot2)

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")



book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_tf_idf

arrange(book_tf_idf,desc(tf_idf))


library(forcats)

book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


# Sentiment mining

library(tidytext)

get_sentiments("afinn")


library(janeaustenr)
library(dplyr)
library(stringr)
library(textdata)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")


tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)


bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")



# sentence level sentiments
p_and_p_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

install.packages("sentimentr")
library(sentimentr)

# https://cran.r-project.org/web/packages/sentimentr/readme/README.html

sentiment(p_and_p_sentences$sentence)
