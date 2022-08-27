library(tm)
library(tidytext)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)
library(textdata)
library(rtweet)
library(tidyverse)


chile = search_tweets(q="boric OR chile OR constitucion OR apruebo OR rechazo",
                      n = 1000,
                      )

chile$full_text[str_detect(chile$full_text,"septiembre")]

chile %>% count(lang) %>% mutate(freq = n /sum(n)) %>% arrange(desc(n))

chile_corpus = Corpus(VectorSource(as.vector(chile$full_text)))
head(chile_corpus)

tidy_chile = chile %>%
  select(created_at,full_text) %>%
  unnest_tokens("word", full_text)
head(tidy_chile)

tidy_chile %>%
  count(word) %>%
  arrange(desc(n))

data("stop_words")
stop_words = bind_rows(stop_words, data_frame(word = c(tm::stopwords("spanish"),"rt","https","t.co"),
                       lexicon = "custom"))

top_words = tidy_chile %>%
  anti_join(stop_words)

top_words %>%
  count(word) %>%
  arrange(desc(n))

top_words %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(x = "Frecuencia", y=NULL) +
  ggtitle("Plebiscito Nueva Constitución Chilena",
          subtitle = "Términos más populares")

tidy_chile_tfidf = chile %>%
  select(created_at,full_text) %>%
  unnest_tokens("word",full_text)

chile_hashtags = chile %>% 
  unnest_tokens(output = "hashtag", input = "text", token = "tweets") %>%
  filter(str_starts(hashtag, "#"))

chile_hashtags = chile_hashtags %>%
  mutate(pleb_hashtag = case_when(str_detect(hashtag, "apruebo") ~1,
                                  str_detect(hashtag, "rechazo") ~1,
                                  TRUE ~ 0)) %>%
  mutate(pleb_hashtag = as.character(pleb_hashtag))

chile_hashtags %>% count(pleb_hashtag) %>% mutate(freq = n / sum(n))

llaitul = chile %>%
  unnest_tokens(output = "llaitul_tk", input = "text", token = "tweets") %>%
  filter(str_detect(llaitul_tk,"llaitul"))
