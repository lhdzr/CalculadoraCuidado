# data processing
library(rtweet)
library(tidyverse)
library(tidytext)
library(ggwordcloud)

### IMPORT RAW TWEET DATA
chile_raw = read_twitter_csv("products/csv/chile_tweets_raw.csv")


### CONVERT TO TIDY FORMAT
tidy_chile = chile_raw %>%
  select(id_str,full_text) %>%
  unnest_tokens("word", full_text)

write.csv(tidy_chile,"products/csv/tidy_chile.csv")

data("stop_words")
stop_words = bind_rows(stop_words, data_frame(word = c(tm::stopwords("spanish"),"rt","https","t.co"),
                                              lexicon = "custom"))
top_words = tidy_chile %>%
  anti_join(stop_words)


# GRÁFICO CON PALABRAS MÁS FRECUENTES
top_words %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(x = "Frecuencia", y=NULL) +
  ggtitle("Plebiscito Nueva Constitución Chilena",
          subtitle = "Términos más populares")


### EXTRACT HASHTAGS
chile_hashtags = chile_raw %>% 
  unnest_tokens(output = "hashtag", input = "text", token = "tweets") %>%
  filter(str_starts(hashtag, "#"))

chile_hashtags = chile_hashtags %>%
  mutate(pleb_hashtag = case_when(str_detect(hashtag, "apruebo") ~1,
                                  str_detect(hashtag, "rechazo") ~1,
                                  str_detect(hashtag, "constitucion") ~1,
                                  str_detect(hashtag, "llaitul") ~1,
                                  str_detect(hashtag, "boric") ~1,
                                  TRUE ~ 0)) %>%
  mutate(pleb_hashtag = as.character(pleb_hashtag))

chile_hashtags %>% count(pleb_hashtag) %>% mutate(freq = n / sum(n))


# GRÁFICO DE BARRAS DE HASHTAGS MÁS UTILIZADOS
chile_hashtags %>%
  filter(pleb_hashtag == "1") %>%
  count(hashtag, sort = TRUE) %>%
  arrange(-n) %>%
  mutate(hashtag = reorder(hashtag, n)) %>%
  slice(1:10) %>%
  ggplot(aes(n,hashtag)) +
  geom_col() +
  labs(x = "Frecuencia", y=NULL) +
  ggtitle("Plebiscito Nueva Constitución Chilena",
          subtitle = "Hashtags más utilizados")


## AGREGAR COLUMNA APRUEBA SEGÚN USO DE HASHTAGS #APRUEBO Y #RECHAZO
chile_hashtags = chile_hashtags %>%
  mutate(aprueba = case_when(str_detect(hashtag, "apruebo") ~2,
                             str_detect(hashtag, "rechazo") ~0,
                             TRUE ~ 1)) %>%
  mutate(aprueba = as.character(aprueba))

head(chile_hashtags$aprueba)

## HASHTAG WORDCLOUD
data_hashtags_wordcloud <- chile_hashtags %>%
  count(hashtag, pleb_hashtag) %>%
  arrange(-n) %>%
  slice(1:35)

# GRÁFICO WORDCLOUD
ggplot(data_hashtags_wordcloud,
      aes(label = hashtag, size = n, color = pleb_hashtag)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 8) + # we set a maximum size for the text
  theme_void()



### SEARCH BY SUBJECT (LLAITUL)
llaitul = chile %>%
  unnest_tokens(output = "llaitul_tk", input = "text", token = "tweets") %>%
  filter(str_detect(llaitul_tk,"llaitul"))

