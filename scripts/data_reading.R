# Este archivo lee los tweets una sola vez, genera un dataframe de tweets
# y un dataframe tidy de las palabras en los tweets, el cual luego agrega al 
# repositorio como csv para trabajarlo por el resto del proyecto.

library(rtweet)
library(tidyverse)
library(tidytext)

chile_raw = search_tweets(q="boric OR chile OR constitucion OR apruebo OR rechazo",
                      n = 1000
                      )

tidy_chile = chile_raw %>%
  select(id_str,full_text) %>%
  unnest_tokens("word", full_text)


write_as_csv(chiledf, "products/csv/chile_tweets_raw.csv", prepend_ids = TRUE, 
             na = "", fileEncoding = "UTF-8")
write.csv(tidy_chile,"products/csv/tidy_chile.csv")
