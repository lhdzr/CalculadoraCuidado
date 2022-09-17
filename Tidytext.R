library(tidyverse)
library(tidytext)

# LECTURA DE DATOS
pages = read.csv("products/csv/pages.csv",encoding = 'utf-8')

# TOKENIZAR POR PALABRAS
tidy_pages = pages %>%
  select(id,text) %>%
  unnest_tokens("word", text)

# DICCIONARIO DE STOPWORDS
data("stop_words")
stop_words = bind_rows(stop_words, data_frame(word = c(tm::stopwords("spanish"),"rt","https","t.co"),
                                              lexicon = "custom"))

# PALABRAS MÁS COMUNES
top_words = tidy_pages %>%
  anti_join(stop_words)

# GRÁFICO CON PALABRAS MÁS COMUNES
top_words %>%
  count(word, sort = TRUE) %>%
  filter(n > 250) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(x = "Frecuencia", y=NULL) +
  ggtitle("Cuidados",
          subtitle = "Términos más populares")


