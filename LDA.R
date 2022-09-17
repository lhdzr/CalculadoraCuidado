library(topicmodels)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)


pages_dtm = cast_dtm(tidy_pages)


paper_words <- pages %>%
  unnest_tokens(word, text) %>%
  count(paper_id, word, sort = TRUE)

# DICCIONARIO DE STOPWORDS
data("stop_words")
stop_words = bind_rows(stop_words, data_frame(word = c(tm::stopwords("spanish"),"rt","https","t.co"),
                                              lexicon = "custom"))

# ELIMINAR STOPWORDS
paper_words = paper_words %>%
  anti_join(stop_words)

total_words <- paper_words %>% 
  group_by(paper_id) %>% 
  summarize(total = sum(n))

paper_words <- left_join(paper_words, total_words)

paper_words

freq_by_rank <- paper_words %>% 
  group_by(paper_id) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)


paper_tf_idf <- paper_words %>%
  bind_tf_idf(word, paper_id, n)

paper_tf_idf

paper_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

paper_bigrams <- pages %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

paper_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- paper_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts


bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united


bigram_tf_idf <- bigrams_united %>%
  count(paper_id, bigram) %>%
  bind_tf_idf(bigram, paper_id, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf
bigram_counts[1:20,]

library(igraph)

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph
library(ggraph)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


papers_dtm = paper_words %>% 
  cast_dtm(paper_id,word,n)

papers_lda = LDA(papers_dtm, k=10, control = list(seed = 1234))
papers_lda

papers_topics = tidy(papers_lda, matrix = "beta")
papers_topics

papers_top_terms <- papers_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

papers_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
