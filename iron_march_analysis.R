library(tidyverse)
library(tidytext)

df <- read_csv('core_message_posts.csv')

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

df <- df %>% 
  mutate(msg_post = cleanFun(msg_post))

df %>% 
  unnest_tokens(word, msg_post) %>% 
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  head(20) %>%
  mutate(word = factor(word, word)) %>%
  ggplot(mapping = aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "#d2232a") +
  #scale_x_discrete(name=NULL) +
  coord_flip() +
  scale_x_discrete(name = 'Word') +
  scale_y_continuous(name = 'Count of Times Used') +
  ggtitle("Top 20 Words Used by Iron March Users", subtitle = "Based on Iron March Forums' SQL Dump, Nov. 2019")


words <- df %>% 
  unnest_tokens(word, msg_post) %>% 
  count(word, msg_author_id, sort = TRUE) 

tot <- words %>% 
  group_by(msg_author_id) %>% 
  summarize(total = sum(n))

words <- left_join(words, tot)

words <- words %>% 
  bind_tf_idf(word, msg_author_id, n)

words %>% select(-total) %>% arrange(desc(tf_idf)) %>% filter(n > 10) 

df %>% 
  filter(msg_author_id == "9864")

bigrams <- df %>% 
  unnest_tokens(bigrams, msg_post, token = "ngrams", n = 2)

bigrams_separated <- bigrams %>%
  separate(bigrams, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!grepl("\\.[a-zA-Z]{1-4}", word1)) %>% 
  filter(!grepl("\\.[a-zA-Z]{1-4}", word2))

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

library(igraph)
library(ggraph)
bigram_graph <- bigram_counts %>%
  filter(n > 25) %>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

  

