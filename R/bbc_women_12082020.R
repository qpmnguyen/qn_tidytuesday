library(tidytuesdayR)
library(tidyverse)
library(ggsci)
library(tidytext)

tuesdata <- tidytuesdayR::tt_load(2020, week = 50)
df <- tuesdata$women

data(stop_words)

df <- df %>% mutate(role = str_to_lower(role), description = str_to_lower(description))
df <- df %>% select(category, role)
df <- df %>% unnest_tokens(word, role) %>% filter(category != "All") %>% anti_join(stop_words) %>% 
  group_by(category) %>% count(word, sort = TRUE) %>% bind_tf_idf(word, category, n) %>% 
  arrange(desc(tf_idf))
plots <- df %>% group_by(category) %>% slice_max(tf_idf, n=4) %>% ungroup() %>% mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(tf_idf, word, fill = category)) + geom_col(show.legend = FALSE) + 
  facet_wrap(~category, scales = "free") + labs(x = "tf-idf", y = NULL) + scale_fill_nejm() + theme_bw()

ggsave(plots, filename = "figures/bbc_women_12082020.png", width = 7)
