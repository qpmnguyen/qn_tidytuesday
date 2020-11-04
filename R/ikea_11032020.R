library(tidyverse)
library(tidytuesdayR)
library(patchwork)
library(ggsci)

data <- tidytuesdayR::tt_load(2020, week = 45)
data <- data$ikea

# first, we remove all designers that start with a number, since that is where there is no designer and the 
# text extraction was wrong 
data <- data %>% mutate(designer = if_else(str_detect(designer, "([0-9]+).*$"), NA_character_, designer)) 
data <- select(data, -X1)
designers <- data$designer  

# Splitting up by "/" character and combine them into a list of unique designers 
designers <- str_split(designers, "/") %>% do.call(c,.) %>% unique()
designers <- designers[-which(designers == "IKEA of Sweden")] # remove generic producer


# Start a new data frame with designers as the primary object  
df <- tibble(designers = designers)

# For each designer, extract total number of items, counts of items per category, mean price per category
for (i in 1:length(designers)){
  d <- data %>% filter(str_detect(data$designer, designers[i])) 
  df$total_objects[i] <- nrow(d)
  df$n_categories[i] <- d %>% group_by(category) %>% summarise(count = n()) %>% nrow()
  df$items[[i]] <- d %>% mutate(sale = ifelse(old_price == "No old price", 0, 1)) %>% 
    summarise(sale = sum(sale)/nrow(d), m_price = mean(price))
}

# unnest items
df <- df %>% unnest(items)

#define color palette
pal <- pal_npg(palette = "nrc")(4)

# top designers by total number of furniture designed
p1 <- df %>% arrange(desc(total_objects)) %>% slice(1:10) %>% 
  ggplot(aes(x = reorder(designers, total_objects), y = total_objects)) + 
  geom_bar(stat = 'identity', fill = pal[1]) + coord_flip() + theme_bw() +
  theme(axis.title.y = element_blank()) +
  labs(x = "Designers", y = "Total number of furniture produced", 
       subtitle = "By total furniture designed") 

# top designers to category of furniture
p2 <- df %>% arrange(desc(n_categories)) %>% slice(1:10) %>% 
  ggplot(aes(x = reorder(designers, n_categories), y = n_categories)) + 
  geom_bar(stat = 'identity', fill = pal[2]) + coord_flip() + theme_bw() + 
  theme(axis.title.y = element_blank()) +
  labs(x = "Designers", y = "Total number of categories of furniture produced", 
       subtitle = "By number of categories of furniture") 

# top designers by price
p3 <- df %>% arrange(desc(m_price)) %>% slice(1:10) %>% 
  ggplot(aes(x = reorder(designers, m_price), y = m_price)) + 
  geom_bar(stat = 'identity', fill = pal[3]) + coord_flip() + theme_bw() +
  theme(axis.title.y = element_blank()) +
  labs(x = "Designers", y = "Mean Price (in SR)", 
       subtitle = "By average furniture price") 

# top designers by sale proportion
p4 <- df %>% arrange(desc(sale)) %>% slice(1:10) %>% 
  ggplot(aes(x = reorder(designers, sale), y = sale)) + 
  geom_bar(stat = 'identity', fill = pal[4]) + coord_flip() + theme_bw() + 
  theme(axis.title.y = element_blank()) +
  labs(x = "Designers", y = "Proportion of designed items on sale", 
       subtitle = "By proportion on sale") 

complete_plot <- p1 + p2 + p3 + p4
complete_plot <- complete_plot + plot_annotation(title = "Top 10 designers")

ggsave("figures/ikea_11032020.png", plot = complete_plot, dpi = 300, width = 13, height = 10)
