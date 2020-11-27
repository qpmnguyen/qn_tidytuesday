library(tidyverse)
library(tidytuesdayR)
library(patchwork)
library(ggsci)
library(stringr)
library(ggmosaic)
library(RColorBrewer)

tuesdat <- tidytuesdayR::tt_load(2020, week=48)

data <- tuesdat$hike_data
# getting length and convert to numeric 
data$length <- data$length %>% gsub("([0-9]+).*$", "\\1", x = .) %>% as.numeric()
data <- data %>% mutate(gain = as.numeric(gain), highpoint = as.numeric(highpoint), rating = as.numeric(rating))
# unnesting features column
data <- data %>% unnest(features) 
# dichotomizing variables 
data <- data %>% mutate(rating = case_when(rating >= 4 ~ "high", 
                                           rating <= 2 ~ "low", TRUE ~ NA_character_)) %>%
  mutate(gain = case_when(gain <= quantile(gain)[2] ~ "low", 
                          gain >= quantile(gain)[3] ~ "high", TRUE ~ NA_character_)) %>% 
  mutate(length = case_when(length <= quantile(length)[2] ~ "short", 
                            length >= quantile(length)[3] ~ "long", TRUE ~ NA_character_)) %>% 
  mutate(highpoint = case_when(highpoint <= quantile(highpoint)[2] ~ "low", 
                               highpoint >= quantile(highpoint)[3] ~ "high", TRUE ~ NA_character_))

data <- data %>% filter(!is.na(rating) & !is.na(length) & !is.na(highpoint)) 

data <- data %>% mutate(length = as.factor(length), rating = as.factor(rating), highpoint = as.factor(highpoint))

mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(15)
plot <- ggplot(data) + geom_mosaic(aes(x = product(features,rating), fill = features)) + 
  scale_fill_manual(values = mycolors) + 
  theme_bw() +
  facet_grid(length ~ highpoint, labeller = label_both) + 
  labs(x = "Ratings", y = "Categories") + theme(legend.position = "none")
ggsave(plot, filename = "figures/hiking_11232020.png", dpi = 300, width = 6, height = 6)
