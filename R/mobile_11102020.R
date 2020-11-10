library(tidyverse)
library(tidytuesdayR)
library(patchwork)
library(ggsci)
library(ggthemes)
library(gganimate)

tuesdata <- tidytuesdayR::tt_load('2020-11-10')
mobile <- tuesdata$mobile
landline <- tuesdata$landline

data <- full_join(mobile, landline)
data <- data %>% pivot_longer(cols = c(mobile_subs, landline_subs)) %>% 
  mutate(name = case_when(name == "mobile_subs" ~ "mobile", name == "landline_subs" ~ "landline")) %>% 
  mutate(value = (value/100) * total_pop) %>% 
  group_by(continent, year, name) %>% 
  summarise(pop = sum(total_pop, na.rm = T), value = sum(value, na.rm = T)) %>%
  mutate(per_capita = (value/pop) * 100)

data <- data %>% filter(year < 2015) # not a lot of data 2015 onwards
plt <- ggplot(data, aes(x = per_capita, y = continent, fill = name)) + 
  geom_bar(stat = "identity", position = "fill") + scale_fill_npg(label = c("Landline", "Mobile")) + 
  labs(x = "Proportion of subscription for each phone type (per 100 people)", 
       y = "Continent", fill = "Phone type", title = "Year: {closest_state}") + theme_bw()
plt + transition_states(year, wrap = TRUE, state_length =  5) + ease_aes('cubic-in-out')
anim_save(filename = "figures/phone_animation.gif")
