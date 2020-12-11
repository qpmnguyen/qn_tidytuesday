library(tidyverse)
library(tidytuesdayR)
library(ggmap)
library(glue)
library(lubridate)
library(gganimate)
library(ggrepel)
library(ggthemes)
library(ggsci)

tuesdata <- tidytuesdayR::tt_load(2020, week = 49)
df <- tuesdata$shelters

# getting the latest year only 
# then retrieving proportion occupied
# then getting uniting all the address columns
df <- df  %>% 
  filter(day(occupancy_date) == 15) %>%
  #filter(occupancy_date > as_date("2018-12-31")) %>% 
  unite("add", c(shelter_address, shelter_city, shelter_province, shelter_postal_code), sep = ",") %>% 
  group_by(add, occupancy_date, shelter_name) %>% summarise(occupancy = sum(occupancy), 
                                                            capacity = sum(capacity)) %>% 
  mutate(full = ifelse(occupancy >= capacity, TRUE, FALSE))  %>% 
  mutate(name = ifelse(full == TRUE, shelter_name, ""))
# getting address into one external data frame so it can be added back later 
addresses <- unique(df$add)
addresses <- data.frame(address = addresses)
# mutate geocode gives longitude and latitude based on address string by google. 
addresses <- mutate_geocode(addresses, location = address)
names(addresses)[1] <- "add"
df <- left_join(df, addresses, by = "add")

# qmplot generates a map plot. geom = "blank" to have more control over color and size in geom_points 
# gg_text_repel generates text with repel for shelters at maximum capacity
plot <- qmplot(lon, lat, data = df, geom = "blank", maptype = "roadmap", source = "google", zoom = 11) + 
  geom_point(aes(x = lon, y = lat, size = capacity, color = full), alpha = 0.8) +
  scale_size(range = c(5, 20)) + 
  geom_text_repel(aes(x = lon, y = lat, label = name), col = "steelblue", size = 6) + 
  scale_color_jama(labels = c("No", "Yes")) + theme_map() +
  theme(axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.title = element_text(size = 25, face = "bold"), 
        plot.subtitle = element_text(size = 20),
        legend.title = element_text(size = 12)) + 
  labs(y = "Latitude", x = "Longitude", color = "Is the shelter full?", size = "Capacity", 
       title = "Shelters around the Toronto area (2017 - 2019) \n by overall capacity and occupied status",
       subtitle = "Date {closest_state}")

# generate transition states by occupancy date with slower easing 
# 720 by 720 canvas with 100 frames 
anim <- plot + transition_states(occupancy_date) + 
            enter_grow() + exit_shrink() + ease_aes('cubic-in-out', interval = 0.01)
animate(anim, width = 720, height = 720, units = "px", nframes = 100)
anim_save(filename="figures/shelters_12012020.gif")
