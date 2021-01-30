library(tidyverse)
library(tidytuesdayR)
library(countrycode)
library(ggsci)
library(showtext)

font_add_google("Oswald", "oswald")
font_add_google("Roboto", "roboto")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2021, week = 5)

plastics <- tuesdata$plastics

# First, filter out countries that are empty and add continent 
plastics <- plastics %>% filter(country != "EMPTY") %>% 
  mutate(continent = countrycode(country, origin = "country.name", destination = "continent")) 

# Second, filter by company, and rename Nestle since it has an accent 
# Definitely missing somethings like Nestle and Nestle with the accents
# Filter to 2019 because of more complete data (no collection during pandemic)
plastics <- plastics %>% filter(parent_company %in% c("PepsiCo", "The Coca-Cola Company", "Nestle", "Nestlé")) %>% 
  mutate(parent_company = case_when(
    parent_company == "Nestle" ~ "Nestlé", 
    parent_company == "The Coca-Cola Company" ~ "Coca-Cola", 
    parent_company == "PepsiCo" ~ "Pepsi",
    TRUE ~ parent_company,
  )) %>% 
  filter(year == 2019)

# Create the data frame by creating counts per company per continent, and then divide that as a proportion of 
# total plastic produced by that company 
df <- plastics %>% group_by(year, parent_company, continent) %>% summarise(tot = sum(grand_total)) %>%
  ungroup() %>% group_by(parent_company) %>% mutate(tot_year = sum(tot, na.rm = T)) %>%
  ungroup() %>% mutate(prop = round(tot/tot_year,2)) %>% 
  mutate(continent = as.factor(continent), continent = fct_reorder(continent, prop, .desc = T))

# Creating a dodged barplot and then rescale it to polar 
# Subtitle was a bit confusing. geom_label was used to better label the x axis, and consequently 
# I have to remove the real x axis. 
# Saving the plot creates some weird issues with linespacing, so I manually adjusted it
plt <- ggplot(df, aes(x = parent_company, y = prop, fill = continent)) + 
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar() + scale_x_reordered() + theme_minimal() + 
  scale_fill_npg() + 
  labs(x = "Company", y = "Proportion of total plastic waste", fill = "Continent",
       title = "Distribution of found plastic waste in 2019 across continents",
       caption = "Visual by Quang Nguyen \n TidyTuesday \n Jan 2021",
       subtitle = "Coca-Cola, Pepsi, and Nestlé were named by Break Free from Plastic in 2020 as 
one of the top plastic polluters for the third year in a row. However, where exactly in the world 
can you find these plastic products? This basic visual breaks down the total number of plastic garbage 
for these companies by the continent where it was found.") + 
  geom_label(aes(parent_company, 0.6, label = parent_company), fill = "steelblue", 
             show.legend = FALSE, color = "white", family = "roboto") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        text = element_text(family = "roboto", size = 15),
        plot.title = element_text(family = "oswald", size = 25),
        plot.subtitle = element_text(size = 15, lineheight = 0.275),
        plot.caption = element_text(size = 12, lineheight = 0.275)) 
ggsave(plot = plt, filename = "figures/plastics_01262021.png", dpi = 300, width = 4, height = 4)  

