library(ggstream)
library(tidyverse)
library(tidytuesdayR)
library(ggpomological)
library(thematic)
library(showtext)
font_add_google("Oswald", "oswald")
font_add_google("Roboto", "roboto")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2021, week = 16)
post_offices <- tuesdata$post_offices
# get converted state populations
pop <- read_csv(file = "data/processed_pop.csv")
pop <- pop %>% rename("state_name" = "Year") %>% mutate(state_name = str_remove(state_name, " \\(People\\)")) %>% 
  pivot_longer(cols = !starts_with("state"), names_to = "year", values_to = "pop") %>% 
  mutate(year = as.numeric(year))

# grabbing names and abbreviations 
states <- bind_cols(state_abb = c(state.abb, "DC"), state_name = c(state.name, "District of Columbia"), 
                    region = c(state.region %>% as.vector(), "South Atlantic"), 
                    division = c(state.division %>% as.vector(), "South Atlantic"))
pop <- inner_join(pop, states) %>% filter(year <= 2000)


# we are also keeping only offices where we know the start date 
# pretend that all post offices with NA discontinued are still operating, and we set the 
# dummy discontinued date to be Inf (for the sake of comparisons).
post_offices <- post_offices %>% filter(!is.na(established)) %>% 
  mutate(discontinued = replace_na(discontinued, Inf)) 

# For each year and state, retreive the number of post offices in operation 
# We're ignoring the continuous variable (some post offices do not operate continuously)
retrieve_num_offices <- function(state, year){
  post_offices %>% filter(state == state) %>% 
    filter(established <= year & discontinued >= year) %>% 
    count() %>% pull(n)
}


# rowwise operation creating a new column called num_off 
pop <- pop %>% rowwise() %>% mutate(num_off = retrieve_num_offices(state_abb, year))
pop <- pop %>% mutate(per_capita = num_off/pop)

plot_pop <- pop %>% group_by(year, division) %>% summarise(pop = sum(pop, na.rm = TRUE), num_off = sum(num_off, na.rm = TRUE)) %>% 
  mutate(per_capita = pop/num_off)

thematic_on(bg = "#5d5d5d", fg = "white", accent = "purple", font = "Roboto")

plt <- ggplot(plot_pop, aes(x = year, y = per_capita, fill = division)) +
  geom_stream(col = "white", type = "ridge", alpha = 0.8, n_grid = 500) + 
  scale_fill_pomological() + 
  labs(x = "Year", y = "Number of people per post office", fill = "Division",
       title = "Number of people per post office by region (1900-2000)", 
       subtitle = "Population data from US Census collated by USA Facts. 
Higher number indicates that a post office is responsible for serving more people, suggesting less dense post office distribution.
The general trend is that the density of post offices decreases over time despite increases in population.",
       caption = "Source: Blevins, Cameron; Helbock, Richard W., 2021, 'US Post Offices'\nhttps://doi.org/10.7910/DVN/NUKCNA, Harvard Dataverse, V1, UNF:6:8ROmiI5/4qA8jHrt62PpyA==[fileUNF]\nVisual by Quang Nguyen") + 
  scale_x_continuous(breaks = c(1900,1950,2000)) + 
  theme(plot.title = element_text(family = "oswald", size = 40, face = "bold"),
        plot.subtitle = element_text(family = "oswald", size = 18, face = "italic"),
        legend.title = element_text(family = "oswald", size = 25, face = "bold"),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "white", size = 0.5),
        axis.line.y = element_line(color = "white", size = 0.5),
        panel.background = element_rect(fill = "#5d5d5d"), plot.caption.position = "panel",
        text = element_text(size = 16, hjust = 0, lineheight = 0.3, face = "plain"),
        axis.title = element_text(hjust = 0.5), 
        legend.key.size = unit(0.2, "inches")) 

ggsave(plot = plt, filename = "figures/post_office_04132021.png", width = 6, height = 4, dpi = 300)
