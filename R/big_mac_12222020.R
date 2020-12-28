library(tidytuesdayR)
library(tidyverse)
library(glue)
library(lubridate)
library(wbstats)
library(ggrepel)
library(ggthemes)
library(extrafont)
library(ggrepel)
library(patchwork)

# importing fonts using extrafont
font_import()
loadfonts(device = "win")

# loading data 
tuesdat <- tidytuesdayR::tt_load(2020,week = 52)
df <- tuesdat$`big-mac`

# Retrieve external data from the world bank. The index is SI.DST.10TH.10 or the percentage of wealth owned by the top 10%
external <- wb_data(indicator = c("SI.DST.10TH.10"), start_date = "2000", end_date = "2020")

# Renaming 
external <- external %>% rename("ind" = "SI.DST.10TH.10", "year" = "date") 

# Extract year for matching
df <- df %>% mutate(year = year(date)) %>% rename(c("iso3c" = "iso_a3", "country" = "name")) 

# Left join and remove nas as well as unncessary columns
df <- left_join(df, external, by = c("year", "iso3c", "country")) %>% 
  select(-c(gdp_dollar, adj_price, eur_raw, gbp_raw, jpy_raw, cny_raw, eur_adjusted, 
            gbp_adjusted, jpy_adjusted, cny_adjusted, iso2c,unit, obs_status, footnote, last_updated)) %>% 
  filter(!is.na(ind)) 

# Only do 2011 and forward which has both adjusted and unadjusted 
df <- df %>% filter(year >= 2011) 


# according to gapminder, the median life expectancy for all countries from 2000 - 2007 is about 71.2
df <- df %>% group_by(year, country) %>% summarise(usd_raw = mean(usd_raw), 
                                                  usd_adj = mean(usd_adjusted), 
                                                  ind = mean(ind))
df <- df %>% pivot_longer(c(usd_raw, usd_adj), names_to = "type", values_to = "index") %>% 
  mutate(status = ifelse(index >= 0, "Over-valuated", "Under-valuated"))
df <- df %>% filter(!is.na(status)) %>% mutate(type = ifelse(type == "usd_adj", "GDP Adjusted", "Raw"))

plt <- ggplot(df , aes(x = index, y = ind)) + 
  #geom_jitter(alpha = 0.6, aes(col = status), show.legend = FALSE) + 
  #geom_boxplot(alpha = 0.4, aes(fill = status)) + 
  geom_point(aes(col = status)) + stat_smooth(aes(col = status)) + 
  facet_grid(type~year)  + 
  theme_bw() + scale_color_manual(values = c("#e3120b", "#4a4a4a")) + 
  scale_fill_nejm() + theme_clean() + 
  labs(y = "Percentage of wealth in top 10%", x = "Big-Mac Index Evaluation", fill = "Relative evaluation \n compared to USD") + 
  theme(text = element_text(family = "Lato"), 
        axis.text.x = element_blank(), axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), legend.position = "bottom", 
        strip.text.y = element_text(size = 12), legend.text = element_text(family = "Lato")) 

plt
ggsave(plt, filename = "figures/big_mac_12222020.png", dpi = 300, width = 9, height = 8, device = "png")
