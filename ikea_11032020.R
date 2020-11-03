library(tidyverse)
library(tidytuesdayR)

data <- tidytuesdayR::tt_load(2020, week = 45)
data <- data$ikea

# first, we remove all designers that start with a number, since that is where there is no designer and the 
# text extraction was wrong 
data <- data %>% mutate(designer = if_else(str_detect(designer, "([0-9]+).*$"), NA_character_, designer)) 

designers <- data$designer  

# Splitting up by "/" character and combine them into a list of unique designers 
designers <- str_split(designers, "/") %>% do.call(c,.) %>% unique()
