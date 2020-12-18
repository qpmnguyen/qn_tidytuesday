library(tidytuesdayR)
library(tidyverse)
library(circlize)
library(glue)
library(jpeg)
library(showtext)

font_add_google(name = "Montserrat")
tuesdat <- tidytuesdayR::tt_load(2020,week = 51)
df <- tuesdat$ninja_warrior
df <- df %>% group_by(season, obstacle_name) %>% count() %>% group_by(season) %>% mutate(prop = n / sum(n))
logo <- readJPEG("data/logo_njw.jpg")

png(filename = "figures/am_ninja_12152020.png", res = 300, width = 3, height = 3, units = "in")
par(family = "Montserrat")
circos.par(gap.degree = 2)
circos.initialize(sectors = df$season, xlim = c(0,3))
circos.track(ylim = c(0,0.15), bg.col = "steelblue", panel.fun = function(x,y){
  dat <- df %>% filter(season == get.cell.meta.data("sector.index")) %>% arrange(desc(prop)) %>% slice(1:3)
  circos.barplot(dat$prop, 1:3 - 0.5, col = c("#284C88","#FFFFFF","#D8282B"))
  circos.text(1.5, 0.2, glue("Season {i}", i = get.cell.meta.data("sector.index")))
  circos.xaxis(h = "bottom", major.at = 1:3 - 0.5, labels = dat$obstacle_name, labels.facing = "clockwise", 
               direction = "inside", labels.cex = 0.8)
})
rasterImage(logo, xleft = -0.3, xright = 0.3, ybottom = -0.3, ytop = 0.3)
title(main = "Top 3 most used obstacles")
dev.off()
circos.clear()

