library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(grid)
library(rworldmap)
library(purrr)
library(maps)
library(sf) 

## Define vector with all European countries: ##
nds <- c("Norway", "Denmark", "Sweden")

## make a df with only the country to overlap
map_data_es <- map_data('world')[map_data('world')$region %in% nds,]

ggplot() +
  ## Second layer: Country map
  geom_polygon(data = map_data_es,
               aes(x=long, y=lat, group = group),
               color = 'white',
               fill = 'gray80',
               linewidth = 0.5) +
  coord_map() +
  coord_fixed(1.5,
              xlim = c(5, 30),
              ylim = c(55, 72)) +
  # theme_void()+
  theme(panel.background = element_rect(fill = 'white'), 
        panel.border = element_blank(), 
        axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank()) -> main_map

## Download data -----------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 6)
tuesdata <- tuesdata[[1]]

sites <- readxl::read_excel('./2024_02_06_heritage/data/whc-sites-2023.xls')
names(sites)
sites_nds <- sites |> 
  filter(states_name_en %in% nds)

main_map + 
  geom_point(data = sites_nds,
               aes(x=longitude, 
                   y=latitude, 
                   group = states_name_en,
                   color = states_name_en))
