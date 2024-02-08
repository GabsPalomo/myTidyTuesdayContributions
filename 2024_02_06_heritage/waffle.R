# Tidy Tuesday 2024-02-06 Scandinavian heritages 
# My goal here was to learn how to create a waffle plot

# packages 
library(waffle)
library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(ggplot2)

# Data 
tuesdata <- tidytuesdayR::tt_load(2024, week = 6) 

# long format so we can facet it by year 
tuesdata <- tuesdata[[1]] |> 
  pivot_longer(cols = '2004':'2022',
               names_to = 'year', 
               values_to = 'n')

## Fonts 
sysfonts::font_add_google("Poppins", "poppins")
showtext::showtext_auto()

## Flags color palette 
flag_cp <- c('#C8102E', '#00205B', '#006AA7')

cp <- c('#5f0f40', '#e36414', '#0f4c5c')

# using the package waffle, let's create a waffle plot 
# faceted by year 
ggplot(tuesdata, 
       aes(fill = country, 
           values = n))+
  coord_equal() +
  theme_void()+
  geom_waffle(color = "black", 
              alpha = 0.8,
              size = 1, 
              n_rows = 5,
              # make_proportional = TRUE, 
              radius = unit(4, "pt"), 
              height = 0.8, 
              width = 0.8) +
  facet_wrap(~year) +
  scale_fill_manual(values = flag_cp)+
  labs(title = "Scandinavian national heritage sites",
       subtitle = stringr::str_wrap("According to UNESCO, a world heritage site is a landmark
                            with legal protection. Designated by UNESCO for having cultural, 
                            historical, scientific, or other forms of significance and are 
                            considered outstanding assets to humanity. Sweden is the country with more national heritage sites overall with 
                            a total of 15 recognized sites by 2022. However, Denmark added 6 new sites from 
                            2004 to 2022. \n", 
                                    width = 110),
       caption = "\n#TidyTuesday 2024 - week 06 | @gabbspalomo") +
  theme(strip.text.x = element_text(hjust = 0.5), 
        plot.margin = margin(0.5, 0, 0.5, 0, 'cm'),
        plot.title = element_text(size = 54, color = "Black", family = 'poppins', face = "bold",
                                  margin = unit(c(0, 0, 0, 0), "cm")),
        plot.title.position = "plot", 
        plot.subtitle = element_text(size = 32, lineheight = 0.5, hjust = 0, colour = "gray20",
                                     margin = unit(c(0.5, 0, 0.8, 0), "cm")), 
        plot.caption = element_text(size = 28, color = 'gray30', family = 'poppins', hjust = 0),
        plot.caption.position = "plot",
        legend.position = 'bottom', 
        legend.title = element_blank(), 
        legend.text = element_text(size = 30, family = 'poppins'),
        legend.box.margin=margin(1,0,0,0, 'cm'),
        strip.text = element_text(size = 48, face = 'bold', family = 'poppins')) -> waffle_plot

waffle_plot

# Export plot 
ggsave(filename = here::here('2024_02_06_heritage', 'plots', 'waffle_plot.jpg'), 
       plot = waffle_plot, 
       scale = 1.5, 
       width = 15, 
       height = 10, 
       units = 'cm', 
       dpi = 300)  

showtext::showtext_end()  
  