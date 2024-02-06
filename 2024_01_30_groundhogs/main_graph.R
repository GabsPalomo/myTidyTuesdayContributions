## My contribution to Tidy Tuesday using the groundhogs dataset 
## 2024/01/30

## Packages -------------------------------------------------------------------------
library(tidyverse)
library(here)
library(fs)
library(httr2)
library(ggthemes)

## Data -------------------------------------------------------------------------
groundhogs <- read_csv(here::here('2024_01_30_groundhogs', 'data', 'groundhogs.csv'))
predictions <- read_csv(here::here('2024_01_30_groundhogs', 'data','predictions.csv'))

## Clean data --------------------------------------------------------------------
## We only want information from TRUE groundhogs and we want to organize it by year
## and the consensus of the majority of groundhogs on whether or not they saw their shadow
## for a particular year 

groundhogs_true <- groundhogs |> 
  dplyr::filter(type == 'Groundhog')

predictions_groundhogs <- dplyr::left_join(groundhogs_true, 
                                           predictions,
                                           by = join_by('id')) 

yearly_pred <- predictions_groundhogs |> 
  group_by(year, shadow) |> 
  summarise(n = n()) |>
  ungroup() |> 
  pivot_wider(names_from = shadow, 
              values_from = n) |> 
  rename('shadow_false' = 'FALSE', 
         'shadow_true' = 'TRUE') |> 
  filter_at(vars('shadow_true', 'shadow_false'),all_vars(!is.na(.))) |> 
  mutate(result = case_when(shadow_true > shadow_false ~ 'long winter', 
                            shadow_false > shadow_true ~ 'early spring', 
                            shadow_true == shadow_false ~ 'tie')) |>  
  mutate(end_point = case_when(shadow_true > shadow_false ~ (shadow_true*-1), 
                               shadow_false > shadow_true ~ shadow_false, 
                               shadow_true == shadow_false ~ NA)) |> 
  select(!'NA') |> 
  mutate_at(c('shadow_true'), ~(. * -1)) |> 
  mutate(result = factor(result, levels = c('tie', 'long winter', 'early spring')))

# My color palette   
my_colors <- c('#184e77', '#b58db6' )

# Dumbell plot 
ggplot(yearly_pred)+
  geom_segment(aes(x = shadow_true, y = year, 
                   xend = shadow_false, yend = year), 
               color = 'gray80', 
               linewidth = 2)+
  geom_point(aes(x = end_point, 
                 y = year, 
                 color = result), 
             size = 4, 
             show.legend = TRUE)+
  scale_color_manual(values = my_colors, 
                     limits = c('long winter', 'early spring'), 
                     name = '')+
  scale_y_continuous(limits = c(1955, 2025), breaks = seq(1955, 2025, by = 5))+
  scale_x_continuous(limits = c(-20, 25), breaks = seq(-20, 25, by=5))+
  labs(title = "What did the groundhog saw? ",
       subtitle = stringr::str_wrap("The plot below shows the number of predictions per year that a true groundhog
                            has seen since 1955. The color represents what the majority predicted:
                            'long winter' or 'early spring'. If there is no color then there was a tie.  
                            If a groundhog ‘sees its shadow’: six more weeks of winter, but
                            if a groundhog does not see its shadow: early spring. \n", 
                           width = 95),
       x = 'Number of predictions',
       y = NULL,
       caption = "\n#TidyTuesday 2024 | data source: https://groundhog-day.com/ | @gabbspalomo") +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'gray40')+
  geom_hline(yintercept = 2024, linetype = 'dashed', color = 'gray40')+
  theme_hc()+
  theme(legend.position = c(0.44, 1),
        legend.direction = 'horizontal',
        legend.background = element_rect(fill='transparent'),
        plot.margin = margin(0.8, 0.5, 0.5, 0.5, 'cm'), 
        plot.title = element_text(size = 20,
                                  colour = "Black",
                                  face = "bold",
                                  margin = unit(c(0, 0, 0, 0), "cm")),
        plot.title.position = "plot", 
        plot.subtitle = element_text(size = 12,
                                     lineheight = 1.1,
                                     hjust = 0,
                                     colour = "gray20",
                                     margin = unit(c(0.5, 0, 0.8, 0), "cm")), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size =14), 
        legend.text = element_text(size = 14)) -> my_plot

# Export plot 
ggsave(filename = here::here('2024_01_30_groundhogs', 'plots', 'my_plot.jpg'), 
       plot = my_plot, 
       width = 20, 
       height = 25, 
       units = 'cm', 
       dpi = 300)  
