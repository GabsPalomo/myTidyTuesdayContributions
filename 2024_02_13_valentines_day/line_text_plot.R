library(tidytuesdayR)
library(ggplot2)
library(geomtextpath)
library(dplyr)
library(tidyr)
library(ggsci) # Color pallettes 
library(cowplot)
library(extrafont)

## Data
tuesdata <- tidytuesdayR::tt_load('2024-02-13')

historical_spending <- tuesdata$historical_spending 
gifts_age <- tuesdata$gifts_age
gifts_gender <- tuesdata$gifts_gender

## Transform historical_spending into a long format 
historical_long <- historical_spending |> 
  mutate(Year = as.character(Year)) |> 
  select(!c(PercentCelebrating, PerPerson)) |> 
  rename('Going out' = 'EveningOut', 
         'Cards' = 'GreetingCards', 
         'Gift cards' = 'GiftCards') |> 
  pivot_longer(cols = Candy:`Gift cards`, 
               names_to = 'Category', 
               values_to = 'Amount') |> 
  mutate(Code = case_when(Category == 'Gift cards' ~ 0.4, 
                          Category == 'Candy' ~ 0.6, 
                          Category == 'Flowers' ~ 0.2, 
                          .default = 1)) |> 
  mutate(Category = factor(Category, levels = c('Candy', 'Cards', 'Clothing', 
                                                'Flowers', 'Gift cards', 'Going out', 
                                                'Jewelry')))

# Fonts 
font_import()
y
loadfonts(device = "win")

# Background color palette 
bg <- c('#000220')
bg <- c('#150d08')
love_cp <- c('Candy' = '#fc245c', 
             'Cards'='#e76161', 
             'Clothing' ='#ffaaaa', 'Flowers'='#ff5b5b', 
             'Gift cards'= '#9c1212', 
             'Going out'='#f36d5f',
             'Jewelry'='#EA949B')

ggplot(data = historical_long,
       aes(x=Year, 
           y=Amount, 
           color = Category, 
           group = Category,
           label = Category)) +
  geom_textline(aes(hjust = Code), 
                size = 6,
                linewidth = 1.5,
                fontface = 2, 
                # hjust = Code,
                text_smoothing = 0.5
                # family = 'notosans'
                )+
  scale_color_manual(values = love_cp)+
  scale_y_continuous(
    labels = scales::dollar_format(prefix="$"),
    limits = c(0, 46),
    breaks = seq(0, 46, by = 5)
    )+
  labs(x = '', 
       y = '')+
  theme_classic()+
  theme(legend.position = "none", 
        plot.margin = margin(0.5, 1, 0.5, 0.5, 'cm'), 
        panel.background = element_rect(fill = bg), 
        plot.background = element_rect(fill = bg, color = bg,linewidth = 5), 
        panel.grid.major.y = element_line(color = '#ffaaaa', linewidth = 0.1), 
        axis.text = element_text(color = '#ffaaaa', family = 'Verdana', size = 12), 
        axis.line = element_blank(), 
        axis.ticks.x = element_line(color = '#ffaaaa')) -> plot01

plot01

## Plot 2: horizontal bars ----------------------------------------------------
age_long <- gifts_age |> 
  # mutate(Year = as.character(Year)) |> 
  select(!c(SpendingCelebrating)) |> 
  rename('Going out' = 'EveningOut', 
         'Cards' = 'GreetingCards', 
         'Gift cards' = 'GiftCards') |> 
  pivot_longer(cols = Candy:`Gift cards`, 
               names_to = 'Category', 
               values_to = 'Amount')|> 
  mutate(Category = factor(Category, levels = c('Candy', 'Cards', 'Clothing', 
                                                'Flowers', 'Gift cards', 'Going out', 
                                                'Jewelry')))

ggplot(age_long)+
  geom_col(aes(y = Age, x = Amount, fill = Category), 
           # alpha = 0.8,
           width = 0.4)+
  geom_text(aes(y = Age, 
                x = Amount, 
                label = scales::dollar(Amount), 
                group = Category), 
            position = position_stack(vjust = 0.5), 
            color = bg) +
  theme(legend.position = 'bottom', 
        legend.direction = 'horizontal', 
        legend.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_manual(values = love_cp, breaks = c('Jewelry', 
                                                 'Going out', 
                                                 'Gift cards', 
                                                 'Flowers',
                                                 'Clothing',
                                                 'Cards', 
                                                 'Candy'))+
  theme_classic()+
  labs(caption = "\n#TidyTuesday 2024 - week 06 | @gabbspalomo")+
  theme(legend.position = "none", 
        legend.title = element_blank(),
        legend.background = element_rect(fill = bg),
        legend.text = element_text(color = '#ffaaaa', family = 'Verdana', size = 10),
        plot.caption = element_text(size = 12, 
                                    color = '#ffaaaa', 
                                    family = 'Verdana', 
                                    hjust = -0.3),
        plot.margin = margin(0.5, 1, 0.5, 0.5, 'cm'), 
        panel.background = element_rect(fill = bg), 
        plot.background = element_rect(fill = bg, color = bg,linewidth = 5), 
        panel.grid.major.y = element_blank(),
        # panel.grid.major.y = element_line(color = '#ffcaca', linewidth = 0.1), 
        axis.text.y = element_text(color = '#ffaaaa', family = 'Verdana', size = 12), 
        axis.text.x = element_blank(),
        axis.line = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.line.y = element_line(color = '#ffaaaa', linewidth = 0.4))+
  guides(fill=guide_legend(nrow=2))-> plot02

plot02

## Title ---------------------------------------------------------------
title <- ggdraw() + 
  draw_label("What are people buying to celebrate Valentine's Day?",
    fontfamily = 'Verdana',
    size = 30,
    fontface = 'bold',
    x = 0.1,
    hjust = 0, 
    color = '#ffaaaa') +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0), 
    panel.background = element_rect(fill = bg), 
    plot.background = element_rect(fill = bg, color = bg,linewidth = 5))

subtitle <- ggdraw() + 
  draw_label("Younger generations are spending more on Valentine's Day. 
  \nSurprisingly, the amount of money spent on Gift cards remains the same across ages.
  \nIn recent years, people seem to be going out less to celebrate but spending more on jewelery, clothing, and gift cards.",
             fontfamily = 'Verdana',
             size = 14,
             # fontface = 'bold',
             x = 0.02,
             hjust = 0, 
             color = '#ffaaaa',
             lineheight = 0.7) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0.2), 
    panel.background = element_rect(fill = bg), 
    plot.background = element_rect(fill = bg, color = bg,linewidth = 5))

## Combine plots -------------------------------------------------------
bp <- plot_grid(plot02, plot01, nrow = 1, rel_widths = c(2, 2))
final_plot <- plot_grid(title, subtitle, bp, nrow = 3, rel_heights = c(0.1, 0.1, 0.6))

ggsave(filename = here::here('2024_02_13_valentines_day', 'plots', 'final_plot.jpg'), 
       plot = final_plot, 
       scale = 2,
       width = 20, 
       height = 10, 
       units = 'cm', 
       dpi = 300) 










# line_plot <- function(data, x, y, group, font_fam){
#   ggplot(data,
#          aes({{x}}, 
#              {{y}}, 
#              color = {{group}}, 
#              group = {{group}},
#              label = {{group}})) +
#     geom_textline(size = 6,
#                   linewidth = 1.5,
#                   fontface = 2, 
#                   hjust = 0.05,
#                   text_smoothing = 0.5
#                   # family = 'notosans'
#     )+
#     scale_color_futurama()+
#     scale_y_continuous(
#       # labels = scales::dollar_format(prefix="$"), 
#                        limits = c(0, 75),
#                        breaks = seq(0, 75, by = 5)
#                        )+
#     labs(x = '', 
#          y = '')+
#     theme_classic()+
#     theme(legend.position = "none", 
#           plot.margin = margin(0.5, 1, 0.5, 0.5, 'cm'), 
#           panel.background = element_rect(fill = bg), 
#           plot.background = element_rect(fill = bg), 
#           panel.grid.major.y = element_line(color = 'gray50', linewidth = 0.1),
#           axis.title = element_text(color = 'gray80', 
#                                     face = 'bold', 
#                                     family = font_fam, 
#                                     size = 36), 
#           axis.text = element_text(color = 'gray80', 
#                                    family = font_fam, 
#                                    size = 12), 
#           axis.line = element_blank())  -> my_plot 
#   
#   return(my_plot)
# }
# 
# my_plot <- line_plot(data = historical_long, 
#                      x = Year, 
#                      y = Amount, 
#                      group = Category, 
#                      font_fam = 'Verdana')
# 
# ggsave(filename = here::here('2024_02_13_valentines_day', 'plots', 'line_text.jpg'), 
#        plot = my_plot, 
#        width = 20, 
#        height = 14, 
#        units = 'cm', 
#        dpi = 300) 
# 
# age_plot <- line_plot(data = age_long, 
#                      x = Age, 
#                      y = Amount, 
#                      group = Category, 
#                      font_fam = 'Verdana')
# 
# ggsave(filename = './2024_02_13_valentines_day/plots/age_plot.jpg', 
#        plot = age_plot, 
#        width = 18, 
#        height = 12, 
#        units = 'cm', 
#        dpi = 300)

 
