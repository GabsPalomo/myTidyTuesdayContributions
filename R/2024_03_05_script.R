library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(dutchmasters) # Color palette 
library(sjPlot)
library(cowplot)

## Data 
tuesdata <- tidytuesdayR::tt_load('2024-03-05')

trashwheel <- tuesdata$trashwheel |> 
  mutate(Name = factor(Name), 
         Year = factor(Year))

# Long format to plot
trashwheel_long <- trashwheel |> 
  mutate(across(c('PlasticBottles':'SportsBalls'), 
                ~ as.vector(scale(.x)))) |> 
  pivot_longer(cols = c('PlasticBottles':'SportsBalls'), 
               names_to = 'Category', 
               values_to = 'Count') |> 
  filter(!is.na(Count)) 

trashwheel_long |> 
  group_by(Year, Category) |> 
  summarise(min = min(Count), 
            max = max(Count))

# Define our theme 
my_theme <- theme_ridges()+
  theme(legend.position = 'none', 
        plot.margin = margin(0, 0, 0, 0.5, 'cm'),
        axis.title = element_text(face = 'bold', size = 16),
        plot.title = element_text(face = 'bold', size = 20),
        strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 16, face = 'bold')) 

# Let's visualize how much of each category does every Trash Wheel process
# The x axis is centered and scaled 
ggplot(trashwheel_long)+
  ggridges::geom_density_ridges(aes(x = Count, y = Category, fill = Category)) +
  scale_fill_dutchmasters(palette = 'anatomy')+
  scale_x_continuous(limits = c(-2, 8))+
  # facet_grid(~Name)+
  xlab('Number of items (scaled)')+
  ylab('')+
  my_theme -> p1


# Let's do a model to determine if one of the categories is contributing more
# to power homes : Each ton of trash equates to on average 500 kilowatts of 
# electricity. An average household will use 30 kilowatts per day.
m1 <- glm(HomesPowered ~ scale(PlasticBottles) + scale(PlasticBags) + scale(Wrappers) + scale(SportsBalls),
          data = trashwheel)

summary(m1)

# Let's plot the results 
plot_model(m1, 
           colors = c("#48211A", "#537270"), 
           line.size = 2, 
           dot.size = 4) +
  my_theme +
  ylab('Estimates (scaled)') +
  labs(caption = " ")+
  theme(plot.title = element_blank())-> p2

plot_model(m1, 
           type = 'pred', 
           terms = c('Wrappers'), 
           colors = c("#B47562"), 
           line.size = 1.5, 
           alpha = 0.3) + 
  xlab('Number of Wrappers')+
  my_theme +
  labs(caption = "\n#TidyTuesday 2024 - week 10 | @gabbspalomo")+
  theme(plot.title = element_blank())-> p3

plot_model(m1, 
           type = 'pred', 
           terms = c('SportsBalls'), 
           colors = c("#556246"), 
           line.size = 1.5, 
           alpha = 0.3)+
  xlab('Number of Sports Balls')+
  my_theme +
  theme(plot.title = element_blank())-> p4

# Create a plot with only title and subtitle 
ggplot()+
  labs(title = "Using trash to power homes in Baltimore",
       subtitle = stringr::str_wrap("Mr. Trash Wheel is a semi-autonomous trash interceptor that is placed at 
       the end of a river, stream or other outfall. Ideally, the plastic Mr. Trash Wheel picks up gets recycled, 
but current sorting technologies are unable to separate the plastics from all the other trash. For the time being, 
the best alternative is to incinerate the trash to create electricity. Based on a GLM model, we can see that there 
is a positive association between the number of Sports Balls being incinarated and the number of homes being powered. 
Interestingly, there is a negative association between the number of wrappers and the number of homes being powered by 
incinerating trash. \n", 
                                    width = 117))+
  theme(plot.title = element_text(size = 24, face = 'bold'), 
        plot.subtitle = element_text(size = 16), 
        plot.caption = element_text(size = 10)) -> tsc

# Get title 
tt <- get_title(tsc)

# Get subtitle 
st <- get_subtitle(tsc)

# Merge all plots together
# First place title and subtitle 
tst <- cowplot::plot_grid(tt, st, nrow = 2, rel_heights = c(0.2, 0.6))

# Now place all plots together 
plots <- cowplot::plot_grid(p1, p4, p2, p3, 
                   ncol = 2)
# Now merge title and subtitle with all plots   
tst_plots <- cowplot::plot_grid(tst, plots, 
                                nrow = 2, 
                                rel_heights = c(0.3, 0.7))+
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))

tst_plots

# Export the plot 
ggsave(plot = tst_plots, 
       filename = './plots/2024_03_05_trashwheeler.jpg', 
       dpi = 500, 
       height = 10, 
       width = 12)


