library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(packcircles) # To create the circles 
library(MoMAColors)
library(plotly)
library(ggtext)

# Download data 
tuesdata <- tidytuesdayR::tt_load('2024-02-20')
isc_grants <- tuesdata$isc_grants

# Summarize total funded by year 
isc_summary <- isc_grants |> 
  group_by(year) |> 
  summarise(total_funded = sum(funded)) |> 
  arrange(total_funded)

# Create the area of each circle (year)
packing <- circleProgressiveLayout(isc_summary$total_funded, sizetype='area')
data <- cbind(isc_summary, packing)
data <- data |> 
  rename(xcirc = x, 
         ycirc = y, 
         radius_circ = radius) |> 
  tibble::rowid_to_column('id')
# Calculate the 50 vertices points for each circle
dat.gg <- circleLayoutVertices(packing, npoints=50)
final_data <- left_join(data, dat.gg, by = join_by(id)) |> 
  mutate(total_funded = scales::dollar(total_funded, 
                                       largest_with_cents = 0))


# Bubble plot 
sysfonts::font_add_google("Montserrat", "montserrat")
sysfonts::font_add_google("Poppins", "poppins")
sysfonts::font_add_google("Josefin Sans", "josefinsans")
showtext::showtext_auto()

subt <- "<b>The R Consortium ISC</b> has been awarding grants since 2016.<br> 
The plot shows the total amount of funding awarded each year. In 2018 the<br> 
total amount awarded was <b style = 'color:#251c4a;'>$289,972</b> whereas in recent years it has been<br> 
significantly less with only <b style = 'color:#4c9a77;'>$51,015</b> in 2023 and <b style = 'color:#78adb7;'>$111,000</b> in 2022.<br>"

ggplot(data = final_data) + 
  geom_polygon(aes(x, 
                   y, 
                   group = year,
                   fill = as.factor(year), 
                   text = total_funded), 
               colour = "white", 
               # alpha = 0.7
               ) +
  scale_fill_manual(values = moma.colors("Sidhu", 10))+
  geom_text(aes(xcirc, 
                ycirc, 
                size = total_funded, 
                label = year), 
            size = 22, 
            family = 'josefinsans', 
            fontface = 'bold',
            color = 'white') +
  labs(title = "ISC grants funded by year ",
       subtitle = subt,
       x = NULL,
       y = NULL,
       caption = "\n#TidyTuesday 2024 | week 8 - 2024-02-20 | @gabbspalomo") +
  coord_equal() +
  theme_void() + 
  theme(legend.position="none", 
        plot.title = element_text(family = 'josefinsans', 
                                  size = 90, 
                                  face = 'bold', 
                                  color = 'gray20'), 
        plot.subtitle = element_markdown(family = 'montserrat', 
                                         colour = 'gray20',
                                         size = 42, 
                                         lineheight = 0.5, 
                                         hjust = 0, 
                                         vjust = 0, 
                                         margin = margin(t=0.3, unit = 'in')), 
        plot.caption = element_text(size = 38, 
                                    color = 'gray40',
                                    margin = margin(b=0.3, unit = 'in')), 
        plot.margin = margin(t=0.5, unit = 'in')) -> my_plot

ggsave(filename = here::here('2024_02_20_isc_grants', 'plots', 'my_plot.jpg'), 
       plot = my_plot, 
       scale = 0.6,
       width = 14, 
       height = 14, 
       units = 'in', 
       dpi = 300)  


## Interactive plot using ggplotly 
# ggplotly(my_plot, 
#          tooltip = 'text') %>% 
#   layout(hoverlabel = list(font = list(size = 20)))


