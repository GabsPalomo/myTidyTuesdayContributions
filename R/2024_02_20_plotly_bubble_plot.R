library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(packcircles) # To create the circles 
library(MoMAColors)
library(plotly)
library(ggtext)
library(ggplot2)
library(extrafont)
loadfonts(device = "win")
windowsFonts()

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

## Bubble plot with a theme that will work for ggplotly

ggplot(data = final_data) + 
  geom_polygon(
    aes(x, 
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
            size = 8, 
            family = 'Tw Cen MT', 
            fontface = 'bold',
            color = 'white') +
  coord_equal() +
  theme_void() + 
  theme(legend.position="none", 
        plot.title = element_text(family = 'Tw Cen MT', 
                                  size = 24, 
                                  face = 'bold', 
                                  color = 'gray20'), 
        plot.subtitle = element_text(family = 'Montserrat', 
                                     colour = 'gray20',
                                     size = 16, 
                                     lineheight = 0.5, 
                                     hjust = 0, 
                                     vjust = 0, 
                                     margin = margin(t=0.3, unit = 'in')), 
        plot.caption = element_text(size = 16, 
                                    color = 'gray40',
                                    margin = margin(b=0.3, unit = 'in')), 
        plot.margin = margin(t=0.5, unit = 'in')) -> my_plot

# ggsave(filename = here::here('2024_02_20_isc_grants', 'plots', 'my_plot.jpg'), 
#        plot = my_plot, 
#        scale = 0.6,
#        width = 14, 
#        height = 14, 
#        units = 'in', 
#        dpi = 300)  

t <- "ISC grants awarded by year"

subt <- "The R Consortium ISC has been awarding grants since 2016. 
The plot shows the total amount of funding awarded each year. In 2018 the
total amount awarded was whereas in recent years it has been
significantly less with only $51,015 in 2023 and in 2022."

capt <- "\n#TidyTuesday 2024 | week 8 - 2024-02-20 | @gabbspalomo"

## Interactive plot using ggplotly 
ggplotly(my_plot,
         tooltip = 'text',
         autosize = F, 
         width = 800, 
         height = 800) %>%
  layout(hoverlabel = list(font = list(size = 20)),
         title = list(text = paste0("<b style='font-size:60px';>ISC grants awarded by year</b>",
                                    "<br>",
                                    "<span style='font-size:20px;'><b>The R Consortium ISC</b> has been awarding grants since 2016.
                                  \nThe plot shows the total amount of funding awarded each year. In 2018 the
                                  \ntotal amount awarded was <b style='color:#251c4a; font-size:20px;'>$289,972</b> whereas in recent years it has been
                                  \nsignificantly less with only <b style='color:#4c9a77;font-size:20px;'>$51,015</b> in 2023 and <b style='color:#78adb7;font-size:20px;'>$111,000</b> in 2022</span>"
                                    ))) -> interactive_plot

htmlwidgets::saveWidget(as_widget(interactive_plot), 
                        "interactive_bubble2.html")


