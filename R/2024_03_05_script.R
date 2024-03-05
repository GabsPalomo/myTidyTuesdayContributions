library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(ggplot2)

## Data 
tuesdata <- tidytuesdayR::tt_load('2024-03-05')

trashwheel <- tuesdata$trashwheel
