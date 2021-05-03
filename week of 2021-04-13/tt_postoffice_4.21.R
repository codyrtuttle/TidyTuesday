###########################################
# Tidy Tuesday week of 4/12
# Cody R Tuttle
# Historic US Post Offices
###########################################

# load pacakges and set options


library(tidyverse)
library(cowplot)
library(scales)
library(janitor)
library(tidytext)
library(mmbtools)
library(lubridate)
library(ggtext)
library(gghighlight)
library(ggrepel)
library(purrr)
library(leaflet)

mmb_load_fonts()

# load data

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

# explore different features

## 3205 counties represented
post_offices %>% 
  group_by(county1, state) %>% 
  count()

## 2/3 have a lat-lon match
post_offices %>% 
  group_by(gnis_match) %>% 
  count()

## 29k still in operation
post_offices %>% 
  filter(is.na(discontinued))


leaflet() %>% 
  addTiles() %>% 
  setView(lat = 37.0902, lng = 95.7129, zoom = 4) %>% 
  addMarkers(data = post_offices, lat = post_offices$latitude, lng = post_offices$longitude)
