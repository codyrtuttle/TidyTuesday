###########################################
# Tidy Tuesday week of 7/19
# Cody R Tuttle
# US Drought Severity
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
library(glue)
library(lubridate)
library(maps)
library(openintro)
library(ggmap)
library(sf)
library(mapproj)

mmb_load_fonts()
options(scipen = 999)

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

states <- map_data("state")

palette <- c("Exceptional drought" = "#830000", 
             "Extreme drought" = "#a14323", 
             "Severe drought" = "#bd7247", 
             "Moderate drought" = "#d7a170", 
             "Abnormally dry" = "#edd09e", 
             "None" = "black")

plot <- drought %>% 
  mutate(year = lubridate::year(valid_start), 
         drought_lvl = as_factor(recode(drought_lvl, "D0" = "Abnormally dry", 
                              "D1" = "Moderate drought", 
                              "D2" = "Severe drought", 
                              "D3" = "Extreme drought",
                              "D4" = "Exceptional drought")),  
         drought_lvl = fct_relevel(drought_lvl, 
                                   levels = c("Exceptional drought", 
                                              "Extreme drought", 
                                              "Severe drought", 
                                              "Moderate drought", 
                                              "Abnormally dry", 
                                              "None"))) %>%
  filter(year > 2009) %>% 
  group_by(year, state_abb, drought_lvl) %>% 
  summarise(avg_pop = mean(pop_total, na.rm = T)) %>%
  ungroup() %>% 
  group_by(year, drought_lvl) %>% 
  summarise(total_pop = sum(avg_pop, na.rm = T)) %>% 
  filter(drought_lvl != "None") %>% 
  ggplot(aes(x = year, y = total_pop)) +
  geom_col(aes(fill = drought_lvl)) +
  scale_fill_manual(values = palette)
  


map <- drought %>% 
  mutate(year = lubridate::year(valid_start), 
         drought_lvl = as_factor(recode(drought_lvl, "D0" = "Abnormally dry", 
                                        "D1" = "Moderate drought", 
                                        "D2" = "Severe drought", 
                                        "D3" = "Extreme drought",
                                        "D4" = "Exceptional drought")),  
         drought_lvl = fct_relevel(drought_lvl, 
                                   levels = c("Exceptional drought", 
                                              "Extreme drought", 
                                              "Severe drought", 
                                              "Moderate drought", 
                                              "Abnormally dry", 
                                              "None")), 
         state = abbr2state(state_abb), 
         region = tolower(state)) %>%
  filter(year > 2009) %>% 
  group_by(region, year, drought_lvl) %>% 
  summarise(avg_pop = mean(pop_total, na.rm = T)) %>%
  ungroup() %>% 
  group_by(region, drought_lvl) %>% 
  summarise(avg_pop = mean(avg_pop, na.rm = T)) %>% 
  ungroup() %>%
  group_by(region) %>% 
  filter(avg_pop == max(avg_pop)) %>% 
  right_join(states) %>% 
  ggplot(aes(long, lat, group = group, fill = drought_lvl)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat45 = 45) +
  scale_fill_manual(values = palette)+
  theme_mmb_map() 

poster <- plot_grid(map, plot, ncol = 1)  

poster  

  



