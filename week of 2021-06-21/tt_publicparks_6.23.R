###########################################
# Tidy Tuesday week of 6/21
# Cody R Tuttle
# Public Parks in US Cities 
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

mmb_load_fonts()

parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')



top5_2020 <- parks %>% 
  filter(year == 2020, rank < 6)

points <- top5_2020 %>% 
  select_at(vars(city, rank, matches('points'))) %>% 
  mutate(city_rank = glue("{rank} - {city}")) %>% 
  select(city_rank, rank,
        'Amenities' = amenities_points, 
        'Median Park Size' = med_park_size_points, 
        'City Parkland %' = park_pct_city_points, 
        '% Near Park' =  pct_near_park_points, 
        'Spending Per Resident' = spend_per_resident_points) %>% 
  pivot_longer(cols = `Amenities`:`Spending Per Resident`, 
               names_to = "category", values_to = "points") %>% 
  arrange(rank)

plot <- ggplot(points, aes(x = points, y = reorder(city_rank, desc(city_rank)), fill = category)) +
  geom_col(position = "stack") +
  labs(x = " ", y = " ", fill = " ",
       title = "Top 5 Cities for Public Parks, 2020", 
       subtitle = "Point breakdown across five categories for top cities. If DC had slightly bigger parks, they likely would have beat Minneapolis for the top spot.", 
       caption = "Data: Trust for Public Land/TidyTuesday \n \nVisualization: Cody R Tuttle") +
  scale_fill_mncol(discrete = T) +
  theme_mmb_basic() +
  theme(
    plot.margin = margin(1,1,1,1,"cm"),
    axis.text.x = element_text(size = 9),
    plot.subtitle = element_markdown(size = 12), 
    plot.caption = element_markdown(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot", 
    plot.caption.position = "plot"
  )

ggsave("H:\\TidyTuesday - updated\\week of 2021-06-21\\tt_publicparks_plot.png", plot, height = 6, width = 11, units = "in", dpi = 200)   

