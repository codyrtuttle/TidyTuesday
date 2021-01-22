###########################################
# Tidy Tuesday week of 12/8
# Cody R Tuttle
# BBC Women Data
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

mmb_load_fonts()

# read in data

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

city <- transit_cost %>% 
  filter(!str_detect(start_year, "[:alpha:]")) %>% 
  mutate(start_year = as.numeric(start_year)) %>% 
  filter(start_year > 2010) %>% 
  group_by(city) %>% 
  summarize(total_cost_km = sum(cost_km_millions, na.rm = T)) %>% 
  arrange(desc(total_cost_km)) %>% 
  slice_head(n = 10)

plot <- city %>% 
  ggplot(aes(x = total_cost_km/10, y = fct_reorder(city, total_cost_km))) +
  geom_col(fill = "#14316e") +
  theme_mmb_basic() +
  theme(
    axis.title.y = element_blank(),
    plot.subtitle = element_markdown(size = 12), 
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot", 
    plot.caption.position = "plot"
  ) +
  xlab("Total spending per kilometer (billions of USD)") +
  labs(
    title = "Global urban investment in transit lines - Top 10 cities", 
    subtitle = "Total spending per kilometer (in billions of USD) of transit line built since 2010. \n \n <b>Two US cities appear in the top 5, while urban China dominates the rest of the list.</b> <br>", 
    caption = "Data: Transit Costs Project/TidyTuesday\nVisualization: Cody R Tuttle")

ggsave("H:\\Tidy Tuesday\\TidyTuesday\\week of 1-4\\tt_transit_plot.png", plot, height = 10, width = 8, units = "in", dpi = 200)
