###########################################
# Tidy Tuesday week of 12/1
# Cody R Tuttle
# Toronto Shelters Data
###########################################

# load pacakges and set options


# install.packages("ggtext")

library(tidyverse)
library(cowplot)
library(scales)
library(janitor)
library(tidytext)
library(mmbtools)
library(lubridate)
library(ggtext)

mmb_load_fonts()

options(scipen = 999)


# load and clean data

shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

families_month <- shelters %>% 
  filter(sector == "Families") %>% 
  mutate(date = as_date(occupancy_date), 
         month = floor_date(date, unit = "month")) %>% 
  group_by(month, shelter_name) %>% 
  summarize(occ_pc = sum(occupancy, na.rm = T)/sum(capacity, na.rm = T))

# plot data

p <- families_month %>% 
  filter(shelter_name != "YWCA - Beatrice House") %>% 
  ggplot(aes(x = month, y = shelter_name, fill = occ_pc)) +
  geom_tile() +
  scale_fill_mncol(palette = "heat", discrete = F, labels = scales::percent) +
  theme_mmb_basic() +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    plot.subtitle = element_markdown(size = 12), 
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot", 
    plot.caption.position = "plot"
  ) +
  labs(
    title = "Toronto Family Shelter Occupancy by Month, 2017-2019", 
    subtitle = "Total occupancy divided by total capacity across shelters for each month. \n \n <b>Robertson House has been consistently over-occupied since 2017, while COSTI Reception Centre has mainly been under-occupied.</b> <br>", 
    fill = "Total % Occupancy", 
    caption = "Data: OpenDataToronto/TidyTuesday\nVisualization: Cody R Tuttle")

 
ggsave("H:\\Tidy Tuesday\\TidyTuesday\\week of 11-30\\tt_shelter_plot.png", p, height = 5, width = 10, units = "in", dpi = 200)  

p
