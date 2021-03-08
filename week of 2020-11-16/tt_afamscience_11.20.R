###########################################
# Tidy Tuesday week of 11/17
# Cody R Tuttle
# African American Achievements Data
###########################################

# load pacakges and set options

library(tidyverse)
library(cowplot)
library(scales)
library(janitor)
library(tidytext)
library(mmbtools)

mmb_load_fonts()

options(scipen = 999)


# load data

firsts <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

# build plot

plot <- science %>% 
  mutate(half_century = case_when(birth < 1750 ~ 1700,
                                  birth < 1800 ~ 1750,
                                  birth < 1850 ~ 1800,
                                  birth < 1900 ~ 1850,
                                  birth < 1950 ~ 1900,
                                  birth < 2000 ~ 1950), 
         year = birth - half_century, 
         half_century_fct = factor(half_century, levels = c("1950", "1900", "1850", "1800", "1750", "1700"
         ))) %>% 
  filter(!is.na(half_century_fct)) %>% 
  group_by(half_century_fct, year) %>% 
  mutate(n_births = n()) %>% 
  ggplot(aes(x = year, y = half_century_fct, fill = n_births)) +
  geom_tile(size = 0.5) +
  scale_fill_viridis_c(direction = -1) +
  theme_mmb_basic() +
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.ticks=element_line(size=0.4),
    plot.title.position = "plot",
    plot.caption.position = "plot", 
    plot.caption = element_text(size = 11, face = "italic")
  ) +
  labs(title = "Birth year of renowned Black scientists", 
       subtitle = "1914 has birthed more recognized Black scientists than any other year",
       fill = "Births per year",
       caption = "Data: Wikipedia/TidyTuesday\nVisualization: Cody R Tuttle")


plot

ggsave(plot, filename = "H:\\Tidy Tuesday\\TidyTuesday\\week of 11-16\\tt_afamsci_plot.png", height = 5, width = 8, units = "in", dpi = 200)
