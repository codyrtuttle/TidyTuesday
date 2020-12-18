###########################################
# Tidy Tuesday week of 12/8
# Cody R Tuttle
# BBC Women Data
###########################################

# load pacakges and set options


# install.packages("ggtext")
# install.packages("rnaturalearth")

library(tidyverse)
library(cowplot)
library(scales)
library(janitor)
library(tidytext)
library(mmbtools)
library(lubridate)
library(ggtext)
library(rnaturalearth)

mmb_load_fonts()

options(scipen = 999)


# load and clean data

ninja_warrior <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')

event_seasons <- ninja_warrior %>% 
  group_by(obstacle_name, season) %>%
  count()

events <- event_seasons %>% 
  group_by(obstacle_name) %>% 
  summarize(n_seasons = n(), 
            min_season = min(season))

seasons_newevents <- events %>% 
  group_by(min_season) %>% 
  count()

seasons_totalevents <- event_seasons %>% 
  group_by(season) %>% 
  count()

p <- seasons_newevents %>% 
  mutate(season = as.factor(min_season)) %>% 
  ggplot(aes(x = min_season, y = n)) +
  geom_col(fill = "#1b478f") +
  scale_x_continuous(breaks = seq(1,10,1)) +
  xlab("Season") +
  ylab("# of New Obstacles") +
  theme_mmb_basic() +
  labs(title = "**American Ninja Warrior**",
       subtitle = "*Number of never before seen obstacles introduced in each season* \n \nSeasons 5, 6, 7, and 10 were particularly creative") +
  theme(plot.title = element_markdown(), 
        plot.subtitle = element_markdown())

ggsave("H:\\Tidy Tuesday\\TidyTuesday\\week of 12-14\\tt_ninjawarrior_plot.png", p, height = 5, width = 10, units = "in", dpi = 200)
