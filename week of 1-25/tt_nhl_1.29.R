###########################################
# Tidy Tuesday week of 1/25
# Cody R Tuttle
# NHL Goals (data for week of 3-3-2020)
###########################################

# load pacakges and set options

# install.packages("gghighlight")
# install.packages("ggrepel")

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

mmb_load_fonts()

# read in data

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

# clean data for step plot of career goals

nhl_goals_cum_2 <- season_goals %>% 
  filter(str_detect(league, "NHL")) %>% 
  group_by(player) %>% 
  mutate(seasons = n()) %>% 
  mutate(cum_goals = cumsum(goals)) %>%
  group_by(player) %>% 
  arrange(desc(cum_goals), .by_group = T) %>% 
  mutate(id = row_number()) %>% 
  ungroup() 

plot <- nhl_goals_cum_2 %>% 
  ggplot(aes(x = age, y = cum_goals, group = player, color = player)) +
  geom_step(size = 1) +
  geom_label_repel(data = nhl_goals_cum_2 %>% filter(rank <= 5) %>% group_by(player) %>% filter(id == 1), 
                   aes(label =  player)) +
  gghighlight(rank < 6, use_direct_label = F,
              unhighlighted_params = list(size = 0.5, color = alpha("grey", 0.5))) +
  scale_color_mncol(discrete = TRUE) +
  xlab("Age") +
  ylab("Cumulative Goals") +
  scale_y_continuous(limits = c(0, 1000)) +
  labs(
    title = "NHL Top 250 Goal Scorers",
    subtitle = "Cumulative career goals by player age \n \n <b>Wayne Gretzy scored 894 total goals in his career, the most in NHL history and rose to the top rapidly.",
    caption = "Data: NHL/TidyTuesday \n \nVisualization: Cody R Tuttle"
  ) +
  theme_mmb_basic() +
  theme(
    legend.position = "none",
    plot.subtitle = element_markdown(size = 12), 
    plot.caption = element_markdown(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot", 
    plot.caption.position = "plot"
  )

ggsave("H:\\Tidy Tuesday\\TidyTuesday\\week of 1-25\\tt_nhl_plot.png", plot, height = 6, width = 9, units = "in", dpi = 200)
