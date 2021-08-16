###########################################
# Tidy Tuesday week of 7/26
# Cody R Tuttle
# Olympic Medals
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
library(gghighlight)
library(ggrepel)

mmb_load_fonts()

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

track <- olympics %>% 
  mutate(team = ifelse(str_detect(team, "Russia|Soviet Union"), 
                       "Russia/Soviet Union", team)) %>% 
  filter(sport == "Athletics" & 
         (str_detect(event, "[[:digit:]]") | str_detect(event, "Marathon"))) %>% 
  group_by(team, year, medal) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(team, year), names_from = medal, values_from = n) %>% 
  group_by(team) %>% 
  mutate(bronze = cumsum(replace_na(Bronze, 0)), 
         silver = cumsum(replace_na(Silver, 0)), 
         gold = cumsum(replace_na(Gold, 0)), 
         medals = bronze+silver+gold, 
         max_medals = max(medals), 
         max = max_medals == medals, 
         label1 = glue("{team} - {max_medals}  medals"), 
         label2 = ifelse(team == "United States", glue("Gold - {label1}"), 
                        ifelse(team == "Great Britain", glue("Silver - {label1}"), 
                               ifelse(team == "Russia/Soviet Union", glue("Bronze - {label1}"), label1)))) %>%
  arrange(desc(max), team) %>% 
  filter(max_medals > 10) %>% 
  ungroup() %>% 
  ggplot(aes(x =  year, y = medals, group = team, color = team)) +
  geom_line(size = 1.5) +
  gghighlight(max(medals) > 180,
              unhighlighted_params = list(size = 0.5, color = alpha("black", 0.5)), 
              label_key = label2) +
  scale_color_manual(values = c("United States" = "#c9b037", 
                                "Great Britain" = "#b4b4b4", 
                                "Russia/Soviet Union" = "#6a3805")) +
  scale_x_continuous(breaks = seq(1896, 2016, by = 20)) +
  labs(
    x = "", 
    y = "", 
    title = "Olympic track and marathon meta-medals", 
    subtitle = "Total medals in track and marathon events, 1896 - 2016",
    caption = "Data: Kaggle/TidyTuesday \n \nVisualization: Cody R Tuttle"
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

track

ggsave("H:\\TidyTuesday - updated\\week of 2021-07-26\\tt_olympic_plot.png", track, 
       height = 9, width = 9, units = "in", dpi = 200)

