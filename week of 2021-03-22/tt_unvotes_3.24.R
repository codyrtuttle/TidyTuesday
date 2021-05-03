###########################################
# Tidy Tuesday week of 3/8
# Cody R Tuttle
# Film Gender Bias 
###########################################

# load pacakges and set options


library(tidyverse)
library(cowplot)
library(scales)
library(janitor)
library(tidytext)
library(lubridate)
library(ggtext)
library(gghighlight)
library(ggrepel)
library(purrr)
library(mmbtools)
library(glue)
library(here)
library(ggiraph)

mmb_load_fonts()

options(scipen = 999)

# load data

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

# merge issues with roll calls

rc_issues <- 
  left_join(roll_calls, issues, by = "rcid")

# issues across year

rc_issues %>% 
  mutate(year = year(as_date(date))) %>% 
  group_by(year, issue) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = n)) +
  geom_line()
  facet_wrap(~issue)

# human rights votes

human_rights <- rc_issues %>% 
  filter(issue == "Human rights") %>% 
  left_join(
    unvotes %>% 
      group_by(rcid, vote) %>%
      count() %>% 
      ungroup(), 
    by = "rcid"
  )

human_rights %>% 
  mutate(year = year(date)) %>% 
  group_by(year, vote) %>% 
  summarize(n = sum(n, na.rm = T)) %>% 
  mutate(pc = n/sum(n)) %>% 
  ggplot() +
  geom_col(aes(x = year, y = n, group = vote, fill = vote), position = "stack") +
  scale_fill_mncol(discrete = T) +
  scale_x_continuous(limits = c(1945, 2020), breaks = seq(1945, 2020, 25)) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  labs(
    x = " ", 
    y = " ", 
    title = "UN Human Rights Votes Through the Years", 
    subtitle = "Issue votes aggregated by year \n \n <b>Countries started voting yes 75% of the time starting in the 1970s</b>", 
    caption = "Data: Harvard Dataverse/TidyTuesday \n \nVisualization: Cody R Tuttle"
  ) +
  theme_mmb_basic() +
  theme(
    plot.title = element_text(size = 15, face = "bold"), 
    plot.subtitle = element_markdown(size = 12), 
    plot.caption = element_markdown(size = 10, face = "italic"),
    plot.title.position = "plot", 
    plot.caption.position = "plot"
  )
