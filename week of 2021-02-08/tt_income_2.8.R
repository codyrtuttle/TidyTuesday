###########################################
# Tidy Tuesday week of 2/8
# Cody R Tuttle
# Wealth and Income Inequality 
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

mmb_load_fonts()

# load data

lifetime_earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv')
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_time <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_time.csv')
income_limits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_limits.csv')
income_aggregate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_aggregate.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')



income_limits %>% 
  mutate(income_quintile = factor(income_quintile, 
                                  levels = c("Lowest", "Second", "Third", "Fourth", "Highest", "Top 5%"))) %>% 
  filter(race %in% c("Black Alone", "White Alone", "Hispanic")) %>% 
  filter(dollar_type == "2019 Dollars") %>% 
  ggplot(aes(x = year, y = income_dollars, group = race, color = race)) +
  geom_line(size = 1) +
  facet_wrap(~income_quintile, nrow = 1) +
  scale_y_continuous(limits = c(0, 300000), breaks = seq(0, 300000, by = 60000), label = scales::dollar) +
  scale_x_continuous(limits = c(1967, 2019), breaks = seq(1980, 2020, 20)) +
  scale_color_mncol(name = "Racial Group") +
  xlab(" ") +
  ylab(" ") +
  labs(
    title = "Income and Racial Inequality in the United States, 1967 - 2019", 
    subtitle = "Incomes grew for earners higher in the distribution substantially more and faster than earners lower in the distribution. \n \nAcross every income group, white earners make more than Black and Hispanic earners.", 
    caption = "Data: Urban Institute/TidyTuesday \n \nVisualization: Cody R Tuttle"
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

# explore other data sets

# home_owner %>% 
#   ggplot(aes(x = year, y = home_owner_pct, group = race, color = race)) +
#   geom_line() +
#   scale_y_continuous(label = scales::percent)
# 
# race_wealth %>% 
#   filter(type == "Median") %>% 
#   ggplot(aes(x = year, y = wealth_family, group = race, color = race)) +
#   geom_line() +
#   scale_y_continuous(label = scales::dollar)
# 
# lifetime_earn %>% 
#   ggplot(aes(x = race, y = lifetime_earn, group = gender, fill = gender)) +
#   geom_col(position = "dodge") +
#   scale_y_continuous(label = scales::dollar)
# 
# income_aggregate %>% 
#   filter(race == "All Races") %>% 
#   filter(income_quintile != "Top 5%") %>% 
#   mutate(income_quintile = factor(income_quintile, levels = c("Highest", "Fourth", "Third", "Second", "Lowest"))) %>% 
#   ggplot(aes(x = year, y = income_share/100, group = income_quintile, fill = income_quintile)) +
#   geom_area(position = "stack") +
#   scale_fill_mncol(breaks = c("Highest", "Fourth", "Third", "Second", "Lowest")) +
#   geom_line(data = income_aggregate %>% filter(race == "All Races" & income_quintile == "Top 5%"),
#             aes(x = year, y = income_share/100),
#             color = "black",
#             size = 1.5) +
#   scale_y_continuous(label = scales::percent) +
#   theme_mmb_basic()
# 
# income_aggregate %>% 
#   filter(race %in% c("Black Alone", "Hispanic", "White Alone")) %>% 
#   filter(income_quintile != "Top 5%") %>% 
#   mutate(income_quintile = factor(income_quintile, levels = c("Highest", "Fourth", "Third", "Second", "Lowest"))) %>% 
#   ggplot(aes(x = year, y = income_share/100, group = income_quintile, fill = income_quintile)) +
#   geom_area(position = "stack") +
#   scale_fill_mncol(breaks = c("Highest", "Fourth", "Third", "Second", "Lowest")) +
#   geom_line(data = income_aggregate %>%
#               filter(race != "All Races" & income_quintile == "Top 5%") %>%
#               filter(!str_detect(race, "Combination")),
#             aes(x = year, y = income_share/100),
#             color = "black",
#             size = 1.5) +
#   facet_wrap(~race) +
#   scale_y_continuous(label = scales::percent) +
#   theme_mmb_basic()
