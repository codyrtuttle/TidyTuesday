###########################################
# Tidy Tuesday - MMB TUG Demonstration
# Cody R Tuttle
# Employment and Earnings
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

# load data

employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

# clean earnings data

earnings_clean <- earn %>% 
  filter(age %in% c("25 to 54 years")) %>% 
  group_by(sex, race, ethnic_origin, year) %>% 
  summarize_at(vars(n_persons, median_weekly_earn), mean, na.rm = T) %>% 
  ungroup() %>% 
  mutate(race = ifelse(ethnic_origin == "Hispanic or Latino", "Hispanic/Latinx", 
                       ifelse(race == "Black or African American", "Black", race))) %>% 
  select(-one_of(c('n_persons', 'ethnic_origin'))) %>% 
  filter(race != "All Races") 

# line graph of earnings over time by race/ethnicity

p1 <- earnings_clean %>% 
  filter(sex == "Both Sexes") %>% 
  ggplot(aes(x = year, y = median_weekly_earn, group = race, color = race)) +
  geom_line(size = 2) +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 1500)) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  scale_color_mncol(discrete = T) + 
  labs(x = NULL, y = NULL, 
       title = "Median Weekly Earnings by Race") +
  theme_mmb_basic() +
  theme(
    plot.title = element_text(size = 11),
    legend.position = "bottom", 
    legend.text = element_text(size = 10), 
    legend.title = element_blank()
  )

# bar chart of ratio of womens to mens earnings in 2018 by racial group

p2 <- earnings_clean %>% 
  filter(sex != "Both Sexes") %>% 
  pivot_wider(names_from = sex, values_from = median_weekly_earn) %>% 
  mutate(ratio = Women/Men) %>% 
  filter(year == 2018) %>% 
  ggplot(aes(x = ratio, y = fct_reorder(race, ratio), fill = race)) +
  geom_col(width = 0.25) +
  geom_text(aes(label = round(ratio, 2)), hjust = -0.5, size = 3, color = "#53565A") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_fill_mncol(discrete = T) +
  labs(x = NULL, y = NULL, 
       title = "Ratio of Womens to Mens Earnings, 2018", 
       caption = "Data: BLS/TidyTuesday \n \nVisualization: Cody R Tuttle") +
  theme_mmb_basic() +
  theme(
    plot.title = element_text(size = 11),
    legend.position = "none",     
    plot.caption = element_markdown(size = 10, face = "italic")
  )

plots <- plot_grid(p1, p2, ncol = 2, rel_widths = c(1, 1))

plots

title <- ggdraw() +
  draw_label(
    "Racial and Gender Inequity in Earnings", 
    fontface = "bold", 
    size = 15,
    color = "#53565A",
    x = 0, 
    hjust = 0
  ) 

subtitle <- ggdraw() +
  draw_label(
    "The plot on the left shows racial disparities in median weekly earnings from 2010 to 2020. \nThe plot on the right shows the ratio of median weekly earnings for women compared to men within each racial group in 2018.\n \nAsian workers may have the highest weekly earnings overall, \nbut they have a large gender disparity, with Asian women only making $0.80 on the dollar compared to Asian men. \nBlack workers have the second lowest weekly earnings, but the gender disparity is much narrower. \nPlease note that racial/ethnic groups are not mutually exclusive: \nThe Hispanic/Latinx group includes all racial groups and that racial groups include all ethnic origins. \n", 
    fontface =  "plain",
    size = 10,
    color = "#53565A",
    x = 0, 
    hjust = 0
  )

poster <- plot_grid(title + 
                      theme_mmb_basic() + 
                      theme(panel.border = element_blank()), 
                    subtitle + 
                      theme_mmb_basic() + 
                      theme(panel.border = element_blank()), 
                    plots, 
                    ncol = 1, rel_heights = c(.2 , .3, 2))

poster

ggsave("H:\\TidyTuesday - updated\\tug_demonstration\\tug_earnings_5.3.png", poster, 
       height = 10, width = 11, units = "in", dpi = 200)
