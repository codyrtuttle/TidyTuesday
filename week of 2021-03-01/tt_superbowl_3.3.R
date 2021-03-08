###########################################
# Tidy Tuesday week of 3/1
# Cody R Tuttle
# Super Bowl Ads 
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

youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

ads <- youtube %>% 
  group_by(year) %>% 
  summarize_at(vars(funny:use_sex), sum, na.rm = T) %>% 
  rename(Funny = funny,
         Celebrity = celebrity, 
         Danger = danger, 
         Animals = animals,
         Patriotic = patriotic,
         `Uses Sex` = use_sex) %>% 
  pivot_longer(cols = Funny:`Uses Sex`, names_to = "category", values_to = "ads")

ads %>% 
  filter(category != "show_product_quickly") %>% 
  ggplot(aes(x = year, y = ads)) +
  geom_line() +
  facet_wrap(~category) +
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, by=3))

brands <- youtube %>% 
  group_by(brand) %>% 
  mutate(n = n())
  summarize_at(vars(funny:use_sex), sum, na.rm = T) %>% 
  rename(Funny = funny,
         Celebrity = celebrity, 
         Danger = danger, 
         Animals = animals,
         Patriotic = patriotic,
         `Uses Sex` = use_sex) %>% 
  pivot_longer(cols = Funny:`Uses Sex`, names_to = "category", values_to = "ads")

brands %>% 
  filter(category != "show_product_quickly") %>% 
  ggplot(aes(x = ads, y = fct_reorder(brand, ads))) +
  geom_col() +
  facet_wrap(~category) 

brands_2 <- youtube %>% 
  group_by(brand) %>% 
  summarize(
    n = n(), 
    Funny = sum(funny, na.rm = T), 
    Celebrity = sum(celebrity, na.rm = T), 
    Danger = sum(danger, na.rm = T), 
    Animals = sum(animals, na.rm = T), 
    Patriotic = sum(patriotic, na.rm = T), 
    `Uses Sex` = sum(use_sex, na.rm = T)
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = Funny:`Uses Sex`, names_to = "category", values_to = "ads") %>% 
  mutate(percent = ads/n)

plot <- brands_2 %>% 
  ggplot() +
  geom_col(aes(x = percent, y = reorder_within(category, percent, brand), fill = category)) +
  facet_wrap(~brand, scales = "free_y", nrow = 2) +
  scale_y_reordered() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 5L)) +
  scale_fill_mncol(discrete = T) +
  xlab(" ") +
  ylab(" ") +
  labs(
    title = "Profiles of Top Brands' Super Bowl Ads, 2000 - 2020", 
    subtitle = "Share of brands' ads that fall into each category - ads can fall into more than one category", 
    caption = "Data: fivethirtyeight/TidyTuesday \n \nVisualization: Cody R Tuttle"
  ) +
  theme_mmb_basic() +
  theme(
    legend.position = "none",
    plot.margin = margin(1,1,1,1,"cm"),
    axis.text.x = element_text(size = 9),
    plot.subtitle = element_markdown(size = 12), 
    plot.caption = element_markdown(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot", 
    plot.caption.position = "plot"
  )

ggsave("H:\\Tidy Tuesday\\TidyTuesday\\week of 2021-03-01\\tt_superbowl_plot.png", plot, height = 6, width = 11, units = "in", dpi = 200)   
  
