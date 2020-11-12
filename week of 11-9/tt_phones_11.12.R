##################################
# Tidy Tuesday week of 11/10
# Cody R Tuttle
# Historical Phones Data
##################################

# load pacakges and set options

#install.packages('tidytext')

library(tidyverse)
library(cowplot)
library(scales)
library(janitor)
library(tidytext)
library(mmbtools)

mmb_load_fonts()

options(scipen = 999)

# load data

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

# join data on country/year and check for inconsistencies

phones <- mobile %>% 
  inner_join(landline %>% 
               select(-total_pop), by = c("entity", "year"))

# get rid of duplicate columns (keep x columns)
phones <- phones %>% 
  rename_at(
    vars(ends_with(".x")),
    ~str_replace(., "\\..$","")
  ) %>% 
  select_at(
    vars(-ends_with(".y"))
  )

# phones %>% 
#   group_by(year) %>% 
#   summarize(mobile_subs = mean(mobile_subs, na.rm = T), 
#             landline_subs = mean(landline_subs, na.rm = T)) %>% 
#   pivot_longer(cols = c(mobile_subs, landline_subs), names_to = "mobile_landline", values_to = "subs_per_100") %>% 
#   ggplot(aes(x = year, y = subs_per_100, group = mobile_landline, color = mobile_landline)) +
#   geom_line()

plot_data <- phones %>% 
  mutate(mobile_diff = mobile_subs - landline_subs) %>% 
  filter(entity %in% c('Germany', 'France', 'Italy', 'Spain', 'Japan', 'Singapore', 'United Kingdom', 
                       'Iceland', 'Norway', 'United States'))

cols <- c("negative" = mncolors["red"], "positive" = mncolors["minnesota_blue"])

plot <- plot_data %>% 
  filter(year %in% c(1990, 1995, 2000, 2005, 2010, 2015)) %>% 
  mutate(entity = reorder_within(entity, mobile_diff, year)) %>% 
  ggplot(aes(x = entity, y = mobile_diff, fill = ifelse(mobile_diff < 0, "negative", "positive"))) +
  geom_hline(yintercept = 0) +
  geom_col(show.legend = F) +
  scale_x_reordered() +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  coord_flip() +
  theme_mmb_basic(axis.title.x = element_blank(), 
                  axis.title.y = element_blank()) +
  facet_wrap(~year, scales = "free_y", nrow = 2)
  
title <- ggdraw() +
  draw_label(
    "Race to mobile freedom", 
    fontface = "bold", 
    size = 15,
    x = 0, 
    hjust = -2.5
  ) + 
  theme(plot.background = element_rect(fill = "#f5f5f2"))

subtitle <- ggdraw() +
  draw_label(
    "This chart shows the difference between mobile phone and landline subscriptions per capita for 10 similarly situated
    countries across the years 1990, 1995, 2000, 2005, 2010, and 2015. Red bars means that a country has more landline
    subscriptions in a given year, and blue bars means that a country has more mobile. By 2005, none of the countries 
    in the sample had more landlines than mobile phones per capita. Italy, Singapore, and Norway consistenty have the 
    highest differentials out of all the countries, and the US improves its standing readily over the years.", 
    size = 12, 
    x = 0, 
    hjust = -0.4
  ) + 
  theme(plot.background = element_rect(fill = "#f5f5f2"))

plot_full <- plot_grid(title +
            theme(plot.background = element_rect(color = "#f5f5f2", fill = "#f5f5f2")), 
          subtitle +
            theme(plot.background = element_rect(color = "#f5f5f2", fill = "#f5f5f2")), 
          plot, 
          ncol = 1, rel_heights = c(0.1, 0.2, 1))

save_plot("H:\\Tidy Tuesday\\TidyTuesday\\week of 11-9\\tt_phones_11.12.png", plot_full, base_height = 8.5, base_width = 15)
