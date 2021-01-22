###########################################
# Tidy Tuesday week of 1/18
# Cody R Tuttle
# Kenya Census
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

# read in data

gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv')
crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')
households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')

hh_clean <- households %>% 
  clean_names() %>% 
  filter(!str_detect(county, "Kenya")) # remove national avg row

plot <- hh_clean %>% 
  ggplot(aes(x = number_of_households/1000, y = average_household_size)) +
  geom_point(color = "#14316e") +
  geom_text(data = hh_clean %>% 
               filter(number_of_households == max(number_of_households) | 
                            average_household_size == max(average_household_size)),
             aes(label = county), 
             nudge_x = -1,
             nudge_y = 0.25) +
  xlab("Households (in thousands)") +
  ylab("Avg. Household Size") +
  theme_mmb_basic() +
  labs(title = "Population and household size in Kenya", 
       subtitle = "Each dot represents one Kenyan county. On average, more populous counties have smaller average household sizes. \n \n <b>Mandera county has one of the smallest populations, but the highest average household size. </b>\n \n <b>Nairobi, the capital city, is Kenya's most populated area and has the lowest average household size.</b> <br>", 
    caption = "Data: rKenyaCensus/TidyTuesday\nVisualization: Cody R Tuttle") +
  theme(plot.subtitle = element_markdown(size = 12), 
        plot.caption = element_markdown(size = 10, face = "italic"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot", 
        plot.caption.position = "plot")

ggsave("H:\\Tidy Tuesday\\TidyTuesday\\week of 1-18\\tt_kenya_plot.png", plot, height = 8, width = 11, units = "in", dpi = 200)
  