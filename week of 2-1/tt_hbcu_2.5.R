###########################################
# Tidy Tuesday week of 2/1
# Cody R Tuttle
# HBCUs 
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

hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')

hbcu_black <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_black.csv')

datasets <- list(hbcu_all, hbcu_black)

# function to clean both data sets

my_function <- function(x){
  pivot_longer(x, cols = `Total enrollment`:`2-year - Private`, names_to = "category", values_to = "students")
}

new_sets <- map(datasets, 
                ~my_function(.))

new_hbcu_all <- new_sets[[1]]
new_hbcu_black <- new_sets[[2]]

# additional cleaning and join data sets

new_hbcu_black <- new_hbcu_black %>% 
  rename(students_black = students)

hbcu <- 
  left_join(new_hbcu_all, new_hbcu_black, by = c("Year", "category")) %>% 
  mutate(black_share = (students_black/students)) %>% 
  filter(category %in% c("Total enrollment", "Total - Private", "Total - Public", "4-year", "2-year"))

ggplot() +
  geom_line(data = filter(hbcu, category == "Total enrollment"), 
            aes(x = Year, y = black_share, group = category, color = category), 
            size = 1.5) +
  geom_line(data = filter(hbcu, category != "Total enrollment"), 
            aes(x = Year, y = black_share, group = category, color = category), 
            size = 0.5) +
  geom_label_repel(data = filter(hbcu, Year == 2015), 
                   aes(x = Year, y = black_share, label = category, color = category)) +
  scale_color_mncol() +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  ylab("") +
  labs(
    title = "HBCU Black Enrollment", 
    subtitle = "Share of students that are Black, by HBCU type \n \n <b>Black students have made up a declining share of the student body at 2-year HBCUs, a contrasting pattern to other types", 
    caption = "Data: data.world/TidyTuesday \n \nVisualization: Cody R Tuttle"
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
