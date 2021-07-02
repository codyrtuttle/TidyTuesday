###########################################
# Tidy Tuesday week of 6/28
# Cody R Tuttle
# Animal Rescues in London
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
library(openintro)

mmb_load_fonts()

animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

data(london_boroughs) 

animal_rescues$borough <- str_to_title(animal_rescues$borough)
animal_rescues$animal_group_parent <- str_to_title(animal_rescues$animal_group_parent)

animal_rescues$borough <- fct_recode(animal_rescues$borough, 
                                      "Barking & Dagenham" = "Barking And Dagenham",
                                      "Hammersmith & Fulham" = "Hammersmith And Fulham",
                                      "Kensington & Chelsea" = "Kensington And Chelsea",
                                      "Kingston" = "Kingston Upon Thames",
                                      "Richmond" = "Richmond Upon Thames")

boroughs <- animal_rescues %>% 
  filter(pump_hours_total != "NULL" | incident_notional_cost != "NULL") %>% 
  filter(cal_year == 2020) %>% 
  mutate(pump_hours_total = as.numeric(pump_hours_total), 
         incident_notional_cost = as.numeric(incident_notional_cost)) %>% 
  group_by(borough) %>% 
  summarise(
    n = n(), 
    hours = sum(pump_hours_total),
    total_spent = sum(incident_notional_cost), 
    average_spent = mean(incident_notional_cost, na.rm = T)
  ) 



### borough map of total calls (left - legend on left)

bor_an_calls <- boroughs %>% 
  left_join(london_boroughs, by = "borough") %>% 
  ggplot() +
  geom_polygon(aes(x =x, y = y, group = borough, fill = n), color = "grey") +
  scale_fill_viridis_c(option = "magma") +
  labs(title = "Total Calls", fill = "") +
  theme_mmb_map() +
  theme(
    legend.position = "left", 
    plot.title = element_text(hjust = 0.5, size = 13, color = "black"), 
    legend.text = element_text(color = "black")
    
  )
  
bor_an_calls
### borough map of total dollars (right - legend on right)

bor_an_dollars <- boroughs %>% 
  left_join(london_boroughs, by = "borough") %>% 
  ggplot() +
  geom_polygon(aes(x =x, y = y, group = borough, fill = total_spent), 
               color = "grey") +
  scale_fill_viridis_c() +
  labs(title = "Total Cost (GBP)", fill = "") +
  theme_mmb_map() +
  theme(
    legend.position = "right", 
    plot.title = element_text(hjust = 0.5, size = 13, color = "black"), 
    legend.text = element_text(color = "black")
  )

bor_an_dollars

### borough map of most common animal (bottom - legend on bottom)

bor_animals <- animal_rescues %>% 
  filter(pump_hours_total != "NULL" | incident_notional_cost != "NULL") %>% 
  filter(cal_year == 2020) %>% 
  mutate(pump_hours_total = as.numeric(pump_hours_total), 
         incident_notional_cost = as.numeric(incident_notional_cost)) %>% 
  group_by(borough, animal_group_parent) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(borough) %>% 
  filter(n == max(n)) %>% 
  left_join(london_boroughs, by = "borough") %>% 
  ggplot() +
  geom_polygon(aes(x =x, y = y, group = borough, fill = animal_group_parent), 
               color = "grey") +
  scale_fill_viridis_d(option = "cividis") +
  labs(title = "Most Common Animal", fill = "", 
       caption = "Data: london.gov/TidyTuesday \n \nVisualization: Cody R Tuttle") +
  theme_mmb_map() +
  theme(
    legend.position = "bottom", 
    plot.title = element_text(hjust = 0.5, size = 13, color = "black"), 
    legend.text = element_text(color = "black"), 
    plot.margin = unit(c(0,4.5,1,4.5), units = "cm"),
    plot.caption = element_markdown(size = 12, face = "italic", color = "black"), 
    plot.caption.position = "plot"
  )

bor_animals

### combine maps into poster

top_plots <- plot_grid(bor_an_calls, bor_an_dollars, nrow = 1)

top_plots

plot_body <- plot_grid(top_plots, bor_animals, ncol = 1, rel_heights = c(.6, 1))

plot_body

title <- ggdraw() +
  draw_label(
    "Animal Rescues in London Boroughs by the London Fire Brigade, 2020", 
    fontface = "bold", 
    size = 15,
    hjust = 0.5
  ) +
  theme(plot.margin = margin(0,0,0,7), 
        plot.background = element_rect(fill = "#f5f5f2", color = NA))

plot_full <- plot_grid(title, plot_body, ncol = 1, rel_heights = c(.1, 1))

plot_full

ggsave("H:\\TidyTuesday - updated\\week of 2021-06-28\\tt_animalrescue_plot.png", plot_full, height = 11, width = 12, units = "in", dpi = 200)
