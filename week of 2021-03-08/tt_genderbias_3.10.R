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

mmb_load_fonts()

options(scipen = 999)


raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')


year<- movies %>%
  filter(year > 1979) %>% 
  mutate(domgross_2013 = as.numeric(ifelse(str_detect(domgross_2013, "#N/A"), 0, domgross_2013))) %>% 
  group_by(year, binary) %>% 
  summarize(n = n(), 
            total_domgross_2013 = sum(domgross_2013, na.rm = T), 
            total_budget_2013 = sum(budget_2013, na.rm = T),
            median_domgross_2013 = median(domgross_2013, na.rm = T), 
            avg_budget_2013 = mean(budget_2013, na.rm = T)) %>% 
  arrange(binary, year) %>% 
  ungroup() %>% 
  group_by(binary) %>% 
  mutate(cum_domgross = cumsum(total_domgross_2013), 
         cum_budget = cumsum(total_budget_2013))

year %>% 
  ggplot(aes(x = year, y = cum_budget, group = binary, color = binary)) +
  geom_step(size = 2) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(name = "Gender Bias Test", values = c("FAIL" = "#050505", "PASS" = "#e8ac07")) +
  xlab(" ") +
  ylab("Cumulative Budget (2013 dollars)") +
  labs(
    title = "How do IMDB films fare on the gender bias test?", 
    subtitle = "The gender bias test asks if a film (a) has at least two named women (b) who talk to each other (c) about something besides a man.\n\n Passing the test gives women a bare minimum of depth. The bar is pretty low. \n \n <b>Either due to the sheer number of movies, or to the outsized budget given them, movies that fail the gender bias test dominate the industry.</b>"
  ) + 
  theme_mmb_basic() +
  theme(
    legend.title = element_text(size = 12),
    plot.subtitle = element_markdown(size = 12), 
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot" 
  )


top_fail <- movies %>% 
  mutate(title_year = glue("{title} ({year})")) %>% 
  mutate(domgross_2013 = as.numeric(ifelse(str_detect(domgross_2013, "#N/A"), 0, domgross_2013))) %>%
  filter(binary == "FAIL") %>% 
  slice_max(n = 10, order_by = domgross_2013)

top_fail %>% 
  ggplot(aes(x = domgross_2013, y = fct_reorder(title_year, domgross_2013))) +
  geom_col(fill = "#050505") +
  scale_x_continuous(labels = scales::dollar) +
  xlab("Domestic Gross Earnings (2013 dollars)") +
  ylab(" ") +
  labs(
    title = "Top 10 Highest Grossing IMDB Films That Failed the Gender Bias Test"
  ) +
  theme_mmb_basic() +
  theme(
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

top_pass <- movies %>% 
  mutate(title_year = glue("{title} ({year})")) %>% 
  mutate(domgross_2013 = as.numeric(ifelse(str_detect(domgross_2013, "#N/A"), 0, domgross_2013))) %>%
  filter(binary == "PASS") %>% 
  slice_max(n = 10, order_by = domgross_2013)

top_pass %>% 
  ggplot(aes(x = domgross_2013, y = fct_reorder(title_year, domgross_2013))) +
  geom_col(fill = "#e8ac07") +
  scale_x_continuous(labels = scales::dollar) +
  xlab("Domestic Gross Earnings (2013 dollars)") +
  ylab(" ") +
  labs(
    title = "Top 10 Highest Grossing IMDB Films That Passed the Gender Bias Test"
  ) +
  theme_mmb_basic() +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )



### Extra code I didn't use

# year %>% 
#   ggplot(aes(x = year, y = avg_budget_2013, group = binary, color = binary)) +
#   geom_line() +
#   scale_y_continuous(labels = scales::dollar)
# 
# 
# top_all <- movies %>% 
#   mutate(title_year = glue("{title} ({year})")) %>% 
#   mutate(domgross_2013 = as.numeric(ifelse(str_detect(domgross_2013, "#N/A"), 0, domgross_2013))) %>%
#   slice_max(n = 10, order_by = domgross_2013)
# 
# top_all %>% 
#   ggplot(aes(x = domgross_2013, y = fct_reorder(title_year, domgross_2013), fill = binary)) +
#   geom_col() +
#   scale_x_continuous(labels = scales::dollar)
