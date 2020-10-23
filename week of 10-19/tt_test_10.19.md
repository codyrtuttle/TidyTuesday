Tidy Tuesday Week of 10/20
================
Cody Tuttle
October 19, 2020

``` r
# install.packages("gganimate")
# install.packages("mapproj")
# install.packages("transformr")
# install.packages("statebins")

library(tidyverse)
library(lubridate)
library(mmbtools)
library(janitor)
library(readr)
library(readxl)
library(openxlsx)
library(writexl)
library(tidytuesdayR)
library(gt)
library(cowplot)
library(gganimate)
library(sf)
library(maps)
library(mapproj)
library(transformr)
library(statebins)

mmb_load_fonts()

options(scipen = 999)
```

``` r
beer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   medal = col_character(),
    ##   beer_name = col_character(),
    ##   brewery = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   category = col_character(),
    ##   year = col_double()
    ## )

``` r
fips <- read_excel("Q:\\Budget Services\\Results Management\\COVID\\Safe Re-opening\\Data\\state fips codes.xlsx")
```

``` r
### join states data pieces

beer_states <- beer %>% 
  group_by(state, year) %>% 
  summarize(medals_total = n()) %>% 
  mutate(medals_total = ifelse(is.na(medals_total), 0, medals_total)) %>% 
  ungroup() %>% 
  left_join(fips, by = c("state" = "state_abbr"))
```

    ## `summarise()` regrouping output by 'state' (override with `.groups` argument)

``` r
plot <- beer_states %>% 
  filter(year >= 2000) %>% 
  ggplot(aes(state = state, fill = medals_total)) +
  geom_statebins(na.rm = T) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = c(0.12, 0.8), 
        legend.justification = c(0, 0),
        legend.direction = "horizontal", 
        plot.title = element_text(size = 16, face = "bold"), 
        plot.subtitle = element_text(size = 14)) +
  scale_fill_mncol(palette = "heat", reverse = T, discrete = F) +
  labs(title = "Total Great American Beer Fest Medals by State", 
          subtitle = "{closest_state}") +
  transition_states(year, transition_length = 20, state_length = 100)
  
animate(plot, fps = 4)
```

![](tt_test_10.19_files/figure-gfm/unnamed-chunk-4-1.gif)<!-- -->

``` r
anim_save("gabf_anim.gif")
```
