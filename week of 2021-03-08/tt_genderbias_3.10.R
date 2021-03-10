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


raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

## change something to test github connection