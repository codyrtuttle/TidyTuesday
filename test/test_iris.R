##### testing git with new install of R/RStudio

library(tidyverse)

data(iris)

iris %>% 
  group_by(Species) %>% 
  summarise(mean = mean(Sepal.Length, na.rm = T)) %>% 
  ggplot(aes(x = fct_reorder(Species, mean), y = mean)) +
  geom_col()
