##################################
### Tidy Tuesday Week of 11/3
### Cody R Tuttle
### IKEA Data
##################################

### install and load packaages

install.packages("cowplot")
install.packages("")

library(tidyverse)
library(cowplot)
library(scales)

### load data

ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

### explore dimensions of interest (i.e, number of categories, designers, sellable online, etc.)


ikea %>% 
  group_by(category) %>% 
  count()  # 17 categories, can work with

ikea %>% 
  group_by(designer) %>% 
  count() %>% 
  arrange(desc(n)) # 381 categories - going to filter down and work with top 10

ikea %>% 
  group_by(sellable_online) %>% 
  count() # only 28 aren't sellable online, so not much of a look?



### filter designers to values that don't contain "IKEA" to get "independent" designers

designers <- ikea %>% 
  filter(!grepl('IKEA', designer)) %>% 
  group_by(designer) %>% 
  summarize(items = n(), 
            avg_price = mean(price, na.rm = T)) %>% 
  ungroup() 

designer_plot <- designers %>% 
  arrange(desc(items)) %>% 
  head(10) %>% 
  ggplot(aes(x = fct_reorder(designer, items), y = items)) +
  geom_col(width = 0.2, fill = "yellow") +
  geom_point(size = 5, color = "navy") +
  ggtitle("Top 10 Designers by Items") +
  ylab("Number of items designed") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0), 
    plot.title.position = "plot",
    plot.background = element_rect(fill = "gray", color = "gray"),
    panel.background = element_rect(fill = "gray", color = "gray"),
    axis.text.y = element_text(color = "black"), 
    axis.text.x = element_text(color = "black"), 
    axis.title.y = element_blank()
  ) +
  coord_flip()

### by categories

category <- ikea %>% 
  group_by(category) %>% 
  summarize(items = n(), 
            avg_price = mean(price, na.rm = T)) %>% 
  ungroup() 

cat_plot1 <- category %>% 
  arrange(desc(items)) %>% 
  head(10) %>% 
  ggplot(aes(x = fct_reorder(category, items), y = items)) +
  geom_col(width = 0.2, fill = "yellow") +
  geom_point(size = 5, color = "navy") +
  ggtitle("10 Biggest Categories by Number of Items") +
  ylab("Number of items") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0), 
    plot.title.position = "plot",
    plot.background = element_rect(fill = "gray", color = "gray"),
    panel.background = element_rect(fill = "gray", color = "gray"),
    axis.text.y = element_text(color = "black"), 
    axis.text.x = element_text(color = "black"), 
    axis.title.y = element_blank()
  ) +
  coord_flip()

cat_plot2 <- category %>% 
  arrange(desc(avg_price)) %>% 
  head(10) %>% 
  ggplot(aes(x = fct_reorder(category, avg_price), y = avg_price)) +
  geom_col(width = 0.2, fill = "yellow") +
  geom_point(size = 5, color = "navy") +
  scale_y_continuous(labels = scales::dollar_format()) +
  ggtitle("10 Biggest Categories by Average Price of Items") +
  ylab("Avg price") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0), 
    plot.title.position = "plot",
    plot.background = element_rect(fill = "gray", color = "gray"),
    panel.background = element_rect(fill = "gray", color = "gray"),
    axis.text.y = element_text(color = "black"), 
    axis.text.x = element_text(color = "black"), 
    axis.title.y = element_blank()
  ) +
  coord_flip()


title <- ggdraw() +
  draw_label(
    "IKEA's top designers and categories", 
    fontface = "bold", 
    size = 15,
    x = 0, 
    hjust = 0
  ) +
  theme(plot.margin = margin(0,0,0,7), 
        plot.background = element_rect(fill = "gray", color = "gray"))


plot_full <- plot_grid(
  title +
    theme(plot.background = element_rect(color = "gray", fill = "gray")),
  plot_grid(designer_plot, cat_plot1, cat_plot2, ncol = 3, rel_widths= c(1,1,1)) +
    theme(plot.background = element_rect(color = "gray", fill = "gray")), 
  ncol = 1, rel_heights = c(0.15, 1)
)

plot_full

save_plot("tt_ikea.png", plot_full, base_height = 8, base_width = 13)
