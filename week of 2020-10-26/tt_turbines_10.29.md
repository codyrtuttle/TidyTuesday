Tidy Tuesday - Week of 10/27
================
Cody Tuttle
10/29/2020

``` r
# install and load pacakges 

library(pacman)

p_load(rlang, gt, tidyverse, lubridate, janitor, readr, cowplot, sf, maps, mapproj, raster, rnaturalearth, rgeos)
```

``` r
# read in tidytuesday data

turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv') %>% 
  mutate(year = as.numeric(str_sub(commissioning_date, 1, 4)))
```

``` r
# load and clean shapefile data

# turbines_sf <- st_as_sf(as.data.frame(turbine), coords = c("longitude", "latitude"))
# 
# st_crs(turbines_sf) <- 4326

can2 <- ne_states(country = "Canada", returnclass = "sf")
```

``` r
prov_yr <- turbine %>% 
  group_by(province_territory, year) %>% 
  summarize(n_turbines = n(), 
            avg_capacity = mean(turbine_rated_capacity_k_w, na.rm = T), 
            avg_rotor_diam = mean(rotor_diameter_m, na.rm = T), 
            avg_hub_height = mean(hub_height_m, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(name = ifelse(province_territory == "Quebec", "Québec", province_territory))

prov <- prov_yr %>% 
  group_by(province_territory) %>% 
  summarize(n_turbines = sum(n_turbines, na.rm = T), 
            n_manu = n(),
            avg_capacity = mean(avg_capacity, na.rm = T), 
            avg_rotor_diam = mean(avg_rotor_diam, na.rm = T), 
            avg_hub_height = mean(avg_hub_height, na.rm = T)) %>% 
  mutate(name = ifelse(province_territory == "Quebec", "Québec", province_territory))


canada <- merge(can2, prov, all.x = T)
```

``` r
p1 <- ggplot() +
  geom_sf(data = canada, aes(geometry = geometry, fill = n_turbines)) +
  coord_sf(crs = "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") +
  theme_void() +
  geom_sf_label(data = canada, aes(label = abbrev)) +
  scale_fill_viridis_c("Number of Turbines", trans = 'reverse')

p1  
```

![](tt_turbines_10.29_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
p2 <- prov_yr %>% 
  ggplot(aes(x = year, y = n_turbines, fill = name)) +
  geom_col(position = "stack") +
  theme_classic() +
  scale_fill_viridis_d() +
  scale_x_continuous(limits = c(2000, 2020), breaks = seq(2000, 2020, 2)) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())

p2
```

![](tt_turbines_10.29_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
title <- ggdraw() +
  draw_label(
    "Most of Canada's Wind Turbines are in Ontario", 
    fontface = "bold", 
    size = 15,
    x = 0, 
    hjust = 0
  ) +
  theme(plot.margin = margin(0,0,0,7))

subtitle <- ggdraw() +
  draw_label(
    "Though Quebec did add a fair few from 2010-2014", 
    fontface =  "plain",
    size = 12,
    x = 0, 
    hjust = 0
  ) +
  theme(plot.margin = margin(0,0,0,7))

plot <- cowplot::plot_grid(title, subtitle, p1, p2, nrow = 4, rel_heights = c(.20, .20, 3, 2))

save_plot("tt_turbines.png", plot, base_height = 8, base_width = 12)
```

``` r
# ggplot() +
#   geom_sf(data = can2, aes(geometry = geometry, fill = adm0_sr)) +
#   geom_sf(data = turbines_sf, aes(size = turbine_rated_capacity_k_w, color = year)) +
#   coord_sf(crs = "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") +
#   theme_void() +
#   scale_size_continuous(range = c(0.5, 3)) +
#   scale_color_continuous(trans = 'reverse')
```
