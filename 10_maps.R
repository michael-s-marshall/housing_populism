rm(list = ls()) 

pacman::p_load(tidyverse, sf, cartogram, patchwork, biscale, cowplot, gridGraphics)

dat <- readRDS("data/level_two_vars_2021_raw.RDS")

lad_shp <- read_sf("data/lad_shp/LAD_DEC_2021_UK_BGC.shp")

full <- lad_shp %>%  
  left_join(dat, by = c("LAD21CD" = "la_code")) %>% 
  drop_na(degree_pct)

full <- st_transform(full, 27700)

full_cart <- cartogram_cont(full, "pop_density", itermax = 3, maxSizeError = 2)

full_cart <- st_transform(full_cart, st_crs(full))

# bivariate map ---------------------------------------------------------------

bi_dat <- bi_class(full_cart, 
                   x = prices, 
                   y = overoccupied_pct,
                   style = "jenks", 
                   dim = 3)

bi_map <- ggplot(bi_dat) +
  geom_sf(aes(fill = bi_class), show.legend = FALSE) +
  bi_scale_fill(pal = "BlueYl", dim = 3) +
  bi_theme()

my_legend <- bi_legend(pal = "BlueYl",
                       dim = 3,
                       xlab = "Median price (log)",
                       ylab = "Overcrowded %",
                       size = 8)

final_plot <- ggdraw() +
  draw_plot(bi_map, 0, 0, 1, 1) +
  draw_plot(my_legend, 0.2, .65, 0.2, 0.2)

final_plot

ggsave("viz/bivariate_map.png",
       width = 25.5, height = 16.575, unit = "cm")

# PCA map -----------------------------------------------------------------

pca_dat <- readRDS("data/level_two_vars_2021.RDS")

pca_dat <- pca_dat %>% 
  select(la_code, pc1, pc2) 

full_cart <- full_cart %>% left_join(pca_dat, by = c("LAD21CD" = "la_code"))

p1 <- full_cart %>% 
  ggplot(aes(fill = pc1)) +
  geom_sf() +
  scale_fill_viridis_c() +
  labs(fill = "PC1") +
  theme_void()

p1

p2 <- full_cart %>% 
  ggplot(aes(fill = pc2)) +
  geom_sf() +
  scale_fill_viridis_c() +
  labs(fill = "PC2") +
  theme_void()

p2

p1 + p2

ggsave("viz/pca_map.png",
       width = 25.5, height = 16.575, unit = "cm")
