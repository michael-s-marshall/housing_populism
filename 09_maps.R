rm(list = ls()) 

pacman::p_load(tidyverse, sf, cartogram, patchwork, biscale, cowplot, gridGraphics, haven, nngeo)

dat <- readRDS("data/level_two_vars_2021_raw.RDS")

lad_shp <- read_sf("data/lad_shp/LAD_DEC_2021_UK_BGC.shp")

full <- lad_shp %>%  
  left_join(dat, by = c("LAD21CD" = "la_code")) %>% 
  drop_na(degree_pct)

full <- st_transform(full, 27700)

full_cart <- cartogram_cont(full, "pop_density", itermax = 3, maxSizeError = 2)

full_cart <- st_transform(full_cart, st_crs(full))

london_cart <- full_cart |> 
  filter(region_code == "E12000007") |> 
  select(LAD21CD, region_code) |> 
  st_union() |> 
  st_buffer(dist = 500) |>  
  st_buffer(dist = -500) |> 
  st_remove_holes()

# PCA map -----------------------------------------------------------------

pca_dat <- readRDS("data/level_two_vars_2021.RDS")

pca_dat <- pca_dat %>% 
  select(la_code, pc1, pc2) |> 
  mutate(pc_cat = case_when(
    pc1 > 0 & pc2 > 0 ~ "High PC1 & High PC2",
    pc1 > 0 & pc2 <= 0 ~ "High PC1 & Low PC2",
    pc1 <= 0 & pc2 > 0 ~ "Low PC1 & High PC2",
    .default = "Low PC1 & Low PC2"
  ))

full_cart <- full_cart %>% left_join(pca_dat, by = c("LAD21CD" = "la_code"))

full_cart |> 
  st_drop_geometry() |> 
  ggplot() +
  geom_point(aes(x = pc1, y = pc2, colour = pc_cat),
             size = 2, alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "PC1", y = "PC2", colour = "PCA Category") +
  scale_colour_viridis_d()

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

# PCA quartiles map ----------------------------------------------------

full_cart <- full_cart %>% 
  mutate(pc1_quarts = cut_number(pc1, 4, labels = c("1st","2nd","3rd","4th")),
         pc2_quarts = cut_number(pc2, 4, labels = c("1st","2nd","3rd","4th")))

p3 <- full_cart %>% 
  drop_na(pc1) %>% 
  ggplot(aes(fill = pc1_quarts)) +
  geom_sf() +
  scale_fill_viridis_d() +
  labs(fill = "PC1 Quartiles") +
  theme_void() +
  theme(legend.text = element_text(size = 7),
        legend.title = element_text(size = 7))

p4 <- full_cart %>%
  drop_na(pc2) %>% 
  ggplot(aes(fill = pc2_quarts)) +
  geom_sf() +
  scale_fill_viridis_d() +
  labs(fill = "PC2 Quartiles") +
  theme_void() +
  theme(legend.text = element_text(size = 7),
        legend.title = element_text(size = 7))

p3 + p4

ggsave("viz/pca_map_quartiles.png",
       width = 25.5, height = 16.575, unit = "cm")

# PCA category map with London overlaid ---------------------------------------

p5 <- full_cart |> 
  drop_na(pc1) |> 
  ggplot() +
  geom_sf(aes(fill = pc_cat)) +
  geom_sf(data = london_cart, alpha = 0, colour = "red", linewidth = 1.2) +
  scale_fill_viridis_d() +
  labs(fill = "PCA Category") +
  theme_void() +
  theme(legend.position = c(0.875,0.8),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7))

p5

# combined final plot ---------------------------------------------------------

free(p5) + p3 / p4 + plot_layout(widths = c(2.3, 1.2))

pg_ggsave <- function(width = c(90, 140, 190), ...){
  if(width == 90){
    ggsave(...,
           units = "mm",
           width = width,
           height = 67.5)
  } else if (width == 140){
    ggsave(...,
           units = "mm",
           width = width,
           height = 105)
  } else {
    ggsave(...,
           units = "mm",
           width = width,
           height = 142.5)
  }
  
}

pg_ggsave(width = 190, "viz/figure_1.pdf")
