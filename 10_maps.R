rm(list = ls()) 

pacman::p_load(tidyverse, sf, cartogram, patchwork, biscale, cowplot, gridGraphics, haven)

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
  theme_void()

p4 <- full_cart %>%
  drop_na(pc2) %>% 
  ggplot(aes(fill = pc2_quarts)) +
  geom_sf() +
  scale_fill_viridis_d() +
  labs(fill = "PC2 Quartiles") +
  theme_void()

p3 + p4

ggsave("viz/pca_map_quartiles.png",
       width = 25.5, height = 16.575, unit = "cm")

# interaction plot from Adler and Ansell ---------------------------------------

rm(list = ls())

dat <- read_dta("data/panel_data/BES2019_W22_v24.0.dta")

lvl2 <- readRDS("data/level_two_vars_2021_raw.RDS")

dat <- dat %>% 
  left_join(lvl2,
            by = c("oslaua_code" = "la_code")) %>% 
  mutate(p_eurefvote = na_if(p_eurefvote, 9999),
         homeowner = as.factor(ifelse(dat$p_housing == 1 | dat$p_housing == 2, "Homeowner", "Non-homeowner")),
         uni = ifelse(p_edlevel > 3, 1, 0),
         white_british = ifelse(p_ethnicity == 1, 1, 0))

dat %>% count(p_eurefvote)
dat %>% count(homeowner, p_housing)

brexit_mod <- glm(p_eurefvote ~  
                    (homeowner * prices) +
                    over_65_pct + under_16_pct + non_uk_pct +
                    pop_density, data = dat, family = "binomial")

summary(brexit_mod)

pacman::p_load(ggeffects)

predictions <- predict_response(brexit_mod, 
                                c("prices [all]", "homeowner"))

as_tibble(predictions) %>% 
  ggplot(aes(x = x, y = predicted, group = group)) +
  geom_ribbon(aes(fill = group, ymax = conf.high, ymin = conf.low), alpha = 0.3) +
  geom_line(aes(colour = group), linewidth = 2) +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  labs(colour = NULL, fill = NULL, x = "Median house price (log)", y = "Predicted probability of voting Leave",
       caption = "Interaction between homeownership and median house prices in predicting support for Leave. Replication of Adler and Ansell (2019).") +
  theme(
    axis.title = element_text(face = "bold",
                              size = 12),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0,
                                size = 12),
    panel.grid.minor = element_blank()
  )

ggsave("viz/replication_adler_ansell.png")
