pacman::p_load(tidyverse, interactions, jtools, readxl)

rm(list = ls())

# helper functions ------------------------------

sum_na <- function(dat){
  out <- dat %>% 
    map_int(~sum(is.na(.)))
  return(out)
}

which_na <- function(dat, var){
  out <- dat %>% 
    filter(is.na({{var}})) %>% 
    select({{var}}, la_code) %>% 
    deframe()
  return(out)
}

rescale01 <- function(x, ...){
  out <- (x - min(x, ...)) / (max(x, ...) - min(x, ...))
  return(out)
}

my_ggsave <- function(...){
  ggsave(...,
         units = "px",
         width = 3796,
         height = 2309)
}

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

# education ---------------------------------------

fp <- "X:/marshall_group/User/gp1mmx/housing_populism/data/"

edu <- read_csv("data/census_education_2021.csv",
                na = c("x","NA"))

edu <- edu %>% 
  rename(la_code = `Area code`,
         degree_pct = `Level 4 qualifications and above (percent)`) %>% 
  select(la_code, degree_pct) |> 
  mutate(degree_pct = degree_pct / 100)

# aps ------------------------------------------------------

aps <- read_xlsx("data/aps_degrees_2016.xlsx",
                 na = c("-","*",""))

aps <- aps |> 
  rename(la_code = mnemonic,
         degrees = `Jan 2016-Dec 2016`) |> 
  select(la_code, degrees) |> 
  unique()

aps2 <- read_xlsx("data/aps_degrees_2016.xlsx", 
                  sheet = "people", na = c("-","*",""))

aps2 <- aps2 |> 
  rename(la_code = mnemonic,
         people = `Jan 2016-Dec 2016`) |> 
  select(la_code, people) |> 
  unique()

working_degrees <- aps |> 
  left_join(aps2, by = "la_code") |> 
  mutate(working_degree_pct = (degrees / people) * 100) |> 
  select(la_code, working_degree_pct)

edu |> 
  left_join(working_degrees, by = "la_code") |> 
  ggplot(aes(x = working_degree_pct, y = degree_pct)) +
  geom_point(alpha = 0.3) +
  geom_smooth()

edu |> 
  left_join(working_degrees, by = "la_code") |> 
  corrr::correlate()

# prices data --------------------------------------

# prices
prices <- read_csv("data/median_house_prices_2021.csv")

names(prices) <- names(prices) %>% 
  str_remove_all("Year ending Sep") %>% 
  str_squish()

prices <- prices %>% 
  rename(la_code = `Local authority code`) %>% 
  mutate(prices = `2016`) %>% 
  select(la_code, prices)

# merging
dat <- edu %>% 
  left_join(prices, by = "la_code") %>% 
  filter(la_code != "K04000001")

# tenure ----------------------------------------------------

tenure <- read_csv("data/tenure_2021.csv")

tenure <- tenure %>%
  rename(
    la_code = 1,
    tenure_code = 3,
    n = Observation
  ) %>% 
  filter(tenure_code != -8) %>% 
  group_by(la_code) %>% 
  mutate(tenure = case_when(
    tenure_code == 0 | tenure_code == 1 | tenure_code == 2 ~ "homeowner_pct",
    tenure_code == 3 | tenure_code == 4 ~ "social_rented_pct",
    tenure_code == 5 | tenure_code == 6 | tenure_code == 7 ~ "private_rented_pct",
    .default = NA),
    tenure_pct = n / sum(n)) %>% 
  ungroup() %>% 
  group_by(la_code, tenure) %>% 
  summarise(tenure_pct = sum(tenure_pct), .groups = "drop") %>% 
  pivot_wider(names_from = tenure, values_from = tenure_pct)

# population data -----------------------------------------------------------

pop <- read_csv("data/population_data_2021.csv")

pop <- pop |>  
  rename(la_code = Code,
         pop_density = `2016 people per sq. km`) |>  
  select(la_code, pop_density)

dat <- dat %>% 
  left_join(pop, by = "la_code")

sum_na(dat)

# earnings ---------------------------------------------------------

earnings <- read_csv("data/earnings.csv", na = c("NA",":"))

earnings <- earnings |> 
  rename(la_code = 3, earnings = `2016`) |> 
  select(la_code, earnings)

# ages -------------------------------------------------------------------

ages <- read_csv("data/las_by_age_2021.csv")

total_pops <- ages |> 
  group_by(ladcode21) |> 
  summarise(total_pop = sum(population_2016),
            .groups = "drop")

under_16_pct <- ages |> 
  filter(age < 16) |> 
  group_by(ladcode21) |> 
  summarise(under_16s = sum(population_2016),
            .groups = "drop") |> 
  left_join(total_pops, by = "ladcode21") |> 
  mutate(under_16_pct = under_16s / total_pop) |> 
  select(ladcode21, under_16_pct)

over_65_pct <- ages |> 
  filter(age >= 65) |> 
  group_by(ladcode21) |> 
  summarise(over_65s = sum(population_2016),
            .groups = "drop") |> 
  left_join(total_pops, by = "ladcode21") |> 
  mutate(over_65_pct = over_65s / total_pop) |> 
  select(ladcode21, over_65_pct)

dat <- dat |> 
  left_join(working_degrees, by = "la_code") |> 
  left_join(tenure, by = "la_code") |> 
  left_join(earnings, by = "la_code") |> 
  left_join(under_16_pct, by = c("la_code" = "ladcode21")) |> 
  left_join(over_65_pct, by = c("la_code" = "ladcode21"))

# referendum -----------------------------------------------------------

ref <- read_csv("data/EU-referendum-result-data.csv")

ref <- ref |> 
  select(Area_Code, Area, Region, Pct_Remain, Pct_Leave)

dat <- dat |> 
  left_join(ref, by = c("la_code" = "Area_Code")) |> 
  mutate(london = as.factor(case_when(Region == "London" ~ "London",
                                      .default = "Not London")),
         london = fct_rev(london))

sum_na(dat)

which_na(dat, Pct_Remain)
which_na(dat, working_degree_pct)

# transformations -----------------------------------------------------------------

dat |> 
  ggplot(aes(x = prices)) +
  geom_histogram(bins = 50, colour = "black", fill = "lightgrey")

dat$log_prices <- log(dat$prices)

dat |> 
  ggplot(aes(x = log_prices)) +
  geom_histogram(bins = 50, colour = "black", fill = "lightgrey")

# bivariate OLS -------------------------------------------------

biv_mod <- lm(Pct_Leave ~ log_prices, data = dat)

summary(biv_mod)

par(mfrow = c(2,2))
plot(biv_mod)

# plots of controls --------------------------------------------------------

dat |> 
  ggplot(aes(x = log_prices, y = Pct_Leave)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method = "lm")

cor.test(dat$log_prices, dat$Pct_Leave)

dat |> 
  ggplot(aes(x = working_degree_pct, y = Pct_Leave)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method = "lm")

cor.test(dat$working_degree_pct, dat$Pct_Leave)

dat |> 
  ggplot(aes(x = degree_pct, y = Pct_Leave)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method = "lm")

cor.test(dat$degree_pct, dat$Pct_Leave)

dat |> 
  ggplot(aes(x = working_degree_pct, y = log_prices)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method = "lm")

cor.test(dat$working_degree_pct, dat$log_prices)

dat |> 
  ggplot(aes(x = degree_pct, y = log_prices)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method = "lm")

cor.test(dat$degree_pct, dat$log_prices)

dat |> 
  ggplot(aes(x = pop_density, y = Pct_Leave)) +
  geom_point(alpha = 1/3) +
  geom_smooth()

cor.test(dat$pop_density, dat$Pct_Leave)

dat |> 
  ggplot(aes(x = london, y = Pct_Leave)) +
  geom_boxplot()

dat |> 
  group_by(london) |> 
  summarise(Pct_Leave = mean(Pct_Leave),
            .groups = "drop")

# multivariate OLS: single control ---------------------------------------------

multiv_mod <- lm(Pct_Leave ~ log_prices + working_degree_pct, data = dat)

summary(multiv_mod)

plot(multiv_mod)

dat[277,]

dat_filtered <- dat |> filter(Area != "City of London")

multiv_mod_filtered <- lm(Pct_Leave ~ log_prices + working_degree_pct, data = dat_filtered)

summary(multiv_mod_filtered)

plot(multiv_mod_filtered)

# multivariate OLS: multiple controls, not incl. degree pct --------------------

ans_mod <- lm(Pct_Leave ~ log_prices + london + pop_density + earnings + under_16_pct + over_65_pct,
              data = dat)

summary(ans_mod)

plot(ans_mod)

ans_mod_filtered <- lm(Pct_Leave ~ log_prices + london + pop_density + earnings + under_16_pct + over_65_pct,
                       data = dat_filtered)

summary(ans_mod_filtered)

plot(ans_mod_filtered)

# multivariate OLS: multiple controls, incl. degree pct ------------------------

add_mod <- lm(Pct_Leave ~ log_prices + working_degree_pct + london + pop_density + earnings + under_16_pct + over_65_pct,
              data = dat)

summary(add_mod)

plot(add_mod)

add_mod_filtered <- lm(Pct_Leave ~ log_prices + working_degree_pct + london + pop_density + earnings + under_16_pct + over_65_pct,
                       data = dat_filtered)

summary(add_mod_filtered)

plot(add_mod_filtered)

# plot of coefficients and comparison of models -------------------------------

plot_coefs(ans_mod)
plot_coefs(add_mod)
anova(ans_mod, add_mod)

summ(ans_mod, vifs = TRUE)
summ(add_mod, vifs = TRUE)

# av plots -------------------------------------------------------------------

# internal use plot
p1 <- dat |> 
  ggplot(aes(x = log_prices, y = Pct_Leave)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", colour = "black", linewidth = 1.25) +
  jtools::theme_nice() +
  coord_cartesian(ylim = c(10,80)) +
  labs(x = "Median prices (log)", y = "Leave %")

p2 <- effect_plot(ans_mod, pred = log_prices, interval = TRUE, plot.points = TRUE,
                  x.label = "Median prices (log)",
                  y.label = "Leave %") +
  coord_cartesian(ylim = c(10,80))
p3 <- effect_plot(add_mod, pred = log_prices, interval = TRUE, partial.residuals = TRUE,
                  x.label = "Median prices (log)",
                  y.label = "Leave %") +
  coord_cartesian(ylim = c(10,80))
p4 <- effect_plot(add_mod, pred = working_degree_pct, interval = TRUE, partial.residuals = TRUE,
                  x.label = "Working age population with degrees %",
                  y.label = "Leave %") +
  coord_cartesian(ylim = c(10,80))

require(patchwork)
p1 + p2 + p3 + p4

my_ggsave("viz/figure_2_aggregate.png")

# journal plot
p1 <- dat |> 
  ggplot(aes(x = log_prices, y = Pct_Leave)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", colour = "black", linewidth = 1.25) +
  jtools::theme_nice() +
  coord_cartesian(ylim = c(10,80)) +
  labs(x = "Median prices (log)", y = "Leave %") +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 7))

p2 <- effect_plot(ans_mod, pred = log_prices, interval = TRUE, plot.points = TRUE,
                  x.label = "Median prices (log)",
                  y.label = "Leave %") +
  coord_cartesian(ylim = c(10,80)) +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 7))
p3 <- effect_plot(add_mod, pred = log_prices, interval = TRUE, partial.residuals = TRUE,
                  x.label = "Median prices (log)",
                  y.label = "Leave %") +
  coord_cartesian(ylim = c(10,80)) +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 7))
p4 <- effect_plot(add_mod, pred = working_degree_pct, interval = TRUE, partial.residuals = TRUE,
                  x.label = "Working age population with degrees %",
                  y.label = "Leave %") +
  coord_cartesian(ylim = c(10,80)) +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 7))

require(patchwork)
p1 + p2 + p3 + p4

pg_ggsave(width = 190, "viz/figure_2.pdf")

# homeownership % and prices --------------------------------------------

prices_21 <- read_csv("data/median_house_prices_2021.csv")

names(prices_21) <- names(prices_21) %>% 
  str_remove_all("Year ending Sep") %>% 
  str_squish()

prices_21 <- prices_21 %>% 
  rename(la_code = `Local authority code`) %>% 
  mutate(prices = `2021`) %>% 
  select(la_code, prices)

dat |> 
  left_join(prices_21, by = "la_code",
            suffix = c("_16","_21")) |>
  mutate(log_prices_21 = log(prices_21)) |> 
  ggplot(aes(x = log_prices_21, y = homeowner_pct)) +
  geom_point(aes(colour = london)) +
  geom_smooth(aes(colour = london, fill = london), method = "lm") +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "Median prices (log)", y = "Homeownership percentage",
       colour = NULL, fill = NULL)
