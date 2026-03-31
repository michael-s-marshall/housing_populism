# setup ------------------------------------------------------------------------

rm(list = ls())

pacman::p_load(corrr, tidyverse)

rmse <- function(y, yhat){
  out <- sqrt(mean((y - yhat)^2))
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

# importing and cleaning data -------------------------------------------------

owners_21 <- read_csv("data/between_census/homeownership_2021.csv")
owners_11 <- read_csv("data/between_census/homeownership_2011.csv")

occ_21 <- read_csv("data/between_census/occupancy_2021.csv")
occ_11 <- read_csv("data/between_census/occupancy_2011.csv")

owners_21 <- owners_21 |> 
  select(mnemonic, Owned) |> 
  rename(owned_pct = Owned)

owners_11 <- owners_11 |> 
  rename(all = `All categories: Tenure`,
         owned = `Owned: Total`) |> 
  mutate(owned_pct = (owned / all) * 100) |> 
  select(mnemonic, owned_pct)

# full merged dataset of homeownership percent
owners_full <- owners_21 |> 
  left_join(owners_11, by = "mnemonic", suffix = c("_21","_11"))

# making under-occupation and overcrowded vars for 2021
occ_21 <- occ_21 |> 
  mutate(overcrowded_pct = `Occupancy rating of bedrooms: -1` + `Occupancy rating of bedrooms: -1`) |> 
  rename(underoccupied_pct = `Occupancy rating of bedrooms: +2 or more`) |> 
  select(mnemonic, overcrowded_pct, underoccupied_pct)

# making under-occupation and overcrowded vars for 2011
names(occ_11)

occ_11 <- occ_11 |> 
  mutate(overcrowded_pct = ((`Occupancy rating (bedrooms) of -1` + `Occupancy rating (bedrooms) of -2 or less`) / `All categories: Occupancy rating bedrooms`) * 100,
         underoccupied_pct = (`Occupancy rating (bedrooms) of +2 or more` / `All categories: Occupancy rating bedrooms`) * 100) |> 
  select(mnemonic, overcrowded_pct, underoccupied_pct)

occ_full <- occ_21 |> 
  left_join(occ_11, by = "mnemonic", suffix = c("_21","_11"))

# homeownership % -----------------------------------------------------

my_theme <- function(){
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.caption.position = "plot",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7))
}

# correlation
owners_full |> 
  select(-mnemonic) |> 
  correlate()

# lm regression
owners_full2 <- owners_full |> na.omit()
owned_lm <- lm(owned_pct_21 ~ owned_pct_11, data = owners_full2)
summary(owned_lm)

rmse(owners_full2$owned_pct_21, 
     predict(owned_lm))

# plot
owners_full |> 
  ggplot(aes(x = owned_pct_11, y = owned_pct_21)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method = "lm") +
  labs(x = "Homeownership percentage 2011", y = "Homeownership percentage 2021") +
  my_theme()

pg_ggsave(width = 140, "viz/between_census_homeownership.pdf")

# overcrowded % --------------------------------------------------------------

# correlation
occ_full |> 
  select(overcrowded_pct_21, overcrowded_pct_11) |> 
  mutate(across(everything(), ~log(.))) |> 
  correlate()

# lm regression
occ_full2 <- occ_full |> na.omit()
over_lm <- lm(overcrowded_pct_21 ~ overcrowded_pct_11, data = occ_full2)
summary(over_lm)

rmse(occ_full2$overcrowded_pct_21, 
     predict(over_lm))

# plot
occ_full |> 
  ggplot(aes(x = log(overcrowded_pct_11), y = log(overcrowded_pct_21))) +
  geom_point(alpha = 1/3) +
  geom_smooth(method = "lm") +
  labs(x = "Overcrowded percentage 2011 (log)", y = "Overcrowded percentage 2021 (log)") +
  my_theme()

pg_ggsave(width = 140, "viz/between_census_overcrowding.pdf")

# underoccupied % --------------------------------------------------------------

# correlation
occ_full |> 
  select(underoccupied_pct_11, underoccupied_pct_21) |> 
  #mutate(across(everything(), ~log(.))) |> 
  correlate()

# lm regression
under_lm <- lm(underoccupied_pct_21 ~ underoccupied_pct_11, data = occ_full2)
summary(under_lm)

rmse(occ_full2$underoccupied_pct_21, 
     predict(under_lm))

# plot
occ_full |> 
  ggplot(aes(x = underoccupied_pct_11, y = underoccupied_pct_21)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method = "lm") +
  labs(x = "Underoccupied percentage 2011", y = "Underoccupied percentage 2021 (log)") +
  my_theme()

pg_ggsave(width = 140, "viz/between_census_underoccupying.pdf")
