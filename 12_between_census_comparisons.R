# setup ------------------------------------------------------------------------

rm(list = ls())

pacman::p_load(corrr, tidyverse)

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
        plot.caption = element_text(hjust = 0,
                                    size = 12))
}

# correlation
owners_full |> 
  select(-mnemonic) |> 
  correlate()

# lm regression
(summary(lm(owned_pct_21 ~ owned_pct_11, data = owners_full)))

# plot
owners_full |> 
  ggplot(aes(x = owned_pct_11, y = owned_pct_21)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method = "lm") +
  labs(x = "Homeownership percentage 2011", y = "Homeownership percentage 2021",
       caption = "Homeownership percentage at local authority district level in 2011 and 2021. Source: Census.") +
  my_theme()

# overcrowded % --------------------------------------------------------------

# correlation
occ_full |> 
  select(overcrowded_pct_21, overcrowded_pct_11) |> 
  mutate(across(everything(), ~log(.))) |> 
  correlate()

# lm regression
(summary(lm(log(overcrowded_pct_21) ~ log(overcrowded_pct_11), data = occ_full)))

# plot
occ_full |> 
  ggplot(aes(x = log(overcrowded_pct_11), y = log(overcrowded_pct_21))) +
  geom_point(alpha = 1/3) +
  geom_smooth(method = "lm") +
  labs(x = "Overcrowded percentage 2011 (log)", y = "Overcrowded percentage 2021 (log)",
       caption = "Overcrowded percentage at local authority district level in 2011 and 2021. Source: Census.") +
  my_theme()

# underoccupied % --------------------------------------------------------------

# correlation
occ_full |> 
  select(underoccupied_pct_11, underoccupied_pct_21) |> 
  #mutate(across(everything(), ~log(.))) |> 
  correlate()

# lm regression
(summary(lm(underoccupied_pct_21 ~ underoccupied_pct_11, data = occ_full)))

# plot
occ_full |> 
  ggplot(aes(x = underoccupied_pct_11, y = underoccupied_pct_21)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method = "lm") +
  labs(x = "Underoccupied percentage 2011", y = "Underoccupied percentage 2021 (log)",
       caption = "Underoccupied percentage at local authority district level in 2011 and 2021. Source: Census.") +
  my_theme()

