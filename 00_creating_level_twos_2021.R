pacman::p_load(tidyverse, haven, caret, randomForest)

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
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

rename_raw <- function(df, vars){
  df <- df %>% 
    mutate(across({{vars}}, \(x) x = x, .names = "{.col}_raw"))
  return(df)
}


# education ---------------------------------------

edu <- read_csv("data/census_education_2021.csv",
                na = c("x","NA"))

edu <- edu %>% 
  rename(la_code = `Area code`,
         degree_pct = `Level 4 qualifications and above (percent)`) %>% 
  select(la_code, degree_pct) |> 
  mutate(degree_pct = degree_pct / 100)

# affordability and prices data --------------------------------------

# affordability ratio
afford <- read_csv("data/affordability_ratio_2021.csv",
                   na = c(":", "NA"))

afford <- afford %>% 
  rename(la_code = `Local authority code`,
         affordability = `2021`) %>% 
  select(la_code, affordability) %>% 
  mutate(affordability_log = log(affordability))

# prices
prices <- read_csv("data/median_house_prices_2021.csv")

names(prices) <- names(prices) %>% 
  str_remove_all("Year ending Sep") %>% 
  str_squish()

prices <- prices %>% 
  rename(la_code = `Local authority code`) %>% 
  mutate(prices = log(`2021`)) %>% 
  select(la_code, prices)

# merging
dat <- edu %>% 
  full_join(afford, by = "la_code") %>% 
  left_join(prices, by = "la_code") %>% 
  filter(la_code != "K04000001")

# population data -----------------------------------------------------------

pop <- read_csv("data/population_data_2021.csv")

pop <- pop %>% 
  rename(la_code = Code,
         pop_density = `2021 people per sq. km`) %>% 
  select(la_code, pop_density)

dat <- dat %>% 
  left_join(pop, by = "la_code")

sum_na(dat)

# tenure ------------------------------------------------------------

tenure <- read_csv("data/tenure_2021.csv")

names(tenure)

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

dat <- dat %>% 
  left_join(tenure, by = "la_code")

sum_na(dat)

# birth country ---------------------------------------------------

bc <- read_csv("data/birth_country_2021.csv")

bc <- bc %>% 
  rename(
    la_code = 1,
    country_code = 3,
    n = Observation
  ) %>% 
  filter(country_code != -8) %>% 
  mutate(birth_country = case_when(
    country_code == 1 ~ "UK",
    .default = "Non-UK")) %>% 
  group_by(la_code, birth_country) %>% 
  summarise(people = sum(n), .groups = "drop_last") %>% 
  mutate(non_uk_pct = people / sum(people)) %>% 
  ungroup() %>% 
  filter(birth_country == "Non-UK") %>% 
  select(la_code, non_uk_pct)

dat <- dat %>% 
  left_join(bc, by = "la_code")

sum_na(dat)

## age of LAs --------------------------------------------------------

ages <- read_csv("data/las_by_age_2021.csv")

total_pops <- ages %>% 
  group_by(ladcode21) %>% 
  summarise(population = sum(population_2021), .groups = "drop")

u16s <- ages %>% 
  filter(age < 16) %>% 
  group_by(ladcode21) %>% 
  summarise(under_16s = sum(population_2021), .groups = "drop")

o65s <- ages %>% 
  filter(age >= 65) %>% 
  group_by(ladcode21) %>% 
  summarise(over_65s = sum(population_2021), .groups = "drop")

total_pops <- total_pops %>% 
  left_join(u16s, by = "ladcode21") %>% 
  left_join(o65s, by = "ladcode21") %>% 
  mutate(over_65_pct = over_65s / population,
         under_16_pct = under_16s / population) %>% 
  select(ladcode21, over_65_pct, under_16_pct) %>% 
  rename(la_code = ladcode21)

dat <- dat %>% 
  left_join(total_pops, by = "la_code")

sum_na(dat)

# hh churn --------------------------------------------------------------

churn <- read_csv("data/hh_churn_oslaua_2023.csv")

churn <- churn |> 
  rename(la_code = area,
         churn = chn2021) |> 
  select(la_code, churn)

dat <- dat |> 
  left_join(churn, by = "la_code")

sum_na(dat)

# overcrowding ------------------------------------------------------------

occ <- read_csv("data/occupancy.csv")

occ <- occ %>% 
  rename(la_code = `Area code`,
         overoccupied = 3,
         standard = 4,
         above_one = 5,
         above_two = 6) 

occ <- occ %>% 
  mutate(total_households = rowSums(across(overoccupied:above_two)),
         overoccupied_pct = overoccupied / total_households) %>% 
  select(la_code, overoccupied_pct)

dat <- dat %>% 
  left_join(occ, by = "la_code")

sum_na(dat)

# temporary accommodation -------------------------------------------------

ta <- read_csv("data/ta_rate_2021.csv")

ta <- ta %>% select(la_code, avg_2021) %>% rename(ta_rate = avg_2021)

dat <- dat %>% 
  left_join(ta, by = "la_code")

sum_na(dat)

which_na(dat, ta_rate)

# region --------------------------------------------------------------------

region <- read_csv("data/lasregionew2021lookup.csv")

region <- region |> 
  rename(la_code = `LA code`,
         region_code = `Region code`) |>  
  select(la_code, region_code)

dat <- dat |> 
  left_join(region, by = "la_code")

sum_na(dat)

# imputing ta_rate for 4 missing authorities ---------------------------------

fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final'
)

predictors <- dat %>%
  select(-la_code, -ta_rate, -affordability_log, -private_rented_pct, -churn) %>% 
  names()

outcome_name <- "ta_rate"

train_dat <- dat %>% 
  column_to_rownames("la_code") %>% 
  as.data.frame() %>% 
  na.omit()

x_trans <- preProcess(train_dat[,predictors])
train_dat[,predictors] <- predict(x_trans, train_dat[,predictors])

# training the lm model 
model_lm <- train(train_dat[,predictors],
                  train_dat[,outcome_name],
                  method= "lm",
                  trControl = fitControl)
summary(model_lm)

# training the random forest
set.seed(123)
model_rf <- train(train_dat[,predictors],
                  train_dat[,outcome_name],
                  method = "rf",
                  trControl = fitControl,
                  tuneLength = 7)

model_rf

# neural network
set.seed(123)
model_nn <- train(train_dat[,predictors],
                  train_dat[,outcome_name],
                  method= "nnet",
                  trControl = fitControl,
                  linout = TRUE,
                  tuneLength = 7)

model_nn

# ensemble 
train_dat$lm_preds <- predict(model_lm, newdata = train_dat)
train_dat$rf_preds <- predict(model_rf, newdata = train_dat)
train_dat$nn_preds <- predict(model_nn, newdata = train_dat)

top_layer <- c("lm_preds","rf_preds","nn_preds")

set.seed(123)
model_en <- train(train_dat[,top_layer],
                  train_dat[,outcome_name],
                  method = "nnet",
                  linout = TRUE,
                  trControl = fitControl,
                  tuneLength = 3)
model_en

dat2 <- dat %>% drop_na(affordability)
dat2[,predictors] <- predict(x_trans, dat2[,predictors])
dat2$lm_preds <- predict(model_lm, newdata = dat2)
dat2$rf_preds <- predict(model_rf, newdata = dat2) 
dat2$nn_preds <- predict(model_nn, newdata = dat2)
dat2$ta_preds <- predict(model_en, newdata = dat2)

dat2 %>% 
  ggplot(aes(x = ta_preds, y = ta_rate)) +
  geom_point(alpha = 0.5) +
  geom_smooth()

dat2 %>% 
  select(ta_preds, ta_rate) %>% 
  corrr::correlate()

dat <- dat %>% 
  left_join(dat2 %>% select(la_code, ta_preds), by = "la_code") %>% 
  mutate(ta_rate_full = ifelse(is.na(ta_rate), ta_preds, ta_rate))

sum_na(dat)

# scaling ---------------------------------------

to_scale <- dat %>% select_if(is.numeric) %>% names()

dat_raw <- dat

dat <- dat %>% rename_raw(all_of(to_scale))

dat[to_scale] <- dat[to_scale] %>% 
  map_df(scale_this)

# PCA ----------------------------------------------------------

pca_dat <- dat |> 
  select(la_code, affordability, homeowner_pct, ta_rate_full, overoccupied_pct) %>% 
  na.omit()

pca_mat <- pca_dat %>% 
  select(-la_code) %>% 
  as.matrix()

pca_fit <- prcomp(pca_mat)

biplot(pca_fit)

pca_fit$rotation
pca_fit$sdev
var_explained <- pca_fit$sdev / sum(pca_fit$sdev)
cumsum(var_explained)

pca_tab <- pca_fit$rotation %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  bind_rows(
    data.frame(rowname="variance_explained",
               PC1 = var_explained[1],
               PC2 = var_explained[2],
               PC3 = var_explained[3],
               PC4 = var_explained[4])
  )

write.csv(pca_tab, file = "tables/pca_table_2021.csv")

pca_dat$pc1 <- scale_this(pca_fit$x[,"PC1"])
pca_dat$pc2 <- 0 - scale_this(pca_fit$x[,"PC2"])

pca_dat %>% 
  ggplot(aes(x = pc1)) +
  geom_density()

pca_dat %>% 
  ggplot(aes(x = pc2)) +
  geom_density()

dat <- dat %>% 
  left_join(pca_dat %>% select(la_code, pc1, pc2), by = "la_code")

saveRDS(dat_raw, "data/level_two_vars_2021_raw.RDS")
saveRDS(dat, "data/level_two_vars_2021.RDS")
