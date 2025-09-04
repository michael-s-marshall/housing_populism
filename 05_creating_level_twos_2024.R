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

# la boundary changes -----------------------------------------------

la_changes <- read_csv("data/la_boundary_changes.csv")

# affordability and prices data --------------------------------------

# affordability ratio
afford <- read_csv("data/affordability_ratio_2024.csv",
                   na = c("[x]", "NA"))

afford <- afford %>% 
  rename(la_code = `Local authority code`,
         affordability = `2024`) %>% 
  select(la_code, affordability) %>% 
  mutate(affordability_log = log(affordability))

# prices
prices <- read_csv("data/median_house_prices_2024.csv",
                   na = c("[x]","NA"))

names(prices) <- names(prices) %>% 
  str_remove_all("Year ending Sep") %>% 
  str_squish()

prices <- prices %>% 
  rename(la_code = `Local authority code`) %>% 
  mutate(prices = log(`2024`)) %>% 
  select(la_code, prices)

# merging
dat <- afford %>%  
  left_join(prices, by = "la_code")

# education ---------------------------------------

edu <- read_csv("data/census_education_2021.csv",
                na = c("x","NA"))

pop_vars <- edu %>% select(contains("number")) %>% names()

edu_add <- edu %>%
  rename(la_2021 = `Area code`) %>% 
  filter(la_2021 %in% la_changes$la_2021) %>% 
  left_join(la_changes, by = "la_2021") %>%
  rename(la_code = la_2024) %>% 
  group_by(la_code) %>% 
  mutate(total_pop = rowSums(across(all_of(pop_vars)))) %>% 
  summarise(
    total_pop = sum(total_pop),
    degree_n = sum(`Level 4 qualifications and above (number)`),
    .groups = "drop"
  ) %>% 
  mutate(degree_pct = degree_n / total_pop) %>% 
  select(la_code, degree_pct)

edu <- edu %>% 
  rename(la_code = `Area code`,
         degree_pct = `Level 4 qualifications and above (percent)`) %>% 
  mutate(degree_pct = degree_pct / 100) %>% 
  filter(!la_code %in% la_changes$la_2021) %>% 
  select(la_code, degree_pct) %>% 
  bind_rows(edu_add)

dat <- dat %>% 
  left_join(edu, by = "la_code")


sum_na(dat)

# population data -----------------------------------------------------------

pop <- read_csv("data/population_data_2024.csv")

pop <- pop %>% 
  rename(la_code = Code,
         pop_density = `2024 people per sq. km`) %>% 
  select(la_code, pop_density)

dat <- dat %>% 
  left_join(pop, by = "la_code")

sum_na(dat)

# tenure ------------------------------------------------------------

tenure <- read_csv("data/tenure_2021.csv")

names(tenure)

tenure_n <- tenure %>%
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
  ungroup()

tenure_add <- tenure_n %>% 
  filter(la_code %in% la_changes$la_2021) %>% 
  left_join(la_changes, by = c("la_code" = "la_2021")) %>% 
  group_by(la_2024, tenure) %>% 
  summarise(tenure_n = sum(n), .groups = "drop_last") %>%
  mutate(tenure_pct = tenure_n / sum(tenure_n)) %>% 
  ungroup() %>% 
  select(-tenure_n) %>% 
  pivot_wider(names_from = tenure, values_from = tenure_pct) %>% 
  rename(la_code = la_2024)

tenure <- tenure_n %>% 
  filter(!la_code %in% la_changes$la_2021) %>% 
  group_by(la_code, tenure) %>% 
  summarise(tenure_pct = sum(tenure_pct), .groups = "drop") %>% 
  pivot_wider(names_from = tenure, values_from = tenure_pct) %>% 
  bind_rows(tenure_add)

dat <- dat %>% 
  left_join(tenure, by = "la_code")

sum_na(dat)

# birth country ---------------------------------------------------

bc <- read_csv("data/birth_country_2021.csv")

bc_n <- bc %>% 
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
  summarise(people = sum(n), .groups = "drop")

bc_add <- bc_n %>% 
  filter(la_code %in% la_changes$la_2021) %>%
  left_join(la_changes, by = c("la_code" = "la_2021")) %>% 
  pivot_wider(names_from = "birth_country", values_from = "people") %>% 
  group_by(la_2024) %>%
  summarise(total_uk = sum(UK),
            total_non_uk = sum(`Non-UK`),
            .groups = "drop") %>% 
  mutate(non_uk_pct = total_non_uk / (total_uk + total_non_uk)) %>% 
  select(la_2024, non_uk_pct) %>% 
  rename(la_code = la_2024)

bc <- bc_n %>%
  filter(!la_code %in% la_changes$la_2021) %>% 
  group_by(la_code) %>% 
  mutate(non_uk_pct = people / sum(people)) %>% 
  ungroup() %>% 
  filter(birth_country == "Non-UK") %>% 
  select(la_code, non_uk_pct) %>% 
  bind_rows(bc_add)

dat <- dat %>% 
  left_join(bc, by = "la_code")

sum_na(dat)

## age of LAs --------------------------------------------------------

ages <- read_csv("data/las_by_age_2024.csv")

total_pops <- ages %>% 
  group_by(ladcode23) %>% 
  summarise(population = sum(population_2024), .groups = "drop")

u16s <- ages %>% 
  filter(age < 16) %>% 
  group_by(ladcode23) %>% 
  summarise(under_16s = sum(population_2024), .groups = "drop")

o65s <- ages %>% 
  filter(age >= 65) %>% 
  group_by(ladcode23) %>% 
  summarise(over_65s = sum(population_2024), .groups = "drop")

total_pops <- total_pops %>% 
  left_join(u16s, by = "ladcode23") %>% 
  left_join(o65s, by = "ladcode23") %>% 
  mutate(over_65_pct = over_65s / population,
         under_16_pct = under_16s / population) %>% 
  select(ladcode23, over_65_pct, under_16_pct) %>% 
  rename(la_code = ladcode23)

dat <- dat %>% 
  left_join(total_pops, by = "la_code")

sum_na(dat)

# hh churn --------------------------------------------------------------

churn <- read_csv("data/hh_churn_oslaua_2023.csv")

churn <- churn |> 
  rename(la_code = area,
         churn = chn2022) |> 
  select(la_code, churn)

dat <- dat |> 
  left_join(churn, by = "la_code")

sum_na(dat)

# overcrowding ------------------------------------------------------------

occ <- read_csv("data/occupancy.csv")

occ_n <- occ %>% 
  rename(la_code = `Area code`,
         overoccupied = 3,
         standard = 4,
         above_one = 5,
         above_two = 6) %>% 
  mutate(total_households = rowSums(across(overoccupied:above_two)))

occ_add <- occ_n %>% 
  filter(la_code %in% la_changes$la_2021) %>% 
  left_join(la_changes, by = c("la_code" = "la_2021")) %>% 
  group_by(la_2024) %>% 
  summarise(
    total_households = sum(total_households),
    total_overoccupied = sum(overoccupied),
    total_underoccupied = sum(above_two),
    .groups = "drop"
  ) %>% 
  mutate(overoccupied_pct = total_overoccupied / total_households,
         underoccupied_pct = total_underoccupied / total_households) %>% 
  select(la_2024, overoccupied_pct, underoccupied_pct) %>% 
  rename(la_code = la_2024)

occ <- occ_n %>%
  filter(!la_code %in% la_changes$la_2021) %>% 
  mutate(total_households = rowSums(across(overoccupied:above_two)),
         overoccupied_pct = overoccupied / total_households,
         underoccupied_pct = above_two / total_households) %>% 
  select(la_code, overoccupied_pct, underoccupied_pct) %>% 
  bind_rows(occ_add)

dat <- dat %>% 
  left_join(occ, by = "la_code")

sum_na(dat)

# temporary accommodation -------------------------------------------------

ta <- read_csv("data/ta_rate_2024.csv")

ta <- ta %>% select(la_code, avg_2024) %>% rename(ta_rate = avg_2024)

dat <- dat %>% 
  left_join(ta, by = "la_code")

sum_na(dat)

which_na(dat, ta_rate)

# possession claims ---------------------------------------------------------

claims <- read_csv("data/possession_statistics_2021.csv")

claims <- claims %>% 
  rename(claims = map_2_landlord_possession_claims) %>% 
  select(la_code, claims)

dat <- dat %>% 
  left_join(claims, by = "la_code")

sum_na(dat)

which_na(dat, claims)

# region --------------------------------------------------------------------

region <- read_csv("data/lad_region_2023.csv")

region <- region |> 
  rename(la_code = `LAD23CD`,
         region_code = `RGN23CD`) |>  
  select(la_code, region_code)

dat <- dat |> 
  left_join(region, by = "la_code")

sum_na(dat)

dat$la_code[is.na(dat$region_code)]  

dat$region_code[is.na(dat$region_code)] <- "W92000004"

sum_na(dat)

# imputing ta_rate for 4 missing authorities ---------------------------------

fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final'
)

predictors <- dat %>%
  select(-la_code, -ta_rate, -affordability_log, -private_rented_pct, -churn, -claims) %>% 
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
                  method = "lm",
                  trControl = fitControl)
model_en
summary(model_en)

dat2 <- dat %>% drop_na(affordability)
dat2[,predictors] <- predict(x_trans, dat2[,predictors])
dat2$lm_preds <- predict(model_lm, newdata = dat2)
dat2$rf_preds <- predict(model_rf, newdata = dat2) 
dat2$nn_preds <- predict(model_nn, newdata = dat2)
dat2$ta_preds <- predict(model_en, newdata = dat2)

dat2 %>% 
  select(lm_preds, rf_preds, nn_preds, ta_preds) %>% 
  pivot_longer(everything(),
               names_to = "model",
               values_to = "pred") %>% 
  ggplot(aes(x = pred, colour = model)) +
  geom_density()

dat2 %>% 
  ggplot(aes(x = ta_preds, y = ta_rate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se=FALSE)

dat2 %>% 
  select(ta_preds, ta_rate) %>% 
  corrr::correlate()

dat <- dat %>% 
  left_join(dat2 %>% select(la_code, ta_preds), by = "la_code") %>% 
  mutate(ta_rate_full = ifelse(is.na(ta_rate), ta_preds, ta_rate))

sum_na(dat)

# imputing claims ------------------------------------------------------

predictors2 <- dat %>%
  select(-la_code, -ta_rate, -affordability_log, -private_rented_pct, -churn, -claims, -ta_preds, -ta_rate_full) %>% 
  names()

outcome_name2 <- "claims"

train_dat2 <- dat %>% 
  column_to_rownames("la_code") %>% 
  as.data.frame() %>% 
  na.omit()

x_trans2 <- preProcess(train_dat2[,predictors2])
train_dat2[,predictors2] <- predict(x_trans2, train_dat2[,predictors2])

# training the lm model 
model_lm2 <- train(train_dat2[,predictors2],
                   train_dat2[,outcome_name2],
                   method= "lm",
                   trControl = fitControl)
summary(model_lm2)

# training the random forest
set.seed(123)
model_rf2 <- train(train_dat2[,predictors2],
                   train_dat2[,outcome_name2],
                   method = "rf",
                   trControl = fitControl,
                   tuneLength = 7)

model_rf2

# neural network
set.seed(123)
model_nn2 <- train(train_dat2[,predictors2],
                   train_dat2[,outcome_name2],
                   method= "nnet",
                   trControl = fitControl,
                   linout = TRUE,
                   tuneLength = 7)

model_nn2

# ensemble 
train_dat2$lm_preds <- predict(model_lm2, newdata = train_dat2)
train_dat2$rf_preds <- predict(model_rf2, newdata = train_dat2)
train_dat2$nn_preds <- predict(model_nn2, newdata = train_dat2)

set.seed(123)
model_en2 <- train(train_dat2[,top_layer],
                   train_dat2[,outcome_name2],
                   method = "nnet",
                   linout = TRUE,
                   trControl = fitControl,
                   tuneLength = 7)
model_en2

dat3 <- dat %>% drop_na(affordability)
dat3[,predictors2] <- predict(x_trans2, dat3[,predictors2])
dat3$lm_preds <- predict(model_lm2, newdata = dat3)
dat3$rf_preds <- predict(model_rf2, newdata = dat3) 
dat3$nn_preds <- predict(model_nn2, newdata = dat3)
dat3$claims_preds <- predict(model_en2, newdata = dat3)

dat3 %>% 
  ggplot(aes(x = claims_preds, y = claims)) +
  geom_point(alpha = 0.5) +
  geom_smooth()

dat3 %>% 
  select(claims_preds, claims) %>% 
  corrr::correlate()

dat <- dat %>% 
  left_join(dat3 %>% select(la_code, claims_preds), by = "la_code") %>% 
  mutate(claims_full = ifelse(is.na(claims), claims_preds, claims))

sum_na(dat)

which_na(dat, claims_full) # isles of scilly

# scaling and saving ---------------------------------------

to_scale <- dat %>% select_if(is.numeric) %>% names()

dat <- dat %>% rename_raw(all_of(to_scale))

dat[to_scale] <- dat[to_scale] %>% 
  map_df(scale_this)

dat

# PCA ----------------------------------------------------------

pca_dat <- dat |> 
  select(la_code, affordability, homeowner_pct, ta_rate_full, 
         overoccupied_pct, underoccupied_pct, claims_full) %>% 
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
               PC4 = var_explained[4],
               PC5 = var_explained[5],
               PC6 = var_explained[6])
    )

write.csv(pca_tab, file = "tables/pca_table_2024.csv")

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

# saving ------------------------------------------------------------------

saveRDS(dat, "data/level_two_vars_2024.RDS")
