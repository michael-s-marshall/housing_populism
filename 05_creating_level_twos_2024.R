pacman::p_load(tidyverse, haven, caret, randomForest, ggcorrplot)

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
         pop_density = `2024 people per sq. km`,
         pop_density_2014 = `2014 people per sq. km`)  |>  
  mutate(pop_density_change = pop_density / pop_density_2014) |> 
  select(la_code, pop_density, pop_density_change)

pop |> 
  ggplot(aes(x = pop_density_change)) +
  geom_density()

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

ta_dat <- dat %>%
  select(-la_code, -affordability_log, -affordability, -private_rented_pct, -churn, -pop_density_change) %>% 
  na.omit()

ta_dat <- ta_dat |> 
  bind_cols(as.data.frame(model.matrix(~region_code, data = ta_dat)[,-1])) |> 
  select(-region_code)

ta_predictors <- ta_dat %>%
  select(-ta_rate)

outcome <- ta_dat$ta_rate

set.seed(123)
in_train <- createDataPartition(outcome, p = 0.9, list = F)
train_predictors <- ta_predictors[in_train,] |> as.data.frame()
test_predictors <- ta_predictors[-in_train,] |> as.data.frame()
train_outcome <- outcome[in_train]
test_outcome <- outcome[-in_train]

numerics <- train_predictors |> select(degree_pct:underoccupied_pct) |> names()

# scaling and centering
x_trans <- preProcess(train_predictors[,numerics])
train_predictors[,numerics] <- predict(x_trans, train_predictors[,numerics])
test_predictors[,numerics] <- predict(x_trans, test_predictors[,numerics])
ta_predictors[,numerics] <- predict(x_trans, ta_predictors[,numerics])

# training the lm model 
model_lm <- train(train_predictors,
                  train_outcome,
                  method= "lm",
                  trControl = fitControl)
summary(model_lm)

# training the random forest
set.seed(123)
model_rf <- train(train_predictors,
                  train_outcome,
                  method = "rf",
                  trControl = fitControl,
                  tuneLength = 7)

model_rf

# neural network
set.seed(123)
model_nn <- train(train_predictors,
                  train_outcome,
                  method= "nnet",
                  trControl = fitControl,
                  linout = TRUE,
                  tuneLength = 7)

model_nn

# ensemble 
test_predictors$pred_rf <- predict(object = model_rf, test_predictors)
test_predictors$pred_lm <- predict(object = model_lm, test_predictors)
test_predictors$pred_nn <- predict(object = model_nn, test_predictors)

#Predicting the out of fold prediction non-decent % for training data
train_predictors$OOF_pred_rf <- model_rf$pred$pred[order(model_rf$pred$rowIndex)]
train_predictors$OOF_pred_lm <- model_lm$pred$pred[order(model_lm$pred$rowIndex)]
train_predictors$OOF_pred_nn <- model_nn$pred$pred[order(model_nn$pred$rowIndex)]

#Predicting non-decent % for the test data
test_predictors$OOF_pred_rf <- predict(model_rf, test_predictors)
test_predictors$OOF_pred_lm <- predict(model_lm, test_predictors)
test_predictors$OOF_pred_nn <- predict(model_nn, test_predictors)

top_layer <- c('OOF_pred_rf', 'OOF_pred_lm', 'OOF_pred_nn')

set.seed(123)
model_en <- train(train_predictors[,top_layer],
                  train_outcome,
                  method = "nnet",
                  linout = TRUE,
                  trControl = fitControl,
                  tuneLength = 3)
model_en

test_predictors$pred_en <- predict(model_en, test_predictors)

sqrt(mean((test_outcome - test_predictors$pred_rf)^2))
sqrt(mean((test_outcome - test_predictors$pred_lm)^2))
sqrt(mean((test_outcome - test_predictors$pred_nn)^2))
sqrt(mean((test_outcome - test_predictors$pred_en)^2))

train_predictors$pred_en <- predict(model_en, train_predictors)

ggplot(data = NULL) +
  geom_density(aes(x = train_outcome)) +
  geom_density(aes(x = train_predictors$pred_en),
               colour = "red") +
  geom_density(aes(x = train_predictors$OOF_pred_rf),
               colour = "blue")

ggplot(data = NULL) +
  geom_density(aes(x = test_outcome)) +
  geom_density(aes(x = test_predictors$pred_en),
               colour = "red") +
  geom_density(aes(x = test_predictors$pred_rf),
               colour = "blue")

pred_dat <- dat %>%
  select(-la_code, -affordability_log, -affordability, -private_rented_pct, -churn) %>%
  bind_cols(as.data.frame(model.matrix(~region_code, data = dat)[,-1]))

pred_dat[,numerics] <- predict(x_trans, pred_dat[,numerics])

pred_dat$OOF_pred_rf <- predict(model_rf, pred_dat)
pred_dat$OOF_pred_lm <- predict(model_lm, pred_dat)
pred_dat$OOF_pred_nn <- predict(model_nn, pred_dat)
pred_dat$en_preds <- predict(model_en, pred_dat)
pred_dat$ta_preds <- predict(model_rf, pred_dat)

ggplot(data = NULL, 
       aes(x = pred_dat$ta_preds, y = dat$ta_rate)) +
  geom_point() +
  geom_smooth()

cor.test(pred_dat$ta_preds, dat$ta_rate)
(summary(lm(pred_dat$ta_rate ~ poly(pred_dat$ta_preds,3))))

dat <- dat %>% 
  mutate(ta_rate_full = ifelse(is.na(ta_rate), pred_dat$ta_preds, ta_rate))

# rents -----------------------------------------------------------------

rents <- read_csv("data/rent_index.csv", na = c("[z]","[x]"))

rents <- rents |>
  filter(`Time period` == "Dec-2021") |>
  rename(la_code = `Area code`,
         rent = `Rental price`) |>
  select(la_code, rent)

rents$la_code[rents$la_code == "E08000038"] <- "E08000016" # barnsley

rents$la_code[rents$la_code == "E08000039"] <- "E08000019" # sheffield

rents |>
  ggplot(aes(x = rent)) +
  geom_density()

rents |>
  ggplot(aes(x = log(rent))) +
  geom_density()

rents$rent <- log(rents$rent)

dat |> 
  left_join(rents, by = "la_code") |> 
  which_na(rent)

dat <- dat |> left_join(rents, by = "la_code")

dat |> sum_na()

# scaling ---------------------------------------

to_scale <- dat %>% select_if(is.numeric) %>% names()

dat_raw <- dat

dat <- dat %>% rename_raw(all_of(to_scale))

dat[to_scale] <- dat[to_scale] %>% 
  map_df(scale_this)

# PCA ----------------------------------------------------------

pca_dat <- dat |> 
  select(la_code, affordability, homeowner_pct, ta_rate_full, 
         overoccupied_pct, underoccupied_pct, rent) %>% 
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

range(pca_dat$pc1)
range(pca_dat$pc2)

pca_dat %>% 
  mutate(pc1_cut = cut_width(pc1, 1)) %>% 
  ggplot(aes(x = pc1)) +
  geom_histogram(aes(fill = pc1_cut), binwidth = 0.05, colour = "black") +
  scale_fill_viridis_d()

pca_dat %>% 
  mutate(pc2_cut = cut_width(pc2, 1)) %>% 
  ggplot(aes(x = pc2)) +
  geom_histogram(aes(fill = pc2_cut), binwidth = 0.05, colour = "black") +
  scale_fill_viridis_d()

dat <- dat %>% 
  left_join(pca_dat %>% select(la_code, pc1, pc2), by = "la_code")

# saving ------------------------------------------------------------------

saveRDS(dat, "data/level_two_vars_2024.RDS")

# correlation matrix -------------------------------------

cor_mat <- dat_raw |> 
  select(la_code, degree_pct, rent, affordability, 
         prices:homeowner_pct,
         social_rented_pct:under_16_pct,
         overoccupied_pct, underoccupied_pct, ta_rate_full) |>
  left_join(dat[,c("la_code","pc1","pc2")],
            by = "la_code") |> 
  select(-la_code) |> 
  cor(use = "complete.obs") |> 
  round(2)

cor_mat

ggcorrplot(cor_mat,
           hc.order = TRUE,
           type = "lower",
           outline.color = "white",
           lab = TRUE)

write.csv(cor_mat, "tables/cor_mat_2024.csv")
