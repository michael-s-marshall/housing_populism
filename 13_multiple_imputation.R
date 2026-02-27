pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance, margins, mice, broom.mixed, ggmice)

rm(list = ls())

# helper function -----------------------------------------------------

select <- dplyr::select

# loading dataset ---------------------------------------------

dat <- readRDS("data/cross_sectional_dat_2021.RDS")

# missing observations --------------------------------

df_immi <- dat %>% 
  select(-tory_2019, -uni, -income,
         -full_time, -education_age, -log_hh, -disabled, -unemployed,
         -part_time, -log_age, 
         -region_fct, -e, -d, -c1, -c2, -b, -cohabiting) %>% 
  rename(LAD = la_code)

sum_na <- function(dat){
  out <- dat %>% 
    map_int(~sum(is.na(.)))
  return(out)
}

sum_na(df_immi)

missing_las <- function(df, x){
  step1 <- df %>% 
    mutate(nas = is.na({{x}})) %>% 
    filter(nas == T) %>% 
    count(LAD) %>% 
    arrange(desc(n))
  out <- step1$n
  names(out) <- step1$LAD
  return(out)
}

# missing affordability is Scotland and City of London
missing_las(df_immi, affordability)

# missing degree pct is Scotland
missing_las(df_immi, degree_pct)

# missing Non-UK born % is Scotland
missing_las(df_immi, non_uk_pct)

# removing scotland -------------------------------------------------------------

# removing NAs
df_eng_wales <- df_immi %>% drop_na(c(degree_pct, non_uk_pct))

df_eng_wales %>% sum_na()

df_immi <- df_immi %>% 
  na.omit()

full_n <- nrow(dat)
full_n
eng_wales_n <- nrow(df_eng_wales)
full_n - eng_wales_n # 2580 observations removed via Scotland

rm(df_eng_wales, df_immi)

# removing level 2 missing observations ------------------------------

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

df_full <- dat %>% 
  mutate(la_code = as.integer(as.factor(la_code))) |> 
  select(-tory_2019,
         -full_time, -education_age, -log_hh, -disabled, -unemployed,
         -part_time, -log_age, 
         -region_fct, -e, -d, -c1, -c2, -b, -cohabiting, #-edu_20plus,
         -rf_uni_preds, -pred_el_round, -income_knn, -income_full, -income_full_knn,
         -ta_rate, -churn_raw, -churn) %>% 
  rename(LAD = la_code)

sum_na(df_full)

level_2s <- df_full |> select(degree_pct:underoccupied_pct_raw,ta_rate_full_raw) |> names()

df_full <- df_full |> drop_na(all_of(level_2s))

sub_ns <- nrow(df_full)
full_n - sub_ns # 2583 removed from level 2

#these_vars <- df_full %>% 
#  select(-income, -uni, -edu_20plus) %>% 
#  names()

# removing missing observations from DV -----------------------------------

sum(is.na(df_full$immigSelf)) # 2582 missing in DV

sum_na(df_full)

df_full <- df_full |> 
  drop_na(immigSelf)

no_missing_dv_n <- nrow(df_full)
no_missing_dv_n # 22948 obs

df_full <- df_full %>% 
  select(id:rent, immigSelf)

sub_ns - nrow(df_full) # confirmed that 2582 removed
final_n <- nrow(df_full)
final_n

sum_na(df_full)

# imputation ---------------------------------------------------------------------

df_full <- df_full %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability,
         region_code = as.factor(region_code))

# Keep only the variables needed for the imputation model
predictors <- df_full |> 
  select(-id, -age_raw, -LAD, -immigSelf, -income, -own_outright, -own_mortgage,
         -prices, -affordability_log, -private_rented_pct, -rent) |> 
  names()

vars_to_impute <- c("income", "LAD", "immigSelf", predictors)
df_subset <- df_full[, vars_to_impute]
df_subset |> sum_na()

# Initialize the MICE model
init <- mice(df_subset, maxit = 0)
meth <- init$method
pred <- init$predictorMatrix

meth["income"] <- "2l.pan"

pred[,"LAD"] <- -2
pred["LAD","LAD"] <- 0
pred["income", "immigSelf"] <- 1
pred["income", predictors] <- 1
pred["income", "income"] <- 0
pred

# Run the multiple imputation
imp_mice <- mice(df_subset, method = meth, predictorMatrix = pred, m = 5, maxit = 5, seed = 123, printFlag = FALSE)

imp_mice$imp

# diagnostics ------------------------------------------------------------------

# ggmice income boxplot
ggmice(imp_mice, aes(x = .imp, y = income)) +
  geom_boxplot() +
  labs(x = "Imputation number")

# ggmice income density plot
ggmice(imp_mice, aes(x = income, group = .imp)) +
  geom_density()

# ggmice uni density plot
ggmice(imp_mice, aes(x = uni, group = .imp)) +
  geom_density()

# ggmice social housing density plot
ggmice(imp_mice, aes(x = social_housing, group = .imp)) +
  geom_density()

# ggmice social housing X affordability boxplot
ggmice(imp_mice, aes(x = .imp, y = social_housing.affordability)) +
  geom_boxplot() +
  labs(x = "Imputation number")

# ggmice social housing density plot
ggmice(imp_mice, aes(x = social_housing.affordability, group = .imp)) +
  geom_density()

# scaling ---------------------------------------------------------------------

imp_mice$imp$income <- as.data.frame(map_df(imp_mice$imp$income, scale_this))

# lmer -------------------------------------------------------------------------

# Fit the multilevel regression to each of the imputed datasets
models_fit <- with(data = imp_mice, exp = {
  lmer(immigSelf ~ social_housing + homeowner + private_renting +
         affordability +
         male + 
         white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
         no_religion + 
         age + income + uni +
         c1_c2 + d_e + non_uk_born + 
         non_uk_pct +
         over_65_pct + under_16_pct + 
         degree_pct +
         social_rented_pct +
         region_code +
         social_housing.affordability + 
         homeowner.affordability +
         (1|LAD))
})

# Pool the results from all the fitted lme4 models
pooled_results <- pool(models_fit)

# View the pooled summary
summary(pooled_results)

# comparison --------------------------------------------------------------------

immi_reg <- read_csv("models/immi_reg_2021_coefficients.csv")
immi_reg <- immi_reg |> select(term, estimate, p.value) |> mutate(model = "EL")

coef_names <- tibble(
  term = unique(c(as.character(summary(pooled_results)$term),immi_reg$term)),
  coef_names = c("Intercept","Social housing","Homeowner","Private renting",
            "Affordability ratio","Gender: Male",
            "Ethnicity: White British","Ethnicity: White other","Ethnicity: Indian", "Ethnicity: Black", "Ethnicity: Chinese", "Ethnicity: Pakistan/Bangladesh","Ethnicity: Mixed-race",
            "Religion: None","Age", "Household income", "Education: Degree", "Social class: C1-C2", "Social class: D-E",
            "Birth country: UK", "Non-UK born %", "Over 65 %", "Under 16 %", "Degree educated %", "Social housing %",
            "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "Wales",
            "Affordability ratio X Social housing", "Affordability ratio X homeowner", "Household income", "Education: Degree", NA, NA)
  )

pacman::p_load(ggh4x)

summary(pooled_results) |> 
  as_tibble() |> 
  select(term, estimate, p.value) |> 
  mutate(model = "MICE") |> 
  bind_rows(immi_reg) |> 
  left_join(coef_names, by = "term") |> 
  mutate(
    sig = case_when(p.value < 0.001 ~ "<0.001",
                    p.value < 0.01 ~ "<0.01",
                    p.value < 0.05 ~ "<0.05",
                    .default = "Not Sig."),
    housing_vars = case_when(str_detect(coef_names, "housing$|Housing|Home|renting|home|Afford") ~ "Housing variables",
                             .default = "Controls"),
    housing_vars = fct_relevel(as.factor(housing_vars),
                               "Housing variables")
    ) |> 
  filter(!str_detect(term,"(Intercept)|Observation")) |>
  na.omit() |> 
  ggplot(aes(x = estimate, y = coef_names, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", linewidth = 1.25) +
  geom_point(aes(colour = model), size = 3.2,
             position = position_dodge(width = 0.4)) +
  scale_colour_viridis_d() +
  scale_shape_manual(values = c(15,17,18,19)) +
  facet_wrap(~housing_vars, ncol = 1, scales = "free_y") +
  force_panelsizes(rows = unit(c(3.5,12.5), "cm"), TRUE) +
  theme_bw() +
  labs(x = "Estimate", y = NULL, colour = "Model", shape = "Significance")

ggsave("viz/imputation_comparison_2021.png",
       units = "px",
       width = 3796,
       height = 2309)

write.csv(summary(pooled_results), "tables/mice_2021.csv")

######################################################################################
## PCA results ---------------------------------------------------------------------
##########################################################################################

rm(list = ls())

# helper function -----------------------------------------------------

select <- dplyr::select

# loading dataset ---------------------------------------------

dat <- readRDS("data/cross_sectional_dat_2021.RDS")

# missing observations --------------------------------

df_immi <- dat %>% 
  select(-tory_2019, -uni, -income,
         -full_time, -education_age, -log_hh, -disabled, -unemployed,
         -part_time, -log_age, 
         -region_fct, -e, -d, -c1, -c2, -b, -cohabiting) %>% 
  rename(LAD = la_code)

sum_na <- function(dat){
  out <- dat %>% 
    map_int(~sum(is.na(.)))
  return(out)
}

sum_na(df_immi)

missing_las <- function(df, x){
  step1 <- df %>% 
    mutate(nas = is.na({{x}})) %>% 
    filter(nas == T) %>% 
    count(LAD) %>% 
    arrange(desc(n))
  out <- step1$n
  names(out) <- step1$LAD
  return(out)
}

# missing affordability is Scotland and City of London
missing_las(df_immi, affordability)

# missing degree pct is Scotland
missing_las(df_immi, degree_pct)

# missing Non-UK born % is Scotland
missing_las(df_immi, non_uk_pct)

# removing scotland -------------------------------------------------------------

# removing NAs
df_eng_wales <- df_immi %>% drop_na(c(degree_pct, non_uk_pct))

df_eng_wales %>% sum_na()

df_immi <- df_immi %>% 
  na.omit()

full_n <- nrow(dat)
full_n
eng_wales_n <- nrow(df_eng_wales)
full_n - eng_wales_n # 2580 observations removed via Scotland

rm(df_eng_wales, df_immi)

# removing level 2 missing observations ------------------------------

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

df_full <- dat %>% 
  mutate(la_code = as.integer(as.factor(la_code))) |> 
  select(-tory_2019,
         -full_time, -education_age, -log_hh, -disabled, -unemployed,
         -part_time, -log_age, 
         -region_fct, -e, -d, -c1, -c2, -b, -cohabiting, #-edu_20plus,
         -rf_uni_preds, -pred_el_round, -income_knn, -income_full, -income_full_knn,
         -ta_rate, -churn_raw, -churn) %>% 
  rename(LAD = la_code)

sum_na(df_full)

level_2s <- df_full |> select(degree_pct, pop_density, pop_density_change,
                              social_rented_pct:under_16_pct,pc1, pc2) |> names()

df_full <- df_full |> drop_na(all_of(level_2s))

sub_ns <- nrow(df_full)
full_n - sub_ns # 2583 removed from level 2

#these_vars <- df_full %>% 
#  select(-income, -uni, -edu_20plus) %>% 
#  names()

# removing missing observations from DV -----------------------------------

sum(is.na(df_full$immigSelf)) # 2582 missing in DV

sum_na(df_full)

df_full <- df_full |> 
  drop_na(immigSelf)

no_missing_dv_n <- nrow(df_full)
no_missing_dv_n # 22948 obs

df_full <- df_full %>% 
  select(id:income,all_of(level_2s),pc1, pc2,immigSelf, region_code)

sub_ns - nrow(df_full) # confirmed that 2582 removed
final_n <- nrow(df_full)
final_n

sum_na(df_full)

# imputation ---------------------------------------------------------------------

df_full <- df_full %>% 
  mutate(social_housing.pc1 = social_housing * pc1,
         homeowner.pc2 = homeowner * pc2,
         region_code = as.factor(region_code))

# Keep only the variables needed for the imputation model
predictors <- df_full |> 
  select(-id, -age_raw, -LAD, -immigSelf, -income, -own_outright, -own_mortgage) |> 
  names()

vars_to_impute <- c("income", "LAD", "immigSelf", predictors)
df_subset <- df_full[, vars_to_impute]
df_subset |> sum_na()

# Initialize the MICE model 
init <- mice(df_subset, maxit = 0)
meth <- init$method
pred <- init$predictorMatrix

meth["income"] <- "2l.pan"

pred[,"LAD"] <- -2
pred["LAD","LAD"] <- 0
pred["income", "immigSelf"] <- 1
pred["income", predictors] <- 1
pred["income", "income"] <- 0
pred

# Run the multiple imputation
imp_mice <- mice(df_subset, method = meth, predictorMatrix = pred, m = 5, maxit = 5, seed = 123, printFlag = FALSE)

imp_mice$imp

# diagnostics ------------------------------------------------------------------

# ggmice income boxplot
ggmice(imp_mice, aes(x = .imp, y = income)) +
  geom_boxplot() +
  labs(x = "Imputation number")

# ggmice income density plot
ggmice(imp_mice, aes(x = income, group = .imp)) +
  geom_density()

# ggmice uni density plot
ggmice(imp_mice, aes(x = uni, group = .imp)) +
  geom_density()

# ggmice social housing density plot
ggmice(imp_mice, aes(x = social_housing, group = .imp)) +
  geom_density()

# ggmice social housing density plot
ggmice(imp_mice, aes(x = social_housing.pc1, group = .imp)) +
  geom_density()

# scaling ---------------------------------------------------------------------

imp_mice$imp$income <- as.data.frame(map_df(imp_mice$imp$income, scale_this))

# lmer -------------------------------------------------------------------------

# Fit the multilevel regression to each of the imputed datasets
models_fit <- with(data = imp_mice, exp = {
  lmer(immigSelf ~ social_housing + homeowner + private_renting +
         pc1 + pc2 +
         male + 
         white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
         no_religion + 
         age + income + uni +
         c1_c2 + d_e + non_uk_born + 
         non_uk_pct +
         over_65_pct + under_16_pct + 
         degree_pct +
         social_rented_pct +
         region_code +
         social_housing.pc1 + 
         homeowner.pc2 +
         (1|LAD))
})

# Pool the results from all the fitted lme4 models
pooled_results <- pool(models_fit)

# View the pooled summary
summary(pooled_results)

# comparison plot -------------------------------------------------------------

immi_pca <- readRDS("models/immi_pca_2021.RDS")
immi_pca <- tidy(immi_pca)
immi_pca <- immi_pca |> select(term, estimate, p.value) |> mutate(model = "EL")

coef_names <- tibble(
  term = unique(c(as.character(summary(pooled_results)$term),immi_pca$term)),
  coef_names = c("Intercept","Social housing","Homeowner","Private renting","PC1","PC2","Gender: Male",
                 "Ethnicity: White British","Ethnicity: White other","Ethnicity: Indian", "Ethnicity: Black", "Ethnicity: Chinese", "Ethnicity: Pakistan/Bangladesh","Ethnicity: Mixed-race",
                 "Religion: None","Age", "Household income", "Education: Degree", "Social class: C1-C2", "Social class: D-E",
                 "Birth country: UK", "Non-UK born %", "Over 65 %", "Under 16 %", "Degree educated %", "Social housing %",
                 "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "Wales",
                 "PC1 X Social housing", "PC2 X homeowner", "Household income", "Education: Degree", NA, NA)
)

summary(pooled_results) |> 
  as_tibble() |> 
  select(term, estimate, p.value) |> 
  mutate(model = "MICE") |> 
  bind_rows(immi_pca) |> 
  left_join(coef_names, by = "term") |> 
  mutate(
    sig = case_when(p.value < 0.001 ~ "<0.001",
                    p.value < 0.01 ~ "<0.01",
                    p.value < 0.05 ~ "<0.05",
                    .default = "Not Sig."),
    housing_vars = case_when(str_detect(coef_names, "housing$|Housing|Home|renting|home|PC1|PC2") ~ "Housing variables",
                             .default = "Controls"),
    housing_vars = fct_relevel(as.factor(housing_vars),
                               "Housing variables")
  ) |> 
  filter(!str_detect(term,"(Intercept)|Observation")) |>
  na.omit() |> 
  ggplot(aes(x = estimate, y = coef_names, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", linewidth = 1.25) +
  geom_point(aes(colour = model), size = 3.2,
             position = position_dodge(width = 0.4)) +
  scale_colour_viridis_d() +
  scale_shape_manual(values = c(15,17,18,19)) +
  facet_wrap(~housing_vars, ncol = 1, scales = "free_y") +
  force_panelsizes(rows = unit(c(3.75,12.25), "cm"), TRUE) +
  theme_bw() +
  labs(x = "Estimate", y = NULL, colour = "Model", shape = "Significance")

ggsave("viz/imputation_comparison_pca_2021.png",
       units = "px",
       width = 3796,
       height = 2309)

write.csv(summary(pooled_results), "tables/mice_pca_2021.csv")

