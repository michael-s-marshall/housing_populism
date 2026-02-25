pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance, margins, mice, broom.mixed, ggmice)

rm(list = ls())

# helper function -----------------------------------------------------

select <- dplyr::select

# loading dataset ---------------------------------------------

dat <- readRDS("data/cross_sectional_dat_2021.RDS")

##############################################################################
# immigself ------------------------------------------------------------------
##############################################################################

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

# removing NAs
df_eng_wales <- df_immi %>% drop_na(c(affordability, degree_pct, non_uk_pct))

df_eng_wales %>% map_int(~sum(is.na(.)))

df_immi <- df_immi %>% 
  na.omit()

1 - (nrow(df_immi) / nrow(df_eng_wales))

rm(df_eng_wales, df_immi)

# predictions from low income model ------------------------------

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

level_2s <- df_full |> select(degree_pct:ta_rate_full_raw) |> names()

full_ns <- nrow(df_full)

df_full <- df_full |> drop_na(all_of(level_2s))

full_ns - nrow(df_full)
sub_ns <- nrow(df_full)

these_vars <- df_full %>% 
  select(-income, -uni, -edu_20plus) %>% 
  names()

sum_na(df_full)

df_full <- df_full %>% 
  drop_na(all_of(these_vars))

sub_ns - nrow(df_full)

rm(full_ns, sub_ns)

df_full <- df_full %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability,
         region_code = as.factor(region_code))

sum_na(df_full)

# Assuming your outcome variable in 'dat' is named 'household_income'
# Keep only the variables needed for the imputation model
predictors <- df_full |> 
  select(male, white_british:d_e, social_housing:age, non_uk_born, 
         homeowner, degree_pct, affordability, pop_density, pop_density_change,
         social_rented_pct:under_16_pct, region_code, uni_full, social_housing.affordability:homeowner.affordability) |> 
  names()

vars_to_impute <- c("income", "LAD", "immigSelf", predictors)
df_subset <- df_full[, vars_to_impute]

df_subset |> sum_na()

# Initialize the MICE model without running it to extract the default method vector
init <- mice(df_subset, maxit = 0)
meth <- init$method
pred <- init$predictorMatrix

# Set the imputation method for household income to "norm" (Bayesian linear regression). 
# This treats the variable as continuous/normally distributed, which aligns with 
# the unrounded normal approach recommended by the article.
meth["income"] <- "2l.pan"

pred["income", "LAD"] <- -2
pred["income", "immigSelf"] <- 1
pred["income", predictors] <- 1
pred["income", "income"] <- 0

# Run the multiple imputation
# (m = 5 imputed datasets is a standard starting point, though you can increase this)
imp_mice <- mice(df_subset, method = meth, predictorMatrix = pred, m = 5, maxit = 5, seed = 123, printFlag = FALSE)

imp_mice$imp

# ggmice boxplot
ggmice(imp_mice, aes(x = .imp, y = income)) +
  geom_boxplot() +
  labs(x = "Imputation number")

# ggmice density plot
ggmice(imp_mice, aes(x = income, group = .imp)) +
  geom_density()

# lmer -------------------------------------------------------------------------

# Fit the multilevel regression to each of the imputed datasets
# We include 'household_income' as an independent variable, along with boilerplate variables
# 'group_var' represents your clustering/random effect variable
models_fit <- with(data = imp_mice, exp = {
  lmer(immigSelf ~ social_housing + homeowner + private_renting +
         affordability +
         male + 
         white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
         no_religion + 
         age + income + uni_full +
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

write.csv(summary(pooled_results), "tables/mice_2021.csv")

######################################################################################
## PCA results ---------------------------------------------------------------------
##########################################################################################

rm(list = ls())

# helper function -----------------------------------------------------

select <- dplyr::select

# loading dataset ---------------------------------------------

dat <- readRDS("data/cross_sectional_dat_2021.RDS")

##############################################################################
# immigself ------------------------------------------------------------------
##############################################################################

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

# removing NAs
df_eng_wales <- df_immi %>% drop_na(c(affordability, degree_pct, non_uk_pct))

df_eng_wales %>% map_int(~sum(is.na(.)))

df_immi <- df_immi %>% 
  na.omit()

1 - (nrow(df_immi) / nrow(df_eng_wales))

rm(df_eng_wales, df_immi)

# predictions from low income model ------------------------------

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

level_2s <- df_full |> select(degree_pct:ta_rate_full_raw) |> names()

full_ns <- nrow(df_full)

df_full <- df_full |> drop_na(all_of(level_2s))

full_ns - nrow(df_full)
sub_ns <- nrow(df_full)

these_vars <- df_full %>% 
  select(-income, -uni, -edu_20plus) %>% 
  names()

sum_na(df_full)

df_full <- df_full %>% 
  drop_na(all_of(these_vars))

sub_ns - nrow(df_full)

rm(full_ns, sub_ns)

df_full <- df_full %>% 
  mutate(social_housing.pc1 = social_housing * pc1,
         homeowner.pc2 = homeowner * pc2,
         region_code = as.factor(region_code))

sum_na(df_full)

# Assuming your outcome variable in 'dat' is named 'household_income'
# Keep only the variables needed for the imputation model
predictors <- df_full |> 
  select(male, white_british:d_e, social_housing:age, non_uk_born, 
         homeowner, degree_pct, pc1, pc2, pop_density, pop_density_change,
         social_rented_pct:under_16_pct, region_code, uni_full, social_housing.pc1:homeowner.pc2) |> 
  names()

vars_to_impute <- c("income", "LAD", "immigSelf", predictors)
df_subset <- df_full[, vars_to_impute]

df_subset |> sum_na()

# Initialize the MICE model without running it to extract the default method vector
init <- mice(df_subset, maxit = 0)
meth <- init$method
pred <- init$predictorMatrix

# Set the imputation method for household income to "norm" (Bayesian linear regression). 
# This treats the variable as continuous/normally distributed, which aligns with 
# the unrounded normal approach recommended by the article.
meth["income"] <- "2l.pan"

pred["income", "LAD"] <- -2
pred["income", "immigSelf"] <- 1
pred["income", predictors] <- 1
pred["income", "income"] <- 0

# Run the multiple imputation
# (m = 5 imputed datasets is a standard starting point, though you can increase this)
imp_mice <- mice(df_subset, method = meth, predictorMatrix = pred, m = 5, maxit = 5, seed = 123, printFlag = FALSE)

imp_mice$imp

# ggmice boxplot
ggmice(imp_mice, aes(x = .imp, y = income)) +
  geom_boxplot() +
  labs(x = "Imputation number")

# ggmice density plot
ggmice(imp_mice, aes(x = income, group = .imp)) +
  geom_density()

# lmer -------------------------------------------------------------------------

# Fit the multilevel regression to each of the imputed datasets
# We include 'household_income' as an independent variable, along with boilerplate variables
# 'group_var' represents your clustering/random effect variable
models_fit <- with(data = imp_mice, exp = {
  lmer(immigSelf ~ social_housing + homeowner + private_renting +
         pc1 + pc2 +
         male + 
         white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
         no_religion + 
         age + income + uni_full +
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

write.csv(summary(pooled_results), "tables/mice_pca_2021.csv")

imp_mice$nmis