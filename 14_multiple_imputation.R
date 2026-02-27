pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance, marginaleffects, mice, broom.mixed, ggmice)

rm(list = ls())

# helper function -----------------------------------------------------

select <- dplyr::select

# loading dataset ---------------------------------------------

dat <- readRDS("data/cross_sectional_dat_2024.RDS")

# missing observations --------------------------------

df_immi <- dat %>% 
  select(-tory_2019, -uni, -income,
         -full_time, -education_age, -log_hh, -disabled, -unemployed,
         -part_time, -log_age, -churn, -churn_raw,
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
full_n - eng_wales_n # 2998 observations removed via Scotland

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
         -el_uni_preds, -pred_el_round, -income_knn, -income_full, -income_full_knn,
         -ta_rate, -churn_raw, -churn) %>% 
  rename(LAD = la_code)

sum_na(df_full)

level_2s <- df_full |> select(degree_pct:underoccupied_pct_raw,ta_rate_full_raw) |> names()

df_full <- df_full |> drop_na(all_of(level_2s))

sub_ns <- nrow(df_full)
full_n - sub_ns # 2998 removed from level 2

#these_vars <- df_full %>% 
#  select(-income, -uni, -edu_20plus) %>% 
#  names()

# removing missing observations from DV -----------------------------------

sum(is.na(df_full$brexit_party)) # 311 missing in DV

sum_na(df_full)

df_full <- df_full |> 
  drop_na(brexit_party)

no_missing_dv_n <- nrow(df_full)
no_missing_dv_n # 27789 obs

df_full <- df_full %>% 
  select(id:rent, brexit_party)

sub_ns - nrow(df_full) # confirmed that 311 removed
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
  select(-id, -age_raw, -LAD, -brexit_party, -income, -own_outright, -own_mortgage,
         -prices, -affordability_log, -private_rented_pct, -rent) |> 
  names()

vars_to_impute <- c("income", "LAD", "brexit_party", predictors)
df_subset <- df_full[, vars_to_impute]
df_subset |> sum_na()

# Initialize the MICE model
init <- mice(df_subset, maxit = 0)
meth <- init$method
pred <- init$predictorMatrix

meth["income"] <- "2l.pan"

pred[,"LAD"] <- -2
pred["LAD","LAD"] <- 0
pred["income", "brexit_party"] <- 1
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

# glmer -------------------------------------------------------------------------

# Fit the multilevel regression to each of the imputed datasets
models_fit <- with(data = imp_mice, exp = {
  glmer(brexit_party ~ private_renting +
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
          (social_housing * affordability) +
          (homeowner * affordability) +
          (1|LAD),
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa"))
})

# Pool the results from all the fitted lme4 models
pooled_results <- pool(models_fit)

# View the pooled summary
pooled_summ <- summary(pooled_results, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)

# comparison --------------------------------------------------------------------

immi_reg <- readRDS("models/immi_reg_2024.RDS")
#immi_reg <- read_csv("models/immi_reg_2021_coefficients.csv")
#immi_reg <- immi_reg |> select(term, estimate, p.value) |> mutate(model = "EL")

immi_conf <- immi_reg |> 
  tidy(conf.int = TRUE,
       exponentiate = TRUE,
       effects = "fixed"#,
       #conf.method = "profile"
       ) |> 
  mutate(term = case_when(term == "social_housing.affordability" ~ "social_housing:affordability",
                          term == "homeowner.affordability" ~ "affordability:homeowner",
                          .default = term)
         )

coef_names <- tibble(
  term = unique(c(as.character(pooled_summ$term),immi_conf$term)),
  coef_names = c("Intercept","Private renting",
                 "Gender: Male",
                 "Ethnicity: White British","Ethnicity: White other","Ethnicity: Indian", "Ethnicity: Black", "Ethnicity: Chinese", "Ethnicity: Pakistan/Bangladesh","Ethnicity: Mixed-race",
                 "Religion: None","Age", "Household income", "Education: Degree", "Social class: C1-C2", "Social class: D-E",
                 "Birth country: UK", "Non-UK born %", "Over 65 %", "Under 16 %", "Degree educated %", "Social housing %",
                 "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "Wales",
                 "Social housing", "Affordability ratio", "Homeowner", 
                 "Affordability ratio X Social housing", "Affordability ratio X Homeowner", "Household income", "Education: Degree", "Population density")
)

pacman::p_load(ggh4x)

pooled_summ |> 
  as_tibble() |> 
  select(term, estimate, p.value, conf.low, conf.high) |> 
  mutate(model = "MICE") |> 
  bind_rows(immi_conf |> select(term, estimate, p.value, conf.low, conf.high) |> mutate(model = "EL")) |> 
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
  #filter(!str_detect(term,"(Intercept)|Observation")) |>
  #na.omit() |> 
  ggplot(aes(x = estimate, y = coef_names)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "lightgrey", linewidth = 1.25) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high, colour = model), linewidth = 1,
                 position = position_dodge(width = 0.4)) +
  geom_point(aes(colour = model, shape = sig), size = 3.2,
             position = position_dodge(width = 0.4)) +
  scale_colour_viridis_d() +
  scale_shape_manual(values = c(15,17,18,19)) +
  facet_wrap(~housing_vars, ncol = 1, scales = "free_y") +
  force_panelsizes(rows = unit(c(3.5,12.5), "cm"), TRUE) +
  theme_bw() +
  labs(x = "Estimate", y = NULL, colour = "Model", shape = "Significance")

#ggsave("viz/imputation_comparison_2021.png",
#       units = "px",
#       width = 3796,
#       height = 2309)
#
#write.csv(summary(pooled_results), "tables/mice_2021.csv")

# marginal effects -----------------------------------------------------------------

# extract the models
model_list <- getfit(models_fit)

# calculate the Odds Ratio for social_housing at specific affordability levels
hist(df_subset$affordability)
afford_quantiles <- quantile(df_subset$affordability, seq(0.1, 0.9, 0.1))

# AMEs for social renters
mfx_list <- lapply(model_list, function(m) {
  avg_slopes(m,
             variables = "social_housing",
             by = "affordability",
             newdata = datagrid(affordability = afford_quantiles, grid_type = "counterfactual"),
             type = "response")       # AMEs
  
})

# pooling estimates
# see for Rubin's rules: https://bookdown.org/mwheymans/bookmi/rubins-rules.html
mfx_df <- bind_rows(lapply(mfx_list, as.data.frame), .id = "imputation")
pooled_ame_data <- mfx_df %>%
  group_by(affordability) %>%
  summarise(
    m = n(),                                     # Number of imputations (e.g., 5)
    Q_bar = mean(estimate),                      # Pooled point estimate
    U_bar = mean(std.error^2),                   # Within-imputation variance
    B = var(estimate),                           # Between-imputation variance
    T_var = U_bar + B + (B / m),                 # Total variance
    se_pooled = sqrt(T_var),                     # Pooled standard error
    conf.low = Q_bar - 1.96 * se_pooled,         # 95% CI Lower Bound
    conf.high = Q_bar + 1.96 * se_pooled,        # 95% CI Upper Bound
    .groups = "drop"
  )

# AMEs for homeowners
mfx_home <- lapply(model_list, function(m) {
  avg_slopes(m,
             variables = "homeowner",
             by = "affordability",
             newdata = datagrid(affordability = afford_quantiles, grid_type = "counterfactual"),
             type = "response")       # AMEs
  
})

# pooling estimates
mfx_home_df <- bind_rows(lapply(mfx_home, as.data.frame), .id = "imputation")
pooled_ame_home <- mfx_home_df %>%
  group_by(affordability) %>%
  summarise(
    m = n(),                                     # Number of imputations (e.g., 5)
    Q_bar = mean(estimate),                      # Pooled point estimate
    U_bar = mean(std.error^2),                   # Within-imputation variance
    B = var(estimate),                           # Between-imputation variance
    T_var = U_bar + B + (B / m),                 # Total variance
    se_pooled = sqrt(T_var),                     # Pooled standard error
    conf.low = Q_bar - 1.96 * se_pooled,         # 95% CI Lower Bound
    conf.high = Q_bar + 1.96 * se_pooled,        # 95% CI Upper Bound
    .groups = "drop"
  ) |> 
  mutate(tenure = "Homeowner")


# 3. Create the marginal effects plot using ggplot2
pooled_plot_data |> 
  mutate(tenure = "Social Housing") |> 
  bind_rows(pooled_ame_home) |> 
  ggplot(aes(x = affordability, y = Q_bar, fill = tenure)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(colour = tenure), linewidth = 1) +
  geom_point(aes(colour = tenure), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.2) +
  labs(
    x = "Affordability (Standardised)",
    y = "Average Marginal Effect",
    colour = "Tenure", fill = "Tenure"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()
