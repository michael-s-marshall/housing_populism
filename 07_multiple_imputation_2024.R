pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance, marginaleffects, mice, broom.mixed, ggmice)

rm(list = ls())

# helper function -----------------------------------------------------

select <- dplyr::select

# loading dataset ---------------------------------------------

dat <- readRDS("data/modelling_dataset_2024.RDS")

# missing observations ----------------------------------------------------

sum_na <- function(dat){
  out <- dat %>% 
    map_int(~sum(is.na(.)))
  return(out)
}

dat |> sum_na()

unselect <- dat |> select(contains("raw"), id) |> names()

dat <- dat |> 
  select(-all_of(unselect), -affordability_log, -prices) |> 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability,
         social_housing.pc1 = social_housing * pc1,
         homeowner.pc2 = homeowner * pc2,
         region_code = as.factor(region_code),
         LAD = as.integer(as.factor(LAD)))

sum_na(dat)

# imputation ---------------------------------------------------------------------

# Initialize the MICE model
init <- mice(dat, maxit = 0)
meth <- init$method
pred <- init$predictorMatrix

meth["income"] <- "2l.pan"

pred[,"LAD"] <- -2
pred["LAD","LAD"] <- 0
pred["income", "brexit_party"] <- 1
pred["income", "income"] <- 0
pred

# Run the multiple imputation
imp_mice <- mice(dat, method = meth, predictorMatrix = pred, m = 5, maxit = 5, seed = 123, printFlag = FALSE)

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

ggmice(imp_mice, aes(x = homeowner.affordability, group = .imp)) +
  geom_density()

ggmice(imp_mice, aes(x = social_housing.pc1, group = .imp)) +
  geom_density()

ggmice(imp_mice, aes(x = homeowner.pc2, group = .imp)) +
  geom_density()

ggmice(imp_mice, aes(x = homeowner, group = .imp)) +
  geom_density()

ggmice(imp_mice, aes(x = social_housing, group = .imp)) +
  geom_density()

ggmice(imp_mice, aes(x = affordability)) +
  geom_density()

# glmer -------------------------------------------------------------------------

start_time <- Sys.time()

nulls_fit <- with(data = imp_mice, exp = {
  glmer(brexit_party ~ (1|LAD),
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa"))
})

summary(getfit(nulls_fit)[[1]])

saveRDS(nulls_fit, file = "models/null_models_mice_2024.RDS")

# fitting the regions inclusive model to each of the imputed datasets
reg_fit <- with(data = imp_mice, exp = {
  glmer(brexit_party ~ private_renting +
          male +
          white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race +
          no_religion +
          age + income + uni +
          c1_c2 + d_e + non_uk_born +
          non_uk_pct + pop_density + pop_density_change +
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

end_time <- Sys.time()
end_time - start_time

saveRDS(reg_fit, file = "models/reg_fit_2024.RDS")

# pooled models
pooled_reg <- pool(reg_fit)

# pooled odds ratios with cis (wald)
reg_summary <- summary(pooled_reg, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)
reg_summary

# marginal effects -----------------------------------------------------------------

# function for pooling AMEs
# see for Rubin's rules: https://bookdown.org/mwheymans/bookmi/rubins-rules.html
my_pool <- function(obj_list, group_vars){
  out <- bind_rows(obj_list, .id = "imp") %>%
    group_by(across({{group_vars}})) %>% 
    summarise(
      .estimate = mean(estimate),
      var_within = mean(std.error^2),
      var_between = var(estimate),
      std.error = sqrt(var_within + (1 + 1/n()) * var_between),
      conf.low = .estimate - 1.96 * std.error,
      conf.high = .estimate + 1.96 * std.error,
      .groups = "drop"
    )
  return(out)
}

# extract the models
reg_models <- getfit(reg_fit)

afford_quantiles <- seq(min(dat$affordability),max(dat$affordability),((max(dat$affordability)-min(dat$affordability))/10))

# AMEs for social renters
mfx_sohs <- map(reg_models, function(m) {
  avg_slopes(m,
             variables = "social_housing",
             by = "affordability",
             newdata = datagrid(affordability = afford_quantiles, grid_type = "counterfactual"),
             type = "response")   
})

# pooling AMEs for social renters
pooled_ame_sohs <- map(mfx_sohs, as.data.frame) |> 
  my_pool(affordability)

# AMEs for homeowners
mfx_home <- map(reg_models, function(m) {
  avg_slopes(m,
             variables = "homeowner",
             by = "affordability",
             newdata = datagrid(affordability = afford_quantiles, grid_type = "counterfactual"),
             type = "response")
})

# pooling AMEs for homeowners
pooled_ame_home <- map(mfx_home, as.data.frame) |> 
  my_pool(affordability)

# plot of marginal effects
pooled_ame_sohs |> 
  bind_rows(pooled_ame_home, .id = "tenure") |>
  mutate(tenure = case_when(tenure == "1" ~ "Social housing", .default = "Homeowner")) |> 
  ggplot(aes()) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, x = affordability, fill = tenure), alpha = 0.2) +
  geom_line(aes(x = affordability, y = .estimate, colour = tenure), linewidth = 1) +
  #geom_point(aes(colour = tenure), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.2) +
  geom_rug(data = dat, aes(x = affordability), alpha = 0.4) +
  labs(
    x = "Affordability (Standardised)",
    y = "Average Marginal Effect",
    colour = "Tenure", fill = "Tenure"
  ) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()

saveRDS(mfx_home, file = "models/AMEs_homeowner_affordability_2024.RDS")
saveRDS(mfx_sohs, file = "models/AMEs_social_affordability_2024.RDS")

# PCA model -------------------------------------------------------------------

start_time <- Sys.time()

pca_fit <- with(imp_mice, exp = {
  glmer(brexit_party ~ private_renting +
          male +
          white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race +
          no_religion +
          age + income + uni +
          c1_c2 + d_e + non_uk_born +
          non_uk_pct + pop_density + pop_density_change +
          over_65_pct + under_16_pct +
          degree_pct +
          social_rented_pct +
          region_code +
          (social_housing * pc1) +
          (homeowner * pc2) +
          (1|LAD),
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa"))
})

end_time <- Sys.time()
end_time - start_time

saveRDS(pca_fit, file = "models/pca_fit_2024.RDS")

# pooled models
pooled_pca <- pool(pca_fit)

# pooled odds ratios with cis (wald)
pca_summary <- summary(pooled_pca, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)
pca_summary

# PCA AMEs --------------------------------------------------------

# extract the PCA models
pca_models <- getfit(pca_fit)

# calculate the Odds Ratio for social_housing at specific affordability levels
pc1_quantiles <- seq(min(dat$pc1),max(dat$pc1),((max(dat$pc1)-min(dat$pc1))/10))
pc2_quantiles <- seq(min(dat$pc2),max(dat$pc2),((max(dat$pc2)-min(dat$pc2))/10))

# AMEs for social renters
mfx_pc1 <- map(pca_models, function(m) {
  avg_slopes(m,
             variables = "social_housing",
             by = "pc1",
             newdata = datagrid(pc1 = pc1_quantiles, grid_type = "counterfactual"),
             type = "response")
  
})

# pooling AMEs for social renters X pc1
pooled_ame_pc1 <- map(mfx_pc1, as.data.frame) |> 
  my_pool(pc1)

# AMEs for homeowners
mfx_pc2 <- map(pca_models, function(m) {
  avg_slopes(m,
             variables = "homeowner",
             by = "pc2",
             newdata = datagrid(pc2 = pc2_quantiles, grid_type = "counterfactual"),
             type = "response")
  
})

# pooling estimates
# pooling AMEs for social renters X pc1
pooled_ame_pc2 <- map(mfx_pc2, as.data.frame) |> 
  my_pool(pc2)

pooled_ame_pc1 |> 
  ggplot(aes(x = pc1, y = .estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.2) +
  labs(
    x = "PC1 (Standardised)",
    y = "Average Marginal Effect",
    title = "Social housing"
  ) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) 

pooled_ame_pc2 |> 
  ggplot(aes(x = pc2, y = .estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.2) +
  labs(
    x = "PC2 (Standardised)",
    y = "Average Marginal Effect",
    title = "Homeowner"
  ) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) 

saveRDS(mfx_pc1, file = "models/AMEs_social_pc1_2024.RDS")
saveRDS(mfx_pc2, file = "models/AMEs_homeowner_pc2_2024.RDS")

# confidence intervals --------------------------------------------------------

start_time <- Sys.time()
set.seed(123)
ci_reg <- map(reg_models, function(m) {
  confint(m,
          #parm = "social_housing",
          parm = c("social_housing","homeowner","affordability","affordability:homeowner","social_housing:affordability","private_renting"),
          method = "profile")}
)

end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
set.seed(123)
ci_pca <- map(pca_models, function(m) {
  confint(m,
          #parm = "social_housing",
          parm = c("social_housing","homeowner","pc2","homeowner:pc2","social_housing:pc1","pc1","private_renting"),
          method = "profile")}
)

end_time <- Sys.time()
end_time - start_time

saveRDS(ci_reg, "models/ci_reg_2024.RDS")
saveRDS(ci_pca, "models/ci_pca_2024.RDS")