pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance, marginaleffects, mice, broom.mixed, ggmice, mitml)

rm(list = ls())

# helper function -----------------------------------------------------

select <- dplyr::select

my_ggsave <- function(...){
  ggsave(...,
         units = "px",
         width = 3796,
         height = 2309)
}

pooled_summary <- function(mitml_obj){
  out <- as.data.frame(testEstimates(mitml_obj)$estimates[,1]) |> 
    rownames_to_column(var = "term") |> 
    bind_cols(confint.mitml.testEstimates(testEstimates(mitml_obj))) |> 
    rename(estimate = 2, conf.low = 3, conf.high = 4)
  return(out)
}

# loading dataset ---------------------------------------------

dat <- readRDS("data/modelling_dataset_2021.RDS")

# missing observations --------------------------------

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

# initialize the MICE model
init <- mice(dat, maxit = 0)
meth <- init$method
pred <- init$predictorMatrix

meth["income"] <- "2l.pan"
meth["uni"] <- "logreg"
meth["social_housing.affordability"]   <- "~ I(social_housing * affordability)"
meth["homeowner.affordability"]   <- "~ I(homeowner * affordability)"
meth["social_housing.pc1"]   <- "~ I(social_housing * pc1)"
meth["homeowner.pc2"]   <- "~ I(homeowner * pc2)"

pred[,"LAD"] <- -2
pred["LAD","LAD"] <- 0
pred["income", "immigSelf"] <- 1
pred["income", "income"] <- 0
pred["uni","LAD"] <- 0
pred["uni","disabled"] <- 0
pred["uni","part_time"] <- 0
pred["uni","full_time"] <- 0
pred["uni",]
pred["income",]

# multiple imputation
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

# converting to mitml class --------------------------------------------------

imp_mitml <- mids2mitml.list(imp_mice)

# lmer -------------------------------------------------------------------------

# NULL models
my_summ <- function(obj){
  out <- summ(obj, digits = 3, re.var = "var")
  return(out)
}

pooled_coefs_plot <- function(obj){
  obj |> 
    pool() |> 
    summary(conf.int = TRUE) |> 
    filter(term != "(Intercept)") |> 
    ggplot(aes(x = estimate, y = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.25, colour = "lightgrey") +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high), linewidth = 1.25) +
    geom_point(shape = 21, fill = "white", size = 3) +
    theme_bw() +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank()) +
    labs(x = "Estimate", y = NULL)
}

null_fit <- with(data = imp_mitml, {
  lmer(immigSelf ~ (1|LAD), REML = FALSE)
})

testEstimates(null_fit, extra.pars = TRUE)

# level 1 models -----------------------------------------------------------

lvl1_fit <- with(data = imp_mitml, {
  lmer(immigSelf ~ private_renting +
         male + 
         white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
         no_religion + 
         age + income + uni +
         c1_c2 + d_e + non_uk_born +
         (social_housing * affordability) + 
         (homeowner * affordability) +
         (1|LAD), REML = FALSE)
})

testEstimates(lvl1_fit, extra.pars = TRUE)

testModels(lvl1_fit, null_fit, method = "D4")

# lvl2 fits ----------------------------------------------------

# level 2 models
lvl2_fit <- with(data = imp_mitml, {
  lmer(immigSelf ~ private_renting +
         male + 
         white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
         no_religion + 
         age + income + uni +
         c1_c2 + d_e + non_uk_born +
         non_uk_pct + pop_density + pop_density_change +
         over_65_pct + under_16_pct + 
         degree_pct +
         social_rented_pct +
         (social_housing * affordability) + 
         (homeowner * affordability) +
         (1|LAD), REML = FALSE)
})

testEstimates(lvl2_fit, extra.pars = TRUE)

testModels(lvl2_fit, null_fit, method = "D4")

anova.mitml.result(lvl2_fit, lvl1_fit, method = "D3")

# region fixed effects models --------------------------------------------------

# models including region fixed effects
reg_fit <- with(data = imp_mitml, {
  lmer(immigSelf ~ private_renting +
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
         (1|LAD), REML = FALSE)
})

testEstimates(reg_fit, extra.pars = TRUE)

confint.mitml.testEstimates(testEstimates(reg_fit))

anova.mitml.result(reg_fit, lvl2_fit, method = "D3")

# removing social housing interaction and testing anova -------------------------

home_only <- with(imp_mitml, {
  lmer(immigSelf ~ private_renting +
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
         social_housing + 
         (homeowner * affordability) +
         (1|LAD), REML = FALSE)
})

anova.mitml.result(home_only, reg_fit, method = "D3")

# removing degree pct -----------------------------------------------------------------

deg_fit <- with(data = imp_mitml, {
  lmer(immigSelf ~ private_renting +
         male + 
         white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
         no_religion + 
         age + income + uni +
         c1_c2 + d_e + non_uk_born + 
         non_uk_pct + pop_density + pop_density_change +
         over_65_pct + under_16_pct + 
         #degree_pct +
         social_rented_pct +
         region_code +
         (social_housing * affordability) + 
         (homeowner * affordability) +
         (1|LAD), REML = FALSE)
})

testEstimates(deg_fit, extra.pars = TRUE)

confint.mitml.testEstimates(testEstimates(deg_fit))

## PCA results ---------------------------------------------------------------------

pca_fit <- with(data = imp_mitml, {
  lmer(immigSelf ~ private_renting +
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
         (1|LAD), REML = FALSE)
})

testEstimates(pca_fit, extra.pars = TRUE)
confint.mitml.testEstimates(testEstimates(pca_fit))
anova.mitml.result(reg_fit, pca_fit, method = "D3")
testModels(pca_fit, null_fit, method = "D3")

saveRDS(reg_fit, "models/reg_fit_2021.RDS")
saveRDS(pca_fit, "models/pca_fit_2021.RDS")

# robustness check - checking linearity ---------------------------------

imp_binned <- within(imp_mitml, {
  afford_tert <- cut_number(affordability, n = 3, labels = c("1","2","3"))
  second_afford <- case_when(afford_tert == "2" ~ 1, .default = 0)
  third_afford <- case_when(afford_tert == "3" ~ 1, .default = 0)
  
  pc1_tert <- cut_number(pc1, n = 3, labels = c("1","2","3"))
  second_pc1 <- case_when(pc1_tert == "2" ~ 1, .default = 0)
  third_pc1 <- case_when(pc1_tert == "3" ~ 1, .default = 0)
  
  pc2_tert <- cut_number(pc2, n = 3, labels = c("1","2","3"))
  second_pc2 <- case_when(pc2_tert == "2" ~ 1, .default = 0)
  third_pc2 <- case_when(pc2_tert == "3" ~ 1, .default = 0)
})

aft_mod <- with(data = imp_binned, {
  lmer(immigSelf ~ private_renting +
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
         (social_housing * second_afford) +
         (social_housing * third_afford) +
         (homeowner * second_afford) +
         (homeowner * third_afford) +
         (1|LAD), REML = FALSE)
})

testEstimates(aft_mod)

pct_mod <- with(data = imp_binned, {
  lmer(immigSelf ~ private_renting +
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
         (social_housing * second_pc1) +
         (social_housing * third_pc1) +
         (homeowner * second_pc2) +
         (homeowner * third_pc2) +
         (1|LAD), REML = FALSE)
})

testEstimates(pct_mod)

# predictions ------------------------------------------------------------

afford_quantiles <- seq(min(dat$affordability),max(dat$affordability),((max(dat$affordability)-min(dat$affordability))/10))

# avg predictions for homeownership
grid_vals <- datagrid(
  model = reg_fit[[1]], 
  affordability = afford_quantiles, 
  homeowner = unique 
)

home_pred_list <- map(reg_fit, function(m) {
  avg_predictions(m,
                  by = c("homeowner","affordability"),
                  newdata = grid_vals)
  
})

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

pooled_results <- my_pool(home_pred_list, c(homeowner, affordability))

viridis_scale <- viridis::viridis(n = 4)[2:3]

h_plot <- ggplot(pooled_results) +
  geom_line(aes(x = affordability, y = .estimate, 
                color = as.factor(homeowner)),
            linewidth = 1) +
  geom_ribbon(aes(x = affordability,
                  fill = as.factor(homeowner),
                  ymin = conf.low, ymax = conf.high),
              alpha = 0.2, color = NA) +
  geom_rug(data = dat, aes(x = affordability), alpha = 0.4) +
  labs(y = "Predicted outcome: Opposition to migration", x = "Affordability (standardised)") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_colour_manual(values = viridis_scale) +
  scale_fill_manual(values = viridis_scale) +
  coord_cartesian(ylim = c(5,9.25)) +
  labs(colour = "Homeowner", fill = "Homeowner")

grid_vals <- datagrid(
  model = reg_fit[[1]], 
  affordability = afford_quantiles, 
  social_housing = unique 
)

sohs_pred_list <- map(reg_fit, function(m) {
  avg_predictions(m,
                  by = c("social_housing","affordability"),
                  newdata = grid_vals)
  
})

pooled_soc_ho <- my_pool(sohs_pred_list, c(social_housing, affordability))

s_plot <- ggplot(pooled_soc_ho) +
  geom_line(aes(x = affordability, y = .estimate, 
                color = as.factor(social_housing)),
            linewidth = 1) +
  geom_ribbon(aes(x = affordability,
                  fill = as.factor(social_housing),
                  ymin = conf.low, ymax = conf.high),
              alpha = 0.2, color = NA) +
  geom_rug(data = dat, aes(x = affordability), alpha = 0.4) +
  labs(y = "Predicted outcome: Opposition to migration", x = "Affordability (standardised)") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  coord_cartesian(ylim = c(5,9.25)) +
  labs(colour = "Social housing", fill = "Social housing")

pacman::p_load(patchwork)

h_plot + s_plot + plot_layout(axis_titles = "collect")

my_ggsave(filename = "viz/predicted_outcome_reg_fit_2021.png")

# moderation effect ------------------------------------------------------------

comp_home <- map(reg_fit, function(m) {
  avg_comparisons(m,
                  variables = "homeowner", 
                  by = "affordability",
                  newdata = datagrid(affordability = afford_quantiles))
})

# moderation effect
comp_sohs <- map(reg_fit, function(m) {
  avg_comparisons(m,
                  variables = "social_housing", 
                  by = "affordability",        
                  newdata = datagrid(affordability = afford_quantiles))
})

comp_home |> 
  my_pool(affordability) |> 
  bind_rows(my_pool(comp_sohs, affordability), .id = "tenure") |> 
  mutate(tenure = case_when(tenure == "1" ~ "Homeowner",
                            .default = "Social housing")) |> 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "lightgrey", linewidth = 1.2) +
  geom_rug(data = dat, aes(x = affordability), colour = "black", alpha = 0.4) +
  geom_ribbon(aes(x = affordability, ymin = conf.low, ymax = conf.high, fill = tenure), alpha = 0.25) +
  geom_line(aes(x = affordability, y = .estimate, colour = tenure), linewidth = 1.25)  +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Affordability (standardised)", y = "Estimate: Opposition to migration", colour = "Tenure", fill = "Tenure")

my_ggsave(filename = "viz/AME_moderation_reg_fit_2021.png")

# joint coef plot ---------------------------------------------------------------------

pooled_reg_summary <- pooled_summary(reg_fit)
pooled_pca_summary <- pooled_summary(pca_fit)

plot_estimates <- pooled_reg_summary |> 
  bind_rows(pooled_pca_summary,
            .id = "Model") |> 
  filter(str_detect(term, "renting|housing|home|affordability|^pc"))

plot_estimates$coef_names <- c("Private renting",
                               "Social housing",
                               "Affordability",
                               "Homeowner",
                               "Social housing X Affordability",
                               "Homeowner X Affordability",
                               "Private renting",
                               "Social housing",
                               "PC1",
                               "Homeowner",
                               "PC2",
                               "Social housing X PC1",
                               "Homeowner X PC2")

plot_estimates |> 
  ggplot(aes(x = estimate, y = coef_names, colour = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey", linewidth = 1.2) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), 
                 linewidth = 1.25,
                 position = position_dodge(width = 0.4)) +
  geom_point(shape = 21, fill = "white", 
             size = 3,
             position = position_dodge(width = 0.4)) +
  labs(x = "Estimate: Opposition to migration", y = NULL) +
  scale_colour_viridis_d() +
  scale_x_continuous(breaks = seq(-0.25,0.75,0.25)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank())

my_ggsave("viz/coef_plot_2021.png")

# bootstrap confints region models -----------------------------------------------------------

# refitting to mice object
reg_fit_mice <- with(data = imp_mice, exp = {
  lmer(immigSelf ~ private_renting +
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
         (1|LAD), REML = FALSE)
})

reg_params <- c("social_housing", "homeowner", "affordability", "affordability:homeowner", "social_housing:affordability", "private_renting")
# function for extraction of fixed effects
get_fixed_effects <- function(model) {
  target_params <- c("social_housing", "homeowner", "affordability", "affordability:homeowner", "social_housing:affordability", "private_renting")
  ests <- lme4::fixef(model)
  return(ests[target_params])
}

# number of cores for parallel processing
num_cores <- detectCores() - 1
my_seeds <- c(101, 102, 103, 104, 105) # seeds for parallel

start_time <- Sys.time()
ci_reg <- map2(getfit(reg_fit_mice), my_seeds, function(m, current_seed) {
  bootMer(m,
          FUN = get_fixed_effects,
          nsim = 500,
          use.u = FALSE,
          type = "parametric",
          parallel = "snow",
          ncpus = num_cores,
          seed = current_seed)
})

end_time <- Sys.time()
end_time - start_time

# extract the 't' matrix from each dataset and bind them into one dataframe
pooled_ci_reg <- map_dfr(ci_reg, ~ as.data.frame(.x$t))
colnames(pooled_ci_reg) <- reg_params

# calculate the 95% Confidence Intervals using the Percentile method
reg_cis <- map_df(pooled_ci_reg, function(x) {
  quantile(x, probs = c(0.025, 0.5, 0.975))
}) |> 
  mutate(term = colnames(pooled_ci_reg), .before = 1) |> 
  rename(estimate = `50%`, `2.5 %` = `2.5%`, `97.5 %` = `97.5%`)

# pooled estimate and wald intervals for comparison
pooled_reg <- pool(reg_fit_mice)

wald_reg <- summary(pooled_reg, conf.int = TRUE) |> 
  filter(term %in% colnames(pooled_ci_reg)) |> 
  select(term, estimate, `2.5 %`, `97.5 %`)

# plotting
reg_cis |>
  bind_rows(wald_reg, .id = "method") |> 
  mutate(Method = case_when(method == "1" ~ "Bootstrap", .default = "Wald")) |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.2, colour = "grey") +
  geom_linerange(aes(xmin = `2.5 %`, xmax = `97.5 %`, 
                     y = term,
                     colour = Method),
                 linewidth = 1.2,
                 position = position_dodge(width = 0.2)) +
  geom_point(aes(x = estimate, y = term, colour = Method),
             shape = 21, size = 3, fill = "white",
             position = position_dodge(width = 0.2)) +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Estimate", y = NULL,
       caption = "Comparison of confidence intervals by method for Model 1.") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0,
                                    size = 12))

my_ggsave("viz/ci_comparison_reg_fit_2021.png")

saveRDS(ci_reg, "models/ci_reg_2021.RDS")

# bootstrap confints PCA models -----------------------------------------------------------

# refitting to mice object
pca_fit_mice <- with(data = imp_mice, exp = {
  lmer(immigSelf ~ private_renting +
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
         (1|LAD), REML = FALSE)
})


pca_params <- c("social_housing", "homeowner", "pc1", "pc2", "homeowner:pc2", "social_housing:pc1", "private_renting")
# function for extraction of fixed effects
get_fixed_effects <- function(model) {
  target_params <- c("social_housing", "homeowner", "pc1", "pc2", "homeowner:pc2", "social_housing:pc1", "private_renting")
  ests <- lme4::fixef(model)
  return(ests[target_params])
}

start_time <- Sys.time()
ci_pca <- map2(getfit(pca_fit_mice), my_seeds, function(m, current_seed) {
  bootMer(m,
          FUN = get_fixed_effects,
          nsim = 500,
          use.u = FALSE,
          type = "parametric",
          parallel = "snow",
          ncpus = num_cores,
          seed = current_seed)
})

end_time <- Sys.time()
end_time - start_time

# extract the 't' matrix from each dataset and bind them into one dataframe
pooled_ci_pca <- map_dfr(ci_pca, ~ as.data.frame(.x$t))
colnames(pooled_ci_pca) <- pca_params

# calculate the 95% Confidence Intervals using the Percentile method
pca_cis <- map_df(pooled_ci_pca, function(x) {
  quantile(x, probs = c(0.025, 0.5, 0.975))
}) |> 
  mutate(term = colnames(pooled_ci_pca), .before = 1) |> 
  rename(estimate = `50%`, `2.5 %` = `2.5%`, `97.5 %` = `97.5%`)

# pooled estimate and wald intervals for comparison
pooled_pca <- pool(pca_fit_mice)

wald_pca <- summary(pooled_pca, conf.int = TRUE) |> 
  filter(term %in% colnames(pooled_ci_pca)) |> 
  select(term, estimate, `2.5 %`, `97.5 %`)

# plotting
pca_cis |>
  bind_rows(wald_pca, .id = "method") |> 
  mutate(Method = case_when(method == "1" ~ "Bootstrap", .default = "Wald")) |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.2, colour = "grey") +
  geom_linerange(aes(xmin = `2.5 %`, xmax = `97.5 %`, 
                     y = term,
                     colour = Method),
                 linewidth = 1.2,
                 position = position_dodge(width = 0.2)) +
  geom_point(aes(x = estimate, y = term, colour = Method),
             shape = 21, size = 3, fill = "white",
             position = position_dodge(width = 0.2)) +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Estimate", y = NULL,
       caption = "Comparison of confidence intervals by method for Model 2.") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0,
                                    size = 12))

my_ggsave("viz/ci_comparison_pca_fit_2021.png")

saveRDS(ci_pca, "models/ci_pca_2021.RDS")
