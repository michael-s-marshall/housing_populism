pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance, marginaleffects, mice, broom.mixed, ggmice)

rm(list = ls())

# helper function -----------------------------------------------------

select <- dplyr::select

my_ggsave <- function(...){
  ggsave(...,
         units = "px",
         width = 3796,
         height = 2309)
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

pred[,"LAD"] <- -2
pred["LAD","LAD"] <- 0
pred["income", "immigSelf"] <- 1
pred["income", "income"] <- 0
pred

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

null_fit <- with(data = imp_mice, exp = {
  lmer(immigSelf ~ (1|LAD), REML = FALSE)
})

pooled_null <- pool(null_fit)

null_fit$analyses[[1]] |> my_summ()

# level 1 models
lvl1_fit <- with(data = imp_mice, exp = {
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

lvl1_fit |> 
  pool() |> 
  summary(conf.int = TRUE)

pooled_coefs_plot(lvl1_fit)

# lvl2 fits ----------------------------------------------------

# level 2 models
lvl2_fit <- with(data = imp_mice, exp = {
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

lvl2_fit |> 
  pool() |> 
  summary(conf.int = TRUE)

pooled_coefs_plot(lvl2_fit)

mice_anova <- function(mod1, mod2){
  out <- map2(.x = mod1[["analyses"]],
              .y = mod2[["analyses"]],
              .f = anova)
  return(out)
}

mice_anova(lvl1_fit, lvl2_fit)

# region fixed effects models --------------------------------------------------

# models including region fixed effects
reg_fit <- with(data = imp_mice, exp = {
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

# pooling models
pooled_reg <- pool(reg_fit)

# summary of pooled models
pooled_reg_summary <- summary(pooled_reg, conf.int = TRUE)

pooled_coefs_plot(reg_fit)

mice_anova(lvl2_fit, reg_fit)

# removing homeownership interaction and testing anova -------------------------

home_only <- with(imp_mice, exp = {
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

mice_anova(home_only, reg_fit)

##########################################################################################
## PCA results ---------------------------------------------------------------------
##########################################################################################

pca_fit <- with(data = imp_mice, exp = {
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

# pooling
pooled_pca <- pool(pca_fit)

# pooled summary
pooled_pca_summary <- summary(pooled_pca, conf.int = TRUE)

pooled_coefs_plot(pca_fit)

mice_anova(reg_fit, pca_fit)

saveRDS(reg_fit, "models/reg_fit_2021.RDS")
saveRDS(pca_fit, "models/pca_fit_2021.RDS")

# predictions ------------------------------------------------------------

# extract the models
model_list <- getfit(reg_fit)

afford_quantiles <- seq(min(dat$affordability),max(dat$affordability),((max(dat$affordability)-min(dat$affordability))/10))

# avg predictions for homeownership
grid_vals <- datagrid(
  model = model_list[[1]], 
  affordability = afford_quantiles, 
  homeowner = unique 
)

home_pred_list <- map(model_list, function(m) {
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
  labs(y = "Predicted outcome\nOpposition to migration", x = "Affordability (standardised)") +
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
  model = model_list[[1]], 
  affordability = afford_quantiles, 
  social_housing = unique 
)

sohs_pred_list <- map(model_list, function(m) {
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
  labs(y = "Predicted outcome\nOpposition to migration", x = "Affordability (standardised)") +
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

comp_home <- map(model_list, function(m) {
  avg_comparisons(m,
                  variables = "homeowner", 
                  by = "affordability",
                  newdata = datagrid(affordability = afford_quantiles))
})

# moderation effect
comp_sohs <- map(model_list, function(m) {
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
  labs(x = "Affordability (standardised)", y = "Estimate\nOpposition to migration", colour = "Tenure", fill = "Tenure")

my_ggsave(filename = "viz/AME_moderation_reg_fit_2021.png")

# joint coef plot ---------------------------------------------------------------------

plot_estimates <- pooled_reg_summary |> 
  as_tibble() |> 
  bind_rows(pooled_pca_summary |> as_tibble(),
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
  labs(x = "Estimate\nOpposition to migration", y = NULL) +
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

start_time <- Sys.time()
set.seed(123)
ci_reg <- map(getfit(reg_fit), function(m) {
  confint(m,
          parm = c("social_housing","homeowner","affordability","affordability:homeowner","social_housing:affordability","private_renting"),
          method = "boot",
          nsim = 500,
          quiet = TRUE)}
  )

end_time <- Sys.time()
end_time - start_time

wald_reg <- summary(pooled_reg, conf.int = TRUE) |> 
  filter(term %in% rownames(ci_reg[[1]]))

ci_reg |>
  map(as.data.frame) |> 
  map(rownames_to_column, var = "term") |> 
  bind_rows(.id = "m") |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.2, colour = "grey") +
  geom_linerange(aes(xmin = `2.5 %`, xmax = `97.5 %`, 
                     y = term), colour = "black",
                 linewidth = 1.5) +
  scale_colour_grey() +
  geom_linerange(data = wald_reg, 
                 aes(xmin = `2.5 %`, xmax = `97.5 %`, y = term),
                 colour = "red", linewidth = 0.7) +
  geom_point(data = wald_reg,
             aes(x = estimate, y = term),
             shape = 21, size = 3, fill = "white") +
  theme_bw() +
  labs(x = "Estimate", y = NULL,
       caption = "Comparison of confidence intervals by method for Model 1.\nBlack lines are bootstrapped 95% confidence intervals. Red lines are Wald 95% confidence intervals.") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0,
                                    size = 12))

my_ggsave("viz/ci_comparison_reg_fit_2021.png")

saveRDS(ci_reg, "models/ci_reg_2021.RDS")

# bootstrap confints PCA models -----------------------------------------------------------

start_time <- Sys.time()
set.seed(123)
ci_pca <- map(getfit(pca_fit), function(m) {
  confint(m,
          parm = c("social_housing","homeowner","pc2","homeowner:pc2","social_housing:pc1","pc1","private_renting"),
          method = "boot",
          nsim = 500,
          quiet = TRUE)}
)

end_time <- Sys.time()
end_time - start_time

wald_pca <- summary(pooled_pca, conf.int = TRUE) |> 
  filter(term %in% rownames(ci_pca[[1]]))

ci_pca |>
  map(as.data.frame) |> 
  map(rownames_to_column, var = "term") |> 
  bind_rows(.id = "m") |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.2, colour = "black") +
  geom_linerange(aes(xmin = `2.5 %`, xmax = `97.5 %`, 
                     y = term), colour = "black",
                 linewidth = 1.5) +
  scale_colour_grey() +
  geom_linerange(data = wald_pca, 
                 aes(xmin = `2.5 %`, xmax = `97.5 %`, y = term),
                 colour = "red", linewidth = 0.7) +
  geom_point(data = wald_pca,
             aes(x = estimate, y = term),
             shape = 21, size = 3, fill = "white") +
  theme_bw() +
  labs(x = "Estimate", y = NULL,
       caption = "Comparison of confidence intervals by method for Model 2.\nBlack lines are bootstrapped 95% confidence intervals. Red lines are Wald 95% confidence intervals.") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0,
                                    size = 12))

my_ggsave("viz/ci_comparison_pca_fit_2021.png")

saveRDS(ci_pca, "models/ci_pca_2021.RDS")
