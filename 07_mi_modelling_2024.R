pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance, marginaleffects, mice, broom.mixed, ggmice)

rm(list = ls())

# helper function -----------------------------------------------------

select <- dplyr::select

pooled_summary <- function(mitml_obj){
  out <- as.data.frame(testEstimates(mitml_obj)$estimates[,1]) |> 
    rownames_to_column(var = "term") |> 
    bind_cols(confint.mitml.testEstimates(testEstimates(mitml_obj))) |> 
    rename(estimate = 2, conf.low = 3, conf.high = 4)
  return(out)
}

# loading dataset ---------------------------------------------

dat <- readRDS("data/modelling_dataset_2024.RDS")

my_ggsave <- function(...){
  ggsave(...,
         units = "px",
         width = 3796,
         height = 2309)
}

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
meth["uni"] <- "logreg"
meth["social_housing.affordability"]   <- "~ I(social_housing * affordability)"
meth["homeowner.affordability"]   <- "~ I(homeowner * affordability)"
meth["social_housing.pc1"]   <- "~ I(social_housing * pc1)"
meth["homeowner.pc2"]   <- "~ I(homeowner * pc2)"

pred[,"LAD"] <- -2
pred["LAD","LAD"] <- 0
pred["income", "brexit_party"] <- 1
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

# glmer -------------------------------------------------------------------------

start_time <- Sys.time()

nulls_fit <- with(data = imp_mitml, {
  glmer(brexit_party ~ (1|LAD),
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa"))
})

testEstimates(nulls_fit, extra.pars = TRUE)

saveRDS(nulls_fit, file = "models/null_models_mice_2024.RDS")

# fitting the regions inclusive model to each of the imputed datasets
reg_fit <- with(data = imp_mitml, {
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

testEstimates(reg_fit, extra.pars = TRUE)
testModels(reg_fit, nulls_fit)
confint.mitml.testEstimates(testEstimates(reg_fit))

reg_summary <- pooled_summary(reg_fit) |> 
  mutate(across(estimate:conf.high, exp))
reg_summary

saveRDS(reg_fit, file = "models/reg_fit_2024.RDS")

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
# reg_models <- getfit(reg_fit)

afford_quantiles <- seq(min(dat$affordability),max(dat$affordability),((max(dat$affordability)-min(dat$affordability))/10))

# AMEs for social renters
mfx_sohs <- map(reg_fit, function(m) {
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
mfx_home <- map(reg_fit, function(m) {
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
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.2) +
  geom_rug(data = dat, aes(x = affordability), alpha = 0.4) +
  labs(
    x = "Affordability (standardised)",
    y = "Average Marginal Effect: Voting Reform UK",
    colour = "Tenure", fill = "Tenure"
  ) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank()) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()

my_ggsave(filename = "viz/AME_plot_reg_fit_2024.png")
saveRDS(mfx_home, file = "models/AMEs_homeowner_affordability_2024.RDS")
saveRDS(mfx_sohs, file = "models/AMEs_social_affordability_2024.RDS")

# PCA model -------------------------------------------------------------------

start_time <- Sys.time()

pca_fit <- with(imp_mitml, {
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

testEstimates(pca_fit, extra.pars = TRUE)
confint.mitml.testEstimates(testEstimates(pca_fit))

map2(.x = reg_fit, .y = pca_fit, .f = ~anova(.x, .y))

pca_summary <- pooled_summary(pca_fit) |> 
  mutate(across(estimate:conf.high, exp))
pca_summary

saveRDS(pca_fit, file = "models/pca_fit_2024.RDS")

# PCA AMEs --------------------------------------------------------

# PCA models
# pca_models <- getfit(pca_fit)

pc1_quantiles <- seq(min(dat$pc1),max(dat$pc1),((max(dat$pc1)-min(dat$pc1))/10))
pc2_quantiles <- seq(min(dat$pc2),max(dat$pc2),((max(dat$pc2)-min(dat$pc2))/10))

# AMEs for social renters
mfx_pc1 <- map(pca_fit, function(m) {
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
mfx_pc2 <- map(pca_fit, function(m) {
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

viridis_scale <- viridis::viridis(2)

sh1 <- pooled_ame_pc1 |> 
  mutate(Tenure = "Social housing") |>
  ggplot() +
  geom_ribbon(aes(x = pc1, ymin = conf.low, ymax = conf.high, fill = Tenure), alpha = 0.2) +
  geom_line(aes(x = pc1, y = .estimate, colour = Tenure), linewidth = 1.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.2) +
  geom_rug(data = dat, aes(x = pc1), alpha = 0.4) +
  labs(
    x = "PC1 (standardised)",
    y = "Average Marginal Effect: Voting Reform UK"
  ) +
  coord_cartesian(ylim = c(-0.05, 0.4)) +
  theme_bw() +
  scale_colour_manual(values = viridis_scale[2]) +
  scale_fill_manual(values = viridis_scale[2]) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank())

sh1

h1 <- pooled_ame_pc2 |> 
  mutate(Tenure = "Homeowner") |> 
  ggplot() +
  geom_ribbon(aes(x = pc2, ymin = conf.low, ymax = conf.high, fill = Tenure), alpha = 0.2) +
  geom_line(aes(x = pc2, y = .estimate, colour = Tenure), linewidth = 1.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.2) +
  geom_rug(data = dat, aes(x = pc2), alpha = 0.4) +
  labs(
    x = "PC2 (standardised)",
    y = "Average Marginal Effect: Voting Reform UK",
    fill = "Tenure", colour = "Tenure"
  ) +
  coord_cartesian(ylim = c(-0.05, 0.4)) +
  scale_colour_manual(values = viridis_scale[1]) +
  scale_fill_manual(values = viridis_scale[1]) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank())

h1

require(patchwork)

h1 + sh1 + plot_layout(axis_titles = "collect",
                       guides = "collect") & 
  theme(
    legend.spacing.y = unit(0, "cm"),     
    legend.margin = ggplot2::margin(0, 0, 0, 0),   
    legend.box.margin = ggplot2::margin(-5, 0, -5, 0) 
    )

my_ggsave("viz/AMES_plot_pca_2024.png")
saveRDS(mfx_pc1, file = "models/AMEs_social_pc1_2024.RDS")
saveRDS(mfx_pc2, file = "models/AMEs_homeowner_pc2_2024.RDS")

# odds ratio plot --------------------------------------------------------

plot_estimates <- reg_summary |> 
  bind_rows(pca_summary,
            .id = "Model") |> 
  mutate(Model = case_when(Model == "1" ~ "3",
                           .default = "4")) |> 
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
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey", linewidth = 1.2) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), 
                 linewidth = 1.25,
                 position = position_dodge(width = 0.4)) +
  geom_point(shape = 21, fill = "white", 
             size = 3,
             position = position_dodge(width = 0.4)) +
  labs(x = "Odds Ratio: Voting Reform UK", y = NULL) +
  scale_colour_viridis_d() +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank())

my_ggsave("viz/odds_ratios_models_2024.png")
