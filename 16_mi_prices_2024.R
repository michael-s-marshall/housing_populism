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
  select(-all_of(unselect), -affordability_log, -affordability) |> 
  mutate(social_housing.prices = social_housing * prices,
         homeowner.prices = homeowner * prices,
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

# glmer -------------------------------------------------------------------------

start_time <- Sys.time()

# fitting the regions inclusive model to each of the imputed datasets
pri_fit <- with(data = imp_mice, exp = {
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
          (social_housing * prices) +
          (homeowner * prices) +
          (1|LAD),
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa"))
})

end_time <- Sys.time()
end_time - start_time

saveRDS(pri_fit, file = "models/pri_fit_2024.RDS")

# pooled models
pooled_pri <- pool(pri_fit)

# pooled odds ratios with cis (wald)
pri_summary <- summary(pooled_pri, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)
pri_summary

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
pri_models <- getfit(pri_fit)

prices_quantiles <- seq(min(dat$prices),max(dat$prices),((max(dat$prices)-min(dat$prices))/10))

# AMEs for social renters
mfx_sohs <- map(pri_models, function(m) {
  avg_slopes(m,
             variables = "social_housing",
             by = "prices",
             newdata = datagrid(prices = prices_quantiles, grid_type = "counterfactual"),
             type = "response")   
})

# pooling AMEs for social renters
pooled_ame_sohs <- map(mfx_sohs, as.data.frame) |> 
  my_pool(prices)

# AMEs for homeowners
mfx_home <- map(pri_models, function(m) {
  avg_slopes(m,
             variables = "homeowner",
             by = "prices",
             newdata = datagrid(prices = prices_quantiles, grid_type = "counterfactual"),
             type = "response")
})

# pooling AMEs for homeowners
pooled_ame_home <- map(mfx_home, as.data.frame) |> 
  my_pool(prices)

# plot of marginal effects
pooled_ame_sohs |> 
  bind_rows(pooled_ame_home, .id = "tenure") |>
  mutate(tenure = case_when(tenure == "1" ~ "Social housing", .default = "Homeowner")) |> 
  ggplot(aes()) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, x = prices, fill = tenure), alpha = 0.2) +
  geom_line(aes(x = prices, y = .estimate, colour = tenure), linewidth = 1) +
  #geom_point(aes(colour = tenure), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.2) +
  geom_rug(data = dat, aes(x = prices), alpha = 0.4) +
  labs(
    x = "Prices log (Standardised)",
    y = "Average Marginal Effect",
    colour = "Tenure", fill = "Tenure"
  ) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()

saveRDS(mfx_home, file = "models/AMEs_homeowner_prices_2024.RDS")
saveRDS(mfx_sohs, file = "models/AMEs_social_prices_2024.RDS")