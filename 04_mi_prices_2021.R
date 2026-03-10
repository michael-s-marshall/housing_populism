pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance, marginaleffects, mice, broom.mixed, ggmice, mitml, parallel)

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
  select(-all_of(unselect), -affordability_log, -affordability, -imp_flag) |> 
  mutate(social_housing.prices = social_housing * prices,
         homeowner.prices = homeowner * prices,
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
meth["social_housing.prices"]   <- "~ I(social_housing * prices)"
meth["homeowner.prices"]   <- "~ I(homeowner * prices)"

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
ggmice(imp_mice, aes(x = social_housing.prices, group = .imp)) +
  geom_density()

ggmice(imp_mice, aes(x = homeowner.prices, group = .imp)) +
  geom_density()

# lmer -------------------------------------------------------------------------

imp_mitml <- mids2mitml.list(imp_mice)

# fitting prices model
pri_fit <- with(data = imp_mitml, {
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
         (social_housing * prices) + 
         (homeowner * prices) +
         (1|LAD), REML = FALSE)
})

# pooled summary
testEstimates(pri_fit, extra.pars = TRUE)

pooled_summary(pri_fit)

saveRDS(pri_fit, "models/pri_fit_2021.RDS")

# moderation effect ------------------------------------------------------------

# function for pooled standard errors
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

# values of prices for moderation effect
price_quantiles <- seq(min(dat$prices),max(dat$prices),((max(dat$prices)-min(dat$prices))/10))

comp_home <- map(pri_fit, function(m) {
  avg_comparisons(m,
                  variables = "homeowner", 
                  by = "prices",
                  newdata = datagrid(prices = price_quantiles))
})

# moderation effect
comp_sohs <- map(pri_fit, function(m) {
  avg_comparisons(m,
                  variables = "social_housing", 
                  by = "prices",
                  newdata = datagrid(prices = price_quantiles))
})

comp_home |> 
  my_pool(prices) |> 
  bind_rows(my_pool(comp_sohs, prices), .id = "tenure") |> 
  mutate(tenure = case_when(tenure == "1" ~ "Homeowner",
                            .default = "Social housing")) |> 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "lightgrey", linewidth = 1.2) +
  geom_rug(data = dat, aes(x = prices), colour = "black", alpha = 0.4) +
  geom_ribbon(aes(x = prices, ymin = conf.low, ymax = conf.high, fill = tenure), alpha = 0.25) +
  geom_line(aes(x = prices, y = .estimate, colour = tenure), linewidth = 1.25)  +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  labs(x = "Prices log (standardised)", y = "Estimate: Opposition to migration", colour = "Tenure", fill = "Tenure")

my_ggsave(filename = "viz/AME_prices_2021.png")

# confints ----------------------------------------------------------------------

# refitting to mice object
pri_fit_mice <- with(data = imp_mice, exp = {
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
         (social_housing * prices) + 
         (homeowner * prices) +
         (1|LAD), REML = FALSE)
})

my_params <- c("social_housing", "homeowner", "prices", "prices:homeowner", "social_housing:prices", "private_renting")
# function for extraction of fixed effects
get_fixed_effects <- function(model) {
  target_params <- c("social_housing", "homeowner", "prices", "prices:homeowner", "social_housing:prices", "private_renting")
  ests <- lme4::fixef(model)
  return(ests[target_params])
}

# number of cores for parallel processing
num_cores <- detectCores() - 1
my_seeds <- c(101, 102, 103, 104, 105) # seeds for parallel

start_time <- Sys.time()
ci_pri <- map2(getfit(pri_fit_mice), my_seeds, function(m, current_seed) {
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
pooled_ci_pri <- map_dfr(ci_pri, ~ as.data.frame(.x$t))
colnames(pooled_ci_pri) <- my_params

# calculate the 95% Confidence Intervals using the Percentile method
pooled_cis <- map_df(pooled_ci_pri, function(x) {
  quantile(x, probs = c(0.025, 0.975))
}) |> 
  mutate(term = colnames(pooled_ci_pri), estimate = NA, .before = 1) |> 
  rename(`2.5 %` = `2.5%`, `97.5 %` = `97.5%`)

# pooled estimate and wald intervals for comparison
pooled_pri <- pool(pri_fit_mice)

wald_pri <- summary(pooled_pri, conf.int = TRUE) |> 
  filter(term %in% colnames(pooled_ci_pri)) |> 
  select(term, estimate, `2.5 %`, `97.5 %`)

# plotting
pooled_cis |>
  bind_rows(wald_pri, .id = "method") |> 
  mutate(Method = case_when(method == "1" ~ "Bootstrap", .default = "Wald")) |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.2, colour = "grey") +
  geom_linerange(aes(xmin = `2.5 %`, xmax = `97.5 %`, 
                     y = term,
                     colour = Method),
                 linewidth = 1.2,
                 position = position_dodge(width = 0.2)) +
  geom_point(data = wald_pri,
             aes(x = estimate, y = term),
             shape = 21, size = 3, fill = "white") +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Estimate", y = NULL,
       caption = "Comparison of confidence intervals by method for model predicting attitudes to immigration and log of prices as measure of affordability.") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0,
                                    size = 12))

my_ggsave("viz/ci_comparison_pri_fit_2021.png")

saveRDS(ci_pri, "models/ci_pri_2021.RDS")
