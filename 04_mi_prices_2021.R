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
pred["income", "immigSelf"] <- 1
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

# lmer -------------------------------------------------------------------------

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

# Fit the multilevel regression to each of the imputed datasets
pri_fit <- with(data = imp_mice, exp = {
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

# Pool the results from all the fitted lme4 models
pooled_pri <- pool(pri_fit)

# View the pooled summary
summary(pooled_pri, conf.int = TRUE)

pooled_coefs_plot(pri_fit)

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

# extract the models
model_list <- getfit(pri_fit)

price_quantiles <- seq(min(dat$prices),max(dat$prices),((max(dat$prices)-min(dat$prices))/10))

comp_home <- map(model_list, function(m) {
  avg_comparisons(m,
                  variables = "homeowner", 
                  by = "prices",
                  newdata = datagrid(prices = price_quantiles))
})

# moderation effect
comp_sohs <- map(model_list, function(m) {
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
  labs(x = "Prices log (Standardised)", y = "Estimate", colour = "Tenure", fill = "Tenure")

my_ggsave(filename = "viz/AME_prices_2021.png")

# confints ----------------------------------------------------------------------

start_time <- Sys.time()
set.seed(123)
ci_pri <- map(getfit(pri_fit), function(m) {
  confint(m,
          parm = c("social_housing","homeowner","prices","prices:homeowner","social_housing:prices","private_renting"),
          method = "boot",
          nsim = 500,
          quiet = TRUE)}
)

end_time <- Sys.time()
end_time - start_time

wald_pri <- summary(pooled_pri, conf.int = TRUE) |> 
  filter(term %in% rownames(ci_pri[[1]]))

ci_pri |>
  map(as.data.frame) |> 
  map(rownames_to_column, var = "term") |> 
  bind_rows(.id = "m") |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.2, colour = "grey") +
  geom_linerange(aes(xmin = `2.5 %`, xmax = `97.5 %`, 
                     y = term), colour = "black",
                 linewidth = 1.5) +
  scale_colour_grey() +
  geom_linerange(data = wald_pri, 
                 aes(xmin = `2.5 %`, xmax = `97.5 %`, y = term),
                 colour = "red", linewidth = 0.7) +
  geom_point(data = wald_pri,
             aes(x = estimate, y = term),
             shape = 21, size = 3, fill = "white") +
  theme_bw() +
  labs(x = "Estimate", y = NULL,
       caption = "Comparison of confidence intervals by method for model predicting attitudes to immigration and log of prices as measure of affordability.\nBlack lines are bootstrapped 95% confidence intervals. Red lines are Wald 95% confidence intervals.") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0,
                                    size = 12))

my_ggsave("viz/ci_comparison_pri_fit_2021.png")

saveRDS(ci_pri, "models/ci_pri_2021.RDS")
