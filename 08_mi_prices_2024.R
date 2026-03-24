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
         LAD = as.integer(as.factor(LAD)),
         uni = as.factor(uni),
         brexit_party = as.factor(brexit_party))

sum_na(dat)

# imputation ---------------------------------------------------------------------

# initialize the MICE model
meth <- character(ncol(dat))
names(meth) <- colnames(dat)
pred <- make.predictorMatrix(dat)

meth["income"] <- "2l.pan"
meth["uni"] <- "logreg"
meth["brexit_party"] <- "logreg"
meth["social_housing.prices"]   <- "~ I(social_housing * prices)"
meth["homeowner.prices"]   <- "~ I(homeowner * prices)"

meth["no_religion"] <- meth["social_housing"] <- meth["private_renting"] <- meth["homeowner"] <- meth["non_uk_born"] <- meth["edu_20plus"] <- meth["edu_15"] <- meth["edu_16"] <- meth["pub_job"] <- meth["p_hh_size"] <- meth["cohabiting"] <- meth["disabled"] <- "pmm"

pred[,"LAD"] <- -2
pred["LAD","LAD"] <- 0
pred["income", "brexit_party"] <- 1
pred["income", "income"] <- 0
pred["uni","LAD"] <- 0
pred["uni","disabled"] <- 0
pred["uni","part_time"] <- 0
pred["uni","full_time"] <- 0
pred["brexit_party","LAD"] <- 0
pred[,"immigSelf0"] <- 0
pred[,"immigSelf1"] <- 0
pred[,"immigSelf2"] <- 0
pred[,"immigSelf3"] <- 0
pred[,"immigSelf4"] <- 0
pred[,"immigSelf5"] <- 0
pred[,"immigSelf6"] <- 0
pred[,"immigSelf7"] <- 0
pred[,"immigSelf8"] <- 0
pred[,"immigSelf9"] <- 0
pred[,"immigSelf10"] <- 0
pred[,"dem_v_dissatisfied"] <- 0
pred[,"dem_dissatisfied"] <- 0
pred[,"dem_satisfied"] <- 0
pred[,"dem_v_satisfied"] <- 0
pred["brexit_party","immigSelf0"] <- 1
pred["brexit_party","immigSelf1"] <- 1
pred["brexit_party","immigSelf2"] <- 1
pred["brexit_party","immigSelf3"] <- 1
pred["brexit_party","immigSelf4"] <- 1
pred["brexit_party","immigSelf5"] <- 1
pred["brexit_party","immigSelf6"] <- 1
pred["brexit_party","immigSelf7"] <- 1
pred["brexit_party","immigSelf8"] <- 1
pred["brexit_party","immigSelf9"] <- 1
pred["brexit_party","immigSelf10"] <- 1
pred["brexit_party","dem_v_dissatisfied"] <- 1
pred["brexit_party","dem_dissatisfied"] <- 1
pred["brexit_party","dem_satisfied"] <- 1
pred["brexit_party","dem_v_satisfied"] <- 1
pred["uni",]
pred["income",]
pred["brexit_party",]

# multiple imputation
set.seed(123)
imp_mice <- mice(dat, method = meth, predictorMatrix = pred, m = 5, maxit = 5, printFlag = FALSE)

# diagnostics ------------------------------------------------------------------

ggmice(imp_mice, aes(x = brexit_party, group = .imp)) +
  geom_density()

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

# converting to mitml class --------------------------------------------------

imp_mitml <- mids2mitml.list(imp_mice)

# glmer -------------------------------------------------------------------------

start_time <- Sys.time()

# fitting the prices model to each of the imputed datasets
pri_fit <- with(data = imp_mitml, {
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

testEstimates(pri_fit)
confint.mitml.testEstimates(testEstimates(pri_fit))

# pooled odds ratios with cis (wald)
pri_summary <- pooled_summary(pri_fit) |> 
  mutate(across(estimate:conf.high, exp))
pri_summary

saveRDS(pri_fit, file = "models/pri_fit_2024.RDS")

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

prices_quantiles <- seq(min(dat$prices),max(dat$prices),((max(dat$prices)-min(dat$prices))/10))

# AMEs for social renters
mfx_sohs <- map(pri_fit, function(m) {
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
mfx_home <- map(pri_fit, function(m) {
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
    x = "Prices log (standardised)",
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

my_ggsave("viz/AMES_prices_2024.png")
saveRDS(mfx_home, file = "models/AMEs_homeowner_prices_2024.RDS")
saveRDS(mfx_sohs, file = "models/AMEs_social_prices_2024.RDS")