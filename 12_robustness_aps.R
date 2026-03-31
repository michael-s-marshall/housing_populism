pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance, marginaleffects, mice, broom.mixed, ggmice, mitml)

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

my_ggsave <- function(...){
  ggsave(...,
         units = "px",
         width = 3796,
         height = 2309)
}

pg_ggsave <- function(width = c(90, 140, 190), ...){
  if(width == 90){
    ggsave(...,
           units = "mm",
           width = width,
           height = 67.5)
  } else if (width == 140){
    ggsave(...,
           units = "mm",
           width = width,
           height = 105)
  } else {
    ggsave(...,
           units = "mm",
           width = width,
           height = 142.5)
  }
  
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
  select(-all_of(unselect), -affordability_log, -prices) |> 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability,
         social_housing.pc1 = social_housing * pc1,
         homeowner.pc2 = homeowner * pc2,
         region_code = as.factor(region_code),
         #LAD = as.integer(as.factor(LAD)),
         uni = as.factor(uni),
         brexit_party = as.factor(brexit_party))

sum_na(dat)

# aps -----------------------------------------------------------------------------

aps <- read_csv("data/aps_quals.csv")

aps <- aps |> 
  rename(LAD = mnemonic,
         aps_qual = `Jan 2024-Dec 2024`) |> 
  mutate(aps_qual = parse_number(aps_qual)) |> 
  select(LAD, aps_qual)

dat |> 
  left_join(aps, by = "LAD") |> 
  select(aps_qual, degree_pct) |> 
  unique() |> 
  ggplot(aes(x = degree_pct, y = aps_qual)) +
  geom_point() +
  geom_smooth()

dat <- dat |> 
  left_join(aps, by = "LAD")

dat <- dat |> 
  mutate(LAD = as.integer(as.factor(LAD))) |> 
  select(-degree_pct)

# imputation ---------------------------------------------------------------------

# initialize the MICE model
meth <- character(ncol(dat))
names(meth) <- colnames(dat)
pred <- make.predictorMatrix(dat)

meth["income"] <- "2l.pan"
meth["uni"] <- "logreg"
meth["brexit_party"] <- "logreg"
meth["social_housing.affordability"]   <- "~ I(social_housing * affordability)"
meth["homeowner.affordability"]   <- "~ I(homeowner * affordability)"
meth["social_housing.pc1"]   <- "~ I(social_housing * pc1)"
meth["homeowner.pc2"]   <- "~ I(homeowner * pc2)"
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
pred[,"dem_v_dissatisfied"] <- 0
pred[,"dem_dissatisfied"] <- 0
pred[,"dem_satisfied"] <- 0
pred[,"dem_v_satisfied"] <- 0
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

map_dbl(imp_mice$imp$brexit_party, .f = function(x) {
  sum(x == "1")
})

# converting to mitml class --------------------------------------------------

imp_mitml <- mids2mitml.list(imp_mice)

# glmer -------------------------------------------------------------------------

start_time <- Sys.time()

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
          aps_qual +
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
confint.mitml.testEstimates(testEstimates(reg_fit))

reg_summary <- pooled_summary(reg_fit) |> 
  mutate(across(estimate:conf.high, exp))
reg_summary

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
          aps_qual +
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

pca_summary <- pooled_summary(pca_fit) |> 
  mutate(across(estimate:conf.high, exp))
pca_summary

# saving -----------------------------------------------------

saveRDS(reg_fit, "models/robustness_aps_reg.RDS")
saveRDS(pca_fit, "models/robustness_aps_pca.RDS")
saveRDS(imp_mice, "models/robustness_aps_imp_mice.RDS")

# comparison plot ---------------------------------------------

reg_main <- readRDS("models/reg_fit_2024.RDS")
pca_main <- readRDS("models/pca_fit_2024.RDS")

reg_main |> 
  pooled_summary() |> 
  mutate(across(estimate:conf.high, exp)) |> 
  bind_rows(reg_summary, .id = "Model") |> 
  mutate(Model = case_when(Model == "2" ~ "APS - Qualifications",
                           Model == "1" ~ "Census - Degrees",
                           .default = NA)) |> 
  filter(term %in% c("private_renting","social_housing","homeowner","affordability","social_housing:affordability","affordability:homeowner")) |> 
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1.2, colour = "grey") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high, colour = Model),
                 position = position_dodge(width = 0.4),
                 linewidth = 1.2) +
  geom_point(aes(colour = Model),
             shape = 21, fill = "white", size = 3.5,
             position = position_dodge(width = 0.4)) +
  theme_bw() +
  labs(x = "Odds Ratio", y = NULL) +
  scale_colour_viridis_d() +
  scale_y_discrete(labels = c("Affordability",
                              "Homeowner X Affordability",
                              "Homeowner",
                              "Private renting",
                              "Social housing",
                              "Social housing X Affordability")) +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

my_ggsave("viz/appendix_aps_quals_affordability.svg")
pg_ggsave(width = 190, "viz/appendix_aps_quals_affordability.pdf")

pca_main |> 
  pooled_summary() |> 
  mutate(across(estimate:conf.high, exp)) |> 
  bind_rows(pca_summary, .id = "Model") |> 
  mutate(Model = case_when(Model == "2" ~ "APS - Qualifications",
                           Model == "1" ~ "Census - Degrees",
                           .default = NA)) |> 
  filter(term %in% c("private_renting","social_housing","homeowner","pc1","pc2","social_housing:pc1","homeowner:pc2")) |> 
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1.2, colour = "grey") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high, colour = Model),
                 position = position_dodge(width = 0.4),
                 linewidth = 1.2) +
  geom_point(aes(colour = Model),
             shape = 21, fill = "white", size = 3.5,
             position = position_dodge(width = 0.4)) +
  theme_bw() +
  labs(x = "Odds Ratio", y = NULL) +
  scale_colour_viridis_d() +
  scale_y_discrete(labels = c("Homeowner",
                              "Homeowner X PC2",
                              "PC1",
                              "PC2",
                              "Private renting",
                              "Social housing",
                              "Social housing X PC1")) +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

my_ggsave("viz/appendix_aps_quals_pca.svg")
pg_ggsave(width = 190, "viz/appendix_aps_quals_pca.pdf")
