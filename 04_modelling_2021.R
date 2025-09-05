pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance, margins)

rm(list = ls())

# helper function -----------------------------------------------------

mlm_diagnostics <- function(mlm_mod){
  
  require(patchwork)
  
  diag_plots <- sjPlot::plot_model(mlm_mod, type = "diag")
  
  (diag_plots[[1]] / diag_plots[[3]]) | diag_plots[[4]]
  
}

select <- dplyr::select

# loading dataset ---------------------------------------------

dat <- readRDS("data/cross_sectional_dat_2021.RDS")

##############################################################################
# immigself ------------------------------------------------------------------
##############################################################################

# missing observations --------------------------------

df_immi <- dat %>% 
  select(-tory_2019, -uni, -income,
         -full_time, -education_age, -log_hh, -disabled, -unemployed,
         -part_time, -log_age, 
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

# removing NAs
df_eng_wales <- df_immi %>% drop_na(c(affordability, degree_pct, non_uk_pct))

df_eng_wales %>% map_int(~sum(is.na(.)))

df_immi <- df_immi %>% 
  na.omit()

1 - (nrow(df_immi) / nrow(df_eng_wales))

rm(df_eng_wales, df_immi)

# predictions from low income model ------------------------------

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

df_full <- dat %>% 
  select(-tory_2019,
         -full_time, -education_age, -log_hh, -disabled, -unemployed,
         -part_time, -log_age, 
         -region_fct, -e, -d, -c1, -c2, -b, -cohabiting, -edu_20plus,
         -nn_uni_preds, -pred_el_round, -income_knn,
         -ta_rate, -ta_preds_raw, -ta_preds,
         -claims, -claims_preds, -claims_raw, -claims_preds_raw) %>% 
  rename(LAD = la_code)

sum_na(df_full)

level_2s <- df_full |> select(degree_pct:ta_rate_full_raw) |> names()

full_ns <- nrow(df_full)

df_full <- df_full |> drop_na(all_of(level_2s))

full_ns - nrow(df_full)
sub_ns <- nrow(df_full)

these_vars <- df_full %>% 
  select(-income, -uni) %>% 
  names()

sum_na(df_full)

df_full <- df_full %>% 
  drop_na(all_of(these_vars))

sub_ns - nrow(df_full)

rm(full_ns, sub_ns)

sum_na(df_full)

# modelling ---------------------------------------------

# ols null model
immi_fit <- lm(immigSelf ~ 1, data = df_full)

# lmer null model
immi_lmer <- lmer(immigSelf ~ (1|LAD), data = df_full, REML = F)

summary(immi_lmer)
summ(immi_lmer, re.variance = "var", digits = 3)

confint(immi_lmer)

get_icc <- function(lmer_obj){
  my_summ <- summary(lmer_obj)
  lvl2_var <- attr(my_summ$varcor[[1]],"stddev")^2
  lvl1_var <- my_summ$sigma^2
  total_var <- lvl1_var + lvl2_var
  icc <- lvl2_var / total_var
  out <- tibble(lvl2_var = lvl2_var,
                lvl1_var = lvl1_var,
                total_var = total_var,
                icc = icc)
  return(out)
}

get_icc(immi_lmer)

logLik(immi_fit)
logLik(immi_lmer)
2 * (logLik(immi_lmer) - logLik(immi_fit))

# hypothesis vars only, testing improved fit from SH interaction --------------

# making interaction terms with scaled values
df_full <- df_full %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability,
         social_housing.pc1 = social_housing * pc1,
         homeowner.pc2 = homeowner * pc2)

immi_hypot <- lmer(immigSelf ~ homeowner.affordability +
                     homeowner + affordability +
                     (1|LAD),
                   data = df_full, REML = FALSE)

summary(immi_hypot)

immi_hypot_sh <- lmer(immigSelf ~ social_housing.affordability +
                        homeowner.affordability +
                        social_housing + homeowner + affordability +
                        (1|LAD),
                      data = df_full, REML = FALSE)

summary(immi_hypot_sh)

# improvement in model fit is significant
anova(immi_hypot, immi_hypot_sh)

# demonstrating that non-UK percent more important than population density --------------------------------

immi_test <- lmer(immigSelf ~ social_housing + homeowner + private_renting +  
                    affordability +
                    male + 
                    white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
                    no_religion + 
                    age + income_full + uni_full +
                    c1_c2 + d_e + non_uk_born + 
                    non_uk_pct + pop_density +
                    over_65_pct + under_16_pct + 
                    degree_pct + 
                    #homeowner_pct + 
                    social_rented_pct +
                    churn + 
                    social_housing.affordability + 
                    homeowner.affordability +
                    (1|LAD),
                  data = df_full, REML = FALSE)
summary(immi_test)

immi_int <- lmer(immigSelf ~ social_housing + homeowner + private_renting +
                   affordability +
                   male + 
                   white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
                   no_religion + 
                   age + income_full + uni_full +
                   c1_c2 + d_e + non_uk_born + 
                   non_uk_pct + #pop_density +
                   over_65_pct + under_16_pct + 
                   degree_pct + 
                   #homeowner_pct + 
                   social_rented_pct +
                   social_housing.affordability + 
                   homeowner.affordability +
                   (1|LAD),
                 data = df_full, REML = FALSE)
summary(immi_int)

anova(immi_test, immi_int)

# diagnostics -----------------------------------------------------------------

mlm_diagnostics(immi_int)

# saving model
saveRDS(immi_int, file = "models/immi_int_2021.RDS")

# amount of level 2 variance explained -----------------------------

1 - (get_icc(immi_int)$lvl2_var / get_icc(immi_lmer)$lvl2_var)

# robustness check - log scale -------------------------------------

immi_log <- lmer(immigSelf ~ (social_housing * affordability_log) + 
                   (homeowner * affordability_log) + 
                   private_renting +
                   male + 
                   white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
                   no_religion + 
                   age + income_full + uni_full +
                   c1_c2 + d_e + non_uk_born + 
                   non_uk_pct + #pop_density +
                   over_65_pct + under_16_pct + 
                   degree_pct + 
                   #homeowner_pct + 
                   social_rented_pct +
                   (1|LAD),
                 data = df_full, REML = FALSE)
summary(immi_log)

# robustness check - with prices -------------------------------------

immi_int_price <- lmer(immigSelf ~ (social_housing * prices) + 
                         (homeowner * prices) + 
                         private_renting +
                         male +
                         white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race +
                         no_religion +
                         age + income_full + uni_full +
                         c1_c2 + d_e + non_uk_born +
                         non_uk_pct + #pop_density +
                         over_65_pct + under_16_pct +
                         degree_pct +
                         #homeowner_pct + 
                         social_rented_pct +
                         (1|LAD),
                       data = df_full, REML = FALSE)
summary(immi_int_price)

1 - (get_icc(immi_int_price)$lvl2_var / get_icc(immi_lmer)$lvl2_var)

AIC(immi_int, immi_int_price)

mlm_diagnostics(immi_int_price)

# robustness check - dummy for region ----------------------------------

immi_reg <- lmer(immigSelf ~ social_housing + homeowner + private_renting +
                   affordability +
                   male + 
                   white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
                   no_religion + 
                   age + income_full + uni_full +
                   c1_c2 + d_e + non_uk_born + 
                   non_uk_pct + #pop_density +
                   over_65_pct + under_16_pct + 
                   degree_pct + 
                   #homeowner_pct + 
                   social_rented_pct +
                   region_code +
                   social_housing.affordability + 
                   homeowner.affordability +
                   (1|LAD),
                 data = df_full, REML = FALSE)
summary(immi_reg)

anova(immi_int, immi_reg) 

# incl. region makes very little difference to estimates
housing_vars <- c("affordability","homeowner","social_housing",
                  "homeowner.affordability","social_housing.affordability")
tibble(
  var = housing_vars,
  no_region = fixef(immi_int)[housing_vars],
  incl_region = fixef(immi_reg)[housing_vars]
) %>% 
  pivot_longer(cols = no_region:incl_region,
               names_to = "model",
               values_to = "estimate") %>% 
  filter(var != "(Intercept)") %>% 
  ggplot(aes(x = estimate, y = var, colour = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.5, 
             colour = "lightgrey") +
  geom_point(size = 3, position = position_dodgev(height = 0.5)) +
  scale_colour_viridis_d() +
  theme_minimal() +
  drop_y_gridlines()

# exploratory analysis - removing degree percentage ----------------------

immi_deg <- lmer(immigSelf ~ social_housing + homeowner + private_renting +
                   affordability +
                   male + 
                   white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
                   no_religion + 
                   age + income_full + uni_full +
                   c1_c2 + d_e + non_uk_born + 
                   non_uk_pct + #pop_density +
                   over_65_pct + under_16_pct + 
                   #degree_pct + 
                   #homeowner_pct + 
                   social_rented_pct +
                   region_code +
                   social_housing.affordability + 
                   homeowner.affordability +
                   (1|LAD),
                 data = df_full, REML = FALSE)

summary(immi_deg)

# visualising interaction term ------------------------------

# making interaction terms with raw values
df_full2 <- df_full %>% 
  mutate(social_housing.affordability = social_housing * affordability_raw,
         homeowner.affordability = homeowner * affordability_raw)

immi_viz <- lmer(immigSelf ~ social_housing + homeowner + private_renting +
                   affordability_raw +
                   male + 
                   white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
                   no_religion + 
                   age + income_full + uni_full +
                   c1_c2 + d_e + non_uk_born + 
                   non_uk_pct + #pop_density +
                   over_65_pct + under_16_pct + 
                   degree_pct + 
                   #homeowner_pct + 
                   social_rented_pct +
                   social_housing.affordability + 
                   homeowner.affordability +
                   (1|LAD),
                 data = df_full2, REML = FALSE)

# anti immigration among social housing tenants 
x_scale <- seq(min(df_full2$affordability_raw), 
               max(df_full2$affordability_raw), 
               (max(df_full2$affordability_raw) - min(df_full2$affordability_raw))/5)

immi_dummy <- expand.grid(
  male = c(mean(df_full2$male)),
  white_british = c(mean(df_full2$white_british)),
  white_other = c(mean(df_full2$white_other)),
  indian = c(mean(df_full2$indian)),
  pakistan_bangladesh = c(mean(df_full2$pakistan_bangladesh)),
  black = c(mean(df_full2$black)),
  chinese = c(mean(df_full2$chinese)),
  mixed_race = c(mean(df_full2$mixed_race)),
  no_religion = c(mean(df_full2$no_religion)),
  income_full = c(mean(df_full2$income_full)),
  uni_full = c(mean(df_full2$uni_full)),
  homeowner = c(0,1),
  private_renting = c(0,1),
  age = mean(df_full2$age),
  c1_c2 = c(mean(df_full2$c1_c2)),
  d_e = c(mean(df_full2$d_e)),
  non_uk_born = c(mean(df_full2$non_uk_born)),
  non_uk_pct = c(mean(df_full2$non_uk_pct)),
  over_65_pct = c(mean(df_full2$over_65_pct)),
  under_16_pct = c(mean(df_full2$under_16_pct)),
  degree_pct = c(mean(df_full2$degree_pct)),
  homeowner_pct = c(mean(df_full2$homeowner_pct)),
  social_rented_pct = c(mean(df_full2$social_rented_pct)),
  social_housing = c(0,1),
  affordability_raw = x_scale
) %>% 
  mutate(social_housing.affordability = social_housing * affordability_raw,
         homeowner.affordability = homeowner * affordability_raw)

acov <- vcov(immi_viz)
fixed <- summary(immi_viz)$coefficients[,"Estimate"]
vars_order <- names(fixed)[-1]
xmat <- immi_dummy %>%
  mutate(int = 1, .before = 1) %>%
  select(int, all_of(vars_order)) %>%
  as.matrix()

immi_dummy$fit <- xmat %*% fixed
immi_dummy$SE <- xmat %*% acov %*% t(xmat) %>%
  diag() %>%
  sqrt()

immi_dummy <- immi_dummy %>%
  mutate(LL = fit - qnorm(0.975)*SE,
         UL = fit + qnorm(0.975)*SE)

pacman::p_load(patchwork)

p1 <- immi_dummy %>%
  mutate(
    tenure = ifelse(social_housing == 1 & homeowner == 0 & private_renting == 0, "Social housing",
                    ifelse(homeowner == 1 & social_housing == 0 & private_renting == 0, "Homeowner",
                           ifelse(private_renting == 1 & social_housing == 0 & homeowner == 0, "Private renting",
                                  "remove")))
  ) %>% 
  filter(tenure != "remove") %>% 
  mutate(tenure = fct_drop(tenure)) %>% 
  ggplot(aes(x = affordability_raw, y = fit,
             colour = tenure)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, group = tenure, fill = tenure,
                  colour = NULL),
              alpha = 0.2) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  labs(x = NULL,
       y = "Fewer immigrants",
       colour = "Tenure",
       fill = "Tenure") +
  coord_cartesian(ylim = c(5.5,9)) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

p2 <- df_full %>% 
  ggplot(aes(x = affordability_raw)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, 
                 colour = "black", fill = "lightgrey") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Affordability ratio", y = "Density")

int_plot <- p1 / plot_spacer() / p2  + plot_layout(heights = c(5, -0.2 , 3))

int_plot

ggsave("viz/interaction_plot_2021.png")

# housing stress measures ------------------------------------------------------

df_full <- df_full %>% 
  mutate(
    social_housing.ta_rate_full = social_housing * ta_rate_full,
    social_housing.overoccupied_pct = social_housing * overoccupied_pct
  )

immi_ta <- lmer(immigSelf ~ social_housing + ta_rate_full +
                  social_housing.ta_rate_full +
                  homeowner + affordability +
                  homeowner.affordability +
                  private_renting +
                  male + 
                  white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
                  no_religion + 
                  age + income_full + uni_full +
                  c1_c2 + d_e + non_uk_born + 
                  non_uk_pct + #pop_density +
                  over_65_pct + under_16_pct + 
                  degree_pct + 
                  #homeowner_pct + 
                  social_rented_pct +
                  region_code +
                  (1|LAD),
                data = df_full, REML = FALSE)
summary(immi_ta)

anova(immi_reg, immi_ta)

# overoccupying
immi_occ <- lmer(immigSelf ~ social_housing + overoccupied_pct +
                   social_housing.overoccupied_pct +
                   homeowner + affordability +
                   homeowner.affordability + 
                   private_renting +
                   male + 
                   white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
                   no_religion + 
                   age + income_full + uni_full +
                   c1_c2 + d_e + non_uk_born + 
                   non_uk_pct + #pop_density +
                   over_65_pct + under_16_pct + 
                   degree_pct + 
                   #homeowner_pct + 
                   social_rented_pct +
                   region_code +
                   (1|LAD),
                 data = df_full, REML = FALSE)
summary(immi_occ)

anova(immi_reg, immi_occ)

# KNN imputation of income ------------------------------------------------

immi_knn <- lmer(immigSelf ~ social_housing + homeowner + private_renting +
                   affordability +
                   male + 
                   white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
                   no_religion + 
                   age + income_full_knn + uni_full +
                   c1_c2 + d_e + non_uk_born + 
                   non_uk_pct + #pop_density +
                   over_65_pct + under_16_pct + 
                   degree_pct + 
                   #homeowner_pct + 
                   social_rented_pct +
                   region_code +
                   social_housing.affordability + 
                   homeowner.affordability +
                   (1|LAD),
                 data = df_full, REML = FALSE)
summary(immi_knn)

anova(immi_reg, immi_knn)

# logit -------------------------------------------------------------------------

df_full |> 
  ggplot(aes(x = immigSelf)) +
  geom_histogram(binwidth = 1, colour = "black", fill = "white")

df_full <- df_full |> 
  mutate(
    immig_binary = ifelse(immigSelf > 5, 1, 0)
  )

immi_glm <- glmer(immig_binary ~ social_housing + homeowner + private_renting +
                    affordability +
                    male + 
                    white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race + 
                    no_religion + 
                    age + income_full + uni_full +
                    c1_c2 + d_e + non_uk_born + 
                    non_uk_pct + #pop_density +
                    over_65_pct + under_16_pct + 
                    degree_pct + 
                    #homeowner_pct + 
                    social_rented_pct +
                    region_code +
                    social_housing.affordability + 
                    homeowner.affordability +
                    (1|LAD),
                  data = df_full, family = binomial("logit"),
                  control = glmerControl(optimizer = "bobyqa"))
summary(immi_glm)

# marginal effects
marginals_glm <- margins(immi_glm, type = "response")
summary(marginals_glm)

# PCA ----------------------------------------

immi_pca <- lmer(immigSelf ~ social_housing + homeowner + private_renting +
                   pc1 + pc2 +
                   male +
                   white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race +
                   no_religion +
                   age + income_full + uni_full +
                   c1_c2 + d_e + non_uk_born +
                   non_uk_pct + #pop_density +
                   over_65_pct + under_16_pct +
                   degree_pct +
                   social_rented_pct +
                   region_code +
                   social_housing.pc1 +
                   homeowner.pc2 +
                   (1|LAD),
                 data = df_full, REML = FALSE)

summary(immi_pca)

anova(immi_pca, immi_reg, immi_occ, immi_ta)

AIC(immi_pca, immi_reg, immi_occ, immi_ta)

saveRDS(immi_pca, file = "models/immi_pca_2021.RDS")

glmr_pca <- glmer(immig_binary ~ social_housing + homeowner + private_renting +
                   pc1 + pc2 +
                   male +
                   white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race +
                   no_religion +
                   age + income_full + uni_full +
                   c1_c2 + d_e + non_uk_born +
                   non_uk_pct + #pop_density +
                   over_65_pct + under_16_pct +
                   degree_pct +
                   social_rented_pct +
                   region_code +
                   social_housing.pc1 +
                   homeowner.pc2 +
                   (1|LAD),
                 data = df_full, family = binomial("logit"),
                 control = glmerControl(optimizer = "bobyqa"))

summary(glmr_pca)
anova(glmr_pca, immi_glm)

marginals_pca <- margins(glmr_pca, type = "response")
summary(marginals_pca)

# viz --------------------------------------------------------------------------

confint_clean <- function(confint_obj, lmer_obj){
  confint_obj %>% 
    as_tibble() %>% 
    bind_cols(
      tibble(term = c(".sig01",".sigma",names(fixef(lmer_obj))))
    ) %>% 
    bind_cols(tibble(estimate = c(NA, NA, fixef(lmer_obj)))) %>% 
    na.omit() %>% 
    filter(term != "(Intercept)") %>% 
    rename(lower = 1, upper = 2)
}


combined_names <- tibble(
  term = c("pc1","social_housing.pc1",
           "social_housing.affordability",
           "pc2","homeowner.pc2",
           "homeowner.affordability",
           "social_housing","homeowner","affordability","private_renting"),
  var_names = c("PC1", "PC1:Social housing",
                "Affordability:Social housing","PC2",
                "PC2:Homeowner","Affordability:Homeowner",
                "Social housing",
                "Homeowner","Affordability","Private renting")
) %>% 
  mutate(
    var_names = fct_relevel(as.factor(var_names),
                            c("PC1:Social housing",
                              "PC2:Homeowner",
                              "PC1",
                              "PC2",
                              "Affordability:Social housing",
                              "Affordability:Homeowner",
                              "Affordability",
                              "Social housing",
                              "Homeowner",
                              "Private renting"))
  )

reg_conf <- confint(immi_reg, method = "profile")
pca_conf <- confint(immi_pca, method = "profile")

bind_rows(
  confint_clean(reg_conf, immi_reg) |> mutate(Model="Model 1"),
  confint_clean(pca_conf, immi_pca) |> mutate(Model="Model 2")
) %>%  
  filter(str_detect(term, "social_housing|homeowner$|affordability|pc1|pc2|private")) %>%  
  left_join(combined_names, by = "term") %>% 
  ggplot(aes(x = estimate, y = var_names, colour = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", 
             linewidth = 1.25, alpha = 0.7) +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.4),
                 linewidth = 1.2) +
  geom_point(shape = 21, fill = "white", size = 3.2,
             position = position_dodge(width = 0.4)) +
  theme_bw() +
  drop_y_gridlines() +
  scale_colour_viridis_d() +
  labs(x = "Estimate", y = NULL)

ggsave("viz/coef_plot_mods_2021.png")

# viz comparing it to model minus degree % ---------------------------------

deg_conf <- confint(immi_deg, method = "profile")

bind_rows(
  confint_clean(reg_conf, immi_reg) |> mutate(Model="Model 1"),
  confint_clean(deg_conf, immi_deg) |> mutate(Model="Model 1 - Degree % omitted")
) %>%  
  filter(str_detect(term, "social_housing|homeowner$|affordability|private")) %>%  
  left_join(combined_names, by = "term") %>% 
  ggplot(aes(x = estimate, y = var_names, colour = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", 
             linewidth = 1.25, alpha = 0.7) +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.4),
                 linewidth = 1.2) +
  geom_point(shape = 21, fill = "white", size = 3.2,
             position = position_dodge(width = 0.4)) +
  theme_bw() +
  drop_y_gridlines() +
  scale_colour_viridis_d() +
  labs(x = "Estimate", y = NULL)

ggsave("viz/comparison_for_degree_pct.png")

# margin plots -----------------------------------------------------------

margin_plot <- function(margins_obj){
  margins_obj %>% 
    summary() %>% 
    as_tibble() %>% 
    ggplot(aes(x = AME, y = factor)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", 
               linewidth = 1.25, alpha = 0.7) +
    geom_linerange(aes(xmin = lower, xmax = upper),
                   position = position_dodge(width = 0.4),
                   linewidth = 1.2) +
    geom_point(shape = 21, fill = "white", size = 3.2,
               position = position_dodge(width = 0.4)) +
    theme_bw() +
    drop_y_gridlines() +
    labs(x = "Average Marginal Effect", y = NULL)
}

margin_plot(marginals_glm)
margin_plot(marginals_pca)

# export summs -------------------------------------------------------------

export_coefs <- c("Intercept" = "(Intercept)",
                  "Affordability ratio" = "affordability",
                  "Affordability ratio:Homeowner" = "homeowner.affordability",
                  "Affordability ratio:Social housing" = "social_housing.affordability",
                  "PC2:Homeowner" = "homeowner.pc2",
                  "PC1:Social housing" = "social_housing.pc1",
                  "PC1" = "pc1",
                  "PC2" = "pc2",
                  "Homeowner" = "homeowner",
                  "Private renting" = "private_renting",
                  "Social housing" = "social_housing",
                  "Age" = "age",
                  "Birth country: UK" = "non_uk_born",
                  "Education: Degree" = "uni_full",
                  "Ethnicity: Black" = "black",
                  "Ethnicity: Chinese" = "chinese",
                  "Ethnicity: Indian" = "indian",
                  "Ethnicity: Mixed-race" = "mixed_race",
                  "Ethnicity: Pakistan/Bangladesh" = "pakistan_bangladesh",
                  "Ethnicity: White British" = "white_british",
                  "Ethnicity: White other" = "white_other",
                  "Gender: Male" = "male",
                  "Household income" = "income_full",
                  "Religion: None" = "no_religion",
                  "Social class: C1-C2" = "c1_c2",
                  "Social class: D-E" = "d_e",
                  "Degree educated %" = "degree_pct",
                  #"Homeownership %" = "homeowner_pct",
                  "Non-UK born %" = "non_uk_pct",
                  "Over 65 %" = "over_65_pct",
                  "Social housing %" = "social_rented_pct",
                  "Under 16 %" = "under_16_pct",
                  "East Midlands" = "region_codeE12000004",
                  "East of England" = "region_codeE12000006",
                  "London" = "region_codeE12000007",
                  "North West" = "region_codeE12000002",
                  "South East" = "region_codeE12000008",
                  "South West" = "region_codeE12000009",
                  "Wales" = "region_codeW92000004",
                  "West Midlands" = "region_codeE12000005",
                  "Yorkshire and the Humber" = "region_codeE12000003"
)

export_summs(
  immi_reg, immi_pca, 
  error_pos = "right",
  error_format = "({conf.low}, {conf.high})",
  digits = 3,
  coefs = export_coefs,
  confint = TRUE,
  conf.method = "profile",
  to.file = "Word",
  file.name = "tables/2021_models_table.docx",
  statistics = c(N = "nobs", AIC = "AIC", `R2 (fixed)` = "r.squared.fixed", `R2 (total)` = "r.squared", logLik = "logLik")
)

export_coefs2 <- c("Intercept" = "(Intercept)",
                   "Affordability ratio" = "affordability",
                   "Affordability ratio:Homeowner" = "homeowner.affordability",
                   "Affordability ratio:Social housing" = "social_housing.affordability",
                   "Affordability ratio (log)" = "affordability_log",
                   "Affordability ratio (log):Homeowner" = "affordability_log:homeowner",
                   "Affordability ratio (log):Social housing" = "social_housing:affordability_log",
                   "Prices (log)" = "prices",
                   "Prices (log):Homeowner" = "prices:homeowner",
                   "Prices (log):Social housing" = "social_housing:prices",
                   "Homeowner" = "homeowner",
                   "Private renting" = "private_renting",
                   "Social housing" = "social_housing",
                   "Age" = "age",
                   "Birth country: UK" = "non_uk_born",
                   "Education: Degree" = "uni_full",
                   "Ethnicity: Black" = "black",
                   "Ethnicity: Chinese" = "chinese",
                   "Ethnicity: Indian" = "indian",
                   "Ethnicity: Mixed-race" = "mixed_race",
                   "Ethnicity: Pakistan/Bangladesh" = "pakistan_bangladesh",
                   "Ethnicity: White British" = "white_british",
                   "Ethnicity: White other" = "white_other",
                   "Gender: Male" = "male",
                   "Household income" = "income_full",
                   "Religion: None" = "no_religion",
                   "Social class: C1-C2" = "c1_c2",
                   "Social class: D-E" = "d_e",
                   "Degree educated %" = "degree_pct",
                   #"Homeownership %" = "homeowner_pct",
                   "Non-UK born %" = "non_uk_pct",
                   "Over 65 %" = "over_65_pct",
                   "Social housing %" = "social_rented_pct",
                   "Under 16 %" = "under_16_pct"
)

export_summs(immi_int, immi_log, immi_int_price,
             model.names = c("Interaction", "Log affordability", "Log prices"),
             error_pos = "right",
             error_format = "({conf.low}, {conf.high})",
             confint = TRUE,
             conf.method = "profile",
             digits = 3,
             coefs = export_coefs2,
             to.file = "Word",
             file.name = "tables/2021_robustness_table.docx",
             statistics = c(N = "nobs", AIC = "AIC", `R2 (fixed)` = "r.squared.fixed", `R2 (total)` = "r.squared", logLik = "logLik")
)

export_coefs3 <- c("Intercept" = "(Intercept)",
                   "Affordability ratio" = "affordability",
                   "Affordability ratio:Homeowner" = "homeowner.affordability",
                   "Affordability ratio:Social housing" = "social_housing.affordability",
                   "PC2:Homeowner" = "homeowner.pc2",
                   "PC1:Social housing" = "social_housing.pc1",
                   "PC1" = "pc1",
                   "PC2" = "pc2",
                   "Homeowner" = "homeowner",
                   "Private renting" = "private_renting",
                   "Social housing" = "social_housing",
                   "Age" = "age",
                   "Birth country: UK" = "non_uk_born",
                   "Education: Degree" = "uni_full",
                   "Ethnicity: Black" = "black",
                   "Ethnicity: Chinese" = "chinese",
                   "Ethnicity: Indian" = "indian",
                   "Ethnicity: Mixed-race" = "mixed_race",
                   "Ethnicity: Pakistan/Bangladesh" = "pakistan_bangladesh",
                   "Ethnicity: White British" = "white_british",
                   "Ethnicity: White other" = "white_other",
                   "Gender: Male" = "male",
                   "Household income" = "income_full",
                   "Religion: None" = "no_religion",
                   "Social class: C1-C2" = "c1_c2",
                   "Social class: D-E" = "d_e",
                   "Degree educated %" = "degree_pct",
                   #"Homeownership %" = "homeowner_pct",
                   "Non-UK born %" = "non_uk_pct",
                   "Over 65 %" = "over_65_pct",
                   "Social housing %" = "social_rented_pct",
                   "Under 16 %" = "under_16_pct",
                   "East Midlands" = "region_codeE12000004",
                   "East of England" = "region_codeE12000006",
                   "London" = "region_codeE12000007",
                   "North West" = "region_codeE12000002",
                   "South East" = "region_codeE12000008",
                   "South West" = "region_codeE12000009",
                   "Wales" = "region_codeW92000004",
                   "West Midlands" = "region_codeE12000005",
                   "Yorkshire and the Humber" = "region_codeE12000003"
)

export_summs(immi_glm, glmr_pca,
             model_names = c("Affordability","PCA"),
             error_pos = "right",
             error_format = "({conf.low}, {conf.high})",
             digits = 3,
             exp = TRUE,
             coefs = export_coefs3,
             to.file = "Word",
             file.name = "tables/2021_logit_table.docx",
             statistics = c(N = "nobs", AIC = "AIC", logLik = "logLik"))

list(immi_lmer, immi_reg, immi_pca) %>% 
  map(summ, re.variance = "var", digits = 3)

list(immi_int, immi_log, immi_int_price) %>% 
  map(summ, re.variance = "var", digits = 3)
