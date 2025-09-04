pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance, margins)

rm(list = ls())

# loading dataset ---------------------------------------------

dat <- readRDS("data/cross_sectional_dat_2024.RDS")

##############################################################################
# Brexit party ---------------------------------------------------------------
##############################################################################

# missing observations --------------------------------

df_immi <- dat %>% 
  select(-tory_2019, -uni, -income,
         -full_time, -education_age, -log_hh, -disabled, -unemployed,
         -part_time, -log_age, -churn, -churn_raw,
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
         -part_time, -log_age, -churn, -churn_raw,
         -region_fct, -e, -d, -c1, -c2, -b, -cohabiting, -edu_20plus,
         -nn_uni_preds, -pred_el_round, -income_knn,
         -ta_rate, -ta_preds_raw, -ta_preds,
         -claims, -claims_raw, -claims_preds, -claims_preds_raw, -claims_full_raw) %>% 
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

###########################################################
# modelling ---------------------------------------------
###########################################################

# hypothesis vars only, testing improved fit from SH interaction --------------

# making interaction terms with scaled values
df_full <- df_full %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability,
         social_housing.ta_rate_full = social_housing * ta_rate_full,
         social_housing.overoccupied_pct = social_housing * overoccupied_pct,
         homeowner.pc2 = homeowner * pc2,
         social_housing.pc1 = social_housing * pc1,
         social_housing.prices = social_housing * prices,
         homeowner.prices = homeowner * prices)

# null model
immi_glmr <- glmer(brexit_party ~ (1|LAD),
                   data = df_full, family = binomial("logit"),
                   control = glmerControl(optimizer = "bobyqa"))

summ(immi_glmr, digits = 3, re.variance = "var")
logLik(immi_glmr)
# demonstrating that non-UK percent more important than population density --------------------------------

immi_test <- glmer(brexit_party ~ social_housing + homeowner + private_renting +
                     affordability +
                     male +
                     white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race +
                     no_religion +
                     age + income_full + uni_full +
                     c1_c2 + d_e + non_uk_born +
                     non_uk_pct + pop_density +
                     over_65_pct + under_16_pct +
                     degree_pct +
                     social_rented_pct +
                     social_housing.affordability +
                     homeowner.affordability +
                     (1|LAD),
                   data = df_full, family = binomial("logit"),
                   control = glmerControl(optimizer = "bobyqa"))
summary(immi_test)

immi_int <- glmer(brexit_party ~ social_housing + homeowner + private_renting +
                    affordability +
                    male +
                    white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race +
                    no_religion +
                    age + income_full + uni_full +
                    c1_c2 + d_e + non_uk_born +
                    #non_uk_pct + 
                    pop_density +
                    over_65_pct + under_16_pct +
                    degree_pct +
                    #homeowner_pct + 
                    social_rented_pct +
                    social_housing.affordability +
                    homeowner.affordability +
                    (1|LAD),
                  data = df_full, family = binomial("logit"),
                  control = glmerControl(optimizer = "bobyqa"))
summary(immi_int)

anova(immi_test, immi_int)

marginals_glm <- margins(immi_int, type = "response")
summary(marginals_glm)

# saving model --------------------------------------------------------

saveRDS(immi_int, file = "models/immi_int_2024.RDS")

# robustness check - log scale -------------------------------------

immi_log <- glmer(brexit_party ~ (social_housing * affordability_log) + 
                   (homeowner * affordability_log) + 
                   private_renting +
                   male +
                   white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race +
                   no_religion +
                   age + income_full + uni_full +
                   c1_c2 + d_e + non_uk_born +
                   #non_uk_pct + 
                   pop_density +
                   over_65_pct + under_16_pct +
                   degree_pct +
                   #homeowner_pct + 
                   social_rented_pct +
                   (1|LAD),
                 data = df_full, family = binomial("logit"),
                 control = glmerControl(optimizer = "bobyqa"))
summary(immi_log)

marginals_log <- margins(immi_log, type = "response")
summary(marginals_log)

saveRDS(immi_log, file = "models/immi_log_2024.RDS")

# robustness check - with prices -------------------------------------

immi_int_price <- glmer(brexit_party ~ social_housing + homeowner + private_renting +
                         prices + 
                         private_renting +
                         male +
                         white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race +
                         no_religion +
                         age + income_full + uni_full +
                         c1_c2 + d_e + non_uk_born +
                         #non_uk_pct + 
                         pop_density +
                         over_65_pct + under_16_pct +
                         degree_pct +
                         #homeowner_pct +
                         social_rented_pct +
                         social_housing.prices +
                         homeowner.prices +
                         (1|LAD),
                       data = df_full, family = binomial("logit"),
                       control = glmerControl(optimizer = "bobyqa"))
summary(immi_int_price)

AIC(immi_int, immi_int_price)

marginals_pri <- margins(immi_int_price, type = "response")
summary(marginals_pri)

saveRDS(immi_int_price, file = "models/immi_int_price_2024.RDS")

# robustness check - dummy for region ----------------------------------

immi_reg <- glmer(brexit_party ~ social_housing + homeowner + private_renting +
                   affordability +
                   male +
                   white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race +
                   no_religion +
                   age + income_full + uni_full +
                   c1_c2 + d_e + non_uk_born +
                   #non_uk_pct + 
                   pop_density +
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
summary(immi_reg)

anova(immi_int, immi_reg) 

saveRDS(immi_reg, file = "models/immi_reg_2024.RDS")

# marginal effects
marginals_reg <- margins(immi_reg, type = "response")
summary(marginals_reg)

# KNN imputation of income ------------------------------------------------

immi_knn <- glmer(brexit_party ~ social_housing + homeowner + private_renting +
                    affordability +
                    male +
                    white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race +
                    no_religion +
                    age + income_full_knn + uni_full +
                    c1_c2 + d_e + non_uk_born +
                    #non_uk_pct + 
                    pop_density +
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
summary(immi_knn)

anova(immi_reg, immi_knn)

# saving marginals -------------------------------------------------------

write.csv(summary(marginals_glm), file = "tables/immi_int_marginals_2024.csv")
write.csv(summary(marginals_log), file = "tables/immi_log_marginals_2024.csv")
write.csv(summary(marginals_pri), file = "tables/immi_int_price_marginals_2024.csv")
write.csv(summary(marginals_reg), file = "tables/immi_reg_marginals_2024.csv")

saveRDS(marginals_reg, file = "models/immi_reg_marginals_2024.RDS")

# PCA ------------------------------------------------------

immi_pca <- glmer(brexit_party ~ social_housing + homeowner + private_renting +
                    pc1 + pc2 +
                    male +
                    white_british + white_other + indian + black + chinese + pakistan_bangladesh + mixed_race +
                    no_religion +
                    age + income_full + uni_full +
                    c1_c2 + d_e + non_uk_born +
                    #non_uk_pct + 
                    pop_density +
                    over_65_pct + under_16_pct +
                    degree_pct +
                    social_rented_pct +
                    region_code +
                    social_housing.pc1 +
                    homeowner.pc2 +
                    (1|LAD),
                  data = df_full, family = binomial("logit"),
                  control = glmerControl(optimizer = "bobyqa"))

summary(immi_pca)

marginals_pca <- margins(immi_pca, type = "response")
summary(marginals_pca)
saveRDS(immi_pca, file = "models/immi_pca_2024.RDS")
write.csv(summary(marginals_pca), file = "tables/immi_pca_marginals_2024.csv")

anova(immi_reg, immi_pca)

# viz --------------------------------------------------------------------------

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

summary(marginals_reg) %>% 
  as_tibble() %>% 
  mutate(Model = "Model 3") %>% 
  bind_rows(as_tibble(summary(marginals_pca)) %>% mutate(Model="Model 4")) %>% 
  left_join(combined_names, by = c("factor" = "term"))  %>%   
  filter(factor %in% combined_names$term) %>% 
  ggplot(aes(x = AME, y = var_names, colour = Model)) +
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
  labs(x = "Average Marginal Effect", y = NULL)

ggsave("viz/ame_plot_mods_2024.png")

list(immi_reg, immi_pca) %>% 
  map(summ, digits = 3, re.variance = "var")

list(immi_int, immi_log, immi_int_price) %>% 
  map(summ, digits = 3, re.variance = "var")

margin_plot <- function(margins_obj, title_string){
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
    labs(x = "Average Marginal Effect", y = NULL, title = title_string)
}

margin_plot(marginals_glm, "No region")
margin_plot(marginals_reg, "Model 3") 
margin_plot(marginals_pca, "Model 4")
