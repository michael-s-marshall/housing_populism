pacman::p_load(tidyverse, haven, jtools, randomForest, caret)

rm(list = ls())

df <- read_dta("data/panel_data/BES2019_W29_v29A.0.dta")

# rescale function --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# ind vars ------------------------------------------

# p_edlevel, p_ethnicity, p_religion, p_housing, age, 
# p_socgrade, p_country_birth, p_past_vote_2019
# ind vars
df <- df %>% 
  mutate(
    uni = fct_collapse(
      as.factor(p_edlevel),
      "1" = c("4","5"),
      "0" = c("0","1","2","3")
    ),
    uni = as.factor(case_when(uni == "1" ~ "yes", 
                              uni == "0" ~ "no", .default = NA)),
    white_british = fct_lump_n(as.factor(p_ethnicity), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    no_religion = fct_lump_n(as.factor(p_religion), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    soc_class = fct_collapse(
      as.factor(p_socgrade),
      "A-B" = c("1","2"),
      "C1-C2" = c("3","4"),
      "D-E" = c("5","6"),
      "Other" = c("8")
    ),
    non_uk_born = fct_lump_n(as.factor(p_country_birth), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    tory_2019 = fct_lump_n(as.factor(p_past_vote_2019), n = 1),
    leave = as.factor(p_eurefvote)
  ) %>% 
  rename(la_code = oslaua_code)

fct_count(df$leave)
class(df$uni)

df$male <- ifelse(df$gender == 1, 1, 0)
df$soc_class[df$soc_class == "Other"] <- NA
df$c1_c2 <- ifelse(df$soc_class == "C1-C2", 1, 0)
df$d_e <- ifelse(df$soc_class == "D-E", 1, 0)
df$income <- ifelse(df$p_gross_household %in% c(16, 17), 
                    NA, df$p_gross_household)
df$own_outright <- ifelse(df$p_housing == 1, 1, 0)
df$private_renting <- ifelse(df$p_housing == 4, 1, 0)
df$social_housing <- ifelse(df$p_housing == 5|df$p_housing == 6, 1, 0)
df$own_mortgage <- ifelse(df$p_housing == 2, 1, 0)
df$homeowner <- ifelse(df$own_outright == 1 | df$own_mortgage == 1, 1, 0)
df$tory_2019 <- ifelse(df$p_turnout_2019 == 0|df$tory_2019 == "Other", 0, 1)
df$tory_2019[df$p_past_vote_2019 == 9999] <- NA
df$non_voter <- df$p_turnout_2019 == 0
df$non_voter[df$p_turnout_2019 == 9999] <- NA
df$immigSelf[df$immigSelf == 9999] <- NA
df$age_raw <- df$age
df$age <- scale_this(df$age)
df$edu_20plus <- ifelse(df$p_education_age == 5, 1, 0)
df$edu_20plus[is.na(df$p_education_age)] <- NA

# vars for income predictions
df$full_time <- ifelse(df$p_work_stat == 1, 1, 0)
df$disabled <- ifelse(df$p_disability %in% c(1, 2), 1, 0)
df$disabled[is.na(df$p_disability)] <- NA
df$p_hh_size <- ifelse(df$p_hh_size %in% c(9, 10), 
                       NA, df$p_hh_size)
df$cohabiting <- ifelse(df$p_marital %in% c(1, 2, 4), 1, 0)
df$cohabiting[is.na(df$p_marital)] <- NA
df$e <- ifelse(df$p_socgrade == 6, 1, 0)
df$d <- ifelse(df$p_socgrade == 5, 1, 0)
df$c2 <- ifelse(df$p_socgrade == 4, 1, 0)
df$c1 <- ifelse(df$p_socgrade == 3, 1, 0)
df$b <- ifelse(df$p_socgrade == 2, 1, 0)
df$pakistan_bangladesh <- ifelse(df$p_ethnicity %in% c(8, 9), 1, 0)
df$unemployed <- ifelse(df$p_work_stat == 6, 1, 0)
df$unemployed[is.na(df$p_work_stat)] <- NA
df$part_time <- ifelse(df$p_work_stat %in% c(2, 3), 1, 0)
df$part_time[is.na(df$p_work_stat)] <- NA
df$region_fct <- as.factor(df$gor)

df %>% count(male, gender)
df %>% count(uni, p_edlevel)
df %>% count(white_british, p_ethnicity)
df %>% count(no_religion, p_religion)
df %>% count(soc_class, c1_c2, d_e, p_socgrade)
df %>% count(income, p_gross_household)
df %>% count(p_housing, own_outright, own_mortgage, homeowner, social_housing, private_renting)
df %>% count(non_uk_born, p_country_birth)
df %>% count(non_voter, p_turnout_2019)
df %>% count(edu_20plus, p_education_age)
df %>% count(immigSelf)
df %>% count(full_time, p_work_stat)
df %>% count(disabled, p_disability)
df %>% count(cohabiting, p_marital)

df <- df %>% 
  mutate(
    education_age = as.factor(p_education_age),
    log_age = log(age_raw),
    log_hh = log(p_hh_size)
  )

df %>% count(education_age, p_education_age)
df %>% count(log_hh, p_hh_size)

# model ---------------------------------------------

mod_dat <- df %>% 
  select(id, uni, full_time, edu_20plus, male, log_hh, unemployed,
         full_time, part_time, white_british, pakistan_bangladesh, age_raw,
         log_age, social_housing, private_renting, non_uk_born,
         own_mortgage, own_outright, region_fct, e, d, c1, c2, b, cohabiting) %>% 
  mutate(age_raw_sq = age_raw ^ 2)

mod_dat |> map_int(~sum(is.na(.)))

mod_dat <- mod_dat |> na.omit()

class(df$uni)

#mod_dat <- mod_dat |> sample_n(size = 500)

# test and train: 0.9/0.1 ratio
set.seed(123)
train_set <- sample_frac(mod_dat, 0.9) %>% as.data.frame()
test_set <- mod_dat %>% filter(!id %in% train_set$id) %>% as.data.frame()

# Defining the training controls -----------------------------

set.seed(123)
cv_folds <- createFolds(train_set$uni, k = 5, returnTrain = TRUE)

fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  index = cv_folds,
  classProbs = TRUE        # REQUIRED for probabilities
)

predictors <- c("full_time","edu_20plus","male","log_hh","unemployed","non_uk_born",
                "part_time","white_british","pakistan_bangladesh","age_raw","age_raw_sq",
                "social_housing","private_renting","own_mortgage",
                "own_outright","region_fct","e","d","c1","c2","b","cohabiting")

outcome_name <- "uni"

predictors2 <- predictors[predictors != "region_fct"]

x_trans <- preProcess(train_set[,predictors2])
train_set[,predictors2] <- predict(x_trans, train_set[,predictors2])
test_set[,predictors2] <- predict(x_trans, test_set[,predictors2])

# training lower layer models -------------------------------------------------

# Training the glm model
set.seed(123)
model_lm <- train(train_set[,predictors],
                  train_set[,outcome_name],
                  method='glm',
                  family='binomial',
                  trControl=fitControl,
                  metric = 'ROC')

summary(model_lm)

# extract Out-of-Fold predictions 
# access model$pred, sort by rowIndex, and extract the prediction
lm_oof <- model_lm$pred %>% 
  inner_join(model_lm$bestTune, by = names(model_lm$bestTune)) %>%
  arrange(rowIndex)
train_set$lm_prob <- lm_oof$yes

# check to ensure alignment
confusionMatrix(train_set$uni, lm_oof$pred)

# Training the nn model
set.seed(123)
model_nn <- train(train_set[,predictors],
                  train_set[,outcome_name],
                  method='nnet',
                  trControl=fitControl,
                  tuneLength=5,
                  trace = FALSE,
                  metric = "ROC") # trace=FALSE to reduce console noise

nn_oof <- model_nn$pred %>% 
  inner_join(model_nn$bestTune, by = names(model_nn$bestTune)) %>%
  arrange(rowIndex)
train_set$nn_prob <- nn_oof$yes

confusionMatrix(train_set$uni, nn_oof$pred)

# train RF model
set.seed(123)
model_rf <- train(train_set[,predictors], 
                  train_set[,outcome_name],
                  method='rf',
                  trControl=fitControl,
                  tuneLength=3,  
                  metric = "ROC")

rf_oof <- model_rf$pred %>% 
  inner_join(model_rf$bestTune, by = names(model_rf$bestTune)) %>%
  arrange(rowIndex)
train_set$rf_prob <- rf_oof$yes

confusionMatrix(train_set$uni, rf_oof$pred)

# training ensemble model -----------------------------------------------------

top_predictors <- c("lm_prob","nn_prob","rf_prob")

set.seed(123)
model_el <- train(train_set[,top_predictors], 
                  train_set[,outcome_name],
                  method='nnet', 
                  trControl=fitControl,
                  tuneLength=5,
                  trace = FALSE)

# Check In-Sample Results
el_preds <- predict(model_el)
confusionMatrix(train_set$uni, el_preds)

test_lm <- ifelse(predict(model_lm, newdata=test_set[,predictors], type="prob")[,"yes"]>0.5, "yes", "no")
test_nn <- ifelse(predict(object = model_nn, newdata = test_set[,predictors])=="yes", "yes", "no")
test_rf <- ifelse(predict(object = model_rf, newdata = test_set[,predictors])=="yes", "yes", "no")

# assign  probabilities to the test dataframe
test_set$lm_prob <- predict(model_lm, newdata = test_set[,predictors], type = "prob")[,"yes"]
test_set$nn_prob <- predict(model_nn, newdata = test_set[,predictors], type = "prob")[,"yes"]
test_set$rf_prob <- predict(model_rf, newdata = test_set[,predictors], type = "prob")[,"yes"]
test_el <- predict(object = model_el, newdata = test_set[,top_predictors])

confusionMatrix(test_set$uni, as.factor(test_lm))
confusionMatrix(test_set$uni, as.factor(test_nn))
confusionMatrix(test_set$uni, as.factor(test_rf))
confusionMatrix(test_set$uni, as.factor(test_el))

saveRDS(model_lm, file = "models/uni_2024/model_glm_W29.RDS")
saveRDS(model_nn, file = "models/uni_2024/model_nn_W29.RDS")
saveRDS(model_rf, file = "models/uni_2024/model_rf_W29.RDS")
saveRDS(model_el, file = "models/uni_2024/model_el_W29.RDS")

# preds for full dataset ---------------------------------------------------

pred_dat <- df %>% 
  select(id, full_time, edu_20plus, male, log_hh, unemployed, non_uk_born,
         full_time, part_time, white_british, pakistan_bangladesh, age_raw,
         social_housing, private_renting,
         own_mortgage, own_outright, region_fct, e, d, c1, c2, b, cohabiting) %>%
  mutate(age_raw_sq = age_raw ^ 2)

pred_dat |> map_int(~sum(is.na(.)))

pred_dat <- pred_dat |> na.omit()

pred_dat[,predictors2] <- predict(x_trans, pred_dat[,predictors2])
pred_dat$lm_prob <- predict(model_lm, newdata = pred_dat[,predictors], type = "prob")[,"yes"]
pred_dat$nn_prob <- predict(model_nn, newdata = pred_dat[,predictors], type = "prob")[,"yes"]
pred_dat$rf_prob <- predict(model_rf, newdata = pred_dat[,predictors], type = "prob")[,"yes"]
pred_dat$el_uni_preds <- predict(object = model_el, newdata = pred_dat[,top_predictors])

fct_count(pred_dat$el_uni_preds, prop = TRUE)

saveRDS(pred_dat, "data/preds_uni_2024.RDS")

