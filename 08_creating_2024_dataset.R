##########################################################################
# creating cross-sectional dataset ---------------------------------------
##########################################################################

# packages ---------------------------------------------------------------

pacman::p_load(tidyverse, haven)

rm(list = ls())

# BES wave 22 ------------------------------------------------------------

dat <- read_dta("data/panel_data/BES2019_W29_v29A.0.dta")

# scaling functions --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

sum_na <- function(dat){
  out <- dat %>% 
    map_int(~sum(is.na(.)))
  return(out)
}

##############################################################
# level 1 vars -----------------------------------------------
##############################################################

dat %>% 
  select(starts_with("p_")) %>% 
  names() %>% 
  map(~count(dat %>% select(starts_with("p_")), .data[[.x]]))

# p_edlevel, p_ethnicity, p_religion, p_housing, age, 
# p_socgrade, p_country_birth, p_past_vote_2019
# ind vars
dat <- dat %>% 
  mutate(
    uni = fct_collapse(
      as.factor(p_edlevel),
      "1" = c("4","5"),
      "0" = c("0","1","2","3")
    ) %>% 
      as.character() %>% 
      parse_double(),
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
      "Unknown" = c("8")
    ),
    non_uk_born = fct_lump_n(as.factor(p_country_birth), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    tory_2019 = fct_lump_n(as.factor(p_past_vote_2019), n = 1)
  ) %>% 
  rename(la_code = oslaua_code)

dat$male <- ifelse(dat$gender == 1, 1, 0)
dat$soc_class[dat$soc_class == "Unknown"] <- NA
dat$c1_c2 <- ifelse(dat$soc_class == "C1-C2", 1, 0)
dat$d_e <- ifelse(dat$soc_class == "D-E", 1, 0)
dat$income <- ifelse(dat$p_gross_household %in% c(16, 17), 
                    NA, dat$p_gross_household)
dat$own_outright <- ifelse(dat$p_housing == 1, 1, 0)
dat$private_renting <- ifelse(dat$p_housing == 4, 1, 0)
dat$social_housing <- ifelse(dat$p_housing == 5|dat$p_housing == 6, 1, 0)
dat$own_mortgage <- ifelse(dat$p_housing == 2, 1, 0)
dat$homeowner <- ifelse(dat$own_outright == 1 | dat$own_mortgage == 1, 1, 0)
#dat$tory_2019 <- ifelse(dat$p_turnout_2019 == 0|dat$tory_2019 == "Other", 0, 1)
#dat$tory_2019[dat$p_past_vote_2019 == 9999] <- NA
dat$non_voter <- dat$p_turnout_2019 == 0
dat$non_voter[dat$p_turnout_2019 == 9999] <- NA
dat$immigSelf[dat$immigSelf == 9999] <- NA
dat$age_raw <- dat$age
dat$age <- scale_this(dat$age)
dat$edu_20plus <- ifelse(dat$p_education_age == 5, 1, 0)
dat$edu_20plus[is.na(dat$p_education_age)] <- NA

# vars for income predictions
dat$full_time <- ifelse(dat$p_work_stat == 1, 1, 0)
dat$disabled <- ifelse(dat$p_disability %in% c(1, 2), 1, 0)
dat$disabled[is.na(dat$p_disability)] <- NA
dat$p_hh_size <- ifelse(dat$p_hh_size %in% c(9, 10), 
                       NA, dat$p_hh_size)
dat$cohabiting <- ifelse(dat$p_marital %in% c(1, 2, 4), 1, 0)
dat$cohabiting[is.na(dat$p_marital)] <- NA
dat$e <- ifelse(dat$p_socgrade == 6, 1, 0)
dat$d <- ifelse(dat$p_socgrade == 5, 1, 0)
dat$c2 <- ifelse(dat$p_socgrade == 4, 1, 0)
dat$c1 <- ifelse(dat$p_socgrade == 3, 1, 0)
dat$b <- ifelse(dat$p_socgrade == 2, 1, 0)
dat$pakistan_bangladesh <- ifelse(dat$p_ethnicity %in% c(8, 9), 1, 0)
dat$unemployed <- ifelse(dat$p_work_stat == 6, 1, 0)
dat$unemployed[is.na(dat$p_work_stat)] <- NA
dat$part_time <- ifelse(dat$p_work_stat %in% c(2, 3), 1, 0)
dat$part_time[is.na(dat$p_work_stat)] <- NA
dat$region_fct <- as.factor(dat$gor)
dat$white_other <- ifelse(dat$p_ethnicity == 2, 1, 0)
dat$black <- ifelse(dat$p_ethnicity == 11 | dat$p_ethnicity == 12 | dat$p_ethnicity == 13, 1, 0)
dat$indian <- ifelse(dat$p_ethnicity == 7, 1, 0)
dat$chinese <- ifelse(dat$p_ethnicity == 14, 1,0)
dat$mixed_race <- ifelse(dat$p_ethnicity %in% c(3,4,5,6), 1, 0)

dat %>% count(male, gender)
dat %>% 
  count(tory_2019, p_turnout_2019, p_past_vote_2019)
dat %>% count(uni, p_edlevel)
dat %>% count(white_british, white_other, pakistan_bangladesh, indian, chinese, black, mixed_race, p_ethnicity)
dat %>% count(no_religion, p_religion)
dat %>% count(soc_class, c1_c2, d_e, p_socgrade)
dat %>% count(income, p_gross_household)
dat %>% count(p_housing, own_outright, own_mortgage, homeowner, social_housing, private_renting)
dat %>% count(non_uk_born, p_country_birth)
dat %>% count(non_voter, p_turnout_2019)
dat %>% count(edu_20plus, p_education_age)
dat %>% count(immigSelf)
dat %>% count(full_time, p_work_stat)
dat %>% count(disabled, p_disability)
dat %>% count(cohabiting, p_marital)

dat <- dat %>% 
  mutate(
    education_age = as.factor(p_education_age),
    log_age = log(age_raw),
    log_hh = log(p_hh_size)
  )

dat %>% count(education_age, p_education_age)
dat %>% count(log_hh, p_hh_size)

dat |> count(generalElectionVote, generalElectionVoteNonVoter)

dat <- dat |> 
  mutate(
    brexit_party = case_when(
      generalElectionVote == 12 | generalElectionVoteNonVoter == 12 ~ 1,
      .default = 0
    ),
    brexit_voter = case_when(generalElectionVote == 12 ~ 1, .default = 0)
  )

dat %>% count(brexit_party, brexit_voter, generalElectionVote, generalElectionVoteNonVoter)

##############################################################
# level 2 vars -----------------------------------------------
##############################################################

level_2s <- readRDS("data/level_two_vars_2024.RDS")

la_changes <- read_csv("data/la_boundary_changes.csv")

dat <- dat %>% 
  left_join(la_changes, by = c("la_code" = "la_2021")) %>% 
  mutate(
    la_code = case_when(is.na(la_2024) ~ la_code,
                    !is.na(la_2024) ~ la_2024,
                    .default = "NA")
  ) %>%  
  left_join(level_2s, by = "la_code")

level_twos <- level_2s %>% select(-la_code) %>% names()

# selecting variables --------------------------------------------------

dat <- dat %>% 
  select(id, brexit_party, brexit_voter, tory_2019, la_code, male, uni,
         white_british, white_other, indian, chinese, black, pakistan_bangladesh, mixed_race,
         no_religion, c1_c2, d_e, 
         own_outright, own_mortgage, social_housing, private_renting,
         disabled, cohabiting, education_age,
         e, d, c2, c1, b, pakistan_bangladesh, part_time, unemployed,
         region_fct, log_age, log_hh, 
         age, age_raw, non_uk_born, homeowner, edu_20plus, income, 
         full_time, all_of(level_twos))

# income and uni predictions --------------------------------------

income_preds <- readRDS("data/income_preds_W29.RDS")
income_preds_knn <- readRDS("data/income_knn_W29.RDS")
income_preds_knn <- income_preds_knn %>% rename(income_knn = income)
uni_preds <- readRDS("data/preds_uni_2024.RDS")

dat <- dat %>% 
  left_join(income_preds %>% select(id, pred_el_round), by = "id") %>% 
  left_join(income_preds_knn %>% select(id, income_knn), by = "id") %>% 
  left_join(uni_preds %>% select(id, nn_uni_preds), by = "id") %>% 
  mutate(
    income_full = ifelse(is.na(income), pred_el_round, income),
    income_full_knn = ifelse(is.na(income), income_knn, income),
    uni_full = ifelse(is.na(uni), nn_uni_preds, uni)
  )

sum_na(dat)

# saving data -------------------------------------------------------

saveRDS(dat, file = "data/cross_sectional_dat_2024.RDS")

# summary statistics -----------------------------------------------

summ_vars <- c("male","white_british","white_other","indian","chinese","black",
               "pakistan_bangladesh","mixed_race","no_religion","c1_c2","d_e",
               "own_outright","own_mortgage","social_housing","private_renting",
               "homeowner","no_religion","age_raw","non_uk_born",
               "degree_pct_raw","affordability_raw","prices_raw","pop_density_raw","churn_raw",
               "homeowner_pct_raw","social_rented_pct_raw","non_uk_pct_raw",
               "over_65_pct_raw","under_16_pct_raw","overoccupied_pct_raw","ta_rate_full_raw",
               "uni_full","income_full","brexit_party")

summ_df <- dat |> 
  filter(str_detect(la_code, "^S")==FALSE) |>
  select(all_of(summ_vars))

summ_table <- summ_df |>  
  summarise(across(all_of(summ_vars), 
                   list(mean = ~mean(.,na.rm = T),
                        sd = ~sd(., na.rm = T)),
                   .names = "{.col}.{.fn}")) |> 
  pivot_longer(everything(),
               names_to = c("var","stat"),
               values_to = "value",
               names_sep = "\\.") |>
  pivot_wider(names_from = "stat", values_from = "value") |> 
  mutate(na = summ_df |> map_int(~sum(is.na(.))),
         var = case_when(var == "non_uk_born" ~ "uk_born", .default = var))

write.csv(summ_table, "tables/summary_statistics_2024.csv")            

region_count <- dat %>% 
  count(region_code) %>% 
  na.omit() %>% 
  mutate(prop = n / sum(n))

write.csv(region_count, "tables/regions_n_2024.csv")
