##########################################################################
# creating cross-sectional dataset ---------------------------------------
##########################################################################

# packages ---------------------------------------------------------------

pacman::p_load(tidyverse, haven)

rm(list = ls())

# BES wave 22 ------------------------------------------------------------

dat <- read_dta("data/panel_data/BES2019_W22_v24.0.dta")

# scaling functions --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

##############################################################
# level 1 vars -----------------------------------------------
##############################################################

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
      "Other" = c("7","8")
    ),
    non_uk_born = fct_lump_n(as.factor(p_country_birth), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    tory_2019 = fct_lump_n(as.factor(p_past_vote_2019), n = 1)
  ) %>% 
  rename(la_code = oslaua_code)

dat$male <- ifelse(dat$gender == 1, 1, 0)
dat$income <- ifelse(dat$p_gross_household %in% c(16, 17), 
                     NA, dat$p_gross_household)
dat$age_raw <- dat$age
dat$age <- scale_this(dat$age)
dat$own_outright <- ifelse(dat$p_housing == 1, 1, 0)
dat$private_renting <- ifelse(dat$p_housing == 4, 1, 0)
dat$social_housing <- ifelse(dat$p_housing == 5 | dat$p_housing == 6, 1, 0)
dat$own_mortgage <- ifelse(dat$p_housing == 2, 1, 0)
dat$homeowner <- ifelse(dat$own_outright == 1 | dat$own_mortgage == 1, 1, 0)
dat$edu_20plus <- ifelse(dat$p_education_age == 5, 1, 0)
dat$edu_20plus[is.na(dat$p_education_age)] <- NA
dat$edu_15 <- ifelse(dat$p_education_age == 1, 1, 0)
dat$edu_15[is.na(dat$p_education_age)] <- NA
dat$edu_16 <- ifelse(dat$p_education_age == 2, 1, 0)
dat$edu_16[is.na(dat$p_education_age)] <- NA
dat$full_time <- ifelse(dat$p_work_stat == 1, 1, 0)
dat$unemployed <- ifelse(dat$p_work_stat == 6, 1, 0)
dat$unemployed[is.na(dat$p_work_stat)] <- NA
dat$part_time <- ifelse(dat$p_work_stat %in% c(2, 3), 1, 0)
dat$part_time[is.na(dat$p_work_stat)] <- NA
dat$retired <- ifelse(dat$p_work_stat == 5, 1, 0)
dat$retired[is.na(dat$p_work_stat)] <- NA
dat <- dat %>% 
  mutate(low_income = ifelse(p_gross_household < 5, 1, 0) %>% as.factor)
dat$low_income[is.na(dat$income)] <- NA
dat$disabled <- ifelse(dat$p_disability %in% c(1, 2), 1, 0)
dat$disabled[is.na(dat$p_disability)] <- NA
dat$p_hh_size <- ifelse(dat$p_hh_size %in% c(9, 10), 
                        NA, dat$p_hh_size)
dat$cohabiting <- ifelse(dat$p_marital %in% c(1, 2, 4), 1, 0)
dat$cohabiting[is.na(dat$p_marital)] <- NA
dat$soc_class[dat$soc_class == "Other"] <- NA
dat$c1_c2 <- ifelse(dat$soc_class == "C1-C2", 1, 0)
dat$d_e <- ifelse(dat$soc_class == "D-E", 1, 0)
dat$pakistan_bangladesh <- ifelse(dat$p_ethnicity == 8|dat$p_ethnicity ==  9, 1, 0)
dat$black <- ifelse(dat$p_ethnicity == 11 |dat$p_ethnicity == 12|dat$p_ethnicity == 13, 1, 0)
dat$white_other <- ifelse(dat$p_ethnicity == 2, 1, 0)
dat$indian <- ifelse(dat$p_ethnicity == 7, 1, 0)
dat$chinese <- ifelse(dat$p_ethnicity == 14, 1, 0)
dat$mixed_race <- ifelse(dat$p_ethnicity %in% c(3, 4, 5, 6), 1, 0)
dat$pub_job <- ifelse(dat$p_job_sector == 2, 1, 0)
dat$pub_job[is.na(dat$p_job_sector)] <- NA

dat$immigSelf[dat$immigSelf == 9999] <- NA
dat$immigEcon <- as.factor(dat$immigEcon)
dat$labour <- ifelse(dat$generalElectionVote == 2, 1, 0)
dat$labour[is.na(dat$generalElectionVote)] <- NA
dat$tory <- ifelse(dat$generalElectionVote == 1, 1, 0)
dat$tory[is.na(dat$generalElectionVote)] <- NA
dat$lib_dem <- ifelse(dat$generalElectionVote == 3, 1, 0)
dat$lib_dem[is.na(dat$generalElectionVote)] <- NA
dat$green <- ifelse(dat$generalElectionVote == 7, 1, 0)
dat$green[is.na(dat$generalElectionVote)] <- NA
dat$reform <- ifelse(dat$generalElectionVote == 12, 1, 0)
dat$reform[is.na(dat$generalElectionVote)] <- NA

dat |> 
  ggplot(aes(x = as.factor(immigEcon), y = immigSelf)) +
  geom_boxplot()

dat |> 
  ggplot(aes(x = as_factor(generalElectionVote), y = immigSelf)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

##############################################################
# level 2 vars -----------------------------------------------
##############################################################

level_2s <- readRDS("data/level_two_vars_2021.RDS")

dat <- dat %>% 
  left_join(level_2s, by = "la_code")

level_twos <- level_2s %>% 
  select(-la_code, -private_rented_pct, 
         -private_rented_pct_raw) %>% 
  names()

# selecting variables --------------------------------------------------

dat <- dat %>% 
  select(id, immigSelf, la_code, male, uni,
         white_british, white_other, indian, chinese, black, pakistan_bangladesh, mixed_race,
         no_religion, c1_c2, d_e, 
         social_housing, private_renting, homeowner,
         age, age_raw, non_uk_born, edu_20plus, edu_15, edu_16,
         income,
         full_time, part_time, unemployed, retired,
         pub_job,
         p_hh_size, cohabiting, disabled,
         immigEcon, labour, tory, lib_dem, green, reform,
         all_of(level_twos)
  )

sum_na <- function(dat){
  out <- dat %>% 
    map_int(~sum(is.na(.)))
  return(out)
}

sum_na(dat)

# reordering immigSelf ------------------------------------------------

dat <- dat %>% 
  rename(immigSelf_pro = immigSelf) %>% 
  mutate(immigSelf = 10 - immigSelf_pro)

dat %>% 
  count(immigSelf, immigSelf_pro)

dat <- dat %>% select(-immigSelf_pro)

# saving data -------------------------------------------------------

saveRDS(dat, file = "data/cross_sectional_dat_2021.RDS")

# summary statistics -----------------------------------------------

summ_vars <- c("male","white_british","white_other","indian","chinese","black",
               "pakistan_bangladesh","mixed_race","no_religion","c1_c2","d_e",
               "social_housing","private_renting",
               "homeowner","no_religion","age_raw","non_uk_born",
               "degree_pct_raw","affordability_raw","prices_raw","pop_density_raw",
               "pop_density_change_raw", "rent_raw",
               "homeowner_pct_raw","social_rented_pct_raw","non_uk_pct_raw",
               "over_65_pct_raw","under_16_pct_raw","overoccupied_pct_raw","ta_rate_full_raw",
               "underoccupied_pct_raw","uni","income",
               "uni","income",
               "immigSelf")

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
  
write.csv(summ_table, "tables/summary_statistics_2021.csv")            

region_count <- dat %>% 
  count(region_code) %>% 
  na.omit() %>% 
  mutate(prop = n / sum(n))

write.csv(region_count, "tables/regions_n_2021.csv")

# missing observations ----------------------------------------

rm(list = ls())

dat <- readRDS("data/cross_sectional_dat_2021.RDS")

# removing level 2s included in the PCA
dat <- dat |> 
  select(-homeowner_pct, -homeowner_pct_raw,
         -overoccupied_pct_raw, -overoccupied_pct,
         -underoccupied_pct, -underoccupied_pct_raw,
         -rent_raw, -rent,
         -ta_rate, -ta_rate_raw, -ta_rate_full, -ta_rate_full_raw) |> 
  rename(LAD = la_code)

sum_na <- function(dat){
  out <- dat %>% 
    map_int(~sum(is.na(.)))
  return(out)
}

sum_na(dat)

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

# missing degree pct is Scotland
missing_las(dat, degree_pct)

# missing from Eng and Wales - 2580
missing_scotland <- nrow(dat) - nrow(dat |> drop_na(degree_pct))
missing_scotland

# 2582 missing from immigself
missing_dv <- nrow(dat) - nrow(dat |> drop_na(affordability, immigSelf)) - missing_scotland
missing_dv

nrow(dat) - (missing_scotland + missing_dv) # 22951 obs remaining

# modelling dataset ------------------------------------------

dat <- dat |> 
  drop_na(degree_pct, affordability)

nrow(dat)

# saving -----------------------------------------

saveRDS(dat, file = "data/modelling_dataset_2021.RDS")
