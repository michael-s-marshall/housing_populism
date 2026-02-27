rm(list = ls())

# running scripts
system("Rscript 00_creating_level_twos_2021.R")
system("Rscript 01_income_predictor_2021.R")
system("Rscript 02_uni_predictor_2021.R")
system("Rscript 03_creating_2021_dataset.R")
system("Rscript 04_modelling_2021.R")

system("Rscript 05_creating_level_twos_2024.R")
system("Rscript 06_income_predictor_2024.R")
system("Rscript 07_uni_predictor_2024.R")
system("Rscript 08_creating_2024_dataset.R")
system("Rscript 09_modelling_2024.R")

system("Rscript 10_maps.R")

system("Rscript 12_between_census_comparisons.R")
system("Rscript 13_multiple_imputation.R")