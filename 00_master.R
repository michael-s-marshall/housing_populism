rm(list = ls())

# running scripts
system("Rscript 01_creating_level_twos_2021.R")
system("Rscript 02_creating_2021_dataset.R")
system("Rscript 03_multiple_imputation_2021.R")
system("Rscript 04_mi_prices_2021.R")

system("Rscript 05_creating_level_twos_2024.R")
system("Rscript 06_creating_2024_dataset.R")
system("Rscript 07_multiple_imputation_2024.R")
system("Rscript 08_mi_prices_2024.R")

system("Rscript 09_maps.R")
system("Rscript 10_between_census_comparisons.R")
