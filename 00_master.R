# running scripts
rm(list = ls())

# the first four scripts create the data and run the analysis for the opposition to migration models
system("Rscript 01_creating_level_twos_2021.R")
system("Rscript 02_creating_2021_dataset.R")
system("Rscript 03_mi_modelling_2021.R")
system("Rscript 04_mi_prices_2021.R")

# scripts five to eight create the data and run the analysis for the Reform UK voting models
system("Rscript 05_creating_level_twos_2024.R")
system("Rscript 06_creating_2024_dataset.R")
system("Rscript 07_mi_modelling_2024.R")
system("Rscript 08_mi_prices_2024.R")

# script 9 creates and saves the maps used for figure 1
system("Rscript 09_maps.R")

# script 10 conducts the comparisons between censuses for the appendix
system("Rscript 10_between_census_comparisons.R")

# script 11 conducts a complete case analysis on the opposition to migration model i.e. without imputation
system("Rscript 11_cca_2021.R")

# script 12 checks the robustness of the Reform UK voting models to using the APS measure of LAD educational attainment
system("Rscript 12_robustness_aps.R")

# script 13 runs the aggregate level analysis on the EU referendum vote
system("Rscript 13_brexit_mod.R")