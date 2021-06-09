# This script writes the final_omics_data from soil data import and tidy.R script
# into a CSV file "data/final_omics_data.csv" 
# the NAs from Roli's spreasdsheet are manually copied into this CSV file which 
# is saved as "data/final_omics_data_with_juana.csv"
# this new CSV is loaded into the script and saved as .RDS file
# this entire process has to be repeated everytime new data enter the 
# final_omics_data.rds file (incubation, fraction CN, etc.)
# the entire character

# Author: Itamar Shabtai
# Version: 2020-01-26

# Libraries

  library(tidyverse)
  library(here)

# load data

  df <- read_rds(here("data/final_omics_data.rds"))

  write_csv(df, here("data/final_omics_data.csv")) # un-comment this only for re-copying into new data table

# manual copying of roli's NA's into this CSV
# saved as "data/final_omics_data_with_juana.csv"

  data_with_juana <- read_csv(here("data/final_omics_data_with_juana.csv")) 

  write_rds(file = here("data/final_omics_data_with_juana.rds"), data_with_juana)

