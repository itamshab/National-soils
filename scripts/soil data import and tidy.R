# This script imports and wrangles all national soils data files into single df

# Author: Itamar Shabtai
# Version: 2020-01-26

# Libraries

library(tidyverse)
library(here)

# Import data files----

omics_soil_metadata <- 
  read_csv("./raw-data/metadata_with_dna.csv",
           col_types = cols(horizon = col_factor(levels = c("a","c"))))

metadata <- 
  read_csv("./raw-data/all_sample_metadata.csv",
           col_types = cols(horizon = col_factor(levels = c("a","b","e", "c","o", "litter"))))

pH_carbonate <- 
  read_csv("./raw-data/pH and carbonate/pH_carbonate_data_summary_final.csv",
           col_types = cols(horizon = col_factor(levels = c("a","c","o"))))

size_fractions <- 
  read_csv("./raw-data/Size fractions/size_fractions_raw_data.csv",
           col_types = cols(horizon = col_factor(levels = c("a","c", "e"))))

moisture <- 
  read_csv("./raw-data/Moisture/soil_moisture_raw_data.csv", 
           col_types = cols(horizon = col_factor(levels = c("a","b", "c", "e", "o"))))

missing_HWC <- 
  read_csv("./raw-data/Moisture/missing_HWC.csv",
           col_types = cols(horizon = col_factor(levels = c("a","b", "c", "e", "o")))) %>% 
  select(1, 5) 

TOC_final_summary <- 
  read_csv("./raw-data/Microbial biomass/TOC_final_summary.csv",
           col_types = cols(horizon = col_factor(levels = c("a","b", "c", "e", "o"))))

HH_extraction <- 
  read_csv("./raw-data/HH extraction/HH_extraction_raw_data.csv")

bulk_soil_CN_isotopes <- 
  read_csv("./raw-data/Elemental and isotopes/bulk_soil_CN_natu_abund_raw_data.csv",
           col_types = cols(horizon = col_factor(levels = c("a","c","e"))))

all_pedon_meta_data <- 
  read_csv("./raw-data/all_pedon_meta_data.csv")

CEC <- 
  read_csv("./raw-data/CEC/CEC_data.csv", 
           col_types = cols(horizon = col_factor(levels = c("a","b", "c", "e", "o")))) %>% 
  select(-1) 

particle_size <- 
  read_csv(here("./raw-data/Particle size distribution/size_data.csv"), 
                          col_types = cols(horizon = col_factor(levels = c("a","c"))))

# Tidy moisture and bulk density----

# calculating gravimetric (GWC), volumetric (VWC), hygroscopic (HWC) water contents, bulk density (db)

core_volume <- 79.52156404 # volume of bulk density cores (cm^3)

moisture_calculated_values <- moisture %>% 
  dplyr::select(2:5,7,9,10,11,12,13) %>% # selecting relevant columns
  #drop_na(bag_wet_soil) %>% # removing missing samples
  #drop_na(tin_airdry_weight) %>% 
  mutate(wet_weight = bag_wet_soil - bag_weight,
         ovendry_weight = tin_ovendry_weight - tin_weight,
         GWC = (wet_weight - ovendry_weight)/ovendry_weight,
         db = ovendry_weight/core_volume,
         VWC = GWC*db,
         HWC = (tin_airdry_weight - tin_ovendry_weight)/ovendry_weight) %>% 
  group_by(unique_ID, pedon_ID, horizon, location) %>% # calculating means from replicates
  summarise(mean_GWC = mean(GWC, na.rm = TRUE),
            mean_HWC = mean(HWC, na.rm = TRUE),
            mean_VWC = mean(VWC, na.rm = TRUE),
            mean_db = mean(db, na.rm = TRUE
            )) 

# Joining with missing_HWC df
moisture_calculated_values <- 
  left_join(moisture_calculated_values, missing_HWC, by = "unique_ID") %>% 
  mutate(mean_HWC = coalesce(mean_HWC.x, mean_HWC.y)) %>% 
  select(-c(6,9))

write_rds(x = moisture_calculated_values, 
          file = here("data/moisture_bulk_density.rds"))

# Tidy pH and carbonate----

# all negative inorganic C assumed equal to zero

# convert inorganic C content to %carbonate in soils

mw_carbonate <- 100.0869 # molar mass of calcium carbonate

mw_c <- 12.0107 # molar mass of C

pH_carbonate_percent <- pH_carbonate %>% 
  mutate(percent_carbonate = mg_IC_g_soil_zero/1000*(mw_carbonate/mw_c)*100)

write_rds(x = pH_carbonate_percent, file = here("data/pH_carbonate.rds"))

# Tidy size fractions----

# calculate fraction amount in g, relative amount and recovery

# silt and clay pan weight were added in excel to avoid NA's

size_fractions_calculated_values <- size_fractions %>% 
  mutate(coarse_fraction_g = sand_and_POM_and_pan_weight_g - sand_pom_pan_weight_g,
         fine_fraction_g = total_sc_and_pan_g - total_sc_pan_g,
         recovery_percent = (coarse_fraction_g + fine_fraction_g) / soil_weight_g * 100,
         relative_coarse_fraction = coarse_fraction_g / soil_weight_g,
         relative_fine_fraction = fine_fraction_g / soil_weight_g,
         fine_coarse_ratio = relative_fine_fraction / relative_coarse_fraction)

write_rds(x = size_fractions_calculated_values, file = here("data/size_fractions.rds"))


# Tidy HH Extraction----

# calculate concentrations of HH-extractable C, Fe and Al per g soil

HH_volume <- 0.01 # extracting solution was 10 mL

HH_dilution <- 10 # all ICP samples diluted 1:9

HH_calculated_values <- HH_extraction %>% 
  mutate(HH_DOC_ppm = npoc_ppm * toc_dilution,
         HH_DOC_mg_g = HH_DOC_ppm * HH_volume / soil_weight_g,
         Fe_mg_g = Fe_ppm * HH_dilution * HH_volume / soil_weight_g,
         Al_mg_g = Al_ppm * HH_dilution * HH_volume / soil_weight_g,
         Fe_plus_Al = Fe_mg_g + Al_mg_g)

write_rds(x = HH_calculated_values, file = here("data/HH_extraction.rds"))


# Tidy SOC---- 

# Subtract IC from TC to find OC

calculated_bulk_CN <- left_join(bulk_soil_CN_isotopes, pH_carbonate_percent)  

calculated_bulk_CN[c("mg_IC_g_soil_zero", "mg_IC_g_soil")][is.na(calculated_bulk_CN[c("mg_IC_g_soil_zero", "mg_IC_g_soil")])] <- 0

calculated_bulk_CN <- calculated_bulk_CN %>% 
  mutate(SOC = percent_C - mg_IC_g_soil_zero*0.1) %>% 
  mutate(TN = percent_N)  

write_rds(x = calculated_bulk_CN, file = here("data/bulk_CN.rds"))


# Tidy microbial biomass C----

NPOC_volume <- 0.05 # extraction volume equals 50 mL

calculated_MBC_DOC <- TOC_final_summary %>% 
  mutate(DOC_mg_g_wet = NPOC * NPOC_volume / unfum_weight,
         DN_mg_g_wet = TN * NPOC_volume / unfum_weight,
         MBC_mg_g_wet = fum_NPOC * NPOC_volume / fum_weight,
         MBN_mg_g_wet = fum_TN * NPOC_volume / fum_weight) %>% 
  group_by(unique_ID, pedon_ID, horizon) %>% 
  summarize(mean_DOC_wet = mean(DOC_mg_g_wet),
            mean_DN_wet = mean(DN_mg_g_wet),
            mean_MBC_wet = mean(MBC_mg_g_wet),
            mean_MBN_wet = mean(MBN_mg_g_wet))

write_rds(x = calculated_MBC_DOC, file = here("data/MBC_DOC.rds"))

# Tidy CEC----

#joins moisture df for unique_ID

CEC_tidy <- CEC %>% 
  left_join(moisture_calculated_values, by = c("horizon", "pedon_ID")) %>% 
  select(-c(10:14)) %>% 
  rename(CEC = Sum) %>% 
  relocate(unique_ID) 

write_rds(x = CEC_tidy, file = here("data/CEC.rds"))

# Tidy mechanical composition----

#joins moisture df for unique_ID

particle_size_tidy <- particle_size %>% 
  left_join(moisture_calculated_values, by = c("horizon", "pedon_ID")) %>% 
  select(-c(1,10:14)) %>% 
  relocate(unique_ID)

write_rds(x = particle_size_tidy, file = here("data/mechanical_composition.rds"))

# Only relevant variable and unique_ID in each dataframe----

ph_carbonate_short <- dplyr::select(pH_carbonate_percent, 1, 6, 8)
CN_short <- dplyr::select(calculated_bulk_CN, 1,5, 6, 14, 15)
HH_short <- dplyr::select(HH_calculated_values, 2, 15, 16:18)
size_short <- dplyr::select(size_fractions_calculated_values, 1, 17, 18,19)
CEC_short <- dplyr::select(CEC_tidy, 1, 4, 5, 6, 7, 8, 9)
particle_short <- dplyr::select(particle_size_tidy, 1, 4, 5, 6, 7, 8)


# joining data frames by unique_ID----

# moisture dataframe has horizons

joined_data <- left_join(metadata, moisture_calculated_values)
joined_data <- left_join(joined_data, ph_carbonate_short) 
joined_data <- left_join(joined_data, CN_short) 
joined_data <- left_join(joined_data, HH_short) 
joined_data <- left_join(joined_data, size_short)
joined_data <- left_join(joined_data, CEC_short)
joined_data <- left_join(joined_data, calculated_MBC_DOC)
joined_data <- left_join(joined_data, particle_short) %>% 
  filter(location != "Stagebarn" | horizon != "c") %>% # remove 13c
  dplyr::select(unique_ID, pedon_ID, horizon, everything())
#joined_data <- read_csv(here("/data/joined_data.csv"))


################################################################################
########RUN THIS CHUNK AFTER ALL MOISTURE DATA COMPLETED########################
################################################################################
# normalizing variable to oven dry weight----

joined_data_moisture_corrected <- joined_data %>% 
  #filter(horizon == c("a", "c")) >% 
mutate(DOC = mean_DOC_wet, #  * (mean_GWC + 1),
         DN = mean_DN_wet, # * (mean_GWC + 1),
         MBC = mean_MBC_wet, # * (mean_GWC + 1),
         MBN = mean_MBN_wet, # * (mean_GWC + 1),
         SOC = SOC, # * (mean_HWC + 1),
         TN = TN, # * (mean_HWC + 1),
         IC = percent_carbonate, # * (mean_HWC + 1),
         HH_DOC = HH_DOC_mg_g, # * (mean_HWC + 1),
         Fe = Fe_mg_g, # * (mean_HWC + 1),
         Al = Al_mg_g, #  * (mean_HWC + 1),
         Fe_Al = Fe_plus_Al) # * (mean_HWC + 1))

# normalizing to unit OC and TN----

final_data <- joined_data_moisture_corrected %>% 
  mutate(MBC_C = MBC / (SOC / 100),
         MBN_N = MBN / (TN / 100),
         DOC_C = DOC / (SOC / 100),
         DN_N = DN / (TN / 100),
         HH_DOC_C = HH_DOC / (SOC / 100)) 

write_rds(x = final_data, file = here("data/normalized_final_data.rds"))

# making a dataframe for the omics project

omics_soil_metadata$pedon_ID <- as.numeric(omics_soil_metadata$pedon_ID)

final_omics_data <- 
  left_join(omics_soil_metadata, final_data, by = c("pedon_ID", "horizon")) %>% 
  write_csv("./data/final_omics_data.csv")

