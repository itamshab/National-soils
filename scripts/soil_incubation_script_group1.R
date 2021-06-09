# This script imports incubation data from excel file saves data to .rds files and plots cumulative mineralization plots

# Author: Itamar Shabtai
# Version: 2020-01-26
# Libraries

library(tidyverse)
library(here)
library(readxl)

# files----

data_file_in <- here("raw-data/Soil respiration/national_soil_respiration_group1.xlsx")

metadata_file_in <- here("raw-data/Soil respiration/national_soil_respiration_group1.xlsx")

file_out <- here("data/national_soil_respiration_group1.rds")

wide_file_out <- here("data/wide_national_soil_respiration_group1.rds")

# plot theme----

mytheme <- theme_bw() + theme(panel.grid = element_blank()) + 
  theme(axis.text.x = element_text(size = 15)) +  
  theme(axis.text.y = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 17)) +
  theme(axis.title.y = element_text(size = 17)) +
  theme(legend.text = element_text(size = 17)) +
  theme(legend.title = element_text(size = 17)) +
  theme(strip.text.x = element_text(size = 17))


# ============================================================================

# import data----

raw_data <- read_excel(data_file_in, sheet = 4, col_names = TRUE) 
raw_data[raw_data<0] <- 0 # change all negative values to zero

metadata <- read_excel(data_file_in, sheet = 2, 
                       col_names = TRUE, 
                       range = "A1:K67") # change range according to sample number
  

# transform to long data structure----

long_data <- pivot_longer(raw_data, c(2:5), #edit columns to include additional dates
             names_to = "Days",values_to = "CO2_mL") 

long_data$Days <- as.numeric(as.character(long_data$Days))  


# calculate mg of CO2 carbon using ideal gas equation----

long_data_mg_co2_c <- long_data %>% 
  mutate(mg_co2_c = (CO2_mL/(295.15*0.08205))*12.01) 


# combine metadata with incubation data----

long_data_joined <- inner_join(metadata, long_data_mg_co2_c, by = "jar_ID") %>% 
  rename(emission = "mg_co2_c") %>% 
  
  
# calculate mineralization (mg C/g soil) and mineralizability (mg C/g soil C)----

mutate(mineralization = emission / soil_g,
       mineralizability = mineralization / (soil_c/soil_g)) %>% 
  
# calculate cumulative mineralization and mineralizability values----

group_by(jar_ID) %>% 
  replace_na(list(emission = 0, mineralization = 0, mineralizability = 0)) %>%
  mutate(cumu_mineralization = cumsum(mineralization),
         cumu_emission = cumsum(emission),
         cumu_mineralizability = cumsum(mineralizability)) 


# calculate mean, sd and se of cumulative values----

mean_data <- long_data_joined %>% 
  group_by(unique_ID, Days) %>%        
  summarize(mean_cumu_emission = mean(cumu_emission),
            mean_cumu_mineralization = mean(cumu_mineralization),
            mean_cumu_mineralizability = mean(cumu_mineralizability),
            sd_cumu_emission = sd(cumu_emission),
            se_cumu_emission = sd_cumu_emission/sqrt(3),
            sd_cumu_mineralization = sd(cumu_mineralization),
            se_cumu_mineralization = sd_cumu_mineralization/sqrt(3),
            sd_cumu_mineralizability = sd(cumu_mineralizability),
            se_cumu_mineralizability = sd_cumu_mineralizability/sqrt(3))


# join mean data with metadata----

short_metadata <- metadata %>% # removes duplicate rows
  distinct(unique_ID, .keep_all = TRUE)

joined_mean_data <- left_join(mean_data, short_metadata, by = "unique_ID") %>%
  select(-qorpak_g, -rep, -jar_ID, -soil_g, -qorpak_soil_g, -WHC, -water_g) %>% 
  relocate(unique_ID, pedon_id, horizon, soil_c, everything()) %>% 
  rename(emission = mean_cumu_emission,
         min = mean_cumu_mineralization, 
         min_c = mean_cumu_mineralizability,
         se_emission = se_cumu_emission,
         se_min = se_cumu_mineralization,
         se_min_c = se_cumu_mineralizability)

write_rds(joined_mean_data, file_out)


# plots----

min_c_plot_group1 <- ggplot(joined_mean_data, aes(Days, min_c, group = unique_ID)) + 
  geom_point(aes(fill = unique_ID), size = 4, shape = 21, alpha = 0.2) +
  geom_errorbar(aes(Days, min_c,
                    ymin = min_c - se_min_c, 
                    ymax = min_c + se_min_c),
                width = 0.1) + 
  geom_line(aes(color = unique_ID)) +
    mytheme + labs(y = expression("Cumulative mineralizability (mg CO"[2]*"/g SOC)"),
                 fill = "unique_ID") +
  theme(legend.position = "right")

min_c_plot_group1

ggsave("./eda/group1_min_c.svg", plot = min_c_plot_group1)


min_plot_group1 <- ggplot(joined_mean_data, aes(Days, min, group = unique_ID)) + 
  geom_point(aes(fill = unique_ID), size = 4, shape = 21, alpha = 0.2) +
  geom_errorbar(aes(Days, min,
                    ymin = min - se_min, 
                    ymax = min + se_min),
                width = 0.1) + 
  geom_line(aes(color = unique_ID)) +
  mytheme + labs(y = expression("Cumulative mineralization (mg CO"[2]*"/g soil)"),
                 fill = "unique_ID") +
  theme(legend.position = "right")

min_plot_group1

ggsave("./eda/group1_min.svg", plot = min_plot_group1)

