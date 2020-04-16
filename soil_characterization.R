library(tidyverse)
library(raster)
# import data and remove NAs
sample_wet_and_dry_weights <- read_csv("sample_wet_and_dry_weights.csv", 
                         col_types = cols(horizon = col_factor(levels = c("o","a", "b", "c", "e")), 
                                                   rocky = col_factor(levels = c("y", "n"))))
# as.numeric <- sample_wet_and_dry_weights$unique_ID
View(sample_wet_and_dry_weights.csv)
tidy_sample_weights <- drop_na(sample_wet_and_dry_weights, bag_wet_soil)

View(tidy_sample_weights)
# drop samples from e horizon
#tidy_data_file <- tidy_data_file 
 # filter(horizon != "e") 
  
# calculate GWC, db, VWC, HWC
tin_weight <- 6.7
core_volume <- 79.52156404
water_content_bulk_density <- tidy_sample_weights %>% 
  mutate(wet_weight = bag_wet_soil - bag_weight,
         ovendry_weight = tin_ovendry_weight - tin_weight,
         GWC = (wet_weight - ovendry_weight)/ovendry_weight,
         db = ovendry_weight/core_volume,
         VWC = GWC*db,
         HWC = (tin_airdry_weight - tin_ovendry_weight)/ovendry_weight)
ggplot(water_content_bulk_density, aes(db)) + geom_freqpoly(bins = 50) + xlab("Bulk density")
ggplot(water_content_bulk_density, aes(db, GWC)) + geom_point(aes(color = horizon)) +
  xlab("Bulk density") +
  ylab("Gravimetric water content")

# calculate mean bulk density and water contents and their CV
mean_values <- water_content_bulk_density %>%
  group_by(unique_ID) %>% #add corrected unique_ID
  mutate(mean_db = mean(db),
         cv_db = cv(db),
         mean_GWC = mean(GWC),
         cv_GWC = cv(GWC),
         mean_VWC = mean(VWC),
         cv_VWC = cv(VWC),
         mean_HWC = mean(HWC),
         cv_HWC = cv(HWC),
        GWC_err = GWC/mean_GWC,
         db_err = db/mean_db)   
  #filter(#db_err < 1.15 & db_err >0.85,
         #GWC_err < 1.25 & GWC_err >0.75) 
 # mutate(cv_GWC2 = cv(GWC))

collapsed_mean_values <- mean_values %>% 
  drop_na(GWC) %>%
  drop_na(HWC) %>% 
  group_by(unique_ID, pedon_ID, horizon, location, rocky) %>% 
  summarise(mean_GWC = mean(GWC),
            mean_HWC = mean(HWC),
            mean_db = mean(db),
            mean_VWC = mean(VWC))


ggplot(mean_values, aes(mean_GWC)) + 
  geom_histogram(aes(fill = horizon, position = "fill")) +
  xlab("mean_GWC") +
  ylab("count")

# import and clean DOC and MBC data from Shimadzu
TOC_summary <- read_csv("C:/Users/itama/Onedrive/Data/National soils project/TOC_summary_only_raw_data.csv")
TOC_summary <- drop_na(TOC_summary, unique_ID)

 # Normalize ppm to mg per mass of wet soil
MBC_summary <- TOC_summary %>%
  mutate(NPOC_per_g_wet = NPOC*.05/unfum_weight,
         TN_per_g_wet = TN*.05/unfum_weight,
         fum_NPOC_per_g_wet = fum_NPOC*.05/fum_weight,
         fum_TN_per_g_wet = fum_TN*.05/fum_weight,
        MBC_per_g_wet = fum_NPOC_per_g_wet - NPOC_per_g_wet,
        MBN_per_g_wet = fum_TN_per_g_wet - TN_per_g_wet)

# calculate means
MBC_means <- MBC_summary %>% 
  drop_na() %>% 
  group_by(unique_ID, Hor, State) %>% 
  summarise(mean_NPOC_per_g_wet = mean(NPOC_per_g_wet),
            mean_TN_per_g_wet = mean(TN_per_g_wet),
            mean_MBC_per_g_wet = mean(MBC_per_g_wet),
            mean_MBN_per_g_wet = mean(MBN_per_g_wet)
            )

MBC_all_reps_means <- MBC_summary %>% 
  group_by(unique_ID, Hor, State) %>% 
  mutate(mean_NPOC_per_g_wet = mean(NPOC_per_g_wet),
            mean_TN_per_g_wet = mean(TN_per_g_wet),
            mean_MBC_per_g_wet = mean(MBC_per_g_wet),
            mean_MBN_per_g_wet = mean(MBN_per_g_wet),
            CV_MBC = cv(MBC_per_g_wet),
            CV_MBN = cv(MBN_per_g_wet),
            CV_NPOC = cv(NPOC_per_g_wet),
            CV_TN = cv(TN_per_g_wet)
  )

# combine tbl with bulk density and GWC with tbl with MBC data
combined_data <- inner_join(collapsed_mean_values, MBC_means, by = "unique_ID", .keep_all = TRUE) %>% 
  dplyr::select(unique_ID, horizon, Hor, pedon_ID, everything()) %>% 
  filter(mean_GWC > 0)  %>% #drop filter and add GWC once available
  mutate(mean_NPOC_dry = mean_NPOC_per_g_wet/(1 + mean_GWC),
         mean_TN_dry = mean_TN_per_g_wet/(1 + mean_GWC),
         mean_MBC_dry = mean_MBC_per_g_wet/(1 + mean_GWC),
         mean_MBN_dry = mean_MBN_per_g_wet/(1 + mean_GWC),
         MBC_MBN = mean_MBC_dry / mean_MBN_dry
         )  
# import pedon data (order, elevation, climate, vegetation)
pedon_data <- read_csv("C:/Users/itama/Box/Data/National soils project/pedon_data.csv", 
                       col_types = cols(pedon_ID = col_integer())) %>% 
  distinct()
combined_pedon_data <- inner_join(combined_data, pedon_data)

 
ggplot(data = combined_pedon_data, aes(order, mean_MBC_dry, color = biome)) + geom_boxplot()
ggplot(data = combined_pedon_data, aes(horizon, mean_MBC_dry, color = biome)) + geom_boxplot() 
ggplot(data = combined_pedon_data, aes(MAP_mm, mean_MBC_dry, color = horizon)) + 
  geom_point()
