#---------------------------------------------
#
#     Botanic Garden Monitoring Program
#
#               Cleaning data
#             
#               Midori Yajima     
#
#---------------------------------------------

# Managing data from the field to make them interoperable 

#------------------------------------------
# 1. Set up ----
#------------------------------------------

# Packages
library(tidyverse)
library(readxl)
library(here)
library(lubridate)
library(measurements)

# Upload data
por_data <- readxl::read_excel(
  here::here("Data","Porometry.xlsx"), 
  skip = 1,
  col_types = c("date", "numeric", "numeric", "text","text", "numeric", "numeric", "numeric"))

por_meta <- readxl::read_excel(
  here::here("Data", "Porometry_metadata.xlsx"))

pm_meta <- readxl::read_excel(
  here::here("Data", "PM_metadata.xlsx"))

herb_data <- read.csv(
  here::here("Data", "Herbarium_metadata.csv"))

#------------------------------------------
# 2. Edit data ----
#------------------------------------------

# Cleaning data to the standards (lowercase, delete spaces, meaningful naming), 
#  keep columns of interest
por_data_clean <-
  por_data %>% 
  dplyr::rename(.,
                date = "Measurement Time",
                gs = "mmol/(m²·s)",
                temp = "°C",
                leaf_id = "...4",
                sensor_serial_number = "Serial Number",
                sensor_cal_number =  "CalNum",
                leaf_sensor_rh =  "% RH...7",
                filter_sensor_rh =  "% RH...8") %>%
  tidyr::separate(.,
                date,
                c("date", "time"),
                sep = " ")


por_meta_clean <-
  por_meta %>% 
  dplyr::select("Date","RH readings (%)","Temperature readings (°C)",
                "Leaf ID",
                "Leaf Direction",
                "PAR per Leaf",
                "Sun Exposure",
                "Health of Leaf",
                "Factors Influencing Porometer",
                "Notes") %>% 
  tidyr::unite(.,
               notes,
               "Factors Influencing Porometer",
               "Notes",
               sep = ",",
               na.rm = TRUE) %>% 
  dplyr::rename(.,
                date = "Date",
                environment_rh ="RH readings (%)",
                environment_temp = "Temperature readings (°C)",
                leaf_id = "Leaf ID",
                leaf_orientation = "Leaf Direction",
                leaf_par = "PAR per Leaf",
                leaf_sun_exposure = "Sun Exposure",
                leaf_appearance = "Health of Leaf")

por_meta_clean$date <- as.character(por_meta_clean$date)


tree_info <-
  pm_meta %>% 
  dplyr::select("Tree",
                "Leaf ID") %>% 
  dplyr::rename(.,
                specimen_specie = "Tree",
                leaf_id = "Leaf ID")

tree_coord <-
  herb_data %>% 
  dplyr::select("GENUS","SPECIES","LAT_DEGREE","LAT_MINUTE","LAT_SECOND",#"LAT_FLAG",
                "LON_DEGREE","LON_MINUTE","LON_SECOND") %>% #,"LON_FLAG") %>% 
  tidyr::unite(.,
               specimen_specie,
               "GENUS","SPECIES",
               sep = " ",
               na.rm = TRUE) %>% 
  tidyr::unite(.,
               lat,
               "LAT_DEGREE","LAT_MINUTE","LAT_SECOND",#"LAT_FLAG",
               sep = " ",
               na.rm = TRUE) %>% 
  tidyr::unite(.,
               lon,
               "LON_DEGREE","LON_MINUTE","LON_SECOND",#"LON_FLAG",
               sep = " ",
               na.rm = TRUE) 

tree_coord$lat <-conv_unit(tree_coord$lat, 
                           from = "deg_min_sec", 
                           to = "dec_deg") %>% 
  as.numeric()

tree_coord$lon <-conv_unit(tree_coord$lon, 
                           from = "deg_min_sec", 
                           to = "dec_deg") %>% 
  as.numeric() 
  
tree_coord$lon <- tree_coord$lon*(-1)

  
# Full dataset
por_data_full <-
  full_join(tree_info,
            tree_coord,
            by = "specimen_specie") %>% 
  full_join(.,
            por_data_clean,
            by = "leaf_id") %>% 
  full_join(.,
            por_meta_clean,
            by = c("date", "leaf_id")) 
  

#------------------------------------------
# 2. Save data ----
#------------------------------------------

write_csv(por_data_full, 
          here::here("Outputs", "Data", "Tcbg_porometry_2022-23-11.csv"))
