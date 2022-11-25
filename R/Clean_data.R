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
library(purrr)

# Upload data
por_data <- 
  readxl::read_excel(here::here("Data","Porometry.xlsx"), 
  skip = 1,
  col_types = c("date", "numeric", "numeric", "text","text", "numeric", 
                "numeric", "numeric"))

por_meta <- 
  readxl::read_excel(here::here("Data", "Porometry_metadata.xlsx"))

pm_meta <- 
  readxl::read_excel(here::here("Data", "PM_metadata.xlsx"))

herb_data <- 
  read.csv(here::here("Data", "Herbarium_metadata.csv"))

pm_data_list <- 
  list.files(here::here("Data", "PM_data"), 
             full.names = TRUE) %>% 
  lapply(., read_excel)
  
pm_data_list[[65]][["Id"]] <- 
  as.double(pm_data_list[[65]][["Id"]])

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

pm_data <- 
  bind_rows(pm_data_list) %>% 
  dplyr::select(-c("Area")) %>% 
  dplyr::rename(.,
                pm_id = "Id",sample_id = "Field",pm_class = "Class",
                pm_area = "Area (μm²)", pm_ecd = "ECD (μm)",
                pm_perimeter = "Perimeter (μm)",pm_shape = "Shape",
                pm_count = "Count",o = "O Wt%",na = "Na Wt%",mg = "Mg Wt%",
                al = "Al Wt%",si = "Si Wt%",k = "K Wt%",ca = "Ca Wt%",ti = "Ti Wt%",
                cr = "Cr Wt%",mn = "Mn Wt%",fe = "Fe Wt%",ni = "Ni Wt%",br = "Br Wt%",
                cl = "Cl Wt%",be = "Be Wt%",sb = "Sb Wt%",'in' = "In Wt%",p = "P Wt%",
                s = "S Wt%",ta = "Ta Wt%",rb = "Rb Wt%",zn = "Zn Wt%",tm = "Tm Wt%",
                mo = "Mo Wt%",ru = "Ru Wt%")
     
pm_meta_clean <-
  pm_meta %>% 
  dplyr::select("Tree","Leaf ID","Sample ID","Notes on leaves") %>% 
  dplyr::rename(.,
                specimen_specie = "Tree",leaf_id = "Leaf ID",
                sample_id = "Sample ID", notes = "Notes on leaves") %>% 
  dplyr::slice(rep(1:n(), 3)) %>% 
  arrange(.,sample_id) %>% 
  dplyr::mutate(sample_n = rep(c("A", "B", "C"), (nrow(.)/3)))%>% 
  tidyr::unite(.,sample_id,
               "sample_id", "sample_n",
               sep = "")

# Full dataset 
#  for stomatal conductance
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

#  for particulate matter
pm_data_full <-
  full_join(pm_meta_clean,
            tree_coord,
            by = "specimen_specie") %>% 
  right_join(.,
             pm_data,
             by = "sample_id")

#------------------------------------------
# 2. Save data ----
#------------------------------------------

readr::write_csv(por_data_full, 
          here::here("Outputs", "Tcbg_porometry_2022-23-11.csv"))

readr::write_csv(pm_data_full,
                 here::here("Outputs", "Tcbg_pm_2022-25-11.csv"))
