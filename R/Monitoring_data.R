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
library(janitor)
library(anytime)

## Upload data
# data for stomatal conductance (porometry)
gs_data <- 
  readxl::read_excel(here::here("Data/Physiology",
                                "gs_2023.xls"), # enter most recent data
  skip = 1,
  col_types = c("date", "numeric", "numeric", "text","text", "numeric", 
                "numeric", "numeric"))

gs_meta <- 
  readxl::read_excel(here::here("Data/Physiology", 
                                "gs_metadata_2023.xlsx"))

# photosynQ data
photosynQ_data <-
  read.csv(here::here("Data/Physiology",
                      "photosynQ_2023.csv")) # enter most recent data

# chlorophyll fluorescence data
chl_data_1 <-
  read.csv(here::here("Data/Physiology",
                      "pocket_pea_27_06_23.csv"), # enter most recent data
           skip = 13) %>% 
  janitor::remove_empty("cols")

chl_data_2 <-
  read.csv(here::here("Data/Physiology",
                      "pocket_pea_28_06_23.csv"), # enter most recent data
           skip = 13) %>% 
  janitor::remove_empty("cols")

chl_data_3 <-
  read.csv(here::here("Data/Physiology",
                      "pocket_pea_29_06_23.csv"), # enter most recent data
           skip = 13) %>% 
  janitor::remove_empty("cols")

# particular matter data
pm_meta <- 
  readxl::read_excel(here::here("Data/Particular matter", 
                                "PM_metadata_2023.xlsx"))  # enter most recent data

pm_data_list <- 
  list.files(here::here("Data/Particular matter", "PM_data_2023"), # enter most recent data
             full.names = TRUE) %>% 
  lapply(., read.delim, 
         header = FALSE) %>% 
  lapply(., janitor::row_to_names,
         row_number = 1)

#------------------------------------------
# 2. Edit data ----
#------------------------------------------
# Cleaning data to the standards (lowercase, delete spaces, meaningful naming), 
#  keep columns of interest

## gs
gs_data_clean <-
  gs_data %>% 
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

# gs notes
gs_meta_clean <-
  gs_meta %>% 
  tidyr::unite(.,
               notes,
               "Wind","Moisture","General Notes", 
               sep = ",",
               na.rm = TRUE) %>% 
  dplyr::rename(.,
                date = "Date",
                leaf_id = "Leaf",
                leaf_orientation = "Orientation",
                leaf_sun_exposure = "Sun Exposure",
                tree_species = "Tree")

gs_meta_clean$date <-
  as.character(gs_meta_clean$date)

# full dataset
gs_data_full <-
  full_join(gs_data_clean,
            gs_meta_clean,
            by = c("date", "leaf_id")) %>% 
  relocate(leaf_id, tree_species)

## photosynQ
photosynQ_data_clean <-
  photosynQ_data %>% 
  select(-c("ID", "Series", "Repeat", "User")) %>% 
  dplyr::rename(.,
                leaf_id = "Notes",
                date = "time") %>% 
  tidyr::separate(.,
                  date,
                  c("date", "time"),
                  sep = " ")

colnames(photosynQ_data_clean) <-
  gsub("\\.", "_", colnames(photosynQ_data_clean)) %>% 
  tolower(.)

# add tree species
tree_info <-
  gs_data_full %>% 
  dplyr::select("tree_species",
                "leaf_id") %>% 
  unique(.)

# full dataset
photosynQ_data_full <-
  full_join(photosynQ_data_clean,
            tree_info) %>% 
  relocate(leaf_id, date, time, time_of_day, tree_species)

## chlorophyll fluorescence
chl_data_full <-
  rbind(chl_data_1, chl_data_2, chl_data_3) %>% 
  dplyr::rename(.,
                tree_species = "ID")

colnames(chl_data_full) <-
  gsub("\\.", "_", colnames(chl_data_full)) %>% 
  tolower(.)

# Particulate matter
pm_data <- 
  bind_rows(pm_data_list) %>% 
  dplyr::select(-c("Area")) %>% 
  tidyr::unite("pm_area", "Area (?m²)","Area (?m\xb2)", na.rm = T) %>% 
  dplyr::rename(.,
                pm_id = "Id", sub_sample_id = "Field",pm_class = "Class",
                pm_ecd = "ECD (?m)", pm_perimeter = "Perimeter (?m)", 
                pm_shape = "Shape", pm_count = "Count")
                # o = "O Wt%",na = "Na Wt%",mg = "Mg Wt%",
                # al = "Al Wt%",si = "Si Wt%",k = "K Wt%",ca = "Ca Wt%",ti = "Ti Wt%",
                # cr = "Cr Wt%",mn = "Mn Wt%",fe = "Fe Wt%",ni = "Ni Wt%",br = "Br Wt%",
                # cl = "Cl Wt%",be = "Be Wt%",sb = "Sb Wt%",'in' = "In Wt%",p = "P Wt%",
                # s = "S Wt%",ta = "Ta Wt%",rb = "Rb Wt%",zn = "Zn Wt%",tm = "Tm Wt%",
                # mo = "Mo Wt%",ru = "Ru Wt%")
     
# keep only information on samples that were analysed
pm_meta_clean <-
  pm_meta %>%
  filter(is.na(notes)| notes!= "Whole sample charging, impossible to analyse") %>% 
  rename(.,
         tree_species = "tree", leaf_id = "leaf id")

# full dataset
pm_data_full <-
  right_join(pm_data,
             pm_meta_clean,
             by = "sub_sample_id") %>% 
  relocate(pm_id, leaf_id, sample_id, sub_sample_id, tree_species)
  

pm_data_full[is.na(pm_data_full)] <- 0

#------------------------------------------
# 2. Save data ----
#------------------------------------------

readr::write_csv(gs_data_full, 
          here::here("Outputs/Data/Physiology", "Tcbg_porometry_2023.csv"))

readr::write_csv(photosynQ_data_full, 
                 here::here("Outputs/Data/Physiology", "Tcbg_photosynq_2023.csv"))

readr::write_csv(chl_data_full, 
                 here::here("Outputs/Data/Physiology", "Tcbg_fluorometry_2023.csv"))

readr::write_csv(pm_data_full,
                 here::here("Outputs/Data/Particulate matter", "Tcbg_pm_2023.csv"))
