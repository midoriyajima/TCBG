#---------------------------------------------
#
#     Botanic Garden Monitoring Program
#
#               Weather data
#             
#               Midori Yajima     
#
#---------------------------------------------

# Visualising weather data from MEt station at tcbg

#------------------------------------------
# 1. Set up ----
#------------------------------------------

# Packages
library(tidyverse)
library(readxl)
library(here)
library(gridExtra)
library(measurements)

# Upload data
met_data <- 
  readxl::read_excel(here::here("Data", "Botanic_gardens_MET_data_2022.xlsx"), # enter most recent data
                     col_types = c("numeric","date", "text", "numeric", 
                                   "numeric","numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric"))[-1,]

#------------------------------------------
# 2. Edit data ----
#------------------------------------------

# Cleaning data to the standards (lowercase, delete spaces, meaningful naming), 
#  separate date-, take only data of interest

met_data_clean <-
  met_data %>% 
  tidyr::separate(.,
                  Date,
                  c("date", "time"),
                  sep = " ") %>% 
  select(-Time) %>% 
  dplyr::select_all(~gsub("\\s", "_", .)) %>% 
  dplyr::select_all(tolower)

met_data_clean$date <- 
  as.Date(met_data_clean$date)


#------------------------------------------
# 5. Save ----
#------------------------------------------
readr::write_csv(met_data_clean, 
                 here::here("Outputs", "Tcbg_met_2022-07-12.csv"))



