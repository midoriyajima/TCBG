#---------------------------------------------
#
#     Botanic Garden Monitoring Program
#
#               Cleaning TRY data
#             
#                Midori Yajima     
#
#---------------------------------------------

# Managing TRY database data for tcbg trees species
#  retrieved from https://www.try-db.org/

#------------------------------------------
# 1. Set up ----
#------------------------------------------

# Packages
library(tidyverse)
library(readxl)
library(here)

# Upload data
try_data <- 
  read.delim(here::here("Data","TRY_traits.txt"),
             skip = 3) %>% 
  select(-X)


try_data_2 <- 
  read.delim(here::here("Data","TRY_traits_2.txt"),
             skip = 3) %>% 
  select(-X)

#------------------------------------------
# 2. Edit data ----
#------------------------------------------

try_data_full <-
  full_join(try_data, try_data_2)

names(try_data_full) <-
  gsub(".", " ", names(try_data_full), fixed = TRUE)

names(try_data_full) <-
  gsub("^\\s+|\\s+$", "", names(try_data_full))

#------------------------------------------
# 2. Save data ----
#------------------------------------------
readr::write_csv(try_data_full, 
                 here::here("Outputs", "Try_data.csv"))
