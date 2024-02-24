#-------------------------------------------------------------
#         
#       Trinity College Botanic Garden Monitoring Program 
#                    Herbarium labels
#
#                     Midori Yajima
#
#-------------------------------------------------------------

# Creating labels for herbarium specimens 

# ----------------------------------------------------------
# 1. Setup ----
# ----------------------------------------------------------

# Packages
library(herblabel)
library(readxl)

# Import data 
herb_data <- 
  read.csv(here::here("Data/Herbarium data", 
                      "herbarium_metadata_2022.csv"))  # insert latest spreadsheet here

#------------------------------------------
# 2. Create labels ----
#------------------------------------------

herbarium_label(dat = herb_data,
                spellcheck = F,
                theme = "KUN")

# please, rename the generated rtf file adding the current year 
# and move to "Outputs/Herbarium label" before pushing to this repo


