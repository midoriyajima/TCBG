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
tcbg <- 
  read.csv(here::here("Data", "Herbarium_metadata.csv"))

#------------------------------------------
# 2. Create labels ----
#------------------------------------------

herbarium_label(dat = tcbg,
                spellcheck = F,
                theme = "KUN")


