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
  readxl::read_excel(here::here("Data", "Botanic_gardens_MET_data_2022.xlsx"),
                     col_types = c("numeric","date", "text", "numeric", 
                                   "numeric","numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric"))[-1,]
met_phoenix_park_data <-
  read.csv(here::here("Data", "Phoenix_park_MET_eirann_data.csv"),
           skip = 15)

met_dublin_airport_data <-
  read.csv(here::here("Data", "Dublin_airport_MET_eirann_data.csv"),
           skip = 23)

#------------------------------------------
# 2. Custom functions ----
#------------------------------------------
plot.rh.by.date<-
  function(date_from, date_to, title){
    plot<-
      met_data_clean %>% 
      filter(date >=date_from & date <= date_to) %>% 
      ggplot() + 
      geom_point(aes(x = time, y = rh, color = "Garden")) +
      geom_point(data = met_eirann_pp_clean %>%
                   filter(date >=date_from & date <= date_to),
                 aes(x = time, y = rhum, color = "Phoenix Park")) +
      geom_point(data = met_eirann_da_clean %>%
                   filter(date >= date_from & date <= date_to),
                 aes(x = time, y = rhum, color = "Dublin Airport")) +
      facet_wrap(~date) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 10))+
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
      ggtitle(title)
    return(plot)
  }

plot.temp.by.date<-
  function(date_from, date_to, title){
    plot<-
      met_data_clean %>%
      filter(date >=date_from & date <= date_to) %>%
      ggplot() +
      geom_point(aes(x = time, y = temp, color = "Garden")) +
      geom_point(data = met_eirann_pp_clean %>%
                   filter(date >=date_from & date <= date_to),
                 aes(x = time, y = temp, color = "Phoenix Park")) +
      geom_point(data = met_eirann_da_clean %>%
                   filter(date >=date_from & date <= date_to),
                 aes(x = time, y = temp, color = "Dublin Airport")) +
      facet_wrap(~date) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 10))+
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
      ggtitle(title)
    return(plot)
  }

plot.pressure.by.date<-
  function(date_from, date_to, title){
    plot<-
      met_data_clean %>%
      filter(date >=date_from & date <= date_to)  %>% 
      ggplot() + 
      geom_point(aes(x = time, y = pressure, color = "Garden")) +
      geom_point(data = met_eirann_pp_clean %>%
                   filter(date >=date_from & date <= date_to) ,
                 aes(x = time, y = msl, color = "Phoenix Park")) +
      geom_point(data = met_eirann_da_clean %>%
                   filter(date >=date_from & date <= date_to),
                 aes(x = time, y = msl, color = "Dublin Airport")) +
      facet_wrap(~date) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 10))+
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
      ggtitle(title)
    return(plot)
  }

plot.wind.by.date<-
  function(date_from, date_to, title){
    plot<-
      met_data_clean %>%
      filter(date >=date_from & date <= date_to) %>% 
      ggplot() + 
      geom_text(aes(x = time, y = wind_speed, angle=-wind_direction+90, color = "Garden"), label="→")+
      geom_text(data = met_eirann_da_clean %>%
                  filter(date >=date_from & date <= date_to),
                aes(x = time, y = wdsp, angle=-wddir+90, color = "Dublin Airport"),label="→") +
      facet_wrap(~date) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
      ggtitle(title)
    return(plot)
  }

save.plot <- 
  function(file_name, plot_name){
    ggplot2::ggsave(
      filename = file_name,
      plot = plot_name,
      width = 10,
      height = 8,
      dpi = 600)
  }

#------------------------------------------
# 3. Edit data ----
#------------------------------------------

# Cleaning data to the standards (lowercase, delete spaces, meaningful naming), 
#  separate date-, take only data of interest

# Garden data
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

# Data from Ireland metereological service
met_eirann_pp_clean <-
  met_phoenix_park_data %>% 
  tidyr::separate(.,
                  date,
                  c("date", "time"),
                  sep = " ")  

met_eirann_pp_clean$date <-
  as.Date.character(met_eirann_pp_clean$date, "%d/%m/%Y")

met_eirann_pp_clean <-
  met_eirann_pp_clean %>% 
  subset(., date >= "2022-06-1" & date < "2022-09-08")

met_eirann_da_clean <-
  met_dublin_airport_data %>% 
  tidyr::separate(.,
                  date,
                  c("date", "time"),
                  sep = " ")  

met_eirann_da_clean$date <-
  as.Date.character(met_eirann_da_clean$date, "%d/%m/%Y")

met_eirann_da_clean <-
  met_eirann_da_clean %>% 
  subset(., date >= "2022-06-1" & date < "2022-09-08")

met_eirann_da_clean$wdsp<-
  measurements::conv_unit(met_eirann_da_clean$wdsp, "knot", "m_per_sec")

#------------------------------------------
# 4. Plot data ----
#------------------------------------------

(rh_june_plot<-
  plot.rh.by.date("2022-06-1","2022-06-30","Daily June relative humidity"))

(rh_july_plot<-
  plot.rh.by.date("2022-07-01", "2022-07-31", "Daily July relative humidity"))

(rh_aug_plot<-
  plot.rh.by.date("2022-08-01", "2022-08-31", "Daily August relative humidity"))

(temp_june_plot<-
    plot.temp.by.date("2022-06-1","2022-06-30","Daily June temperatures"))

(temp_july_plot<-
    plot.temp.by.date("2022-07-01", "2022-07-31", "Daily July temperatures"))

(temp_aug_plot<-
    plot.temp.by.date("2022-08-01", "2022-08-31", "Daily August temperatures"))

(press_june_plot<-
    plot.pressure.by.date("2022-06-1","2022-06-30","Daily June pressure"))

(press_july_plot<-
    plot.pressure.by.date("2022-07-01", "2022-07-31", "Daily July pressure"))

(press_aug_plot<-
    plot.pressure.by.date("2022-08-01", "2022-08-31", "Daily August pressure"))

(wind_june_plot<-
    plot.wind.by.date("2022-06-1","2022-06-30","Daily June wind"))

(wind_july_plot<-
    plot.wind.by.date("2022-07-01", "2022-07-31", "Daily July wind"))

(wind_aug_plot<-
    plot.wind.by.date("2022-08-01", "2022-08-31", "Daily August wind"))


#------------------------------------------
# 5. Save ----
#------------------------------------------

figure_list <- 
  grep("_plot", names(.GlobalEnv), value = TRUE)


figure_list %>% 
  purrr::set_names() %>% 
  purrr::walk(
    .x = .,
    .f = ~ save.plot(
      file_name =  here::here(paste0("Outputs/Figures/",.x,".png")),
      plot_name = get(.x)))

readr::write_csv(met_data_clean, 
                 here::here("Outputs", "Tcbg_met_2022-07-12.csv"))



