#---------------------------------------------
#
#     Botanic Garden Monitoring Program
#
#               Gs data summary
#             
#               Midori Yajima     
#
#---------------------------------------------

# First look at the data for stomatal conductance

#------------------------------------------
# 1. Set up ----
#------------------------------------------

# Packages
library(tidyverse)
library(readxl)
library(here)
library (kableExtra)
library(ggpubr)
library(vegan)
library(ggpmisc)
library(gghighlight)
library(hrbrthemes)
library(ggthemes)
library(ggimage)
library(ggtext)

# Upload data
gs_22 <- 
  read.csv(here::here("Outputs", "Tcbg_porometry_2022-23-11.csv"))

gs_23 <- 
  read.csv(here::here("Outputs", "Tcbg_porometry_2023-20-09.csv"))

tree_list <-
  readxl::read_excel(here::here("Data", "03_Tree_list.xlsx")) %>% 
  rename(.,
         specimen_specie = "Specimen")
pm_23 <-
  read.csv(here::here("Outputs", "Tcbg_pm_2023-23-09.csv"))

pm_22 <- 
  read.csv(here::here("Outputs", "Tcbg_pm_2022-25-11.csv"))

#------------------------------------------
# 2. Custom Functions ----
#------------------------------------------
plot.highlight.species <-
  function(species_name){
    plot <-
      ggplot() +
      geom_jitter (data = gs_22 %>%
                     group_by(Tree) %>%
                     filter(Tree == species_name) %>% 
                     summarise(meangs = mean(gs)) %>%
                     mutate(year = "2022"),
                   aes(x = year,
                       y = meangs),
                   color = "#8B0A50",
                   size=3, alpha=0.8, width=0.5) +
      geom_jitter (data = gs_23 %>%
                     group_by(Tree) %>%
                     filter(Tree == species_name) %>% 
                     summarise(meangs = mean(gs)) %>%
                     mutate(year = "2023") %>% 
                     add_row (year = "2024") %>%
                     add_row (year = "2025") %>%
                     add_row (year = "2026"),
                   aes(x = year,
                       y = meangs),
                   color = "#8B0A50",
                   size=3, alpha=0.8, width=0.5) +
      geom_jitter (data = gs_22 %>%
                     mutate(year = "2022") %>% 
                     group_by(Tree) %>%
                     filter(Tree == species_name), 
                   aes(x = year, 
                       y = gs),
                   color = "#8B0A50",
                   size=2, alpha=0.1, width=0.3) +
      geom_jitter (data = gs_23 %>%
                     mutate(year = "2023") %>%
                     add_row (year = "2024") %>%
                     add_row (year = "2025") %>%
                     add_row (year = "2026") %>%
                     group_by(Tree) %>% 
                     filter(Tree == species_name),
                   aes(x = year,
                       y = gs),
                   color = "#8B0A50",
                   size=2, alpha=0.1, width=0.3) +
      # scale_color_viridis_d (na.translate = F) +
      theme_ipsum_ps() +
      labs(y= bquote("Stomatal conductance (mmol" ~ m^-2~s^-1*')'), 
           x = "Year") +
      theme(axis.text.x = element_text(angle = 45, 
                                       vjust = 0.5,
                                       hjust=1,),
            strip.text.x = element_text(face = "bold.italic")) 
  
 # gghighlight(specimen_specie == species_name, keep_scales = T, 
 #                 use_direct_label = F )
    return(plot)
  }

plot.highlight.species.pm <-
  function(species_name){
plot <-
    ggplot() +
  geom_jitter (data = pm_22 %>% 
                 filter(tree == species_name) %>% 
                 mutate(year = "2022") %>%
                 add_row (year = "2024") %>%
                 add_row (year = "2025") %>%
                 add_row (year = "2026") %>%
                 count(tree, sample_id, pm_class, year) %>% 
                 group_by(tree, pm_class) %>% 
                 mutate(n_sqmm = (n/0.089587)),
               aes(x = year, y = n_sqmm, color = pm_class),
               #shape = pm_class),
               size=2,
               alpha=0.1,
               width=0.3) +
  geom_jitter (data = pm_23 %>% 
                 filter(tree == species_name) %>% 
                 mutate(year = "2023") %>%
                 add_row (year = "2024") %>%
                 add_row (year = "2025") %>%
                 add_row (year = "2026") %>%
                 count(tree, sub_sample_id, pm_class, year) %>% 
                 group_by(tree, pm_class) %>% 
                 mutate(n_sqmm = (n/0.089587)),
               aes(x = year, y = n_sqmm, color = pm_class),
               #shape = pm_class),
               size=2,
               alpha=0.1,
               width=0.3) +
  geom_jitter (data = pm_22 %>%
                 filter(tree == species_name) %>% 
                 mutate(year = "2022") %>%
                 add_row (year = "2024") %>%
                 add_row (year = "2025") %>%
                 add_row (year = "2026") %>%
                 count(tree, sample_id, pm_class, year) %>% 
                 group_by(tree, pm_class) %>% 
                 mutate(n_sqmm = (n/0.089587)) %>%  
                 group_by(tree, pm_class, year) %>% 
                 summarise(meansqmm = mean(n_sqmm)),
               aes(x = year, y = meansqmm, color = pm_class),
               size=3, 
               alpha=0.8, 
               width=0.3) +
  geom_jitter (data = pm_23 %>%
                 filter(tree == species_name) %>% 
                 mutate(year = "2023") %>%
                 add_row (year = "2024") %>%
                 add_row (year = "2025") %>%
                 add_row (year = "2026") %>%
                 count(tree, sub_sample_id, pm_class, year) %>% 
                 group_by(tree, pm_class) %>% 
                 mutate(n_sqmm = (n/0.089587)) %>%  
                 group_by(tree, pm_class, year) %>% 
                 summarise(meansqmm = mean(n_sqmm)),
               aes(x = year, y = meansqmm, color = pm_class),
               size=3, 
               alpha=0.8, 
               width=0.3) +
  scale_color_manual(values = c("PM10" = "#8B0A50",
                                "PM2.5" = "black"),
                     na.translate = F) +
  theme_ipsum_pub() +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5,
                                   hjust=1)) +
  labs(y= "Number of PM/sqmm", x = "Year", col = "PM category")

return(plot)
  }

save.plot <- 
  function(file_name, plot_name){
    ggplot2::ggsave(
      filename = file_name,
      plot = plot_name,
      width = 15,
      height = 13,
      dpi = 600)
  }

#------------------------------------------
# 3. Edit data----
#------------------------------------------
gs <-
  gs %>% 
  left_join(tree_list) %>% 
  select(-c("Specimen ID", "Lat", "Long")) %>% 
  mutate(source = "garden")

gs_22$leaf_par<-
  as.double(gs_22$leaf_par)

gs_22 <- 
  rename(gs_22, Tree = specimen_specie)

pm_22 <-
  rename(pm_22, tree = specimen_specie)

#-----------------------------------------------
# 4. Stomatal conductance ----
#-----------------------------------------------
# (gs_website_mean_plot <-
#    gs %>%
#    group_by(specimen_specie) %>% 
#    summarise(meangs = mean(gs)) %>% 
#    mutate(year = "2022") %>% 
#    add_row (year = "2023") %>% 
#    add_row (year = "2024") %>%
#    add_row (year = "2025") %>% 
#    add_row (year = "2026") %>% 
#    ggplot(aes(x = year, 
#               y = meangs, 
#               colour = specimen_specie)) +
#    geom_jitter (size=2, 
#                 alpha=0.7, 
#                 width=0.3) +
#    scale_color_viridis_d(na.translate = F) +
#    theme_ipsum() +
#    theme(axis.text.x = element_text(angle = 45, 
#                                     vjust = 0.5,
#                                     hjust=1))) +
#   labs(y= "Stomatal conductance", x = "Year")

# (gs_website_mean_plot <-
#     gs %>%
#     group_by(specimen_specie) %>%
#     summarise(meangs = mean(gs)) %>%
#     mutate(year = "2022") %>%
#     add_row (year = "2023") %>%
#     add_row (year = "2024") %>%
#     add_row (year = "2025") %>%
#     add_row (year = "2026") %>%
#     ggplot(aes(x = year,
#                y = meangs)) +
#     geom_image (aes(image = image),
#                 position = position_stack(vjust = 0.5)) +
#     theme_ipsum() +
#     theme(axis.text.x = element_text(angle = 45,
#                                      vjust = 0.5,
#                                      hjust=1))) +
#    labs(y= "Stomatal conductance", x = "Year")

# (gs_website_complete_plot <-
#     gs %>%
#     mutate(year = "2022") %>% 
#     add_row (year = "2023") %>%
#     add_row (year = "2024") %>%
#     add_row (year = "2025") %>%
#     add_row (year = "2026") %>%
#     group_by(specimen_specie) %>% 
#     ggplot(aes(x = year, 
#                y = gs, 
#                colour = specimen_specie)) +
#     geom_jitter (size=2, 
#                  alpha=0.7, 
#                  width=0.3) +
#     scale_color_viridis_d(na.translate = F) +
#     theme_ipsum() +
#     theme(axis.text.x = element_text(angle = 45, 
#                                      vjust = 0.5,
#                                      hjust=1))) +
#   labs(y= "Stomatal conductance", x = "Year")

(gs_website_complete_mean_plot <-
    ggplot() +
   geom_jitter (data = gs_22 %>%
                  filter(Tree != "Azara microphylla") %>%
                  filter(Tree != "Clethra arborea") %>%
                  group_by(Tree) %>%
                  summarise(meangs = mean(gs)) %>%
                  mutate(year = "2022"),
                aes(x = year,
                    y = meangs),
                color = "#8B0A50",
                size=3, alpha=0.8, width=0.5) +
    geom_jitter (data = gs_23 %>%
                   # filter(specimen_specie != "Azara microphylla") %>%
                   # filter(specimen_specie != "Clethra arborea") %>%
                   group_by(Tree) %>%
                   summarise(meangs = mean(gs)) %>%
                   mutate(year = "2023") %>%
                   # add_row (year = "2023") %>%
                   add_row (year = "2024") %>%
                   add_row (year = "2025") %>%
                   add_row (year = "2026"),
                 aes(x = year,
                     y = meangs),
                     color = "#8B0A50",
                 size=3, alpha=0.8, width=0.5) +
   geom_jitter (data = gs_22 %>%
                  filter(Tree != "Azara microphylla") %>%
                  filter(Tree != "Clethra arborea") %>%
                  mutate(year = "2022") %>%
                  # add_row (year = "2023") %>%
                  add_row (year = "2024") %>%
                  add_row (year = "2025") %>%
                  add_row (year = "2026") %>%
                  group_by(Tree),
                aes(x = year,
                    y = gs),
                color = "#8B0A50",
                size=2, alpha=0.1, width=0.3) +
    geom_jitter (data = gs_23 %>%
                   # filter(specimen_specie != "Azara microphylla") %>%
                   # filter(specimen_specie != "Clethra arborea") %>%
                   mutate(year = "2023") %>%
                   # add_row (year = "2023") %>%
                   add_row (year = "2024") %>%
                   add_row (year = "2025") %>%
                   add_row (year = "2026") %>%
                   group_by(Tree),
                 aes(x = year,
                     y = gs),
                     color = "#8B0A50",
                 size=2, alpha=0.1, width=0.3) +
    # scale_color_viridis_d (na.translate = F) +
    theme_ipsum_ps() +
    facet_wrap(Tree ~ .) +
    theme(axis.text.x = element_text(angle = 45, 
                                     vjust = 0.5,
                                     hjust=1,),
          strip.text.x = element_text(face = "bold.italic")) +
  labs(y= bquote("Stomatal conductance (mmol" ~ m^-2~s^-1*')'),
       x = "Year"))



species <- unique(gs_23$Tree)

list_species <- map(species, plot.highlight.species)

list_species <- list_species %>% set_names(species)

#-----------------------------------------------
# 4. Particulate matter ----
#-----------------------------------------------
(pm_scat_plot <-
    ggplot() +
    geom_jitter (data = pm_22 %>% 
                   filter(tree != "Azara microphylla") %>% 
                   filter(tree != "Clethra arborea") %>% 
                   mutate(year = "2022") %>%
                   add_row (year = "2024") %>%
                   add_row (year = "2025") %>%
                   add_row (year = "2026") %>%
                   count(tree, sample_id, pm_class, year) %>% 
                   group_by(tree, pm_class) %>% 
                   mutate(n_sqmm = (n/0.089587)),
                 aes(x = year, y = n_sqmm, color = pm_class),
                 #shape = pm_class),
                 size=2,
                 alpha=0.1,
                 width=0.3) +
   geom_jitter (data = pm_23 %>%
                  mutate(year = "2023") %>%
                  add_row (year = "2024") %>%
                  add_row (year = "2025") %>%
                  add_row (year = "2026") %>%
                  count(tree, sub_sample_id, pm_class, year) %>%
                  group_by(tree, pm_class) %>%
                  mutate(n_sqmm = (n/0.089587)),
                aes(x = year, y = n_sqmm, color = pm_class),
                #shape = pm_class),
                size=2,
                alpha=0.1,
                width=0.3) +
    geom_jitter (data = pm_22 %>%
                   filter(tree != "Azara microphylla") %>%
                   filter(tree != "Clethra arborea") %>%
                   mutate(year = "2022") %>%
                   add_row (year = "2024") %>%
                   add_row (year = "2025") %>%
                   add_row (year = "2026") %>%
                   count(tree, sample_id, pm_class, year) %>%
                   group_by(tree, pm_class) %>%
                   mutate(n_sqmm = (n/0.089587)) %>%
                   group_by(tree, pm_class, year) %>%
                   summarise(meansqmm = mean(n_sqmm)),
                 aes(x = year, y = meansqmm, color = pm_class),
                 size=3,
                 alpha=0.8,
                 width=0.3) +
   geom_jitter (data = pm_23 %>%
                  mutate(year = "2023") %>%
                  add_row (year = "2024") %>%
                  add_row (year = "2025") %>%
                  add_row (year = "2026") %>%
                  count(tree, sub_sample_id, pm_class, year) %>%
                  group_by(tree, pm_class) %>%
                  mutate(n_sqmm = (n/0.089587)) %>%
                  group_by(tree, pm_class, year) %>%
                  summarise(meansqmm = mean(n_sqmm)),
                aes(x = year, y = meansqmm, color = pm_class),
                size=3,
                alpha=0.8,
                width=0.3) +
   scale_color_manual(values = c("PM10" = "#8B0A50",
                                 "PM2.5" = "black"),
                      na.translate = F) +
#    scale_shape(na.translate = F) +
    theme_ipsum_pub() +
    theme(axis.text.x = element_text(angle = 45, 
                                     vjust = 0.5,
                                     hjust=1),
          strip.text.x = element_text(face = "bold.italic")) +
   facet_wrap(tree ~ .) +
  labs(y= "Number of PM/sqmm", x = "Year", col = "Tree species", shape = "PM category"))


species_pm <- unique(pm_23$tree)

list_species_pm <- map(species_pm, plot.highlight.species.pm)

list_species_pm <- list_species_pm %>% set_names(species_pm)

#------------------------------------------
# 5. Save ----
#------------------------------------------

figure_list <- 
  grep("_plot", names(.GlobalEnv), value = TRUE)

# For the plots not in a list already
figure_list %>% 
  purrr::set_names() %>% 
  purrr::walk(
    .x = .,
    .f = ~ save.plot(
      file_name =  here::here(paste0("Outputs/Figures/",.x,".png")),
      plot_name = get(.x)))

save.plot <- 
  function(file_name, plot_name){
    ggplot2::ggsave(
      filename = file_name,
      plot = plot_name,
      width = 15,
      height = 13,
      dpi = 600)
  }


# For the plots in a list 
## gs
sapply(1:length(list_species), function(i) ggsave(
  path = here::here("Outputs/Figures/"),
  filename = paste0("gs", names(list_species[i]),".png"), 
  plot = list_species[[i]], 
  width = 10,
  height = 8,
  dpi = 600))

## PM
sapply(1:length(list_species_pm), function(i) ggsave(
  path = here::here("Outputs/Figures/"),
  filename = paste0("pm",names(list_species_pm[i]),".png"), 
  plot = list_species_pm[[i]], 
  width = 10,
  height = 8,
  dpi = 600))
