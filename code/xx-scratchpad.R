## mapping test ###############################################################
library(sf)
library(tidyverse)


uc <- st_read("data/01-raw/maps/Local_Administrative_Units_Level_1_January_2018_Ultra_Generalised_Clipped_Boundaries_in_United_Kingdom.shp")

uc %>% 
  filter(grepl("^E", lau118cd)) -> eng
plot(st_geometry(eng))

uc %>% 
  filter(grepl("^S", lau118cd)) -> sco
plot(st_geometry(sco))


