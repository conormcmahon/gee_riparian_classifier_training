
library(sf)
library(here)
library(tidyverse)
library(janitor)

veg_camp_points <- st_read(here::here("..","VegCampPoints","ds1020.gdb"),
                           layer="ds1020")

nccag_classes <- read_csv(here::here("..","NCCAG","veg_guilds.csv")) %>%
  janitor::clean_names()  
nccag_classes <- nccag_classes %>%
  pivot_longer(2:ncol(nccag_classes), names_to="guild", values_to="dataval") %>% 
  drop_na(dataval) %>% 
  dplyr::select(1:2)

nccag_obligate_riparian <- nccag_classes %>% 
  filter((guild %in% c("hr_t", "h_mr_s", "xr_t")))
nccag_facultative_riparian <- nccag_classes %>%
  filter(!(guild %in% c("hr_t", "h_mr_s", "xr_t", "unclear")))

veg_camp_upland <- veg_camp_points[!(veg_camp_points$NVCS_Name %in% nccag_classes$veg_type),] %>%
  select("NVCS_Name", "SurveyDate") %>%
  mutate(year = as.numeric(substr(SurveyDate, 1,4)),
         hydro_type = "upland",
         code = 3,
         riparian_obligate_code = 2) %>% 
  filter(!grepl("Populus", NVCS_Name),
         !grepl("Platanus", NVCS_Name),
         !grepl("Salix", NVCS_Name),
         !grepl("Prosopis", NVCS_Name),
         !grepl("Willow", NVCS_Name),
         !grepl("Cottonwood", NVCS_Name),
         !grepl("Sycamore", NVCS_Name),
         !grepl("Mesquite", NVCS_Name),
         !grepl("Alnus", NVCS_Name),
         !grepl("Alder", NVCS_Name),
         !grepl("Acer", NVCS_Name),
         !grepl("Maple", NVCS_Name),
         !grepl("Betula", NVCS_Name),
         !grepl("Birch", NVCS_Name),
         !grepl("Walnut", NVCS_Name),
         !grepl("Juglans", NVCS_Name),
         !grepl("Ash", NVCS_Name),
         !grepl("Fraxinus", NVCS_Name),
         !grepl("Riparian", NVCS_Name),
         !grepl("Baccharis salicifolia", NVCS_Name),
         !grepl("Acacia", NVCS_Name),
         !grepl("Chilopsis", NVCS_Name),
         !grepl("Palo", NVCS_Name),
         !grepl("Parkinsonia", NVCS_Name),
         !grepl("Apricot", NVCS_Name),
         !grepl("Prunus", NVCS_Name),
         !grepl("Tamarix", NVCS_Name),
         !grepl("Tamarisk", NVCS_Name),
         !grepl("Hackberry", NVCS_Name),
         !grepl("Celtis", NVCS_Name)) %>%
  drop_na(NVCS_Name)
veg_camp_riparian_facultative <- veg_camp_points[(veg_camp_points$NVCS_Name %in% nccag_facultative_riparian$veg_type),] %>%
  select("NVCS_Name", "SurveyDate") %>%
  filter(!grepl("Populus", NVCS_Name),
         !grepl("Platanus", NVCS_Name),
         !grepl("Salix", NVCS_Name),
         !grepl("Prosopis", NVCS_Name),
         !grepl("Willow", NVCS_Name),
         !grepl("Cottonwood", NVCS_Name),
         !grepl("Sycamore", NVCS_Name),
         !grepl("Mesquite", NVCS_Name),
         !grepl("Alnus", NVCS_Name),
         !grepl("Alder", NVCS_Name),
         !grepl("Acer", NVCS_Name),
         !grepl("Maple", NVCS_Name),
         !grepl("Betula", NVCS_Name),
         !grepl("Birch", NVCS_Name),
         !grepl("Walnut", NVCS_Name),
         !grepl("Juglans", NVCS_Name),
         !grepl("Ash", NVCS_Name),
         !grepl("Fraxinus", NVCS_Name),
         !grepl("Riparian", NVCS_Name),
         !grepl("Baccharis salicifolia", NVCS_Name),
         !grepl("Acacia", NVCS_Name),
         !grepl("Chilopsis", NVCS_Name),
         !grepl("Palo", NVCS_Name),
         !grepl("Parkinsonia", NVCS_Name),
         !grepl("Apricot", NVCS_Name),
         !grepl("Prunus", NVCS_Name),
         !grepl("Tamarix", NVCS_Name),
         !grepl("Tamarisk", NVCS_Name),
         !grepl("Hackberry", NVCS_Name),
         !grepl("Celtis", NVCS_Name)) %>%
  mutate(year = as.numeric(substr(SurveyDate, 1,4)),
         hydro_type = "riparian_facultative",
         code = 2,
         riparian_obligate_code = 2) 
veg_camp_riparian_obligate <- veg_camp_points[(veg_camp_points$NVCS_Name %in% nccag_obligate_riparian$veg_type),] %>%
  select("NVCS_Name", "SurveyDate") %>%
  mutate(year = as.numeric(substr(SurveyDate, 1,4)),
         hydro_type = "riparian",
         code = 1,
         riparian_obligate_code = 1)

veg_camp_output <- rbind(veg_camp_upland, 
                         veg_camp_riparian_facultative, 
                         veg_camp_riparian_obligate)
st_write(veg_camp_output, 
         here::here("..","VegCampPoints"),
         layer="veg_points_all",
         driver="ESRI Shapefile",
         delete_dsn = TRUE)

riparian_subset <- veg_camp_riparian_obligate[sample(1:nrow(veg_camp_riparian_obligate), 1000),]
facultative_riparian_subset <- veg_camp_riparian_facultative[sample(1:nrow(veg_camp_riparian_facultative), 500),]
upland_subset <- veg_camp_upland[sample(1:nrow(veg_camp_upland), 500),]
veg_camp_output_sub <- rbind(riparian_subset, 
                             facultative_riparian_subset, 
                             upland_subset)
veg_camp_output_sub$rprn <- ((veg_camp_output_sub$riparian_obligate_code - 1)*-1)+1
veg_camp_output_sub$guld <- veg_camp_output_sub$code
st_write(veg_camp_output_sub, 
         here::here("..","VegCampPoints"),
         layer="veg_points_sub",
         driver="ESRI Shapefile",
         delete_dsn = TRUE)