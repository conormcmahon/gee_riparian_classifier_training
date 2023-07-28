
library(sf)
library(here)
library(tidyverse)
library(janitor)

# Get all NCCAG data
nccag <- st_read(here::here("..","NCCAG","i02_NCCAG_Vegetation.shp"))
# Read in manually labelled guild types across veg classes 
nccag_classes <- read_csv(here::here("..","NCCAG","veg_guilds.csv")) %>%
  janitor::clean_names()  
nccag_classes <- nccag_classes %>%
  pivot_longer(2:ncol(nccag_classes), names_to="guild", values_to="dataval") %>% 
  drop_na(dataval) %>% 
  dplyr::select(1:2)


# Subset to NCCAG data which are marked as either 'hydroriparian tree' or 'hydroriparian shrub'
#   common examples - Populus, Platanus, Salix...
nccag_hydroriparian_riparian_types <- nccag_classes %>% 
  filter(guild %in% c("hr_t", "h_mr_s")) 
nccag_hydroriparian <- nccag[nccag$VEGETATION %in% unique(nccag_hydroriparian_riparian_types$veg_type),]
# Filter to get ONLY the values at least twice the area of a Landsat pixel
nccag_hydroriparian_areas <- st_area(nccag_hydroriparian)
nccag_hydroriparian_large <- nccag_hydroriparian[as.numeric(nccag_hydroriparian_areas) > (2*30^2),]
# add useful metadata
nccag_hydroriparian_large$guild = rep("hydroriparian", nrow(nccag_hydroriparian_large))
nccag_hydroriparian_large$riparian = rep(1, nrow(nccag_hydroriparian_large))
nccag_hydroriparian_large$year = lubridate::year(nccag_hydroriparian_large$DATE_DATA_)
# Output hydroriparian shapefile
st_write(nccag_hydroriparian_large, 
         here::here("..","NCCAG","hydroriparian_woody_plants.shp"), 
         driver = "ESRI Shapefile",
         delete_dsn = TRUE)


# Subset to NCCAG data which are marked as 'hydroriparian tree'
#   common examples - Populus, Platanus, NOT most Salix
nccag_hydroriparian_trees <- nccag_classes %>% 
  filter(guild %in% c("hr_t")) 
nccag_hydroriparian_tree <- nccag[nccag$VEGETATION %in% unique(nccag_hydroriparian_trees$veg_type),]
# Filter to get ONLY the values at least twice the area of a Landsat pixel
nccag_hydroriparian_tree_areas <- st_area(nccag_hydroriparian_tree)
nccag_hydroriparian_tree_large <- nccag_hydroriparian_tree[as.numeric(nccag_hydroriparian_tree_areas) > (2*30^2),]
# add useful metadata
nccag_hydroriparian_tree_large$guild = rep("hydroriparian", nrow(nccag_hydroriparian_tree_large))
nccag_hydroriparian_tree_large$riparian = rep(1, nrow(nccag_hydroriparian_tree_large))
nccag_hydroriparian_tree_large$year = lubridate::year(nccag_hydroriparian_tree_large$DATE_DATA_)
# Output hydroriparian shapefile
st_write(nccag_hydroriparian_tree_large, 
         here::here("..","NCCAG","hydroriparian_trees.shp"), 
         driver = "ESRI Shapefile",
         delete_dsn = TRUE)



# Subset to NCCAG data which are marked as woody riparian species (add 'xeroriparian tree')
#   compared to the above, this adds plants like Tamarix, Prosopis, Parkinsonia (lots of more arid region plants)
nccag_woody_riparian_types <- nccag_classes %>% 
  filter(guild %in% c("hr_t", "h_mr_s", "xr_t")) 
nccag_woody_riparian <- nccag[nccag$VEGETATION %in% unique(nccag_woody_riparian_types$veg_type),]
# Filter to get ONLY the values at least twice the area of a Landsat pixel
nccag_woody_riparian_areas <- st_area(nccag_woody_riparian)
nccag_woody_riparian_large <- nccag_woody_riparian[as.numeric(nccag_woody_riparian_areas) > (2*30^2),]
# add useful metadata
nccag_woody_riparian_large$guild = rep("woody_riparian", nrow(nccag_woody_riparian_large))
nccag_woody_riparian_large$riparian = rep(1, nrow(nccag_woody_riparian_large))
nccag_woody_riparian_large$year = lubridate::year(nccag_woody_riparian_large$DATE_DATA_)
# Output woody riparian shapefile
st_write(nccag_woody_riparian_large, 
         here::here("..","NCCAG","woody_riparian_plants.shp"), 
         driver = "ESRI Shapefile",
         delete_dsn = TRUE)