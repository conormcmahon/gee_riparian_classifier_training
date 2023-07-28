
library(sf)
library(here)
library(tidyverse)
library(janitor)

# Read in manually labelled guild types across veg classes 
nccag_classes <- read_csv(here::here("..","NCCAG","veg_guilds.csv")) %>%
  janitor::clean_names()  
nccag_classes <- nccag_classes %>%
  pivot_longer(2:ncol(nccag_classes), names_to="guild", values_to="dataval") %>% 
  drop_na(dataval) %>% 
  dplyr::select(1:2)

# Get list of all files which match .gdb or .gpkg types
files_gdb <- list.files(path=here::here("..","VegCamp"), pattern="*.gdb")
files_gpkg <- list.files(path=here::here("..","VegCamp"), pattern="*.gpkg")
# vegCamp class codes
#   these are really messy and vary between the datasets. All of the below are names used for the primary veg class in one database or another:
veg_type_variables <- c("MCVName", "MCVNAME", "MCV_name", "NVCSName", "NVCSNAME", "LABEL_1", "label_1", "NVCS_Name", "NVCS_name", "nvcs_name", "nvcsname", "mcvname", "cwhr")
# Load first dataset
# need to preserve dataset name, get veg name
# add year info, size, centroid --> climate and latitude information
# filter to drop units too small (< 2 landsat pixels?)
# filter to upland veg types (-GDE types)
new_dataset <- st_read(here::here("..","VegCamp",files_gdb[[1]])) %>% 
  janitor::clean_names()
veg_type_index <- which(names(new_dataset) %in% veg_type_variables)[[1]]
veg_camp <- new_dataset[,veg_type_index]
veg_camp$collection <- files_gdb[[1]]
names(veg_camp) <- c("veg_type", "shape", "collection")
st_geometry(veg_camp) <- "shape"
for(ind in 2:length(files_gdb))
{
  new_dataset <- st_read(here::here("..","VegCamp",files_gdb[[ind]])) %>% 
    janitor::clean_names()
  veg_type_index <- which(names(new_dataset) %in% veg_type_variables)[[1]]
  new_dataset <- new_dataset[,veg_type_index]
  new_dataset$collection <- files_gdb[[ind]]
  names(new_dataset) <- c("veg_type", "shape", "collection")
  st_geometry(new_dataset) <- "shape"
  new_dataset <- st_zm(new_dataset, drop=TRUE)
  
  veg_camp <- rbind(veg_camp, new_dataset)
}
for(ind in 1:length(files_gpkg))
{
  new_dataset <- st_read(here::here("..","VegCamp",files_gpkg[[ind]])) %>% 
    janitor::clean_names()
  veg_type_index <- which(names(new_dataset) %in% veg_type_variables)[[1]]
  new_dataset <- new_dataset[,veg_type_index]
  new_dataset$collection <- files_gpkg[[ind]]
  names(new_dataset) <- c("veg_type", "shape", "collection")
  st_geometry(new_dataset) <- "shape"
  new_dataset <- st_zm(new_dataset, drop=TRUE)
  
  veg_camp <- rbind(veg_camp, new_dataset)
}

# do some cleaning... remove all polygons with empty geometry:
veg_camp_valid <- veg_camp[!st_is_empty(veg_camp),]
# retain only features at least as large as 10 landsat pixels:
veg_camp_areas <- st_area(veg_camp_valid)
veg_camp_large <- veg_camp_valid[as.numeric(veg_camp_areas) > (10*30^2),]

# Write combined data to disk
st_write(veg_camp_large, 
         here::here("..","VegCamp"),
         layer="all_polygons_filtered",
         driver="ESRI Shapefile")

# Generate list of NVCSNames and other vegetation labels from VegCamp 
# These are then manually divided between guilds (e.g. phreatophytic riparian deciduous vs. not)
veg_camp_labels <- unique(veg_camp_large$veg_type)
write_csv(data.frame(vegetation = veg_camp_labels), here::here("veg_camp_labels.csv"))


# simple upland
upland_simple <- veg_camp_large %>%
  filter(!grepl("Populus", veg_type),
         !grepl("Platanus", veg_type),
         !grepl("Salix", veg_type),
         !grepl("Prosopis", veg_type),
         !grepl("Willow", veg_type),
         !grepl("Cottonwood", veg_type),
         !grepl("Sycamore", veg_type),
         !grepl("Mesquite", veg_type),
         !grepl("Alnus", veg_type),
         !grepl("Alder", veg_type),
         !grepl("Acer", veg_type),
         !grepl("Maple", veg_type),
         !grepl("Betula", veg_type),
         !grepl("Birch", veg_type),
         !grepl("Walnut", veg_type),
         !grepl("Juglans", veg_type),
         !grepl("Ash", veg_type),
         !grepl("Fraxinus", veg_type),
         !grepl("Riparian", veg_type),
         !grepl("Baccharis salicifolia", veg_type),
         !grepl("Acacia", veg_type),
         !grepl("Chilopsis", veg_type),
         !grepl("Palo", veg_type),
         !grepl("Parkinsonia", veg_type),
         !grepl("Apricot", veg_type),
         !grepl("Prunus", veg_type),
         !grepl("Tamarix", veg_type),
         !grepl("Tamarisk", veg_type),
         !grepl("Hackberry", veg_type),
         !grepl("Celtis", veg_type),
         !grepl("Quercus", veg_type),
         !grepl("Oak", veg_type),
         !grepl("Woodland", veg_type),
         !grepl("Forest", veg_type),
         !grepl("Riparian", veg_type),
         !grepl("Eucalyptus", veg_type)) 


# Set random seed
set.seed(1)

# Generate Training Data
ag_plots <- veg_camp_large %>% filter(veg_type=="Agriculture")
ag_points <- st_sample(ag_plots, 1000) 

hydroriparian <- st_read(here::here("..","NCCAG","hydroriparian_trees.shp"))
hydroriparian_areas <- st_area(hydroriparian)
hydroriparian_large <- hydroriparian %>% 
  filter(as.numeric(hydroriparian_areas) > (10*30^2))
riparian_points <- st_sample(hydroriparian_large, 1500)

oak_plots <- veg_camp_large %>% filter(grepl("Quercus", veg_camp_large$veg_type))
oak_points <- st_sample(ag_plots, 1000) 

upland_points <- st_sample(upland_simple, 1000)

training_points <- data.frame(code = c(rep(1, length(riparian_points)),
                                       rep(2, length(oak_points)),
                                       rep(3, length(upland_points)),
                                       rep(4, length(ag_points))),
                              riparian = c(rep(1, length(riparian_points)),
                                       rep(2, length(oak_points)+length(upland_points)+length(ag_points))))
st_geometry(training_points) <- st_geometry(c(riparian_points, oak_points, upland_points, ag_points))
st_write(training_points, 
         here::here("..","VegCampPointsSynth"),
         layer="training_points_no_xrt",
         driver="ESRI Shapefile",
         delete_dsn = TRUE)

# Generate Validation Data

ag_plots <- veg_camp_large %>% filter(veg_type=="Agriculture")
ag_points <- st_sample(ag_plots, 1000) 

hydroriparian <- st_read(here::here("..","NCCAG","hydroriparian_trees.shp"))
hydroriparian_areas <- st_area(hydroriparian)
hydroriparian_large <- hydroriparian %>% 
  filter(as.numeric(hydroriparian_areas) > (10*30^2))
riparian_points <- st_sample(hydroriparian_large, 1500)

oak_plots <- veg_camp_large %>% filter(grepl("Quercus", veg_camp_large$veg_type))
oak_points <- st_sample(ag_plots, 1000) 

upland_points <- st_sample(upland_simple, 1000)

validation_points <- data.frame(code = c(rep(1, length(riparian_points)),
                                       rep(2, length(oak_points)),
                                       rep(3, length(upland_points)),
                                       rep(4, length(ag_points))),
                              riparian = c(rep(1, length(riparian_points)),
                                           rep(2, length(oak_points)+length(upland_points)+length(ag_points))))
st_geometry(validation_points) <- st_geometry(c(riparian_points, oak_points, upland_points, ag_points))
st_write(validation_points, 
         here::here("..","VegCampPointsSynth"),
         layer="validation_points_no_xrt",
         driver="ESRI Shapefile",
         delete_dsn = TRUE)

