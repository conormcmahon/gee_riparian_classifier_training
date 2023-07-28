
# Source libraries

library(sf)
library(raster)
library(tidyverse)
library(fasterize)

source(here::here("aggregate_custom.r"))

# Load phenology raster
gee_datastack <- stack("D:/SERDP/GEE_Classifier/Dar_manual_delineations/sym_fr_datastack_2021.tif")
# Load LiDAR-derived HOD Layer
hod_10m_lidar <- raster("D:/SERDP/GEE_Classifier/Dar_manual_delineations/hod_10m_median.tif")
chm_min_10m_lidar <- raster("D:/SERDP/GEE_Classifier/Dar_manual_delineations/chm_10m_min.tif")
chm_max_10m_lidar <- raster("D:/SERDP/GEE_Classifier/Dar_manual_delineations/chm_10m_max.tif")
chm_avg_10m_lidar <- raster("D:/SERDP/GEE_Classifier/Dar_manual_delineations/chm_10m_average.tif")
lidar_layers <- stack(crop(hod_10m_lidar,chm_min_10m_lidar), chm_min_10m_lidar, chm_max_10m_lidar, chm_avg_10m_lidar)
names(lidar_layers) <- c("hod_lidar","chm_min","chm_max","chm_avg")
lidar_layers <- extend(lidar_layers, gee_datastack)
# Load guilds layer
guilds_prediction <- raster("D:/SERDP/GEE_Classifier/Dar_manual_delineations/sym_fr_guilds_2021.tif")
# Stack GEE data, LiDAR HOD, and GEE guild prediction
gee_datastack <- stack(gee_datastack, guilds_prediction, hod_10m_lidar, chm_min_10m_lidar, chm_max_10m_lidar, chm_avg_10m_lidar)

# Load manual delineations from Dar
sym_creek_shapefiles <- st_transform(st_read("D:/SERDP/GEE_Classifier/Dar_manual_delineations/all_creeks_merged.shp"),
                                st_crs(gee_datastack))
# Add numeric index for creek names
sym_creek_shapefiles$layer_num <- 1:nrow(sym_creek_shapefiles)
# Do the same with a buffered creek shapefile (expanded outwards by 50 m)
sym_creek_buffered <- st_transform(st_read("D:/SERDP/GEE_Classifier/Dar_manual_delineations/creeks_buffered.shp"),
                                     st_crs(gee_datastack))
# Add numeric index for creek names
sym_creek_buffered$layer_num <- 1:nrow(sym_creek_buffered)

# Rasterize the shapefiles to get creek name at each raster cell
sym_creek_raster <- fasterize(sym_creek_shapefiles, gee_datastack[[1]], field="layer_num", fun="sum")
sym_creek_raster_buffered <- fasterize(sym_creek_buffered, gee_datastack[[1]], field="layer_num", fun="sum")
gee_datastack$creek_ind <- sym_creek_raster
gee_datastack$creek_ind_buffered <- sym_creek_raster_buffered
creek_list <- sym_creek_shapefiles$layer
names(creek_list) <- sym_creek_shapefiles$layer_num

# Apply meaningful names to data stack
names(gee_datastack) <- c(paste("NDVI_",1:12,sep=""), "summer_ndvi","winter_ndvi","stdev_ndvi","max_ndvi",
                             "hod_1","hod_2","flowaccum","dem","slope","precip","tmax","tmin","vpd","latitude",
                             "pheno_change_angle","blue","green","red","NIR","SWIR_1","SWIR_2","guild_prediction","creek","creek_ind","creek_buffered")

  
# Mask phenoseries to creeks, get values into data frame
creeks_datastack <- as.data.frame(gee_datastack, xy=TRUE) %>% mutate(creek_name = creek_list[creek])

creeks_lidar_combined_raster <- stack(crop(gee_datastack, lidar_layers), lidar_layers)

ggplot(creeks_datastack %>% drop_na(creek)) + 
  geom_density_2d_filled(aes(x=dem,y=(summer_ndvi-winter_ndvi)), contour_var = "ndensity") + 
  geom_hline(yintercept=0, col="red") + 
  facet_wrap(~creek_name)

# Get areal overlap of my and Dar's maps, within a certain threshold HOD_1 value
hod_thresh <- 10
all_buffer_pixels <- creeks_datastack %>% 
  filter(!is.na(creek_buffered))
gee_riparian <- all_buffer_pixels %>% filter(guild_prediction %in% c(1,2))
dar_riparian <- all_buffer_pixels %>% filter(!is.na(creek))
gee_riparian_hodfilt <- gee_riparian %>% filter(hod_1 < hod_thresh)
# Fraction of GEE-predicted riparian pixels which are in Dar's map
nrow(gee_riparian_hodfilt %>% filter(!is.na(creek))) / nrow(gee_riparian_hodfilt)
# Fraction of Dar's pixels which are in GEE prediction
nrow(gee_riparian_hodfilt %>% filter(!is.na(creek))) / nrow(dar_riparian)

creeks_pheno_long <- creeks_datastack %>% 
  pivot_longer(3:54, names_to="month_string", values_to="NDVI") %>%
  mutate(month = as.numeric(substr(month_string,37,38)))
# Create a phenology plot for each creek
ggplot(creeks_pheno_long) + 
  geom_density2d_filled(aes(x=month,y=NDVI), contour_var = "ndensity") + 
  facet_wrap(~creek)


# Load LiDAR data
chm_raster <- raster("D:/SERDP/Mission_Creek/LiDAR/chm/mosaic.tif")
names(chm_raster) <- "canopy_height"
dar_rattlesnake_lidar_crs <- st_transform(dar_rattlesnake, st_crs(chm_raster))
dar_mission_lidar_crs <- st_transform(dar_mission, st_crs(chm_raster))
rattlesnake_chm <- as.data.frame(mask(chm_raster, dar_rattlesnake_lidar_crs), xy=TRUE) %>%
  mutate(creek = "Rattlesnake Creek")
mission_chm <- as.data.frame(mask(chm_raster, dar_mission_lidar_crs))%>%
  mutate(creek = "Mission Creek")
all_creeks_chm <- rbind(rattlesnake_chm,
                        mission_chm)
# Create a histogram to visualize CHM distribution by creek
ggplot(all_creeks_chm) + geom_histogram(aes(x=canopy_height)) + 
  facet_wrap(~creek)
  scale_x_continuous(limits=c(0,40))
  
  
# Both datasets together
#   first, aggregate LiDAR CHM up to phenoseries scale
chm_raster_reproj <- projectRaster(chm_raster, crs=crs(phenoseries), res=res(chm_raster), method="bilinear")










