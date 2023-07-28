
library(tidyverse)
library(sf)
library(janitor)

# Note - Repeat this for each .gdb file in the CalVeg dataset!

# Load Dataset
veg_data <- st_read(here::here("..","CalVeg","S_USA.EVMid_R05_SouCoast.gdb")) %>% 
  janitor::clean_names()
# Remove very Small Features (< 5 landsat pixel area)
veg_data_large <- veg_data %>% 
  filter((veg_data$shape_area*(1e10)) > (5*30^2))
total_area <- sum(veg_data_large$shape_area)
# To save memory, drop the rest of the data
rm(veg_data)

# get list of all divisions in input
division_list <- unique(veg_data_large$nvcs_division)
division_list <- division_list[!is.na(division_list)]
# riparian divisions
riparian_divisions <- c("D013","D193")
woodland_divisions <- unique(veg_data_large[veg_data_large$nvcs_class=="C01",]$nvcs_division)
woodland_divisions <- woodland_divisions[!is.na(woodland_divisions)]
non_riparian_woodland_divisions <- woodland_divisions[!(woodland_divisions %in% riparian_divisions)]
# Upland Veg
upland_divisions <- division_list[!(division_list %in% woodland_divisions)]
# get list of all cultural formations in input (non-natural veg types)
cult_form_list <- unique(veg_data_large$cultural_formation)
cult_form_list <- cult_form_list[!is.na(cult_form_list)]

# Sample by NVCS division
#   Clever routine to sample from individual polygons in random manner weighted by polygon size
#   Avoids prohibitively expensive task of choosing a random point from entire ecoregion (geometry has too many vertices, math is untenable)
sampleByNVCSDivision <- function(division, data, num_samples)
{
  # Get all polygons which include the target division
  division_data <- data %>% 
    filter(nvcs_division == division)
  division_data$ind <- 1:nrow(division_data)
  # function to get X copies of the index of a veg dataframe, weighted by size
  getWeightVector <- function(veg_polygon)
  {
    values <- rep(veg_polygon$ind, floor(veg_polygon$shape_area*(1e10)/(5*30^2)))
    return(values)
  }
  weighted_index_vector <- unlist(apply(division_data, MARGIN=1, FUN=getWeightVector))
  
  if(num_samples > length(weighted_index_vector))
    vector_indices <- weighted_index_vector
  else 
    vector_indices <- sample(weighted_index_vector, num_samples)
  
  getLabelledPoint <- function(polygon_index, polygons)
  {
    point_st <- st_sample(polygons[polygon_index,], 1)
    point_sf <- st_sf(point_st)
    st_join(point_sf, polygons[polygon_index,])
  }
  
  # List of SF feature collections, each one point
  output_points <- (lapply(vector_indices, getLabelledPoint, polygons=division_data))
  # Single SF feature collection for all points
  return(do.call(rbind, output_points))
}


# sample by Cultural Formation (non-natural vegetation)
sampleByCulturalFormation <- function(formation, data, num_samples)
{
  formation_data <- data %>% 
    filter(cultural_formation == formation)
  formation_data$ind <- 1:nrow(formation_data)
  # function to get X copies of the index of a veg dataframe, weighted by size
  getWeightVector <- function(veg_polygon)
  {
    values <- rep(veg_polygon$ind, floor(veg_polygon$shape_area*(1e10)/(5*30^2)))
    return(values)
  }
  weighted_index_vector <- unlist(apply(formation_data, MARGIN=1, FUN=getWeightVector))
  
  if(num_samples > length(weighted_index_vector))
    vector_indices <- weighted_index_vector
  else 
    vector_indices <- sample(weighted_index_vector, num_samples)
  
  getLabelledPoint <- function(polygon_index, polygons)
  {
    point_st <- st_sample(polygons[polygon_index,], 1)
    point_sf <- st_sf(point_st)
    st_join(point_sf, polygons[polygon_index,])
  }
  
  # List of SF feature collections, each one point
  output_points <- (lapply(vector_indices, getLabelledPoint, polygons=formation_data))
  # Single SF feature collection for all points
  return(do.call(rbind, output_points))
}

# ----------------------------------------------------------------------------------------
# ------------------------------- Set Up Training Data -----------------------------------
# ----------------------------------------------------------------------------------------

# Set random seed for repeatability 
set.seed(1)

total_training_points = 1000

#    get riparian flooded woodland sampling points --> 1/2 of training points
points_per_division <- floor(total_training_points/2/length(riparian_divisions))
riparian_training_points <- lapply(riparian_divisions, 
                                   sampleByNVCSDivision, 
                                   data=veg_data_large, 
                                   num_samples=points_per_division)
#   get upland sampling points --> 1/6 of training points
points_per_division <- floor(total_training_points/6/length(non_riparian_woodland_divisions))
woodland_training_points <- lapply(non_riparian_woodland_divisions, 
                                 sampleByNVCSDivision, 
                                 data=veg_data_large, 
                                 num_samples=points_per_division)
#   get upland sampling points --> 1/6 of training points
points_per_division <- floor(total_training_points/6/length(upland_divisions))
upland_training_points <- lapply(upland_divisions,
                                 sampleByNVCSDivision, 
                                 data=veg_data_large, 
                                 num_samples=points_per_division)
#   get crop sampling points --> 1/6 of training points
points_per_division <- floor(total_training_points/6/length(cult_form_list))
crop_training_points <- lapply(cult_form_list,
                                sampleByCulturalFormation,
                                data=veg_data_large,
                                num_samples=points_per_division)
#upland_training_points$guild <- 4
# Combine the above
training <- rbind(do.call(rbind, riparian_training_points), 
                  do.call(rbind, woodland_training_points), 
                  do.call(rbind, upland_training_points), 
                  do.call(rbind, crop_training_points))
# add binary flag for winter-deciduous riparian trees
training$riparian <- (training$nvcs_division %in% riparian_divisions)
# add binary flag for natural woodlands/forest
training$woodland <- (training$nvcs_division %in% woodland_divisions)
# add guild code
#   1 -> riparian winter deciduous tree
#   2 -> non-winter-deciduous tree (may be riparian, facultatively)
#   3 -> upland natural vegetation
#   4 -> crop
training$guild <- rep(0, nrow(training))
training[training$nvcs_division %in% riparian_divisions,]$guild <- 1
training[training$nvcs_division %in% non_riparian_woodland_divisions,]$guild <- 2
training[training$nvcs_division %in% upland_divisions,]$guild <- 3
training[!is.na(training$cultural_formation),]$guild <- 4
# write output to disk
training_sub <- training %>%
  dplyr::select("nvcs_class",
                "nvcs_subclass",
                "nvcs_formation",
                "nvcs_division",
                "nvcs_macrogroup",
                "nvcs_group",
                "nvcs_alliance",
                "nvcs_association",
                "cultural_class",
                "cultural_subclass",
                "cultural_formation",
                "cultural_subformation",
                "cultural_group",
                "cultural_subgroup",
                "cultural_type",
                "cultural_subtype",
                "source_date_year",
                "source_date_month",
                "source_date_day",
                "calvegzone",
                "riparian",
                "woodland",
                "guild",
                "geometry")
names(training_sub) <- base::abbreviate(names(training_sub))
st_geometry(training_sub) <- "gmtr"
training_sub$date <- as.Date(paste(training_sub$src_dt_y,training_sub$src_dt_m,training_sub$src_dt_d,sep="-"))
st_write(training_sub, 
         here::here("..","CalVeg","calveg_training.shp"),
         layer="calveg_training",
         driver="ESRI Shapefile",
         delete_dsn = TRUE)


# ----------------------------------------------------------------------------------------
# ------------------------------ Set Up Validation Data ----------------------------------
# ----------------------------------------------------------------------------------------

total_validation_points <- 250

# Generate Validation Data
#    get riparian flooded woodland sampling points 
points_per_division <- floor(total_validation_points/2/length(riparian_divisions))
riparian_validation_points <- lapply(riparian_divisions, 
                                     sampleByNVCSDivision, 
                                     data=veg_data_large, 
                                     num_samples=points_per_division)
#    get upland sampling points 
points_per_division <- floor(total_validation_points/6/length(non_riparian_woodland_divisions))
woodland_validation_points <- lapply(non_riparian_woodland_divisions, 
                                 sampleByNVCSDivision, 
                                 data=veg_data_large, 
                                 num_samples=points_per_division)
#    get upland sampling points 
points_per_division <- floor(total_validation_points/6/length(upland_divisions))
upland_validation_points <- lapply(upland_divisions, 
                                   sampleByNVCSDivision, 
                                   data=veg_data_large, 
                                   num_samples=points_per_division)
#   get crop sampling points 
points_per_division <- floor(total_validation_points/6/length(cult_form_list))
crop_validation_points <- lapply(cult_form_list,
                               sampleByCulturalFormation,
                               data=veg_data_large,
                               num_samples=points_per_division)
# Combine the above
validation <- rbind(do.call(rbind, riparian_validation_points), 
                    do.call(rbind, woodland_validation_points), 
                    do.call(rbind, upland_validation_points), 
                    do.call(rbind, crop_validation_points))
# add binary flag for winter-deciduous riparian trees
validation$riparian <- (validation$nvcs_division %in% riparian_divisions)
# add binary flag for natural woodlands/forest
validation$woodland <- (validation$nvcs_division %in% woodland_divisions)
# add guild code
#   1 -> riparian winter deciduous tree
#   2 -> non-winter-deciduous tree (may be riparian, facultatively)
#   3 -> upland natural vegetation
#   4 -> crop
validation$guild <- rep(0, nrow(validation))
validation[validation$nvcs_division %in% riparian_divisions,]$guild <- 1
validation[validation$nvcs_division %in% non_riparian_woodland_divisions,]$guild <- 2
validation[validation$nvcs_division %in% upland_divisions,]$guild <- 3
validation[!is.na(validation$cultural_formation),]$guild <- 4
# write output to disk
validation_sub <- validation %>%
  dplyr::select("nvcs_class",
                "nvcs_subclass",
                "nvcs_formation",
                "nvcs_division",
                "nvcs_macrogroup",
                "nvcs_group",
                "nvcs_alliance",
                "nvcs_association",
                "cultural_class",
                "cultural_subclass",
                "cultural_formation",
                "cultural_subformation",
                "cultural_group",
                "cultural_subgroup",
                "cultural_type",
                "cultural_subtype",
                "source_date_year",
                "source_date_month",
                "source_date_day",
                "calvegzone",
                "riparian",
                "woodland",
                "guild",
                "geometry")
names(validation_sub) <- base::abbreviate(names(validation_sub))
st_geometry(validation_sub) <- "gmtr"
validation_sub$date <- as.Date(paste(validation_sub$src_dt_y,validation_sub$src_dt_m,validation_sub$src_dt_d,sep="-"))
st_write(validation_sub, 
         here::here("..","CalVeg","calveg_validation.shp"),
         layer="calveg_validation",
         driver="ESRI Shapefile",
         delete_dsn = TRUE)

