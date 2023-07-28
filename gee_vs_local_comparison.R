
# Source libraries

library(sf)
library(raster)
library(tidyverse)
library(fasterize)
library(caret)

source("D:/SERDP/GEE_Classifier/gee_riparian_classifier_training/aggregate_custom.R")

local_san_antonio_guilds <- raster("D:/SERDP/Vandenberg/Landsat/classified/classes_2019.tif")
gee_san_antonio_guilds <- raster("D:/SERDP/GEE_Classifier/Phenology/example_outputs_landsat_sentinel/san_antonio_guilds_landsat_2020.tif")
gee_san_antonio_datastack <- stack("D:/SERDP/GEE_Classifier/Phenology/example_outputs_landsat_sentinel/san_antonio_datastack_landsat_2020.tif")
low_relief_san_antonio <- (gee_san_antonio_datastack[[18]] < 5)
local_san_antonio_guilds <- aggregate_custom(local_san_antonio_guilds, gee_san_antonio_guilds, reproj_method = "ngb")

san_antonio_creek_data <- as.data.frame(stack(local_san_antonio_guilds, gee_san_antonio_guilds, gee_san_antonio_datastack), xy=TRUE) 
names(san_antonio_creek_data) <- c("x", "y", "local_guild", "gee_guild", paste("ndvi_",1:12,sep=""), "summer_ndvi", "winter_ndvi", "stdev_ndvi", "max_ndvi", 
                                   "hod_1", "hod_2", "flow_accum", "dem", "slope", "precip", "tmax", "tmin", "vpd", "latitude",
                                   "blue", "green", "red", "NIR", "SWIR_1", "SWIR_2")
santa_ynez_creek_data$low_relief <- santa_ynez_creek_data$hod_2 < 10

plot((gee_san_antonio_guilds==1) + (gee_san_antonio_guilds==2)*low_relief_san_antonio)
plot(local_san_antonio_guilds==1)

# Fraction of local_san_antonio decid riparian labelled as deciduous --> 74%
nrow(san_antonio_creek_data %>% filter(local_guild == 1, gee_guild == 1)) / nrow(san_antonio_creek_data %>% filter(local_guild == 1))
# Fraction of gee deciduous riparian which was labelled riparian in local model --> 64.1%
nrow(san_antonio_creek_data %>% filter(local_guild == 1, gee_guild == 1)) / nrow(san_antonio_creek_data %>% filter(gee_guild == 1))
# Fraction of things labelled as deciduous, or evergreen woodland within HOD_2 < 10, which was deciduous riparian in the local model --> 62.0%
nrow(san_antonio_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1)) & as.logical(local_guild == 1))) / nrow(san_antonio_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1))))
# Similar to above, but inverted: fraction of local riparian which was included in GEE woodland --> 94.9%
nrow(san_antonio_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1)) & as.logical(local_guild == 1))) / nrow(san_antonio_creek_data %>% filter(as.logical(local_guild == 1)))





local_santa_ynez_guilds <- raster("D:/SERDP/Vandenberg/Landsat/classified/classes_2019.tif")
gee_santa_ynez_guilds <- raster("D:/SERDP/GEE_Classifier/Phenology/example_outputs_landsat_sentinel/santa_ynez_guilds_landsat_2020.tif")
gee_santa_ynez_datastack <- stack("D:/SERDP/GEE_Classifier/Phenology/example_outputs_landsat_sentinel/santa_ynez_datastack_landsat_2020.tif")
low_relief_santa_ynez <- (gee_santa_ynez_datastack[[18]] < 5)
local_santa_ynez_guilds <- aggregate_custom(local_santa_ynez_guilds, gee_santa_ynez_guilds, reproj_method = "ngb")

santa_ynez_creek_data <- as.data.frame(stack(local_santa_ynez_guilds, gee_santa_ynez_guilds, gee_santa_ynez_datastack), xy=TRUE)
names(santa_ynez_creek_data) <- c("x", "y", "local_guild", "gee_guild", paste("ndvi_",1:12,sep=""), "summer_ndvi", "winter_ndvi", "stdev_ndvi", "max_ndvi", 
                                   "hod_1", "hod_2", "flow_accum", "dem", "slope", "precip", "tmax", "tmin", "vpd", "latitude",
                                   "blue", "green", "red", "NIR", "SWIR_1", "SWIR_2")
santa_ynez_creek_data$low_relief <- santa_ynez_creek_data$hod_2 < 10

plot((gee_santa_ynez_guilds==1) + (gee_santa_ynez_guilds==2)*low_relief_santa_ynez)
plot(local_santa_ynez_guilds==1)

# Fraction of local_san_antonio decid riparian labelled as deciduous --> 72.1%
nrow(santa_ynez_creek_data %>% filter(local_guild == 1, gee_guild == 1)) / nrow(santa_ynez_creek_data %>% filter(local_guild == 1))
# Fraction of gee deciduous riparian which was labelled riparian in local model --> 64.4%
nrow(santa_ynez_creek_data %>% filter(local_guild == 1, gee_guild == 1)) / nrow(santa_ynez_creek_data %>% filter(gee_guild == 1))
# Fraction of things labelled as deciduous, or evergreen woodland within HOD_2 < 10, which was deciduous riparian in the local model --> 57.6%
nrow(santa_ynez_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1)) & as.logical(local_guild == 1))) / nrow(santa_ynez_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1))))
# Similar to above, but inverted: fraction of local riparian which was included in GEE woodland --> 96.2%
nrow(santa_ynez_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1)) & as.logical(local_guild == 1))) / nrow(santa_ynez_creek_data %>% filter(as.logical(local_guild == 1)))






local_pendleton_guilds <- raster("D:/SERDP/Pendleton/R/veg_hydro_stats/classes.tif")
crs(local_pendleton_guilds) <- crs("EPSG:32611")
gee_pendleton_guilds <- raster("D:/SERDP/GEE_Classifier/Phenology/example_outputs_landsat_sentinel/pendleton_guilds_landsat_2019.tif")
gee_pendleton_datastack <- stack("D:/SERDP/GEE_Classifier/Phenology/example_outputs_landsat_sentinel/pendleton_datastack_landsat_2019.tif")
low_relief_pendleton <- (gee_pendleton_datastack[[18]] < 5)
local_pendleton_guilds <- aggregate_custom(local_pendleton_guilds, gee_pendleton_guilds, reproj_method = "ngb")
gee_pendleton_guilds <- crop(gee_pendleton_guilds, local_pendleton_guilds)
gee_pendleton_datastack <- crop(gee_pendleton_datastack, local_pendleton_guilds)

pendleton_creek_data <- as.data.frame(stack(local_pendleton_guilds, gee_pendleton_guilds, gee_pendleton_datastack), xy=TRUE)
names(pendleton_creek_data) <- c("x", "y", "local_guild", "gee_guild", paste("ndvi_",1:12,sep=""), "summer_ndvi", "winter_ndvi", "stdev_ndvi", "max_ndvi", 
                                  "hod_1", "hod_2", "flow_accum", "dem", "slope", "precip", "tmax", "tmin", "vpd", "latitude",
                                  "blue", "green", "red", "NIR", "SWIR_1", "SWIR_2")
pendleton_creek_data$low_relief <- pendleton_creek_data$hod_2 < 10

plot((gee_pendleton_guilds==1) + (gee_pendleton_guilds==2)*low_relief_pendleton)
plot(local_pendleton_guilds==1)

# Fraction of local_san_antonio decid riparian labelled as deciduous --> 26.3%
nrow(pendleton_creek_data %>% filter(local_guild == 1, gee_guild == 1) %>% drop_na(local_guild, gee_guild)) / nrow(pendleton_creek_data %>% filter(local_guild == 1) %>% drop_na(local_guild, gee_guild))
# Fraction of gee deciduous riparian which was labelled riparian in local model --> 54.1%
nrow(pendleton_creek_data %>% filter(local_guild == 1, gee_guild == 1) %>% drop_na(local_guild, gee_guild)) / nrow(pendleton_creek_data %>% filter(gee_guild == 1) %>% drop_na(local_guild, gee_guild))
# Fraction of things labelled as deciduous, or evergreen woodland within HOD_2 < 10, which was deciduous riparian in the local model --> 48.7%
nrow(pendleton_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1)) & as.logical(local_guild == 1)) %>% drop_na(local_guild, gee_guild)) / nrow(pendleton_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1))) %>% drop_na(local_guild, gee_guild))
# Similar to above, but inverted: fraction of local riparian which was included in GEE woodland --> 53.0%
nrow(pendleton_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1)) & as.logical(local_guild == 1)) %>% drop_na(local_guild, gee_guild)) / nrow(pendleton_creek_data %>% filter(as.logical(local_guild == 1)) %>% drop_na(local_guild, gee_guild))

pendleton_segments <- st_read("D:/SERDP/Pendleton/R/veg_hydro_stats/rf_training_polygons/rf_training.shp")
st_transform(pendleton_segments, st_crs(gee_pendleton_guilds))

pendleton_segment_data <- raster::extract(stack(local_pendleton_guilds, gee_pendleton_guilds, gee_pendleton_datastack), pendleton_segments)

pendleton_classes <- c("Riparian","Chaparral","Annual Grassland","Turfgrass","Suburb","Impervious","Water","Other")
names(pendleton_classes) <- 1:7
extractedPendletonToDF <- function(index)
{
  output_samples <- as.data.frame(pendleton_segment_data[[index]])
  if(nrow(output_samples) == 0)
  {
    bad_data <- rep(-9999, length(names(pendleton_creek_data)[3:(ncol(pendleton_creek_data)-1)]))
    names(bad_data) <-names(pendleton_creek_data)[3:(ncol(pendleton_creek_data)-1)]
    return(bad_data)
  }  
  names(output_samples) <-names(pendleton_creek_data)[3:(ncol(pendleton_creek_data)-1)]
  output_samples$class_num <- 1*(pendleton_segments[index,]$level_2=="riparian")*(pendleton_segments[index,]$level_3 %in% c("trees","shrubs")) + 
    2*(pendleton_segments[index,]$level_3 %in% c("chaparral", "north_slope")) + 
    3*(pendleton_segments[index,]$level_3 == "grassland") + 3*(pendleton_segments[index,]$level_2=="riparian")*(pendleton_segments[index,]$level_3 == "annual")
    4*(pendleton_segments[index,]$level_3 == "turf") + 
    5*(pendleton_segments[index,]$level_3 == "suburb") + 
    6*(pendleton_segments[index,]$level_2 == "paving") + 
    7*(pendleton_segments[index,]$level_1 == "water") 
  output_samples$class_num <- output_samples$class_num + (output_samples$class_num == 0) * 8
  output_samples$level_1 <- pendleton_segments[index,]$level_1
  output_samples$level_2 <- pendleton_segments[index,]$level_2
  output_samples$level_3 <- pendleton_segments[index,]$level_3
  output_samples$class <- as.character(pendleton_classes[output_samples$class_num])
  output_samples$riparian_woodland <- (output_samples$gee_guild == 1) + (output_samples$gee_guild == 2)*(output_samples$hod_2 < 10)
  output_samples$sample_num <- index
  return(output_samples)
}

pendleton_sample_df <- bind_rows(lapply(1:length(pendleton_segment_data), extractedPendletonToDF))

# NOTE this shows first input (GEE label) as the prediction values, and second (manual labels) as the reference
confusionMatrix(as.factor((pendleton_sample_df %>% filter(class != "Other") %>% drop_na(class, gee_guild, local_guild))$riparian_woodland==1),as.factor((pendleton_sample_df %>% filter(class != "Other") %>% drop_na(class, gee_guild, local_guild))$class=="Riparian"))
# NOTE - the above gives:
# Confusion Matrix and Statistics
# 
# Reference
# Prediction FALSE  TRUE
# FALSE 17858   219
# TRUE    397  2267
# 
# Accuracy : 0.9703          
# 95% CI : (0.9679, 0.9726)
# No Information Rate : 0.8801          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.8635          
# 
# Mcnemar's Test P-Value : 9.926e-13       
#                                           
#             Sensitivity : 0.9783          
#             Specificity : 0.9119          
#          Pos Pred Value : 0.9879          
#          Neg Pred Value : 0.8510          
#              Prevalence : 0.8801          
#          Detection Rate : 0.8610          
#    Detection Prevalence : 0.8716          
#       Balanced Accuracy : 0.9451          
#                                           
#        'Positive' Class : FALSE 
#   85.1% user's accuracy for riparian woodland class 
#   91.2% producer's accuracy for riparian woodland class
#   97.0% overall accuracy, kappa = 0.864 (unbalanced, because riparian class is rare)
#   98.8% user's accuracy for upland
#   97.8% producer's accuracy for upland 
# Which other classes were greatest source of error? 
#  Collimation error (non woodland labelled woodland):
pendleton_sample_df %>% 
  filter(class != "Other", class != "Riparian", riparian_woodland == 1) %>% 
  group_by(class, gee_guild) %>% 
  summarize(count = n(), mean_hod_1 = mean(hod_1), min_hod = min(hod_1), max_hod=max(hod_1), mean_hod_2 = mean(hod_2), mean_summer_greenup = mean(ndvi_6+ndvi_7+ndvi_8-ndvi_1-ndvi_2-ndvi_3)) %>%
  ungroup() %>% 
  mutate(fractional_count = count / sum(count))
#  some notes... 83.6% of all collimation was chaparral that was labelled by GEE as evergreen woodland (71.0%), plus a smaller fraction labelled as deciduous riparian (13.6%). 
#  the remainder was labelled annual grassland in the reference, supposedly (although 23 observations of "annual grassland" had mean summer greenup of 0.202, which may indicate errors in my manually-mapped reference layer)

#  Omission error (riparian areas not labelled riparian woodland):
pendleton_sample_df %>% 
  filter(class != "Other", class == "Riparian", riparian_woodland != 1) %>% 
  group_by(gee_guild) %>% 
  summarize(count = n(), mean_hod_1 = mean(hod_1), min_hod = min(hod_1), max_hod=max(hod_1), mean_hod_2 = mean(hod_2), mean_summer_greenup = mean(ndvi_6+ndvi_7+ndvi_8-ndvi_1-ndvi_2-ndvi_3)) %>%
  mutate(fractional_count = count / sum(count))
# class            gee_guild count mean_hod_1 min_hod max_hod mean_hod_2 mean_summer_greenup fractional_count
# <chr>                <dbl> <int>      <dbl>   <dbl>   <dbl>      <dbl>               <dbl>            <dbl>
#   1 Annual Grassland         1    23       3.26       0       9       3.78             0.202              0.377
# 2 Annual Grassland         2    38       2.74       0       8       3.16            -0.245              0.623
# 3 Chaparral                1    54      13.8        0      59      43.1              0.00787            0.161
# 4 Chaparral                2   282       5.32       0      22       4.81            -0.0867             0.839
#  some notes... 61.6% of omissions were labelled woodland, but were just too high (mean HOD = 12.2). Many of these probably have deciduous veg mixed in too (mean_summer_greenup = 0.15) and should be labelled deciduous
#  32.4% of omissions were labelled upland veg, but I suspect these are all mis-labelled riparian? The mean summer greenup is 0.262, and mean hod_1 is only 2.59. Not sure why these weren't labelled riparian to start with...



# Confusion Matrix for deciduous riparian vs. reference riparian 
confusionMatrix(as.factor((pendleton_sample_df %>% filter(class != "Other") %>%  drop_na(class, gee_guild, local_guild))$gee_guild==1),as.factor((pendleton_sample_df %>% filter(class != "Other") %>% drop_na(class, gee_guild, local_guild))$class=="Riparian"))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction FALSE  TRUE
# FALSE 18178  1253
# TRUE     77  1233
# 
# Accuracy : 0.9359          
# 95% CI : (0.9325, 0.9392)
# No Information Rate : 0.8801          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.618           
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.9958          
#             Specificity : 0.4960          
#          Pos Pred Value : 0.9355          
#          Neg Pred Value : 0.9412          
#              Prevalence : 0.8801          
#          Detection Rate : 0.8764          
#    Detection Prevalence : 0.9368          
#       Balanced Accuracy : 0.7459          
#                                           
#        'Positive' Class : FALSE  
# NOTE - the above gives:
#   94.1% user's accuracy for riparian deciduous - BUT note that a lot of this is 'mixed woodland'
#   49.6% producer's accuracy for riparian deciduous
#   93.6% overall accuracy, kappa = 0.618 (unbalanced, because riparian class is rare)
#   93.6% user's accuracy for upland
#   99.6% producer's accuracy for upland 

# Make some pretty plots of phenology from GEE? 
#   first, pivot data to longer format
pendleton_df_pheno <- pendleton_sample_df %>% 
  pivot_longer(4:15, names_to="month_string", values_to="NDVI") %>%
  mutate(month = as.numeric(substr(month_string, 6,10))) 


ggplot() + 
  geom_density_2d_filled(data=pendleton_df_pheno %>% drop_na(class,gee_guild) %>% filter(class != "Other"), 
                         aes(x=month,y=NDVI), contour_var = "ndensity") + 
  geom_line(data=pendleton_df_pheno %>% drop_na(class,gee_guild) %>% filter(class != "Other") %>% 
              group_by(class,gee_guild,month) %>% 
              summarize(mean_NDVI = median(NDVI)), 
            aes(x=month,y=mean_NDVI), 
            col="red") + 
  scale_y_continuous(limits=c(0,1)) + 
  facet_wrap(~as.character(class)+gee_guild, nrow=3) +
  xlab("Month") + 
  ylab("Greenness (NDVI)")


ggplot() + 
  geom_density_2d_filled(data=pendleton_df_pheno %>% drop_na(class) %>% filter(class != "Other"), 
                         aes(x=month,y=NDVI), contour_var = "ndensity") + 
  geom_line(data=pendleton_df_pheno %>% drop_na(class) %>% filter(class != "Other") %>% 
              group_by(class,month) %>% 
              summarize(mean_NDVI = median(NDVI)), 
            aes(x=month,y=mean_NDVI), 
            col="red") + 
  scale_y_continuous(limits=c(0,1)) + 
  facet_wrap(~as.character(class), ncol=1) +
  xlab("Month") + 
  ylab("Greenness (NDVI)")



pendleton_sample_df %>% drop_na(class, gee_guild) %>% group_by(class, gee_guild) %>% summarize(count = n(), hod_1 = mean(hod_1), frac_low = mean(hod_2 < 10), summer_ndvi = mean(summer_ndvi))
# of 2486 pixels labelled riparian manually,
#   49.5% were labelled deciduous by GEE
#   47.0 were labelled evergreen, of which 88.5% (41.6% of all riparian pixels in GEE) were HOD < 10
#   5.4% were labelled evergreen upland (due to the HOD threshold)
# of 8592 pixels manually labelled as chaparral, 
#   76.6% were labelled as evergreen woodland, of which 4.3% (3.3% of all chaparral pixels) were HOD < 10 and labelled riparian 
#   16.4% were labelled upland vegetation. Among these, the mean summer NDVI was only 0.442, compared to 0.637 in the woodland output
#   6.4% were mis-labelled as cultivated vegetation 
#   0.6% were mis-labelled as deciduous riparian 
# of the 9663 pixels labelled as annual grassland manually,
#   74.5% were labelled 'cultivated' 
#   24.9% were labelled 'upland'
#   0.2% were mislabelled deciduous, and 0.42% as evergreen 






local_san_pedro_guilds <- raster("D:/SERDP/huachuca/riparian_zone/random_forest/classified_sub.tif")
gee_san_pedro_guilds <- raster("D:/SERDP/GEE_Classifier/Phenology/example_outputs_landsat_sentinel/san_pedro_guilds_sentinel_2_2019.tif")
gee_san_pedro_datastack <- stack("D:/SERDP/GEE_Classifier/Phenology/example_outputs_landsat_sentinel/san_pedro_datastack_sentinel_2_2019.tif")
low_relief_san_pedro <- (gee_san_pedro_datastack[[18]] < 5)
local_san_pedro_guilds <- aggregate_custom(local_san_pedro_guilds, gee_san_pedro_guilds, reproj_method = "ngb")
gee_san_pedro_guilds <- crop(gee_san_pedro_guilds, local_san_pedro_guilds)
gee_san_pedro_datastack <- crop(gee_san_pedro_datastack, local_san_pedro_guilds)

san_pedro_creek_data <- as.data.frame(stack(local_san_pedro_guilds, gee_san_pedro_guilds, gee_san_pedro_datastack), xy=TRUE)
names(san_pedro_creek_data) <- c("x", "y", "local_guild", "gee_guild", paste("ndvi_",1:12,sep=""), "summer_ndvi", "winter_ndvi", "stdev_ndvi", "max_ndvi", 
                                 "hod_1", "hod_2", "flow_accum", "dem", "slope", "precip", "tmax", "tmin", "vpd", "latitude",
                                 "blue", "green", "red", "NIR", "SWIR_1", "SWIR_2")
san_pedro_creek_data$low_relief <- san_pedro_creek_data$hod_2 < 10

plot((gee_san_pedro_guilds==1) + (gee_san_pedro_guilds==2)*low_relief_san_pedro)
plot(local_san_pedro_guilds==1)

# Fraction of local_san_antonio decid riparian labelled as deciduous --> 62.8%
nrow(san_pedro_creek_data %>% filter(local_guild %in% c(1,2), gee_guild == 1) %>% drop_na(local_guild, gee_guild)) / nrow(san_pedro_creek_data %>% filter(local_guild %in% c(1,2)) %>% drop_na(local_guild, gee_guild))
# Fraction of gee deciduous riparian which was labelled riparian in local model --> 89.5%
nrow(san_pedro_creek_data %>% filter(local_guild %in% c(1,2), gee_guild == 1) %>% drop_na(local_guild, gee_guild)) / nrow(san_pedro_creek_data %>% filter(gee_guild == 1) %>% drop_na(local_guild, gee_guild))
# Fraction of things labelled as deciduous, or evergreen woodland within HOD_2 < 10, which was deciduous riparian in the local model --> 89.5%
nrow(san_pedro_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1)) & as.logical(local_guild %in% c(1,2))) %>% drop_na(local_guild, gee_guild)) / nrow(san_pedro_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1))) %>% drop_na(local_guild, gee_guild))
# Similar to above, but inverted: fraction of local riparian which was included in GEE woodland --> 62.8%
nrow(san_pedro_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1)) & as.logical(local_guild %in% c(1,2))) %>% drop_na(local_guild, gee_guild)) / nrow(san_pedro_creek_data %>% filter(as.logical(local_guild %in% c(1,2))) %>% drop_na(local_guild, gee_guild))

san_pedro_segments <- st_read("D:/SERDP/Huachuca/vegetation_maps/aerial_veg_interpretation.shp")
san_pedro_segments <- st_transform(san_pedro_segments, st_crs(gee_san_pedro_guilds))

san_pedro_segment_data <- raster::extract(stack(local_san_pedro_guilds, gee_san_pedro_guilds, gee_san_pedro_datastack), san_pedro_segments, cellnumbers=TRUE, sp=TRUE)

san_pedro_classes <- c("Cottonwood","Mesquite","Grassland")
names(san_pedro_classes) <- 1:3
extractedSanPedroToDF <- function(index)
{
  output_samples <- as.data.frame(san_pedro_segment_data[[index]])
  if(nrow(output_samples) == 0)
  {
    bad_data <- rep(-9999, length(c("cell", names(san_pedro_creek_data)[3:(ncol(san_pedro_creek_data)-1)])))
    names(bad_data) <- c("cell", names(san_pedro_creek_data)[3:(ncol(san_pedro_creek_data)-1)])
    return(bad_data)
  }  
  names(output_samples) <- c("cell", names(san_pedro_creek_data)[3:(ncol(san_pedro_creek_data)-1)])
  output_samples$class_num <- 1*(san_pedro_segments[index,]$class=="PopFre") + 
    2*(san_pedro_segments[index,]$class=="ProVel") + 
    3*(san_pedro_segments[index,]$class=="grass") 
  output_samples$class_num <- output_samples$class_num + (output_samples$class_num == 0) * 4
  output_samples$class_code <- san_pedro_segments[index,]$class
  output_samples$structure <- san_pedro_segments[index,]$structure
  output_samples$riparian <- san_pedro_segments[index,]$riparian
  output_samples$phenology <- san_pedro_segments[index,]$phenology
  output_samples$class <- as.character(san_pedro_classes[output_samples$class_num])
  output_samples$riparian_woodland <- (output_samples$gee_guild == 1) + (output_samples$gee_guild == 2)*(output_samples$hod_2 < 10)
  output_samples$sample_num <- index
  return(output_samples)
}

san_pedro_sample_df <- bind_rows(lapply(1:length(san_pedro_segment_data), extractedSanPedroToDF))



# NOTE this shows first input (GEE label) as the prediction values, and second (manual labels) as the reference
confusionMatrix(as.factor((san_pedro_sample_df %>% drop_na(class, gee_guild, local_guild))$riparian_woodland==1),as.factor((san_pedro_sample_df %>% drop_na(class, gee_guild, local_guild))$riparian==1))
# NOTE - the above gives, with 3594 total samples:
#     Reference
#     Prediction FALSE TRUE
#     FALSE  1594  115
#     TRUE      0 1885
#   100% user's accuracy for riparian woodland class 
#   94.3% producer's accuracy for riparian woodland class
#   96.8% overall accuracy, kappa = 0.968 (unbalanced, because riparian class is rare)
#   93.2% user's accuracy for upland
#   100% producer's accuracy for upland 
# Which other classes were greatest source of error? 
#  Collimation error (non woodland labelled woodland):
san_pedro_sample_df %>% 
  filter(riparian != 1, riparian_woodland == 1) %>% 
  group_by(class, gee_guild) %>% 
  summarize(count = n(), mean_hod_1 = mean(hod_1), min_hod = min(hod_1), max_hod=max(hod_1), mean_hod_2 = mean(hod_2), mean_summer_greenup = mean(ndvi_6+ndvi_7+ndvi_8-ndvi_1-ndvi_2-ndvi_3)) %>%
  ungroup() %>% 
  mutate(fractional_count = count / sum(count))
# there was NO collimation error - no upland pixels classified as riparian. 

#  Omission error (riparian areas not labelled riparian woodland):
san_pedro_sample_df %>% 
  filter(riparian == 1, riparian_woodland != 1) %>% 
  group_by(gee_guild, class) %>% 
  summarize(count = n(), mean_hod_1 = mean(hod_1), min_hod = min(hod_1), max_hod=max(hod_1), mean_hod_2 = mean(hod_2), mean_summer_greenup = mean(ndvi_6+ndvi_7+ndvi_8-ndvi_1-ndvi_2-ndvi_3)) %>%
  mutate(fractional_count = count / sum(count))
# gee_guild class      count mean_hod_1 min_hod max_hod mean_hod_2 mean_summer_greenup fractional_count
# <dbl> <chr>      <int>      <dbl>   <dbl>   <dbl>      <dbl>               <dbl>            <dbl>
#   1         3 Cottonwood    55       2.55       0      11       2.45               0.640            0.561
#   2         3 Mesquite      43       2.12       0       5       2.84               0.512            0.439
#   3         4 Cottonwood    17       3.65       0      11       3.65               0.693            1   
# 7.1% of cottonwood pixels were mislabeled as an upland class
# 4.3% of mesquite pixels were mislabelled as an upland class


# Make some pretty plots of phenology from GEE? 
#   first, pivot data to longer format
san_pedro_df_pheno <- san_pedro_sample_df %>% 
  pivot_longer(4:15, names_to="month_string", values_to="NDVI") %>%
  mutate(month = as.numeric(substr(month_string, 6,10))) 


ggplot() + 
  geom_density_2d_filled(data=san_pedro_df_pheno %>% drop_na(class,gee_guild) %>% filter(class != "Other"), 
                         aes(x=month,y=NDVI), contour_var = "ndensity") + 
  geom_line(data=san_pedro_df_pheno %>% drop_na(class,gee_guild) %>% filter(class != "Other") %>% 
              group_by(class,month) %>% 
              summarize(mean_NDVI = median(NDVI)), 
            aes(x=month,y=mean_NDVI), 
            col="red") + 
  scale_y_continuous(limits=c(0,1)) + 
  facet_wrap(~(class), nrow=3) +
  xlab("Month") + 
  ylab("Greenness (NDVI)")


ggplot() + 
  geom_density_2d_filled(data=pendleton_df_pheno %>% drop_na(class) %>% filter(class != "Other"), 
                         aes(x=month,y=NDVI), contour_var = "ndensity") + 
  geom_line(data=pendleton_df_pheno %>% drop_na(class) %>% filter(class != "Other") %>% 
              group_by(class,month) %>% 
              summarize(mean_NDVI = median(NDVI)), 
            aes(x=month,y=mean_NDVI), 
            col="red") + 
  scale_y_continuous(limits=c(0,1)) + 
  facet_wrap(~as.character(class), ncol=1) +
  xlab("Month") + 
  ylab("Greenness (NDVI)")















local_sym_guilds <- raster("D:/SERDP/GEE_Classifier/Dar_manual_delineations/sym_fr_guilds_2021.tif")*0 + 1
gee_sym_guilds <- raster("D:/SERDP/GEE_Classifier/Dar_manual_delineations/sym_fr_guilds_2021.tif")
gee_sym_datastack <- stack("D:/SERDP/GEE_Classifier/Dar_manual_delineations/sym_fr_datastack_2021.tif")
low_relief_sym <- (gee_sym_datastack[[18]] < 5)
local_sym_guilds <- aggregate_custom(local_sym_guilds, gee_sym_guilds, reproj_method = "ngb")
gee_sym_guilds <- crop(gee_sym_guilds, local_sym_guilds)
gee_sym_datastack <- crop(gee_sym_datastack, local_sym_guilds)

sym_creek_data <- as.data.frame(stack(local_sym_guilds, gee_sym_guilds, gee_sym_datastack), xy=TRUE)
names(sym_creek_data) <- c("x", "y", "local_guild", "gee_guild", paste("ndvi_",1:12,sep=""), "summer_ndvi", "winter_ndvi", "stdev_ndvi", "max_ndvi", 
                                 "hod_1", "hod_2", "flow_accum", "dem", "slope", "precip", "tmax", "tmin", "vpd", "latitude",
                                 "blue", "green", "red", "NIR", "SWIR_1", "SWIR_2")
sym_creek_data$low_relief <- sym_creek_data$hod_2 < 10

plot((gee_sym_guilds==1) + (gee_sym_guilds==2)*low_relief_sym)
plot(local_sym_guilds==1)

# Fraction of local_san_antonio decid riparian labelled as deciduous --> 62.8%
nrow(sym_creek_data %>% filter(local_guild %in% c(1,2), gee_guild == 1) %>% drop_na(local_guild, gee_guild)) / nrow(sym_creek_data %>% filter(local_guild %in% c(1,2)) %>% drop_na(local_guild, gee_guild))
# Fraction of gee deciduous riparian which was labelled riparian in local model --> 89.5%
nrow(sym_creek_data %>% filter(local_guild %in% c(1,2), gee_guild == 1) %>% drop_na(local_guild, gee_guild)) / nrow(sym_creek_data %>% filter(gee_guild == 1) %>% drop_na(local_guild, gee_guild))
# Fraction of things labelled as deciduous, or evergreen woodland within HOD_2 < 10, which was deciduous riparian in the local model --> 89.5%
nrow(sym_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1)) & as.logical(local_guild %in% c(1,2))) %>% drop_na(local_guild, gee_guild)) / nrow(sym_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1))) %>% drop_na(local_guild, gee_guild))
# Similar to above, but inverted: fraction of local riparian which was included in GEE woodland --> 62.8%
nrow(sym_creek_data %>% filter(as.logical(low_relief*(gee_guild==2)+(gee_guild==1)) & as.logical(local_guild %in% c(1,2))) %>% drop_na(local_guild, gee_guild)) / nrow(sym_creek_data %>% filter(as.logical(local_guild %in% c(1,2))) %>% drop_na(local_guild, gee_guild))

sym_segments <- st_read("D:/SERDP/GEE_Classifier/Dar_manual_delineations/class_labels.shp")
sym_segments <- st_transform(sym_segments, st_crs(gee_sym_guilds))

sym_segment_data <- raster::extract(readAll(stack(local_sym_guilds, gee_sym_guilds, gee_sym_datastack)), sym_segments, cellnumbers=TRUE, sp=TRUE)

sym_classes <- c("Riparian Deciduous","Evergreen","Shrub","Grass","Bare")
names(sym_classes) <- 1:3
extractedSanPedroToDF <- function(index)
{
  output_samples <- as.data.frame(sym_segment_data[[index]])
  if(nrow(output_samples) == 0)
  {
    bad_data <- rep(-9999, length(c("cell", names(sym_creek_data)[3:(ncol(sym_creek_data)-1)])))
    names(bad_data) <- c("cell", names(sym_creek_data)[3:(ncol(sym_creek_data)-1)])
    return(bad_data)
  }  
  names(output_samples) <- c("cell", names(sym_creek_data)[3:(ncol(sym_creek_data)-1)])
  output_samples$class_num <- 1*(sym_segments[index,]$phenology=="deciduous") + 
    2*(sym_segments[index,]$phenology=="evergreen") + 
    3*(sym_segments[index,]$phenology=="drt_decid") +
    4*(sym_segments[index,]$phenology=="annual") +
    5*(sym_segments[index,]$phenology=="NA") 
  output_samples$class_num <- output_samples$class_num + (output_samples$class_num == 0) * 6
  output_samples$class_code <- sym_segments[index,]$species
  output_samples$structure <- sym_segments[index,]$structure
  output_samples$riparian <- (sym_segments[index,]$phenology == "deciduous") + (sym_segments[index,]$phenology == "evergreen")*(output_samples$hod_2 < 10)
  output_samples$phenology <- sym_segments[index,]$phenology
  output_samples$class <- as.character(sym_classes[output_samples$class_num])
  output_samples$riparian_woodland <- (output_samples$gee_guild == 1) + (output_samples$gee_guild == 2)*(output_samples$hod_2 < 10)
  output_samples$sample_num <- index
  return(output_samples)
}

sym_sample_df <- bind_rows(lapply(1:length(sym_segment_data), extractedSanPedroToDF))

# Stats broken down by group
sym_sample_df %>% group_by(class, gee_guild) %>% summarize(count = n(), hod_1 = mean(hod_1), frac_low = mean(hod_2 < 10))
# class              gee_guild count hod_1 frac_low
# <chr>                  <dbl> <int> <dbl>    <dbl>
#  Bare                       0    12  0       1    
#  Bare                       3    25 30.3     0.64 
#  Bare                       4     3  3       1    
#  Evergreen                  2   275 10.7     0.415
#  Evergreen                  4     7 15       0    
#  Grass                      4   127 20.7     0.236
#  Riparian Deciduous         1    82  4.61    0.805
#  Riparian Deciduous         2   139  3.76    0.878
#  Riparian Deciduous         4     1  7       1    
#  Shrub                      1     6  2       1    
#  Shrub                      2    88 52.4     0.102
#  Shrub                      3    11 11.4     0.273
#  Shrub                      4     3 30.3     0  
# of 204 points labelled deciduous manually, 
#   36% were labelled deciduous in GEE
#   62% were labelled evergreen in GEE
#   0.4% were labelled upland in GEE
#   of the points labelled evergreen, 87.8% were below 10 m in hod_2 and so were labelled 'riparian woodland'
#   so, 91.9% of pixels labelled deciduous riparian manually were called riparian woodland in GEE
#   most of the rest (7.6%) were called upland evergreen - probably due to the coarseness of the SRTM dataset and narrow canyons in mountains
# of 282 points labelled evergreen manually,
#   almost all were classified as evergreen in GEE. 7 were classified as crops
#   none were classified as deciduous

# NOTE this shows first input (GEE label) as the prediction values, and second (manual labels) as the reference
confusionMatrix(as.factor((sym_sample_df %>% drop_na(class, gee_guild, local_guild))$riparian_woodland==1),as.factor((sym_sample_df %>% drop_na(class, gee_guild, local_guild))$riparian==1))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction FALSE TRUE
# FALSE   428   18
# TRUE     15  318
# 
# Accuracy : 0.9576         
# 95% CI : (0.941, 0.9707)
# No Information Rate : 0.5687         
# P-Value [Acc > NIR] : <2e-16         
# 
# Kappa : 0.9136         
# 
# Mcnemar's Test P-Value : 0.7277         
#                                          
#             Sensitivity : 0.9661         
#             Specificity : 0.9464         
#          Pos Pred Value : 0.9596         
#          Neg Pred Value : 0.9550         
#              Prevalence : 0.5687         
#          Detection Rate : 0.5494         
#    Detection Prevalence : 0.5725         
#       Balanced Accuracy : 0.9563         
#                                          
#        'Positive' Class : FALSE   
#  Based on the above...
#   95.4% user's accuracy for riparian woodland class 
#   94.5% producer's accuracy for riparian woodland class
#   95.8% overall accuracy, kappa = 0.914 (unbalanced, because riparian class is rare)
#   95.9% user's accuracy for upland
#   96.6% producer's accuracy for upland 
       
# Which other classes were greatest source of error? 
#  Collimation error (non woodland labelled woodland):
sym_sample_df %>% 
  filter(riparian != 1, riparian_woodland == 1) %>% 
  group_by(class, gee_guild) %>% 
  summarize(count = n(), mean_hod_1 = mean(hod_1), min_hod = min(hod_1), max_hod=max(hod_1), mean_hod_2 = mean(hod_2), mean_summer_greenup = mean(ndvi_6+ndvi_7+ndvi_8-ndvi_1-ndvi_2-ndvi_3)) %>%
  ungroup() %>% 
  mutate(fractional_count = count / sum(count))
# class gee_guild count mean_hod_1 min_hod max_hod mean_hod_2 mean_summer_greenup fractional_count
# <chr>     <dbl> <int>      <dbl>   <dbl>   <dbl>      <dbl>               <dbl>            <dbl>
#  Shrub         1     6       2          2       2       5                  0.450              0.4
#  Shrub         2     9       4.33       4       5       4.33              -0.540              0.6
# all pixels labelled riparian which were not actually riparian were of the 'shrub' class (4.5% of mapped riparian pixels)
# of these, 40% were labelled deciduous, and 60% were labelled evergreen 

#  Omission error (riparian areas not labelled riparian woodland):
sym_sample_df %>% 
  filter(riparian == 1, riparian_woodland != 1) %>% 
  group_by(gee_guild, class) %>% 
  summarize(count = n(), mean_hod_1 = mean(hod_1), min_hod = min(hod_1), max_hod=max(hod_1), mean_hod_2 = mean(hod_2), mean_summer_greenup = mean(ndvi_6+ndvi_7+ndvi_8-ndvi_1-ndvi_2-ndvi_3)) %>%
  mutate(fractional_count = count / sum(count))
# gee_guild class              count mean_hod_1 min_hod max_hod mean_hod_2 mean_summer_greenup fractional_count
# <dbl> <chr>              <int>      <dbl>   <dbl>   <dbl>      <dbl>               <dbl>            <dbl>
#   2 Riparian Deciduous    17       12.3       3      19       15.2               0.408                1
#   4 Riparian Deciduous     1        7         7       7        9                 0.148                1
# all the omitted riparian pixels were of the deciduous type
# 94% of those omitted pixels (which accounted for 5% of all true riparian woodland pixels) were labelled evergreen, and were mapped as 'too high' to be riparian
# Again, note that there are issues with using SRTM HOD values in narrow mountain canyons 


# Make some pretty plots of phenology from GEE? 
#   first, pivot data to longer format
sym_df_pheno <- sym_sample_df %>% 
  pivot_longer(4:15, names_to="month_string", values_to="NDVI") %>%
  mutate(month = as.numeric(substr(month_string, 6,10))) 


ggplot() + 
  geom_density_2d_filled(data=sym_df_pheno %>% drop_na(class,gee_guild) %>% filter(class != "Other"), 
                         aes(x=month,y=NDVI), contour_var = "ndensity") + 
  geom_line(data=sym_df_pheno %>% drop_na(class,gee_guild) %>% filter(class != "Other") %>% 
              group_by(class,month) %>% 
              summarize(mean_NDVI = median(NDVI)), 
            aes(x=month,y=mean_NDVI), 
            col="red") + 
  scale_y_continuous(limits=c(0,1)) + 
  facet_wrap(~as.character(class), nrow=5) +
  xlab("Month") + 
  ylab("Greenness (NDVI)")








# pendleton change in riparian woodland over time
santa_margarita_shapefile <- st_read("D:/SERDP/Pendleton/Riparian_Zone/manual/coarse/santa_margarita_shell.shp")
pendleton_datastack_files <- list.files("D:/SERDP/Pendleton/gee_timeseries/", pattern="*guild*")
pendleton_guild_files <- list.files("D:/SERDP/Pendleton/gee_timeseries/", pattern="*datastack*")
pendleton_cloudiness_files <- list.files("D:/SERDP/Pendleton/gee_timeseries/", pattern="*cloud*")
years <- str_extract(pendleton_guild_files, "[0-9]{4}")
pendleton_datastack_timeseries <- stack(lapply(paste("D:/SERDP/Pendleton/gee_timeseries/",pendleton_datastack_files,sep=""),stack))
pendleton_cloudiness_timeseries <- stack(lapply(paste("D:/SERDP/Pendleton/gee_timeseries/",pendleton_cloudiness_files,sep=""),stack))
names(pendleton_datastack_timeseries) <- paste(rep(c(paste("ndvi_",1:12,sep=""), "summer_ndvi", "winter_ndvi", "stdev_ndvi", "max_ndvi", 
                                                     "hod_1", "hod_2", "flow_accum", "dem", "slope", "precip", "tmax", "tmin", "vpd", "latitude",
                                                     "blue", "green", "red", "NIR", "SWIR_1", "SWIR_2"),
                                                   length(pendleton_guild_files)),
                                               "_", unlist(lapply(years, function(x) { rep(x, dim(pendleton_datastack_timeseries)[[3]]/length(years)) })), sep="")
pendleton_guild_timeseries <- stack(lapply(paste("D:/SERDP/Pendleton/gee_timeseries/",pendleton_guild_files,sep=""),raster))
pendleton_cloudiness_timeseries <- stack(lapply(paste("D:/SERDP/Pendleton/gee_timeseries/",pendleton_cloudiness_files,sep=""),stack))
pendleton_riparian_woodlands <- (pendleton_guild_timeseries == 1) + (pendleton_guild_timeseries == 2) * (pendleton_datastack_timeseries[[18]] < 10)

santa_margarita_timeseries <- mask(crop(pendleton_datastack_timeseries, santa_margarita_shapefile), santa_margarita_shapefile)

pendleton_phenology_timeseries <- pendleton_datastack_timeseries[[rep(1:12, 34) + unlist(lapply((0:33)*32, function(x) {rep(x,12)} ))]]
writeRaster(pendleton_phenology_timeseries, "D:/SERDP/Pendleton/gee_timeseries/phenology_timeseries.tif")







# pendleton change in riparian woodland over time
# san_pedro_shapefile <- st_read("D:/SERDP/Pendleton/Riparian_Zone/manual/coarse/santa_margarita_shell.shp")
san_pedro_guild_files <- list.files("D:/SERDP/Huachuca/gee_timeseries/", pattern="*guild*")
san_pedro_datastack_files <- list.files("D:/SERDP/Huachuca/gee_timeseries/", pattern="*datastack*")
san_pedro_cloudiness_files <- list.files("D:/SERDP/Huachuca/gee_timeseries/", pattern="*cloud*")
years <- str_extract(san_pedro_guild_files, "[0-9]{4}")
san_pedro_datastack_timeseries <- stack(lapply(paste("D:/SERDP/Huachuca/gee_timeseries/",san_pedro_datastack_files,sep=""),stack))
san_pedro_cloudiness_timeseries <- stack(lapply(paste("D:/SERDP/Huachuca/gee_timeseries/",san_pedro_cloudiness_files,sep=""),stack))
san_pedro_guilds_timeseries <- stack(lapply(paste("D:/SERDP/Huachuca/gee_timeseries/",san_pedro_guild_files,sep=""),stack))
names(san_pedro_datastack_timeseries) <- paste(rep(c(paste("ndvi_",1:12,sep=""), "summer_ndvi", "winter_ndvi", "stdev_ndvi", "max_ndvi", 
                                                     "hod_1", "hod_2", "flow_accum", "dem", "slope", "precip", "tmax", "tmin", "vpd", "latitude",
                                                     "blue", "green", "red", "NIR", "SWIR_1", "SWIR_2"),
                                                   length(san_pedro_guild_files)),
                                               "_", unlist(lapply(years, function(x) { rep(x, dim(san_pedro_datastack_timeseries)[[3]]/length(years)) })), sep="")
san_pedro_guild_timeseries <- stack(lapply(paste("D:/SERDP/Huachuca/gee_timeseries/",san_pedro_guild_files,sep=""),raster))
san_pedro_cloudiness_timeseries <- stack(lapply(paste("D:/SERDP/Huachuca/gee_timeseries/",san_pedro_cloudiness_files,sep=""),stack))
san_pedro_riparian_woodlands <- (san_pedro_guild_timeseries == 1) + (san_pedro_guild_timeseries == 2) * (san_pedro_datastack_timeseries[[18]] < 10)

#santa_margarita_timeseries <- mask(crop(san_pedro_datastack_timeseries, santa_margarita_shapefile), santa_margarita_shapefile)

#san_pedro_phenology_timeseries <- san_pedro_datastack_timeseries[[rep(1:12, 34) + unlist(lapply((0:33)*32, function(x) {rep(x,12)} ))]]
#writeRaster(san_pedro_phenology_timeseries, "D:/SERDP/Huachuca/gee_timeseries/phenology_timeseries.tif")

san_pedro_ndvi_1 <- san_pedro_datastack_timeseries[[(0:32)*32+1]]
san_pedro_ndvi_2 <- san_pedro_datastack_timeseries[[(0:32)*32+2]]
san_pedro_ndvi_3 <- san_pedro_datastack_timeseries[[(0:32)*32+3]]
san_pedro_ndvi_4 <- san_pedro_datastack_timeseries[[(0:32)*32+4]]
san_pedro_ndvi_5 <- san_pedro_datastack_timeseries[[(0:32)*32+5]]
san_pedro_ndvi_6 <- san_pedro_datastack_timeseries[[(0:32)*32+6]]
san_pedro_ndvi_7 <- san_pedro_datastack_timeseries[[(0:32)*32+7]]
san_pedro_ndvi_8 <- san_pedro_datastack_timeseries[[(0:32)*32+8]]
san_pedro_ndvi_9 <- san_pedro_datastack_timeseries[[(0:32)*32+9]]
san_pedro_ndvi_10 <- san_pedro_datastack_timeseries[[(0:32)*32+10]]
san_pedro_ndvi_11 <- san_pedro_datastack_timeseries[[(0:32)*32+11]]
san_pedro_ndvi_12 <- san_pedro_datastack_timeseries[[(0:32)*32+12]]

san_pedro_phenology_timeseries <- as.data.frame(stack(san_pedro_ndvi_1, san_pedro_ndvi_2, san_pedro_ndvi_3, san_pedro_ndvi_4, san_pedro_ndvi_5, san_pedro_ndvi_6, san_pedro_ndvi_7, san_pedro_ndvi_8, san_pedro_ndvi_9, san_pedro_ndvi_10, san_pedro_ndvi_11, san_pedro_ndvi_12, sum(san_pedro_guilds_timeseries==1)/33 > 0.5), xy=TRUE)
san_pedro_phenology_longer <- san_pedro_phenology_timeseries %>% 
  pivot_longer(3:(ncol(san_pedro_phenology_timeseries)-1), 
               names_to="long_format_name",
               values_to="NDVI") %>%
  mutate(year = as.numeric(substr(long_format_name, 29, 32)),
         month = as.numeric(substr(long_format_name, 34, 100)))