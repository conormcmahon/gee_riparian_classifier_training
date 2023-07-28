

library(raster)
library(tidyverse)
library(sf)
library(fasterize)

temp_data <- stack("D:/SERDP/GEE_Classifier/Landsat_TIR/east_grove_temp_timeseries_absolute.tif")

study_areas <- st_read("D:/santa_clara_birds/study_sites_feb25/study_sites_feb25.shp")
study_areas <- st_transform(study_areas, st_crs('EPSG:4326'))

study_areas_raster <- fasterize(study_areas, temp_data[[1]], field="name", fun="sum")

temp_data <- stack(temp_data, study_areas_raster)

temp_data_df <- as.data.frame(temp_data, xy=TRUE)
names(temp_data_df) <- c("x","y",paste("temperature_",2014:2021,sep=""),"region")
temp_data_df_filt <- temp_data_df %>% filter(!is.na(region))

temp_data_df_longer <- temp_data_df_filt %>% 
  pivot_longer(cols = 3:10,
               values_to = "temp_diff",
               names_to = "year_string") %>%
  mutate(year = as.numeric(substr(year_string, 13, 16)))

ggplot(temp_data_df_longer) + 
  geom_boxplot(aes(x=year, group=year, y=temp_diff)) + 
  facet_wrap(~region, ncol=1) + 
  geom_hline(yintercept=0, col="red", linetype="dashed")