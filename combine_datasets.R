

library(sf)
library(tidyverse)

# Combine Training Datasets

sou_coa_train <- st_read(here::here("..","CalVeg","sou_coast","calveg_training.shp"))
sou_int_train <- st_read(here::here("..","CalVeg","sou_int","calveg_training.shp"))
sou_srr_train <- st_read(here::here("..","CalVeg","sou_sierra","calveg_training.shp"))
cen_coa_train <- st_read(here::here("..","CalVeg","cen_coast","calveg_training.shp"))
cen_val_train <- st_read(here::here("..","CalVeg","cen_valley","calveg_training.shp"))
grt_bsn_train <- st_read(here::here("..","CalVeg","grt_bsn","calveg_training.shp"))

all_train <- rbind(sou_coa_train,
                   sou_int_train,
                   sou_srr_train,
                   cen_coa_train,
                   cen_val_train,
                   grt_bsn_train)
all_train$date <- as.Date(paste(all_train$src_dt_y,all_train$src_dt_m,all_train$src_dt_d,sep="-"))

st_write(all_train, 
         here::here("..","CalVeg","calveg_training_all.shp"),
         layer="calveg_training_all",
         driver="ESRI Shapefile",
         delete_dsn = TRUE)
# also print separate datasets for the riparian and non-riparian classes 
st_write(all_train %>% filter(rprn == 1), 
         here::here("..","CalVeg","calveg_training_riparian.shp"),
         layer="calveg_validation_riparian",
         driver="ESRI Shapefile",
         delete_dsn = TRUE)
st_write(all_train %>% filter(rprn != 1), 
         here::here("..","CalVeg","calveg_training_nonriparian.shp"),
         layer="calveg_validation_nonriparian",
         driver="ESRI Shapefile",
         delete_dsn = TRUE)

# Combine Validation Datasets

sou_coa_valid <- st_read(here::here("..","CalVeg","sou_coast","calveg_validation.shp"))
sou_int_valid <- st_read(here::here("..","CalVeg","sou_int","calveg_validation.shp"))
sou_srr_valid <- st_read(here::here("..","CalVeg","sou_sierra","calveg_validation.shp"))
cen_coa_valid <- st_read(here::here("..","CalVeg","cen_coast","calveg_validation.shp"))
cen_val_valid <- st_read(here::here("..","CalVeg","cen_valley","calveg_validation.shp"))
grt_bsn_valid <- st_read(here::here("..","CalVeg","grt_bsn","calveg_validation.shp"))

all_valid <- rbind(sou_coa_valid,
                   sou_int_valid,
                   sou_srr_valid,
                   cen_coa_valid,
                   cen_val_valid,
                   grt_bsn_valid)
all_valid$date <- as.Date(paste(all_valid$src_dt_y,all_valid$src_dt_m,all_valid$src_dt_d,sep="-"))


st_write(all_valid, 
         here::here("..","CalVeg","calveg_validation_all.shp"),
         layer="calveg_validation_all",
         driver="ESRI Shapefile",
         delete_dsn = TRUE)
# also print separate datasets for the riparian and non-riparian classes 
st_write(all_valid %>% filter(rprn == 1), 
         here::here("..","CalVeg","calveg_validation_riparian.shp"),
         layer="calveg_validation_riparian",
         driver="ESRI Shapefile",
         delete_dsn = TRUE)
st_write(all_valid %>% filter(rprn != 1), 
         here::here("..","CalVeg","calveg_validation_nonriparian.shp"),
         layer="calveg_validation_nonriparian",
         driver="ESRI Shapefile",
         delete_dsn = TRUE)
