
library(sf)
library(tidyverse)
library(here)
library(caret)

# Note - first, many local and annual shapefiles of training points are merged in QGIS using the tool
#   Vector -> Data Management Tools -> Merge Vector Layers

pheno_bands <- c("X00_NDVI", "X01_NDVI", "X02_NDVI", "X03_NDVI", "X04_NDVI", "X05_NDVI", "X06_NDVI", "X07_NDVI", "X08_NDVI", "X09_NDVI", "X10_NDVI","X11_NDVI")

# First off, generate some visualizations around the overall riparian training dataset
riparian_training <- st_read(here::here("..","CalVeg","gee_pheno_points","riparian_training.shp"))
riparian_training_last_year <- st_read(here::here("..","CalVeg","gee_pheno_points","riparian_training_lastyear.shp")) %>%
  select(c(pheno_bands, "geometry"))
names(riparian_training_last_year) <- c(paste(pheno_bands, "_last_year", sep=""), "geometry")
# Get angle between current and previous year's phenology vectors
#   theta = acos( A • B / |A| / |B| )
riparian_training <- distinct(st_join(riparian_training, riparian_training_last_year)) 
current_spectra <- as.data.frame(riparian_training)[,pheno_bands]
previous_spectra <- as.data.frame(riparian_training)[,paste(pheno_bands,"_last_year",sep="")]
current_spectral_magnitude <- as.numeric(sqrt(rowSums(current_spectra*current_spectra)))
previous_spectral_magnitude <- as.numeric(sqrt(rowSums(previous_spectra*previous_spectra)))
riparian_training$pheno_change_angle <- acos( as.numeric(rowSums(current_spectra*previous_spectra)) / previous_spectral_magnitude / current_spectral_magnitude )

# Add in NVCS designations
riparian_training_nvcs <- st_read(here::here("..","CalVeg","calveg_training_riparian.shp"))
riparian_training_nvcs_polygon <- st_transform(st_buffer(st_transform(riparian_training_nvcs[1:16], st_crs("epsg:32610")), 20), st_crs(riparian_training))
riparian_training <- distinct(st_join(riparian_training, riparian_training_nvcs_polygon))

# Add in winter and summer spectra
reflectance_bands <- c("blue_summe","green_summ","red_summer","NIR_summer","SWIR1_summ","SWIR2_summ",
                       "blue_winte","green_wint","red_winter","NIR_winter","SWIR1_wint","SWIR2_wint")
wavelengths <- c(0.485,0.56,0.66,0.835,1.65,2.22,
                 0.485,0.56,0.66,0.835,1.65,2.22)
spectra_riparian <- st_read(here::here("..","CalVeg","gee_pheno_points","seasonal_spectra_all.shp"))
spectra_riparian <- spectra_riparian[,c(reflectance_bands,"guld","geometry")]
spectra_riparian_long <- spectra_riparian %>% 
  pivot_longer(1:12,names_to="band",values_to="reflectance") %>%
  mutate(band = band) %>%
  mutate(band_ind = match(band, reflectance_bands)) %>% 
  mutate(season = band_ind <= 6) %>%
  mutate(season_fact = c("winter","summer")[season*1+1],
         wavelength = wavelengths[band_ind])
# Reflectance Plots
ggplot() +
  geom_line(data = spectra_riparian_long  %>% 
              group_by(guld, wavelength, season_fact) %>% 
              summarize(reflectance = median(reflectance, na.rm=TRUE)), 
            aes(x=wavelength,y=reflectance, group=season_fact, col=season_fact)) + facet_wrap(~guld) + 
  scale_y_continuous(limits=c(0,0.4))
riparian_training <- distinct(st_join(riparian_training, spectra_riparian %>% select(-guld)))

# write a new copy of the data which puts phenology bands in order, and before other data (more readable for humans)
st_write(st_join(riparian_training[,pheno_bands], 
                 riparian_training[,which(!((names(riparian_training)) %in% pheno_bands))]),
         here::here("..","CalVeg","gee_pheno_points","riparian_training_ordered.shp"),
         append=FALSE)

riparian_training_pheno <- riparian_training[, c(pheno_bands, "dem", "slope", "tmin", "vpd", "latitude")]
riparian_training_pheno_longer <- as.data.frame(riparian_training_pheno) %>% 
  pivot_longer(1:12, names_to="month_string", values_to="NDVI") %>% 
  mutate(month=as.numeric(substr(month_string,2,3)))

ggplot(riparian_training) + geom_histogram(aes(x=src_dt_y))

ggplot(riparian_training_pheno_longer) + geom_density_2d_filled(aes(y=NDVI, x=month))






# Now, something similar for the non-riparian points:


# First off, generate some visualizations around the overall riparian training dataset
nonriparian_training <- st_read(here::here("..","CalVeg","gee_pheno_points","nonriparian_training.shp"))
nonriparian_training_last_year <- st_read(here::here("..","CalVeg","gee_pheno_points","nonriparian_training_lastyear.shp")) %>%
  select(c(pheno_bands, "geometry"))
names(nonriparian_training_last_year) <- c(paste(pheno_bands, "_last_year", sep=""), "geometry")
# Get angle between current and previous year's phenology vectors
#   theta = acos( A • B / |A| / |B| )
nonriparian_training <- distinct(st_join(nonriparian_training, nonriparian_training_last_year)) 
current_spectra <- as.data.frame(nonriparian_training)[,pheno_bands]
previous_spectra <- as.data.frame(nonriparian_training)[,paste(pheno_bands,"_last_year",sep="")]
current_spectral_magnitude <- as.numeric(sqrt(rowSums(current_spectra*current_spectra)))
previous_spectral_magnitude <- as.numeric(sqrt(rowSums(previous_spectra*previous_spectra)))
nonriparian_training$pheno_change_angle <- acos( as.numeric(rowSums(current_spectra*previous_spectra)) / previous_spectral_magnitude / current_spectral_magnitude )


# Add in NVCS designations
nonriparian_training_nvcs <- st_read(here::here("..","CalVeg","calveg_training_nonriparian.shp"))
nonriparian_training_nvcs_polygon <- st_transform(st_buffer(st_transform(nonriparian_training_nvcs[1:16], st_crs("epsg:32610")), 20), st_crs(nonriparian_training))
nonriparian_training <- st_join(nonriparian_training, nonriparian_training_nvcs_polygon)


# Add in winter and summer spectra
reflectance_bands <- c("blue_summe","green_summ","red_summer","NIR_summer","SWIR1_summ","SWIR2_summ",
                       "blue_winte","green_wint","red_winter","NIR_winter","SWIR1_wint","SWIR2_wint")
wavelengths <- c(0.485,0.56,0.66,0.835,1.65,2.22,
                 0.485,0.56,0.66,0.835,1.65,2.22)
spectra_nonriparian <- st_read(here::here("..","CalVeg","gee_pheno_points","seasonal_spectra_all.shp"))
spectra_nonriparian <- spectra_nonriparian[,c(reflectance_bands,"guld","geometry")]
spectra_nonriparian_long <- spectra_riparian %>% 
  pivot_longer(1:12,names_to="band",values_to="reflectance") %>%
  mutate(band = band) %>%
  mutate(band_ind = match(band, reflectance_bands)) %>% 
  mutate(season = band_ind <= 6) %>%
  mutate(season_fact = c("winter","summer")[season*1+1],
         wavelength = wavelengths[band_ind])
# Reflectance Plots
ggplot() +
  geom_line(data = spectra_riparian_long  %>% 
              group_by(guld, wavelength, season_fact) %>% 
              summarize(reflectance = median(reflectance, na.rm=TRUE)), 
            aes(x=wavelength,y=reflectance, group=season_fact, col=season_fact)) + facet_wrap(~guld) + 
  scale_y_continuous(limits=c(0,0.4))
nonriparian_training <- distinct(st_join(nonriparian_training, spectra_riparian %>% select(-guld)))




# write a new copy of the data which puts phenology bands in order, and before other data (more readable for humans)
st_write(st_join(nonriparian_training[,pheno_bands], 
                 nonriparian_training[,which(!((names(nonriparian_training)) %in% pheno_bands))]),
         here::here("..","CalVeg","gee_pheno_points","nonriparian_training_ordered.shp"),
         append=FALSE)

nonriparian_training_pheno <- nonriparian_training[, c(pheno_bands, "dem", "slope", "tmin", "vpd", "latitude", "guld", "clvg")] %>%
  mutate(max_ndvi = max(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI),
         min_ndvi = min(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI))

nonriparian_training_pheno_longer <- as.data.frame(nonriparian_training_pheno) %>% 
  pivot_longer(1:12, names_to="month_string", values_to="NDVI") %>% 
  mutate(month=as.numeric(substr(month_string,2,3)))


ggplot(nonriparian_training_pheno_longer) + geom_density_2d_filled(aes(y=NDVI, x=month)) + facet_wrap(~guld)




# Example Classifier
all_data <- rbind(riparian_training,nonriparian_training) 
all_data <- st_join(all_data[,pheno_bands], 
                    all_data[,which(!((names(all_data)) %in% pheno_bands))]) %>% 
  mutate(guld = as.factor(guld)) %>% 
  mutate(rprn = as.factor(rprn)) %>% 
  mutate(wdln = as.factor(wdln)) %>%
  rowwise() %>%
  mutate(max_ndvi = max(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI),
         min_ndvi = min(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI)) %>%
  mutate(stdev_ndvi = sd(c(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI)),
         mean_ndvi = mean(c(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI)),
         ndvi_change = max_ndvi-min_ndvi,
         summer_ndvi = (X05_NDVI+X06_NDVI+X07_NDVI)/3,
         winter_ndvi = (X00_NDVI+X01_NDVI)/2) %>%
  mutate(summer_greenup = summer_ndvi - winter_ndvi)
# For some reason, there are a bunch of duplicate points. They seem to have identical metadata so lets just delete them down to one copy
all_data <- distinct(all_data)

# write a new copy of the data which puts phenology bands in order, and before other data (more readable for humans)
st_write(all_data,
         here::here("..","CalVeg","gee_pheno_points","all_training_data_ordered.shp"),
         append=FALSE)

ggplot(all_data) + 
  geom_histogram(aes(x=pheno_change_angle)) + 
  facet_wrap(~guld)



# Clustering on guilds to reject invalid points 

num_clusters <- 30
set.seed(343)
# Clusters on Deciduous Riparian Woodland 
riparian_clusters <- kmeans(as.matrix((all_data %>% filter(guld==1))[,pheno_bands])[,1:12],centers=num_clusters)
riparian_cluster_means <- as.data.frame(riparian_clusters$centers) %>%
  mutate(cluster = 1:num_clusters) %>%
  mutate(summer_NDVI = (X05_NDVI + X06_NDVI + X07_NDVI)/3,
         winter_NDVI = (X01_NDVI + X02_NDVI)/2) %>%
  mutate(summer_greenup = summer_NDVI - winter_NDVI) %>%
  rowwise() %>%
  mutate(stdev_NDVI = sd(c(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI)),
         mean_NDVI = mean(c(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI)))
non_deciduous_riparian <- riparian_cluster_means %>% 
  filter(((summer_greenup < 0.1) + (summer_NDVI < 0.5)) > 0) %>%
  pivot_longer(1:12, names_to = "month_string", values_to = "NDVI") %>%
  mutate(month = substr(month_string, 2,3))
deciduous_riparian <- riparian_cluster_means %>% 
  filter(((summer_greenup > 0.1) * (summer_NDVI > 0.5)) > 0) %>%
  pivot_longer(1:12, names_to = "month_string", values_to = "NDVI") %>%
  mutate(month = substr(month_string, 2,3))
ggplot(deciduous_riparian) + 
  geom_line(aes(x=month, y=NDVI, group=cluster, col=as.factor(cluster)))
ggplot(non_deciduous_riparian) + 
  geom_line(aes(x=month, y=NDVI, group=cluster, col=as.factor(cluster)))

riparian_data <- all_data %>% filter(guld==1)
riparian_data$cluster <- riparian_clusters$cluster
deciduous_riparian_st <- riparian_data %>% 
  filter(cluster %in% unique(deciduous_riparian$cluster),
         hod_1 < 20) 
st_write(st_join(deciduous_riparian_st[,pheno_bands], 
                 deciduous_riparian_st[,which(!((names(deciduous_riparian_st)) %in% pheno_bands))]),
         here::here("..","CalVeg","gee_pheno_points","deciduous_riparian_data_filtered.shp"),
         append=FALSE)
# Get non-deciduous riparian points, plus points which are deciduous but not riparian (HOD_1 > 20)
non_deciduous_riparian_st <- riparian_data %>% 
  filter(cluster %in% unique(non_deciduous_riparian$cluster))
non_deciduous_riparian_st <- rbind(non_deciduous_riparian_st, 
                                   riparian_data %>% filter(cluster %in% unique(deciduous_riparian$cluster), hod_1 >= 20))
st_write(st_join(non_deciduous_riparian_st[,pheno_bands], 
                 non_deciduous_riparian_st[,which(!((names(non_deciduous_riparian_st)) %in% pheno_bands))]),
         here::here("..","CalVeg","gee_pheno_points","non_deciduous_riparian_data_filtered.shp"),
         append=FALSE)

# Now, repeat for evergreen woodland class
set.seed(12)
evergreen_clusters <- kmeans(as.matrix((all_data %>% filter(guld==2))[,pheno_bands])[,1:12], centers=num_clusters)
evergreen_clusters$cluster <- evergreen_clusters$cluster + num_clusters
evergreen_cluster_means <- as.data.frame(evergreen_clusters$centers) %>%
  mutate(cluster = (num_clusters+1):(num_clusters*2)) %>%
  rowwise() %>%
  mutate(summer_NDVI = (X05_NDVI + X06_NDVI + X07_NDVI)/3,
         winter_NDVI = (X01_NDVI + X02_NDVI)/2,
         stdev_NDVI = sd(c(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI)),
         mean_NDVI = mean(c(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI))) %>%
  mutate(summer_greenup = summer_NDVI - winter_NDVI)
evergreen_woodland <- evergreen_cluster_means %>% 
  filter(mean_NDVI > 0.5, stdev_NDVI < mean_NDVI/10) %>%
  pivot_longer(1:12, names_to = "month_string", values_to = "NDVI") %>%
  mutate(month = substr(month_string, 2,3)) 
sparsely_wooded <- evergreen_cluster_means %>% 
  filter(mean_NDVI < 0.5) %>%
  pivot_longer(1:12, names_to = "month_string", values_to = "NDVI") %>%
  mutate(month = substr(month_string, 2,3)) 
# Add in the vegetation from the riparian class which fits the evergreen standard
evergreen_woodland <- rbind(evergreen_woodland, 
                            non_deciduous_riparian %>% filter(mean_NDVI > 0.5, stdev_NDVI < mean_NDVI/10))
ggplot(evergreen_woodland) + 
  geom_line(aes(x=month, y=NDVI, group=cluster, col=as.factor(cluster)))


evergreen_data <- all_data %>% filter(guld==2)
evergreen_data$cluster <- evergreen_clusters$cluster
evergreen_woodland_st <- rbind(evergreen_data %>% filter(cluster %in% unique(evergreen_woodland$cluster)) %>% mutate(guld = 2, rprn = 0, wdln = 1),
                               riparian_data %>% filter(cluster %in% unique(evergreen_woodland$cluster)) %>% mutate(guld = 2, rprn = 0, wdln = 1))
st_write(st_join(evergreen_woodland_st[,pheno_bands], 
                 evergreen_woodland_st[,which(!((names(evergreen_woodland_st)) %in% pheno_bands))]),
         here::here("..","CalVeg","gee_pheno_points","evergreen_woodland_data_filtered.shp"),
         append=FALSE)


# We'll also build clusters for the upland and agriculture classes, but won't filter on them
# These actually represent many subclasses and it's not an objective of this project to separate them
set.seed(49)
upland_clusters <- kmeans(as.matrix((all_data %>% filter(guld==3))[,pheno_bands])[,1:12], centers=num_clusters)
upland_clusters$cluster <- upland_clusters$cluster + 2*num_clusters
upland_cluster_means <- as.data.frame(upland_clusters$centers) %>%
  mutate(cluster = (2*num_clusters+1):(num_clusters*3)) %>%
  rowwise() %>%
  mutate(summer_NDVI = (X05_NDVI + X06_NDVI + X07_NDVI)/3,
         winter_NDVI = (X01_NDVI + X02_NDVI)/2,
         stdev_NDVI = sd(c(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI)),
         mean_NDVI = mean(c(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI))) %>%
  mutate(summer_greenup = summer_NDVI - winter_NDVI)
upland <- upland_cluster_means %>% 
  pivot_longer(1:12, names_to = "month_string", values_to = "NDVI") %>%
  mutate(month = substr(month_string, 2,3)) 
# Also, add in the riparian data with too low NDVI to be woodland 
upland <- rbind(upland, 
                non_deciduous_riparian %>% filter(mean_NDVI < 0.5))


ggplot(upland) + 
  geom_line(aes(x=month, y=NDVI, group=cluster, col=as.factor(cluster)))

upland_data <- all_data %>% filter(guld==3)
upland_data$cluster <- upland_clusters$cluster
upland_st <- rbind(upland_data %>% mutate(guld = 3, rprn = 0, wdln = 0),
                   riparian_data %>% filter(cluster %in% unique(upland$cluster)) %>% mutate(guld = 3, rprn = 0, wdln = 0),
                   riparian_data %>% filter(cluster %in% unique(deciduous_riparian$cluster), hod_1 > 20) %>% mutate(guld = 3, rprn = 0, wdln = 0))
st_write(st_join(upland_st[,pheno_bands], 
                 upland_st[,which(!((names(upland_st)) %in% pheno_bands))]),
         here::here("..","CalVeg","gee_pheno_points","upland_data_clustered.shp"),
         append=FALSE)




# So now, the 'cultural' agriculture/planted class
set.seed(49)
cultural_clusters <- kmeans(as.matrix((all_data %>% filter(guld==4))[,pheno_bands])[,1:12], centers=num_clusters)
cultural_clusters$cluster <- cultural_clusters$cluster + 3*num_clusters
cultural_cluster_means <- as.data.frame(cultural_clusters$centers) %>%
  mutate(cluster = (3*num_clusters+1):(num_clusters*4)) %>%
  rowwise() %>%
  mutate(summer_NDVI = (X05_NDVI + X06_NDVI + X07_NDVI)/3,
         winter_NDVI = (X01_NDVI + X02_NDVI)/2,
         stdev_NDVI = sd(c(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI)),
         mean_NDVI = mean(c(X00_NDVI, X01_NDVI, X02_NDVI, X03_NDVI, X04_NDVI, X05_NDVI, X06_NDVI, X07_NDVI, X08_NDVI, X09_NDVI, X10_NDVI,X11_NDVI))) %>%
  mutate(summer_greenup = summer_NDVI - winter_NDVI)
cultural <- cultural_cluster_means %>% 
  pivot_longer(1:12, names_to = "month_string", values_to = "NDVI") %>%
  mutate(month = substr(month_string, 2,3)) 
ggplot(cultural) + 
  geom_line(aes(x=month, y=NDVI, group=cluster, col=as.factor(cluster)))

cultural_data <- all_data %>% filter(guld==4)
cultural_data$cluster <- cultural_clusters$cluster
cultural_st <- cultural_data 
st_write(st_join(cultural_st[,pheno_bands], 
                 cultural_st[,which(!((names(cultural_st)) %in% pheno_bands))]),
         here::here("..","CalVeg","gee_pheno_points","cultural_data_clustered.shp"),
         append=FALSE)




# Combine newly filtered datasets

filtered_data <- rbind(deciduous_riparian_st,
                         evergreen_woodland_st,
                         upland_st,
                         cultural_st) 
filtered_data <- filtered_data[!is.na(filtered_data$pheno_change_angle),]
filtered_data <- filtered_data[!is.na(filtered_data$blue_summe),]
filtered_data <- filtered_data[!is.na(filtered_data$blue_winte),]
st_write(st_join(filtered_data[,pheno_bands],
                 filtered_data[,which(!((names(filtered_data)) %in% pheno_bands))]),
         here::here("..","CalVeg","gee_pheno_points","all_training_data_filtered.shp"),
         append=FALSE)

filtered_data_pretty_factors <- filtered_data
levels(filtered_data_pretty_factors$guld) <- c("Deciduous Riparian Woodland","Other Woodland","Other Natural Vegetation","Cultivated Vegetation")
levels(filtered_data_pretty_factors$rprn) <- c("Deciduous Riparian Woodland","Other Vegetation Type")
levels(filtered_data_pretty_factors$wdln) <- c("Woodland","Other Vegetation Type")
levels(filtered_data_pretty_factors$clvg) <- c("Central Valley","Southern Sierran","Central Coast","South Coast","South Interior","Great Basin")


# 
# # Some sanity check filters...
# #   Riparian woodland should be deciduous. Here, we'll say that NDVI in the season should have to vary by at least 0.2
# riparian_indices <- which(as.numeric(as.character(all_data$rprn)) == 1)
# riparian_data <- all_data[riparian_indices,]
# deciduous_riparian_data <- riparian_data[which(riparian_data$ndvi_summer_increase > 0.1),]
# unchanging_riparian_data <- riparian_data[which(riparian_data$ndvi_summer_increase <= 0.1),] %>%
#   mutate(rprn = 0,
#          wdln = 1,
#          guld = 2)
# filtered_data <- rbind(all_data[-riparian_indices,],
#                        deciduous_riparian_data,
#                        unchanging_riparian_data)
# #   Woodland should have maximum NDVI at some point during the year of at least 0.4
# woodland_indices <- which(as.numeric(as.character(filtered_data$rprn)) == 1)
# woodland_data <- filtered_data[woodland_indices,]
# dense_woodland_data <- woodland_data[which(woodland_data$summer_ndvi > 0.4),]
# sparse_woodland_data <- woodland_data[which(woodland_data$summer_ndvi <= 0.4),] %>%
#   mutate(rprn = 0,
#          wdln = 0,
#          guld = 3)
# filtered_data <- rbind(filtered_data[-woodland_indices,],
#                        dense_woodland_data,
#                        sparse_woodland_data) 
# filtered_data_pretty_factors <- filtered_data
# levels(filtered_data_pretty_factors$guld) <- c("Deciduous Riparian Woodland","Other Woodland","Other Natural Vegetation","Cultivated Vegetation")
# levels(filtered_data_pretty_factors$rprn) <- c("Deciduous Riparian Woodland","Other Vegetation Type")
# levels(filtered_data_pretty_factors$wdln) <- c("Woodland","Other Vegetation Type")
# levels(filtered_data_pretty_factors$clvg) <- c("Central Valley","Southern Sierran","Central Coast","South Coast","South Interior","Great Basin")
# 
# 
# # write a new copy of the filtered data
# st_write(st_join(filtered_data[,pheno_bands], 
#                  filtered_data[,which(!((names(filtered_data)) %in% pheno_bands))]),
#          here::here("..","CalVeg","gee_pheno_points","all_training_data_filtered.shp"),
#          append=FALSE)

  

# Pivot to a long format, with NDVI for each month as a separate row
filtered_data_pheno_longer = as.data.frame(filtered_data[, c(pheno_bands, "dem", "slope", "tmin", "vpd", "latitude", "guld", "rprn", "wdln", "clvg", "max_ndvi", "min_ndvi", "stdev_ndvi", "cluster", "NDVI_Diff", "max_ndvi", "min_ndvi", "stdev_ndvi", "mean_ndvi", "ndvi_change", "summer_ndvi", "winter_ndvi", "summer_greenup")]) %>% 
  pivot_longer(1:12, names_to="month_string", values_to="NDVI") %>%  
  mutate(month=as.numeric(substr(month_string,2,3)))
filtered_data_pheno_longer_pretty_factors <- filtered_data_pheno_longer
levels(filtered_data_pheno_longer_pretty_factors$guld) <- c("Deciduous Riparian Woodland","Evergreen Woodland","Other Natural Vegetation","Cultivated Vegetation")
levels(filtered_data_pheno_longer_pretty_factors$rprn) <- c("Deciduous Riparian Woodland","Other Vegetation Type")
levels(filtered_data_pheno_longer_pretty_factors$wdln) <- c("Woodland","Other Vegetation Type")
levels(filtered_data_pheno_longer_pretty_factors$clvg) <- c("Central Valley","Southern Sierran","Central Coast","South Coast","South Interior","Great Basin")

# Print a pretty phenology graph showing differences by ecoregion and vegetation type
veg_type_ecoregion_plot <- ggplot() + 
  geom_density_2d_filled(data=filtered_data_pheno_longer_pretty_factors, 
                         aes(x=month,y=NDVI), contour_var = "ndensity") + 
  geom_line(data=filtered_data_pheno_longer_pretty_factors  %>% 
              group_by(guld,clvg,month) %>% 
              summarize(mean_NDVI = median(NDVI)), 
            aes(x=month,y=mean_NDVI), 
            col="red") + 
  scale_y_continuous(limits=c(0,1)) + 
  facet_wrap(~clvg+guld, nrow=6) +
  labs(xlab = "Month",
       ylab = "Greenness (NDVI)") + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())
veg_type_ecoregion_plot
ggsave(here::here("guild_ecoregion_phenology_plot.png"), veg_type_ecoregion_plot, width=5, height=6)
# Print pretty plots showing variation in other variables by region
ggplot() + 
  geom_freqpoly(data = filtered_data_pretty_factors,
                 aes(x=summer_greenup, y=0.5*..density..)) + 
  geom_vline(xintercept=0) + 
  facet_wrap(~guld+clvg, nrow=4)
ggplot() + 
  geom_freqpoly(data = filtered_data_pretty_factors,
                 aes(x=summer_ndvi, y=0.5*..density..)) + 
  geom_vline(xintercept=0) + 
  facet_wrap(~guld+clvg, nrow=4)

# Print a pretty phenology graph showing clusters for upland vegetation
ggplot() + 
  geom_density_2d_filled(data=filtered_data_pheno_longer_pretty_factors %>% 
                           filter(guld == "Other Natural Vegetation"), 
                         aes(x=month, y=NDVI, contour_var = "ndensity")) + 
  geom_line(data=filtered_data_pheno_longer_pretty_factors  %>% 
              filter(guld == "Other Natural Vegetation") %>%
              group_by(cluster,month) %>% 
              summarize(mean_NDVI = median(NDVI)), 
            aes(x=month,y=mean_NDVI), 
            col="red") + 
  facet_wrap(~cluster) + 
  scale_y_continuous(limits=c(0,1)) + 
  labs(xlab = "Month",
       ylab = "Greenness (NDVI)")

# Print a pretty phenology graph showing phenology clusters for cultivated vegetation
ggplot() + 
  geom_density_2d_filled(data=filtered_data_pheno_longer_pretty_factors %>% 
                           filter(guld == "Cultivated Vegetation"), 
                         aes(x=month, y=NDVI, contour_var = "ndensity")) + 
  geom_line(data=filtered_data_pheno_longer_pretty_factors  %>% 
              filter(guld == "Cultivated Vegetation") %>%
              group_by(cluster,month) %>% 
              summarize(mean_NDVI = median(NDVI)), 
            aes(x=month,y=mean_NDVI), 
            col="red") + 
  facet_wrap(~cluster) + 
  scale_y_continuous(limits=c(0,1)) + 
  labs(xlab = "Month",
       ylab = "Greenness (NDVI)")




# Set up training datasets - three copies, one for riparian test, one for woodland, one for guild
#  Riparian data first
set.seed(2401)
num_training <- 200
num_validation <- 100
riparian_data <- filtered_data[filtered_data$guld ==1,]
riparian_training_indices <- sample(1:nrow(riparian_data), num_training*3)
remaining_indices <- (1:nrow(riparian_data))[-riparian_training_indices]
riparian_validation_indices <- sample(1:length(remaining_indices), num_validation*3)
riparian_training_points <- riparian_data[riparian_training_indices,]
riparian_validation_points <- riparian_data[riparian_validation_indices,]
#  Next, woodland data
woodland_data <- filtered_data[filtered_data$guld ==2,]
woodland_training_indices <- sample(1:nrow(woodland_data), num_training)
remaining_indices <- (1:nrow(woodland_data))[-woodland_training_indices]
woodland_validation_indices <- sample(1:length(remaining_indices), num_validation)
woodland_training_points <- woodland_data[woodland_training_indices,]
woodland_validation_points <- woodland_data[woodland_validation_indices,]
#  Next, other natural vegetation 
upland_data <- filtered_data[filtered_data$guld ==3,]
upland_training_indices <- sample(1:nrow(upland_data), num_training)
remaining_indices <- (1:nrow(upland_data))[-upland_training_indices]
upland_validation_indices <- sample(1:length(remaining_indices), num_validation)
upland_training_points <- upland_data[upland_training_indices,]
upland_validation_points <- upland_data[upland_validation_indices,]
#  Last, cultivated vegetation (crops, etc.)
cultivated_data <- filtered_data[filtered_data$guld ==4,]
cultivated_training_indices <- sample(1:nrow(cultivated_data), num_training)
remaining_indices <- (1:nrow(cultivated_data))[-cultivated_training_indices]
cultivated_validation_indices <- sample(1:length(remaining_indices), num_validation)
cultivated_training_points <- cultivated_data[cultivated_training_indices,]
cultivated_validation_points <- cultivated_data[cultivated_validation_indices,]
# assemble training/validation datasets
training <- rbind(riparian_training_points,
                  woodland_training_points,
                  upland_training_points,
                  cultivated_training_points)
validation <- rbind(riparian_validation_points,
                    woodland_validation_points,
                    upland_validation_points,
                    cultivated_validation_points)


# Random Forest Training - this one doesn't use any climate data, and predicts all the data types
#    Removed all winter spectra, plus some topographic information, because they weren't improving accuracy:
#    slope+irrig+blue_winte+green_wint+red_winter+NIR_winter+SWIR1_wint+SWIR2_wint
set.seed(100)
modFit_rf_guld <- train(data = training,
                   guld ~ X00_NDVI+X01_NDVI+X02_NDVI+X03_NDVI+X04_NDVI+X05_NDVI+X06_NDVI+X07_NDVI+X08_NDVI+X09_NDVI+X10_NDVI+X11_NDVI+summer_ndvi+winter_ndvi+stdev_ndvi+max_ndvi+dem+hod_1+hod_2+latitude+blue_summe+green_summ+red_summer+NIR_summer+SWIR1_summ+SWIR2_summ+pheno_change_angle,
                   method = "rf")
# Validation results
validation_results <- predict(modFit_rf_guld, validation)
confusionMatrix(validation_results, validation$guld)
print(varImp(modFit_rf_guld)$importance %>% arrange(-Overall))

# Random Forest Training - this one doesn't use any climate data, and predicts only binary for riparian / not riparian 
modFit_rf_rprn <- train(data = training,
                        rprn ~ X00_NDVI+X01_NDVI+X02_NDVI+X03_NDVI+X04_NDVI+X05_NDVI+X06_NDVI+X07_NDVI+X08_NDVI+X09_NDVI+X10_NDVI+X11_NDVI+summer_ndvi+winter_ndvi+stdev_ndvi+max_ndvi+dem+hod_1+hod_2+latitude+blue_summe+green_summ+red_summer+NIR_summer+SWIR1_summ+SWIR2_summ+pheno_change_angle,
                        method = "rf")
# Validation results
validation_results <- predict(modFit_rf_rprn, validation)
confusionMatrix(validation_results, validation$rprn)
print(varImp(modFit_rf_rprn)$importance %>% arrange(-Overall))

# Random Forest Training - this one doesn't use any climate data, and predicts only binary for woodland / not woodland 
modFit_rf_wdln <- train(data = training,
                        as.factor(wdln) ~ X00_NDVI+X01_NDVI+X02_NDVI+X03_NDVI+X04_NDVI+X05_NDVI+X06_NDVI+X07_NDVI+X08_NDVI+X09_NDVI+X10_NDVI+X11_NDVI+summer_ndvi+winter_ndvi+stdev_ndvi+max_ndvi+dem+hod_1+hod_2+latitude+blue_summe+green_summ+red_summer+NIR_summer+SWIR1_summ+SWIR2_summ+pheno_change_angle,
                        method = "rf")
# Validation results
validation_results <- predict(modFit_rf_wdln, validation)
confusionMatrix(validation_results, validation$wdln)
print(varImp(modFit_rf_wdln)$importance %>% arrange(-Overall))

# Random Forest Training - this one doesn't use ecoregion, but it does use the climate data, and it predicts guilds
modFit_rf_climate <- train(data = training,
                        guld ~ X00_NDVI+X01_NDVI+X02_NDVI+X03_NDVI+X04_NDVI+X05_NDVI+X06_NDVI+X07_NDVI+X08_NDVI+X09_NDVI+X10_NDVI+X11_NDVI+summer_ndvi+winter_ndvi+stdev_ndvi+max_ndvi+dem+hod_1+hod_2+latitude+tmax+tmin+precip+vpd+blue_summe+green_summ+red_summer+NIR_summer+SWIR1_summ+SWIR2_summ+pheno_change_angle,
                        method = "rf")
# Validation results
validation_results <- predict(modFit_rf_climate, validation)
confusionMatrix(validation_results, validation$guld)
print(varImp(modFit_rf_climate)$importance %>% arrange(-Overall))


# Random Forest Training - this one doesn't use climate data, but it does use the ecoregion, and it predicts guilds
modFit_rf_ecoregion <- train(data = training,
                           guld ~ X00_NDVI+X01_NDVI+X02_NDVI+X03_NDVI+X04_NDVI+X05_NDVI+X06_NDVI+X07_NDVI+X08_NDVI+X09_NDVI+X10_NDVI+X11_NDVI+summer_ndvi+winter_ndvi+stdev_ndvi+max_ndvi+dem+hod_1+hod_2+latitude+clvg+blue_summe+green_summ+red_summer+NIR_summer+SWIR1_summ+SWIR2_summ+pheno_change_angle,
                           method = "rf")
# Validation results
validation_results <- predict(modFit_rf_ecoregion, validation)
confusionMatrix(validation_results, validation$guld)
print(varImp(modFit_rf_ecoregion)$importance %>% arrange(-Overall))

# Write a shapefile out which includes the training/validation data and associated prediction
# Write a shapefile out which includes the validation data and associated prediction
training$prediction <- predict(modFit_rf_guld, training)
training$correct <- training$prediction == training$guld
st_write(cbind(training[,pheno_bands], 
               training[,which(!((names(training)) %in% pheno_bands))]),
         here::here("..","CalVeg","gee_pheno_points","training_data_predicted.shp"),
         append=FALSE)
write_csv(cbind(training[,pheno_bands], 
                training[,which(!((names(training)) %in% pheno_bands))]),
          here::here("..","CalVeg","gee_pheno_points","training_data_predicted.csv"))


validation$prediction <- predict(modFit_rf_guld, validation)
validation$correct <- validation$prediction == validation$guld
validation$riparian_overestimate <- (validation$prediction==1)*(!validation$correct)
validation$woodland_overestimate <- (validation$prediction==2)*(!validation$correct)
st_write(cbind(validation[,pheno_bands], 
                 validation[,which(!((names(validation)) %in% pheno_bands))]),
         here::here("..","CalVeg","gee_pheno_points","validation_data_predicted.shp"),
         append=FALSE)
write_csv(cbind(validation[,pheno_bands], 
                validation[,which(!((names(validation)) %in% pheno_bands))]),
          here::here("..","CalVeg","gee_pheno_points","validation_data_predicted.csv"))

