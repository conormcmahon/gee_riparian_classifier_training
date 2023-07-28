
library(sf)
library(tidyverse)

training_data <- rbind(st_read(here::here("..","CalVeg","gee_pheno_points","calveg_training_pheno_90s.shp")),
                       st_read(here::here("..","CalVeg","gee_pheno_points","calveg_training_pheno_00s.shp")),
                       st_read(here::here("..","CalVeg","gee_pheno_points","calveg_training_pheno_10s.shp")))

guild_text <- c("Riparian Winter-deciduous", "Non-riparian Woodland", "Non-woodland Natural", "Crops")
names(guild_text) <- 1:4
training_data$guild_text <- guild_text[as.numeric(as.character(training_data$guld))]
riparian_text <- c("Non- or Facultative-Riparian", "Riparian")
names(riparian_text) <- 1:2
training_data$riparian_text <- riparian_text[as.numeric(as.character(training_data$rprn))+1]
woodland_text <- c("Non-woodland", "Woodland")
names(woodland_text) <- 1:2
training_data$woodland_text <- woodland_text[as.numeric(as.character(training_data$wdln))+1]
zone_text <- as.factor(c("North Coast", "North Interior", "North Sierran", "South Sierran", "Central Valley",
               "Central Coast", "South Coast", "South Interior", "Great Basin"))
names(zone_text) <- 1:9
training_data$zone_text <- zone_text[as.numeric(as.character(training_data$clvg))]

training_filt <- training_data %>% filter(NDVI_Diff > 0.2,
                                          hod_1 < 50, 
                                          X06_NDVI > 0.5)

training_longer <- training_filt %>%
  pivot_longer(c("X00_NDVI","X01_NDVI","X02_NDVI","X03_NDVI","X04_NDVI","X05_NDVI","X06_NDVI","X07_NDVI","X08_NDVI","X09_NDVI","X10_NDVI","X11_NDVI"), names_to="month_char",values_to="ndvi") %>%
  mutate(month = as.numeric(substr(month_char,2,3)))


training_summary <- training_longer %>%
  group_by(guild_text, month, zone_text) %>%
  summarize(ndvi_mean = mean(ndvi, na.rm=TRUE),
            ndvi_q_05 = quantile(ndvi, probs=0.05, na.rm=TRUE),
            ndvi_q_25 = quantile(ndvi, probs=0.25, na.rm=TRUE),
            ndvi_q_50 = quantile(ndvi, probs=0.50, na.rm=TRUE),
            ndvi_q_75 = quantile(ndvi, probs=0.75, na.rm=TRUE),
            ndvi_q_95 = quantile(ndvi, probs=0.95, na.rm=TRUE))

# Visualize phenology by region and vegetation type 
# Visualize phenology by region and vegetation type 
ggplot(training_summary) + 
  geom_line(aes(x=month,y=ndvi_mean, col="black")) + 
  geom_line(aes(x=month,y=ndvi_q_25, col="blue")) +  
  geom_line(aes(x=month,y=ndvi_q_75, col="blue")) + 
  facet_wrap(~guild_text + zone_text, nrow=4) + 
  ggtitle("Phenology by Land Cover Type and Region") + 
  xlab("Month") + 
  ylab("NDVI")

ggplot(training_longer) + geom_bin_2d(aes(x=as.numeric(month), y=ndvi), bins=12) + facet_wrap(~guild_text + clvg, nrow=4) + scale_x_continuous(breaks=(0:11))
ggplot(training_longer) + geom_density_2d(aes(x=as.numeric(month), y=ndvi)) + facet_wrap(~guld + clvg, nrow=4) + scale_x_continuous(breaks=(0:11))

# Visualize other parameters by region and vegetation type
ggplot(training_data) + 
  geom_histogram(aes(x=dem)) + 
  facet_wrap(~guld + clvg, nrow=4)


# Visualize other parameters by region and vegetation type
ggplot(training_data) + 
  geom_histogram(aes(x=NDVI_Diff)) + 
  facet_wrap(~guild_text + zone_text, nrow=4)


names(training_data) <- base::abbreviate(names(training_data))
st_geometry(training_data) <- "gmtr"
st_write(training_data, 
         here::here("..","CalVeg","gee_points_pheno","riparian_training.shp"),
         layer="riparian_training",
         driver="ESRI Shapefile")