
library(sf)
library(tidyverse)

calveg_database_files <- list.files(here::here("..","CalVeg"), "S_USA.EVMid")

nvcs_data <- data.frame()

for(file in calveg_database_files)
{
  calveg <- st_read(here::here("..","CalVeg",file))
  calveg_natural <- as.data.frame(calveg) %>%
    select("NVCS_CLASS","NVCS_SUBCLASS", "NVCS_FORMATION", "NVCS_DIVISION", "NVCS_MACROGROUP", "ECOREGION_DIVISION", "SHAPE_Area") %>%
    drop_na(NVCS_MACROGROUP) %>%
    group_by(NVCS_MACROGROUP) %>%
    summarize(NVCS_CLASS=as.character(Mode(NVCS_CLASS)),
              NVCS_SUBCLASS=as.character(Mode(NVCS_SUBCLASS)),
              NVCS_FORMATION=as.character(Mode(NVCS_FORMATION)),
              NVCS_DIVISION=as.character(Mode(NVCS_DIVISION)),
              ECOREGION_DIVISION=as.character(Mode(ECOREGION_DIVISION)),
              area_total = sum(SHAPE_Area)*1e10/(30^2),
              area_median = median(SHAPE_Area)*1e10/(30^2))
  
  if(nrow(nvcs_data) == 0)
  {
    nvcs_data <- calveg_natural
  } else {
    nvcs_data <- rbind(nvcs_data, calveg_natural)
  } 
}

write_csv(nvcs_data, here::here("calveg_all_formations.csv"))

nvcs_unique <- nvcs_data %>% 
  select(-ECOREGION_DIVISION) %>% 
  select(-area_median) %>%
  group_by(NVCS_MACROGROUP) %>% 
  summarize(NVCS_CLASS=as.character(Mode(NVCS_CLASS)),
            NVCS_SUBCLASS=as.character(Mode(NVCS_CLASS)),
            NVCS_FORMATION=as.character(Mode(NVCS_FORMATION)),
            NVCS_DIVISION=as.character(Mode(NVCS_DIVISION)),
            area_total = sum(area_total)*1e10/(30^2))


write_csv(nvcs_unique, here::here("calveg_all_formations_unique.csv"))





# deal with 'all data' example
all_data_vafb <- raster("D:/SERDP/GEE_Classifier/Phenology/vandenberg/vandenberg_all_data_2020_phenoseries.tif")