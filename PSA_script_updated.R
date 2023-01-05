library(sf)
library(tmap)
library(tidyverse)
library(raster)
library(spatstat)
library(dplyr)
library(janitor)
library(readxl)
library(tidyr)
library(rgeos)
library(rgdal)

# Load in data
Africa <- st_read("Africa_SCH_STH_shapefile/Countries.shp")
Health_district<- st_read("Africa_SCH_STH_shapefile/Health_Districts.shp")
Regions <- st_read("Africa_SCH_STH_shapefile/Regional.shp")
STH <- read_xlsx("SoilTransmittedHelminths_dataset28052017.xlsx",
                 guess_max = min(8300, n_max= 17388),
                 na = " ")

Datatypelist <- STH%>%
  summarise_all(class)%>%
  pivot_longer(everything(),
               names_to = "All variables",
               values_to = "Variable Class")
Datatypelist

# Check the CRS for the Africa polygons
st_geometry(Africa)
st_crs(Africa)$proj4string

Africa <- Africa %>%
  st_set_crs(., 4326)

Africa_proj <- Africa %>%
  st_transform(., 3857)

st_crs(Africa_proj)$proj4string

Africa_proj <- Africa_proj %>%
  janitor::clean_names()

Africa_proj_sim <- Africa_proj%>%
  st_simplify(., dTolerance = 75) %>%
  dplyr::select(adm0_name, geometry)

# Regional borders
Regions <- Regions %>%
  st_set_crs(., 4326)%>%
  st_transform(., 3857)

Nga_reg <- Regions %>%
  janitor::clean_names()%>%
  filter(str_detect(adm0_name, "Nigeria"))

# Stage 1: Broad picture of the general STH percentage in Africa
# To obtain the count of STH postive in each country
# Since there are some missing values that reads as '999999', they have to be omitted.
STH_cleaned <- STH%>%
  filter(sth_positive < '99999')

summary(STH_cleaned)

STH_count <- STH_cleaned%>%
  group_by(country)%>%
  summarise(sth_positive = mean(sth_positive))

STH_Examined <- STH_cleaned %>%
  group_by(country)%>%
  summarise(sth_examined = mean(sth_examined))

STH_iso3 <- STH_cleaned %>%
  dplyr::select(country, iso3)

STH_iso3 <- distinct(STH_iso3)

STH_examined_count <- merge(STH_count, STH_Examined, by='country')
STH_examined_count <- merge(STH_examined_count, STH_iso3, by='country')

# Obtain the percentage of positive STH diagnosis in each country
STH_percent <- STH_examined_count%>%
  mutate(percent= sth_positive/sth_examined)

Africa_proj_sim <- Africa_proj_sim%>%
  rename(country = adm0_name)

Africa_STHpercentage <-merge(Africa_proj_sim, STH_percent, by='country', all=T)

# Sort the country according to the STH percentage values
Africa_STH_D_order <- Africa_STHpercentage[order(-Africa_STHpercentage$percent),]
Africa_STH_D_order
# Ignore the warning message, the empty units are purposely added to allow the legend to show that some countries have missing data
tm_shape(Africa_proj_sim) + 
  tm_polygons() + 
  tm_shape(Africa_proj_sim) +
  tm_borders() +
  tm_shape(Africa_STH_D_order) +
  tm_fill(col= 'percent') +
  tm_shape(Africa_proj) + tm_text("adm0_name", size = 0.5) +
  tm_style("white") +
  tm_layout( "STH Prevalence across Africa (%)", title.size = 1, title.position= c('center','bottom')) +
  tm_scale_bar(position = c("right", "bottom"), width = 0.15, size = 0.5) +
  tm_compass(position = c("left", "top"), size = 1) 

# This map only shows the overview of the Whole African continent using mean values, it does not show accurate results of each country
# Before moving on to focus on Nigeria, the following analysis will use the health district for performing geostatistical modelling
st_crs(Health_district)$proj4string
Health_district <- Health_district %>%
  st_set_crs(., 4326)

Health_district_proj <- Health_district %>%
  st_transform(., 3857)

summary(Health_district_proj)

# Filter out only Nigeria's health district boundaries
Health_district_proj <- Health_district_proj%>%
  janitor::clean_names()

NGA_healthdistrict <- Health_district_proj%>%
  filter(str_detect(admin0, "Nigeria"))

# Inspect
tm_shape(NGA_healthdistrict) + tm_borders()

# Select out the points of STH in South Africa
NGA_STH <- STH_cleaned%>%
  filter(str_detect(country, "Nigeria"))

NGA_STH_pts <- NGA_STH%>%
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = 4326)%>%
  st_transform(., 3857)

# Spatial subset the points with Nigeria's borders
NGA_STH_sub <- NGA_STH_pts[NGA_healthdistrict, ]
# Inspect
tm_shape(NGA_healthdistrict) + tm_polygons(alpha = 0, border.col = "black") + 
  tm_shape(NGA_STH_sub) + tm_dots(col = 'blue') + 
  tm_scale_bar(position = c("right","bottom")) +
  tm_compass(position = c("left", "top"))

# Join the STH with the map and visualise it
NGA_STH_joined <- st_join(NGA_healthdistrict, NGA_STH_sub, by = 'iu_id')

NGA_STH_joined <- NGA_STH_joined%>%
  mutate(percentage = sth_positive/sth_examined)

NGA_STH_joined <- NGA_STH_joined%>%
  mutate(percentage = sth_positive/sth_examined)%>%
  mutate(perc_to_100 = percentage*100)

# Inspect
tm_shape(NGA_STH_joined) + 
  tm_polygons(col='percentage', palette='Greens', border.col = NULL) + 
  tm_scale_bar(position = c("right","bottom")) +
  tm_shape(Nga_reg) + tm_borders("grey25",lwd = 0.8) +
  tm_shape(Nga_reg) +
  tm_text("adm1_name", size = "AREA") +
  tm_compass(position = c("left", "top"), size = 0.8) +
  tm_layout("STH percentage in Nigeria", title.size = 0.8, title.position = c('center','top'),  legend.title.size = 0.8)

# Make binary column for the STH percentage for suitability analysis in later terms
NGA_STH_joined$binary <- NA 
NGA_STH_joined$binary[NGA_STH_joined$percentage == 0] <- 0
NGA_STH_joined$binary[NGA_STH_joined$percentage != 0] <- 1 
NGA_STH_joined <- NGA_STH_joined[!is.na(NGA_STH_joined$binary),]

# Check if the binary column is cleaned
table(NGA_STH_joined$binary)

# Read in the raster layers for Soil profile
SOC <- raster("Worldsoilraster/soc_0-5cm_mean_5000_3857.tif")
pH <- raster("Worldsoilraster/phh20_0-5cm_mean_5000_3857.tif")
WV <- raster("Worldsoilraster/wv0010_0-5cm_mean_5000_3857.tif")


# Crop the raster to Nigeria
SOCcrop <- crop(SOC, extent(Nga_reg))
SOC_NGA <- mask(SOCcrop, Nga_reg)
plot(SOC_NGA)
summary(SOC_NGA)
SOC_NGA

pHcrop <- crop(pH, extent(Nga_reg))
pH_NGA <- mask(pHcrop, Nga_reg)
plot(pH_NGA)
pH_NGA

WVcrop <- crop(WV, extent(Nga_reg))
WV_NGA <- mask(WVcrop, Nga_reg)
plot(WV_NGA)
WV_NGA

# Make sure all the shapefile and raster data are in the projection of EPSG:3857
tm_shape(SOC_NGA) + tm_raster(style = "cont", title = "Mean Soil Organic Carbon", palette= "-Spectral") +
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

tm_shape(pH_NGA) + tm_raster(style = "cont", title = "Mean Soil Organic Carbon", palette= "-Spectral") +
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

tm_shape(WV_NGA) + tm_raster(style = "cont", title = "Mean Soil Organic Carbon", palette= "-Spectral") +
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# Create Raster Stack
Soil_stk <- stack(SOC_NGA, pH_NGA, WV_NGA)
nlayers(Soil_stk)

tm_shape(Nga_reg) + tm_polygons() + tm_shape(NGA_STH_joined) + tm_dots(col = "red")

