# Load packages
library(sf)
library(tmap)
library(tidyverse)
library(raster)
library(spatstat)
library(dplyr)
library(janitor)
library(readxl)
library(rgdal)
library(ggplot2)
library(ggspatial)
library(tmaptools)
library(rmapshaper)

# Read in data
Africa <- st_read("Africa_SCH_STH_shapefile/Countries.shp")
Health_district<- st_read("Africa_SCH_STH_shapefile/Health_Districts.shp")
Regions <- st_read("Africa_SCH_STH_shapefile/Regional.shp")
STH <- read_xlsx("SoilTransmittedHelminths_dataset28052017.xlsx",
                 guess_max = min(8300, n_max= 17388))



# Check if the data is read in right with correct data types
Datatypelist <- STH%>%
  summarise_all(class)%>%
  pivot_longer(everything(),
               names_to = "All variables",
               values_to = "Variable Class")
Datatypelist


#Check the CRS for the Africa polygons
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

plot(Africa_proj_sim)

# Stage 1: Broad picture of the general STH percentage in Africa
#to obtain the count of STH postive in each country
# since there are some missing values that reads as '999999', they have to be omitted.
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

#obtain the percentage of positive STH diagnosis in each country
STH_percent <- STH_examined_count%>%
  mutate(percent= sth_positive/sth_examined)

Africa_proj_sim <- Africa_proj_sim%>%
  rename(country = adm0_name)

# Merge the Africa map with the STH points data and allow all row to be kept
Africa_STHpercentage <-merge(Africa_proj_sim, STH_percent, by='country', all=T)

#sort the country according to the STH percentage values
Africa_STH_D_order <- Africa_STHpercentage[order(-Africa_STHpercentage$percent),]
Africa_STH_D_order
# Ignore the warning message, the empty units are purposely added to allow the legend to show that some countries have missing data
Africa_STH_D_order <- Africa_STHpercentage[order(-Africa_STHpercentage$percent),]
#map the STH prevalence in Africa based on the data collected
tm_shape(Africa_proj_sim) + 
  tm_polygons() + 
  tm_shape(Africa_proj_sim) +
  tm_borders() +
  tm_shape(Africa_STH_D_order) +
  tm_fill(col= 'percent') +
  tm_style("white") +
  tm_layout( "STH Prevalence across Africa (%)", title.size = 0.7, title.position= c('center','bottom')) +
  tm_scale_bar(position = c("left", "bottom"), width = 0.15, size = 0.5) +
  tm_compass(position = c("left", "top"), size = 2) 


#This map only shows the overview of the Whole African continent using mean values, it does not show accurate results of each country
#Before moving on to focus on Nigeria, the following analysis will use the health district for performing geostatistical modelling
st_crs(Health_district)$proj4string
Health_district <- Health_district %>%
  st_set_crs(., 4326)

Health_district_proj <- Health_district %>%
  st_transform(., 3857)

summary(Health_district_proj)

# filter out only Southern Africa's health district boundaries
Health_district_proj <- Health_district_proj%>%
  janitor::clean_names()

NGA_healthdistrict <- Health_district_proj%>%
  filter(str_detect(admin0, "Nigeria"))

#inspect
tm_shape(NGA_healthdistrict) + tm_borders()

#select out the points of STH in South Africa
NGA_STH <- STH_cleaned%>%
  filter(str_detect(country, "Nigeria"))

NGA_STH_pts <- NGA_STH%>%
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = 4326)%>%
  st_transform(., 3857)

tm_shape(NGA_healthdistrict) +
  tm_borders(col='grey') +
  tm_shape(NGA_STH_pts) +
  tm_dots(col = 'blue')

#spatial subset the points with Nigeria's borders
NGA_STH_sub <- NGA_STH_pts[NGA_healthdistrict, ]
#inspect
tm_shape(NGA_healthdistrict) + tm_polygons(alpha = 0, border.col = "black") + 
  tm_shape(NGA_STH_sub) + tm_dots(col = 'blue') + 
  tm_scale_bar(position = c("right","bottom")) +
  tm_compass(position = c("left", "top"))

NGA_STH_joined <- st_join(NGA_healthdistrict, NGA_STH_sub, by = 'iu_id')

NGA_STH_joined <- NGA_STH_joined%>%
  mutate(percentage = sth_positive/sth_examined)

NGA_STH_sub <- NGA_STH_sub%>%
  mutate(percentage = sth_positive/sth_examined)%>%
  mutate(perc_to_100 = percentage*100)


#since the points are not where STH is detected, instead they are points where STH has been examined and diagnosed
#try kriging on NGA
library("gstat")
library("geoR")
library(sp)

 #inspect
tm_shape(NGA_healthdistrict) + tm_polygons(alpha = 0, border.col = "black") + 
  tm_shape(NGA_STH_sub) + tm_dots() + 
  tm_scale_bar(position = c("left","bottom")) +
  tm_compass(position = c("right", "bottom"))

tm_shape(NGA_STH_joined) + 
  tm_polygons(col='percentage', palette='Greens') +
  tm_scale_bar(position = c("right","bottom")) +
  tm_compass(position = c("left", "top")) +
  tm_layout("STH percentage in Nigeria", title.size = 0.8, title.position = c('center','top'))

#There are some wards without data, the STH percentage are unknown, therefore Kriging could be adopted to predict the STH percentage

NGA_STH_sub_sp <- as(NGA_STH_sub, "Spatial")

NGASTH_emp.variogram <- variogram(percentage~1, NGA_STH_sub_sp)
NGASTH_emp.variogram
plot(NGASTH_emp.variogram)
#nugget - 0.024
#range - 200005 
#sill - 0.033
#partial sill - 0.018 
# Fit best model
best_NGASTH_emp.variogram <- fit.variogram(NGASTH_emp.variogram, model = vgm(c("Exp", "Gau", "Sph")))
best_NGASTH_emp.variogram
plot(NGASTH_emp.variogram, best_NGASTH_emp.variogram, main  = "Gaussian model (Nug: 0.023445432, PSill:0.008277129, Range: 161882.2m)")

#build blank raster template
RasterTemplate <- raster(NGA_STH_sub_sp)
res(RasterTemplate) <- 5000
grid.interpolation <- as(RasterTemplate, 'SpatialGrid')
# interpolation 
modelKrigingGAU <- gstat(formula = percentage~1, locations = NGA_STH_sub_sp, model = best_NGASTH_emp.variogram)
# add results of the interpolation to the raster template --> predicted STH and variation of STH
Kriged_STH <- predict(modelKrigingGAU, grid.interpolation)

# save both prediction and variance to a brick raster
brickedKriged_STH_Results <- brick(Kriged_STH)
# slice the layers out
exp.prediction <- raster(brickedKriged_STH_Results, layer = 1)
exp.variance <- raster(brickedKriged_STH_Results, layer = 2)

writeRaster(exp.prediction, "Predicted STH levels in NGA.tif", format="GTiff", overwrite = TRUE)
writeRaster(exp.variance, "Variance STH levels in NGA.tif", format="GTiff", overwrite = TRUE)

NGA_shp_sp <- as(NGA_STH_joined, "Spatial")
exp.prediction_masked <- mask(exp.prediction, NGA_shp_sp)


frameExtent <- st_bbox(NGA_shp_sp)
frameExtent

tm_shape(exp.prediction_masked, bbox = frameExtent) + tm_raster(title = "Predicted STH ppb", style = "cont", palette = "Reds") +
  tm_shape(NGA_shp_sp) + tm_polygons(alpha = 0, border.col = "black") +
  tm_scale_bar(position = c("left","bottom")) 
  tm_shape(NGA_STH_joined) + tm_dots() +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5, legend.position = c("right", "bottom"))
