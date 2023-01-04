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
                 guess_max = min(8300, n_max= 17388),
                 na = " ")


# Check if the data is read in right with correct data types
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

plot(Africa_proj_sim)

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

# Merge the Africa map with the STH points data and allow all row to be kept
Africa_STHpercentage <-merge(Africa_proj_sim, STH_percent, by='country', all=T)

# Sort the country according to the STH percentage values
Africa_STH_D_order <- Africa_STHpercentage[order(-Africa_STHpercentage$percent),]
Africa_STH_D_order
# Ignore the warning message, the empty units are purposely added to allow the legend to show that some countries have missing data
Africa_STH_D_order <- Africa_STHpercentage[order(-Africa_STHpercentage$percent),]
# Map the STH prevalence in Africa based on the data collected
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

# Filter out only Southern Africa's health district boundaries
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

tm_shape(NGA_healthdistrict) +
  tm_borders(col='grey') +
  tm_shape(NGA_STH_pts) +
  tm_dots(col = 'blue')

# Spatial subset the points with Nigeria's borders
NGA_STH_sub <- NGA_STH_pts[NGA_healthdistrict, ]
# Inspect
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


# Inspect
tm_shape(NGA_STH_joined) + 
  tm_polygons(col='percentage', palette='Greens', border.col = NULL) + 
  tm_scale_bar(position = c("right","bottom")) +
  tm_shape(Nga_reg) + tm_borders("grey25",lwd = 0.8) +
  tm_shape(Nga_reg) +
  tm_text("adm1_name", size = "AREA") +
  tm_compass(position = c("left", "top"), size = 0.8) +
  tm_layout("STH percentage in Nigeria", title.size = 0.8, title.position = c('center','top'),  legend.title.size = 0.8)

# The points are not where STH is detected, but locations to test the prevalence of STH
ngasoil <- st_read("Nigeria_soils_data/Legacy_soils_and_National_surveys.shp")
st_crs(ngasoil)$proj4string

ngasoil <- ngasoil %>%
  st_set_crs(., 4326) %>%
  st_transform(., 3857)

ngasoilsub <- ngasoil[NGA_healthdistrict, ]

nga_soil_sth_joined <- st_join(NGA_STH_joined, ngasoilsub)

# Inspect points with map
tm_shape(NGA_healthdistrict) + tm_polygons(alpha = 0, border.col = "black") + 
  tm_shape(ngasoilsub) + tm_dots(col = 'blue') + 
  tm_scale_bar(position = c("right","bottom")) +
  tm_compass(position = c("left", "top")) 

# Map the percentage after joining to check if the join was correct
plot1 <- tm_shape(nga_soil_sth_joined) + 
  tm_polygons(col='percentage', palette='Greens', border.col = NULL) + 
  tm_scale_bar(position = c("right","bottom")) +
  tm_shape(Nga_reg) + tm_borders("grey25",lwd = 0.8) +
  tm_shape(Nga_reg) +
  tm_text("adm1_name", size = "AREA") +
  tm_compass(position = c("left", "top"), size = 0.8) +
  tm_layout("STH percentage in Nigeria", title.size = 1, title.position = c('center','top'),  legend.title.size = 0.8, legend.outside.size = 0.3)


# Map the pH after joining
plot2 <- tm_shape(nga_soil_sth_joined) + 
  tm_fill(col='pH', palette='Greens') +
  tm_shape(Nga_reg) + tm_borders("grey25",lwd = 0.8) +
  tm_shape(Nga_reg) + 
  tm_text("adm1_name", size = "AREA") +
  tm_scale_bar(position = c("right","bottom"), ) +
  tm_compass(position = c("left", "top"), size = 0.8) +
  tm_layout("Soil pH level in Nigeria", title.size = 1, title.position = c('center','top'), legend.title.size = 0.8, legend.outside.size = 0.3)
  
# Map the OC after joining
plot3 <- tm_shape(nga_soil_sth_joined) + 
  tm_fill(col='OC', palette='Greens') +
  tm_shape(Nga_reg) + tm_borders("grey25",lwd = 0.8) +
  tm_scale_bar(position = c("right","bottom")) +
  tm_shape(Nga_reg) +
  tm_text("adm1_name", size = "AREA") +
  tm_compass(position = c("left", "top"), size = 0.8) +
  tm_layout("Soil Organic Carbon level in Nigeria", title.size = 1, title.position = c('center','top'), legend.title.size = 0.8, legend.outside.size = 0.3)

# Map the P after joining
plot4 <- tm_shape(nga_soil_sth_joined) + 
  tm_fill(col='P', palette='Greens') +
  tm_shape(Nga_reg) + tm_borders("grey25",lwd = 0.8) +
  tm_scale_bar(position = c("right","bottom")) +
  tm_shape(Nga_reg) +
  tm_text("adm1_name", size = "AREA") +
  tm_compass(position = c("left", "top"), size = 0.8) +
  tm_layout("Soil Phosphorus level in Nigeria", title.size = 1, title.position = c('center', 'top'), legend.title.size = 0.8, legend.outside.size = 0.3)


tmap_arrange(plot1, plot2, plot3, plot4, nrow = 2)

# Stage 2 Fit non-spatial linear regression
# response variable: STH percentage
# predictors: Soil pH and soil organic carbon and phosphorus
# residuals should exhibit normal distribution(random) --> if not, spatial factor might need to be considered
# since all plots have different scale, it is better to vstandardise them with log() to make them comparable

# the na values in the dataframe is not able to run in lm(), so the dataframe has to be cleaned
nga_soil_sth_cleaned <-nga_soil_sth_joined %>%
  dplyr::select(admin1, admin2, sth_examined, sth_positive, percentage, pH, OC, P, geometry)


nga_soil_sth_cleaned <- nga_soil_sth_cleaned %>%
  dplyr::filter(percentage !=0)

nga_soil_sth_cleaned <- nga_soil_sth_cleaned %>%
  dplyr::filter(pH !=0)

nga_soil_sth_cleaned <- nga_soil_sth_cleaned %>%
  dplyr::filter(P !=0)

nga_soil_sth_cleaned <- nga_soil_sth_cleaned %>%
  dplyr::filter(OC !=0)

# Non-spatial linear regression
modelMLR <- lm(log10(percentage) ~ log10(pH) + log10(OC) + log10(P), data = nga_soil_sth_cleaned)
options(scipen = 7)
summary(modelMLR)

nga_soil_sth_cleaned$RESIDUALS <- modelMLR$residuals


# Reporting basic summary measures to have an idea of its distribution before plotting them on map
summary(nga_soil_sth_cleaned$RESIDUALS)

tm_shape(nga_soil_sth_cleaned) + tm_fill("RESIDUALS", style = "cont", midpoint = 0, palette = "-RdBu") +
  tm_shape(NGA_STH_joined) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)  
