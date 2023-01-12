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
library(spdep)
library(rJava)
library(dismo)
library(sp)
library(here)

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

NGA_border <- Africa_proj %>%
  filter(str_detect(adm0_name, "Nigeria")) %>%
  dplyr::select(adm0_name, geometry)
plot(NGA_border)

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
  tm_shape(Africa_proj) + tm_text("adm0_name", size = "AREA", along.lines = T) +
  tm_style("white") +
  tm_layout( "STH Prevalence across Africa (%)", title.size = 1, title.position= c(0.3,'bottom')) +
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

# Inspect
tm_shape(NGA_STH_joined) + 
  tm_polygons(col='percentage', palette= "YlGn", border.col = NULL) + 
  tm_scale_bar(position = c("right","bottom")) +
  tm_shape(Nga_reg) + tm_borders("grey25",lwd = 0.8) +
  tm_shape(Nga_reg) +
  tm_text("adm1_name", size = "AREA") +
  tm_compass(position = c("left", "top"), size = 0.8) +
  tm_layout("STH percentage in Nigeria", title.size = 0.8, title.position = c('center','top'),  legend.title.size = 0.8)

# Making maps with both Africa and Nigeria
library(ggplot2)
AFA <- Africa_STH_D_order %>%
  ggplot() +
  geom_sf(
    aes(fill = percent), 
    lwd = 0,
    colour = "white",
  ) +
  scale_fill_gradientn(
    colors = c("#9DBF9E", "#FCB97D", "#A84268"),
    na.value = "grey80",
    oob = scales::squish,
    labels = scales::percent,
    name = "STH %" 
  ) 

#NGA
mainmap <- NGA_STH_joined %>%
  ggplot() +
  geom_sf(data = NGA_border) +
  geom_sf(
    aes(fill = percentage), 
    lwd = 0,
    colour = "white"
  ) +
  scale_fill_gradientn(
    colors = c("#9DBF9E", "#FCB97D", "#A84268"),
    na.value = "grey80",
    oob = scales::squish,
    labels = scales::percent,
    name = "STH%") +
  theme_void() +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(0, .9)
  ) 

# Get the bounding box of the map
extent(NGA_STH_joined)

AFA +
  coord_sf(
    xlim = c(297048.4, 1634178),
    ylim = c(475852.3, 1561830),
    expand = FALSE
  )


library(cowplot)
ggdraw(mainmap) +
  draw_plot(
    {
      AFA + 
        coord_sf(
          xlim = c(297048.4, 1634178),
          ylim = c(475852.3, 1561830),
          expand = FALSE) +
        theme(legend.position = "none")
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.5, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.46, 
    height = 0.46)

AFA_rec <- 
  AFA +
  geom_rect(
    xmin = 297048.4,
    ymin = 475852.3,
    xmax = 1634178,
    ymax = 1561830,
    fill = NA, 
    colour = "black",
    size = 0.6
  )

Map <- mainmap %>% 
  ggdraw() +
  draw_plot(
    {
      AFA_rec + 
        coord_sf() +
        theme(legend.position = "none")
    },
    x = 0.64, 
    y = 0,
    width = 0.35, 
    height = 0.35
  )

Map

# Make binary column for the STH presence and absence point data for suitability analysis in later terms
NGA_STH_sub <- NGA_STH_sub%>%
  mutate(percentage = sth_positive/sth_examined)%>%
  mutate(perc_to_100 = percentage*100)

NGA_STH_sub$binary <- NA 
NGA_STH_sub$binary[NGA_STH_sub$percentage == 0] <- 0
NGA_STH_sub$binary[NGA_STH_sub$percentage != 0] <- 1 
NGA_STH_sub <- NGA_STH_sub[!is.na(NGA_STH_joined$binary),]

summary(NGA_STH_sub)
# Check if the binary column is cleaned
table(NGA_STH_sub$binary)

# Read in the raster layers for Soil profile
SOC <- raster("Worldsoilraster/soc_0-5cm_mean_5000_3857.tif")
pH <- raster("Worldsoilraster/phh20_0-5cm_mean_5000_3857.tif")
WV <- raster("Worldsoilraster/wv0010_0-5cm_mean_5000_3857.tif")
N <- raster("Worldsoilraster/nitrogen_0-5cm_mean_5000_3857.tif")


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

Ncrop <- crop(N, extent(Nga_reg))
N_NGA <- mask(Ncrop, Nga_reg)
plot(N_NGA)
N_NGA

# Make sure all the shapefile and raster data are in the projection of EPSG:3857
tm_shape(SOC_NGA) + tm_raster(style = "cont", title = "Mean Soil Organic Carbon level", palette= "-Spectral") +
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)


tm_shape(pH_NGA) + tm_raster(style = "cont", title = "Mean soil pH level", palette= "-Spectral") +
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

tm_shape(WV_NGA) + tm_raster(style = "cont", title = "Mean Soil Water retention - 10 kPa ", palette= "-Spectral") +
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

tm_shape(N_NGA) + tm_raster(style = "cont", title = "Mean Soil Nitrogen level", palette= "-Spectral") +
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)


# Visualising the Soil profile that could have correlation with STH occurrence
m1 <- tm_shape(SOC_NGA) + tm_raster(style = "cont", title = "	g/kg", palette= "Oranges") +
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.position = c("right", "bottom"), title.position = c("left", "bottom"), title = "SOC")

m2 <- tm_shape(pH_NGA) + tm_raster(style = "cont", title = "pHx10", palette= "Blues") +
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.position = c("right", "bottom"), title.position = c("left", "bottom"), title = "B")

m3 <- tm_shape(WV_NGA) + tm_raster(style = "cont", title = "g/100g", palette= "-Spectral") +
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.position = c("right", "bottom"), title.position = c("left", "bottom"), title = "C")

m4 <- tm_shape(N_NGA) + tm_raster(style = "cont", title = "	cg/kg", palette= "Greens") +
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.position = c("right", "bottom"), title.position = c("left", "bottom"), title = "D")

tmap_arrange(m1, m2, m3, m4, nrow = 2)

# create a multi-band raster stack
Soil_stk <- stack(SOC_NGA, pH_NGA, WV_NGA, N_NGA)
nlayers(Soil_stk)
names(Soil_stk) <- c("Soil_Organic_Carbon", "pH", "Water_retention", "Nitrogen" )

# Prepare background data for pseudo-background points as absence (0) --> a set of control points
set.seed(20000106)
NGA_border_sp <- as(NGA_border, Class = "Spatial")
Background_points <- spsample(NGA_border_sp, n=2*nrow(NGA_STH_sub), "random")


# Checking the warning message
rgdal::rgdal_extSoftVersion()
rgdal::new_proj_and_gdal()
sf::sf_extSoftVersion()

# Raster Extraction
STH_points_soil <- raster::extract(Soil_stk, NGA_STH_sub)
Background_points_soil <- raster::extract(Soil_stk, Background_points)


STH_points_soil <- data.frame(STH_points_soil, binary=1)
Background_points_soil <- data.frame(Background_points_soil,binary=0)

head(STH_points_soil, n=5)
head(Background_points_soil, n=5)

# Cross validation before constructing the STH risk model: k-fold cross validation
set.seed(20000106)
# using k-fold function to split data into 4 equal parts
select <- kfold(STH_points_soil, 4)
# 25% of the fire data use for testing the model
STH_points_soil_test <- STH_points_soil[select==1,]
# 75% of the fire data use for training the model
STH_points_soil_train <- STH_points_soil[select!=1,]

# Repeat for background points 
set.seed(20000106)
select <- kfold(Background_points_soil, 4)
Background_points_soil_test <- Background_points_soil[select==1,]
Background_points_soil_train <- Background_points_soil[select!=1,]

# Row bind bothe the training and testing datasets together
training_data <- rbind(STH_points_soil_train, Background_points_soil_train)
testing_data <- rbind(STH_points_soil_test, Background_points_soil_test)

# Fit the niche model using the Maximum Entropy (MAXENT) algorithm,
model_training <- maxent(x=training_data[,c(1:4)], p=training_data[,5], args=c("responsecurves"))
plot(model_training, pch=19, xlab = "Percentage [%]", cex=1.2)
response(model_training)
cross_validation <- evaluate(p=testing_data[testing_data$binary==1,], a=testing_data[testing_data$binary==0,], model = model_training)
cross_validation 
plot(cross_validation, 'ROC', cex=1.2)
prob_STHtransmission <- predict(model_training, Soil_stk)

tm_shape(prob_STHtransmission) +
  tm_raster(title = "Predicted probability", palette = '-RdYlBu', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(main.title = "Predicted Probability of STH transmission [%]", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.85, 0), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(-0.003, -0.01), text.size = 0.6, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c(0.93, 0.9), text.size = 0.9)

threshold_value <- threshold(cross_validation, "spec_sens")
# report value
threshold_value

create_classes_vector <- c(0, threshold_value, 0, threshold_value, 1, 1)
create_clasess_matrix <- matrix(create_classes_vector, ncol = 3, byrow = TRUE)
create_clasess_matrix
suitability_STHtransmission <- reclassify(prob_STHtransmission, create_clasess_matrix)
# Map
tm_shape(suitability_STHtransmission) + tm_raster(style = "cat", title = "Threshold", palette= c("lightgrey", "red"), labels = c("Safe", "Trigger Points")) +
  tm_shape(Nga_reg) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)


# 4 fold analysis
# split plot panel into 4 segments for 4 AUC plots
par(mfrow=c(2,2))
# create a list() object to dump results inside `eMAX`
eMAX<-list()

# use STH_points_soil
# use Background_points_soil
folds <- 4

kfold_pres <- kfold(STH_points_soil, folds)
kfold_back <- kfold(Background_points_soil, folds)

set.seed(20000106)
# adapting loop code from https://rpubs.com/mlibxmda/GEOG70922_Week5
# takes a long time to run 4-fold
for (i in 1:folds) {
  train <- STH_points_soil[kfold_pres!= i,]
  test <- STH_points_soil[kfold_pres == i,]
  backTrain<-Background_points_soil[kfold_back!=i,]
  backTest<-Background_points_soil[kfold_back==i,]
  dataTrain<-rbind(train,backTrain)
  dataTest<-rbind(test,backTest)
  maxnet_eval <- maxent(x=dataTrain[,c(1:4)], p=dataTrain[,5], args=c("responsecurves"))
  eMAX[[i]] <- evaluate(p=dataTest[dataTest$binary==1,],a=dataTest[dataTest$binary==0,], maxnet_eval)
  plot(eMAX[[i]],'ROC')
}

aucMAX <- sapply( eMAX, function(x){slot(x, 'auc')} )
# report 4 of the AUC
aucMAX
# find the mean of AUC (and it must be > 0.50)
mean(aucMAX)

#Get maxTPR+TNR for the maxnet model
Opt_MAX<-sapply( eMAX, function(x){ x@t[which.max(x@TPR + x@TNR)] } )
Opt_MAX

Mean_OptMAX<-mean(Opt_MAX)
Mean_OptMAX
# use Mean_OptMAX as threshold for mapping suitability

#final results is AUC: 0.6290031 ; threshold: 0.6490391