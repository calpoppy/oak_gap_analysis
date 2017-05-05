
## load packages; set working directory
library(raster); library(rgdal); library(sp); library(FedData); library(rgeos); library(rasterVis)
setwd("./SDM")

## set up spatial templates
  # create Albers Equal Area CRS; landcover layer is in this projection
aea <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # create a template raster with the parameters that will be common for all rasters that need to be stacked
example <- raster(nrow=621, ncol=1405, xmn=-2493045, xmx=2342655, ymn=91397.96, ymx=3310005, crs=aea)

## load and standardize predictor raster layers

  # 1. NLCD: land cover 2011
landcover <- raster('./NLCD/nlcd_2011_landcover_2011_edition_2014_10_10.img')
landcover_proj <- projectRaster(landcover, example)
      #### I'm not sure if this landcover file has been properly imported and projected... it does not look right in the 
      #### final raster stack...

  # 2. SSURGO: soils 
#us_soils <- get_ssurgo([[[insert arguments]]]])
  ##### ^ I have not gotten this to work. I dont think I have enough space on my computer for it all.
  ##### I did make a list of all variables we need from this database, according to the predictors used by Iverson in DISTRIB.
  ##### My working GoogleSheet with this list of predictors is here: 
          ## https://docs.google.com/a/mortonarb.org/spreadsheets/d/1UEm9jtuHNgZ-UvN7kt7Is2ou9gL2-W-Cj6STq8_9mIs/edit?usp=sharing

  # 3. PRISM: climate monthly norms
    # elevation
dem <- raster("PRISM/dem/PRISM_us_dem_4km_bil.bil")
    # precipitation
ppt_05 <- raster("PRISM/ppt_05/PRISM_ppt_30yr_normal_4kmM2_05_bil.bil")
ppt_06 <- raster("PRISM/ppt_06/PRISM_ppt_30yr_normal_4kmM2_06_bil.bil")
ppt_07 <- raster("PRISM/ppt_07/PRISM_ppt_30yr_normal_4kmM2_07_bil.bil")
ppt_08 <- raster("PRISM/ppt_08/PRISM_ppt_30yr_normal_4kmM2_08_bil.bil")
ppt_09 <- raster("PRISM/ppt_09/PRISM_ppt_30yr_normal_4kmM2_09_bil.bil")
ppt_annual <- raster("PRISM/ppt_annual/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
    # temperature
tmean_01 <- raster("PRISM/tmean_01/PRISM_tmean_30yr_normal_4kmM2_01_bil.bil")
tmean_05 <- raster("PRISM/tmean_05/PRISM_tmean_30yr_normal_4kmM2_05_bil.bil")
tmean_06 <- raster("PRISM/tmean_06/PRISM_tmean_30yr_normal_4kmM2_06_bil.bil")
tmean_07 <- raster("PRISM/tmean_07/PRISM_tmean_30yr_normal_4kmM2_07_bil.bil")
tmean_08 <- raster("PRISM/tmean_08/PRISM_tmean_30yr_normal_4kmM2_08_bil.bil")
tmean_09 <- raster("PRISM/tmean_09/PRISM_tmean_30yr_normal_4kmM2_09_bil.bil")
tmean_annual <- raster("PRISM/tmean_annual/PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil")
    # vapor pressure deficit
vpdmax <- raster("PRISM/vpdmax/PRISM_vpdmax_30yr_normal_4kmM2_annual_bil.bil")
vpdmin <- raster("PRISM/vpdmin/PRISM_vpdmin_30yr_normal_4kmM2_annual_bil.bil")

## create RasterStack of all layers
    # create list of PRISM layers
predictors_list <- c(dem, ppt_05, ppt_06, ppt_07, ppt_08, ppt_09, ppt_annual, tmean_01, tmean_05, tmean_06,
      tmean_07, tmean_08, tmean_09, tmean_annual, vpdmin, vpdmax)
    # stack all in list
predictors_stack <- stack(predictors_list)
    # project the new rasterstack
predictors_stack_proj <- projectRaster(predictors_stack, example)
    # stack landcover and climate layers; write file
landcover_clim_stack <- stack(c(predictors_stack_proj, landcover_proj)) 
writeRaster(landcover_clim_stack, "./landcover_clim_stack", format = "GTiff", overwrite = TRUE) # landcover data doesn't look right

## load occurrence point files, convert to coordinates, and project to aea
Q_pts <- read.csv("./Q_concern_pts_all.csv", as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE)
coordinates(Q_pts) <- c("LONG","LAT")
Q_gps <- SpatialPoints(coordinates(Q_pts))
proj4string(Q_gps) <- CRS("+init=epsg:4326") # WGS84
Q_gps <- spTransform(Q_gps, aea)

## extract predictors data for each point
Q_extracted <- data.frame(extract(landcover_clim_stack, coordinates(Q_gps)))
plot(contl_us_aea)
points(Q_gps$"LONG", Q_gps$"LAT")
Q_pts_envir <- cbind(Q_pts, Q_extracted) ### ?? dont think this is working ? how do I join the layer extractions with the original pt. data?

write.csv(Q_extracted, "./Q_pts_extracted.csv")
