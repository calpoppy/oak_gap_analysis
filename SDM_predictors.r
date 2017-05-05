
## load packages; set working directory
library(raster); library(rgdal); library(sp); library(FedData); library(rgeos); library(rasterVis)
setwd("./SDM")

aea <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
example <- raster(nrow=621, ncol=1405, xmn=-2493045, xmx=2342655, ymn=91397.96, ymx=3310005, crs=aea)
## load US map shapefile to use as clipping layer
us_map <- readOGR('./us_map/cb_2015_us_nation_20m.shp')
#proj4string(us_map) # +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0
#plot(us_map)
  # clip US map to only include continental US
#box_coords <- matrix(c(-130, -65, -65, -130, -130, 50, 50, 20, 20, 50), ncol=2)
#contl_box <- SpatialPolygons(list(Polygons(list(Polygon(box_coords)), 1)), proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
#proj4string(contl_box)
#contl_us_nad83 <- gIntersection(us_map, contl_box, byid = TRUE, drop_lower_td = TRUE)
#plot(contl_us_nad83)
  # transform layer to common CRS
contl_us_aea <- spTransform(us_map, aea)

#landcover_4km <- crop(landcover_4km, extent(contl_us_aea))


## load, clip, and project (aea, 4 km res) raster layers
#extet <- extent(-2493045, 2342655, 91397.96, 3310005) # least common extent
  # 1.land cover: NLCD, 30 m resolution
landcover <- raster('./NLCD/nlcd_2011_landcover_2011_edition_2014_10_10.img')
#res(landcover) <- c(0.04166667, 0.04166667)
landcover_proj <- projectRaster(landcover, example)
#landcover <- resample(landcover, example, method='ngb', filename='./NLCD/landcover_sample')
#proj4string(landcover) # aea
#levelplot(landcover)
#landcover_4km <- projectRaster(crs = aea, landcover, method = "ngb")
  # crop to correct extent
#landcover <- setExtent(landcover, ext, keepres = F)

#levelplot(landcover_4km)
#writeRaster(landcover, "./NLCD/landcover_us_4km", format = "GTiff", overwrite = TRUE)
# landcover_4km <- mask(landcover_4km, contl_us_aea) ## ?? what does this do exactly?

  # 2.soils: SSURGO
#us_soils <- get_ssurgo(template = contl_us_nad83, label='us_soils')

  #load climate data: PRISM, 4 km resolution
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
  #create list of all PRISM layers
predictors_list <- c(dem, ppt_05, ppt_06, ppt_07, ppt_08, ppt_09, ppt_annual, tmean_01, tmean_05, tmean_06,
      tmean_07, tmean_08, tmean_09, tmean_annual, vpdmin, vpdmax)
  #stack all in list
predictors_stack <- stack(predictors_list)
  #project the new rasterstack
predictors_stack_proj <- projectRaster(predictors_stack, example)
  #stack landcover and climate layers; write file
landcover_clim_stack <- stack(c(predictors_stack_proj, landcover_proj))
writeRaster(landcover_clim_stack, "./landcover_clim_stack", format = "GTiff", overwrite = TRUE)

Q_pts <- read.csv("./Q_concern_pts_all.csv", as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE)
coordinates(Q_pts) <- c("LONG","LAT")
Q_gps <- SpatialPoints(coordinates(Q_pts))
proj4string(Q_gps) <- CRS("+init=epsg:4326")
Q_gps <- spTransform(Q_gps, aea)
Q_extracted <- data.frame(extract(landcover_clim_stack, coordinates(Q_gps)))
plot(contl_us_aea)
points(Q_gps$"LONG", Q_gps$"LAT")
Q_pts_envir <- cbind(Q_pts, Q_extracted) ### ?? maybe not working ? how do I join the layer extractions with the original pt. data??

write.csv(Q_extracted, "./Q_pts_extracted.csv")



#dem <- resample(dem,)
#dem <- setExtent(dem, example, keepres = T)

ppt_05
ppt_05 <- projectRaster(ppt_05, example)
ppt_05


proj4string(dem) # +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0
#plot(dem, main = "Elevation")
#plot(dem)
#levelplot(dem_proj)
    # precipitation
ppt_05 <- raster("PRISM/ppt_05/PRISM_ppt_30yr_normal_4kmM2_05_bil.bil")
,
  xmn=-2355829, xmx=2257361, ymn=268858, ymx=3164258,
  crs=NA, template=NULL)
ppt_05_proj <- projectRaster(crs = aea, ppt_05, method = "bilinear")
ppt_05_proj <- setExtent(ppt_05_proj, ext, keepres = T)
ppt_06 <- raster("PRISM/ppt_06/PRISM_ppt_30yr_normal_4kmM2_06_bil.bil")
ppt_06_proj <- projectRaster(crs = aea, ppt_06, method = "bilinear")
ppt_06_proj <- setExtent(ppt_06_proj, ext, keepres = T)
ppt_07 <- raster("PRISM/ppt_07/PRISM_ppt_30yr_normal_4kmM2_07_bil.bil")
ppt_07_proj <- projectRaster(crs = aea, ppt_07, method = "bilinear")
ppt_07_proj <- setExtent(ppt_07_proj, ext, keepres = T)
ppt_08 <- raster("PRISM/ppt_08/PRISM_ppt_30yr_normal_4kmM2_08_bil.bil")
ppt_08_proj <- projectRaster(crs = aea, ppt_08, method = "bilinear")
ppt_08_proj <- setExtent(ppt_08_proj, ext, keepres = T)
ppt_09 <- raster("PRISM/ppt_09/PRISM_ppt_30yr_normal_4kmM2_09_bil.bil")
ppt_09_proj <- projectRaster(crs = aea, ppt_09, method = "bilinear")
ppt_09_proj <- setExtent(ppt_09_proj, ext, keepres = T)
ppt_annual <- raster("PRISM/ppt_annual/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
ppt_annual_proj <- projectRaster(crs = aea, ppt_annual, method = "bilinear")
ppt_annual_proj <- setExtent(ppt_annual_proj, ext, keepres = T)
    # temperature
tmean_01 <- raster("PRISM/tmean_01/PRISM_tmean_30yr_normal_4kmM2_01_bil.bil")
tmean_01_proj <- projectRaster(crs = aea, tmean_01, method = "bilinear")
tmean_01_proj <- setExtent(tmean_01_proj, ext, keepres = T)
tmean_05 <- raster("PRISM/tmean_05/PRISM_tmean_30yr_normal_4kmM2_05_bil.bil")
tmean_05_proj <- projectRaster(crs = aea, tmean_05, method = "bilinear")
tmean_05_proj <- setExtent(tmean_05_proj, ext, keepres = T)
tmean_06 <- raster("PRISM/tmean_06/PRISM_tmean_30yr_normal_4kmM2_06_bil.bil")
tmean_06_proj <- projectRaster(crs = aea, tmean_06, method = "bilinear")
tmean_06_proj <- setExtent(tmean_06_proj, ext, keepres = T)
tmean_07 <- raster("PRISM/tmean_07/PRISM_tmean_30yr_normal_4kmM2_07_bil.bil")
tmean_07_proj <- projectRaster(crs = aea, tmean_07, method = "bilinear")
tmean_07_proj <- setExtent(tmean_07_proj, ext, keepres = T)
tmean_08 <- raster("PRISM/tmean_08/PRISM_tmean_30yr_normal_4kmM2_08_bil.bil")
tmean_08_proj <- projectRaster(crs = aea, tmean_08, method = "bilinear")
tmean_08_proj <- setExtent(tmean_08_proj, ext, keepres = T)
tmean_09 <- raster("PRISM/tmean_09/PRISM_tmean_30yr_normal_4kmM2_09_bil.bil")
tmean_09_proj <- projectRaster(crs = aea, tmean_09, method = "bilinear")
tmean_09_proj <- setExtent(tmean_09_proj, ext, keepres = T)
tmean_annual <- raster("PRISM/tmean_annual/PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil")
tmean_annual_proj <- projectRaster(crs = aea, tmean_annual, method = "bilinear")
tmean_annual_proj <- setExtent(tmean_annual_proj, ext, keepres = T)
    # min and max vapor pressure deficit
      # data are available for each month individually -- located in same folder locally
      # which month(s) do we want to use?
vpdmax <- raster("PRISM/vpdmax/PRISM_vpdmax_30yr_normal_4kmM2_annual_bil.bil")
vpdmax_proj <- projectRaster(crs = aea, vpdmax, method = "bilinear")
vpdmax_proj <- setExtent(vpdmax_proj, ext, keepres = T)
vpdmin <- raster("PRISM/vpdmin/PRISM_vpdmin_30yr_normal_4kmM2_annual_bil.bil")
vpdmin_proj <- projectRaster(crs = aea, vpdmin, method = "bilinear")
vpdmin_proj <- setExtent(vpdmin_proj, ext, keepres = T)

## stack all layers
  # create list of layers
predictors_list <- c(dem, ppt_05)
  # stack all in list
predictors_stack <- stack(predictors_list)








#doesnt work:

download.file('http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2011&FNAME=nlcd_2011_landcover_2011_edition_2014_10_10.zip',
              destfile={f <- tempfile()}, quiet=TRUE, cacheOK=FALSE)
unzip(f, exdir={d <- tempdir()})
library(rasterVis)
r <- raster(file.path(d, 'nlcd_2011_landcover_2011_edition_2014_10_10.img'), crs=CRS('+proj=longlat'))
levelplot(r, margin=FALSE, col.regions=rainbow)
