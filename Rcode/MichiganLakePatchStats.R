####################### Patch stats on Michigan lakes ##########################################
# Date: 11-8-18
# updated: 12-10-18
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(SDMTools)

###### Input data ######
# Michigan shapefile
mich_shp <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/STATE/Michigan.shp")

# Michigan lakes from NHD (and convert to same crs as other data) #uncut borders allows lakes to spill across state line instead of being cut off
#mich_lakes_NHD <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_Mich/NHD_waterbody_Mich_unCut_borders.shp") 
#mich_lakes_NHD <- spTransform(mich_lakes_NHD, CRSobj=crs(mich_shp))

# LAGOS NE lakes (to account for border lakes, contains all lakes in Michigan or within 10km of Michigan)
lakes_4ha <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/LAGOS_NE_All_Lakes_4ha/LAGOS_NE_4ha_within10km_mich.shp")


##### Main program ####
# Calculated as if lakes are patches in the same way as terrestrial patches
# not taking into account dispersal abilities of species
# essentially a landscape structure analysis based on lake shape and other patch-type stats

# create subset of Michigan lakes based on lake size (patch size) cutoff
#lake_size_cutoff <- 0.04 #sq km (1 sq km = 100 ha) for NHD
#mich_lakes_NHD_sub <- subset(mich_lakes_NHD, AREASQKM >= lake_size_cutoff)

# keep only FTYPE = LakePond or Reservoir
#mich_lakes_NHD_sub <- subset(mich_lakes_NHD_sub, FTYPE=='LakePond' | FTYPE=='Reservoir')

# get rid of Great Lakes for now
#great_lakes_comid <- c(904140245, 904140244, 904140243, 904140248)
#mich_lakes_NHD_sub <- subset(mich_lakes_NHD_sub, !(COMID %in% great_lakes_comid))

# convert lake subset polygon to raster
## Set up a raster "template" for a 30 m grid
#r <- raster(extent(mich_lakes_NHD_sub), res=30) #res in meters (crs of input)
rlagos <- raster(extent(lakes_4ha), res=30)

## Rasterize the shapefile (assigning NHD COMID as patch value)
#mich_lake_raster <- rasterize(mich_lakes_NHD_sub, r, field=as.numeric(mich_lakes_NHD_sub@data$COMID))
mich_lake_raster <- rasterize(lakes_4ha , rlagos, field=as.numeric(lakes_4ha@data$lagoslakei))

# a simpler, vector-based alternative: https://rpubs.com/dgolicher/9458
# can also check out landscapemetrics package, or spatialEco::land.metrics
# calculate various patch metrics, PatchStat help file defines column names
# can join to shapefile by patchID
# PatchStat function seems great, but not clear how "core" is defined. Nothing obvious in function code
# source code has cryptic C code
# some back-calculations seem to be about 30m inward, but not sure if this is always 30m or based on cell width

test <- PatchStat(mat=mich_lake_raster, cellsize=30, latlon=F) 
test$edge.area <- test$area - test$core.area
test$edge.area.index <- test$edge.area/test$area

# Basic plots of PatchStat output
par(mfrow=c(3,3))
hist(test$area, main='Area', xlab='sq m')
hist(test$perimeter, main='Perimeter', xlab='m')
hist(test$perim.area.ratio, main='Perim/Area', xlab='m/sq m') #but decreases with increasing lake size
hist(test$core.area, main='Core area', xlab='sq m')
hist(test$edge.area, main='Edge area', xlab='sq m')
hist(test$shape.index, main='Shape index', xlab='Lo-Hi') #1=square shape, higher number=more complex shape. Corrects for perim/area issue
hist(test$frac.dim.index, main='Fractal dim index', xlab='Lo-Hi')#1=square, 2=highly complex shape. Also corrects for perim/area issue
hist(test$core.area.index, main='Core area index', xlab='Lo-Hi')#pct of patch that is core area
hist(test$edge.area.index, main='Edge area index', xlab='Lo-Hi')#pct of patch that is edge area

# save output
#write.csv(test, file="Data/MichiganLakePatchStats_wBorderStates.csv")

