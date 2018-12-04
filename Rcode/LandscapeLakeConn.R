####################### Landscape connectivity of lakes ########################################
# Date: 11-8-18
# updated: 12-4-18
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(grainscape)
library(ggplot2)
library(SDMTools)
library(rgeos)
library(dplyr)

#### input data ####
setwd("C:/Users/FWL/Documents/FreshwaterConservation")

# cost raster from ArcGIS
mich_cost <- raster("C:/Ian_GIS/FreshwaterConservation/Exports/MI_Cost.tif")

# Michigan shapefile
mich_shp <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/STATE/Michigan.shp")

# Michigan lakes from NHD (and convert to same crs as other data) #uncut borders allows lakes to spill across state line instead of being cut off
#mich_lakes_NHD <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_Mich/NHD_waterbody_Mich_unCut_borders.shp") 
#mich_lakes_NHD <- spTransform(mich_lakes_NHD, CRSobj=crs(mich_shp))

# LAGOS NE lakes
lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_pts/LAGOS_US_All_Lakes_4ha_pts.shp")
lakes_4ha_poly <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/LAGOS_NE_All_Lakes_4ha/LAGOS_NE_All_Lakes_4ha.shp")

# Zonal statistics table of Cost Distance surface for each lake (ArcGIS output)
zonal_cost_dist <- read.csv("C:/Ian_GIS/FreshwaterConservation/Exports/MI_CostDist_lagoslakeid.csv")

# PADUS raster (protected areas database)
PADUS <- raster("C:/Ian_GIS/FreshwaterConservation/Exports/MI_PADUS.tif")

############################ Main program ############################
# from grainscape...haven't gotten to work yet
#patchyMPG <- MPG(mich_cost, patch = (mich_cost == 9))

### exploratory analysis of lake zonal cost data
hist(zonal_cost_dist$MEAN, breaks=seq(0,30000,100))
hist(zonal_cost_dist$MEAN, breaks=seq(0,30000,1000), xlim=c(0,10000))
lower_uppers <- quantile(zonal_cost_dist$MEAN, c(0.05, 0.95))

# how many really isolated lakes? (based on arbitrary cutoff of 95th percentile)
nrow(subset(zonal_cost_dist, MEAN >= lower_uppers[2]))

lakes_4ha_pts_ZN <- merge(lakes_4ha_pts, zonal_cost_dist, by='lagoslakei', all.x=F)
lakes_4ha_pts_ZN_isol <- subset(lakes_4ha_pts_ZN, MEAN >= lower_uppers[2])

plot(mich_shp)
plot(PADUS, add=T, col=c('darkgreen','darkolivegreen1','gray45'))
plot(lakes_4ha_pts_ZN_isol, col='orange', pch=20, add=T)
title('Terrestrially isolated lakes, protected areas')
mtext(side=3, paste0('n lakes: ', nrow(lakes_4ha_pts_ZN_isol@data)))

hist(zonal_cost_dist$MEAN, breaks=seq(0,30000,1000), xlim=c(0,10000), xlab=paste0('Low to High'),
     main='Relative cost distance', ylab='')

############# Patch stats on Michigan lakes ########
lake_size_cutoff <- 0.04 #sq km (1 sq km = 100 ha) for NHD

# create subset of Michigan lakes based on lake size (patch size) cutoff
#mich_lakes_NHD_sub <- subset(mich_lakes_NHD, AREASQKM >= lake_size_cutoff)

# keep only FTYPE = LakePond or Reservoir
#mich_lakes_NHD_sub <- subset(mich_lakes_NHD_sub, FTYPE=='LakePond' | FTYPE=='Reservoir')
mich_lakes_LAGOS <- subset(lakes_4ha_poly, STATE=='MI')

# get rid of Great Lakes for now
#great_lakes_comid <- c(904140245, 904140244, 904140243, 904140248)
#mich_lakes_NHD_sub <- subset(mich_lakes_NHD_sub, !(COMID %in% great_lakes_comid))

# convert lake subset polygon to raster
## Set up a raster "template" for a 30 m grid
#r <- raster(extent(mich_lakes_NHD_sub), res=30) #res in meters (crs of input)
rlagos <- raster(extent(mich_lakes_LAGOS), res=30)

## Rasterize the shapefile (assigning NHD COMID as patch value)
#mich_lake_raster <- rasterize(mich_lakes_NHD_sub, r, field=as.numeric(mich_lakes_NHD_sub@data$COMID))
mich_lake_raster <- rasterize(mich_lakes_LAGOS , rlagos, field=as.numeric(mich_lakes_LAGOS@data$lagoslakei))

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

## calculate core area manually using negative buffer around lake polygons
# Warning! If inner buffer is too wide, can wipe out small lakes
# therefore, not a great method
# unfortunately, raster::buffer does not allow negative buffers
#
# mich_lakes_NHD_sub_buff <- gBuffer(mich_lakes_NHD_sub, byid=T, width=-10) #meters
# core_df <- data.frame(COMID=mich_lakes_NHD_sub@data$COMID, AREASQKM=mich_lakes_NHD_sub@data$AREASQKM)
# core_df$core_area_sqm <- gArea(mich_lakes_NHD_sub_buff, byid=T)
# core_df$edge_area_sqm <- core_df$AREASQKM*1000000 - core_df$core_area_sqm
# core_df$COMID <- as.integer(as.character(core_df$COMID))
# 
# manual_patch_stats <- left_join(test, core_df, by=c('patchID'='COMID'))
# manual_patch_stats$core.area.index_manual <- manual_patch_stats$core_area_sqm/manual_patch_stats$area

# basic class-level metrics (all lakes are the same "class") Warning: slow
class_stats <- ClassStat(mat=mich_lake_raster, cellsize=30, bkgd=NA, latlon=F)
