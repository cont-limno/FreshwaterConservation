####################### Landscape connectivity of lakes ########################################
# Date: 11-8-18
# updated: 12-6-18
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
#library(grainscape)
library(ggplot2)
library(SDMTools)
library(rgeos)
library(dplyr)
#library(spatialEco)

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

##### D-fine functions #####
################### Lake patch statistics ###################
lake_patch_metrics <- function(lagoslakeid, lake_shp, dispersal_buff, cellsize){
  #lagoslakeid: unique lake ID in LAGOS
  #lake_shp: shapefile of lake polygons
  #dispersal_buff: maximum overland dispersal distance, meters
  #cellsize: pixel width of analysis
  
  # extract focal lake, buffer it by dispersal width, erase focal lake from buffer zone
  focal_lake <- lagoslakeid
  focal_lake_shp <- subset(lake_shp, lagoslakei %in% lagoslakeid)
  focal_lake_buff <- gBuffer(focal_lake_shp, byid=T, width=dispersal_buff)
  focal_lake_buff_erased <- erase(focal_lake_buff, focal_lake_shp)
  
  # identify lakes that intersect with buffer (only lake sections within buffer)
  intersecting_lakes <- raster::intersect(lake_shp, focal_lake_buff_erased)
  rtemp <- raster(extent(intersecting_lakes), res=cellsize)#create blank raster before rasterizing lakes based on lagoslakeid
  intersecting_lakes_ras <- rasterize(intersecting_lakes , rtemp, field=as.numeric(intersecting_lakes@data$lagoslakei.1))
  
  # identify lakes that intersect with buffer (FULL lake polygons)
  intersecting_lakes_full <- subset(lake_shp, lagoslakei %in% unique(intersecting_lakes@data$lagoslakei.1))
  rtemp <- raster(extent(intersecting_lakes_full), res=cellsize)
  intersecting_lakes_full_ras <- rasterize(intersecting_lakes_full, rtemp, field=as.numeric(intersecting_lakes_full@data$lagoslakei))
  
  # patch statistics for portions of lakes that fall within buffer (only interested in perimeter, which is edge/entry into lakes)
  BuffPatchStatz <- PatchStat(intersecting_lakes_ras, cellsize=cellsize, latlon=F) #verified by Ian: does not perform on whole lake; just intersecting part
  
  # patch statistics for full lakes intersecting with buffer, including portions of those lakes outside buffer
  FullLakePatchStatz <- PatchStat(intersecting_lakes_full_ras, cellsize=cellsize, latlon=F)
  newcolnames <- c(names(FullLakePatchStatz)[1], paste0(names(FullLakePatchStatz)[2:ncol(FullLakePatchStatz)],'.full.lake'))
  colnames(FullLakePatchStatz) <- newcolnames
  FullLakePatchStatz$EdgeLength_m <- BuffPatchStatz$perimeter
  FullLakePatchStatz$LakeAreaInBuffer_sqm <- BuffPatchStatz$area
  FullLakePatchStatz <- FullLakePatchStatz[,c('area.full.lake','EdgeLength_m','LakeAreaInBuffer_sqm','shape.index.full.lake')]
  # NEED TO CALCULATE TOTALS FOR ALL OF THESE EXCEPT SHAPE INDEX (get mean)
  abc <- colSums(FullLakePatchStatz[,1:3], na.rm=T)
  d <- mean(FullLakePatchStatz$shape.index.full.lake, na.rm=T)
  nLakePatches <- nrow(FullLakePatchStatz) 
  output <- data.frame(lagoslakeid=lagoslakeid, nLakePatches=nLakePatches, area.full.lake.sqm=abc[1], EdgeLength_m=abc[2], LakeAreaInBuffer_sqm=abc[3],
                       mean.shape.index.full.lake=d)
  rownames(output) <- lagoslakeid
  return(output)
  
  # clean up
  FullLakePatchStatz <- NULL
  newcolnames <- NULL
  intersecting_lakes_full <- NULL
  intersecting_lakes_full_ras <- NULL
  intersecting_lakes <- NULL
  intersecting_lakes_ras <- NULL
  BuffPatchStatz <- NULL
  focal_lake <- NULL
  focal_lake_buff <- NULL
  focal_lake_buff_erased <- NULL
  focal_lake_shp <- NULL
  rtemp <- NULL
  output <- NULL
  abc <- NULL
  d <- NULL
  nLakePatches <- NULL
}
# test function
whee <- lake_patch_metrics(lagoslakeid=48001, lake_shp=mich_lakes_LAGOS, cellsize=30, dispersal_buff=dispersal_buff)
mich_lagoslakeids <- unique(mich_lakes_LAGOS@data$lagoslakei)

whee <- lake_patch_metrics(lagoslakeid=mich_lagoslakeids[10], lake_shp=mich_lakes_LAGOS, cellsize=30, dispersal_buff=dispersal_buff)



focal_lake <- mich_lagoslakeids[10]
focal_lake_shp <- subset(mich_lakes_LAGOS, lagoslakei %in% focal_lake)
focal_lake_buff <- gBuffer(focal_lake_shp, byid=T, width=dispersal_buff)
focal_lake_buff_erased <- erase(focal_lake_buff, focal_lake_shp)

# identify lakes that intersect with buffer (only lake sections within buffer)
intersecting_lakes <- raster::intersect(mich_lakes_LAGOS, focal_lake_buff_erased)
rtemp <- raster(extent(intersecting_lakes), res=30)
intersecting_lakes_ras <- rasterize(intersecting_lakes , rtemp, field=as.numeric(intersecting_lakes@data$lagoslakei.1))

# identify lakes that intersect with buffer (FULL lake polygons)
intersecting_lakes_full <- subset(mich_lakes_LAGOS, lagoslakei %in% unique(intersecting_lakes@data$lagoslakei.1))
rtemp <- raster(extent(intersecting_lakes_full), res=30)
intersecting_lakes_full_ras <- rasterize(intersecting_lakes_full, rtemp, field=as.numeric(intersecting_lakes_full@data$lagoslakei))

plot(focal_lake_buff_erased, col='gold')
plot(intersecting_lakes_ras, col='dodgerblue', add=T)

# patch statistics for portions of lakes that fall within buffer (only interested in perimeter, which is edge/entry into lakes)
BuffPatchStatz <- PatchStat(intersecting_lakes_ras, cellsize=30, latlon=F) #verified by Ian: does not perform on whole lake; just intersecting part
EdgeLength_m <- BuffPatchStatz$perimeter

# pathc statistics for full lakes intersecting with buffer, including portions of those lakes outside buffer
FullLakePatchStatz <- PatchStat(intersecting_lakes_full_ras, cellsize=30, latlon=F)

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

############################ Main program ############################
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

######### Determine amount of lake "habitat" within species-specific dispersal buffers around lakes ######
# may not make sense; barrier is getting to the lake, not moving within it
#dispersal_buff <- 2000 #meters

# change all lakes to value 1, all other to NA (so can use sum in extract and not worry about NAs)
#reclass_mat <- matrix(data=c(1,maxValue(mich_lake_raster),1,0,0,NA), nrow=2,ncol=3,byrow=T)
#mich_lakes_raster1s <- reclassify(mich_lake_raster, reclass_mat)

## testing area
# count lake cells within dispersal buffer
# mich_lagoslakeids <- unique(mich_lakes_LAGOS@data$lagoslakei)
# 
# mich_lakes_buff <- gBuffer(mich_lakes_LAGOS, byid=T, width=dispersal_buff)
# mich_lakes_buff_areas_ha <- gArea(mich_lakes_buff, byid=T)/10000 #convert sq m to hectares
# 
# # test_lakes <- mich_lagoslakeids[1:10]
# # test_lakes_buff <- subset(mich_lakes_buff, lagoslakei %in% test_lakes)
# # test_lakes_buff_areas_ha <- gArea(test_lakes_buff, byid=T)/10000
# 
# # MANNY FOR THE WIN!
# # https://mgimond.github.io/Spatial/raster-operations-in-r.html 
# # outputs spatialpoylgonsdataframe with sp=T with last column as number of cells within buffer
# lake_cells_buff <- extract(mich_lakes_raster1s, mich_lakes_buff, function(x,...)sum(x, na.rm=T), sp=F)
# # create new column of lake area within buffer (ha):
# # multiply number of cells by cell area (900 sq m), subtract focal lake area (ha, converted to sq m), then divide by 10000 to get into hectares
# lake_cells_buff_df <- data.frame(lagoslakeid=mich_lakes_buff@data$lagoslakei, LakeArea_ha=mich_lakes_buff@data$Lake_Area_,
#                                  FullBufferArea_ha=mich_lakes_buff_areas_ha, BuffLakeCells=lake_cells_buff)
# # calculate area of buffer zone excluding focal lake area
# lake_cells_buff_df$nonFocalLakeBuffArea_ha <- lake_cells_buff_df$FullBufferArea_ha - lake_cells_buff_df$LakeArea_ha
# # calculate lake area based on number of lake cells within buffer zone
# lake_cells_buff_df$nonFocalLakeArea_ha <- ((lake_cells_buff_df$BuffLakeCells*900)/10000) - lake_cells_buff_df$LakeArea_ha 
# # in some cases, may be no other lakes in buffer zone and slight mismatches in area estimates could yield negative lake area leftover; convert this to 0
# lake_cells_buff_df$nonFocalLakeArea_ha <- ifelse(lake_cells_buff_df$nonFocalLakeArea_ha < 0, 0, lake_cells_buff_df$nonFocalLakeArea_ha)
# # calculate % of buffer zone (excluding focal lake area) occupied by non focal lakes
# lake_cells_buff_df$nonFocalLakeArea_pct <- lake_cells_buff_df$nonFocalLakeArea_ha/lake_cells_buff_df$nonFocalLakeBuffArea_ha

### Lake edge area within buffers

############## Scratch area ##################
#### Determine amount of lake "habitat" within species-specific dispersal buffers around lakes
# may not make sense; barrier is getting to the lake, not moving within it
#dispersal_buff <- 2000 #meters

# change all lakes to value 1, all other to NA (so can use sum in extract and not worry about NAs)
#reclass_mat <- matrix(data=c(1,maxValue(mich_lake_raster),1,0,0,NA), nrow=2,ncol=3,byrow=T)
#mich_lakes_raster1s <- reclassify(mich_lake_raster, reclass_mat)

## testing area
# count lake cells within dispersal buffer
# mich_lagoslakeids <- unique(mich_lakes_LAGOS@data$lagoslakei)
# 
# mich_lakes_buff <- gBuffer(mich_lakes_LAGOS, byid=T, width=dispersal_buff)
# mich_lakes_buff_areas_ha <- gArea(mich_lakes_buff, byid=T)/10000 #convert sq m to hectares
# 
# # test_lakes <- mich_lagoslakeids[1:10]
# # test_lakes_buff <- subset(mich_lakes_buff, lagoslakei %in% test_lakes)
# # test_lakes_buff_areas_ha <- gArea(test_lakes_buff, byid=T)/10000
# 
# # MANNY FOR THE WIN!
# # https://mgimond.github.io/Spatial/raster-operations-in-r.html 
# # outputs spatialpoylgonsdataframe with sp=T with last column as number of cells within buffer
# lake_cells_buff <- extract(mich_lakes_raster1s, mich_lakes_buff, function(x,...)sum(x, na.rm=T), sp=F)
# # create new column of lake area within buffer (ha):
# # multiply number of cells by cell area (900 sq m), subtract focal lake area (ha, converted to sq m), then divide by 10000 to get into hectares
# lake_cells_buff_df <- data.frame(lagoslakeid=mich_lakes_buff@data$lagoslakei, LakeArea_ha=mich_lakes_buff@data$Lake_Area_,
#                                  FullBufferArea_ha=mich_lakes_buff_areas_ha, BuffLakeCells=lake_cells_buff)
# # calculate area of buffer zone excluding focal lake area
# lake_cells_buff_df$nonFocalLakeBuffArea_ha <- lake_cells_buff_df$FullBufferArea_ha - lake_cells_buff_df$LakeArea_ha
# # calculate lake area based on number of lake cells within buffer zone
# lake_cells_buff_df$nonFocalLakeArea_ha <- ((lake_cells_buff_df$BuffLakeCells*900)/10000) - lake_cells_buff_df$LakeArea_ha 
# # in some cases, may be no other lakes in buffer zone and slight mismatches in area estimates could yield negative lake area leftover; convert this to 0
# lake_cells_buff_df$nonFocalLakeArea_ha <- ifelse(lake_cells_buff_df$nonFocalLakeArea_ha < 0, 0, lake_cells_buff_df$nonFocalLakeArea_ha)
# # calculate % of buffer zone (excluding focal lake area) occupied by non focal lakes
# lake_cells_buff_df$nonFocalLakeArea_pct <- lake_cells_buff_df$nonFocalLakeArea_ha/lake_cells_buff_df$nonFocalLakeBuffArea_ha


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
#class_stats <- ClassStat(mat=mich_lake_raster, cellsize=30, bkgd=NA, latlon=F)
