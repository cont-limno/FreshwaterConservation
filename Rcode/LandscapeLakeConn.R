####################### Landscape connectivity of lakes ########################################
# Date: 11-8-18
# updated: 12-8-18
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
#library(grainscape)
library(ggplot2)
library(SDMTools)
library(rgeos)
library(dplyr)
library(spatialEco)

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

# Pre-calculated patch metric data (from SDMTools::PatchStat)
LakePatchMetrics <- read.csv("Data/MichiganLakePatchStats_wBorderStates.csv")

##### D-fine functions #####

source("Rcode/functions/lake_patch_metrics.R")

################### Lake patch statistics within specified buffer zones ###################
# may be species-based or hypothetical buffer distance from focal lake

## Loop function over all Michigan LAGOS lakes (>= 4ha)
mich_lakes_LAGOS <- subset(lakes_4ha_poly, STATE=='MI')
mich_lagoslakeids <- unique(mich_lakes_LAGOS@data$lagoslakei)

# set dispersal buffer width and cellsize for raster analysis
dispersal_buff <- 2000 #meters
cellsize <- 30 #m cell width

# test function
# use all 4ha lakes so can account for connectivity into neighboring states
# patchmetric_df contains WI, OH, IN lakes within 10km of Michigan border
# probably don't have to worry about Ontario due to river border, but may need to eliminate Lake St. Clair
whee <- lake_patch_metrics(lagoslakeid=70807, lake_shp=lakes_4ha_poly, cellsize=30, dispersal_buff=dispersal_buff, patchmetric_df=LakePatchMetrics)

# create empty data frame, with each iteration filling in a row
LakeBufferPatchStatz <- data.frame(matrix(NA, nrow = length(mich_lagoslakeids), ncol = 13))
colnames(LakeBufferPatchStatz) <- c('lagoslakeid','FullLakeArea_ha','FullLakePerimeter_km','FullLakeEdgeArea_ha',
                                    'AvgFullLakeShapeIndex','FullLakeCoreArea_ha','AvgFullLakeCoreAreaIndex','nLakePatches','BuffArea_ha','EdgeLengthInBuff_km',
                                    'EdgeAreaInBuff_ha','EdgeAreaInBuff_pct','BufferWidth_m')
for (i in 1:length(mich_lagoslakeids)){
  tump <- lake_patch_metrics(lagoslakeid=mich_lagoslakeids[i], lake_shp=lakes_4ha_poly, dispersal_buff=dispersal_buff, cellsize=cellsize, patchmetric_df=LakePatchMetrics)
  LakeBufferPatchStatz[i,] <- tump
  tump <- NULL
}

# tripped: 
# lagoslakeid: 23961
# 62520: erase lake from buffer generates empty output, causing error, even though lake and buffer appear to overlap in plot()
# 1362: ran manually, worked...not sure of issue. Seems to be trying to process non-existent intersecting lakes despite conditional
# 47003: did not check why...

for (i in 3938:length(mich_lagoslakeids)){
  tump <- lake_patch_metrics(lagoslakeid=mich_lagoslakeids[i], lake_shp=lakes_4ha_poly, dispersal_buff=dispersal_buff, cellsize=cellsize, patchmetric_df=LakePatchMetrics)
  LakeBufferPatchStatz[i,] <- tump
  tump <- NULL
}


### Analysis of lake zonal cost data (used ArcGIS zonal stats to extract cost distance for each lake)
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
