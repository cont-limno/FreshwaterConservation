####################### Lake protection by LAGOS lake connectivity class #######################
# Use LAGOS crosswalk to match LAGOS conn class to lakes in NHDplusV2/LakeCat
# Output is new crosswalk table exported for analysis in Lakes_in_ProtectedAreas_2019.R
# Date: 2-7-19
# updated: 4-10-19
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(dplyr)
library(reshape2)
library(ggplot2)

#### Input data ####
setwd('C:/Users/FWL/Documents/FreshwaterConservation')

PADUS_table <- read.csv("Data/PADUS.csv")

NHD_pts <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_waterbody_pts.shp")
NHD_pts <- subset(NHD_pts, FTYPE=='LakePond' | FTYPE=='Reservoir')
NHD_pts <- subset(NHD_pts, AREASQKM >= 0.01)

# X-walk table for linking NHD to LAGOS lakes
xwalk <- read.csv("C:/Ian_GIS/FreshwaterConservation/LAGOS_Lake_Link_v1.csv") #too large for github

# LAGOS-US lakes >= 1ha with connectivity classes
LAGOS_1ha <- shapefile("C:/Ian_GIS/LAGOS_US_GIS/LAGOS_US_All_lakes_1ha_Conn.shp")

#### Main program ####
# add in lake connectivity classes from LAGOS
xwalk_reduced <- xwalk[,c('lagoslakeid','nhdplusv2_reachcode','nhdplusv2_comid')] #keep only desired columns in xwalk table for joining
xwalk_reduced <- xwalk_reduced[!duplicated(xwalk_reduced$lagoslakeid),] #first remove duplicate lagoslakeids
LAGOS_1ha_xwalk <- merge(LAGOS_1ha@data[,c('lagoslakei','LakeConnec','AreaSqKm')], xwalk_reduced, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
LAGOS_1ha_xwalk_export <- LAGOS_1ha_xwalk[!duplicated(LAGOS_1ha_xwalk$nhdplusv2_comid),] #remove dup COMID
#write.csv(LAGOS_1ha_xwalk_export, "Data/LakeConn_1ha_LAGOS_NHD_Xwalk.csv")

## Anything special about the non-matches?
NHD_area_df <- NHD_pts@data[,c('COMID','AREASQKM')]
#NHD_area_df <- NHD_area_df[!duplicated(NHD_area_df$COMID),]
#colSums(is.na(NHD_pts@data)) #count number of NAs in each column

# Get lake area (hectares) for matching LAGOS-NHD lakes
match_COMID_area <- LAGOS_1ha_xwalk_export
match_COMID_area$sqha <- match_COMID_area$AreaSqKm*100
summary(match_COMID_area$sqha)

# Get lake area (hectares) for non matching LAGOS-NHD lakes
non_match_COMID_area <- subset(NHD_area_df, !(COMID %in% match_COMID_area$nhdplusv2_comid))
non_match_COMID_area$sqha <- non_match_COMID_area$AREASQKM*100
summary(non_match_COMID_area$sqha)

# Compare size distributions for matching and non matching lakes
par(mfrow=c(1,2))
hist(match_COMID_area$sqha, main='Matching LAGOS-NHD', xlim=c(0,400), breaks=seq(0,340000,10), xlab='Hectares',
     ylim=c(0,180000))
mtext(side=3, paste0('nlakes: ', nrow(match_COMID_area)))
hist(non_match_COMID_area$sqha, main='Non matching LAGOS-NHD', xlim=c(0,400),breaks=seq(0,5800000,10), xlab='Hectares',
     ylim=c(0,180000))
mtext(side=3, paste0('nlakes: ', nrow(non_match_COMID_area)))
