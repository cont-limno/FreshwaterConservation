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

# X-walk table for linking NHD to LAGOS lakes
xwalk <- read.csv("C:/Ian_GIS/FreshwaterConservation/LAGOS_Lake_Link_v1.csv") #too large for github

# LAGOS-US lakes >= 1ha with connectivity classes
LAGOS_1ha <- shapefile("C:/Ian_GIS/LAGOS_US_GIS/LAGOS_US_All_lakes_1ha_Conn.shp")

#### Main program ####
# add in lake connectivity classes from LAGOS
xwalk_reduced <- xwalk[,c('lagoslakeid','nhdplusv2_reachcode','nhdplusv2_comid')] #keep only desired columns in xwalk table for joining
xwalk_reduced <- xwalk_reduced[!duplicated(xwalk_reduced$lagoslakeid),] #first remove duplicate lagoslakeids
LAGOS_1ha_xwalk <- merge(LAGOS_1ha@data[,c('lagoslakei','LakeConnec')], xwalk_reduced, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
LAGOS_1ha_xwalk <- LAGOS_1ha_xwalk[!duplicated(LAGOS_1ha_xwalk$nhdplusv2_comid),] #remove dup COMID
#write.csv(LAGOS_1ha_xwalk, "Data/LakeConn_1ha_LAGOS_NHD_Xwalk.csv")
