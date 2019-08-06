####################### Lake protection by LAGOS lake connectivity class #######################
# Use LAGOS crosswalk to match LAGOS conn class to lakes in NHDplusV2/LakeCat
# Output is new crosswalk table exported for analysis in Lakes_in_ProtectedAreas_2019.R
# Date: 2-7-19
# updated: 8-6-19
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

LAGOS_FCODES <- c(39000,39004,39009,39010,39011,39012,43600,43613,43615,43617,43618,43619,43621)
#39000: lake/pond, no attributes
#39004: lake/pond, perennial
#39009: lake/pond, perennial, average water elevation
#39010: lake/pond, perennial, normal pool
#39011: lake/pond, perennial, date of photo
#39012: lake/pond, perennial, spillway elevation
#43600: reservoir
#43613: reservoir, construction material, perennial
#43615: reservoir, construction material, perennial
#43617: reservoir, water storage
#43618: reservoir, constuction material
#43619: reservoir, constuction material
#43621: reservoir, water storage, perennial

### Others in NHD with FTYPE as LakePond or Reservoir
# 43624: reservoir, treatment
# 43601: reservoir, aquaculture
# 43606: reservoir, disposal
# 39001: lake/pond, intermittent ### KEEP?
# 39005: lake/pond, intermittent, high water elevation ### KEEP?
# 39006: lake/pond, intermittent, date of photo ### KEEP?
# 43607: reservoir, evaporator
# 43609: reservoir, treatment, cooling pond
# 43605: reservoir, disposal tailings 
intermittent_FCODES <- c(39001,39005,39006)
all_FCODES <- c(LAGOS_FCODES, intermittent_FCODES)

NHD_pts <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_waterbody_pts.shp")
NHD_pts <- subset(NHD_pts, FTYPE=='LakePond' | FTYPE=='Reservoir')
NHD_pts <- subset(NHD_pts, AREASQKM >= 0.01) #1ha
NHD_pts <- NHD_pts[!duplicated(NHD_pts@data$COMID),] #remove duplicate COMID
NHD_pts <- subset(NHD_pts, FCODE %in% all_FCODES)

#NHD_pts_inter <- subset(NHD_pts, FCODE %in% intermittent_FCODES)
NHD_pts_perm <- subset(NHD_pts, FCODE %in% LAGOS_FCODES)

# X-walk table for linking NHD to LAGOS lakes
xwalk <- read.csv("C:/Ian_GIS/FreshwaterConservation/LAGOS_Lake_Link_v1.csv") #too large for github

# LAGOS-US lakes >= 1ha with connectivity classes
LAGOS_1ha <- shapefile("C:/Ian_GIS/LAGOS_US_GIS/LAGOS_US_All_lakes_1ha_Conn.shp")

#### Main program ####
# add in lake connectivity classes from LAGOS
xwalk_reduced <- xwalk[,c('lagoslakeid','nhdplusv2_reachcode','nhdplusv2_comid')] #keep only desired columns in xwalk table for joining
xwalk_reduced <- xwalk_reduced[!duplicated(xwalk_reduced$lagoslakeid),] #first remove duplicate lagoslakeids
LAGOS_1ha_xwalk <- merge(LAGOS_1ha@data[,c('lagoslakei','LakeConnec')], xwalk_reduced, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
LAGOS_1ha_xwalk_export <- LAGOS_1ha_xwalk[!duplicated(LAGOS_1ha_xwalk$nhdplusv2_comid),] #remove dup COMID
#write.csv(LAGOS_1ha_xwalk_export, "Data/LakeConn_1ha_LAGOS_NHD_Xwalk.csv")

## what is the distribution of lake conn classes in LAGOS?
LAGOS_1ha_conn_totals <- as.data.frame(LAGOS_1ha@data %>%
                                        group_by(LakeConnec) %>%
                                         tally())
LAGOS_1ha_conn_totals$pct <- LAGOS_1ha_conn_totals$n/sum(LAGOS_1ha_conn_totals$n)

## Compare to distribution of lake conn classes that MATCHED between LAGOS and NHD
matching_conn_totals <- as.data.frame(LAGOS_1ha_xwalk_export %>%
                                         group_by(LakeConnec) %>%
                                         tally())
matching_conn_totals$pct <- matching_conn_totals$n/sum(matching_conn_totals$n)
