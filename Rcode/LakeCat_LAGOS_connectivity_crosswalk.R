####################### Lake protection by LAGOS lake connectivity class #######################
# Use LAGOS crosswalk to match LAGOS conn class to lakes in NHDplusV2/LakeCat
# Output is new crosswalk table exported for analysis in Lakes_in_ProtectedAreas_2019.R
# Date: 2-7-19
# updated: 4-15-19
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
LAGOS_1ha_xwalk <- merge(LAGOS_1ha@data[,c('lagoslakei','LakeConnec','AreaSqKm')], xwalk_reduced, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
LAGOS_1ha_xwalk_export <- LAGOS_1ha_xwalk[!duplicated(LAGOS_1ha_xwalk$nhdplusv2_comid),] #remove dup COMID
#write.csv(LAGOS_1ha_xwalk_export, "Data/LakeConn_1ha_LAGOS_NHD_Xwalk.csv")

## Anything special about the non-matches?
NHD_area_df <- NHD_pts@data[,c('COMID','AREASQKM')]
NHD_area_df_perm <- NHD_pts_perm@data[,c('COMID','AREASQKM')]
#NHD_area_df <- NHD_area_df[!duplicated(NHD_area_df$COMID),]
#colSums(is.na(NHD_pts@data)) #count number of NAs in each column

# Get lake area (hectares) for matching LAGOS-NHD lakes
match_COMID_area <- LAGOS_1ha_xwalk_export
match_COMID_area$ha <- match_COMID_area$AreaSqKm*100
summary(match_COMID_area$ha)

# Get lake area (hectares) for non matching LAGOS-NHD lakes
#non_match_COMID_area <- subset(NHD_area_df, !(COMID %in% match_COMID_area$nhdplusv2_comid))
non_match_COMID_area <- subset(NHD_area_df_perm, !(COMID %in% match_COMID_area$nhdplusv2_comid))
non_match_COMID_area$ha <- non_match_COMID_area$AREASQKM*100
summary(non_match_COMID_area$ha)

# Compare size distributions for matching and non matching lakes
par(mfrow=c(1,2))
hist(match_COMID_area$ha, main='Matching LAGOS-NHD', xlim=c(0,400), breaks=seq(0,340000,10), xlab='Hectares',
     ylim=c(0,180000))
mtext(side=3, paste0('nlakes: ', nrow(match_COMID_area)))
hist(non_match_COMID_area$ha, main='Non matching LAGOS-NHD', xlim=c(0,400),breaks=seq(0,5800000,10), xlab='Hectares',
     ylim=c(0,180000))
mtext(side=3, paste0('nlakes: ', nrow(non_match_COMID_area)))

quantile(match_COMID_area$ha)
quantile(non_match_COMID_area$ha)

## Regardless of NHD matching, what is the distribution of lake conn classes in LAGOS?
LAGOS_1ha_conn_totals <- as.data.frame(LAGOS_1ha@data %>%
                                        group_by(LakeConnec) %>%
                                         tally())
LAGOS_1ha_conn_totals$pct <- LAGOS_1ha_conn_totals$n/sum(LAGOS_1ha_conn_totals$n)

## Compare to distribution of lake conn classes that MATCHED between LAGOS and NHD
matching_conn_totals <- as.data.frame(LAGOS_1ha_xwalk_export %>%
                                         group_by(LakeConnec) %>%
                                         tally())
matching_conn_totals$pct <- matching_conn_totals$n/sum(matching_conn_totals$n)


# Does NHD FCODE affect matching?
NHD_FCODE_df <- NHD_pts@data[,c('COMID','AREASQKM','FCODE')]
all_FCODE_totals <- as.data.frame(NHD_FCODE_df %>%
                                    group_by(FCODE) %>%
                                    tally())

match_COMID_area_FCODE <- merge(match_COMID_area, NHD_FCODE_df, by.x='nhdplusv2_comid', by.y='COMID')
non_match_COMID_area_FCODE <- merge(non_match_COMID_area, NHD_FCODE_df, by='COMID')

a1 <- as.data.frame(match_COMID_area_FCODE %>%
                group_by(FCODE) %>%
                tally())
names(a1) <- c('FCODE','match')
a2 <- as.data.frame(non_match_COMID_area_FCODE %>%
                group_by(FCODE) %>%
                tally())
names(a2) <- c('FCODE','nonmatch')

FCODE_summary <- merge(a1,a2, by='FCODE', all=T)
FCODE_summary <- merge(FCODE_summary, all_FCODE_totals, by='FCODE', all=T)
FCODE_summary$match_pct <- FCODE_summary$match/FCODE_summary$n
FCODE_summary$nonmatch_pct <- FCODE_summary$nonmatch/FCODE_summary$n
