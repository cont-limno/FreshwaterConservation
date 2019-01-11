#### Calculate percent protected for lake watersheds and snapping turtle dispersal buffers #####
# Date: 1-3-19
# updated: 1-11-18
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(LAGOSNE)
library(raster)
library(rgeos)

#### Input data ####
dt <- lagosne_load(version = '1.087.1') #returns list of data.frame objects

setwd('C:/Users/FWL/Documents/FreshwaterConservation')

# results from ArcGIS tabulate area
PADUS_IWS <- read.csv("Data/PADUS_LAGOSNE_IWS.csv")
PADUS_buff <- read.csv("Data/PADUS_Mich_4ha_lakes_2020mBuff.csv") #2020 m snapping turtle dispersal buffer (Patrick et al. 2012)

# shapefile of lake buffers (needed to calculate areas)
buff_shp <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/LAGOS_NE_All_Lakes_4ha/LAGOS_NE_4ha_within10km_mich_2020mBuff.shp")

# LAGOS NE lakes
lakes_4ha_poly <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/LAGOS_NE_All_Lakes_4ha/LAGOS_NE_All_Lakes_4ha.shp")

############## Main program #################
# identify Mich lagoslakeids (focal lakes)
mich_lakes_4ha <- subset(lakes_4ha_poly, STATE=='MI')
mich_lagoslakeids <- unique(mich_lakes_4ha@data$lagoslakei)

# calculate % of each IWS and lake buffer protected based on tabulate area results above
# first need to get areas of IWS and lake buffers, join to tables
# only keep Michigan lakes

buff_shp@data$buff_area_ha <- gArea(buff_shp, byid=T)/10000
buff_shp <- subset(buff_shp, lagoslakei %in% mich_lagoslakeids)
buff_areas <- data.frame(lagoslakeid=buff_shp@data$lagoslakei, buff_area_ha=buff_shp@data$buff_area_ha)
IWS_areas <- data.frame(lagoslakeid=dt$iws$lagoslakeid, IWS_area_ha=dt$iws$iws_ha)
IWS_areas <- subset(IWS_areas, lagoslakeid %in% mich_lagoslakeids)

PADUS_IWS <- merge(PADUS_IWS, IWS_areas, by.x='LAGOSLAKEI', by.y='lagoslakeid', all=F)
PADUS_buff <- merge(PADUS_buff, buff_areas, by.x='LAGOSLAKEI', by.y='lagoslakeid', all=F)

# calculate total area of GAPS 1-2 and GAPS 1-3
PADUS_IWS$GAP12_IWS_ha <- (PADUS_IWS$A_1 + PADUS_IWS$A_2)/10000
PADUS_IWS$GAP123_IWS_ha <- (PADUS_IWS$A_1 + PADUS_IWS$A_2 + PADUS_IWS$A_3)/10000

PADUS_buff$GAP12_buff_ha <- (PADUS_buff$A_1 + PADUS_buff$A_2)/10000
PADUS_buff$GAP123_buff_ha <- (PADUS_buff$A_1 + PADUS_buff$A_2 + PADUS_buff$A_3)/10000

# calculate percents
PADUS_IWS$GAP12_IWS_pct <- PADUS_IWS$GAP12_IWS_ha/PADUS_IWS$IWS_area_ha
PADUS_IWS$GAP123_IWS_pct <- PADUS_IWS$GAP123_IWS_ha/PADUS_IWS$IWS_area_ha

PADUS_buff$GAP12_buff_pct <- PADUS_buff$GAP12_buff_ha/PADUS_buff$buff_area_ha
PADUS_buff$GAP123_buff_pct <- PADUS_buff$GAP123_buff_ha/PADUS_buff$buff_area_ha

# due to raster calculations, some buffers or IWS came out slightly > 100% protected; change these to 100%
PADUS_IWS$GAP12_IWS_pct <- ifelse(PADUS_IWS$GAP12_IWS_pct > 1, 1, PADUS_IWS$GAP12_IWS_pct)
PADUS_IWS$GAP123_IWS_pct <- ifelse(PADUS_IWS$GAP123_IWS_pct > 1, 1, PADUS_IWS$GAP123_IWS_pct)

PADUS_buff$GAP12_buff_pct <- ifelse(PADUS_buff$GAP12_buff_pct > 1, 1, PADUS_buff$GAP12_buff_pct)
PADUS_buff$GAP123_buff_pct <- ifelse(PADUS_buff$GAP123_buff_pct > 1, 1, PADUS_buff$GAP123_buff_pct)

# What is the correlation between % IWS protected and % buff protected?
mich_lakes_df <- as.data.frame(mich_lakes_4ha@data)
mich_lakes_df <- mich_lakes_df[,c('lagoslakei','GNIS_Name')]

mich_lakes_df <- merge(mich_lakes_df, PADUS_IWS, by.x='lagoslakei', by.y='LAGOSLAKEI', all.x=T)
mich_lakes_df <- merge(mich_lakes_df, PADUS_buff, by.x='lagoslakei', by.y='LAGOSLAKEI', all.x=T)

plot(mich_lakes_df$GAP12_IWS_pct ~ mich_lakes_df$GAP12_buff_pct, pch=20)
plot(mich_lakes_df$GAP123_IWS_pct ~ mich_lakes_df$GAP123_buff_pct, pch=20)

cor(mich_lakes_df$GAP12_IWS_pct, mich_lakes_df$GAP12_buff_pct, use='pairwise.complete.obs', method='pearson')
cor(mich_lakes_df$GAP123_IWS_pct, mich_lakes_df$GAP123_buff_pct, use='pairwise.complete.obs', method='pearson')

# save output for use in other analysis
write.csv(PADUS_IWS, file="Data/PADUS_MI_IWS_pct.csv")
write.csv(PADUS_buff, file="Data/PADUS_MI_Buff2020m_pct.csv")
