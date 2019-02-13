####################### Characteristics of lakes in US protected areas #########################
# Date: 2-12-19
# updated:
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(reshape2)
library(vioplot)
library(rgdal)

#### input data ####
setwd("C:/Users/FWL/Documents/FreshwaterConservation")

# lower 48 states
lower48 <- shapefile("C:/Ian_GIS/cb_2016_us_state_500k/lower48.shp") #same crs as NHD_pts

#### Protected and unprotected lakes (centroids) ####
protected_GAPS12 <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/NHD_protect_pts_GAPS12.shp")
protected_GAPS123 <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/NHD_protect_pts_GAPS123.shp")

unprotected_GAPS12 <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/NHD_unprotect_pts_GAPS12.shp")
unprotected_GAPS123 <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/NHD_unprotect_pts_GAPS123.shp")

#### LakeCat data for analyzing lake characteristics (protected and unprotected) ####
# Protected land by GAP status by local catchments and network watersheds (LakeCat)
# From US Protected Areas Database (PADUS) v 1.4
# https://gapanalysis.usgs.gov/padus/data/download/
PADUS_LakeCat <- read.csv("Data/PADUS.csv")

# LakeCat data downloaded November 2018 (some files too large for github repo, so stored all locally)
# ftp://newftp.epa.gov/EPADataCommons/ORD/NHDPlusLandscapeAttributes/LakeCat/FinalTables/
elevation <- read.csv("C:/Ian_GIS/LakeCat/Elevation.csv")
NLCD_2011 <- read.csv("C:/Ian_GIS/LakeCat/NLCD2011.csv")
RoadDensity <- read.csv("C:/Ian_GIS/LakeCat/RoadDensity.csv")
PRISM <- read.csv("C:/Ian_GIS/LakeCat/PRISM_1981_2010.csv")
WetIndex <- read.csv("C:/Ian_GIS/LakeCat/WetIndx.csv")
Mines <- read.csv("C:/Ian_GIS/LakeCat/Mines.csv")
Dams <- read.csv("C:/Ian_GIS/LakeCat/NABD.csv")
Toxic <- read.csv("C:/Ian_GIS/LakeCat/EPA_FRS.csv")
ForestLoss <- read.csv("C:/Ian_GIS/LakeCat/ForestLossByYear0013.csv")
Fahr <- read.csv("C:/Ian_GIS/LakeCat/FirePerimeters.csv")
Deposition <- read.csv("C:/Ian_GIS/LakeCat/NADP.csv")
Impervious <- read.csv("C:/Ian_GIS/LakeCat/ImperviousSurfaces2011.csv")
Runoff <- read.csv("C:/Ian_GIS/LakeCat/Runoff.csv")
Baseflow <- read.csv("C:/Ian_GIS/LakeCat/BFI.csv")

#### D-fine constants ####
lake_sqkm_cutoff <- 0.01 #=1ha

############################## Main program ######################################
# remove lakes smaller than minimum threshold
protected_GAPS12 <- subset(protected_GAPS12, AREASQKM >= lake_sqkm_cutoff)
protected_GAPS123 <- subset(protected_GAPS123, AREASQKM >= lake_sqkm_cutoff)

unprotected_GAPS12 <- subset(unprotected_GAPS12, AREASQKM >= lake_sqkm_cutoff)
unprotected_GAPS123 <- subset(unprotected_GAPS123, AREASQKM >= lake_sqkm_cutoff)

# get COMIDs for lakes
protected_GAPS12_COMID <- protected_GAPS12@data$COMID
protected_GAPS123_COMID <- protected_GAPS123@data$COMID

unprotected_GAPS12_COMID <- unprotected_GAPS12@data$COMID
unprotected_GAPS123_COMID <- unprotected_GAPS123@data$COMID

# calculate total % GAP status 1-2 and 1-3 for catchments and watersheds
PADUS_LakeCat$PctGAP_Status12Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat
PADUS_LakeCat$PctGAP_Status123Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat + PADUS_LakeCat$PctGAP_Status3Cat

PADUS_LakeCat$PctGAP_Status12Ws <- PADUS_LakeCat$PctGAP_Status1Ws + PADUS_LakeCat$PctGAP_Status2Ws
PADUS_LakeCat$PctGAP_Status123Ws <- PADUS_LakeCat$PctGAP_Status1Ws + PADUS_LakeCat$PctGAP_Status2Ws + PADUS_LakeCat$PctGAP_Status3Ws

# get rid of unwanted columns
PADUS_LakeCat <- PADUS_LakeCat[,c('COMID','PctGAP_Status12Cat','PctGAP_Status12Ws','PctGAP_Status123Cat','PctGAP_Status123Ws')]

# join LakeCat tables to protected lakes
PADUS_protected_GAPS12 <- subset(PADUS_LakeCat, COMID %in% protected_GAPS12_COMID)
PADUS_protected_GAPS123 <- subset(PADUS_LakeCat, COMID %in% protected_GAPS123_COMID)
PADUS_unprotected_GAPS12 <- subset(PADUS_LakeCat, COMID %in% unprotected_GAPS12_COMID)
PADUS_unprotected_GAPS123 <- subset(PADUS_LakeCat, COMID %in% unprotected_GAPS123_COMID)

# Export shapefiles of % protected for mapping in ArcGIS
# PADUS_protected_GAPS12_export <- merge(protected_GAPS12, PADUS_protected_GAPS12, by='COMID', all.x=F)
# dsnname <- "C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts"
# layername <- "NHD_protect_pts_GAPS12_pct"
# writeOGR(PADUS_protected_GAPS12_export, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)
# 
# PADUS_protected_GAPS123_export <- merge(protected_GAPS123, PADUS_protected_GAPS123, by='COMID', all.x=F)
# dsnname <- "C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts"
# layername <- "NHD_protect_pts_GAPS123_pct"
# writeOGR(PADUS_protected_GAPS123_export, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)
# 
# PADUS_unprotected_GAPS12_export <- merge(unprotected_GAPS12, PADUS_unprotected_GAPS12, by='COMID', all.x=F)
# dsnname <- "C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts"
# layername <- "NHD_unprotect_pts_GAPS12_pct"
# writeOGR(PADUS_unprotected_GAPS12_export, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)
# 
# PADUS_unprotected_GAPS123_export <- merge(unprotected_GAPS123, PADUS_unprotected_GAPS123, by='COMID', all.x=F)
# dsnname <- "C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts"
# layername <- "NHD_unprotect_pts_GAPS123_pct"
# writeOGR(PADUS_unprotected_GAPS123_export, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)




### Basic plots: do lakes in protected areas have protected catchments and watersheds?
par(mfrow=c(2,2))
hist(PADUS_protected_GAPS12$PctGAP_Status12Cat, xlab='GAPS12, Cat', main='Protected lakes')
hist(PADUS_protected_GAPS12$PctGAP_Status12Ws, xlab='GAPS12, Ws', main='Protected lakes')
hist(PADUS_protected_GAPS123$PctGAP_Status123Cat, xlab='GAPS123, Cat', main='Protected lakes')
hist(PADUS_protected_GAPS123$PctGAP_Status123Ws, xlab='GAPS123, Ws', main='Protected lakes')

hist(PADUS_unprotected_GAPS12$PctGAP_Status12Cat, xlab='GAPS12, Cat', main='Unprotected lakes')
hist(PADUS_unprotected_GAPS12$PctGAP_Status12Ws, xlab='GAPS12, Ws', main='Unprotected lakes')
hist(PADUS_unprotected_GAPS123$PctGAP_Status123Cat, xlab='GAPS123, Cat', main='Unprotected lakes')
hist(PADUS_unprotected_GAPS123$PctGAP_Status123Ws, xlab='GAPS123, Ws', main='Unprotected lakes')

# # combine protected and unprotected lakes into single data frame
# PADUS_protected_GAPS12$Protected <- 'Protected'
# PADUS_unprotected_GAPS12$Protected <- 'Unprotected'
# PADUS_protected_GAPS123$Protected <- 'Protected'
# PADUS_unprotected_GAPS123$Protected <- 'Unprotected'
# 
# PADUS_GAPS12_ProUn_Cat <- rbind.data.frame(PADUS_protected_GAPS12, PADUS_unprotected_GAPS12)
# PADUS_GAPS12_ProUn_Cat <- melt(PADUS_GAPS12_ProUn_Cat, id.vars='Protected', measure.vars='PctGAP_Status12Cat')
# boxplot(value ~ Protected, PADUS_GAPS12_ProUn_Cat, main='GAPS12, Cat')
# 
# PADUS_GAPS12_ProUn_Ws <- rbind.data.frame(PADUS_protected_GAPS12, PADUS_unprotected_GAPS12)
# PADUS_GAPS12_ProUn_Ws <- melt(PADUS_GAPS12_ProUn_Ws, id.vars='Protected', measure.vars='PctGAP_Status12Ws')
# boxplot(value ~ Protected, PADUS_GAPS12_ProUn_Ws, main='GAPS12, Ws')
# 
# PADUS_GAPS123_ProUn_Cat <- rbind.data.frame(PADUS_protected_GAPS123, PADUS_unprotected_GAPS123)
# PADUS_GAPS123_ProUn_Cat <- melt(PADUS_GAPS123_ProUn_Cat, id.vars='Protected', measure.vars='PctGAP_Status123Cat')
# boxplot(value ~ Protected, PADUS_GAPS123_ProUn_Cat, main='GAPS123, Cat')
# 
# PADUS_GAPS123_ProUn_Ws <- rbind.data.frame(PADUS_protected_GAPS123, PADUS_unprotected_GAPS123)
# PADUS_GAPS123_ProUn_Ws <- melt(PADUS_GAPS123_ProUn_Ws, id.vars='Protected', measure.vars='PctGAP_Status123Ws')
# boxplot(value ~ Protected, PADUS_GAPS123_ProUn_Ws, main='GAPS123, Ws')

# or try with violin plots
# PERCENT PROTECTION
par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12$PctGAP_Status12Cat),na.omit(PADUS_unprotected_GAPS12$PctGAP_Status12Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('GAPS12, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12$PctGAP_Status12Ws),na.omit(PADUS_unprotected_GAPS12$PctGAP_Status12Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('GAPS12, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123$PctGAP_Status123Cat),na.omit(PADUS_unprotected_GAPS123$PctGAP_Status123Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('GAPS123, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123$PctGAP_Status123Ws),na.omit(PADUS_unprotected_GAPS123$PctGAP_Status123Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('GAPS123, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

### now see if protected vs. unprotected lakes have different characteristics
# calculate total LULC variables
# Land use/cover: calculate some new "total" variables
NLCD_2011$PctTotalForest2011Cat <- NLCD_2011$PctConif2011Cat + NLCD_2011$PctDecid2011Cat + NLCD_2011$PctMxFst2011Cat
NLCD_2011$PctTotalAg2011Cat <- NLCD_2011$PctCrop2011Cat + NLCD_2011$PctHay2011Cat
NLCD_2011$PctTotalWetland2011Cat <- NLCD_2011$PctWdWet2011Cat + NLCD_2011$PctHbWet2011Cat
NLCD_2011$PctTotalForest2011Ws <- NLCD_2011$PctConif2011Ws + NLCD_2011$PctDecid2011Ws + NLCD_2011$PctMxFst2011Ws
NLCD_2011$PctTotalAg2011Ws <- NLCD_2011$PctCrop2011Ws + NLCD_2011$PctHay2011Ws
NLCD_2011$PctTotalWetland2011Ws <- NLCD_2011$PctWdWet2011Ws + NLCD_2011$PctHbWet2011Ws

PADUS_protected_GAPS12_NLCD <- merge(PADUS_protected_GAPS12, NLCD_2011, by='COMID', all.x=F) 
PADUS_protected_GAPS123_NLCD <- merge(PADUS_protected_GAPS123, NLCD_2011, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_NLCD <- merge(PADUS_unprotected_GAPS12, NLCD_2011, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_NLCD <- merge(PADUS_unprotected_GAPS123, NLCD_2011, by='COMID', all.x=F)

# AGRICULTURE
par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_NLCD$PctTotalAg2011Cat),na.omit(PADUS_unprotected_GAPS12_NLCD$PctTotalAg2011Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('% Ag, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_NLCD$PctTotalAg2011Ws),na.omit(PADUS_unprotected_GAPS12_NLCD$PctTotalAg2011Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('% Ag, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_NLCD$PctTotalAg2011Cat),na.omit(PADUS_unprotected_GAPS123_NLCD$PctTotalAg2011Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('% Ag, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_NLCD$PctTotalAg2011Ws),na.omit(PADUS_unprotected_GAPS123_NLCD$PctTotalAg2011Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('% Ag, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# FOREST
par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_NLCD$PctTotalForest2011Cat),na.omit(PADUS_unprotected_GAPS12_NLCD$PctTotalForest2011Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('% Forest, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_NLCD$PctTotalForest2011Ws),na.omit(PADUS_unprotected_GAPS12_NLCD$PctTotalForest2011Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('% Forest, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_NLCD$PctTotalForest2011Cat),na.omit(PADUS_unprotected_GAPS123_NLCD$PctTotalForest2011Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('% Forest, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_NLCD$PctTotalForest2011Ws),na.omit(PADUS_unprotected_GAPS123_NLCD$PctTotalForest2011Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('% Forest, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# WETLAND
par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_NLCD$PctTotalWetland2011Cat),na.omit(PADUS_unprotected_GAPS12_NLCD$PctTotalWetland2011Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('% Wetland, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_NLCD$PctTotalWetland2011Ws),na.omit(PADUS_unprotected_GAPS12_NLCD$PctTotalWetland2011Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('% Wetland, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_NLCD$PctTotalWetland2011Cat),na.omit(PADUS_unprotected_GAPS123_NLCD$PctTotalWetland2011Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('% Wetland, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_NLCD$PctTotalWetland2011Ws),na.omit(PADUS_unprotected_GAPS123_NLCD$PctTotalWetland2011Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('% Wetland, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# ROAD DENSITY
PADUS_protected_GAPS12_roads <- merge(PADUS_protected_GAPS12, RoadDensity, by='COMID', all.x=F) 
PADUS_protected_GAPS123_roads <- merge(PADUS_protected_GAPS123, RoadDensity, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_roads <- merge(PADUS_unprotected_GAPS12, RoadDensity, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_roads <- merge(PADUS_unprotected_GAPS123, RoadDensity, by='COMID', all.x=F)

par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_roads$RdDensCat),na.omit(PADUS_unprotected_GAPS12_roads$RdDensCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Road density, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_roads$RdDensWs),na.omit(PADUS_unprotected_GAPS12_roads$RdDensWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Road density, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_roads$RdDensCat),na.omit(PADUS_unprotected_GAPS123_roads$RdDensCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Road density, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_roads$RdDensWs),na.omit(PADUS_unprotected_GAPS123_roads$RdDensWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Road density, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# IMPERVIOUS
PADUS_protected_GAPS12_imp <- merge(PADUS_protected_GAPS12, Impervious, by='COMID', all.x=F) 
PADUS_protected_GAPS123_imp <- merge(PADUS_protected_GAPS123, Impervious, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_imp <- merge(PADUS_unprotected_GAPS12, Impervious, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_imp <- merge(PADUS_unprotected_GAPS123, Impervious, by='COMID', all.x=F)

par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_imp$PctImp2011Cat),na.omit(PADUS_unprotected_GAPS12_imp$PctImp2011Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Impervious, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_imp$PctImp2011Ws),na.omit(PADUS_unprotected_GAPS12_imp$PctImp2011Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('Impervious, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_imp$PctImp2011Cat),na.omit(PADUS_unprotected_GAPS123_imp$PctImp2011Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Impervious, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_imp$PctImp2011Ws),na.omit(PADUS_unprotected_GAPS123_imp$PctImp2011Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('Impervious, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# ELEVATION
PADUS_protected_GAPS12_elev <- merge(PADUS_protected_GAPS12, elevation, by='COMID', all.x=F) 
PADUS_protected_GAPS123_elev <- merge(PADUS_protected_GAPS123, elevation, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_elev <- merge(PADUS_unprotected_GAPS12, elevation, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_elev <- merge(PADUS_unprotected_GAPS123, elevation, by='COMID', all.x=F)

par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_elev$ElevCat),na.omit(PADUS_unprotected_GAPS12_elev$ElevCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Elevation, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_elev$ElevWs),na.omit(PADUS_unprotected_GAPS12_elev$ElevWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Elevation, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_elev$ElevCat),na.omit(PADUS_unprotected_GAPS123_elev$ElevCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Elevation, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_elev$ElevWs),na.omit(PADUS_unprotected_GAPS123_elev$ElevWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Elevation, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# WetIndex    
PADUS_protected_GAPS12_WetIndex <- merge(PADUS_protected_GAPS12, WetIndex, by='COMID', all.x=F) 
PADUS_protected_GAPS123_WetIndex <- merge(PADUS_protected_GAPS123, WetIndex, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_WetIndex <- merge(PADUS_unprotected_GAPS12, WetIndex, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_WetIndex <- merge(PADUS_unprotected_GAPS123, WetIndex, by='COMID', all.x=F)

par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_WetIndex$WetIndexCat),na.omit(PADUS_unprotected_GAPS12_WetIndex$WetIndexCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('WetIndex, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_WetIndex$WetIndexWs),na.omit(PADUS_unprotected_GAPS12_WetIndex$WetIndexWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('WetIndex, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_WetIndex$WetIndexCat),na.omit(PADUS_unprotected_GAPS123_WetIndex$WetIndexCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('WetIndex, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_WetIndex$WetIndexWs),na.omit(PADUS_unprotected_GAPS123_WetIndex$WetIndexWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('WetIndex, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# FIRE
Fahr$TotalPctFireCat <- rowSums(Fahr[,7:17])
Fahr$TotalPctFireWs <- rowSums(Fahr[,18:28])
PADUS_protected_GAPS12_fahr <- merge(PADUS_protected_GAPS12, Fahr, by='COMID', all.x=F) 
PADUS_protected_GAPS123_fahr <- merge(PADUS_protected_GAPS123, Fahr, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_fahr <- merge(PADUS_unprotected_GAPS12, Fahr, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_fahr <- merge(PADUS_unprotected_GAPS123, Fahr, by='COMID', all.x=F)

par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_fahr$TotalPctFireCat),na.omit(PADUS_unprotected_GAPS12_fahr$TotalPctFireCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Fire, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_fahr$TotalPctFireWs),na.omit(PADUS_unprotected_GAPS12_fahr$TotalPctFireWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Fire, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_fahr$TotalPctFireCat),na.omit(PADUS_unprotected_GAPS123_fahr$TotalPctFireCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Fire, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_fahr$TotalPctFireWs),na.omit(PADUS_unprotected_GAPS123_fahr$TotalPctFireWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Fire, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# Forest Loss
ForestLoss$TotalPctFrstLossCat <- rowSums(ForestLoss[,7:19])
ForestLoss$TotalPctFrstLossWs <- rowSums(ForestLoss[,20:32])
PADUS_protected_GAPS12_forestloss <- merge(PADUS_protected_GAPS12, ForestLoss, by='COMID', all.x=F) 
PADUS_protected_GAPS123_forestloss <- merge(PADUS_protected_GAPS123, ForestLoss, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_forestloss <- merge(PADUS_unprotected_GAPS12, ForestLoss, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_forestloss <- merge(PADUS_unprotected_GAPS123, ForestLoss, by='COMID', all.x=F)

par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_forestloss$TotalPctFrstLossCat),na.omit(PADUS_unprotected_GAPS12_forestloss$TotalPctFrstLossCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('ForestLoss, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_forestloss$TotalPctFrstLossWs),na.omit(PADUS_unprotected_GAPS12_forestloss$TotalPctFrstLossWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('ForestLoss, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_forestloss$TotalPctFrstLossCat),na.omit(PADUS_unprotected_GAPS123_forestloss$TotalPctFrstLossCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('ForestLoss, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_forestloss$TotalPctFrstLossWs),na.omit(PADUS_unprotected_GAPS123_forestloss$TotalPctFrstLossWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('ForestLoss, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# PRECIPITATION
PADUS_protected_GAPS12_PRISM <- merge(PADUS_protected_GAPS12, PRISM, by='COMID', all.x=F) 
PADUS_protected_GAPS123_PRISM <- merge(PADUS_protected_GAPS123, PRISM, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_PRISM <- merge(PADUS_unprotected_GAPS12, PRISM, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_PRISM <- merge(PADUS_unprotected_GAPS123, PRISM, by='COMID', all.x=F)

par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_PRISM$Precip8110Cat),na.omit(PADUS_unprotected_GAPS12_PRISM$Precip8110Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Precip, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_PRISM$Precip8110Ws),na.omit(PADUS_unprotected_GAPS12_PRISM$Precip8110Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('Precip, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_PRISM$Precip8110Cat),na.omit(PADUS_unprotected_GAPS123_PRISM$Precip8110Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Precip, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_PRISM$Precip8110Ws),na.omit(PADUS_unprotected_GAPS123_PRISM$Precip8110Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('Precip, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# TEMPERATURE
par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_PRISM$Tmean8110Cat),na.omit(PADUS_unprotected_GAPS12_PRISM$Tmean8110Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Temp, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_PRISM$Tmean8110Ws),na.omit(PADUS_unprotected_GAPS12_PRISM$Tmean8110Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('Temp, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_PRISM$Tmean8110Cat),na.omit(PADUS_unprotected_GAPS123_PRISM$Tmean8110Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Temp, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_PRISM$Tmean8110Ws),na.omit(PADUS_unprotected_GAPS123_PRISM$Tmean8110Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('Temp, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# DAMS
PADUS_protected_GAPS12_dams <- merge(PADUS_protected_GAPS12, Dams, by='COMID', all.x=F) 
PADUS_protected_GAPS123_dams <- merge(PADUS_protected_GAPS123, Dams, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_dams <- merge(PADUS_unprotected_GAPS12, Dams, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_dams <- merge(PADUS_unprotected_GAPS123, Dams, by='COMID', all.x=F)

par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_dams$NABD_DensCat),na.omit(PADUS_unprotected_GAPS12_dams$NABD_DensCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Dams, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_dams$NABD_DensWs),na.omit(PADUS_unprotected_GAPS12_dams$NABD_DensWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Dams, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_dams$NABD_DensCat),na.omit(PADUS_unprotected_GAPS123_dams$NABD_DensCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Dams, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_dams$NABD_DensWs),na.omit(PADUS_unprotected_GAPS123_dams$NABD_DensWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Dams, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# MINES
PADUS_protected_GAPS12_mines <- merge(PADUS_protected_GAPS12, Mines, by='COMID', all.x=F) 
PADUS_protected_GAPS123_mines <- merge(PADUS_protected_GAPS123, Mines, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_mines <- merge(PADUS_unprotected_GAPS12, Mines, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_mines <- merge(PADUS_unprotected_GAPS123, Mines, by='COMID', all.x=F)

par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_mines$MineDensCat),na.omit(PADUS_unprotected_GAPS12_mines$MineDensCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Mines, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_mines$MineDensWs),na.omit(PADUS_unprotected_GAPS12_mines$MineDensWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Mines, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_mines$MineDensCat),na.omit(PADUS_unprotected_GAPS123_mines$MineDensCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Mines, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_mines$MineDensWs),na.omit(PADUS_unprotected_GAPS123_mines$MineDensWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Mines, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# SN Deposition
PADUS_protected_GAPS12_depos <- merge(PADUS_protected_GAPS12, Deposition, by='COMID', all.x=F) 
PADUS_protected_GAPS123_depos <- merge(PADUS_protected_GAPS123, Deposition, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_depos <- merge(PADUS_unprotected_GAPS12, Deposition, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_depos <- merge(PADUS_unprotected_GAPS123, Deposition, by='COMID', all.x=F)

par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_depos$SN_2008Cat),na.omit(PADUS_unprotected_GAPS12_depos$SN_2008Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Deposition, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_depos$SN_2008Ws),na.omit(PADUS_unprotected_GAPS12_depos$SN_2008Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('Deposition, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_depos$SN_2008Cat),na.omit(PADUS_unprotected_GAPS123_depos$SN_2008Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Deposition, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_depos$SN_2008Ws),na.omit(PADUS_unprotected_GAPS123_depos$SN_2008Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('Deposition, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# Runoff    
PADUS_protected_GAPS12_runoff <- merge(PADUS_protected_GAPS12, Runoff, by='COMID', all.x=F) 
PADUS_protected_GAPS123_runoff <- merge(PADUS_protected_GAPS123, Runoff, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_runoff <- merge(PADUS_unprotected_GAPS12, Runoff, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_runoff <- merge(PADUS_unprotected_GAPS123, Runoff, by='COMID', all.x=F)

par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_runoff$RunoffCat),na.omit(PADUS_unprotected_GAPS12_runoff$RunoffCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Runoff, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_runoff$RunoffWs),na.omit(PADUS_unprotected_GAPS12_runoff$RunoffWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Runoff, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_runoff$RunoffCat),na.omit(PADUS_unprotected_GAPS123_runoff$RunoffCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Runoff, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_runoff$RunoffWs),na.omit(PADUS_unprotected_GAPS123_runoff$RunoffWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Runoff, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# Baseflow    
PADUS_protected_GAPS12_baseflow <- merge(PADUS_protected_GAPS12, Baseflow, by='COMID', all.x=F) 
PADUS_protected_GAPS123_baseflow <- merge(PADUS_protected_GAPS123, Baseflow, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_baseflow <- merge(PADUS_unprotected_GAPS12, Baseflow, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_baseflow <- merge(PADUS_unprotected_GAPS123, Baseflow, by='COMID', all.x=F)

par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_baseflow$BFICat),na.omit(PADUS_unprotected_GAPS12_baseflow$BFICat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Baseflow, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_baseflow$BFIWs),na.omit(PADUS_unprotected_GAPS12_baseflow$BFIWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Baseflow, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_baseflow$BFICat),na.omit(PADUS_unprotected_GAPS123_baseflow$BFICat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Baseflow, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_baseflow$BFIWs),na.omit(PADUS_unprotected_GAPS123_baseflow$BFIWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Baseflow, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# Toxic    
PADUS_protected_GAPS12_Toxic <- merge(PADUS_protected_GAPS12, Toxic, by='COMID', all.x=F) 
PADUS_protected_GAPS123_Toxic <- merge(PADUS_protected_GAPS123, Toxic, by='COMID', all.x=F)
PADUS_unprotected_GAPS12_Toxic <- merge(PADUS_unprotected_GAPS12, Toxic, by='COMID', all.x=F) 
PADUS_unprotected_GAPS123_Toxic <- merge(PADUS_unprotected_GAPS123, Toxic, by='COMID', all.x=F)

# Superfund sites
par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_Toxic$SuperfundDensCat),na.omit(PADUS_unprotected_GAPS12_Toxic$SuperfundDensCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Superfund, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_Toxic$SuperfundDensWs),na.omit(PADUS_unprotected_GAPS12_Toxic$SuperfundDensWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Superfund, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_Toxic$SuperfundDensCat),na.omit(PADUS_unprotected_GAPS123_Toxic$SuperfundDensCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('Superfund, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_Toxic$SuperfundDensWs),na.omit(PADUS_unprotected_GAPS123_Toxic$SuperfundDensWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('Superfund, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# TRI sites
par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_Toxic$TRIDensCat),na.omit(PADUS_unprotected_GAPS12_Toxic$TRIDensCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('TRI sites, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_Toxic$TRIDensWs),na.omit(PADUS_unprotected_GAPS12_Toxic$TRIDensWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('TRI sites, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_Toxic$TRIDensCat),na.omit(PADUS_unprotected_GAPS123_Toxic$TRIDensCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('TRI sites, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_Toxic$TRIDensWs),na.omit(PADUS_unprotected_GAPS123_Toxic$TRIDensWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('TRI sites, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# NPDES sites
par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_Toxic$NPDESDensCat),na.omit(PADUS_unprotected_GAPS12_Toxic$NPDESDensCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('NPDES sites, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_Toxic$NPDESDensWs),na.omit(PADUS_unprotected_GAPS12_Toxic$NPDESDensWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('NPDES sites, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_Toxic$NPDESDensCat),na.omit(PADUS_unprotected_GAPS123_Toxic$NPDESDensCat), 
        names=c('Protected','Unprotected'), col='gray50')
title('NPDES sites, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_Toxic$NPDESDensWs),na.omit(PADUS_unprotected_GAPS123_Toxic$NPDESDensWs), 
        names=c('Protected','Unprotected'), col='gray50')
title('NPDES sites, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# Catchment, watershed area
par(mfrow=c(2,2))
vioplot(na.omit(PADUS_protected_GAPS12_Toxic$CatAreaSqKm),na.omit(PADUS_unprotected_GAPS12_Toxic$CatAreaSqKm), 
        names=c('Protected','Unprotected'), col='gray50')
title('Area, Cat')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS12_Toxic$WsAreaSqKm),na.omit(PADUS_unprotected_GAPS12_Toxic$WsAreaSqKm), 
        names=c('Protected','Unprotected'), col='gray50')
title('Area, Ws')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_Toxic$CatAreaSqKm),na.omit(PADUS_unprotected_GAPS123_Toxic$CatAreaSqKm), 
        names=c('Protected','Unprotected'), col='gray50')
title('Area, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123_Toxic$WsAreaSqKm),na.omit(PADUS_unprotected_GAPS123_Toxic$WsAreaSqKm), 
        names=c('Protected','Unprotected'), col='gray50')
title('Area, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# LAKE AREA
par(mfrow=c(2,2))
vioplot(na.omit(protected_GAPS12@data$AREASQKM),na.omit(unprotected_GAPS12@data$AREASQKM), 
        names=c('Protected','Unprotected'), col='gray50')
title('Lake Area')
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(protected_GAPS123@data$AREASQKM),na.omit(unprotected_GAPS123@data$AREASQKM), 
        names=c('Protected','Unprotected'), col='gray50')
title('Lake Area')
mtext(side=3, 'Multi-use', cex=0.75)
