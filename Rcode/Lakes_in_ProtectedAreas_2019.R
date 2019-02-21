####################### Characteristics of lakes in US protected areas #########################
# Date: 2-12-19
# updated: 2-14-19
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(reshape2)
library(vioplot)
library(rgdal)
library(gridExtra)

#### input data ####
setwd("C:/Users/FWL/Documents/FreshwaterConservation")

# lower 48 states
lower48 <- shapefile("C:/Ian_GIS/cb_2016_us_state_500k/lower48.shp") #same crs as NHD_pts

#### Protected and unprotected lakes (centroids) ####
protected_GAPS12 <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/NHD_protect_pts_GAPS12.shp")
protected_GAPS123 <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/NHD_protect_pts_GAPS123.shp")
protected_GAP3only <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/NHD_protect_pts_GAP3only.shp")

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
protected_GAP3only <- subset(protected_GAP3only, AREASQKM >= lake_sqkm_cutoff)

unprotected_GAPS12 <- subset(unprotected_GAPS12, AREASQKM >= lake_sqkm_cutoff)
unprotected_GAPS123 <- subset(unprotected_GAPS123, AREASQKM >= lake_sqkm_cutoff)

# get COMIDs for lakes
protected_GAPS12_COMID <- protected_GAPS12@data$COMID
protected_GAPS123_COMID <- protected_GAPS123@data$COMID
protected_GAP3only_COMID <- protected_GAP3only$COMID

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

## What proportion of lakes is protected (simply falls within a protected area)?
nrow(PADUS_protected_GAPS12)/(nrow(PADUS_protected_GAPS12) + nrow(PADUS_unprotected_GAPS12))
nrow(PADUS_protected_GAPS123)/(nrow(PADUS_protected_GAPS123) + nrow(PADUS_unprotected_GAPS123))

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
hist_colors <- c('firebrick','darksalmon','moccasin','lightskyblue','dodgerblue4')
hist(PADUS_protected_GAPS12$PctGAP_Status12Cat, xlab='% protected', main='Protected lakes',breaks=seq(0,100,20),
     col=hist_colors, ylim=c(0,40000))
mtext(side=3, 'Strict, Cat', cex=0.75)
hist(PADUS_protected_GAPS12$PctGAP_Status12Ws, xlab='% protected', main='Protected lakes',breaks=seq(0,100,20),
     col=hist_colors, ylim=c(0,40000))
mtext(side=3, 'Strict, Ws', cex=0.75)
hist(PADUS_protected_GAPS123$PctGAP_Status123Cat, xlab='% protected', main='Protected lakes',breaks=seq(0,100,20),
     col=hist_colors, ylim=c(0,40000))
mtext(side=3, 'Multi-use, Cat', cex=0.75)
hist(PADUS_protected_GAPS123$PctGAP_Status123Ws, xlab='% protected', main='Protected lakes',breaks=seq(0,100,20),
     col=hist_colors, ylim=c(0,40000))
mtext(side=3, 'Multi-use, Ws', cex=0.75)

hist(PADUS_unprotected_GAPS12$PctGAP_Status12Cat, xlab='GAPS12, Cat', main='Unprotected lakes',breaks=seq(0,100,20))
hist(PADUS_unprotected_GAPS12$PctGAP_Status12Ws, xlab='GAPS12, Ws', main='Unprotected lakes',breaks=seq(0,100,20))
hist(PADUS_unprotected_GAPS123$PctGAP_Status123Cat, xlab='GAPS123, Cat', main='Unprotected lakes',breaks=seq(0,100,20))
hist(PADUS_unprotected_GAPS123$PctGAP_Status123Ws, xlab='GAPS123, Ws', main='Unprotected lakes',breaks=seq(0,100,20))

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
mtext(side=3, 'Strict', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123$PctGAP_Status123Cat),na.omit(PADUS_unprotected_GAPS123$PctGAP_Status123Cat), 
        names=c('Protected','Unprotected'), col='gray50')
title('GAPS123, Cat')
mtext(side=3, 'Multi-use', cex=0.75)

vioplot(na.omit(PADUS_protected_GAPS123$PctGAP_Status123Ws),na.omit(PADUS_unprotected_GAPS123$PctGAP_Status123Ws), 
        names=c('Protected','Unprotected'), col='gray50')
title('GAPS123, Ws')
mtext(side=3, 'Multi-use', cex=0.75)

# lakes that fall in strictly protected areas
par(mfrow=c(1,1))
vioplot(na.omit(PADUS_protected_GAPS12$PctGAP_Status12Cat),na.omit(PADUS_protected_GAPS12$PctGAP_Status123Cat), 
        names=c('Strict','Multi-use'), col='gray50')
title('GAPS12, Cat')
mtext(side=3, 'Strict', cex=0.75)

# lakes that fall in multi-use areas (need to remove strict?)


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

######## Experimenting with multi-panel/fancier violin/box plotz ####
library(ggpubr)
plot_colorz <- c('forestgreen','tan','gray40')

# LAKE AREA
temp_a <- protected_GAPS12@data
temp_a$Protection <- 'Strict'
temp_b <- protected_GAPS123@data
temp_b$Protection <- 'Multi-use'
temp_b <- subset(temp_b, !(COMID %in% temp_a$COMID)) #only keep COMID not in strict protection
temp_c <- unprotected_GAPS123@data
temp_c$Protection <- 'Unprotected'

temp_df <- rbind.data.frame(temp_a, temp_b, temp_c)
temp_df <- temp_df[!duplicated(temp_df$COMID), ] #in case there are a few duplicates...not sure why there would be, but there are a few
temp_df_melted <- temp_df[,c('AREASQKM','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])

letang <- ggplot(temp_df_melted, aes(x=Protection, y=log(value), fill=Protection)) + 
  geom_violin() + 
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Lake area",x="", y = "")

# par(mfrow=c(1,1))
# temp_df$Protection <- factor(temp_df$Protection, levels=c("Strict", "Multi-use", "Unprotected")) #reorder boxes
# boxplot(AREASQKM ~ Protection, data=temp_df, las=1, main='Lake area (sq km)', col=plot_colorz, ylim=c(0,5))
# 
# 
# area_boxplot <- ggboxplot(temp_df, x = "Protection",
#          y = c("AREASQKM"),
#          combine = T, 
#          color = "Protection", palette = "jco",
#          ylab = "Sq km", 
#          add = "median_iqr")
# 
# ggpar(area_boxplot, legend='none', ylim=c(0,1))

# CLIMATE
temp_a <- na.omit(PADUS_protected_GAPS12_PRISM)
temp_a$Protection <- 'Strict'
temp_b <- na.omit(PADUS_protected_GAPS123_PRISM)
temp_b$Protection <- 'Multi-use'
temp_b <- subset(temp_b, !(COMID %in% temp_a$COMID)) #only keep COMID not in strict protection
temp_c <- na.omit(PADUS_unprotected_GAPS123_PRISM)
temp_c$Protection <- 'Unprotected'

temp_df <- rbind.data.frame(temp_a, temp_b, temp_c)
temp_df <- temp_df[!duplicated(temp_df$COMID), ] #in case there are a few duplicates...not sure why there would be, but there are a few

# http://www.sthda.com/english/wiki/print.php?id=132
#library(devtools)
#install_github("kassambara/easyGgplot2")
#library(easyGgplot2)
temp_df_melted <- temp_df[,c('Precip8110Cat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])

dupuis <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() + 
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Precipitation",x="", y = "")

temp_df_melted <- temp_df[,c('Tmean8110Cat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])
crosby <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Temperature",x="", y = "")

grid.arrange(dupuis, crosby, nrow=1)

# LULC
temp_a <- PADUS_protected_GAPS12_NLCD
temp_a$Protection <- 'Strict'
temp_b <- PADUS_protected_GAPS123_NLCD
temp_b$Protection <- 'Multi-use'
temp_b <- subset(temp_b, !(COMID %in% temp_a$COMID)) #only keep COMID not in strict protection
temp_c <- PADUS_unprotected_GAPS123_NLCD
temp_c$Protection <- 'Unprotected'

temp_df <- rbind.data.frame(temp_a, temp_b, temp_c)
temp_df <- temp_df[!duplicated(temp_df$COMID), ] #in case there are a few duplicates...not sure why there would be, but there are a few

ploht <- ggboxplot(temp_df, x = "Protection",
                   y = c("PctTotalForest2011Cat", "PctTotalAg2011Cat", "PctTotalWetland2011Cat"),
                   combine = T,
                   ylab = "Percent",
                   color = "Protection", palette = plot_colorz, xlab=F)
ggpar(ploht, legend='none')

temp_df_melted <- temp_df[,c('PctTotalForest2011Cat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])
malkin <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Forest",x="", y = "")

temp_df_melted <- temp_df[,c('PctTotalAg2011Cat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])
kunitz <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Agriculture",x="", y = "")

temp_df_melted <- temp_df[,c('PctTotalWetland2011Cat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])
fleury <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Wetlands",x="", y = "")

grid.arrange(malkin, kunitz, fleury, nrow=1)

# Toxic stuff
temp_a <- PADUS_protected_GAPS12_Toxic
temp_a$Protection <- 'Strict'
temp_b <- PADUS_protected_GAPS123_Toxic
temp_b$Protection <- 'Multi-use'
temp_b <- subset(temp_b, !(COMID %in% temp_a$COMID)) #only keep COMID not in strict protection
temp_c <- PADUS_unprotected_GAPS123_Toxic
temp_c$Protection <- 'Unprotected'

temp_df <- rbind.data.frame(temp_a, temp_b, temp_c)
temp_df <- temp_df[!duplicated(temp_df$COMID), ] #in case there are a few duplicates...not sure why there would be, but there are a few

temp_df_melted <- temp_df[,c('SuperfundDensCat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])
dumo <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Superfund",x="", y = "")

temp_df_melted <- temp_df[,c('TRIDensCat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])
dumo <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="TRI",x="", y = "")

# Mines
temp_a <- PADUS_protected_GAPS12_mines
temp_a$Protection <- 'Strict'
temp_b <- PADUS_protected_GAPS123_mines
temp_b$Protection <- 'Multi-use'
temp_b <- subset(temp_b, !(COMID %in% temp_a$COMID)) #only keep COMID not in strict protection
temp_c <- PADUS_unprotected_GAPS123_mines
temp_c$Protection <- 'Unprotected'

temp_df <- rbind.data.frame(temp_a, temp_b, temp_c)
temp_df <- temp_df[!duplicated(temp_df$COMID), ] #in case there are a few duplicates...not sure why there would be, but there are a few

temp_df_melted <- temp_df[,c('MineDensCat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])
murray <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Mines",x="", y = "")+
  scale_y_continuous(limits=c(0,0.5))

# Road density
temp_a <- PADUS_protected_GAPS12_roads
temp_a$Protection <- 'Strict'
temp_b <- PADUS_protected_GAPS123_roads
temp_b$Protection <- 'Multi-use'
temp_b <- subset(temp_b, !(COMID %in% temp_a$COMID)) #only keep COMID not in strict protection
temp_c <- PADUS_unprotected_GAPS123_roads
temp_c$Protection <- 'Unprotected'

temp_df <- rbind.data.frame(temp_a, temp_b, temp_c)
temp_df <- temp_df[!duplicated(temp_df$COMID), ] #in case there are a few duplicates...not sure why there would be, but there are a few

temp_df_melted <- temp_df[,c('RdDensCat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])
hedberg <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Rd Density",x="", y = "")

# Deposition
temp_a <- PADUS_protected_GAPS12_depos
temp_a$Protection <- 'Strict'
temp_b <- PADUS_protected_GAPS123_depos
temp_b$Protection <- 'Multi-use'
temp_b <- subset(temp_b, !(COMID %in% temp_a$COMID)) #only keep COMID not in strict protection
temp_c <- PADUS_unprotected_GAPS123_depos
temp_c$Protection <- 'Unprotected'

temp_df <- rbind.data.frame(temp_a, temp_b, temp_c)
temp_df <- temp_df[!duplicated(temp_df$COMID), ] #in case there are a few duplicates...not sure why there would be, but there are a few

temp_df_melted <- temp_df[,c('SN_2008Cat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])
lacouture <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="SN deposition",x="", y = "")

# Impervious
temp_a <- PADUS_protected_GAPS12_imp
temp_a$Protection <- 'Strict'
temp_b <- PADUS_protected_GAPS123_imp
temp_b$Protection <- 'Multi-use'
temp_b <- subset(temp_b, !(COMID %in% temp_a$COMID)) #only keep COMID not in strict protection
temp_c <- PADUS_unprotected_GAPS123_imp
temp_c$Protection <- 'Unprotected'

temp_df <- rbind.data.frame(temp_a, temp_b, temp_c)
temp_df <- temp_df[!duplicated(temp_df$COMID), ] #in case there are a few duplicates...not sure why there would be, but there are a few

temp_df_melted <- temp_df[,c('PctImp2011Cat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])
lange <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Impervious",x="", y = "")+
  scale_y_continuous(limits=c(0,10))

# Dams
temp_a <- PADUS_protected_GAPS12_dams
temp_a$Protection <- 'Strict'
temp_b <- PADUS_protected_GAPS123_dams
temp_b$Protection <- 'Multi-use'
temp_b <- subset(temp_b, !(COMID %in% temp_a$COMID)) #only keep COMID not in strict protection
temp_c <- PADUS_unprotected_GAPS123_dams
temp_c$Protection <- 'Unprotected'

temp_df <- rbind.data.frame(temp_a, temp_b, temp_c)
temp_df <- temp_df[!duplicated(temp_df$COMID), ] #in case there are a few duplicates...not sure why there would be, but there are a few

temp_df_melted <- temp_df[,c('NABD_DensCat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])
langlang <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Dams",x="", y = "")+
  scale_y_continuous(limits=c(0,0.01))

# Forest loss
temp_a <- PADUS_protected_GAPS12_forestloss
temp_a$Protection <- 'Strict'
temp_b <- PADUS_protected_GAPS123_forestloss
temp_b$Protection <- 'Multi-use'
temp_b <- subset(temp_b, !(COMID %in% temp_a$COMID)) #only keep COMID not in strict protection
temp_c <- PADUS_unprotected_GAPS123_forestloss
temp_c$Protection <- 'Unprotected'

temp_df <- rbind.data.frame(temp_a, temp_b, temp_c)
temp_df <- temp_df[!duplicated(temp_df$COMID), ] #in case there are a few duplicates...not sure why there would be, but there are a few

temp_df_melted <- temp_df[,c('TotalPctFrstLossCat','Protection')]
temp_df_melted <- melt(temp_df_melted, id.vars='Protection')
temp_df_melted$Protection <- as.factor(temp_df_melted$Protection)
temp_df_melted$Protection <- factor(temp_df_melted$Protection,levels(temp_df_melted$Protection)[c(2,1,3)])
potash <- ggplot(temp_df_melted, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Forest loss",x="", y = "")+
  scale_y_continuous(limits=c(0,0.1))



# too slow
# ggdensity(temp_df,
#           x = c("PctTotalForest2011Cat", "PctTotalAg2011Cat", "PctTotalWetland2011Cat"),
#           y = "..density..",
#           combine = T,                  # Combine the 3 plots
#           xlab = "Percent", 
#           add = "median",                  # Add median line. 
#           rug = T,                      # Add marginal rug
#           color = "Protection", 
#           fill = "Protection",
#           palette = c('forestgreen','tan','gray40'))

