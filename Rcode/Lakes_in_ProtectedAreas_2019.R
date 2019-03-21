####################### Characteristics of lakes in US protected areas #########################
# Date: 2-12-19
# updated: 3-7-19
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(reshape2)
library(vioplot)
library(rgdal)
library(gridExtra)
library(dplyr)
library(ggplot2)

#### input data ####
setwd("C:/Users/FWL/Documents/FreshwaterConservation")

# lower 48 states
lower48 <- shapefile("Data/lower48/lower48.shp") #same crs as NHD_pts

## Protected lakes (centroids)
protected_GAPS12 <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/Arcgis/NHD_pts_GAPS12_ArcGIS_select.shp")
protected_GAP3only <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/Arcgis/NHD_pts_GAP3_only_ArcGIS_select.shp")

## Other lakes
# NHD waterbodies (converted to points in ArcGIS) (NHDPlusV2 National dataset; downloaded November 2018)
NHD_pts <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_waterbody_pts_LakePondReservoir.shp")

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

# LAGOS conn class data
LAGOSconn <- read.csv("Data/LakeConn_1ha_LAGOS_NHD_Xwalk.csv")

#### D-fine constants ####
lake_sqkm_cutoff <- 0.01 #=1ha

############################## Main program ######################################
## calculate total % GAP status 1-2 and 1-3 for catchments and watersheds
PADUS_LakeCat$PctGAP_Status12Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat
PADUS_LakeCat$PctGAP_Status123Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat + PADUS_LakeCat$PctGAP_Status3Cat

PADUS_LakeCat$PctGAP_Status12Ws <- PADUS_LakeCat$PctGAP_Status1Ws + PADUS_LakeCat$PctGAP_Status2Ws
PADUS_LakeCat$PctGAP_Status123Ws <- PADUS_LakeCat$PctGAP_Status1Ws + PADUS_LakeCat$PctGAP_Status2Ws + PADUS_LakeCat$PctGAP_Status3Ws

# get rid of unwanted columns
PADUS_LakeCat <- PADUS_LakeCat[,c('COMID','PctGAP_Status12Cat','PctGAP_Status12Ws','PctGAP_Status123Cat','PctGAP_Status123Ws','PctGAP_Status3Cat','PctGAP_Status3Ws')]

# how correlated is catchment protection with watershed protection?
cor(PADUS_LakeCat$PctGAP_Status12Cat, PADUS_LakeCat$PctGAP_Status12Ws, method='pearson', use='pairwise.complete.obs')
cor(PADUS_LakeCat$PctGAP_Status3Cat, PADUS_LakeCat$PctGAP_Status3Ws, method='pearson', use='pairwise.complete.obs')

## Basic subsetting and joining PADUS data to lakes
# Strictly protected lakes
protected_GAPS12_df <- subset(protected_GAPS12@data, AREASQKM >= lake_sqkm_cutoff) #remove too small lakes
protected_GAPS12_df <- protected_GAPS12_df[!duplicated(protected_GAPS12_df$COMID),] #remove duplicate COMIDs
protected_GAPS12_df_PADUS <- merge(protected_GAPS12_df, PADUS_LakeCat, by='COMID', all.y=F) #join to PADUS table

# Multi-use protected lakes
protected_GAP3only_df <- subset(protected_GAP3only@data, AREASQKM >= lake_sqkm_cutoff) #remove too small lakes
protected_GAP3only_df <- protected_GAP3only_df[!duplicated(protected_GAP3only_df$COMID),] #remove duplicate COMIDs
protected_GAP3only_df_PADUS <- merge(protected_GAP3only_df, PADUS_LakeCat, by='COMID', all.y=F) #join to PADUS table

# get rid of lakes counted as both strict and multi (treat as strict); issues with overlapping polygons in PADUS
double_lakes <- intersect(protected_GAPS12_df_PADUS$COMID, protected_GAP3only_df_PADUS$COMID)
protected_GAP3only_df_PADUS <- subset(protected_GAP3only_df_PADUS, !(COMID %in% double_lakes)) #take out lakes that occur in both

# Unprotected lakes: all other lakes in LakeCat
unprotected_df <- merge(PADUS_LakeCat, NHD_pts, by='COMID', all=F) #merge to NHD to get lake area column
unprotected_df <- subset(unprotected_df, AREASQKM >= lake_sqkm_cutoff) #remove too small lakes
unprotected_df <- unprotected_df[!duplicated(unprotected_df$COMID),] #remove duplicate COMIDs

unprotected_df <- subset(unprotected_df, !(COMID %in% protected_GAPS12_df_PADUS$COMID))#remove strictly protected lakes
unprotected_df <- subset(unprotected_df, !(COMID %in% protected_GAP3only_df_PADUS$COMID))#remove multi-use lakes

# save unprotected COMIDs
write.csv(unprotected_df$COMID, "Data/unprotected_COMID.csv")

## What proportion of lakes is protected/unprotected (simply falls within a protected area)?
total_n_lakes <- nrow(unprotected_df) + nrow(protected_GAP3only_df_PADUS) + nrow(protected_GAPS12_df_PADUS)
nrow(protected_GAPS12_df_PADUS)/total_n_lakes
nrow(protected_GAP3only_df_PADUS)/total_n_lakes
nrow(unprotected_df)/total_n_lakes

# # # Export shapefiles of % protected for mapping in ArcGIS (first merge back to NHD pts)
# PADUS_protected_GAPS12_export <- merge(NHD_pts, protected_GAPS12_df_PADUS, by='COMID', all.x=F)
# dsnname <- "C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts"
# layername <- "NHD_protect_pts_GAPS12_pct"
# writeOGR(PADUS_protected_GAPS12_export, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)
# # # #
# PADUS_protected_GAP3only_export <- merge(NHD_pts, protected_GAP3only_df_PADUS, by='COMID', all.x=F)
# dsnname <- "C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts"
# layername <- "NHD_protect_pts_GAP3only_pct"
# writeOGR(PADUS_protected_GAP3only_export, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)
# # # #
# PADUS_unprotected_export <- merge(NHD_pts, unprotected_df, by='COMID', all.x=F)
# dsnname <- "C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts"
# layername <- "NHD_unprotected_pts"
# writeOGR(PADUS_unprotected_export, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

### Basic plots: do lakes in protected areas have protected catchments and watersheds?
png("Figures/protected_lakes_histogram.png", width = 7,height = 5,units = 'in',res=300)
  par(mfrow=c(2,2))
  # PLOT A
  par(mar=c(2,3,4,0.5)) #bot,left,top,right
  hist_colors <- c('firebrick','darksalmon','moccasin','lightskyblue','dodgerblue4')
  hist(protected_GAPS12_df_PADUS$PctGAP_Status12Cat, xlab='', main='A) Strict, catchment',breaks=seq(0,100,20),
      col=hist_colors, ylim=c(0,20000))
  title(ylab='Frequency', line=2.2)
  mtext(side=3, paste0(nrow(protected_GAPS12_df_PADUS), ' lakes'), cex=0.75, line=0.5) #line= adjusts position 
  # PLOT B
  par(mar=c(2,2,4,1.5)) #bot,left,top,right
  hist(protected_GAPS12_df_PADUS$PctGAP_Status12Ws, xlab='', main='B) Strict, watershed',breaks=seq(0,100,20),
      col=hist_colors, ylim=c(0,20000), ylab='')
  mtext(side=3, paste0(nrow(protected_GAPS12_df_PADUS), ' lakes'), cex=0.75, line=0.5) #line= adjusts position 
  # PLOT C
  par(mar=c(3,3,3,0.5)) #bot,left,top,right
  hist(protected_GAP3only_df_PADUS$PctGAP_Status3Cat, xlab='% protected', main='C) Multi-use, catchment',breaks=seq(0,100,20),
      col=hist_colors, ylim=c(0,20000))
  title(xlab='% protected', line=2)
  title(ylab='Frequency', line=2.2)
  mtext(side=3, paste0(nrow(protected_GAP3only_df_PADUS), ' lakes'), cex=0.75)
  # PLOT D
  par(mar=c(3,2,3,1.5)) #bot,left,top,right
  hist(protected_GAP3only_df_PADUS$PctGAP_Status3Ws, xlab='% protected', main='D) Multi-use, watershed',breaks=seq(0,100,20),
      col=hist_colors, ylim=c(0,20000), ylab='')
  title(xlab='% protected', line=2)
  mtext(side=3, paste0(nrow(protected_GAP3only_df_PADUS), ' lakes'), cex=0.75)
dev.off()

# What proportion of lakes that occur in protected areas has X amount of catchment/watershed protection?
summary(protected_GAPS12_df_PADUS)
nrow(subset(protected_GAPS12_df_PADUS, PctGAP_Status12Cat < 20))/nrow(protected_GAPS12_df_PADUS)
nrow(subset(protected_GAPS12_df_PADUS, PctGAP_Status12Ws < 20))/nrow(protected_GAPS12_df_PADUS)
nrow(subset(protected_GAPS12_df_PADUS, PctGAP_Status12Cat > 80))/nrow(protected_GAPS12_df_PADUS)
nrow(subset(protected_GAPS12_df_PADUS, PctGAP_Status12Ws > 80))/nrow(protected_GAPS12_df_PADUS)

summary(protected_GAP3only_df_PADUS)
nrow(subset(protected_GAP3only_df_PADUS, PctGAP_Status3Cat < 20))/nrow(protected_GAP3only_df_PADUS)
nrow(subset(protected_GAP3only_df_PADUS, PctGAP_Status3Ws < 20))/nrow(protected_GAP3only_df_PADUS)
nrow(subset(protected_GAP3only_df_PADUS, PctGAP_Status3Cat > 80))/nrow(protected_GAP3only_df_PADUS)
nrow(subset(protected_GAP3only_df_PADUS, PctGAP_Status3Ws > 80))/nrow(protected_GAP3only_df_PADUS)

# Create subset of lakes with fully protected catchments/watersheds
protected_GAPS12_df_PADUS_100pct <- subset(protected_GAPS12_df_PADUS, PctGAP_Status12Cat >= 100)
protected_GAP3only_df_PADUS_100pct <- subset(protected_GAP3only_df_PADUS, PctGAP_Status3Cat >= 100)
nrow(subset(protected_GAPS12_df_PADUS, PctGAP_Status12Ws >= 100))
nrow(subset(protected_GAP3only_df_PADUS, PctGAP_Status3Ws >= 100))

# What proportion of lakes have fully protected catchments/watersheds?
nrow(protected_GAPS12_df_PADUS_100pct)/total_n_lakes
nrow(protected_GAP3only_df_PADUS_100pct)/total_n_lakes
nrow(subset(protected_GAPS12_df_PADUS, PctGAP_Status12Ws >= 100))/total_n_lakes
nrow(subset(protected_GAP3only_df_PADUS, PctGAP_Status3Ws >= 100))/total_n_lakes

##  Violin plot of protection across all US lakes
all_lake_COMID <- c(unprotected_df$COMID, protected_GAPS12_df_PADUS$COMID, protected_GAP3only_df_PADUS$COMID) 
violin_df <- subset(PADUS_LakeCat, COMID %in% all_lake_COMID)
violin_df[is.na(violin_df)] <- 0 #convert NAs to 0; treat NA protection as 0 protection

v1 <- violin_df$PctGAP_Status12Cat
v2 <- violin_df$PctGAP_Status3Cat
v3 <- violin_df$PctGAP_Status12Ws
v4 <- violin_df$PctGAP_Status3Ws

png("Figures/violin_pct_protected.png", width = 7,height = 5,units = 'in',res=300)
  par(mfrow=c(1,1))
  par(las=1,bty="l")  ## my preferred setting
  par(mar=c(3,4,2,0.5)) #bot,left,top,right
  plot(0.5:5,0.5:5,type="n",ylim=c(0,100),
      axes=FALSE,ann=FALSE)
  vioplot(v1,v2,v3,v4,add=T, col='gray70')
  axis(side=1,at=1:4,labels=c('Strict, Cat', 'Multi-use, Cat', 'Strict, Ws', 'Multi-use, Ws'))
  axis(side=2,at=seq(0,100,10),labels=seq(0,100,10))
  title(ylab='Percent protected', line=2.2)
dev.off()

# how many lakes have 0 protection?
length(subset(v1, v1 <1 ))/length(v1) #strict cat
length(subset(v2, v2 <1 ))/length(v2) #multi use cat
length(subset(v3, v3 <1 ))/length(v3) #strict ws
length(subset(v4, v1 <1 ))/length(v4) # multi-use ws

### now see if protected vs. unprotected lakes have different characteristics
# calculate some total/new variables
# LULC
NLCD_2011$PctTotalForest2011Cat <- NLCD_2011$PctConif2011Cat + NLCD_2011$PctDecid2011Cat + NLCD_2011$PctMxFst2011Cat
NLCD_2011$PctTotalAg2011Cat <- NLCD_2011$PctCrop2011Cat + NLCD_2011$PctHay2011Cat
NLCD_2011$PctTotalWetland2011Cat <- NLCD_2011$PctWdWet2011Cat + NLCD_2011$PctHbWet2011Cat
NLCD_2011$PctTotalForest2011Ws <- NLCD_2011$PctConif2011Ws + NLCD_2011$PctDecid2011Ws + NLCD_2011$PctMxFst2011Ws
NLCD_2011$PctTotalAg2011Ws <- NLCD_2011$PctCrop2011Ws + NLCD_2011$PctHay2011Ws
NLCD_2011$PctTotalWetland2011Ws <- NLCD_2011$PctWdWet2011Ws + NLCD_2011$PctHbWet2011Ws
# Fire
Fahr$TotalPctFireCat <- rowSums(Fahr[,7:17])
Fahr$TotalPctFireWs <- rowSums(Fahr[,18:28])
# Forest Loss
ForestLoss$TotalPctFrstLossCat <- rowSums(ForestLoss[,7:19])
ForestLoss$TotalPctFrstLossWs <- rowSums(ForestLoss[,20:32])

## Create new merged tables for plotting
# LULC
protected_GAPS12_df_PADUS_NLCD <- merge(protected_GAPS12_df_PADUS, NLCD_2011, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_NLCD <- merge(protected_GAP3only_df_PADUS, NLCD_2011, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_NLCD <- merge(protected_GAPS12_df_PADUS_100pct, NLCD_2011, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_NLCD <- merge(protected_GAP3only_df_PADUS_100pct, NLCD_2011, by='COMID', all.x=F)
unprotected_df_NLCD <- merge(unprotected_df, NLCD_2011, by='COMID', all.x=F)

# Road density
protected_GAPS12_df_PADUS_roads <- merge(protected_GAPS12_df_PADUS, RoadDensity, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_roads <- merge(protected_GAP3only_df_PADUS, RoadDensity, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_roads <- merge(protected_GAPS12_df_PADUS_100pct, RoadDensity, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_roads <- merge(protected_GAP3only_df_PADUS_100pct, RoadDensity, by='COMID', all.x=F)
unprotected_df_roads <- merge(unprotected_df, RoadDensity, by='COMID', all.x=F)

# Impervious
protected_GAPS12_df_PADUS_imp <- merge(protected_GAPS12_df_PADUS, Impervious, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_imp <- merge(protected_GAP3only_df_PADUS, Impervious, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_imp <- merge(protected_GAPS12_df_PADUS_100pct, Impervious, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_imp <- merge(protected_GAP3only_df_PADUS_100pct, Impervious, by='COMID', all.x=F)
unprotected_df_imp <- merge(unprotected_df, Impervious, by='COMID', all.x=F)

# Elevation
protected_GAPS12_df_PADUS_elev <- merge(protected_GAPS12_df_PADUS, elevation, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_elev <- merge(protected_GAP3only_df_PADUS, elevation, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_elev <- merge(protected_GAPS12_df_PADUS_100pct, elevation, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_elev <- merge(protected_GAP3only_df_PADUS_100pct, elevation, by='COMID', all.x=F)
unprotected_df_elev <- merge(unprotected_df, elevation, by='COMID', all.x=F)

# Wetness index
protected_GAPS12_df_PADUS_WetIndex <- merge(protected_GAPS12_df_PADUS, WetIndex, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_WetIndex <- merge(protected_GAP3only_df_PADUS, WetIndex, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_WetIndex <- merge(protected_GAPS12_df_PADUS_100pct, WetIndex, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_WetIndex <- merge(protected_GAP3only_df_PADUS_100pct, WetIndex, by='COMID', all.x=F)
unprotected_df_WetIndex <- merge(unprotected_df, WetIndex, by='COMID', all.x=F)

# Fire
protected_GAPS12_df_PADUS_Fahr <- merge(protected_GAPS12_df_PADUS, Fahr, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_Fahr <- merge(protected_GAP3only_df_PADUS, Fahr, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_Fahr <- merge(protected_GAPS12_df_PADUS_100pct, Fahr, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_Fahr <- merge(protected_GAP3only_df_PADUS_100pct, Fahr, by='COMID', all.x=F)
unprotected_df_Fahr <- merge(unprotected_df, Fahr, by='COMID', all.x=F)

# Forest loss
protected_GAPS12_df_PADUS_ForestLoss <- merge(protected_GAPS12_df_PADUS, ForestLoss, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_ForestLoss <- merge(protected_GAP3only_df_PADUS, ForestLoss, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_ForestLoss <- merge(protected_GAPS12_df_PADUS_100pct, ForestLoss, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_ForestLoss <- merge(protected_GAP3only_df_PADUS_100pct, ForestLoss, by='COMID', all.x=F)
unprotected_df_ForestLoss <- merge(unprotected_df, ForestLoss, by='COMID', all.x=F)

# Climate
protected_GAPS12_df_PADUS_PRISM <- merge(protected_GAPS12_df_PADUS, PRISM, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_PRISM <- merge(protected_GAP3only_df_PADUS, PRISM, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_PRISM <- merge(protected_GAPS12_df_PADUS_100pct, PRISM, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_PRISM <- merge(protected_GAP3only_df_PADUS_100pct, PRISM, by='COMID', all.x=F)
unprotected_df_PRISM <- merge(unprotected_df, PRISM, by='COMID', all.x=F)

# Dam density
protected_GAPS12_df_PADUS_Dams <- merge(protected_GAPS12_df_PADUS, Dams, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_Dams <- merge(protected_GAP3only_df_PADUS, Dams, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_Dams <- merge(protected_GAPS12_df_PADUS_100pct, Dams, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_Dams <- merge(protected_GAP3only_df_PADUS_100pct, Dams, by='COMID', all.x=F)
unprotected_df_Dams <- merge(unprotected_df, Dams, by='COMID', all.x=F)

# Mines
protected_GAPS12_df_PADUS_Mines <- merge(protected_GAPS12_df_PADUS, Mines, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_Mines <- merge(protected_GAP3only_df_PADUS, Mines, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_Mines <- merge(protected_GAPS12_df_PADUS_100pct, Mines, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_Mines <- merge(protected_GAP3only_df_PADUS_100pct, Mines, by='COMID', all.x=F)
unprotected_df_Mines <- merge(unprotected_df, Mines, by='COMID', all.x=F)

# Deposition
protected_GAPS12_df_PADUS_Deposition <- merge(protected_GAPS12_df_PADUS, Deposition, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_Deposition <- merge(protected_GAP3only_df_PADUS, Deposition, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_Deposition <- merge(protected_GAPS12_df_PADUS_100pct, Deposition, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_Deposition <- merge(protected_GAP3only_df_PADUS_100pct, Deposition, by='COMID', all.x=F)
unprotected_df_Deposition <- merge(unprotected_df, Deposition, by='COMID', all.x=F)

# Runoff
protected_GAPS12_df_PADUS_Runoff <- merge(protected_GAPS12_df_PADUS, Runoff, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_Runoff <- merge(protected_GAP3only_df_PADUS, Runoff, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_Runoff <- merge(protected_GAPS12_df_PADUS_100pct, Runoff, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_Runoff <- merge(protected_GAP3only_df_PADUS_100pct, Runoff, by='COMID', all.x=F)
unprotected_df_Runoff <- merge(unprotected_df, Runoff, by='COMID', all.x=F)

# Baseflow
protected_GAPS12_df_PADUS_Baseflow <- merge(protected_GAPS12_df_PADUS, Baseflow, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_Baseflow <- merge(protected_GAP3only_df_PADUS, Baseflow, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_Baseflow <- merge(protected_GAPS12_df_PADUS_100pct, Baseflow, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_Baseflow <- merge(protected_GAP3only_df_PADUS_100pct, Baseflow, by='COMID', all.x=F)
unprotected_df_Baseflow <- merge(unprotected_df, Baseflow, by='COMID', all.x=F)

# Toxic
protected_GAPS12_df_PADUS_Toxic <- merge(protected_GAPS12_df_PADUS, Toxic, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_Toxic <- merge(protected_GAP3only_df_PADUS, Toxic, by='COMID', all.x=F)
protected_GAPS12_df_PADUS_100pct_Toxic <- merge(protected_GAPS12_df_PADUS_100pct, Toxic, by='COMID', all.x=F)
protected_GAP3only_df_PADUS_100pct_Toxic <- merge(protected_GAP3only_df_PADUS_100pct, Toxic, by='COMID', all.x=F)
unprotected_df_Toxic <- merge(unprotected_df, Toxic, by='COMID', all.x=F)

######### Multi-panel violin plotz
plot_colorz <- c('palegreen','forestgreen','tan','tan1','gray40')

# CLIMATE
temp_var <- c('Tmean8110Cat','Precip8110Cat','ELEVATION','AREASQKM','CatAreaSqKm','WsAreaSqKm')
temp_a <- protected_GAPS12_df_PADUS_PRISM[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_PRISM[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_PRISM[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_PRISM[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_PRISM[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

precip_df <- temp_df[,c('Precip8110Cat','Protection')]
precip_df <- melt(precip_df, id.vars='Protection')
precip_df$Protection <- as.factor(precip_df$Protection)
precip_df$Protection <- factor(precip_df$Protection,levels(precip_df$Protection)[c(4,3,2,1,5)])
precip_violin <- ggplot(precip_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Precipitation",x="", y = "mm")

temperature_df <- temp_df[,c('Tmean8110Cat','Protection')]
temperature_df <- melt(temperature_df, id.vars='Protection')
temperature_df$Protection <- as.factor(temperature_df$Protection)
temperature_df$Protection <- factor(temperature_df$Protection,levels(temperature_df$Protection)[c(4,3,2,1,5)])
temperature_violin <- ggplot(temperature_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Temperature",x="", y = "deg C")

grid.arrange(precip_violin, temperature_violin, nrow=1)

# Catchment and watershed area
catchment_area_df <- temp_df[,c('CatAreaSqKm','Protection')]
catchment_area_df <- melt(catchment_area_df, id.vars='Protection')
catchment_area_df$Protection <- as.factor(catchment_area_df$Protection)
catchment_area_df$Protection <- factor(catchment_area_df$Protection,levels(catchment_area_df$Protection)[c(4,3,2,1,5)])
catchment_area_violin <- ggplot(catchment_area_df, aes(x=Protection, y=log(value), fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Catchment area",x="", y = "")

watershed_area_df <- temp_df[,c('WsAreaSqKm','Protection')]
watershed_area_df <- melt(watershed_area_df, id.vars='Protection')
watershed_area_df$Protection <- as.factor(watershed_area_df$Protection)
watershed_area_df$Protection <- factor(watershed_area_df$Protection,levels(watershed_area_df$Protection)[c(4,3,2,1,5)])
watershed_area_violin <- ggplot(watershed_area_df, aes(x=Protection, y=log(value), fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Watershed area",x="", y = "")

grid.arrange(catchment_area_violin, watershed_area_violin, nrow=1)

# Lake area and elevation
lake_area_df <- temp_df[,c('AREASQKM','Protection')]
lake_area_df <- melt(lake_area_df, id.vars='Protection')
lake_area_df$Protection <- as.factor(lake_area_df$Protection)
lake_area_df$Protection <- factor(lake_area_df$Protection,levels(lake_area_df$Protection)[c(4,3,2,1,5)])
lake_area_violin <- ggplot(lake_area_df, aes(x=Protection, y=log(value), fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Lake area",x="", y = "log sq km")

elevation_df <- temp_df[,c('ELEVATION','Protection')]
elevation_df <- subset(elevation_df, ELEVATION >= 1e-2) #removing extremely negative values (so negative, must be wrong)
elevation_df <- melt(elevation_df, id.vars='Protection')
elevation_df$Protection <- as.factor(elevation_df$Protection)
elevation_df$Protection <- factor(elevation_df$Protection,levels(elevation_df$Protection)[c(4,3,2,1,5)])
elevation_violin <- ggplot(elevation_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Elevation",x="", y = "m")

grid.arrange(lake_area_violin, elevation_violin, nrow=1)

# LULC
temp_var <- c('PctTotalForest2011Cat','PctTotalAg2011Cat','PctTotalWetland2011Cat')
temp_a <- protected_GAPS12_df_PADUS_NLCD[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_NLCD[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_NLCD[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_NLCD[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_NLCD[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

forest_df <- temp_df[,c('PctTotalForest2011Cat','Protection')]
forest_df <- melt(forest_df, id.vars='Protection')
forest_df$Protection <- as.factor(forest_df$Protection)
forest_df$Protection <- factor(forest_df$Protection,levels(forest_df$Protection)[c(4,3,2,1,5)])
forest_violin <- ggplot(forest_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Forest",x="", y = "Percent")

ag_df <- temp_df[,c('PctTotalAg2011Cat','Protection')]
ag_df <- melt(ag_df, id.vars='Protection')
ag_df$Protection <- as.factor(ag_df$Protection)
ag_df$Protection <- factor(ag_df$Protection,levels(ag_df$Protection)[c(4,3,2,1,5)])
ag_violin <- ggplot(ag_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Agriculture",x="", y = "Percent")

wetland_df <- temp_df[,c('PctTotalWetland2011Cat','Protection')]
wetland_df <- melt(wetland_df, id.vars='Protection')
wetland_df$Protection <- as.factor(wetland_df$Protection)
wetland_df$Protection <- factor(wetland_df$Protection,levels(wetland_df$Protection)[c(4,3,2,1,5)])
wetland_violin <- ggplot(wetland_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Wetlands",x="", y = "Percent")

grid.arrange(forest_violin, ag_violin, wetland_violin, nrow=1)

# Road density and impervious
temp_var <- c('RdDensCat','RdDensWs')
temp_a <- protected_GAPS12_df_PADUS_roads[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_roads[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_roads[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_roads[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_roads[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

roads_df <- temp_df[,c('RdDensCat','Protection')]
roads_df <- melt(roads_df, id.vars='Protection')
roads_df$Protection <- as.factor(roads_df$Protection)
roads_df$Protection <- factor(roads_df$Protection,levels(roads_df$Protection)[c(4,3,2,1,5)])
roads_violin <- ggplot(roads_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Road density",x="", y = "km/sq km")

temp_var <- c('PctImp2011Cat','PctImp2011Ws')
temp_a <- protected_GAPS12_df_PADUS_imp[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_imp[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_imp[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_imp[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_imp[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

imp_df <- temp_df[,c('PctImp2011Cat','Protection')]
imp_df <- melt(imp_df, id.vars='Protection')
imp_df$Protection <- as.factor(imp_df$Protection)
imp_df$Protection <- factor(imp_df$Protection,levels(imp_df$Protection)[c(4,3,2,1,5)])
imp_violin <- ggplot(imp_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Impervious",x="", y = "Percent")

grid.arrange(roads_violin, imp_violin, nrow=1)

## Forest loss, fire
temp_var <- c('TotalPctFrstLossCat','TotalPctFrstLossWs')
temp_a <- protected_GAPS12_df_PADUS_ForestLoss[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_ForestLoss[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_ForestLoss[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_ForestLoss[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_ForestLoss[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

ForestLoss_df <- temp_df[,c('TotalPctFrstLossCat','Protection')]
ForestLoss_df <- melt(ForestLoss_df, id.vars='Protection')
ForestLoss_df$Protection <- as.factor(ForestLoss_df$Protection)
ForestLoss_df$Protection <- factor(ForestLoss_df$Protection,levels(ForestLoss_df$Protection)[c(4,3,2,1,5)])
ForestLoss_violin <- ggplot(ForestLoss_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="ForestLoss",x="", y = "Percent")+
  scale_y_continuous(limits=c(0,25))

temp_var <- c('TotalPctFireCat','TotalPctFireWs')
temp_a <- protected_GAPS12_df_PADUS_Fahr[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_Fahr[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_Fahr[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_Fahr[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_Fahr[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

Fahr_df <- temp_df[,c('TotalPctFireCat','Protection')]
Fahr_df <- melt(Fahr_df, id.vars='Protection')
Fahr_df$Protection <- as.factor(Fahr_df$Protection)
Fahr_df$Protection <- factor(Fahr_df$Protection,levels(Fahr_df$Protection)[c(4,3,2,1,5)])
Fahr_violin <- ggplot(Fahr_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Fire",x="", y = "Percent")+
  scale_y_continuous(limits=c(0,25))

grid.arrange(ForestLoss_violin, Fahr_violin, nrow=1)

## Runoff and baseflow
temp_var <- c('RunoffCat','RunoffWs')
temp_a <- protected_GAPS12_df_PADUS_Runoff[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_Runoff[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_Runoff[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_Runoff[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_Runoff[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

Runoff_df <- temp_df[,c('RunoffCat','Protection')]
Runoff_df <- melt(Runoff_df, id.vars='Protection')
Runoff_df$Protection <- as.factor(Runoff_df$Protection)
Runoff_df$Protection <- factor(Runoff_df$Protection,levels(Runoff_df$Protection)[c(4,3,2,1,5)])
Runoff_violin <- ggplot(Runoff_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Runoff",x="", y = "mm/month")

temp_var <- c('BFICat','BFIWs')
temp_a <- protected_GAPS12_df_PADUS_Baseflow[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_Baseflow[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_Baseflow[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_Baseflow[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_Baseflow[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

Baseflow_df <- temp_df[,c('BFICat','Protection')]
Baseflow_df <- melt(Baseflow_df, id.vars='Protection')
Baseflow_df$Protection <- as.factor(Baseflow_df$Protection)
Baseflow_df$Protection <- factor(Baseflow_df$Protection,levels(Baseflow_df$Protection)[c(4,3,2,1,5)])
Baseflow_violin <- ggplot(Baseflow_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Baseflow index",x="", y = "% baseflow of total inflow")

grid.arrange(Runoff_violin, Baseflow_violin, nrow=1)

## Dams
temp_var <- c('NABD_DensCat','NABD_DensWs')
temp_a <- protected_GAPS12_df_PADUS_Dams[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_Dams[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_Dams[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_Dams[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_Dams[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

Dams_df <- temp_df[,c('NABD_DensCat','Protection')]
Dams_df <- melt(Dams_df, id.vars='Protection')
Dams_df$Protection <- as.factor(Dams_df$Protection)
Dams_df$Protection <- factor(Dams_df$Protection,levels(Dams_df$Protection)[c(4,3,2,1,5)])
Dams_violin <- ggplot(Dams_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Dam density",x="", y = "Dams/sq km")

grid.arrange(Runoff_violin, Baseflow_violin, Dams_violin, nrow=1)

## Mines
temp_var <- c('MineDensCat','MineDensWs')
temp_a <- protected_GAPS12_df_PADUS_Mines[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_Mines[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_Mines[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_Mines[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_Mines[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

Mines_df <- temp_df[,c('MineDensCat','Protection')]
Mines_df <- melt(Mines_df, id.vars='Protection')
Mines_df$Protection <- as.factor(Mines_df$Protection)
Mines_df$Protection <- factor(Mines_df$Protection,levels(Mines_df$Protection)[c(4,3,2,1,5)])
Mines_violin <- ggplot(Mines_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Mine density",x="", y = "Mines/sq km")

grid.arrange(roads_violin, imp_violin, Mines_violin, nrow=1)

## Topo wetness index
temp_var <- c('WetIndexCat','WetIndexWs')
temp_a <- protected_GAPS12_df_PADUS_WetIndex[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_WetIndex[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_WetIndex[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_WetIndex[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_WetIndex[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

WetIndex_df <- temp_df[,c('WetIndexCat','Protection')]
WetIndex_df <- melt(WetIndex_df, id.vars='Protection')
WetIndex_df$Protection <- as.factor(WetIndex_df$Protection)
WetIndex_df$Protection <- factor(WetIndex_df$Protection,levels(WetIndex_df$Protection)[c(4,3,2,1,5)])
WetIndex_violin <- ggplot(WetIndex_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Wetness Index",x="", y = "")

grid.arrange(Dams_violin, Mines_violin, nrow=1)
grid.arrange(elevation_violin, WetIndex_violin, nrow=1)

## Toxic sites
temp_var <- c('NPDESDensCat','TRIDensCat','SuperfundDensCat')
temp_a <- protected_GAPS12_df_PADUS_Toxic[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_Toxic[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_Toxic[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_Toxic[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_Toxic[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

NPDES_df <- temp_df[,c('NPDESDensCat','Protection')]
NPDES_df <- melt(NPDES_df, id.vars='Protection')
NPDES_df$Protection <- as.factor(NPDES_df$Protection)
NPDES_df$Protection <- factor(NPDES_df$Protection,levels(NPDES_df$Protection)[c(4,3,2,1,5)])
NPDES_violin <- ggplot(NPDES_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Nat'l pollutant discharge sites",x="", y = "#/sq km")

TRI_df <- temp_df[,c('TRIDensCat','Protection')]
TRI_df <- melt(TRI_df, id.vars='Protection')
TRI_df$Protection <- as.factor(TRI_df$Protection)
TRI_df$Protection <- factor(TRI_df$Protection,levels(TRI_df$Protection)[c(4,3,2,1,5)])
TRI_violin <- ggplot(TRI_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Toxic release inventory sites",x="", y = "#/sq km")

Superfund_df <- temp_df[,c('SuperfundDensCat','Protection')]
Superfund_df <- melt(Superfund_df, id.vars='Protection')
Superfund_df$Protection <- as.factor(Superfund_df$Protection)
Superfund_df$Protection <- factor(Superfund_df$Protection,levels(Superfund_df$Protection)[c(4,3,2,1,5)])
Superfund_violin <- ggplot(Superfund_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Superfund sites",x="", y = "#/sq km")

grid.arrange(NPDES_violin, TRI_violin, Superfund_violin, nrow=1)

## Deposition
temp_var <- c('SN_2008Cat','SN_2008Ws')
temp_a <- protected_GAPS12_df_PADUS_Deposition[,temp_var]
temp_a$Protection <- 'Strict center'
temp_b <- protected_GAPS12_df_PADUS_100pct_Deposition[,temp_var]
temp_b$Protection <- 'Strict cat'
temp_c <- protected_GAP3only_df_PADUS_Deposition[,temp_var]
temp_c$Protection <- 'Multi-use center'
temp_d <- protected_GAP3only_df_PADUS_100pct_Deposition[,temp_var]
temp_d$Protection <- 'Multi-use cat'
temp_e <- unprotected_df_Deposition[,temp_var]
temp_e$Protection <- 'Unprotected'
temp_df <- rbind.data.frame(temp_a, temp_b, temp_c, temp_d, temp_e)

Deposition_df <- temp_df[,c('SN_2008Cat','Protection')]
Deposition_df <- melt(Deposition_df, id.vars='Protection')
Deposition_df$Protection <- as.factor(Deposition_df$Protection)
Deposition_df$Protection <- factor(Deposition_df$Protection,levels(Deposition_df$Protection)[c(4,3,2,1,5)])
Deposition_violin <- ggplot(Deposition_df, aes(x=Protection, y=value, fill=Protection)) + 
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  theme_classic()+
  theme(legend.position='none')+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_fill_manual(values=plot_colorz)+
  labs(title="Sulfur+nitrogen deposition",x="", y = "kg/ha/yr")
Deposition_violin

## Summary stats for different variables: protected vs. unprotected
# custom function calculates min, 5th percentile, median, 95th percentile, max, mean, sd
summary_statz <- function(df){
  #df=data frame, with columns 'Protection' and 'value'
  statz <- as.data.frame(df %>% 
  group_by(Protection) %>%
  summarize(min=min(value, na.rm=T), p5th=quantile(value, 0.05, na.rm=T),median=median(value, na.rm=T), p95th=quantile(value, 0.95, na.rm=T),
  max=max(value, na.rm=T), mean=mean(value, na.rm=T), sd=sd(value, na.rm=T)))
  statz$varname <- paste(df$variable[1])
  return(statz)
}

lake_area_statz <- summary_statz(lake_area_df)
elevation_statz <- summary_statz(elevation_df)
catchment_area_statz <- summary_statz(catchment_area_df)
watershed_area_statz <- summary_statz(watershed_area_df)
forest_statz <- summary_statz(forest_df)
ag_statz <- summary_statz(ag_df)
wetland_statz <- summary_statz(wetland_df)
roads_statz <- summary_statz(roads_df)
impervious_statz <- summary_statz(imp_df)
forest_loss_statz <- summary_statz(ForestLoss_df)
fire_statz <- summary_statz(Fahr_df)
depos_statz <- summary_statz(Deposition_df)
mines_statz <- summary_statz(Mines_df)
dams_statz <- summary_statz(Dams_df)
superfund_statz <- summary_statz(Superfund_df)
TRI_statz <- summary_statz(TRI_df)
NPDES_statz <- summary_statz(NPDES_df)
WetIndex_statz <- summary_statz(WetIndex_df)
runoff_statz <- summary_statz(Runoff_df)
baseflow_statz <- summary_statz(Baseflow_df)
precip_statz <- summary_statz(precip_df)
temperature_statz <- summary_statz(temperature_df)

all_summary_statz <- rbind.data.frame(lake_area_statz, elevation_statz, catchment_area_statz, watershed_area_statz,
                                      forest_statz, ag_statz, wetland_statz, roads_statz, impervious_statz,
                                      forest_loss_statz, fire_statz, depos_statz, mines_statz, dams_statz,
                                      superfund_statz, TRI_statz, NPDES_statz, WetIndex_statz, precip_statz,
                                      temperature_statz, runoff_statz, baseflow_statz)
#write.csv(all_summary_statz, file='Data/protected_v_unprotected_stats.csv')

############# Analyze by LAGOS conn class #################
## Summarize conn types for all lakes
LakeConn_countz <- as.data.frame(LAGOSconn %>%
                                   group_by(LakeConnec) %>%
                                   tally())

barplot(LakeConn_countz$n, names.arg=LakeConn_countz$LakeConnec, las=1)

# Strict protection, lake center
protected_GAPS12_df_PADUS_conn <- merge(protected_GAPS12_df_PADUS, LAGOSconn, by.x='COMID', 
                                        by.y='nhdplusv2_comid', all.x=F)

protected_GAPS12_conn_countz <- as.data.frame(protected_GAPS12_df_PADUS_conn %>%
                                                group_by(LakeConnec) %>%
                                                tally())
protected_GAPS12_conn_countz <- merge(protected_GAPS12_conn_countz, LakeConn_countz, by='LakeConnec')
colnames(protected_GAPS12_conn_countz) <- c('LakeConnec','n_protected', 'n_total')
protected_GAPS12_conn_countz$prop_protected <- protected_GAPS12_conn_countz$n_protected/protected_GAPS12_conn_countz$n_total
protected_GAPS12_conn_countz$Group <- 'Strict center'

# multi-use, lake center
protected_GAP3only_df_PADUS_conn <- merge(protected_GAP3only_df_PADUS, LAGOSconn, by.x='COMID', 
                                          by.y='nhdplusv2_comid', all.x=F)

protected_GAPS3only_conn_countz <- as.data.frame(protected_GAP3only_df_PADUS_conn %>%
                                                   group_by(LakeConnec) %>%
                                                   tally())
protected_GAPS3only_conn_countz <- merge(protected_GAPS3only_conn_countz, LakeConn_countz, by='LakeConnec')
colnames(protected_GAPS3only_conn_countz) <- c('LakeConnec','n_protected', 'n_total')
protected_GAPS3only_conn_countz$prop_protected <- protected_GAPS3only_conn_countz$n_protected/protected_GAPS3only_conn_countz$n_total
protected_GAPS3only_conn_countz$Group <- 'Multi-use center'

# strict, 100% cat protection
protected_GAPS12_df_PADUS_100pct_conn <- merge(protected_GAPS12_df_PADUS_100pct, LAGOSconn, by.x='COMID', 
                                               by.y='nhdplusv2_comid', all.x=F)

protected_GAPS12_100pct_conn_countz <- as.data.frame(protected_GAPS12_df_PADUS_100pct_conn %>%
                                                       group_by(LakeConnec) %>%
                                                       tally())
protected_GAPS12_100pct_conn_countz <- merge(protected_GAPS12_100pct_conn_countz, LakeConn_countz, by='LakeConnec')
colnames(protected_GAPS12_100pct_conn_countz) <- c('LakeConnec','n_protected', 'n_total')
protected_GAPS12_100pct_conn_countz$prop_protected <- protected_GAPS12_100pct_conn_countz$n_protected/protected_GAPS12_100pct_conn_countz$n_total
protected_GAPS12_100pct_conn_countz$Group <- 'Strict cat'

# multi-use, 100% cat protection
protected_GAP3only_df_PADUS_100pct_conn <- merge(protected_GAP3only_df_PADUS_100pct, LAGOSconn, by.x='COMID', 
                                                 by.y='nhdplusv2_comid', all.x=F)

protected_GAP3only_100pct_conn_countz <- as.data.frame(protected_GAP3only_df_PADUS_100pct_conn %>%
                                                         group_by(LakeConnec) %>%
                                                         tally())
protected_GAP3only_100pct_conn_countz <- merge(protected_GAP3only_100pct_conn_countz, LakeConn_countz, by='LakeConnec')
colnames(protected_GAP3only_100pct_conn_countz) <- c('LakeConnec','n_protected', 'n_total')
protected_GAP3only_100pct_conn_countz$prop_protected <- protected_GAP3only_100pct_conn_countz$n_protected/protected_GAP3only_100pct_conn_countz$n_total
protected_GAP3only_100pct_conn_countz$Group <- 'Multi-use cat'

# unprotected
unprotected_df_conn <- merge(unprotected_df, LAGOSconn, by.x='COMID', 
                             by.y='nhdplusv2_comid', all.x=F)

unprotected_df_conn_countz <- as.data.frame(unprotected_df_conn %>%
                                              group_by(LakeConnec) %>%
                                              tally())
unprotected_df_conn_countz <- merge(unprotected_df_conn_countz, LakeConn_countz, by='LakeConnec')
colnames(unprotected_df_conn_countz) <- c('LakeConnec','n_unprotected', 'n_total')
unprotected_df_conn_countz$prop_protected <- unprotected_df_conn_countz$n_unprotected/unprotected_df_conn_countz$n_total
unprotected_df_conn_countz$Group <- 'Unprotected'

## Stacked barplots
stacked_df <- rbind.data.frame(protected_GAPS12_conn_countz[,c(1,4,5)],protected_GAPS3only_conn_countz[,c(1,4,5)],
                               unprotected_df_conn_countz[,c(1,4,5)])
stacked_df$Group <- as.factor(stacked_df$Group)

#png('Figures/stacked_bar_conn_type.png',width = 7.5,height = 4.75,units = 'in',res=300)
stack_gg1 <- ggplot(stacked_df, aes(x = LakeConnec, y = prop_protected, fill = Group)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Proportion of lakes protected") +
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  ggtitle('A) Protected lake = lake center in protected area')+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
  scale_x_discrete(labels=c('Drainage, lake/stream','Drainage, stream','Headwater','Isolated'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  theme(axis.title.y = element_text(vjust=2.7))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("Multi-use center" = "navajowhite2", "Strict center" = "olivedrab3","Unprotected" = "gray70"),
                    labels=c('Multi-use','Strict','Unprotected'))+
  #theme(legend.position=c(0.88,0.16))+ #manually reposition legend inside plot
  theme(legend.position='none')+
  theme(legend.title=element_blank()) #remove legend title
  stack_gg1
#dev.off()

# Same stacked bar idea, but treating protected lakes as those with fully protected catchments
# calculate "unprotected" based on all lake catchments that are not 100% protected (strict or multi-use)
temp_ryan <- unprotected_df_conn_countz[,c(1,4,5)]
temp_ryan$prop_protected <- 1- (protected_GAPS12_100pct_conn_countz$prop_protected + protected_GAP3only_100pct_conn_countz$prop_protected)
stacked_df2 <- rbind.data.frame(protected_GAPS12_100pct_conn_countz[,c(1,4,5)],protected_GAP3only_100pct_conn_countz[,c(1,4,5)], temp_ryan)

#png('Figures/stacked_bar_conn_type_cat100.png',width = 7.5,height = 4.75,units = 'in',res=300)
stack_gg2 <- ggplot(stacked_df2, aes(x = LakeConnec, y = prop_protected, fill = Group)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Proportion of lakes protected") +
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  ggtitle('B) Protected lake = 100% catchment protected')+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
  scale_x_discrete(labels=c('Drainage, lake/stream','Drainage, stream','Headwater','Isolated'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  theme(axis.title.y = element_text(vjust=2.7))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("Multi-use cat" = "navajowhite2", "Strict cat" = "olivedrab3","Unprotected" = "gray70"),
                    labels=c('Multi-use','Strict','Unprotected'))+
  theme(legend.position=c(0.86,0.21))+ #manually reposition legend inside plot
  theme(legend.text=element_text(size=8))+
  theme(legend.title=element_blank()) #remove legend title
  stack_gg2
#dev.off()

# multi-panel stacked barplot for different definitions of lake protection
png('Figures/panel_stacked_bar_conn_type_cat100.png',height = 7.5,width = 6,units = 'in',res=300)
  grid.arrange(stack_gg1, stack_gg2, nrow=2)
dev.off()
######################### who actually looked this far down? ######################################