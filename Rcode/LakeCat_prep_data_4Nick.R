############# LakeCat protected area analysis: prep data for Nick ##############################
# Date: 11-26-18
# updated: 3-20-19
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(dplyr)
library(reshape2)
library(ggplot2)
library(raster)
library(vioplot)

#### input data ####
setwd("C:/Users/FWL/Documents/FreshwaterConservation")

# NHD waterbodies (converted to points in ArcGIS) (NHDPlusV2 National dataset; downloaded November 2018)
NHD_pts <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_waterbody_pts.shp")

## Protected lakes (centroids)
protected_GAPS12 <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/NHD_protect_pts_GAPS12_pct.shp")
protected_GAP3only <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/NHD_protect_pts_GAP3only_pct.shp")
protected_GAPS12_COMIDs_100 <- subset(protected_GAPS12, PGAP_S12C >=100)
protected_GAPS12_COMIDs_100 <- unique(protected_GAPS12_COMIDs_100@data$COMID)
protected_GAP3only_COMIDs_100 <- subset(protected_GAP3only, PGAP_S3C >=100)
protected_GAP3only_COMIDs_100 <- unique(protected_GAP3only_COMIDs_100@data$COMID)

# unprotected lake COMIDs
unprotected_COMIDs <- read.csv("Data/unprotected_COMID.csv")[,2]

# NARS ecoregion and US state table by COMID
state_NARS_COMIDs <- read.csv("Data/state_NARS_COMID.csv")

# lower 48 states
lower48 <- shapefile("Data/lower48/lower48.shp") #same crs as NHD_pts

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
min_protected <- 0 # keep data points (lake catchments) with percent protected above this value

############## Main program #############
# Calculate total catchment and watershed protection for GAPS 1-2
PADUS_LakeCat$PctGAP_Status12Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat
#PADUS_LakeCat$PctGAP_Status123Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat + PADUS_LakeCat$PctGAP_Status3Cat
#PADUS_LakeCat$PctGAP_Status12Ws <- PADUS_LakeCat$PctGAP_Status1Ws + PADUS_LakeCat$PctGAP_Status2Ws
#PADUS_LakeCat$PctGAP_Status123Ws <- PADUS_LakeCat$PctGAP_Status1Ws + PADUS_LakeCat$PctGAP_Status2Ws + PADUS_LakeCat$PctGAP_Status3Ws

## create columns for protected vs. unprotected (based on different % catchment protected thresholds)
# for 75, 90 and 100 % catchment protection (individually for GAP Status 1-2 and 1-3)
# PADUS_LakeCat$ProtectGAP12Cat_75 <- ifelse(PADUS_LakeCat$PctGAP_Status12Cat >= 75, "Protected75", "Unprotected75")
# PADUS_LakeCat$ProtectGAP123Cat_75 <- ifelse(PADUS_LakeCat$PctGAP_Status123Cat >= 75, "Protected75", "Unprotected75")
# PADUS_LakeCat$ProtectGAP12Cat_90 <- ifelse(PADUS_LakeCat$PctGAP_Status12Cat >= 90, "Protected90", "Unprotected90")
# PADUS_LakeCat$ProtectGAP123Cat_90 <- ifelse(PADUS_LakeCat$PctGAP_Status123Cat >= 90, "Protected90", "Unprotected90")
PADUS_LakeCat$ProtectGAP12Cat_100 <- ifelse(PADUS_LakeCat$COMID %in% protected_GAPS12_COMIDs_100, "Protected100", "Unprotected100")
summary(as.factor(PADUS_LakeCat$ProtectGAP12Cat_100))#check
#PADUS_LakeCat$ProtectGAP123Cat_100 <- ifelse(PADUS_LakeCat$PctGAP_Status123Cat >= 100, "Protected100", "Unprotected100")
PADUS_LakeCat$ProtectGAP3Cat_100 <- ifelse(PADUS_LakeCat$COMID %in% protected_GAP3only_COMIDs_100, "Protected100", "Unprotected100")
summary(as.factor(PADUS_LakeCat$ProtectGAP3Cat_100))#check

# for 75, 90 and 100 % watershed protection (individually for GAP Status 1-2 and 1-3)
# PADUS_LakeCat$ProtectGAP12Ws_75 <- ifelse(PADUS_LakeCat$PctGAP_Status12Ws >= 75, "Protected75", "Unprotected75")
# PADUS_LakeCat$ProtectGAP123Ws_75 <- ifelse(PADUS_LakeCat$PctGAP_Status123Ws >= 75, "Protected75", "Unprotected75")
# PADUS_LakeCat$ProtectGAP12Ws_90 <- ifelse(PADUS_LakeCat$PctGAP_Status12Ws >= 90, "Protected90", "Unprotected90")
# PADUS_LakeCat$ProtectGAP123Ws_90 <- ifelse(PADUS_LakeCat$PctGAP_Status123Ws >= 90, "Protected90", "Unprotected90")
#PADUS_LakeCat$ProtectGAP12Ws_100 <- ifelse(PADUS_LakeCat$PctGAP_Status12Ws >= 100, "Protected100", "Unprotected100")
#PADUS_LakeCat$ProtectGAP123Ws_100 <- ifelse(PADUS_LakeCat$PctGAP_Status123Ws >= 100, "Protected100", "Unprotected100")
#PADUS_LakeCat$ProtectGAP3Ws_100 <- ifelse(PADUS_LakeCat$PctGAP_Status3Ws >= 100, "Protected100", "Unprotected100")

## create columns for protected vs. unprotected based on lake centers
PADUS_LakeCat$ProtectGAP12_ctr <- ifelse(PADUS_LakeCat$COMID %in% unique(protected_GAPS12@data$COMID), 'ProtectedCtr','UnprotectedCtr')
summary(as.factor(PADUS_LakeCat$ProtectGAP12_ctr))#check

PADUS_LakeCat$ProtectGAP3_ctr <- ifelse(PADUS_LakeCat$COMID %in% unique(protected_GAP3only@data$COMID), 'ProtectedCtr','UnprotectedCtr')
summary(as.factor(PADUS_LakeCat$ProtectGAP3_ctr))#check

## create columns for unprotected (lake doesn't fall in protected area)
PADUS_LakeCat$Unprotected <- ifelse(PADUS_LakeCat$COMID %in% unprotected_COMIDs, 'Unprotected','Protected')
summary(as.factor(PADUS_LakeCat$Unprotected))#check

#### Filter out small lakes ####
lake_sqkm_cutoff <- 0.01 #=1ha

# Lake area
NHD_pts@data$COMID <- as.numeric(NHD_pts@data$COMID) #COMID was character in NHD; must convert
PADUS_NHD <- left_join(PADUS_LakeCat, NHD_pts@data, by='COMID')
PADUS_NHD <- PADUS_NHD[!duplicated(PADUS_NHD$COMID),] #remove duplicate COMID (6 for some reason)
PADUS_NHD <- subset(PADUS_NHD, AREASQKM >= lake_sqkm_cutoff)
COMID_1ha_above <- PADUS_NHD$COMID #get string of COMIDs >= 1ha for further subsetting below
PADUS_LakeCat <- subset(PADUS_LakeCat, COMID %in% COMID_1ha_above) #overall table

# Lake max depth
#PADUS_NHD_maxdepth <- subset(PADUS_NHD, MaxDepth > 0) #meters
#PADUS_NHD_maxdepth <- subset(PADUS_NHD_maxdepth, COMID %in% COMID_1ha_above)

# Elevation
PADUS_elevation <- full_join(PADUS_LakeCat, elevation, by='COMID')
PADUS_elevation <- subset(PADUS_elevation, COMID %in% COMID_1ha_above)
cor(PADUS_elevation$ElevCat, PADUS_elevation$ElevWs, method='pearson', use='pairwise.complete.obs')

# Topographic wetness index
PADUS_WetIndex <- full_join(PADUS_LakeCat, WetIndex, by='COMID')
PADUS_WetIndex <- subset(PADUS_WetIndex, COMID %in% COMID_1ha_above)
cor(PADUS_WetIndex$WetIndexCat, PADUS_WetIndex$WetIndexWs, method='pearson', use='pairwise.complete.obs')

# Land use/cover: calculate some new "total" variables
NLCD_2011$PctTotalForest2011Cat <- NLCD_2011$PctConif2011Cat + NLCD_2011$PctDecid2011Cat + NLCD_2011$PctMxFst2011Cat
NLCD_2011$PctTotalAg2011Cat <- NLCD_2011$PctCrop2011Cat + NLCD_2011$PctHay2011Cat
NLCD_2011$PctTotalWetland2011Cat <- NLCD_2011$PctWdWet2011Cat + NLCD_2011$PctHbWet2011Cat
NLCD_2011$PctTotalForest2011Ws <- NLCD_2011$PctConif2011Ws + NLCD_2011$PctDecid2011Ws + NLCD_2011$PctMxFst2011Ws
NLCD_2011$PctTotalAg2011Ws <- NLCD_2011$PctCrop2011Ws + NLCD_2011$PctHay2011Ws
NLCD_2011$PctTotalWetland2011Ws <- NLCD_2011$PctWdWet2011Ws + NLCD_2011$PctHbWet2011Ws
PADUS_NLCD2011 <- full_join(PADUS_LakeCat, NLCD_2011, by='COMID')
PADUS_NLCD2011 <- subset(PADUS_NLCD2011, COMID %in% COMID_1ha_above)
cor(PADUS_NLCD2011$PctTotalForest2011Cat, PADUS_NLCD2011$PctTotalForest2011Ws, method='pearson', use='pairwise.complete.obs')
cor(PADUS_NLCD2011$PctTotalAg2011Cat, PADUS_NLCD2011$PctTotalAg2011Ws, method='pearson', use='pairwise.complete.obs')
cor(PADUS_NLCD2011$PctTotalWetland2011Cat, PADUS_NLCD2011$PctTotalWetland2011Ws, method='pearson', use='pairwise.complete.obs')

# Road density
PADUS_RoadDensity <- full_join(PADUS_LakeCat, RoadDensity, by='COMID')
PADUS_RoadDensity <- subset(PADUS_RoadDensity, COMID %in% COMID_1ha_above)
cor(PADUS_RoadDensity$RdDensCat, PADUS_RoadDensity$RdDensWs, method='pearson', use='pairwise.complete.obs')

# Impervious surface
PADUS_Impervious <- full_join(PADUS_LakeCat, Impervious, by='COMID')
PADUS_Impervious <- subset(PADUS_Impervious, COMID %in% COMID_1ha_above)
cor(PADUS_Impervious$PctImp2011Cat, PADUS_Impervious$PctImp2011Ws, method='pearson', use='pairwise.complete.obs')

# Mines
PADUS_Mines <- full_join(PADUS_LakeCat, Mines, by='COMID')
PADUS_Mines <- subset(PADUS_Mines, COMID %in% COMID_1ha_above)

# Dams
PADUS_Dams <- full_join(PADUS_LakeCat, Dams, by='COMID')
PADUS_Dams <- subset(PADUS_Dams, COMID %in% COMID_1ha_above)

# Runoff
PADUS_runoff <- full_join(PADUS_LakeCat, Runoff, by='COMID')
PADUS_runoff <- subset(PADUS_runoff, COMID %in% COMID_1ha_above)
cor(PADUS_runoff$RunoffCat, PADUS_runoff$RunoffWs, method='pearson', use='pairwise.complete.obs')

# Baseflow
PADUS_baseflow <- full_join(PADUS_LakeCat, Baseflow, by='COMID')
PADUS_baseflow <- subset(PADUS_baseflow, COMID %in% COMID_1ha_above)
cor(PADUS_baseflow$BFICat, PADUS_baseflow$BFIWs, method='pearson', use='pairwise.complete.obs')

# Toxic point source pollution
PADUS_Toxic <- full_join(PADUS_LakeCat, Toxic, by='COMID')
PADUS_Toxic <- subset(PADUS_Toxic, COMID %in% COMID_1ha_above)

# Non point source pollution
PADUS_Deposition <- full_join(PADUS_LakeCat, Deposition, by='COMID')
PADUS_Deposition <- subset(PADUS_Deposition, COMID %in% COMID_1ha_above)

# Fire
Fahr$TotalPctFireCat <- rowSums(Fahr[,7:17])
Fahr$TotalPctFireWs <- rowSums(Fahr[,18:28])
PADUS_Fahr <- full_join(PADUS_LakeCat, Fahr, by='COMID')
PADUS_Fahr <- subset(PADUS_Fahr, COMID %in% COMID_1ha_above)

# Forest loss
ForestLoss$TotalPctFrstLossCat <- rowSums(ForestLoss[,7:19])
ForestLoss$TotalPctFrstLossWs <- rowSums(ForestLoss[,20:32])
PADUS_ForestLoss <- full_join(PADUS_LakeCat, ForestLoss, by='COMID')
PADUS_ForestLoss <- subset(PADUS_ForestLoss, COMID %in% COMID_1ha_above)
cor(PADUS_ForestLoss$TotalPctFrstLossCat, PADUS_ForestLoss$TotalPctFrstLossWs, method='pearson', use='pairwise.complete.obs')

# Climate
PADUS_PRISM <- full_join(PADUS_LakeCat, PRISM, by='COMID') 
PADUS_PRISM <- subset(PADUS_PRISM, COMID %in% COMID_1ha_above)
cor(PADUS_PRISM$Precip8110Cat, PADUS_PRISM$Precip8110Ws, method='pearson', use='pairwise.complete.obs')
cor(PADUS_PRISM$Tmean8110Cat, PADUS_PRISM$Tmean8110Ws, method='pearson', use='pairwise.complete.obs')

#### create master table of desired variables
# Calculate drainage area proxy
PADUS_NHD$DrainageRatio <- PADUS_NHD$AREASQKM/PADUS_NHD$WsAreaSqKm

a <- PADUS_NHD[,c('COMID','PctGAP_Status12Cat','PctGAP_Status3Cat',
                  'ProtectGAP12_ctr','ProtectGAP3_ctr',
                  'ProtectGAP12Cat_100','ProtectGAP3Cat_100','Unprotected','AREASQKM','CatAreaSqKm','DrainageRatio')]
b <- PADUS_elevation[,c('COMID','ElevCat')]
c <- PADUS_WetIndex[,c('COMID','WetIndexCat')]
d <- PADUS_NLCD2011[,c('COMID','PctTotalForest2011Cat','PctTotalAg2011Cat','PctTotalWetland2011Cat','PctConif2011Cat')]
e <- PADUS_RoadDensity[,c('COMID','RdDensCat')]
f <- PADUS_Impervious[,c('COMID','PctImp2011Cat')]
g <- PADUS_Mines[,c('COMID','MineDensCat')]
h <- PADUS_Dams[,c('COMID','NABD_DensCat')]
i <- PADUS_runoff[,c('COMID','RunoffCat')]
j <- PADUS_baseflow[,c('COMID','BFICat')]
k <- PADUS_Toxic[,c('COMID','NPDESDensCat','SuperfundDensCat','TRIDensCat')]
l <- PADUS_Deposition[,c('COMID','SN_2008Cat')]
m <- PADUS_Fahr[,c('COMID','TotalPctFireCat')]
n <- PADUS_ForestLoss[,c('COMID','TotalPctFrstLossCat')]
o <- PADUS_PRISM[,c('COMID','Precip8110Cat','Tmean8110Cat')]
p <- state_NARS_COMIDs[,c('COMID','WSA9','WSA9_NAME','STUSPS')]
#xx <- Reduce(function(...) merge(..., all=T), list(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))
cat_table <- Reduce(function(...) merge(..., all=T), list(a,b,c,d,e,f,i,j,l,n,o,p))
cat_table <- cat_table[!duplicated(cat_table$COMID),] #remove duplicate COMIDs
#cat_table <- xx[,c(1,4,5,8,9,10,11,12,13,2,20:22,24,26:29,34,36,38,40,42,44,46:48,52,54,56,58,59)]
#Ws_table <- xx[,c(1,6,7,14:19,3,20,21,23,25,30:33,35,37,39,41,43,45,49:51,53,55,57,60,61)]
#write.csv(cat_table, file="Data/Nick/catchment_table.csv")
#write.csv(Ws_table, file="Data/Nick/watershed_table.csv")
