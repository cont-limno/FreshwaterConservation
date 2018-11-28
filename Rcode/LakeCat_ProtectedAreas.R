####################### LakeCat protected area analysis ########################################
# Date: 11-26-18
# updated: 11-27-18
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(dplyr)
library(reshape2)
library(ggplot2)
library(raster)

#### input data ####
setwd("C:/Users/FWL/Documents/FreshwaterConservation")

# NHD waterbodies (converted to points in ArcGIS) (NHDPlusV2 National dataset; downloaded November 2018)
NHD_pts <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_waterbody_pts.shp")

# lower 48 states
lower48 <- shapefile("C:/Ian_GIS/cb_2016_us_state_500k/lower48.shp") #same crs as NHD_pts

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

#### D-fine functions ####
## these functions make decent exploratory plots, but axes/labels, spacing admittedly not perfect

# exploratory scatter plot with correlation coefficient (used for percent catchment protected vs. some variable)
expo_plot <- function(xvar, yvar, dataframe, min_protected){
  #xvar: explanatory variable
  #yvar: protected status variable
  #dataframe: dataframe containing data
  #min_protected: keep data points with percent protected above this value
  data_sub <- dataframe[dataframe[,yvar] > min_protected, ]
  plot(data_sub[,yvar] ~ data_sub[,xvar], data=data_sub, pch=20, xlab=xvar, ylab=yvar)
  plot_cor <- round(cor(data_sub[,yvar], data_sub[,xvar], use='pairwise.complete.obs'),3)
  mtext(side=3, paste0("r = ", plot_cor))
  plot_cor = NULL
  data_sub = NULL
}

# function that makes 3 boxplots with pre-merged PADUS-LakeCat data (GAPS 1-2)
tri_boxplot <- function(dataframe, yvar, ylimits){
  #dataframe: dataframe containing PADUS and other data to plot, with % protected cutoffs of 75, 90 and 100
  #yvar: y axis variable (for which comparing protected vs. unprotected)
  #ylimits: y axis limits for easy manual adjustment for all plots
  melt_colnames <- c('level','variable','percent')
  test <- melt(dataframe, id.vars='ProtectGAP12Cat_75',measure.vars=yvar,na.rm=T)
  colnames(test) <- melt_colnames
  test$level <- as.factor(test[,1])
  test2 <- melt(dataframe, id.vars='ProtectGAP12Cat_90',measure.vars=yvar,na.rm=T)
  colnames(test2) <- melt_colnames
  test2$level <- as.factor(test2[,1])
  test3 <- melt(dataframe, id.vars='ProtectGAP12Cat_100',measure.vars=yvar,na.rm=T)
  colnames(test3) <- melt_colnames
  test3$level <- as.factor(test3[,1])
  xlabz <- c('Protected','Unprotected')
  titlez <- c('Threshold: 75%', 'Threshold: 90%', 'Threshold: 100%')
  boxplot(percent ~ level, data=test, las=1, col=c('green','gray'), ylim=ylimits, names=xlabz, outline=F, main=titlez[1], ylab=yvar)
  boxplot(percent ~ level, data=test2, las=1, col=c('green','gray'), ylim=ylimits, names=xlabz, outline=F, main=titlez[2])
  boxplot(percent ~ level, data=test3, las=1, col=c('green','gray'), ylim=ylimits, names=xlabz, outline=F, main=titlez[3])
  test=NULL
  test2=NULL
  test3=NULL
}

# same as previous function, but with 6 plots for GAPS 1-2 and 1-3
six_boxplot <- function(dataframe, yvar, ylimits){
  #dataframe: dataframe containing PADUS and other data to plot, with % protected cutoffs of 75, 90 and 100
  #yvar: y axis variable (for which comparing protected vs. unprotected)
  #ylimits: y axis limits for easy manual adjustment for all plots
  par(mfrow=c(2,3))
  melt_colnames <- c('level','variable','percent')
  
  # GAPS 1-2 (strict protection)
  test <- melt(dataframe, id.vars='ProtectGAP12Cat_75',measure.vars=yvar,na.rm=T)
  colnames(test) <- melt_colnames
  test$level <- as.factor(test[,1])
  test2 <- melt(dataframe, id.vars='ProtectGAP12Cat_90',measure.vars=yvar,na.rm=T)
  colnames(test2) <- melt_colnames
  test2$level <- as.factor(test2[,1])
  test3 <- melt(dataframe, id.vars='ProtectGAP12Cat_100',measure.vars=yvar,na.rm=T)
  colnames(test3) <- melt_colnames
  test3$level <- as.factor(test3[,1])
  
  # GAPS 1-3 (strict protection + multi-use landscapes)
  test4 <- melt(dataframe, id.vars='ProtectGAP123Cat_75',measure.vars=yvar,na.rm=T)
  colnames(test4) <- melt_colnames
  test4$level <- as.factor(test4[,1])
  test5 <- melt(dataframe, id.vars='ProtectGAP123Cat_90',measure.vars=yvar,na.rm=T)
  colnames(test5) <- melt_colnames
  test5$level <- as.factor(test5[,1])
  test6 <- melt(dataframe, id.vars='ProtectGAP123Cat_100',measure.vars=yvar,na.rm=T)
  colnames(test6) <- melt_colnames
  test6$level <- as.factor(test6[,1])
  
  # plotting
  xlabz <- c('Protected','Unprotected')
  titlez <- c('Threshold: 75%', 'Threshold: 90%', 'Threshold: 100%')
  boxplot(percent ~ level, data=test, las=1, col=c('green','gray'), ylim=ylimits, names=xlabz, outline=T, main=titlez[1], ylab=yvar, xlab='GAPS 1-2')
  boxplot(percent ~ level, data=test2, las=1, col=c('green','gray'), ylim=ylimits, names=xlabz, outline=T, main=titlez[2], xlab='GAPS 1-2')
  boxplot(percent ~ level, data=test3, las=1, col=c('green','gray'), ylim=ylimits, names=xlabz, outline=T, main=titlez[3], xlab='GAPS 1-2')
  boxplot(percent ~ level, data=test4, las=1, col=c('dodgerblue','gray'), ylim=ylimits, names=xlabz, outline=T, main=titlez[1], ylab=yvar, xlab='GAPS 1-3')
  boxplot(percent ~ level, data=test5, las=1, col=c('dodgerblue','gray'), ylim=ylimits, names=xlabz, outline=T, main=titlez[2], xlab='GAPS 1-3')
  boxplot(percent ~ level, data=test6, las=1, col=c('dodgerblue','gray'), ylim=ylimits, names=xlabz, outline=T, main=titlez[3], xlab='GAPS 1-3')
  
  # clear crap
  test=NULL
  test2=NULL
  test3=NULL
  test4=NULL
  test5=NULL
  test6=NULL
  xlabz=NULL
  titlez=NULL
  melt_colnames=NULL
}

############## Main program #############
# Calculate total catchment and watershed protection for GAPS 1-3
PADUS_LakeCat$PctGAP_Status12Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat
PADUS_LakeCat$PctGAP_Status123Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat + PADUS_LakeCat$PctGAP_Status3Cat
PADUS_LakeCat$PctGAP_Status12Ws <- PADUS_LakeCat$PctGAP_Status1Ws + PADUS_LakeCat$PctGAP_Status2Ws
PADUS_LakeCat$PctGAP_Status123Ws <- PADUS_LakeCat$PctGAP_Status1Ws + PADUS_LakeCat$PctGAP_Status2Ws + PADUS_LakeCat$PctGAP_Status3Ws

# create columns for protected vs. unprotected (based on different % catchment protected thresholds)
# for 75, 90 and 100 % catchment protection (individually for GAP Status 1-2 and 1-3)
PADUS_LakeCat$ProtectGAP12Cat_75 <- ifelse(PADUS_LakeCat$PctGAP_Status12Cat >= 75, "Protected75", "Unprotected75")
PADUS_LakeCat$ProtectGAP123Cat_75 <- ifelse(PADUS_LakeCat$PctGAP_Status123Cat >= 75, "Protected75", "Unprotected75")
PADUS_LakeCat$ProtectGAP12Cat_90 <- ifelse(PADUS_LakeCat$PctGAP_Status12Cat >= 90, "Protected90", "Unprotected90")
PADUS_LakeCat$ProtectGAP123Cat_90 <- ifelse(PADUS_LakeCat$PctGAP_Status123Cat >= 90, "Protected90", "Unprotected90")
PADUS_LakeCat$ProtectGAP12Cat_100 <- ifelse(PADUS_LakeCat$PctGAP_Status12Cat >= 100, "Protected100", "Unprotected100")
PADUS_LakeCat$ProtectGAP123Cat_100 <- ifelse(PADUS_LakeCat$PctGAP_Status123Cat >= 100, "Protected100", "Unprotected100")

# for 75, 90 and 100 % watershed protection (individually for GAP Status 1-2 and 1-3)
PADUS_LakeCat$ProtectGAP12Ws75 <- ifelse(PADUS_LakeCat$PctGAP_Status12Ws >= 75, "Protected75", "Unprotected75")
PADUS_LakeCat$ProtectGAP123Ws75 <- ifelse(PADUS_LakeCat$PctGAP_Status123Ws >= 75, "Protected75", "Unprotected75")
PADUS_LakeCat$ProtectGAP12Ws90 <- ifelse(PADUS_LakeCat$PctGAP_Status12Ws >= 90, "Protected90", "Unprotected90")
PADUS_LakeCat$ProtectGAP123Ws90 <- ifelse(PADUS_LakeCat$PctGAP_Status123Ws >= 90, "Protected90", "Unprotected90")
PADUS_LakeCat$ProtectGAP12Ws100 <- ifelse(PADUS_LakeCat$PctGAP_Status12Ws >= 100, "Protected100", "Unprotected100")
PADUS_LakeCat$ProtectGAP123Ws100 <- ifelse(PADUS_LakeCat$PctGAP_Status123Ws >= 100, "Protected100", "Unprotected100")


#### Topography ####
# elevation vs. protected
PADUS_elevation <- full_join(PADUS_LakeCat, elevation, by='COMID')
expo_plot(xvar='ElevCat', yvar='PctGAP_Status12Cat', dataframe=PADUS_elevation, min_protected=min_protected)
expo_plot(xvar='ElevCat', yvar='PctGAP_Status123Cat', dataframe=PADUS_elevation, min_protected=min_protected)

# Topographic wetness index vs. protected
PADUS_WetIndex <- full_join(PADUS_LakeCat, WetIndex, by='COMID')
expo_plot(xvar='WetIndexCat', yvar='PctGAP_Status12Cat', dataframe=PADUS_WetIndex, min_protected=min_protected)
expo_plot(xvar='WetIndexCat', yvar='PctGAP_Status123Cat', dataframe=PADUS_WetIndex, min_protected=min_protected)

#### Development ####
# road density vs. protected
PADUS_RoadDensity <- full_join(PADUS_LakeCat, RoadDensity, by='COMID')
expo_plot(xvar='RdDensCat', yvar='PctGAP_Status12Cat', dataframe=PADUS_RoadDensity, min_protected=min_protected)
expo_plot(xvar='RdDensCat', yvar='PctGAP_Status123Cat', dataframe=PADUS_RoadDensity, min_protected=min_protected)

# Land use/cover vs. protected
NLCD_2011$PctTotalForest2011Cat <- NLCD_2011$PctConif2011Cat + NLCD_2011$PctDecid2011Cat + NLCD_2011$PctMxFst2011Cat
NLCD_2011$PctTotalAg2011Cat <- NLCD_2011$PctCrop2011Cat + NLCD_2011$PctHay2011Cat
NLCD_2011$PctTotalWetland2011Cat <- NLCD_2011$PctWdWet2011Cat + NLCD_2011$PctHbWet2011Cat
PADUS_NLCD2011 <- full_join(PADUS_LakeCat, NLCD_2011, by='COMID')
PADUS_Impervious <- full_join(PADUS_LakeCat, Impervious, by='COMID')

nlcd_vars <- c(names(NLCD_2011)[9:22], names(NLCD_2011)[39:41])

for (i in 1:length(nlcd_vars)){
  expo_plot(xvar=nlcd_vars[i], yvar='PctGAP_Status12Cat', dataframe=PADUS_NLCD2011, min_protected=min_protected)
}

for (i in 1:length(nlcd_vars)){
  expo_plot(xvar=nlcd_vars[i], yvar='PctGAP_Status123Cat', dataframe=PADUS_NLCD2011, min_protected=min_protected)
}

# Mines (mines/sq km)
PADUS_Mines <- full_join(PADUS_LakeCat, Mines, by='COMID')
expo_plot(xvar='MineDensCat', yvar='PctGAP_Status12Cat', dataframe=PADUS_Mines, min_protected=min_protected)
expo_plot(xvar='MineDensCat', yvar='PctGAP_Status123Cat', dataframe=PADUS_Mines, min_protected=min_protected)

## Hydrology
# Dams (dams/sq km)
PADUS_Dams <- full_join(PADUS_LakeCat, Dams, by='COMID')
expo_plot(xvar='NABD_DensCat', yvar='PctGAP_Status12Cat', dataframe=PADUS_Dams, min_protected=min_protected)
expo_plot(xvar='NABD_DensCat', yvar='PctGAP_Status123Cat', dataframe=PADUS_Dams, min_protected=min_protected)
# runoff and baseflow
PADUS_runoff <- full_join(PADUS_LakeCat, Runoff, by='COMID')
PADUS_baseflow <- full_join(PADUS_LakeCat, Baseflow, by='COMID')

# Toxic pollution
PADUS_Toxic <- full_join(PADUS_LakeCat, Toxic, by='COMID')
# toxic release inventory sites (TRI)/sq km
expo_plot(xvar='TRIDensCat', yvar='PctGAP_Status12Cat', dataframe=PADUS_Toxic, min_protected=min_protected)
expo_plot(xvar='TRIDensCat', yvar='PctGAP_Status123Cat', dataframe=PADUS_Toxic, min_protected=min_protected)
# superfund sites/sq km
expo_plot(xvar='SuperfundDensCat', yvar='PctGAP_Status12Cat', dataframe=PADUS_Toxic, min_protected=min_protected)
expo_plot(xvar='SuperfundDensCat', yvar='PctGAP_Status123Cat', dataframe=PADUS_Toxic, min_protected=min_protected)
# natl pollutant discharge elimination system sites/sq km
expo_plot(xvar='NPDESDensCat', yvar='PctGAP_Status12Cat', dataframe=PADUS_Toxic, min_protected=min_protected)
expo_plot(xvar='NPDESDensCat', yvar='PctGAP_Status123Cat', dataframe=PADUS_Toxic, min_protected=min_protected)

# NADP (deposition) (kg sulfur + nitrogen/ha/yr)(2008)
PADUS_Deposition <- full_join(PADUS_LakeCat, Deposition, by='COMID')
expo_plot(xvar='SN_2008Cat', yvar='PctGAP_Status12Cat', dataframe=PADUS_Deposition, min_protected=min_protected)
expo_plot(xvar='SN_2008Cat', yvar='PctGAP_Status123Cat', dataframe=PADUS_Deposition, min_protected=min_protected)

# Forest loss by year
ForestLoss$TotalPctFrstLossCat <- rowSums(ForestLoss[,7:19])
PADUS_ForestLoss <- full_join(PADUS_LakeCat, ForestLoss, by='COMID')
expo_plot(xvar='TotalPctFrstLossCat', yvar='PctGAP_Status12Cat', dataframe=PADUS_ForestLoss, min_protected=min_protected)
expo_plot(xvar='TotalPctFrstLossCat', yvar='PctGAP_Status123Cat', dataframe=PADUS_ForestLoss, min_protected=min_protected)

# Fire perimeters
Fahr$TotalPctFireCat <- rowSums(Fahr[,7:17])
PADUS_Fahr <- full_join(PADUS_LakeCat, Fahr, by='COMID')
expo_plot(xvar='TotalPctFireCat', yvar='PctGAP_Status12Cat', dataframe=PADUS_Fahr, min_protected=min_protected)
expo_plot(xvar='TotalPctFireCat', yvar='PctGAP_Status123Cat', dataframe=PADUS_Fahr, min_protected=min_protected)

#### Climate ####
# climate vs. protected
PADUS_PRISM <- full_join(PADUS_LakeCat, PRISM, by='COMID') 
expo_plot(xvar='Precip8110Cat', yvar='PctGAP_Status12Cat', dataframe=PADUS_PRISM, min_protected=min_protected)
expo_plot(xvar='Precip8110Cat', yvar='PctGAP_Status123Cat', dataframe=PADUS_PRISM, min_protected=min_protected)
expo_plot(xvar='Tmean8110Cat', yvar='PctGAP_Status12Cat', dataframe=PADUS_PRISM, min_protected=min_protected)
expo_plot(xvar='Tmean8110Cat', yvar='PctGAP_Status123Cat', dataframe=PADUS_PRISM, min_protected=min_protected)

####### Protection level analysis and mapping protected lakes ###
par(mfrow=c(2,2))
boxplot(PADUS_LakeCat$PctGAP_Status12Cat, las=1, ylab='Percent protected', main='GAPS 1-2')
boxplot(PADUS_LakeCat$PctGAP_Status123Cat, las=1, ylab='Percent protected', main='GAPS 1-3')
hist(PADUS_LakeCat$PctGAP_Status12Cat, main='GAPS 1-2')
hist(PADUS_LakeCat$PctGAP_Status123Cat, main='GAPS 1-3')

hist(PADUS_LakeCat$PctGAP_Status12Cat, main='GAPS 1-2', freq=F)
hist(PADUS_LakeCat$PctGAP_Status123Cat, main='GAPS 1-3',freq=F)

## How many lakes with 100% catchment protected?
# GAPS 1-2
GAP12_100pct_protected_Cat <- subset(PADUS_LakeCat, PctGAP_Status12Cat >= 100)
nrow(GAP12_100pct_protected_Cat)
GAP12_100pct_protected_Cat_COMID <- GAP12_100pct_protected_Cat$COMID
NHD_100pct_protected_Cat_GAP12 <- subset(NHD_pts, COMID %in% GAP12_100pct_protected_Cat_COMID)
par(mfrow=c(1,1))
# plot(lower48)
# plot(NHD_100pct_protected_Cat_GAP12, add=T, col='darkgreen', pch=20)
# title("NHD lakes with 100% catchment protected GAPS 1-2")
# mtext(side=3, paste0("n = ", length(GAP12_100pct_protected_Cat_COMID)))

# GAPS 1-3
GAP123_100pct_protected_Cat <- subset(PADUS_LakeCat, PctGAP_Status123Cat >= 100)
nrow(GAP123_100pct_protected_Cat)
GAP123_100pct_protected_Cat_COMID <- GAP123_100pct_protected_Cat$COMID
NHD_100pct_protected_Cat_GAP123 <- subset(NHD_pts, COMID %in% GAP123_100pct_protected_Cat_COMID)
# plot(lower48)
# plot(NHD_100pct_protected_Cat_GAP123, add=T, col='lightgreen', pch=20)
# title("NHD lakes with 100% catchment protected GAPS 1-3")
# mtext(side=3, paste0("n = ", length(GAP123_100pct_protected_Cat_COMID)))

plot(lower48)
plot(NHD_100pct_protected_Cat_GAP123, add=T, col='lightgreen', pch=20)
plot(NHD_100pct_protected_Cat_GAP12, add=T, col='darkgreen', pch=20)
legend('bottomleft', legend=c('GAPS 1-2', 'GAPS 1-3'), col=c('darkgreen','lightgreen'), pch=c(16,16))
title("NHD lakes with 100% catchment protected")
mtext(side=3, paste0("GAPS 1-2: ", length(GAP12_100pct_protected_Cat_COMID), " lakes", ", GAPS 1-3: ", length(GAP123_100pct_protected_Cat_COMID), " lakes"))

# Percent of NHD lakes with 100% catchment protection
nrow(NHD_100pct_protected_Cat_GAP12)/nrow(NHD_pts)
nrow(NHD_100pct_protected_Cat_GAP123)/nrow(NHD_pts)

## How many lakes with 100% watershed protected?
# GAPS 1-2
GAP12_100pct_protected_Ws <- subset(PADUS_LakeCat, PctGAP_Status12Ws >= 100)
nrow(GAP12_100pct_protected_Ws)
GAP12_100pct_protected_Ws_COMID <- GAP12_100pct_protected_Ws$COMID
NHD_100pct_protected_Ws_GAP12 <- subset(NHD_pts, COMID %in% GAP12_100pct_protected_Ws_COMID)
par(mfrow=c(1,1))
# plot(lower48)
# plot(NHD_100pct_protected_Ws_GAP12, add=T, col='darkgreen', pch=20)
# title("NHD lakes with 100% watershed protected GAPS 1-2")
# mtext(side=3, paste0("n = ", length(GAP12_100pct_protected_Ws_COMID)))

# GAPS 1-3
GAP123_100pct_protected_Ws <- subset(PADUS_LakeCat, PctGAP_Status123Ws >= 100)
nrow(GAP123_100pct_protected_Ws)
GAP123_100pct_protected_Ws_COMID <- GAP123_100pct_protected_Ws$COMID
NHD_100pct_protected_Ws_GAP123 <- subset(NHD_pts, COMID %in% GAP123_100pct_protected_Ws_COMID)
# plot(lower48)
# plot(NHD_100pct_protected_Ws_GAP123, add=T, col='lightgreen', pch=20)
# title("NHD lakes with 100% watershed protected GAPS 1-3")
# mtext(side=3, paste0("n = ", length(GAP123_100pct_protected_Ws_COMID)))

plot(lower48)
plot(NHD_100pct_protected_Ws_GAP123, add=T, col='lightgreen', pch=20)
plot(NHD_100pct_protected_Ws_GAP12, add=T, col='darkgreen', pch=20)
legend('bottomleft', legend=c('GAPS 1-2', 'GAPS 1-3'), col=c('darkgreen','lightgreen'), pch=c(16,16))
title("NHD lakes with 100% watershed protected")
mtext(side=3, paste0("GAPS 1-2: ", length(GAP12_100pct_protected_Ws_COMID), " lakes", ", GAPS 1-3: ", length(GAP123_100pct_protected_Ws_COMID), " lakes"))

# Percent of NHD lakes with 100% watershed protection
nrow(NHD_100pct_protected_Ws_GAP12)/nrow(NHD_pts)
nrow(NHD_100pct_protected_Ws_GAP123)/nrow(NHD_pts)

## Because histogram so hard to read, generate table of # and % of NHD lakes at different levels of protection
# GAPS 1-2
NHD_protected_table_GAP12 <- data.frame(matrix(nrow=2, ncol=12))
colnames(NHD_protected_table_GAP12) <- c('GAP12_0pct','GAP12_0_9pct','GAP12_10_19pct','GAP12_20_29pct',
                                  'GAP12_30_39pct','GAP12_40_49pct','GAP12_50_59pct','GAP12_60_69pct',
                                  'GAP12_70_79pct','GAP12_80_89pct','GAP12_90_99pct','GAP12_100pct')
rownames(NHD_protected_table_GAP12) <- c('pct_lakes','nlakes')
NHD_protected_table_GAP12[1,1] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat ==0))/nrow(NHD_pts)
NHD_protected_table_GAP12[2,1] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat ==0))
NHD_protected_table_GAP12[1,2] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat > 0 & PctGAP_Status12Cat < 9.5))/nrow(NHD_pts)
NHD_protected_table_GAP12[2,2] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat > 0 & PctGAP_Status12Cat < 9.5))
NHD_protected_table_GAP12[1,3] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 9.5 & PctGAP_Status12Cat < 19.5))/nrow(NHD_pts)
NHD_protected_table_GAP12[2,3] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 9.5 & PctGAP_Status12Cat < 19.5))
NHD_protected_table_GAP12[1,4] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 19.5 & PctGAP_Status12Cat < 29.5))/nrow(NHD_pts)
NHD_protected_table_GAP12[2,4] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 19.5 & PctGAP_Status12Cat < 29.5))
NHD_protected_table_GAP12[1,5] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 29.5 & PctGAP_Status12Cat < 39.5))/nrow(NHD_pts)
NHD_protected_table_GAP12[2,5] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 29.5 & PctGAP_Status12Cat < 39.5))
NHD_protected_table_GAP12[1,6] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 39.5 & PctGAP_Status12Cat < 49.5))/nrow(NHD_pts)
NHD_protected_table_GAP12[2,6] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 39.5 & PctGAP_Status12Cat < 49.5))
NHD_protected_table_GAP12[1,7] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 49.5 & PctGAP_Status12Cat < 59.5))/nrow(NHD_pts)
NHD_protected_table_GAP12[2,7] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 49.5 & PctGAP_Status12Cat < 59.5))
NHD_protected_table_GAP12[1,8] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 59.5 & PctGAP_Status12Cat < 69.5))/nrow(NHD_pts)
NHD_protected_table_GAP12[2,8] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 59.5 & PctGAP_Status12Cat < 69.5))
NHD_protected_table_GAP12[1,9] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 69.5 & PctGAP_Status12Cat < 79.5))/nrow(NHD_pts)
NHD_protected_table_GAP12[2,9] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 69.5 & PctGAP_Status12Cat < 79.5))
NHD_protected_table_GAP12[1,10] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 79.5 & PctGAP_Status12Cat < 89.5))/nrow(NHD_pts)
NHD_protected_table_GAP12[2,10] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 79.5 & PctGAP_Status12Cat < 89.5))
NHD_protected_table_GAP12[1,11] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 89.5 & PctGAP_Status12Cat < 99.5))/nrow(NHD_pts)
NHD_protected_table_GAP12[2,11] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 89.5 & PctGAP_Status12Cat < 99.5))
NHD_protected_table_GAP12[1,12] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 99.5))/nrow(NHD_pts)
NHD_protected_table_GAP12[2,12] <- nrow(subset(PADUS_LakeCat, PctGAP_Status12Cat >= 99.5))
NHD_protected_table_GAP12[1,] <- round(NHD_protected_table_GAP12[1,], 4) #round off first row

# GAPS 1-3
# Because histogram so hard to read, generate table of # and % of NHD lakes at different levels of protection
NHD_protected_table_GAP123 <- data.frame(matrix(nrow=2, ncol=12))
colnames(NHD_protected_table_GAP123) <- c('GAP123_0pct','GAP123_0_9pct','GAP123_10_19pct','GAP123_20_29pct',
                                          'GAP123_30_39pct','GAP123_40_49pct','GAP123_50_59pct','GAP123_60_69pct',
                                          'GAP123_70_79pct','GAP123_80_89pct','GAP123_90_99pct','GAP123_100pct')
rownames(NHD_protected_table_GAP123) <- c('pct_lakes','nlakes')
NHD_protected_table_GAP123[1,1] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat ==0))/nrow(NHD_pts)
NHD_protected_table_GAP123[2,1] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat ==0))
NHD_protected_table_GAP123[1,2] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat > 0 & PctGAP_Status123Cat < 9.5))/nrow(NHD_pts)
NHD_protected_table_GAP123[2,2] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat > 0 & PctGAP_Status123Cat < 9.5))
NHD_protected_table_GAP123[1,3] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 9.5 & PctGAP_Status123Cat < 19.5))/nrow(NHD_pts)
NHD_protected_table_GAP123[2,3] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 9.5 & PctGAP_Status123Cat < 19.5))
NHD_protected_table_GAP123[1,4] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 19.5 & PctGAP_Status123Cat < 29.5))/nrow(NHD_pts)
NHD_protected_table_GAP123[2,4] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 19.5 & PctGAP_Status123Cat < 29.5))
NHD_protected_table_GAP123[1,5] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 29.5 & PctGAP_Status123Cat < 39.5))/nrow(NHD_pts)
NHD_protected_table_GAP123[2,5] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 29.5 & PctGAP_Status123Cat < 39.5))
NHD_protected_table_GAP123[1,6] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 39.5 & PctGAP_Status123Cat < 49.5))/nrow(NHD_pts)
NHD_protected_table_GAP123[2,6] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 39.5 & PctGAP_Status123Cat < 49.5))
NHD_protected_table_GAP123[1,7] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 49.5 & PctGAP_Status123Cat < 59.5))/nrow(NHD_pts)
NHD_protected_table_GAP123[2,7] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 49.5 & PctGAP_Status123Cat < 59.5))
NHD_protected_table_GAP123[1,8] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 59.5 & PctGAP_Status123Cat < 69.5))/nrow(NHD_pts)
NHD_protected_table_GAP123[2,8] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 59.5 & PctGAP_Status123Cat < 69.5))
NHD_protected_table_GAP123[1,9] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 69.5 & PctGAP_Status123Cat < 79.5))/nrow(NHD_pts)
NHD_protected_table_GAP123[2,9] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 69.5 & PctGAP_Status123Cat < 79.5))
NHD_protected_table_GAP123[1,10] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 79.5 & PctGAP_Status123Cat < 89.5))/nrow(NHD_pts)
NHD_protected_table_GAP123[2,10] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 79.5 & PctGAP_Status123Cat < 89.5))
NHD_protected_table_GAP123[1,11] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 89.5 & PctGAP_Status123Cat < 99.5))/nrow(NHD_pts)
NHD_protected_table_GAP123[2,11] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 89.5 & PctGAP_Status123Cat < 99.5))
NHD_protected_table_GAP123[1,12] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 99.5))/nrow(NHD_pts)
NHD_protected_table_GAP123[2,12] <- nrow(subset(PADUS_LakeCat, PctGAP_Status123Cat >= 99.5))
NHD_protected_table_GAP123[1,] <- round(NHD_protected_table_GAP123[1,], 4) #round off first row

# Do protected local catchments tend to have protected watersheds? 
par(mfrow=c(1,1))
plot(PADUS_LakeCat$PctGAP_Status12Cat ~ PADUS_LakeCat$PctGAP_Status12Ws, pch=20, main='GAPS 1-2', xlab='Ws', ylab='Cat')
plot(PADUS_LakeCat$PctGAP_Status123Cat ~ PADUS_LakeCat$PctGAP_Status123Ws, pch=20, main='GAPS 1-3', xlab='Ws', ylab='Cat')


##### Comparing protected vs. unprotected lakes (using different definitions of "protected") based on % watershed protected
# par(mfrow=c(1,3))
# tri_boxplot(dataframe=PADUS_ForestLoss, yvar="TotalPctFrstLossCat", ylimits=c(0,5))
# tri_boxplot(dataframe=PADUS_Fahr, yvar="TotalPctFireCat", ylimits=c(0,100))
# tri_boxplot(dataframe=PADUS_PRISM, yvar="Tmean8110Cat", ylimits=c(-10,30))
# tri_boxplot(dataframe=PADUS_PRISM, yvar="Precip8110Cat", ylimits=c(0,2500))
# tri_boxplot(dataframe=PADUS_elevation, yvar="ElevCat", ylimits=c(-100,1500))

# Climate
six_boxplot(dataframe=PADUS_PRISM, yvar="Precip8110Cat", ylimits=c(0,2500)) #mm
six_boxplot(dataframe=PADUS_PRISM, yvar="Tmean8110Cat", ylimits=c(-10,30)) #degC

# Topography
six_boxplot(PADUS_elevation, yvar="ElevCat", ylimits=c(-100,4500)) #meters
six_boxplot(PADUS_WetIndex, yvar="WetIndexCat", ylimits=c(0,2000)) #index

# Land use/cover
six_boxplot(PADUS_NLCD2011, yvar="PctTotalForest2011Cat", ylimits=c(0,100)) #percent
six_boxplot(PADUS_NLCD2011, yvar="PctConif2011Cat", ylimits=c(0,100)) #percent
six_boxplot(PADUS_NLCD2011, yvar="PctMxFst2011Cat", ylimits=c(0,100)) #percent
six_boxplot(PADUS_NLCD2011, yvar="PctDecid2011Cat", ylimits=c(0,100)) #percent
six_boxplot(PADUS_NLCD2011, yvar="PctTotalAg2011Cat", ylimits=c(0,100)) #percent
six_boxplot(PADUS_NLCD2011, yvar="PctTotalWetland2011Cat", ylimits=c(0,100)) #percent
six_boxplot(PADUS_RoadDensity, yvar="RdDensCat", ylimits=c(0,30)) #km rds/sq km
six_boxplot(PADUS_Impervious, yvar='PctImp2011Cat', ylimits=c(0,10)) #percent

six_boxplot(PADUS_Mines, yvar="MineDensCat", ylimits=c(0,1)) #mines/sq km #same zero problem visually
PADUS_Mines_sub <- subset(PADUS_Mines, MineDensCat > 0)
six_boxplot(PADUS_Mines_sub, yvar="MineDensCat", ylimits=c(0,5))

# Local catchment area
six_boxplot(PADUS_LakeCat, yvar="CatAreaSqKm", ylimits=c(0,5)) #sq km

# Disturbance
six_boxplot(PADUS_Fahr, yvar="TotalPctFireCat", ylimits=c(0,10)) #percent (So few lakes with fire; need to subset before plotting)
six_boxplot(PADUS_ForestLoss, yvar="TotalPctFrstLossCat", ylimits=c(0,10)) #percent

### Lake morphometry
NHD_pts@data$COMID <- as.numeric(NHD_pts@data$COMID) #COMID was character in NHD; must convert
PADUS_NHD <- left_join(PADUS_LakeCat, NHD_pts@data, by='COMID')
PADUS_NHD <- PADUS_NHD[!duplicated(PADUS_NHD$COMID),] #remove duplicate COMID (6 for some reason)

# lake area
six_boxplot(PADUS_NHD, yvar="AREASQKM", ylimits=c(0,10)) #sq km

# max depth
PADUS_NHD_maxdepth <- subset(PADUS_NHD, MaxDepth > 0) #think it's meters...
six_boxplot(PADUS_NHD_maxdepth, yvar="MaxDepth", ylimits=c(0,20))

# Hydrology
six_boxplot(PADUS_Dams, yvar="NABD_DensCat", ylimits=c(0,1)) #dams/sq km #so many zeros that doesn't work visually
PADUS_Dams_sub <- subset(PADUS_Dams, NABD_DensCat > 0)
six_boxplot(PADUS_Dams_sub, yvar="NABD_DensCat", ylimits=c(0,15))

six_boxplot(PADUS_baseflow, yvar='BFICat', ylimits=c(0,100)) #percent of total inflow that is baseflow
six_boxplot(PADUS_runoff, yvar='RunoffCat', ylimits=c(0,1500)) #mm/month 1971-2000

# Seriously, I hate ggplot
# ggplot(testz, aes(variable, percent, fill=level)) +
#   geom_boxplot(outlier.shape=NA) +
#   ylim(0,5) +
#   scale_fill_manual(values=c('dodgerblue','gray','dodgerblue','gray','dodgerblue','gray')) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   scale_x_discrete(breaks=c("Protected75","Unprotected75","Protected90","Unprotected90","Protected100","Unprotected100"),
#                    labels=c("75", "75", "90","90","100","100"))
