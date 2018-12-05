####################### Lake protection by US state ############################################
# Date: 12-5-18
# updated:
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(rgeos)
library(dplyr)
library(tmap)

#### Input data ####
setwd('C:/Users/FWL/Documents/FreshwaterConservation')

PADUS_table <- read.csv("Data/PADUS.csv")

lower48 <- shapefile("C:/Ian_GIS/cb_2016_us_state_500k/lower48.shp")

NHD_pts <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_waterbody_pts.shp")
NHD_pts_lakes <- subset(NHD_pts, FTYPE=='LakePond' | FTYPE=='Reservoir')

#### D-fine functions ####
protected_lakes_by_state <- function(NHD_pts_lakes_PADUS, lower48, protection_cutoff){
  #NHD_pts_lakes_PADUS: NHD lake centroids, merged with PADUS data
  #lower48: lower48 state polygons, same crs as NHD
  #protection_cutoff: % watershed or catchment protected to be considered a protected lake
  lower48_names <- lower48@data$NAME
  # data fraome of lake IDs
  NHD_pts_lakes_PADUS@data$rowID <- rownames(NHD_pts_lakes_PADUS@data)
  rowid_NHD_df <- data.frame(rowID=NHD_pts_lakes_PADUS@data$rowID, COMID=NHD_pts_lakes_PADUS@data$COMID)
  # number of lakes per state
  # subset points that fall in each state polygon
  # sp::over doesn't retain attribute data from points, so create data frame to join those data back later based on rowid
  lower48_COMID <- sp::over(NHD_pts_lakes_PADUS, lower48, returnList = F)
  lower48_COMID$joinID <- rownames(lower48_COMID)
  lower48_COMID <- merge(lower48_COMID, rowid_NHD_df, by.x='joinID', by.y='rowID')
  
  # get rid of factor; would cause problems later
  lower48_COMID$COMID <- as.numeric(levels(lower48_COMID$COMID))[lower48_COMID$COMID]
  
  # define protected lakes based on % Ws and Cat protected
  protected_lakes_gap12Cat <- subset(NHD_pts_lakes_PADUS, PctGAP_Status12Cat >= protection_cutoff)
  protected_lakes_gap123Cat <- subset(NHD_pts_lakes_PADUS, PctGAP_Status123Cat >= protection_cutoff)
  protected_lakes_gap12Ws <- subset(NHD_pts_lakes_PADUS, PctGAP_Status12Ws >= protection_cutoff)
  protected_lakes_gap123Ws <- subset(NHD_pts_lakes_PADUS, PctGAP_Status123Ws >= protection_cutoff)
  
  # number of protected lakes by state
  lower48_lakes_bystate_gap12Cat <- colSums(gContains(lower48, protected_lakes_gap12Cat, byid = T))
  lower48_lakes_bystate_gap123Cat <- colSums(gContains(lower48, protected_lakes_gap123Cat, byid = T))
  lower48_lakes_bystate_gap12Ws <- colSums(gContains(lower48, protected_lakes_gap12Ws, byid = T))
  lower48_lakes_bystate_gap123Ws <- colSums(gContains(lower48, protected_lakes_gap123Ws, byid = T))
  #setNames(lower48_lakes_bystate, lower48_names)
  lower48_protected_lakes_DF <- data.frame(State=lower48_names, ProtectedLakes_gap12Cat=lower48_lakes_bystate_gap12Cat,
                                           ProtectedLakes_gap123Cat=lower48_lakes_bystate_gap123Cat,
                                           ProtectedLakes_gap12Ws=lower48_lakes_bystate_gap12Ws,
                                           ProtectedLakes_gap123Ws=lower48_lakes_bystate_gap123Ws)
  
  # proportion of protected lakes by state (out of total lakes in each state)
  # count number of rows (COMIDs, therefore lakes) per unique state
  lake_countz_state <- lower48_COMID %>%
    group_by(NAME) %>%
    tally()
  colnames(lake_countz_state) <- c("State","nLakes")
  lake_countz_protected_lower48 <- merge(lake_countz_state, lower48_protected_lakes_DF, by="State", all.x=F)
  lake_countz_protected_lower48$PropProtected_gap12Cat <- lake_countz_protected_lower48$ProtectedLakes_gap12Cat/lake_countz_protected_lower48$nLakes
  lake_countz_protected_lower48$PropProtected_gap123Cat <- lake_countz_protected_lower48$ProtectedLakes_gap123Cat/lake_countz_protected_lower48$nLakes
  lake_countz_protected_lower48$PropProtected_gap12Ws <- lake_countz_protected_lower48$ProtectedLakes_gap12Ws/lake_countz_protected_lower48$nLakes
  lake_countz_protected_lower48$PropProtected_gap123Ws <- lake_countz_protected_lower48$ProtectedLakes_gap123Ws/lake_countz_protected_lower48$nLakes
  # clean up for next iteration
  protected_lakes_gap12Cat <- NULL
  protected_lakes_gap123Cat <- NULL
  protected_lakes_gap12Ws <- NULL
  protected_lakes_gap123Ws <- NULL
  lake_countz_state <- NULL
  lower48_protected_lakes_DF <- NULL
  lower48_lakes_bystate_gap12Cat <- NULL
  lower48_lakes_bystate_gap123Cat <- NULL
  lower48_lakes_bystate_gap12Ws <- NULL
  lower48_lakes_bystate_gap123Ws <- NULL
  lower48_COMID <- NULL
  rowid_NHD_df <- NULL
  lower48_names <- NULL
  rowid_NHD_df <- NULL
  return(lake_countz_protected_lower48)
}

############################ Main program #####################################
## Number and proportion of protected lakes per state
NHD_pts_lakes_PADUS <- merge(NHD_pts_lakes, PADUS_table, by='COMID', all=F)

# calculate total protection for GAPS 1-2 and GAPS 1-3
NHD_pts_lakes_PADUS$PctGAP_Status12Cat <- NHD_pts_lakes_PADUS$PctGAP_Status1Cat + NHD_pts_lakes_PADUS$PctGAP_Status2Cat
NHD_pts_lakes_PADUS$PctGAP_Status123Cat <- NHD_pts_lakes_PADUS$PctGAP_Status1Cat + NHD_pts_lakes_PADUS$PctGAP_Status2Cat + NHD_pts_lakes_PADUS$PctGAP_Status3Cat
NHD_pts_lakes_PADUS$PctGAP_Status12Ws <- NHD_pts_lakes_PADUS$PctGAP_Status1Ws + NHD_pts_lakes_PADUS$PctGAP_Status2Ws
NHD_pts_lakes_PADUS$PctGAP_Status123Ws <- NHD_pts_lakes_PADUS$PctGAP_Status1Ws + NHD_pts_lakes_PADUS$PctGAP_Status2Ws + NHD_pts_lakes_PADUS$PctGAP_Status3Ws

# set protection cutoff (for local catchment and network watershed) and execute function
protected_lakes_100pct <- protected_lakes_by_state(NHD_pts_lakes_PADUS, lower48, protection_cutoff=100)
protected_lakes_90pct <- protected_lakes_by_state(NHD_pts_lakes_PADUS, lower48, protection_cutoff=90)
protected_lakes_75pct <- protected_lakes_by_state(NHD_pts_lakes_PADUS, lower48, protection_cutoff=75)

## Mapping
# 100%
lake_countz_protected100_lower48_shp <- merge(lower48, protected_lakes_100pct, by.x='NAME', by.y='State')
map_breaks <- c(0,0.01,0.05,0.1,0.25,0.5,Inf)

tm_shape(lake_countz_protected100_lower48_shp)+
  tm_fill('PropProtected_gap12Cat', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Cat',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected100_lower48_shp)+
  tm_fill('PropProtected_gap123Cat', style='fixed', title='Prop Lakes Protected, GAPS 1-3, Cat',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected100_lower48_shp)+
  tm_fill('PropProtected_gap12Ws', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Ws',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected100_lower48_shp)+
  tm_fill('PropProtected_gap123Ws', style='fixed', title='Prop Lakes Protected, GAPS 1-3, Ws',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

# 90%
lake_countz_protected90_lower48_shp <- merge(lower48, protected_lakes_90pct, by.x='NAME', by.y='State')

tm_shape(lake_countz_protected90_lower48_shp)+
  tm_fill('PropProtected_gap12Cat', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Cat',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected90_lower48_shp)+
  tm_fill('PropProtected_gap123Cat', style='fixed', title='Prop Lakes Protected, GAPS 1-3, Cat',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected90_lower48_shp)+
  tm_fill('PropProtected_gap12Ws', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Ws',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected90_lower48_shp)+
  tm_fill('PropProtected_gap123Ws', style='fixed', title='Prop Lakes Protected, GAPS 1-3, Ws',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

# 75%
lake_countz_protected75_lower48_shp <- merge(lower48, protected_lakes_75pct, by.x='NAME', by.y='State')

tm_shape(lake_countz_protected75_lower48_shp)+
  tm_fill('PropProtected_gap12Cat', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Cat',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected75_lower48_shp)+
  tm_fill('PropProtected_gap123Cat', style='fixed', title='Prop Lakes Protected, GAPS 1-3, Cat',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected75_lower48_shp)+
  tm_fill('PropProtected_gap12Ws', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Ws',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected75_lower48_shp)+
  tm_fill('PropProtected_gap123Ws', style='fixed', title='Prop Lakes Protected, GAPS 1-3, Ws',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()


## seems that states with more protected lakes have fewer lakes?
# 100%
par(mfrow=c(2,2))
plot(PropProtected_gap12Cat ~ nLakes, data=protected_lakes_100pct, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-2, Cat')
mtext(side=3, '100 pct protected')
legend('topright', bty='n', legend=paste0('r = ', round(cor(protected_lakes_100pct$PropProtected_gap12Cat, protected_lakes_100pct$nLakes, use='pairwise.complete.obs'),3)))

plot(PropProtected_gap123Cat ~ nLakes, data=protected_lakes_100pct, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-3, Cat')
mtext(side=3, '100 pct protected')
legend('topright', bty='n', legend=paste0('r = ', round(cor(protected_lakes_100pct$PropProtected_gap123Cat, protected_lakes_100pct$nLakes, use='pairwise.complete.obs'),3)))

plot(PropProtected_gap12Ws ~ nLakes, data=protected_lakes_100pct, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-2, Ws')
mtext(side=3, '100 pct protected')
legend('topright', bty='n', legend=paste0('r = ', round(cor(protected_lakes_100pct$PropProtected_gap12Ws, protected_lakes_100pct$nLakes, use='pairwise.complete.obs'),3)))

plot(PropProtected_gap123Ws ~ nLakes, data=protected_lakes_100pct, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-3, Ws')
mtext(side=3, '100 pct protected')
legend('topright', bty='n', legend=paste0('r = ', round(cor(protected_lakes_100pct$PropProtected_gap123Ws, protected_lakes_100pct$nLakes, use='pairwise.complete.obs'),3)))

# 90%
par(mfrow=c(2,2))
plot(PropProtected_gap12Cat ~ nLakes, data=protected_lakes_90pct, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-2, Cat')
mtext(side=3, '90 pct protected')
legend('topright', bty='n', legend=paste0('r = ', round(cor(protected_lakes_90pct$PropProtected_gap12Cat, protected_lakes_90pct$nLakes, use='pairwise.complete.obs'),3)))

plot(PropProtected_gap123Cat ~ nLakes, data=protected_lakes_90pct, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-3, Cat')
mtext(side=3, '90 pct protected')
legend('topright', bty='n', legend=paste0('r = ', round(cor(protected_lakes_90pct$PropProtected_gap123Cat, protected_lakes_90pct$nLakes, use='pairwise.complete.obs'),3)))

plot(PropProtected_gap12Ws ~ nLakes, data=protected_lakes_90pct, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-2, Ws')
mtext(side=3, '90 pct protected')
legend('topright', bty='n', legend=paste0('r = ', round(cor(protected_lakes_90pct$PropProtected_gap12Ws, protected_lakes_90pct$nLakes, use='pairwise.complete.obs'),3)))

plot(PropProtected_gap123Ws ~ nLakes, data=protected_lakes_90pct, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-3, Ws')
mtext(side=3, '90 pct protected')
legend('topright', bty='n', legend=paste0('r = ', round(cor(protected_lakes_90pct$PropProtected_gap123Ws, protected_lakes_90pct$nLakes, use='pairwise.complete.obs'),3)))

#75%
par(mfrow=c(2,2))
plot(PropProtected_gap12Cat ~ nLakes, data=protected_lakes_75pct, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-2, Cat')
mtext(side=3, '75 pct protected')
legend('topright', bty='n', legend=paste0('r = ', round(cor(protected_lakes_75pct$PropProtected_gap12Cat, protected_lakes_75pct$nLakes, use='pairwise.complete.obs'),3)))

plot(PropProtected_gap123Cat ~ nLakes, data=protected_lakes_75pct, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-3, Cat')
mtext(side=3, '75 pct protected')
legend('topright', bty='n', legend=paste0('r = ', round(cor(protected_lakes_75pct$PropProtected_gap123Cat, protected_lakes_75pct$nLakes, use='pairwise.complete.obs'),3)))

plot(PropProtected_gap12Ws ~ nLakes, data=protected_lakes_75pct, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-2, Ws')
mtext(side=3, '75 pct protected')
legend('topright', bty='n', legend=paste0('r = ', round(cor(protected_lakes_75pct$PropProtected_gap12Ws, protected_lakes_75pct$nLakes, use='pairwise.complete.obs'),3)))

plot(PropProtected_gap123Ws ~ nLakes, data=protected_lakes_75pct, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-3, Ws')
mtext(side=3, '75 pct protected')
legend('topright', bty='n', legend=paste0('r = ', round(cor(protected_lakes_75pct$PropProtected_gap123Ws, protected_lakes_75pct$nLakes, use='pairwise.complete.obs'),3)))