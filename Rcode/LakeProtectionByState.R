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
library(rgdal)

#### Input data ####
setwd('C:/Users/FWL/Documents/FreshwaterConservation')

PADUS_table <- read.csv("Data/PADUS.csv")

lower48 <- shapefile("C:/Ian_GIS/cb_2016_us_state_500k/lower48.shp")
NARS_regions <- shapefile("C:/Ian_GIS/NLA_ecoregions/Export_Output.shp")
NARS_regions <- spTransform(NARS_regions, crs(lower48)) #get into same crs as other data in analysis

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

# same basic function, but by NARS ecoregion
protected_lakes_by_NARS <- function(NHD_pts_lakes_PADUS, NARS_regions, protection_cutoff){
  #NHD_pts_lakes_PADUS: NHD lake centroids, merged with PADUS data
  #NARS_regions: Natl aquatic resource survey ecoregion polygons, same crs as NHD
  #protection_cutoff: % watershed or catchment protected to be considered a protected lake
  NARS_names <- NARS_regions@data$WSA9
  # data fraome of lake IDs
  NHD_pts_lakes_PADUS@data$rowID <- rownames(NHD_pts_lakes_PADUS@data)
  rowid_NHD_df <- data.frame(rowID=NHD_pts_lakes_PADUS@data$rowID, COMID=NHD_pts_lakes_PADUS@data$COMID)
  # number of lakes per region
  # subset points that fall in each region polygon
  # sp::over doesn't retain attribute data from points, so create data frame to join those data back later based on rowid
  NARS_COMID <- sp::over(NHD_pts_lakes_PADUS, NARS_regions, returnList = F)
  NARS_COMID$joinID <- rownames(NARS_COMID)
  NARS_COMID <- merge(NARS_COMID, rowid_NHD_df, by.x='joinID', by.y='rowID')
  
  # get rid of factor; would cause problems later
  NARS_COMID$COMID <- as.numeric(levels(NARS_COMID$COMID))[NARS_COMID$COMID]
  
  # define protected lakes based on % Ws and Cat protected
  protected_lakes_gap12Cat <- subset(NHD_pts_lakes_PADUS, PctGAP_Status12Cat >= protection_cutoff)
  protected_lakes_gap123Cat <- subset(NHD_pts_lakes_PADUS, PctGAP_Status123Cat >= protection_cutoff)
  protected_lakes_gap12Ws <- subset(NHD_pts_lakes_PADUS, PctGAP_Status12Ws >= protection_cutoff)
  protected_lakes_gap123Ws <- subset(NHD_pts_lakes_PADUS, PctGAP_Status123Ws >= protection_cutoff)
  
  # number of protected lakes by region
  NARS_lakes_byregion_gap12Cat <- colSums(gContains(NARS_regions, protected_lakes_gap12Cat, byid = T))
  NARS_lakes_byregion_gap123Cat <- colSums(gContains(NARS_regions, protected_lakes_gap123Cat, byid = T))
  NARS_lakes_byregion_gap12Ws <- colSums(gContains(NARS_regions, protected_lakes_gap12Ws, byid = T))
  NARS_lakes_byregion_gap123Ws <- colSums(gContains(NARS_regions, protected_lakes_gap123Ws, byid = T))
  
  NARS_protected_lakes_DF <- data.frame(Region=NARS_names, ProtectedLakes_gap12Cat=NARS_lakes_byregion_gap12Cat,
                                        ProtectedLakes_gap123Cat=NARS_lakes_byregion_gap123Cat,
                                        ProtectedLakes_gap12Ws=NARS_lakes_byregion_gap12Ws,
                                        ProtectedLakes_gap123Ws=NARS_lakes_byregion_gap123Ws)
  
  # proportion of protected lakes by state (out of total lakes in each state)
  # count number of rows (COMIDs, therefore lakes) per unique state
  lake_countz_NARS <- NARS_COMID %>%
    group_by(WSA9) %>%
    tally()
  colnames(lake_countz_NARS) <- c("Region","nLakes")
  lake_countz_protected_NARS <- merge(lake_countz_NARS, NARS_protected_lakes_DF, by="Region", all.x=F)
  lake_countz_protected_NARS$PropProtected_gap12Cat <- lake_countz_protected_NARS$ProtectedLakes_gap12Cat/lake_countz_protected_NARS$nLakes
  lake_countz_protected_NARS$PropProtected_gap123Cat <- lake_countz_protected_NARS$ProtectedLakes_gap123Cat/lake_countz_protected_NARS$nLakes
  lake_countz_protected_NARS$PropProtected_gap12Ws <- lake_countz_protected_NARS$ProtectedLakes_gap12Ws/lake_countz_protected_NARS$nLakes
  lake_countz_protected_NARS$PropProtected_gap123Ws <- lake_countz_protected_NARS$ProtectedLakes_gap123Ws/lake_countz_protected_NARS$nLakes
  # clean up for next iteration
  protected_lakes_gap12Cat <- NULL
  protected_lakes_gap123Cat <- NULL
  protected_lakes_gap12Ws <- NULL
  protected_lakes_gap123Ws <- NULL
  lake_countz_NARS <- NULL
  NARS_protected_lakes_DF <- NULL
  NARS_lakes_byregion_gap12Cat <- NULL
  NARS_lakes_byregion_gap123Cat <- NULL
  NARS_lakes_byregion_gap12Ws <- NULL
  NARS_lakes_byregion_gap123Ws <- NULL
  NARS_COMID <- NULL
  rowid_NHD_df <- NULL
  NARS_names <- NULL
  rowid_NHD_df <- NULL
  return(lake_countz_protected_NARS)
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

par(mfrow=c(1,1))
statenames <- protected_lakes_100pct$State
barplot(protected_lakes_100pct$PropProtected_gap12Ws, las=1, names.arg=statenames, horiz=T, cex.names=0.5)

# wrangle table of proportion of lakes protected by state to export and reformat in Excel
LakeProtection_byState <- data.frame(State=protected_lakes_100pct$State, nLakes=protected_lakes_100pct$nLakes,
                                     PPCat_12_100=protected_lakes_100pct$PropProtected_gap12Cat,
                                     PPWs_12_100=protected_lakes_100pct$PropProtected_gap12Ws,
                                     PPCat_123_100=protected_lakes_100pct$PropProtected_gap123Cat,
                                     PPWs_123_100=protected_lakes_100pct$PropProtected_gap123Ws)

tmp90_table <- data.frame(State=protected_lakes_90pct$State,
                          PPCat_12_90=protected_lakes_90pct$PropProtected_gap12Cat,
                          PPWs_12_90=protected_lakes_90pct$PropProtected_gap12Ws,
                          PPCat_123_90=protected_lakes_90pct$PropProtected_gap123Cat,
                          PPWs_123_90=protected_lakes_90pct$PropProtected_gap123Ws)

tmp75_table <- data.frame(State=protected_lakes_75pct$State,
                          PPCat_12_75=protected_lakes_75pct$PropProtected_gap12Cat,
                          PPWs_12_75=protected_lakes_75pct$PropProtected_gap12Ws,
                          PPCat_123_75=protected_lakes_75pct$PropProtected_gap123Cat,
                          PPWs_123_75=protected_lakes_75pct$PropProtected_gap123Ws)

LakeProtection_byState <- merge(LakeProtection_byState, tmp90_table, by='State')
LakeProtection_byState <- merge(LakeProtection_byState, tmp75_table, by='State')
#write.csv(LakeProtection_byState, file='Data/LakeProtection_byState.csv')

#### Same analysis, but by NARS ecoregions

# set protection cutoff (for local catchment and network watershed) and execute function
protected_lakes_NARS_100pct <- protected_lakes_by_NARS(NHD_pts_lakes_PADUS, NARS_regions, protection_cutoff=100)
protected_lakes_NARS_90pct <- protected_lakes_by_NARS(NHD_pts_lakes_PADUS, NARS_regions, protection_cutoff=90)
protected_lakes_NARS_75pct <- protected_lakes_by_NARS(NHD_pts_lakes_PADUS, NARS_regions, protection_cutoff=75)


barnames <- protected_lakes_NARS_100pct$Region
barcolors <- c('orange','lightpink','coral','darkseagreen','darkkhaki','lawngreen','dodgerblue','orange4','slateblue')
ylim <- c(0,0.15)
barplot(protected_lakes_NARS_100pct$PropProtected_gap12Cat, las=1, ylab='Proportion of lakes protected', names.arg=barnames,
        col=barcolors, main='100% protected', ylim=ylim)

barplot(protected_lakes_NARS_90pct$PropProtected_gap12Cat, las=1, ylab='Proportion of lakes protected', names.arg=barnames,
        col=barcolors, main='90% protected', ylim=ylim)

barplot(protected_lakes_NARS_75pct$PropProtected_gap12Cat, las=1, ylab='Proportion of lakes protected', names.arg=barnames,
        col=barcolors, main='75% protected', ylim=ylim)


## Mapping
# 100%
lake_countz_protected100_NARS_shp <- merge(NARS_regions, protected_lakes_NARS_100pct, by.x='WSA9', by.y='Region')
lake_countz_protected90_NARS_shp <- merge(NARS_regions, protected_lakes_NARS_90pct, by.x='WSA9', by.y='Region')
lake_countz_protected75_NARS_shp <- merge(NARS_regions, protected_lakes_NARS_75pct, by.x='WSA9', by.y='Region')

dsnname <- "C:/Ian_GIS/FreshwaterConservation/PADUS_LakeCat"
layername <- 'Protected100_NARS'
#writeOGR(lake_countz_protected100_NARS_shp, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

## putting hold on R mapping...err message with "orphaned holes"
# map_breaks <- c(0,0.01,0.05,0.1,0.25,0.5,Inf)
# 
# tm_shape(lake_countz_protected100_NARS_shp)+
#   tm_fill('PropProtected_gap12Cat', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Cat',
#           breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
#   tm_borders()
# 
# tm_shape(lake_countz_protected100_NARS_shp)+
#   tm_fill('PropProtected_gap123Cat', style='fixed', title='Prop Lakes Protected, GAPS 1-3, Cat',
#           breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
#   tm_borders()
# 
# tm_shape(lake_countz_protected100_NARS_shp)+
#   tm_fill('PropProtected_gap12Ws', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Ws',
#           breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
#   tm_borders()
# 
# tm_shape(lake_countz_protected100_NARS_shp)+
#   tm_fill('PropProtected_gap123Ws', style='fixed', title='Prop Lakes Protected, GAPS 1-3, Ws',
#           breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
#   tm_borders()
