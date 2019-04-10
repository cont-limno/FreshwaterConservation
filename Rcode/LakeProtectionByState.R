####################### Lake protection by US state and NARS ecoregion #########################
# Date: 12-5-18
# updated: 4-10-19
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(rgeos)
library(dplyr)
library(tmap)
library(rgdal)
library(reshape2)
library(ggplot2)
library(gridExtra)

#### Input data ####
setwd('C:/Users/FWL/Documents/FreshwaterConservation')

PADUS_table <- read.csv("Data/PADUS.csv")

lower48 <- shapefile("Data/lower48/lower48.shp")
NARS_regions <- shapefile("Data/NARS_ecoregions/NARS_ecoregions.shp") #Natl Aquatic Resource Survey Ecoregions
NARS_regions <- spTransform(NARS_regions, crs(lower48)) #get into same crs as other data in analysis

NHD_pts <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_waterbody_pts.shp")
NHD_pts_lakes <- subset(NHD_pts, FTYPE=='LakePond' | FTYPE=='Reservoir')

## define constants
protection_cutoff <- 100 #percent catchment protected for lake to count as protected

## Protected lakes (centroids)
protected_GAPS12 <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/NHD_protect_pts_GAPS12_pct.shp")
protected_GAP3only <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts/NHD_protect_pts_GAP3only_pct.shp")
protected_GAPS12_COMIDs_ctr <- unique(protected_GAPS12@data$COMID)
protected_GAP3only_COMIDS_ctr <- unique(protected_GAP3only@data$COMID)
protected_GAPS12_COMIDs_100 <- subset(protected_GAPS12, PGAP_S12C >=protection_cutoff)
protected_GAPS12_COMIDs_100 <- unique(protected_GAPS12_COMIDs_100@data$COMID)
protected_GAP3only_COMIDs_100 <- subset(protected_GAP3only, PGAP_S3C >=protection_cutoff)
protected_GAP3only_COMIDs_100 <- unique(protected_GAP3only_COMIDs_100@data$COMID)

# unprotected lake COMIDs
unprotected_COMIDs <- read.csv("Data/unprotected_COMID.csv")[,2]

#### D-fine functions ####
source("Rcode/functions/protected_lakes_by_state.R")
source("Rcode/functions/protected_lakes_by_NARS.R")

############################ Main program #####################################
## Number and proportion of protected lakes per state
NHD_pts_lakes_PADUS <- merge(NHD_pts_lakes, PADUS_table, by='COMID', all=F)
NHD_pts_lakes_PADUS <- subset(NHD_pts_lakes_PADUS, AREASQKM >= 0.01) #remove lakes smaller than 1 ha

# calculate total protection for GAPS 1-2 and GAPS 1-3
NHD_pts_lakes_PADUS$PctGAP_Status12Cat <- NHD_pts_lakes_PADUS$PctGAP_Status1Cat + NHD_pts_lakes_PADUS$PctGAP_Status2Cat
#NHD_pts_lakes_PADUS$PctGAP_Status123Cat <- NHD_pts_lakes_PADUS$PctGAP_Status1Cat + NHD_pts_lakes_PADUS$PctGAP_Status2Cat + NHD_pts_lakes_PADUS$PctGAP_Status3Cat
NHD_pts_lakes_PADUS$PctGAP_Status12Ws <- NHD_pts_lakes_PADUS$PctGAP_Status1Ws + NHD_pts_lakes_PADUS$PctGAP_Status2Ws
#NHD_pts_lakes_PADUS$PctGAP_Status123Ws <- NHD_pts_lakes_PADUS$PctGAP_Status1Ws + NHD_pts_lakes_PADUS$PctGAP_Status2Ws + NHD_pts_lakes_PADUS$PctGAP_Status3Ws

## set protection cutoff (for local catchment and network watershed) and execute functions
LakeProtection_byState <- protected_lakes_by_state(NHD_pts_lakes_PADUS, lower48)
LakeProtection_byNARS <- protected_lakes_by_NARS(NHD_pts_lakes_PADUS, NARS_regions) #warning: slow

LakeProtection_byState_Export <- LakeProtection_byState[,1:2]
LakeProtection_byState_Export$Strict_ctr <- paste0(LakeProtection_byState$ProtectedLakes_gap12_ctr, ' (', round(LakeProtection_byState$PropProtected_gap12_ctr,2),')')
LakeProtection_byState_Export$Multi_ctr <- paste0(LakeProtection_byState$ProtectedLakes_gap3_ctr, ' (', round(LakeProtection_byState$PropProtected_gap3_ctr,2),')')
LakeProtection_byState_Export$Strict_Cat100 <- paste0(LakeProtection_byState$ProtectedLakes_gap12_Cat100, ' (', round(LakeProtection_byState$PropProtected_gap12_Cat100,2),')')
LakeProtection_byState_Export$Multi_Cat100 <- paste0(LakeProtection_byState$ProtectedLakes_gap3_Cat100, ' (', round(LakeProtection_byState$PropProtected_gap3_Cat100,2),')')
LakeProtection_byState_Export$Unprotected <- paste0(LakeProtection_byState$unprotected_lakes, ' (', round(LakeProtection_byState$PropUnprotected,2),')')

# export table and reformat in Excel
#write.csv(LakeProtection_byState_Export, file='Data/LakeProtection_byState.csv')
#write.csv(LakeProtection_byNARS, file='Data/LakeProtection_byNARS.csv')

### USES INTERMEDIATE DATA FROM FUNCTIONS: data only exist if run functions line by line
#state_NARS_COMID <- merge(NARS_COMID[,c(2:4)], lower48_COMID[,c(6,11)], by='COMID',all=F)
#state_NARS_COMID <- state_NARS_COMID[!duplicated(state_NARS_COMID$COMID),] #remove duplicate COMID
#write.csv(state_NARS_COMID, file='Data/state_NARS_COMID.csv')

## Mapping
# by state
lake_countz_protected100_lower48_shp <- merge(lower48, LakeProtection_byState, by.x='NAME', by.y='State')
map_breaks <- c(0,0.01,0.05,0.1,0.25,0.5,Inf)

tm_shape(lake_countz_protected100_lower48_shp)+
  tm_fill('PropProtected_gap12_ctr', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Ctr',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected100_lower48_shp)+
  tm_fill('PropProtected_gap3_ctr', style='fixed', title='Prop Lakes Protected, GAP 3, Ctr',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected100_lower48_shp)+
  tm_fill('PropProtected_gap12_Cat100', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Cat100',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected100_lower48_shp)+
  tm_fill('PropProtected_gap3_Cat100', style='fixed', title='Prop Lakes Protected, GAP 3, Cat100',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

# by ecoregion (abandon for now: some weird error about orphaned hole)
# lake_countz_protected_NARS_shp <- merge(NARS_regions, LakeProtection_byNARS, by.x='WSA9', by.y='Ecoregion')
# map_breaks <- c(0,0.01,0.05,0.1,0.25,0.5,Inf)
# 
# tm_shape(lake_countz_protected_NARS_shp)+
#   tm_fill('PropProtected_gap12_ctr', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Ctr',
#           breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
#   tm_borders()
# 
# tm_shape(lake_countz_protected_NARS_shp)+
#   tm_fill('PropProtected_gap3_ctr', style='fixed', title='Prop Lakes Protected, GAP 3, Ctr',
#           breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
#   tm_borders()
# 
# tm_shape(lake_countz_protected_NARS_shp)+
#   tm_fill('PropProtected_gap12_Cat100', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Cat100',
#           breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
#   tm_borders()
# 
# tm_shape(lake_countz_protected_NARS_shp)+
#   tm_fill('PropProtected_gap3_Cat100', style='fixed', title='Prop Lakes Protected, GAP 3, Cat100',
#           breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
#   tm_borders()

## seems that states with more protected lakes have fewer lakes?
# 100%
# par(mfrow=c(2,2))
# plot(PropProtected_gap12_ctr ~ nLakes, data=LakeProtection_byState, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-2, Center')
# mtext(side=3, 'Lake center')
# legend('topright', bty='n', legend=paste0('r = ', round(cor(LakeProtection_byState$PropProtected_gap12_ctr, LakeProtection_byState$nLakes, use='pairwise.complete.obs'),3)))
# 
# plot(PropProtected_gap3_ctr ~ nLakes, data=LakeProtection_byState, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAP 3, Center')
# mtext(side=3, 'Lake center')
# legend('topright', bty='n', legend=paste0('r = ', round(cor(LakeProtection_byState$PropProtected_gap3_ctr, LakeProtection_byState$nLakes, use='pairwise.complete.obs'),3)))
# 
# plot(PropProtected_gap12_Cat100 ~ nLakes, data=LakeProtection_byState, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-2, Cat 100')
# mtext(side=3, '100 pct protected')
# legend('topright', bty='n', legend=paste0('r = ', round(cor(LakeProtection_byState$PropProtected_gap12_Cat100, LakeProtection_byState$nLakes, use='pairwise.complete.obs'),3)))
# 
# plot(PropProtected_gap3_Cat100 ~ nLakes, data=LakeProtection_byState, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAP 3, Cat 100')
# mtext(side=3, '100 pct protected')
# legend('topright', bty='n', legend=paste0('r = ', round(cor(LakeProtection_byState$PropProtected_gap3_Cat100, LakeProtection_byState$nLakes, use='pairwise.complete.obs'),3)))

## Barplots of lake protection by state
barplot_df_ctr <- LakeProtection_byState[,c(1,8:9)]
barplot_df_ctr$PropProtected_gap12_ctr <- barplot_df_ctr$PropProtected_gap12_ctr*100 #convert prop to pct
barplot_df_ctr$PropProtected_gap3_ctr <- barplot_df_ctr$PropProtected_gap3_ctr*100
melted_Cat_ctr <- melt(barplot_df_ctr, id.vars='State')
melted_Cat_ctr <- merge(melted_Cat_ctr, LakeProtection_byState[,1:2], by='State') # add in number of lakes column to melted df for plot

barplot_df_Cat100 <- LakeProtection_byState[,c(1,10:11)]
barplot_df_Cat100$PropProtected_gap12_Cat100 <- barplot_df_Cat100$PropProtected_gap12_Cat100*100 #convert prop to pct
barplot_df_Cat100$PropProtected_gap3_Cat100 <- barplot_df_Cat100$PropProtected_gap3_Cat100*100
melted_Cat_Cat100 <- melt(barplot_df_Cat100, id.vars='State')
melted_Cat_Cat100 <- merge(melted_Cat_Cat100, LakeProtection_byState[,1:2], by='State') # add in number of lakes column to melted df for plot

# with help from: https://stackoverflow.com/questions/20349929/stacked-bar-plot-in-r
state_ctr_plot <- ggplot(melted_Cat_ctr, aes(x = reorder(State, nLakes), y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  ggtitle('a) Protected lake = lake center in protected area')+
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  scale_y_continuous(limits=c(0,70), breaks=seq(0,70,10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
  theme(axis.text.y=element_text(color='black'))+
  scale_fill_manual("legend", values = c("PropProtected_gap12_ctr" = "olivedrab3", "PropProtected_gap3_ctr" = "navajowhite2"),
                    labels=c('Strict','Multi-use'))+
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(legend.position=c(0.89,0.9))+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

state_Cat100_plot <- ggplot(melted_Cat_Cat100, aes(x = reorder(State, nLakes), y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  ggtitle('b) Protected lake = 100% catchment protected')+
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  scale_y_continuous(limits=c(0,70), breaks=seq(0,70,10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
  theme(axis.text.y=element_text(color='black'))+
  scale_fill_manual("legend", values = c("PropProtected_gap12_Cat100" = "olivedrab3", "PropProtected_gap3_Cat100" = "navajowhite2"),
                    labels=c('Strict','Multi-use'))+
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(legend.position=c("none"))+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

#png('Figures/LakeProtectionByState.png',width = 7.5,height = 6,units = 'in',res=300)
  grid.arrange(state_ctr_plot, state_Cat100_plot, nrow=2)
#dev.off()

## Barplots of lake protection by ecoregion
barplot_df_ctr_NARS <- LakeProtection_byNARS[,c(1,8:9)]
barplot_df_ctr_NARS$PropProtected_gap12_ctr <- barplot_df_ctr_NARS$PropProtected_gap12_ctr*100 #convert prop to pct
barplot_df_ctr_NARS$PropProtected_gap3_ctr <- barplot_df_ctr_NARS$PropProtected_gap3_ctr*100
melted_Cat_ctr_NARS <- melt(barplot_df_ctr_NARS, id.vars='Ecoregion')
melted_Cat_ctr_NARS <- merge(melted_Cat_ctr_NARS, LakeProtection_byNARS[,1:2], by='Ecoregion') # add in number of lakes column to melted df for plot
melted_Cat_ctr_NARS$PlotOrder <- c(8,8,9,9,3,3,7,7,4,4,6,6,5,5,2,2,1,1)

barplot_df_Cat100_NARS <- LakeProtection_byNARS[,c(1,10:11)]
barplot_df_Cat100_NARS$PropProtected_gap12_Cat100 <- barplot_df_Cat100_NARS$PropProtected_gap12_Cat100*100 #convert prop to pct
barplot_df_Cat100_NARS$PropProtected_gap3_Cat100 <- barplot_df_Cat100_NARS$PropProtected_gap3_Cat100*100
melted_Cat_Cat100_NARS <- melt(barplot_df_Cat100_NARS, id.vars='Ecoregion')
melted_Cat_Cat100_NARS <- merge(melted_Cat_Cat100_NARS, LakeProtection_byNARS[,1:2], by='Ecoregion') # add in number of lakes column to melted df for plot
melted_Cat_Cat100_NARS$PlotOrder <- c(8,8,9,9,3,3,7,7,4,4,6,6,5,5,2,2,1,1) 

ecoregion_ctr_plot <- ggplot(melted_Cat_ctr_NARS, aes(x = reorder(Ecoregion, PlotOrder), y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  ggtitle('a) Protected lake = lake center in protected area')+
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  scale_y_continuous(limits=c(0,70), breaks=seq(0,70,10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
  theme(axis.text.y=element_text(color='black'))+
  scale_fill_manual("legend", values = c("PropProtected_gap12_ctr" = "olivedrab3", "PropProtected_gap3_ctr" = "navajowhite2"),
                    labels=c('Strict','Multi-use'))+
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(legend.position=c(0.89,0.9))+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

ecoregion_Cat100_plot <- ggplot(melted_Cat_Cat100_NARS, aes(x = reorder(Ecoregion, PlotOrder), y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  ggtitle('b) Protected lake = 100% catchment protected')+
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  scale_y_continuous(limits=c(0,70), breaks=seq(0,70,10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
  theme(axis.text.y=element_text(color='black'))+
  scale_fill_manual("legend", values = c("PropProtected_gap12_Cat100" = "olivedrab3", "PropProtected_gap3_Cat100" = "navajowhite2"),
                    labels=c('Strict','Multi-use'))+
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(legend.position="none")+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

#png('Figures/LakeProtectionByNARS.png',width = 4.5,height = 6,units = 'in',res=300)
  grid.arrange(ecoregion_ctr_plot, ecoregion_Cat100_plot, nrow=2)
#dev.off()
