####################### Lake protection by US state and NARS ecoregion #########################
# Date: 7-30-19; rerunning previous analysis with 80% protection threshold
# updated:
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

lake_sqkm_cutoff <- 0.01 #=1ha

PADUS_table <- read.csv("Data/PADUS.csv")

lower48 <- shapefile("Data/lower48/lower48.shp")
NARS_regions <- shapefile("Data/NARS_ecoregions/NARS_ecoregions.shp") #Natl Aquatic Resource Survey Ecoregions
NARS_regions <- spTransform(NARS_regions, crs(lower48)) #get into same crs as other data in analysis

NHD_pts <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_waterbody_pts.shp")
NHD_pts <- subset(NHD_pts, FTYPE=='LakePond' | FTYPE=='Reservoir')
NHD_pts <- subset(NHD_pts, AREASQKM >= lake_sqkm_cutoff)
NHD_pts <- NHD_pts[!duplicated(NHD_pts@data$COMID),] #remove duplicate COMID
NHD_pts <- subset(NHD_pts, FCODE %in% all_FCODES)

#NHD_pts_inter <- subset(NHD_pts, FCODE %in% intermittent_FCODES)
NHD_pts_perm <- subset(NHD_pts, FCODE %in% LAGOS_FCODES)

## define constants
protection_cutoff <- 80 #percent catchment protected for lake to count as protected

## Protected lakes (centroids); subdivide into all, permanent and intermittent lakes
protected_GAPS12 <- shapefile("Data/NHD/NHD_waterbody_pts/NHD_protect_pts_GAPS12_pct.shp")
protected_GAPS12_perm <- subset(protected_GAPS12, FCODE %in% LAGOS_FCODES)

protected_GAP3only <- shapefile("Data/NHD/NHD_waterbody_pts/NHD_protect_pts_GAP3only_pct.shp")
protected_GAP3only_perm <- subset(protected_GAP3only, FCODE %in% LAGOS_FCODES)

protected_GAPS12_COMIDs_ctr <- unique(protected_GAPS12@data$COMID)
protected_GAP3only_COMIDS_ctr <- unique(protected_GAP3only@data$COMID)

protected_GAPS12_COMIDs_perm <- unique(protected_GAPS12_perm@data$COMID)
protected_GAP3only_COMIDS_perm <- unique(protected_GAP3only_perm@data$COMID)

protected_GAPS12_COMIDs_80 <- subset(protected_GAPS12, PGAP_S12C >=protection_cutoff)
protected_GAPS12_COMIDs_80 <- unique(protected_GAPS12_COMIDs_80@data$COMID)

protected_GAPS12_COMIDs_80_perm <- subset(protected_GAPS12_perm, PGAP_S12C >=protection_cutoff)
protected_GAPS12_COMIDs_80_perm <- unique(protected_GAPS12_COMIDs_80_perm@data$COMID)

protected_GAP3only_COMIDs_80 <- subset(protected_GAP3only, PGAP_S3C >=protection_cutoff)
protected_GAP3only_COMIDs_80 <- unique(protected_GAP3only_COMIDs_80@data$COMID)

protected_GAP3only_COMIDs_80_perm <- subset(protected_GAP3only_perm, PGAP_S3C >=protection_cutoff)
protected_GAP3only_COMIDs_80_perm <- unique(protected_GAP3only_COMIDs_80_perm@data$COMID)


# unprotected lake COMIDs
unprotected_COMIDs <- read.csv("Data/unprotected_COMID.csv")[,2]

#### D-fine functions ####
source("Rcode/functions/protected_lakes_by_state_80pct.R")
source("Rcode/functions/protected_lakes_by_NARS_80pct.R")

############################ Main program #####################################
# calculate total protection for GAPS 1-2 and GAPS 1-3
PADUS_table$PctGAP_Status12Cat <- PADUS_table$PctGAP_Status1Cat + PADUS_table$PctGAP_Status2Cat
#PADUS_table$PctGAP_Status123Cat <- PADUS_table$PctGAP_Status1Cat + PADUS_table$PctGAP_Status2Cat + PADUS_table$PctGAP_Status3Cat
PADUS_table$PctGAP_Status12Ws <- PADUS_table$PctGAP_Status1Ws + PADUS_table$PctGAP_Status2Ws
#PADUS_table$PctGAP_Status123Ws <- PADUS_table$PctGAP_Status1Ws + PADUS_table$PctGAP_Status2Ws + PADUS_table$PctGAP_Status3Ws

# Number and proportion of protected lakes per state
NHD_pts_PADUS <- merge(NHD_pts, PADUS_table, by='COMID', all=F)
NHD_pts_PADUS_perm <- merge(NHD_pts_perm, PADUS_table, by='COMID',all=F)

## set protection cutoff (for local catchment and network watershed) and execute functions
LakeProtection_byState <- protected_lakes_by_state_80pct(NHD_pts_PADUS, lower48)

LakeProtection_byNARS <- protected_lakes_by_NARS_80pct(NHD_pts_PADUS, NARS_regions) #warning: slow
LakeProtection_byNARS_perm <- protected_lakes_by_NARS_80pct(NHD_pts_PADUS_perm, NARS_regions) #warning: slow

## get ready to export output data
LakeProtection_byState_Export <- LakeProtection_byState[,1:2]
LakeProtection_byState_Export$Strict_ctr <- paste0(LakeProtection_byState$ProtectedLakes_gap12_ctr, ' (', round(LakeProtection_byState$PropProtected_gap12_ctr,2),')')
LakeProtection_byState_Export$Multi_ctr <- paste0(LakeProtection_byState$ProtectedLakes_gap3_ctr, ' (', round(LakeProtection_byState$PropProtected_gap3_ctr,2),')')
LakeProtection_byState_Export$Strict_Cat80 <- paste0(LakeProtection_byState$ProtectedLakes_gap12_Cat80, ' (', round(LakeProtection_byState$PropProtected_gap12_Cat80,2),')')
LakeProtection_byState_Export$Multi_Cat80 <- paste0(LakeProtection_byState$ProtectedLakes_gap3_Cat80, ' (', round(LakeProtection_byState$PropProtected_gap3_Cat80,2),')')
LakeProtection_byState_Export$Unprotected <- paste0(LakeProtection_byState$unprotected_lakes, ' (', round(LakeProtection_byState$PropUnprotected,2),')')

LakeProtection_byState_Export_perm <- LakeProtection_byState_perm[,1:2]
LakeProtection_byState_Export_perm$Strict_ctr <- paste0(LakeProtection_byState_perm$ProtectedLakes_gap12_ctr, ' (', round(LakeProtection_byState_perm$PropProtected_gap12_ctr,2),')')
LakeProtection_byState_Export_perm$Multi_ctr <- paste0(LakeProtection_byState_perm$ProtectedLakes_gap3_ctr, ' (', round(LakeProtection_byState_perm$PropProtected_gap3_ctr,2),')')
LakeProtection_byState_Export_perm$Strict_Cat80 <- paste0(LakeProtection_byState_perm$ProtectedLakes_gap12_Cat80, ' (', round(LakeProtection_byState_perm$PropProtected_gap12_Cat80,2),')')
LakeProtection_byState_Export_perm$Multi_Cat80 <- paste0(LakeProtection_byState_perm$ProtectedLakes_gap3_Cat80, ' (', round(LakeProtection_byState_perm$PropProtected_gap3_Cat80,2),')')
LakeProtection_byState_Export_perm$Unprotected <- paste0(LakeProtection_byState_perm$unprotected_lakes, ' (', round(LakeProtection_byState_perm$PropUnprotected,2),')')

# differences in number of permanent, intermittent lakes vs. full datasets
ecoregions_lake_n <- cbind.data.frame(LakeProtection_byNARS[,1:2], LakeProtection_byNARS_perm[,2], LakeProtection_byNARS_inter[,2])
colnames(ecoregions_lake_n) <- c('Ecoregion','nLakes_all','nLakes_perm','nLakes_inter')
ecoregions_lake_n$pct_perm <- ecoregions_lake_n$nLakes_perm/ecoregions_lake_n$nLakes_all
  
# export table and reformat in Excel
#write.csv(LakeProtection_byState_Export_perm, file='Data/LakeProtection_byState_perm_80pct.csv')
#write.csv(LakeProtection_byNARS_perm, file='Data/LakeProtection_byNARS_perm_80pct.csv')

### USES INTERMEDIATE DATA FROM FUNCTIONS: data only exist if run functions line by line
#state_NARS_COMID <- merge(NARS_COMID[,c(2:4)], lower48_COMID[,c(6,11)], by='COMID',all=F)
#state_NARS_COMID <- state_NARS_COMID[!duplicated(state_NARS_COMID$COMID),] #remove duplicate COMID
#write.csv(state_NARS_COMID, file='Data/state_NARS_COMID.csv')

## Mapping
# by state
lake_countz_protected80_lower48_shp <- merge(lower48, LakeProtection_byState, by.x='NAME', by.y='State')
map_breaks <- c(0,0.01,0.05,0.1,0.25,0.5,Inf)

tm_shape(lake_countz_protected80_lower48_shp)+
  tm_fill('PropProtected_gap12_ctr', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Ctr',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected80_lower48_shp)+
  tm_fill('PropProtected_gap3_ctr', style='fixed', title='Prop Lakes Protected, GAP 3, Ctr',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected80_lower48_shp)+
  tm_fill('PropProtected_gap12_Cat80', style='fixed', title='Prop Lakes Protected, GAPS 1-2, Cat80',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()

tm_shape(lake_countz_protected80_lower48_shp)+
  tm_fill('PropProtected_gap3_Cat80', style='fixed', title='Prop Lakes Protected, GAP 3, Cat80',
          breaks=map_breaks, textNA = 'NA', colorNA = 'gray')+
  tm_borders()


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
# plot(PropProtected_gap12_Cat80 ~ nLakes, data=LakeProtection_byState, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAPS 1-2, Cat 100')
# mtext(side=3, '100 pct protected')
# legend('topright', bty='n', legend=paste0('r = ', round(cor(LakeProtection_byState$PropProtected_gap12_Cat80, LakeProtection_byState$nLakes, use='pairwise.complete.obs'),3)))
# 
# plot(PropProtected_gap3_Cat80 ~ nLakes, data=LakeProtection_byState, pch=20, ylab='Prop lakes protected', ylim=c(0,0.7), las=1, main='GAP 3, Cat 100')
# mtext(side=3, '100 pct protected')
# legend('topright', bty='n', legend=paste0('r = ', round(cor(LakeProtection_byState$PropProtected_gap3_Cat80, LakeProtection_byState$nLakes, use='pairwise.complete.obs'),3)))

## Barplots of lake protection by state
barplot_df_ctr <- LakeProtection_byState[,c(1,8:9)]
barplot_df_ctr$PropProtected_gap12_ctr <- barplot_df_ctr$PropProtected_gap12_ctr*100 #convert prop to pct
barplot_df_ctr$PropProtected_gap3_ctr <- barplot_df_ctr$PropProtected_gap3_ctr*100
melted_Cat_ctr <- melt(barplot_df_ctr, id.vars='State')
melted_Cat_ctr <- merge(melted_Cat_ctr, LakeProtection_byState[,1:2], by='State') # add in number of lakes column to melted df for plot

barplot_df_ctr_perm <- LakeProtection_byState_perm[,c(1,8:9)]
barplot_df_ctr_perm$PropProtected_gap12_ctr <- barplot_df_ctr_perm$PropProtected_gap12_ctr*100 #convert prop to pct
barplot_df_ctr_perm$PropProtected_gap3_ctr <- barplot_df_ctr_perm$PropProtected_gap3_ctr*100
melted_Cat_ctr_perm <- melt(barplot_df_ctr_perm, id.vars='State')
melted_Cat_ctr_perm <- merge(melted_Cat_ctr_perm, LakeProtection_byState_perm[,1:2], by='State') # add in number of lakes column to melted df for plot

barplot_df_Cat80 <- LakeProtection_byState[,c(1,10:11)]
barplot_df_Cat80$PropProtected_gap12_Cat80 <- barplot_df_Cat80$PropProtected_gap12_Cat80*100 #convert prop to pct
barplot_df_Cat80$PropProtected_gap3_Cat80 <- barplot_df_Cat80$PropProtected_gap3_Cat80*100
melted_Cat_Cat80 <- melt(barplot_df_Cat80, id.vars='State')
melted_Cat_Cat80 <- merge(melted_Cat_Cat80, LakeProtection_byState[,1:2], by='State') # add in number of lakes column to melted df for plot

barplot_df_Cat80_perm <- LakeProtection_byState_perm[,c(1,10:11)]
barplot_df_Cat80_perm$PropProtected_gap12_Cat80 <- barplot_df_Cat80_perm$PropProtected_gap12_Cat80*100 #convert prop to pct
barplot_df_Cat80_perm$PropProtected_gap3_Cat80 <- barplot_df_Cat80_perm$PropProtected_gap3_Cat80*100
melted_Cat_Cat80_perm <- melt(barplot_df_Cat80_perm, id.vars='State')
melted_Cat_Cat80_perm <- merge(melted_Cat_Cat80_perm, LakeProtection_byState_perm[,1:2], by='State') # add in number of l

# with help from: https://stackoverflow.com/questions/20349929/stacked-bar-plot-in-r
# all lakes
# state_ctr_plot <- ggplot(melted_Cat_ctr, aes(x = reorder(State, nLakes), y = value, fill = variable)) + 
#   geom_bar(stat = "identity") +
#   xlab("") +
#   ylab("Percent of lakes protected") +
#   ggtitle('a) Protected lake = lake center in protected area')+
#   guides(fill = guide_legend(reverse=T)) +
#   #theme_bw() +
#   scale_y_continuous(limits=c(0,70), breaks=seq(0,70,10)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
#   theme(axis.text.y=element_text(color='black'))+
#   scale_fill_manual("legend", values = c("PropProtected_gap12_ctr" = "olivedrab3", "PropProtected_gap3_ctr" = "navajowhite2"),
#                     labels=c('Strict','Multi-use'))+
#   geom_hline(yintercept=17, linetype='dashed', color='black')+
#   theme(legend.position=c(0.89,0.9))+ #manually reposition legend inside plot
#   theme(legend.title=element_blank()) #remove legend title
# 
# state_Cat80_plot <- ggplot(melted_Cat_Cat80, aes(x = reorder(State, nLakes), y = value, fill = variable)) + 
#   geom_bar(stat = "identity") +
#   xlab("") +
#   ylab("Percent of lakes protected") +
#   ggtitle('b) Protected lake = 80% catchment protected')+
#   guides(fill = guide_legend(reverse=T)) +
#   #theme_bw() +
#   scale_y_continuous(limits=c(0,70), breaks=seq(0,70,10)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
#   theme(axis.text.y=element_text(color='black'))+
#   scale_fill_manual("legend", values = c("PropProtected_gap12_Cat80" = "olivedrab3", "PropProtected_gap3_Cat80" = "navajowhite2"),
#                     labels=c('Strict','Multi-use'))+
#   geom_hline(yintercept=17, linetype='dashed', color='black')+
#   theme(legend.position=c("none"))+ #manually reposition legend inside plot
#   theme(legend.title=element_blank()) #remove legend title
# 
# png('Figures/LakeProtectionByState.png',width = 7.5,height = 6,units = 'in',res=600)
#   grid.arrange(state_ctr_plot, state_Cat80_plot, nrow=2)
# dev.off()

# permanent lakes only
state_ctr_plot_perm <- ggplot(melted_Cat_ctr_perm, aes(x = reorder(State, nLakes), y = value, fill = variable)) + 
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

state_Cat80_plot_perm <- ggplot(melted_Cat_Cat80_perm, aes(x = reorder(State, nLakes), y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  ggtitle('b) Protected lake = 80% catchment protected')+
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  scale_y_continuous(limits=c(0,70), breaks=seq(0,70,10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
  theme(axis.text.y=element_text(color='black'))+
  scale_fill_manual("legend", values = c("PropProtected_gap12_Cat80" = "olivedrab3", "PropProtected_gap3_Cat80" = "navajowhite2"),
                    labels=c('Strict','Multi-use'))+
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(legend.position=c("none"))+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

png('Figures/LakeProtectionByState_perm_80pct.png',width = 7.5,height = 6,units = 'in',res=600)
  grid.arrange(state_ctr_plot_perm, state_Cat80_plot_perm, nrow=2)
dev.off()

## Barplots of lake protection by ecoregion
barplot_df_ctr_NARS <- LakeProtection_byNARS[,c(1,8:9)]
barplot_df_ctr_NARS$PropProtected_gap12_ctr <- barplot_df_ctr_NARS$PropProtected_gap12_ctr*100 #convert prop to pct
barplot_df_ctr_NARS$PropProtected_gap3_ctr <- barplot_df_ctr_NARS$PropProtected_gap3_ctr*100
melted_Cat_ctr_NARS <- melt(barplot_df_ctr_NARS, id.vars='Ecoregion')
melted_Cat_ctr_NARS <- merge(melted_Cat_ctr_NARS, LakeProtection_byNARS[,1:2], by='Ecoregion') # add in number of lakes column to melted df for plot
melted_Cat_ctr_NARS$PlotOrder <- c(8,8,9,9,3,3,7,7,4,4,6,6,5,5,1,1,2,2)

barplot_df_ctr_NARS_perm <- LakeProtection_byNARS_perm[,c(1,8:9)]
barplot_df_ctr_NARS_perm$PropProtected_gap12_ctr <- barplot_df_ctr_NARS_perm$PropProtected_gap12_ctr*100 #convert prop to pct
barplot_df_ctr_NARS_perm$PropProtected_gap3_ctr <- barplot_df_ctr_NARS_perm$PropProtected_gap3_ctr*100
melted_Cat_ctr_NARS_perm <- melt(barplot_df_ctr_NARS_perm, id.vars='Ecoregion')
melted_Cat_ctr_NARS_perm <- merge(melted_Cat_ctr_NARS_perm, LakeProtection_byNARS_perm[,1:2], by='Ecoregion') # add in number of lakes column to melted df for plot
melted_Cat_ctr_NARS_perm$PlotOrder <- c(8,8,9,9,3,3,7,7,4,4,6,6,5,5,1,1,2,2)

barplot_df_Cat80_NARS <- LakeProtection_byNARS[,c(1,10:11)]
barplot_df_Cat80_NARS$PropProtected_gap12_Cat80 <- barplot_df_Cat80_NARS$PropProtected_gap12_Cat80*100 #convert prop to pct
barplot_df_Cat80_NARS$PropProtected_gap3_Cat80 <- barplot_df_Cat80_NARS$PropProtected_gap3_Cat80*100
melted_Cat_Cat80_NARS <- melt(barplot_df_Cat80_NARS, id.vars='Ecoregion')
melted_Cat_Cat80_NARS <- merge(melted_Cat_Cat80_NARS, LakeProtection_byNARS[,1:2], by='Ecoregion') # add in number of lakes column to melted df for plot
melted_Cat_Cat80_NARS$PlotOrder <- c(8,8,9,9,3,3,7,7,4,4,6,6,5,5,1,1,2,2) 

barplot_df_Cat80_NARS_perm <- LakeProtection_byNARS_perm[,c(1,10:11)]
barplot_df_Cat80_NARS_perm$PropProtected_gap12_Cat80 <- barplot_df_Cat80_NARS_perm$PropProtected_gap12_Cat80*100 #convert prop to pct
barplot_df_Cat80_NARS_perm$PropProtected_gap3_Cat80 <- barplot_df_Cat80_NARS_perm$PropProtected_gap3_Cat80*100
melted_Cat_Cat80_NARS_perm <- melt(barplot_df_Cat80_NARS_perm, id.vars='Ecoregion')
melted_Cat_Cat80_NARS_perm <- merge(melted_Cat_Cat80_NARS_perm, LakeProtection_byNARS_perm[,1:2], by='Ecoregion') # add in number of lakes column to melted df for plot
melted_Cat_Cat80_NARS_perm$PlotOrder <- c(8,8,9,9,3,3,7,7,4,4,6,6,5,5,1,1,2,2) 

# all lakes
# ecoregion_ctr_plot <- ggplot(melted_Cat_ctr_NARS, aes(x = reorder(Ecoregion, PlotOrder), y = value, fill = variable)) + 
#   geom_bar(stat = "identity") +
#   xlab("") +
#   ylab("Percent of lakes protected") +
#   ggtitle('a) Protected lake = lake center in protected area')+
#   guides(fill = guide_legend(reverse=T)) +
#   #theme_bw() +
#   scale_y_continuous(limits=c(0,70), breaks=seq(0,70,10)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
#   theme(axis.text.y=element_text(color='black'))+
#   scale_fill_manual("legend", values = c("PropProtected_gap12_ctr" = "olivedrab3", "PropProtected_gap3_ctr" = "navajowhite2"),
#                     labels=c('Strict','Multi-use'))+
#   geom_hline(yintercept=17, linetype='dashed', color='black')+
#   theme(legend.position=c(0.89,0.9))+ #manually reposition legend inside plot
#   theme(legend.title=element_blank()) #remove legend title
# 
# ecoregion_Cat80_plot <- ggplot(melted_Cat_Cat80_NARS, aes(x = reorder(Ecoregion, PlotOrder), y = value, fill = variable)) + 
#   geom_bar(stat = "identity") +
#   xlab("") +
#   ylab("Percent of lakes protected") +
#   ggtitle('b) Protected lake = 80% catchment protected')+
#   guides(fill = guide_legend(reverse=T)) +
#   #theme_bw() +
#   scale_y_continuous(limits=c(0,70), breaks=seq(0,70,10)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
#   theme(axis.text.y=element_text(color='black'))+
#   scale_fill_manual("legend", values = c("PropProtected_gap12_Cat80" = "olivedrab3", "PropProtected_gap3_Cat80" = "navajowhite2"),
#                     labels=c('Strict','Multi-use'))+
#   geom_hline(yintercept=17, linetype='dashed', color='black')+
#   theme(legend.position="none")+ #manually reposition legend inside plot
#   theme(legend.title=element_blank()) #remove legend title

png('Figures/LakeProtectionByNARS.png',width = 4.5,height = 6,units = 'in',res=600)
  grid.arrange(ecoregion_ctr_plot, ecoregion_Cat80_plot, nrow=2)
dev.off()

# permanent lakes only
ecoregion_ctr_plot_perm <- ggplot(melted_Cat_ctr_NARS_perm, aes(x = reorder(Ecoregion, PlotOrder), y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  ggtitle('a) Protected lake = lake center in protected area')+
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  scale_y_continuous(limits=c(0,80), breaks=seq(0,80,10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
  theme(axis.text.y=element_text(color='black'))+
  scale_fill_manual("legend", values = c("PropProtected_gap12_ctr" = "olivedrab3", "PropProtected_gap3_ctr" = "navajowhite2"),
                    labels=c('Strict','Multi-use'))+
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(legend.position=c(0.89,0.9))+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

ecoregion_Cat80_plot_perm <- ggplot(melted_Cat_Cat80_NARS_perm, aes(x = reorder(Ecoregion, PlotOrder), y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  ggtitle('b) Protected lake = 80% catchment protected')+
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  scale_y_continuous(limits=c(0,80), breaks=seq(0,80,10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
  theme(axis.text.y=element_text(color='black'))+
  scale_fill_manual("legend", values = c("PropProtected_gap12_Cat80" = "olivedrab3", "PropProtected_gap3_Cat80" = "navajowhite2"),
                    labels=c('Strict','Multi-use'))+
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(legend.position="none")+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

png('Figures/LakeProtectionByNARS_perm_80pct.png',width = 4.5,height = 6,units = 'in',res=600)
  grid.arrange(ecoregion_ctr_plot_perm, ecoregion_Cat80_plot_perm, nrow=2)
dev.off()

