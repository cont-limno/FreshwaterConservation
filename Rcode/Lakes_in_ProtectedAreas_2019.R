####################### Characteristics of lakes in US protected areas #########################
# Date: 2-12-19
# updated: 7-30-19; add 80% lake catchment protection threshold
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

# permanent lakes only in LAGOS_FCODES
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

# lower 48 states
lower48 <- shapefile("Data/lower48/lower48.shp") #same crs as NHD_pts

## Protected lakes (centroids) (used ArcGIS select by location, then exported shapefile)
protected_GAPS12 <- shapefile("Data/NHD/NHD_waterbody_pts/Arcgis/NHD_pts_GAPS12_ArcGIS_select.shp")
protected_GAPS12  <- subset(protected_GAPS12 , FTYPE=='LakePond' | FTYPE=='Reservoir')
protected_GAPS12 <- subset(protected_GAPS12, AREASQKM >= lake_sqkm_cutoff)
protected_GAPS12 <- protected_GAPS12 [!duplicated(protected_GAPS12 @data$COMID),] #remove duplicate COMID
protected_GAPS12 <- subset(protected_GAPS12, FCODE %in% LAGOS_FCODES)#permanent lakes only

protected_GAP3only <- shapefile("Data/NHD/NHD_waterbody_pts/Arcgis/NHD_pts_GAP3_only_ArcGIS_select.shp")
protected_GAP3only <- subset(protected_GAP3only, FTYPE=='LakePond' | FTYPE=='Reservoir')
protected_GAP3only <- subset(protected_GAP3only, AREASQKM >= lake_sqkm_cutoff)
protected_GAP3only <- protected_GAP3only[!duplicated(protected_GAP3only@data$COMID),] #remove duplicate COMID
protected_GAP3only <- subset(protected_GAP3only, FCODE %in% LAGOS_FCODES)#permanent lakes only

# get rid of lakes counted as both strict and multi (treat as strict); issues with overlapping polygons in PADUS
double_lakes <- intersect(protected_GAPS12@data$COMID, protected_GAP3only@data$COMID)
protected_GAP3only <- subset(protected_GAP3only, !(COMID %in% double_lakes)) #take out lakes that occur in both

## Other lakes
# NHD waterbodies (converted to points in ArcGIS) (NHDPlusV2 National dataset; downloaded November 2018)
NHD_pts <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_waterbody_pts.shp")
NHD_pts <- subset(NHD_pts, FTYPE=='LakePond' | FTYPE=='Reservoir')
NHD_pts <- subset(NHD_pts, AREASQKM >= lake_sqkm_cutoff)
NHD_pts <- NHD_pts[!duplicated(NHD_pts@data$COMID),] #remove duplicate COMID
NHD_pts <- subset(NHD_pts, FCODE %in% LAGOS_FCODES)

#NHD_pts_inter <- subset(NHD_pts, FCODE %in% intermittent_FCODES)
#NHD_pts_perm <- subset(NHD_pts, FCODE %in% LAGOS_FCODES)

#### LakeCat data for analyzing lake characteristics (protected and unprotected) ####
# Protected land by GAP status by local catchments and network watersheds (LakeCat)
# From US Protected Areas Database (PADUS) v 1.4
# https://gapanalysis.usgs.gov/padus/data/download/
PADUS_LakeCat <- read.csv("Data/PADUS.csv")

# LAGOS conn class data
LAGOSconn <- read.csv("Data/LakeConn_1ha_LAGOS_NHD_Xwalk.csv")

############################## Main program ######################################
## calculate total % GAP status 1-2 and 1-3 for catchments and watersheds
PADUS_LakeCat$PctGAP_Status12Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat
PADUS_LakeCat$PctGAP_Status123Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat + PADUS_LakeCat$PctGAP_Status3Cat

PADUS_LakeCat$PctGAP_Status12Ws <- PADUS_LakeCat$PctGAP_Status1Ws + PADUS_LakeCat$PctGAP_Status2Ws
PADUS_LakeCat$PctGAP_Status123Ws <- PADUS_LakeCat$PctGAP_Status1Ws + PADUS_LakeCat$PctGAP_Status2Ws + PADUS_LakeCat$PctGAP_Status3Ws

# get rid of unwanted columns
PADUS_LakeCat <- PADUS_LakeCat[,c('COMID','PctGAP_Status12Cat','PctGAP_Status12Ws','PctGAP_Status123Cat','PctGAP_Status123Ws','PctGAP_Status3Cat','PctGAP_Status3Ws')]
#PADUS_LakeCat <- merge(NHD_pts@data[,c('COMID','FCODE','FTYPE','ELEVATION','AREASQKM')], PADUS_LakeCat, by='COMID')

# how correlated is catchment protection with watershed protection?
cor(PADUS_LakeCat$PctGAP_Status12Cat, PADUS_LakeCat$PctGAP_Status12Ws, method='pearson', use='pairwise.complete.obs')
cor(PADUS_LakeCat$PctGAP_Status3Cat, PADUS_LakeCat$PctGAP_Status3Ws, method='pearson', use='pairwise.complete.obs')

# merge protected lake datasets to PADUS data
protected_GAPS12 <- merge(protected_GAPS12, PADUS_LakeCat, by='COMID')
protected_GAP3only <- merge(protected_GAP3only, PADUS_LakeCat, by='COMID')

# Unprotected lakes: all other lakes in LakeCat
unprotected_df <- merge(PADUS_LakeCat, NHD_pts, by='COMID', all=F) #merge to NHD to get lake area column
unprotected_df <- subset(unprotected_df, !(COMID %in% protected_GAPS12@data$COMID))#remove strictly protected lakes
unprotected_df <- subset(unprotected_df, !(COMID %in% protected_GAP3only@data$COMID))#remove multi-use lakes

# save unprotected COMIDs
#write.csv(unprotected_df$COMID, "Data/unprotected_COMID.csv")

## What proportion of lakes is protected/unprotected (simply falls within a protected area)?
total_n_lakes <- nrow(unprotected_df) + nrow(protected_GAPS12@data) + nrow(protected_GAP3only@data)
nrow(protected_GAPS12@data)/total_n_lakes
nrow(protected_GAP3only@data)/total_n_lakes
(nrow(protected_GAPS12@data)+nrow(protected_GAP3only@data))/total_n_lakes
nrow(unprotected_df)/total_n_lakes

# Export shapefiles of % protected for mapping in ArcGIS (first merge back to NHD pts)
# # Strict
# dsnname <- "Data/NHD/NHD_waterbody_pts"
# layername <- "NHD_protect_pts_GAPS12_pct"
# writeOGR(protected_GAPS12, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)
# # Multi-use
# dsnname <- "Data/NHD/NHD_waterbody_pts"
# layername <- "NHD_protect_pts_GAP3only_pct"
# writeOGR(protected_GAP3only, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)
# # Unprotected
# PADUS_unprotected_export <- merge(NHD_pts, unprotected_df, by='COMID', all.x=F)
# dsnname <- "C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_protected_pts" #too large to store on github
# layername <- "NHD_unprotected_pts"
# writeOGR(PADUS_unprotected_export, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

### Basic plots: do lakes in protected areas have protected catchments and watersheds?
png("Figures/protected_lakes_histogram_perm.png", width = 7,height = 5,units = 'in',res=600)
  par(mfrow=c(2,2))
  # PLOT A
  par(mar=c(2,3,4,0.5)) #bot,left,top,right
  hist_colors <- c('firebrick','darksalmon','moccasin','lightskyblue','dodgerblue4')
  hist(protected_GAPS12@data$PctGAP_Status12Cat, xlab='', main='a) Strict, catchment',breaks=seq(0,100,20),
      col=hist_colors, ylim=c(0,20000), ylab='')
  title(ylab='Frequency', line=2.2)
  mtext(side=3, paste0(nrow(protected_GAPS12@data), ' lakes'), cex=0.75, line=0.5) #line= adjusts position 
  # PLOT B
  par(mar=c(2,2,4,1.5)) #bot,left,top,right
  hist(protected_GAPS12@data$PctGAP_Status12Ws, xlab='', main='b) Strict, watershed',breaks=seq(0,100,20),
      col=hist_colors, ylim=c(0,20000), ylab='')
  mtext(side=3, paste0(nrow(protected_GAPS12@data), ' lakes'), cex=0.75, line=0.5) #line= adjusts position 
  # PLOT C
  par(mar=c(3,3,3,0.5)) #bot,left,top,right
  hist(protected_GAP3only@data$PctGAP_Status3Cat, xlab='% protected', main='c) Multi-use, catchment',breaks=seq(0,100,20),
      col=hist_colors, ylim=c(0,20000), ylab='')
  title(xlab='% protected', line=2)
  title(ylab='Frequency', line=2.2)
  mtext(side=3, paste0(nrow(protected_GAP3only@data), ' lakes'), cex=0.75)
  # PLOT D
  par(mar=c(3,2,3,1.5)) #bot,left,top,right
  hist(protected_GAP3only@data$PctGAP_Status3Ws, xlab='% protected', main='d) Multi-use, watershed',breaks=seq(0,100,20),
      col=hist_colors, ylim=c(0,20000), ylab='')
  title(xlab='% protected', line=2)
  mtext(side=3, paste0(nrow(protected_GAP3only@data), ' lakes'), cex=0.75)
dev.off()

## Catchment and watershed plots separately (2 row, 1 col)
## Same figures, but for permanent lakes only
png("Figures/protected_lakes_histogram_Cat_perm.png", width = 3.5,height = 5,units = 'in',res=600)
  par(mfrow=c(2,1))
  # PLOT A
  par(mar=c(2,3,4,0.5)) #bot,left,top,right
  hist_colors <- c('firebrick','darksalmon','moccasin','lightskyblue','dodgerblue4')
  hist(protected_GAPS12@data$PctGAP_Status12Cat, xlab='', main='',breaks=seq(0,100,20),
       col=hist_colors, ylim=c(0,20000), adj=0)
  title(ylab='Frequency', line=2.2)
  #mtext(side=3, paste0(nrow(protected_GAPS12@data), ' lakes'), cex=0.75, line=0.5) #line= adjusts position 

  # PLOT C
  par(mar=c(3,3,3,0.5)) #bot,left,top,right
  hist(protected_GAP3only@data$PctGAP_Status3Cat, xlab='% protected', main='',breaks=seq(0,100,20),
      col=hist_colors, ylim=c(0,20000), adj=0)
  title(xlab='% protected', line=2)
  title(ylab='Frequency', line=2.2)
  #mtext(side=3, paste0(nrow(protected_GAP3only@data), ' lakes'), cex=0.75)
dev.off()

png("Figures/protected_lakes_histogram_Ws_perm.png", width = 3.5,height = 5,units = 'in',res=600)
  par(mfrow=c(2,1))
  # PLOT 1
  par(mar=c(2,3,4,0.5)) #bot,left,top,right
  hist_colors <- c('firebrick','darksalmon','moccasin','lightskyblue','dodgerblue4')
  hist(protected_GAPS12@data$PctGAP_Status12Ws, xlab='', main='',breaks=seq(0,100,20),
      col=hist_colors, ylim=c(0,20000), adj=0)
  title(ylab='Frequency', line=2.2)
  #mtext(side=3, paste0(nrow(protected_GAPS12@data), ' lakes'), cex=0.75, line=0.5) #line= adjusts position 

  # PLOT 2
  par(mar=c(3,3,3,0.5)) #bot,left,top,right
  hist(protected_GAP3only@data$PctGAP_Status3Ws, xlab='% protected', main='',breaks=seq(0,100,20),
      col=hist_colors, ylim=c(0,20000), adj=0)
  title(xlab='% protected', line=2)
  title(ylab='Frequency', line=2.2)
  #mtext(side=3, paste0(nrow(protected_GAP3only@data), ' lakes'), cex=0.75)
dev.off()

## What proportion of lakes that occur in protected areas has X amount of catchment/watershed protection?
# Strict
summary(protected_GAPS12@data)
nrow(subset(protected_GAPS12@data, PctGAP_Status12Cat < 20))/nrow(protected_GAPS12@data)
nrow(subset(protected_GAPS12@data, PctGAP_Status12Ws < 20))/nrow(protected_GAPS12@data)
nrow(subset(protected_GAPS12@data, PctGAP_Status12Cat > 80))/nrow(protected_GAPS12@data)
nrow(subset(protected_GAPS12@data, PctGAP_Status12Ws > 80))/nrow(protected_GAPS12@data)

# multi-use
summary(protected_GAP3only@data)
nrow(subset(protected_GAP3only@data, PctGAP_Status3Cat < 20))/nrow(protected_GAP3only@data)
nrow(subset(protected_GAP3only@data, PctGAP_Status3Ws < 20))/nrow(protected_GAP3only@data)
nrow(subset(protected_GAP3only@data, PctGAP_Status3Cat > 80))/nrow(protected_GAP3only@data)
nrow(subset(protected_GAP3only@data, PctGAP_Status3Ws > 80))/nrow(protected_GAP3only@data)

# Create subset of lakes with fully protected catchments/watersheds
protected_GAPS12_df_PADUS_100pct <- subset(protected_GAPS12@data, PctGAP_Status12Cat >= 100)
protected_GAP3only_df_PADUS_100pct <- subset(protected_GAP3only@data, PctGAP_Status3Cat >= 100)
nrow(subset(protected_GAPS12@data, PctGAP_Status12Ws >= 100))
nrow(subset(protected_GAP3only@data, PctGAP_Status3Ws >= 100))

# Create subset of lakes with 80% protected catchments/watersheds
protected_GAPS12_df_PADUS_80pct <- subset(protected_GAPS12@data, PctGAP_Status12Cat >= 80)
protected_GAP3only_df_PADUS_80pct <- subset(protected_GAP3only@data, PctGAP_Status3Cat >= 80)
nrow(subset(protected_GAPS12@data, PctGAP_Status12Ws >= 80))
nrow(subset(protected_GAP3only@data, PctGAP_Status3Ws >= 80))

# What proportion of lakes have fully protected catchments/watersheds?
nrow(protected_GAPS12_df_PADUS_100pct)/total_n_lakes
nrow(protected_GAP3only_df_PADUS_100pct)/total_n_lakes
nrow(subset(protected_GAPS12@data, PctGAP_Status12Ws >= 100))/total_n_lakes
nrow(subset(protected_GAP3only@data, PctGAP_Status3Ws >= 100))/total_n_lakes

# What proportion of lakes at least 80% protected catchments/watersheds?
nrow(protected_GAPS12_df_PADUS_80pct)/total_n_lakes
nrow(protected_GAP3only_df_PADUS_80pct)/total_n_lakes
nrow(subset(protected_GAPS12@data, PctGAP_Status12Ws >= 80))/total_n_lakes
nrow(subset(protected_GAP3only@data, PctGAP_Status3Ws >= 80))/total_n_lakes

# What % of protected lakes is intermittent?
#nrow(protected_GAPS12_inter)/sum(nrow(protected_GAPS12_perm)+nrow(protected_GAPS12_inter))
#nrow(protected_GAP3only_inter)/sum(nrow(protected_GAP3only_perm)+nrow(protected_GAP3only_inter))
#sum(nrow(protected_GAPS12_inter)+nrow(protected_GAP3only_inter))/sum(sum(nrow(protected_GAPS12_perm)+nrow(protected_GAPS12_inter))+sum(nrow(protected_GAP3only_perm)+nrow(protected_GAP3only_inter)))

## Breakdown of protected vs. unprotected lakes by NHD FCODE
unprotected_FCODE <- as.data.frame(unprotected_df %>%
                                   group_by(FCODE) %>%
                                   tally())
names(unprotected_FCODE) <- c('FCODE','unprotected')

protected_GAPS12_ctr_FCODE <- as.data.frame(protected_GAPS12@data %>%
                                              group_by(FCODE) %>%
                                              tally())
names(protected_GAPS12_ctr_FCODE) <- c('FCODE','GAPS12_ctr')

protected_GAP3only_ctr_FCODE <- as.data.frame(protected_GAP3only@data %>%
                                              group_by(FCODE) %>%
                                              tally())
names(protected_GAP3only_ctr_FCODE) <- c('FCODE','GAP3_ctr')

protected_GAPS12_100pct_FCODE <- as.data.frame(protected_GAPS12_df_PADUS_100pct %>%
                                              group_by(FCODE) %>%
                                              tally())
names(protected_GAPS12_100pct_FCODE) <- c('FCODE','GAPS12_100pct')

protected_GAP3only_100pct_FCODE <- as.data.frame(protected_GAP3only_df_PADUS_100pct %>%
                                                 group_by(FCODE) %>%
                                                 tally())
names(protected_GAP3only_100pct_FCODE) <- c('FCODE','GAP3_100pct')

FCODE_table <- Reduce(function(x, y) merge(x, y, all=T), list(protected_GAPS12_ctr_FCODE, protected_GAP3only_ctr_FCODE, protected_GAPS12_100pct_FCODE,
                                                                 protected_GAP3only_100pct_FCODE, unprotected_FCODE))

FCODE_table$total_ctr <- rowSums(FCODE_table[,c(2,3,6)], na.rm=T)
FCODE_table$total_100pct <- rowSums(FCODE_table[,c(4,5,6)], na.rm=T)

##  Violin plot of protection across all US lakes
all_lake_COMID <- c(unprotected_df$COMID, protected_GAPS12@data$COMID, protected_GAP3only@data$COMID) 
violin_df <- subset(PADUS_LakeCat, COMID %in% all_lake_COMID)
violin_df[is.na(violin_df)] <- 0 #convert NAs to 0; treat NA protection as 0 protection

v1 <- violin_df$PctGAP_Status12Cat
v2 <- violin_df$PctGAP_Status3Cat
v3 <- violin_df$PctGAP_Status12Ws
v4 <- violin_df$PctGAP_Status3Ws

png("Figures/violin_pct_protected.png", width = 7,height = 5,units = 'in',res=600)
  par(mfrow=c(1,1))
  par(las=1,bty="l")  ## my preferred setting
  par(mar=c(3,4,2,0.5)) #bot,left,top,right
  plot(0.5:5,0.5:5,type="n",ylim=c(0,100),
      axes=FALSE,ann=FALSE)
  vioplot(v1,v2,add=T, col='gray70')
  axis(side=1,at=1:2,labels=c('Strict', 'Multi-use'))
  axis(side=2,at=seq(0,100,10),labels=seq(0,100,10))
  title(ylab='Percent protected', line=2.2)
dev.off()

# how many lakes have 0 protection?
length(subset(v1, v1 <1 ))/length(v1) #strict cat
length(subset(v2, v2 <1 ))/length(v2) #multi use cat
length(subset(v3, v3 <1 ))/length(v3) #strict ws
length(subset(v4, v4 <1 ))/length(v4) # multi-use ws

############# Analyze by LAGOS conn class #################
## Create new merged tables for plotting
protected_GAPS12_df_PADUS <- protected_GAPS12@data
protected_GAP3only_df_PADUS <- protected_GAP3only@data

## Summarize conn types for all lakes
LakeConn_countz <- as.data.frame(LAGOSconn %>%
                                   group_by(LakeConnec) %>%
                                   tally())

barplot(LakeConn_countz$n, names.arg=LakeConn_countz$LakeConnec, las=1)

# Match rate?
sum(LakeConn_countz$n)/total_n_lakes
sum(LakeConn_countz$n)/nrow(NHD_pts@data)

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

# strict, 80% cat protection
protected_GAPS12_df_PADUS_80pct_conn <- merge(protected_GAPS12_df_PADUS_80pct, LAGOSconn, by.x='COMID', 
                                               by.y='nhdplusv2_comid', all.x=F)

protected_GAPS12_80pct_conn_countz <- as.data.frame(protected_GAPS12_df_PADUS_80pct_conn %>%
                                                       group_by(LakeConnec) %>%
                                                       tally())
protected_GAPS12_80pct_conn_countz <- merge(protected_GAPS12_80pct_conn_countz, LakeConn_countz, by='LakeConnec')
colnames(protected_GAPS12_80pct_conn_countz) <- c('LakeConnec','n_protected', 'n_total')
protected_GAPS12_80pct_conn_countz$prop_protected <- protected_GAPS12_80pct_conn_countz$n_protected/protected_GAPS12_80pct_conn_countz$n_total
protected_GAPS12_80pct_conn_countz$Group <- 'Strict cat'

# multi-use, 80% cat protection
protected_GAP3only_df_PADUS_80pct_conn <- merge(protected_GAP3only_df_PADUS_80pct, LAGOSconn, by.x='COMID', 
                                                 by.y='nhdplusv2_comid', all.x=F)

protected_GAP3only_80pct_conn_countz <- as.data.frame(protected_GAP3only_df_PADUS_80pct_conn %>%
                                                         group_by(LakeConnec) %>%
                                                         tally())
protected_GAP3only_80pct_conn_countz <- merge(protected_GAP3only_80pct_conn_countz, LakeConn_countz, by='LakeConnec')
colnames(protected_GAP3only_80pct_conn_countz) <- c('LakeConnec','n_protected', 'n_total')
protected_GAP3only_80pct_conn_countz$prop_protected <- protected_GAP3only_80pct_conn_countz$n_protected/protected_GAP3only_80pct_conn_countz$n_total
protected_GAP3only_80pct_conn_countz$Group <- 'Multi-use cat'

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
stacked_df <- rbind.data.frame(protected_GAPS12_conn_countz[,c(1,4,5)],protected_GAPS3only_conn_countz[,c(1,4,5)])
stacked_df$prop_protected <- stacked_df$prop_protected*100 #convert prop to pct
stacked_df$Group <- as.factor(stacked_df$Group)
stacked_df$Group <- factor(stacked_df$Group,levels(stacked_df$Group)[c(2,1)])
#stacked_df$LakeConnec <-factor(stacked_df$LakeConnec,levels(stacked_df$LakeConnec)[c(1,3,4,2)])
stacked_df$LakeConnec <-factor(stacked_df$LakeConnec,levels(stacked_df$LakeConnec)[c(4,3,2,1)]) #reorder least to most conn

#png('Figures/stacked_bar_conn_type.png',width = 7.5,height = 4.75,units = 'in',res=300)
stack_gg1 <- ggplot(stacked_df, aes(x = LakeConnec, y = prop_protected, fill = Group)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  ggtitle('a) Protected lake = lake center in protected area')+
  scale_y_continuous(limits=c(0,30), breaks=seq(0,30,5)) +
  scale_x_discrete(labels=c('IS','HW','DRS','DRLS'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(axis.title.y = element_text(vjust=2.7, color='black'))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("Multi-use center" = "navajowhite2", "Strict center" = "olivedrab3"),#,"Unprotected" = "gray70"),
                    labels=c('Strict','Multi-use'))+
  theme(legend.position=c(0.13,0.86))+ #manually reposition legend inside plot
  theme(axis.text.y = element_text(color='black'), axis.text.x=element_text(color='black'))+
  #theme(legend.position='none')+
  theme(legend.title=element_blank()) #remove legend title
  stack_gg1
#dev.off()

# Same stacked bar idea, but treating protected lakes as those with fully protected catchments
# calculate "unprotected" based on all lake catchments that are not 100% protected (strict or multi-use)
#temp_ryan <- unprotected_df_conn_countz[,c(1,4,5)]
#temp_ryan$prop_protected <- 1- (protected_GAPS12_100pct_conn_countz$prop_protected + protected_GAP3only_100pct_conn_countz$prop_protected)
stacked_df2 <- rbind.data.frame(protected_GAPS12_100pct_conn_countz[,c(1,4,5)],protected_GAP3only_100pct_conn_countz[,c(1,4,5)])
stacked_df2$prop_protected <- stacked_df2$prop_protected*100 #convert prop to pct
stacked_df2$Group <- as.factor(stacked_df2$Group)
stacked_df2$Group <- factor(stacked_df2$Group,levels(stacked_df2$Group)[c(2,1)])
#stacked_df2$LakeConnec <-factor(stacked_df2$LakeConnec,levels(stacked_df2$LakeConnec)[c(1,3,4,2)])
stacked_df2$LakeConnec <-factor(stacked_df2$LakeConnec,levels(stacked_df2$LakeConnec)[c(4,3,2,1)]) #reorder least to most conn

#png('Figures/stacked_bar_conn_type_cat100.png',width = 7.5,height = 4.75,units = 'in',res=300)
stack_gg2 <- ggplot(stacked_df2, aes(x = LakeConnec, y = prop_protected, fill = Group)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  ggtitle('b) Protected lake = 100% catchment protected')+
  scale_y_continuous(limits=c(0,30), breaks=seq(0,30,5)) +
  scale_x_discrete(labels=c('IS','HW','DRS','DRLS'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(axis.title.y = element_text(vjust=2.7, color='black'))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("Multi-use cat" = "navajowhite2", "Strict cat" = "olivedrab3"),#,"Unprotected" = "gray70"),
                    labels=c('Strict','Multi-use'))+
  theme(legend.position=c('none'))+ #manually reposition legend inside plot
  theme(legend.text=element_text(size=8))+
  theme(axis.text.y = element_text(color='black'), axis.text.x=element_text(color='black'))+
  theme(legend.title=element_blank()) #remove legend title
  stack_gg2
#dev.off()

# Same stacked bar idea, but treating protected lakes as those with at least 80% protected catchments
# calculate "unprotected" based on all lake catchments that are not 80% protected (strict or multi-use)
stacked_df3 <- rbind.data.frame(protected_GAPS12_80pct_conn_countz[,c(1,4,5)],protected_GAP3only_80pct_conn_countz[,c(1,4,5)])
stacked_df3$prop_protected <- stacked_df3$prop_protected*100 #convert prop to pct
stacked_df3$Group <- as.factor(stacked_df3$Group)
stacked_df3$Group <- factor(stacked_df3$Group,levels(stacked_df3$Group)[c(2,1)])
#stacked_df3$LakeConnec <-factor(stacked_df3$LakeConnec,levels(stacked_df3$LakeConnec)[c(1,3,4,2)])
stacked_df3$LakeConnec <-factor(stacked_df3$LakeConnec,levels(stacked_df3$LakeConnec)[c(4,3,2,1)]) #reorder least to most conn
  
#png('Figures/stacked_bar_conn_type_cat80.png',width = 7.5,height = 4.75,units = 'in',res=300)
stack_gg3 <- ggplot(stacked_df3, aes(x = LakeConnec, y = prop_protected, fill = Group)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  ggtitle('b) Protected lake = 80% catchment protected')+
  scale_y_continuous(limits=c(0,30), breaks=seq(0,30,5)) +
  scale_x_discrete(labels=c('IS','HW','DRS','DRLS'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(axis.title.y = element_text(vjust=2.7, color='black'))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("Multi-use cat" = "navajowhite2", "Strict cat" = "olivedrab3"),#,"Unprotected" = "gray70"),
                    labels=c('Strict','Multi-use'))+
  theme(legend.position=c('none'))+ #manually reposition legend inside plot
  theme(legend.text=element_text(size=8))+
  theme(axis.text.y = element_text(color='black'), axis.text.x=element_text(color='black'))+
  theme(legend.title=element_blank()) #remove legend title
stack_gg3
#dev.off()

# multi-panel stacked barplot for different definitions of lake protection
png('Figures/panel_stacked_bar_conn_type_cat100_perm.png',height = 6,width = 4.5,units = 'in',res=300)
  grid.arrange(stack_gg1, stack_gg2, nrow=2)
dev.off()

png('Figures/panel_stacked_bar_conn_type_cat80_perm.png',height = 6,width = 4.5,units = 'in',res=300)
  grid.arrange(stack_gg1, stack_gg3, nrow=2)
dev.off()

png('Figures/panel_stacked_bar_conn_type_cat80_100_perm.png',height = 6,width = 4.5,units = 'in',res=300)
  grid.arrange(stack_gg1, stack_gg2, stack_gg3, nrow=3)
dev.off()

######################### who actually looked this far down? ######################################