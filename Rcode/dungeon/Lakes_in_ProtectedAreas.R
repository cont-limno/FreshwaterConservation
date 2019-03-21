######################## Freshwaters in US protected areas #####################################
# Date: 9-13-18
# updated: 10-24-18
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(LAGOSNE)
library(raster)
library(rgeos)
library(rgdal)
library(tmap)
library(vioplot)
library(dplyr)

#### set working directory to your own! ####
setwd("C:/Users/FWL/Dropbox/MSB-2-2015/Manuscripts/FreshwaterConservation") #IanMcC

#### input data ####
# water quality data
dt <- lagosne_load(version = '1.087.1') #returns list of data.frame objects
secchi <- dt$secchi
epi_nutr <- dt$epi_nutr

# reformat dates and subset to summer period (6/15-9/15) for 1980 onward
secchi$sampledate <- as.Date(secchi$sampledate, format="%m/%d/%Y")
epi_nutr$sampledate <- as.Date(epi_nutr$sampledate, format="%m/%d/%Y")
secchi$monthday <- format(secchi$sampledate, format="%m%d")
epi_nutr$monthday <- format(epi_nutr$sampledate, format="%m%d")
secchi <- secchi[secchi$monthday >= '0615' & secchi$monthday <= '0915',]
epi_nutr <- epi_nutr[epi_nutr$monthday >= '0615' & epi_nutr$monthday <= '0915',]
secchi <- subset(secchi, sampleyear >= 1980)
epi_nutr <- subset(epi_nutr, sampleyear >= 1980)

# GIS data
lakes_4ha_pts <- shapefile("LAGOS_US_GIS/LAGOS_US_All_Lakes_4ha_pts/LAGOS_US_All_Lakes_4ha_pts.shp")
#lakes_4ha_poly <- shapefile("LAGOS_US_GIS/LAGOS_US_All_Lakes_4ha_v0.2.shp")
lakes_1ha_conn <- shapefile("LAGOS_US_GIS/LAGOS_US_All_lakes_1ha_Conn.shp")

states_shp <- shapefile("cb_2016_us_state_500k/lower48.shp")
states_shp <- spTransform(states_shp, CRSobj = crs(lakes_4ha_pts)) #reproject to LAGOS projection

# Watershed shapefiles (LAGOS NE or US)
Buff1500m <- shapefile("LAGOS_US_GIS/LAGOS_US_All_Lakes_4ha_v0.2_1500mBuff.shp")

# table of protected area by different watershed extents (Tabulate Area in ArcGIS from 30m raster version of PADUS)
# contains lands with Gaps 1-3 status
# does not contain rows for watershed with no protected land
PADUS_table_Buff1500m <- read.csv("PADUS/PADUS_1500mBuff/Buff1500m_PADUS_TabulateArea_US.csv")

######## Main program #########

### Individual lake watersheds
# using 1500m buffers until watershed shapefile is ready

## Calculate % Buff1500m protected by Gap status
Buff1500m_pct_protected_shp <- merge(Buff1500m, PADUS_table_Buff1500m, by.x='lagoslakei', by.y='LAGOSLAKEI')
# convert sq meters to hectares then divide by hectare column (Buff1500m area)
# problem with writeOGR if don't convert to hectares
Buff1500m_pct_protected_shp@data$VALUE_1 <- Buff1500m_pct_protected_shp@data$VALUE_1/10000
Buff1500m_pct_protected_shp@data$VALUE_2 <- Buff1500m_pct_protected_shp@data$VALUE_2/10000
Buff1500m_pct_protected_shp@data$VALUE_3 <- Buff1500m_pct_protected_shp@data$VALUE_3/10000
Buff1500m_pct_protected_shp@data$Ha <- gArea(Buff1500m_pct_protected_shp, byid=T)/10000 #calculate area of each buffer in sq meters, convert to hectares

Buff1500m_pct_protected_shp@data$PctGap1 <- Buff1500m_pct_protected_shp@data$VALUE_1/Buff1500m_pct_protected_shp@data$Ha 
Buff1500m_pct_protected_shp@data$PctGap2 <- Buff1500m_pct_protected_shp@data$VALUE_2/Buff1500m_pct_protected_shp@data$Ha 
Buff1500m_pct_protected_shp@data$PctGap3 <- Buff1500m_pct_protected_shp@data$VALUE_3/Buff1500m_pct_protected_shp@data$Ha 
Buff1500m_pct_protected_shp@data$TotalGaps12 <- Buff1500m_pct_protected_shp@data$PctGap1 + Buff1500m_pct_protected_shp@data$PctGap2
Buff1500m_pct_protected_shp@data$TotalGaps123 <- Buff1500m_pct_protected_shp@data$PctGap1 + Buff1500m_pct_protected_shp@data$PctGap2 + Buff1500m_pct_protected_shp@data$PctGap3

# some watersheds may have slightly > 100% protected due to topology or raster cell issues
Buff1500m_pct_protected_shp@data$PctGap1 <- ifelse(Buff1500m_pct_protected_shp@data$PctGap1 > 1, 1, Buff1500m_pct_protected_shp@data$PctGap1)
Buff1500m_pct_protected_shp@data$PctGap2 <- ifelse(Buff1500m_pct_protected_shp@data$PctGap2 > 1, 1, Buff1500m_pct_protected_shp@data$PctGap2)
Buff1500m_pct_protected_shp@data$PctGap3 <- ifelse(Buff1500m_pct_protected_shp@data$PctGap3 > 1, 1, Buff1500m_pct_protected_shp@data$PctGap3)
Buff1500m_pct_protected_shp@data$TotalGaps12 <- ifelse(Buff1500m_pct_protected_shp@data$TotalGaps12 > 1, 1, Buff1500m_pct_protected_shp@data$TotalGaps12)
Buff1500m_pct_protected_shp@data$TotalGaps123 <- ifelse(Buff1500m_pct_protected_shp@data$TotalGaps123 > 1, 1, Buff1500m_pct_protected_shp@data$TotalGaps123)

#writeOGR(Buff1500m_pct_protected_shp, dsn='PADUS/PADUS_1500mBuff', layer='Buff1500m_pct_gaps123_US', 
#          overwrite_layer = T, driver='ESRI Shapefile')

# May be easier to map using lake points rather than polygons:
layover_df <- Buff1500m_pct_protected_shp@data[,c(1,33:41)]
Buff1500m_pct_protected_shp_pts <- merge(lakes_4ha_pts, layover_df, by='lagoslakei')
#writeOGR(Buff1500m_pct_protected_shp_pts, dsn='PADUS/PADUS_1500mBuff', layer='Buff1500m_pct_gaps123_US_pts', 
#          overwrite_layer = T, driver='ESRI Shapefile')

par(mfrow=c(1,2))
boxplot(Buff1500m_pct_protected_shp@data$TotalGaps12, main='Gaps 1-2', las=1, ylim=c(0,1), ylab='Prop watershed protected')
boxplot(Buff1500m_pct_protected_shp@data$TotalGaps123, main='Gaps 1-3', las=1, ylim=c(0,1), ylab='Prop watershed protected')

# NOTE THAT VIOLIN PLOTS DON'T SHOW WATERSHEDS WITH NO PROTECTED LAND
par(mfrow=c(1,1))
vioplot(na.omit(Buff1500m_pct_protected_shp@data$TotalGaps12), na.omit(Buff1500m_pct_protected_shp@data$TotalGaps123),
        names=c('Gaps 1-2', 'Gaps 1-3'), col='white', ylim=c(0,1))
title('Buff1500m PADUS')
mtext(side=3,'Prop watershed protected')

hist(Buff1500m_pct_protected_shp@data$TotalGaps12)

# how many lakes have completely protected watersheds?
nrow(subset(Buff1500m_pct_protected_shp@data, TotalGaps12 >= 1))
nrow(subset(Buff1500m_pct_protected_shp@data, TotalGaps123 >= 1))

# protected watersheds by lake type?
Buff1500m_pct_protected_conn <- merge(Buff1500m_pct_protected_shp@data, lakes_1ha_conn@data, by='lagoslakei', all.x=F)

# create indicator column of fully protected watersheds and make NA = not fully protected
Buff1500m_pct_protected_conn$FullyGaps12 <- ifelse(Buff1500m_pct_protected_conn$TotalGaps12 >= 1, "Fully",'NotFully')
Buff1500m_pct_protected_conn$FullyGaps12[is.na(Buff1500m_pct_protected_conn$FullyGaps12)] <- "NotFully"

Buff1500m_pct_protected_conn$FullyGaps123 <- ifelse(Buff1500m_pct_protected_conn$TotalGaps123 >= 1, "Fully",'NotFully')
Buff1500m_pct_protected_conn$FullyGaps123[is.na(Buff1500m_pct_protected_conn$FullyGaps123)] <- "NotFully"

# how many lakes of each type have fully protected watersheds?
protected_lake_table <- data.frame(Type=c('Headwater','Isolated','DR_Stream','DR_LakeStream'), Gaps12=NA, Gaps13=NA)
protected_lake_table[1,2] <- nrow(subset(Buff1500m_pct_protected_conn, LakeConnec=='Headwater' & TotalGaps12 >=1))
protected_lake_table[2,2] <- nrow(subset(Buff1500m_pct_protected_conn, LakeConnec=='Isolated' & TotalGaps12 >=1))
protected_lake_table[3,2] <-nrow(subset(Buff1500m_pct_protected_conn, LakeConnec=='DR_Stream' & TotalGaps12 >=1))
protected_lake_table[4,2] <-nrow(subset(Buff1500m_pct_protected_conn, LakeConnec=='DR_LakeStream' & TotalGaps12 >=1))
protected_lake_table[1,3] <- nrow(subset(Buff1500m_pct_protected_conn, LakeConnec=='Headwater' & TotalGaps123 >=1))
protected_lake_table[2,3] <- nrow(subset(Buff1500m_pct_protected_conn, LakeConnec=='Isolated' & TotalGaps123 >=1))
protected_lake_table[3,3] <-nrow(subset(Buff1500m_pct_protected_conn, LakeConnec=='DR_Stream' & TotalGaps123 >=1))
protected_lake_table[4,3] <-nrow(subset(Buff1500m_pct_protected_conn, LakeConnec=='DR_LakeStream' & TotalGaps123 >=1))

# add column for number of lakes in each type
protected_lake_table$nLakes <- NA
protected_lake_table[1,4] <- nrow(subset(Buff1500m_pct_protected_conn, LakeConnec=='Headwater'))
protected_lake_table[2,4] <- nrow(subset(Buff1500m_pct_protected_conn, LakeConnec=='Isolated'))
protected_lake_table[3,4] <- nrow(subset(Buff1500m_pct_protected_conn, LakeConnec=='DR_Stream'))
protected_lake_table[4,4] <- nrow(subset(Buff1500m_pct_protected_conn, LakeConnec=='DR_LakeStream'))

# calculate % of each lake type that has fully protected watershed
protected_lake_table$PctGaps12 <- round(protected_lake_table$Gaps12/protected_lake_table$nLakes, 3)
protected_lake_table$PctGaps13 <- round(protected_lake_table$Gaps13/protected_lake_table$nLakes, 3)

# barplot
pcts <- as.matrix(t(protected_lake_table[,c(5,6)]))
barplot(pcts, main="Fully protected watersheds by lake type", ylab='Prop Lakes by type',
        names.arg= protected_lake_table$Type, ylim=c(0,0.02),
        xlab="Lake type", col=c("darkgreen","orange"),
        legend = c('Gaps 1-2','Gaps 1-3'), beside=T)

countz <- as.matrix(t(protected_lake_table[,c(2,3)]))
barplot(countz, main="Fully protected watersheds by lake type", ylab='n Lakes by type',
        names.arg= protected_lake_table$Type, ylim=c(0,700),
        xlab="Lake type", col=c("darkgreen","orange"),
        legend = c('Gaps 1-2','Gaps 1-3'), beside=T)

boxplot(Buff1500m_pct_protected_conn$TotalGaps12 ~ Buff1500m_pct_protected_conn$LakeConnec, las=1,
        ylab='Pct protected', main='Gaps 1-2')
boxplot(Buff1500m_pct_protected_conn$TotalGaps123 ~ Buff1500m_pct_protected_conn$LakeConnec, las=1,
        ylab='Pct protected', main='Gaps 1-3')

headwater_df <- subset(Buff1500m_pct_protected_conn, LakeConnec=='Headwater')
isolated_df <- subset(Buff1500m_pct_protected_conn, LakeConnec=='Isolated')
DR_stream_df <- subset(Buff1500m_pct_protected_conn, LakeConnec=='DR_Stream')
DR_lakestream_df <- subset(Buff1500m_pct_protected_conn, LakeConnec=='DR_LakeStream')

hist(headwater_df$TotalGaps12, xlim=c(0,1), ylim=c(0,20000), col="orange", breaks=seq(0,1,0.1),
     xaxt='n', las=1, xlab='Prop watershed protected', ylab='', main='Headwater lakes')
hist(headwater_df$TotalGaps123, add=T, xlim=c(0,1), ylim=c(0,20000), breaks=seq(0,1,0.1), col=rgb(0.5,0.5,0.5, 0.5),
     xlab='', xaxt='n', yaxt='n', main='')
axis(side=1, at=seq(0,1,0.1), labels=seq(0,1,0.1), cex.axis=0.8)
legend('topright',legend=c('Gaps 1-2','Gaps 1-3'), col=c('orange','gray'), pch=c(15,15), bty='n')

hist(isolated_df$TotalGaps12, xlim=c(0,1), ylim=c(0,20000), col="orange", breaks=seq(0,1,0.1),
     xaxt='n', las=1, xlab='Prop watershed protected', ylab='', main='Isolated lakes')
hist(isolated_df$TotalGaps123, add=T, xlim=c(0,1), ylim=c(0,20000), breaks=seq(0,1,0.1), col=rgb(0.5,0.5,0.5, 0.5),
     xlab='', xaxt='n', yaxt='n', main='')
axis(side=1, at=seq(0,1,0.1), labels=seq(0,1,0.1), cex.axis=0.8)
legend('topright',legend=c('Gaps 1-2','Gaps 1-3'), col=c('orange','gray'), pch=c(15,15), bty='n')

hist(DR_stream_df$TotalGaps12, xlim=c(0,1), ylim=c(0,20000), col="orange", breaks=seq(0,1,0.1),
     xaxt='n', las=1, xlab='Prop watershed protected', ylab='', main='DR_stream lakes')
hist(DR_stream_df$TotalGaps123, add=T, xlim=c(0,1), ylim=c(0,20000), breaks=seq(0,1,0.1), col=rgb(0.5,0.5,0.5, 0.5),
     xlab='', xaxt='n', yaxt='n', main='')
axis(side=1, at=seq(0,1,0.1), labels=seq(0,1,0.1), cex.axis=0.8)
legend('topright',legend=c('Gaps 1-2','Gaps 1-3'), col=c('orange','gray'), pch=c(15,15), bty='n')

hist(DR_lakestream_df$TotalGaps12, xlim=c(0,1), ylim=c(0,20000), col="orange", breaks=seq(0,1,0.1),
     xaxt='n', las=1, xlab='Prop watershed protected', ylab='', main='DR_lakestream lakes')
hist(DR_lakestream_df$TotalGaps123, add=T, xlim=c(0,1), ylim=c(0,20000), breaks=seq(0,1,0.1), col=rgb(0.5,0.5,0.5, 0.5),
     xlab='', xaxt='n', yaxt='n', main='')
axis(side=1, at=seq(0,1,0.1), labels=seq(0,1,0.1), cex.axis=0.8)
legend('topright',legend=c('Gaps 1-2','Gaps 1-3'), col=c('orange','gray'), pch=c(15,15), bty='n')

###### What are the characteristics of fully protected lakes? ######

### Difference in elevation?
elev_df1 <- Buff1500m_pct_protected_conn[,c('Elevation.x','FullyGaps12')]
tapply(elev_df1$Elevation.x, elev_df1$FullyGaps12, summary)
par(mfrow=c(1,2))
elev_df1$lnElev <- log(elev_df1$Elevation.x)
boxplot(Elevation.x ~ FullyGaps12, data=elev_df1, col=c('darkgreen','white'),
        ylab='Elevation (ft)', main='Gaps 1-2', ylim=c(0,100))
# log-transform?
elev_df1b <- subset(elev_df1, lnElev >=0)
boxplot(lnElev ~ FullyGaps12, data=elev_df1b, col=c('darkgreen','white'),
        ylab='ln (Elevation (ft))', main='Gaps 1-2', ylim=c())

# comparing fully vs. not fully protected
# with help from: https://www.r-bloggers.com/two-sample-students-t-test-2/ 
a <- subset(elev_df1, FullyGaps12=='Fully')[,1]
b <- subset(elev_df1, FullyGaps12=='NotFully')[,1]
var.test(a,b) #if p > 0.05, have homoskedasticity
t.test(a,b, var.equal=F, paired=F)# perform's Welch's test with unequal variances and unpaired data

elev_df2 <- Buff1500m_pct_protected_conn[,c('Elevation.x','FullyGaps123')]
tapply(elev_df2$Elevation.x, elev_df2$FullyGaps123, summary)
#boxplot(Elevation.x ~ FullyGaps123, data=elev_df2, col=c('orange','white'),
#        ylab='Elevation (ft)', main='Gaps 1-3', ylim=c(0,100))

a <- subset(elev_df2, FullyGaps123=='Fully')[,1]
b <- subset(elev_df2, FullyGaps123=='NotFully')[,1]
var.test(a,b) #if p > 0.05, have homoskedasticity
t.test(a,b, var.equal=F, paired=F)# perform's Welch's test with unequal variances and unpaired data

### Difference in lake area?
area_df1 <- Buff1500m_pct_protected_conn[,c('Hectares.x','FullyGaps12')]
tapply(area_df1$Hectares.x, area_df1$FullyGaps12, summary)
boxplot(Hectares.x ~ FullyGaps12, data=area_df1, col=c('darkgreen','white'),
        ylab='Lake area (hectares)', main='Gaps 1-2', ylim=c(0,100))
# log transform?
area_df1$lnArea <- log(area_df1$Hectares.x)
boxplot(lnArea ~ FullyGaps12, data=area_df1, col=c('darkgreen','white'),
        ylab='ln(Lake area (hectares))', main='Gaps 1-2', ylim=c())


# comparing fully vs. not fully protected
a <- subset(area_df1, FullyGaps12=='Fully')[,1]
b <- subset(area_df1, FullyGaps12=='NotFully')[,1]
var.test(a,b) #if p > 0.05, have homoskedasticity
t.test(a,b, var.equal=T, paired=F)# set var.equal=T if have homoskedasticity

area_df2 <- Buff1500m_pct_protected_conn[,c('Hectares.x','FullyGaps123')]
tapply(area_df2$Hectares.x, area_df2$FullyGaps123, summary)
#boxplot(Hectares.x ~ FullyGaps123, data=area_df2, col=c('darkgreen','white'),
#        ylab='Lake area (hectares)', main='Gaps 1-3', ylim=c(0,100))

a <- subset(area_df2, FullyGaps123=='Fully')[,1]
b <- subset(area_df2, FullyGaps123=='NotFully')[,1]
var.test(a,b) #if p > 0.05, have homoskedasticity
t.test(a,b, var.equal=F, paired=F)# perform's Welch's test with unequal variances and unpaired data

######## comparison of protected vs. unprotected lakes  ###########
### how many of these lakes have water quality data?
PADUS_12_lakes_ID <- subset(Buff1500m_pct_protected_conn, TotalGaps12 >=1)$lagoslakei
PADUS_123_lakes_ID <- subset(Buff1500m_pct_protected_conn, TotalGaps123 >=1)$lagoslakei

# Secchi
PADUS_12_secchi <- subset(secchi, lagoslakeid %in% PADUS_12_lakes_ID)
length(unique(PADUS_12_secchi$lagoslakeid))/length(PADUS_12_lakes_ID)
nonPADUS_12_secchi <- subset(secchi, !(lagoslakeid %in% PADUS_12_lakes_ID)) #all other lakes with secchi data

PADUS_123_secchi <- subset(secchi, lagoslakeid %in% PADUS_123_lakes_ID)
length(unique(PADUS_123_secchi$lagoslakeid))/length(PADUS_123_lakes_ID)
nonPADUS_123_secchi <- subset(secchi, !(lagoslakeid %in% PADUS_123_lakes_ID))

# epi_nutr
PADUS_12_epi_nutr <- subset(epi_nutr, lagoslakeid %in% PADUS_12_lakes_ID)
length(unique(PADUS_12_epi_nutr$lagoslakeid))/length(PADUS_12_lakes_ID)

PADUS_12_TP <- subset(PADUS_12_epi_nutr, tp >= 0)
length(unique(PADUS_12_TP$lagoslakeid))/length(PADUS_12_lakes_ID)
nonPADUS_12_TP <- subset(epi_nutr, !(lagoslakeid %in% PADUS_12_lakes_ID))

PADUS_12_chla <- subset(PADUS_12_epi_nutr, chla >= 0)
length(unique(PADUS_12_chla$lagoslakeid))/length(PADUS_12_lakes_ID)
nonPADUS_12_chla <- subset(epi_nutr, !(lagoslakeid %in% PADUS_12_lakes_ID))

PADUS_123_epi_nutr <- subset(epi_nutr, lagoslakeid %in% PADUS_123_lakes_ID)
length(unique(PADUS_123_epi_nutr$lagoslakeid))/length(PADUS_123_lakes_ID)

PADUS_123_TP <- subset(PADUS_123_epi_nutr, tp >= 0)
length(unique(PADUS_123_TP$lagoslakeid))/length(PADUS_123_lakes_ID)
nonPADUS_123_TP <- subset(epi_nutr, !(lagoslakeid %in% PADUS_123_lakes_ID))

PADUS_123_chla <- subset(PADUS_123_epi_nutr, chla >= 0)
length(unique(PADUS_123_chla$lagoslakeid))/length(PADUS_123_lakes_ID)
nonPADUS_123_chla <- subset(epi_nutr, !(lagoslakeid %in% PADUS_123_lakes_ID))

## water quality different in PADUS lakes? 
PADUS_12_secchi_mean <- as.data.frame(PADUS_12_secchi %>% 
  group_by(lagoslakeid) %>%
  summarize(nObs = n(), mean = mean(secchi, na.rm=T)))
PADUS_12_secchi_mean$PADUS_gap <- 'Gaps1-2'
summary(PADUS_12_secchi_mean$mean)

nonPADUS_12_secchi_mean <- as.data.frame(nonPADUS_12_secchi %>% 
  group_by(lagoslakeid) %>%
  summarize(nObs = n(), mean = mean(secchi, na.rm=T)))
nonPADUS_12_secchi_mean$PADUS_gap <- 'nonGaps1-2'
summary(nonPADUS_12_secchi_mean$mean)

par(mfrow=c(1,2))
PADUS_12_secchi_BOX <- rbind.data.frame(PADUS_12_secchi_mean, nonPADUS_12_secchi_mean)
boxplot(mean ~ PADUS_gap, data=PADUS_12_secchi_BOX, las=1, ylab='Mean Secchi', col=c('darkgreen','white'))

PADUS_123_secchi_mean <- as.data.frame(PADUS_123_secchi %>% 
  group_by(lagoslakeid) %>%
  summarize(nObs = n(), mean = mean(secchi, na.rm=T)))
PADUS_123_secchi_mean$PADUS_gap <- 'Gaps1-3'
summary(PADUS_123_secchi_mean$mean)

nonPADUS_123_secchi_mean <- as.data.frame(nonPADUS_123_secchi %>% 
  group_by(lagoslakeid) %>%
  summarize(nObs = n(), mean = mean(secchi, na.rm=T)))
nonPADUS_123_secchi_mean$PADUS_gap <- 'nonGaps1-3'
summary(nonPADUS_123_secchi_mean$mean)

PADUS_123_secchi_BOX <- rbind.data.frame(PADUS_123_secchi_mean, nonPADUS_123_secchi_mean)
boxplot(mean ~ PADUS_gap, data=PADUS_123_secchi_BOX, las=1, ylab='Mean Secchi', col=c('orange','white'))

## TP
PADUS_12_TP_mean <- as.data.frame(PADUS_12_TP %>% 
                                     group_by(lagoslakeid) %>%
                                     summarize(nObs = n(), mean = mean(tp, na.rm=T)))
PADUS_12_TP_mean$PADUS_gap <- 'Gaps1-2'
summary(PADUS_12_TP_mean$mean)

nonPADUS_12_TP_mean <- as.data.frame(nonPADUS_12_TP %>% 
                                        group_by(lagoslakeid) %>%
                                        summarize(nObs = n(), mean = mean(tp, na.rm=T)))
nonPADUS_12_TP_mean$PADUS_gap <- 'nonGaps1-2'
summary(nonPADUS_12_TP_mean$mean)

PADUS_12_TP_BOX <- rbind.data.frame(PADUS_12_TP_mean, nonPADUS_12_TP_mean)
boxplot(mean ~ PADUS_gap, data=PADUS_12_TP_BOX, las=1, ylab='Mean TP', ylim=c(0,80), col=c('darkgreen','white'))

PADUS_123_TP_mean <- as.data.frame(PADUS_123_TP %>% 
                                       group_by(lagoslakeid) %>%
                                       summarize(nObs = n(), mean = mean(tp, na.rm=T)))
PADUS_123_TP_mean$PADUS_gap <- 'Gaps1-3'
summary(PADUS_123_TP_mean$mean)

nonPADUS_123_TP_mean <- as.data.frame(nonPADUS_123_TP %>% 
                                          group_by(lagoslakeid) %>%
                                          summarize(nObs = n(), mean = mean(tp, na.rm=T)))
nonPADUS_123_TP_mean$PADUS_gap <- 'nonGaps1-3'
summary(nonPADUS_123_TP_mean$mean)

PADUS_123_TP_BOX <- rbind.data.frame(PADUS_123_TP_mean, nonPADUS_123_TP_mean)
  boxplot(mean ~ PADUS_gap, data=PADUS_123_TP_BOX, las=1, ylab='Mean TP', ylim=c(0,80), col=c('orange','white'))

# chla
PADUS_12_chla_mean <- as.data.frame(PADUS_12_chla %>% 
                                       group_by(lagoslakeid) %>%
                                       summarize(nObs = n(), mean = mean(chla, na.rm=T)))
PADUS_12_chla_mean$PADUS_gap <- 'Gaps1-2'
summary(PADUS_12_chla_mean$mean)

nonPADUS_12_chla_mean <- as.data.frame(nonPADUS_12_chla %>% 
                                          group_by(lagoslakeid) %>%
                                          summarize(nObs = n(), mean = mean(chla, na.rm=T)))
nonPADUS_12_chla_mean$PADUS_gap <- 'nonGaps1-2'
summary(nonPADUS_12_chla_mean$mean)

PADUS_12_chla_BOX <- rbind.data.frame(PADUS_12_chla_mean, nonPADUS_12_chla_mean)
boxplot(mean ~ PADUS_gap, data=PADUS_12_chla_BOX, las=1, ylab='Mean chla', ylim=c(0,50), col=c('darkgreen','white'))

PADUS_123_chla_mean <- as.data.frame(PADUS_123_chla %>% 
                                         group_by(lagoslakeid) %>%
                                         summarize(nObs = n(), mean = mean(chla, na.rm=T)))
PADUS_123_chla_mean$PADUS_gap <- 'Gaps1-3'
summary(PADUS_123_chla_mean$mean)

nonPADUS_123_chla_mean <- as.data.frame(nonPADUS_123_chla %>% 
                                            group_by(lagoslakeid) %>%
                                            summarize(nObs = n(), mean = mean(chla, na.rm=T)))
nonPADUS_123_chla_mean$PADUS_gap <- 'nonGaps1-3'
summary(nonPADUS_123_chla_mean$mean)

PADUS_123_chla_BOX <- rbind.data.frame(PADUS_123_chla_mean, nonPADUS_123_chla_mean)
boxplot(mean ~ PADUS_gap, data=PADUS_123_chla_BOX, las=1, ylab='Mean chla', ylim=c(0,50), col=c('orange','white'))

# IWS connectivity
PADUS_12_conn_iws <- subset(dt$iws.conn, lagoslakeid %in% PADUS_12_lakes_ID)
PADUS_12_conn_iws$PADUS_gap <- 'Gaps1-2'
nonPADUS_12_conn_iws <- subset(dt$iws.conn, !(lagoslakeid %in% PADUS_12_lakes_ID))
nonPADUS_12_conn_iws$PADUS_gap <- 'nonGaps1-2'
PADUS_12_conn_iws_BOX <- rbind.data.frame(PADUS_12_conn_iws, nonPADUS_12_conn_iws)
PADUS_12_conn_iws_BOX$lnLakes <- log(PADUS_12_conn_iws_BOX$iws_lakes_overlapping_area_pct)
PADUS_12_conn_iws_BOX_sub <- subset(PADUS_12_conn_iws_BOX, !(lnLakes == '-Inf'))

par(mfrow=c(2,3))
#boxplot(iws_lakes_overlapping_area_pct ~ PADUS_gap, data=PADUS_12_conn_iws_BOX, las=1, ylim=c(), main='IWS lakes', col=c('darkgreen','white'), ylab='% cover')
boxplot(lnLakes ~ PADUS_gap, data=PADUS_12_conn_iws_BOX_sub, las=1, ylim=c(), main='ln IWS lakes', col=c('darkgreen','white'), ylab='% cover')
boxplot(iws_streamdensity_streams_density_mperha ~ PADUS_gap, data=PADUS_12_conn_iws_BOX, las=1, ylim=c(0,40), main='IWS stream density', col=c('darkgreen','white'), ylab='mperha')
boxplot(iws_wl_allwetlandsdissolved_overlapping_area_pct ~ PADUS_gap, data=PADUS_12_conn_iws_BOX, las=1, ylim=c(), main='IWS wetlands', col=c('darkgreen','white'), ylab='% cover')

PADUS_123_conn_iws <- subset(dt$iws.conn, lagoslakeid %in% PADUS_123_lakes_ID)
PADUS_123_conn_iws$PADUS_gap <- 'Gaps1-3'
nonPADUS_123_conn_iws <- subset(dt$iws.conn, !(lagoslakeid %in% PADUS_123_lakes_ID))
nonPADUS_123_conn_iws$PADUS_gap <- 'nonGaps1-3'
PADUS_123_conn_iws_BOX <- rbind.data.frame(PADUS_123_conn_iws, nonPADUS_123_conn_iws)
PADUS_123_conn_iws_BOX$lnLakes <- log(PADUS_123_conn_iws_BOX$iws_lakes_overlapping_area_pct)
PADUS_123_conn_iws_BOX_sub <- subset(PADUS_123_conn_iws_BOX, !(lnLakes == '-Inf'))

#boxplot(iws_lakes_overlapping_area_pct ~ PADUS_gap, data=PADUS_123_conn_iws_BOX, las=1, col=c('orange','white'), ylab='% cover')
boxplot(lnLakes ~ PADUS_gap, data=PADUS_123_conn_iws_BOX_sub, las=1, ylim=c(), col=c('orange','white'), ylab='% cover')
boxplot(iws_streamdensity_streams_density_mperha ~ PADUS_gap, data=PADUS_123_conn_iws_BOX, las=1, ylim=c(0,40), col=c('orange','white'), ylab='mperha')
boxplot(iws_wl_allwetlandsdissolved_overlapping_area_pct ~ PADUS_gap, data=PADUS_123_conn_iws_BOX, las=1, ylim=c(), col=c('orange','white'), ylab='% cover')

# IWS LULC
PADUS_12_lulc_iws <- subset(dt$iws.lulc, lagoslakeid %in% PADUS_12_lakes_ID)
PADUS_12_lulc_iws$PADUS_gap <- 'Gaps1-2'
nonPADUS_12_lulc_iws <- subset(dt$iws.lulc, !(lagoslakeid %in% PADUS_12_lakes_ID))
nonPADUS_12_lulc_iws$PADUS_gap <- 'nonGaps1-2'
PADUS_12_lulc_iws_BOX <- rbind.data.frame(PADUS_12_lulc_iws, nonPADUS_12_lulc_iws)
PADUS_12_lulc_iws_BOX$TotalForest2011_pct <- PADUS_12_lulc_iws_BOX$iws_nlcd2006_pct_41 + PADUS_12_lulc_iws_BOX$iws_nlcd2006_pct_42 + PADUS_12_lulc_iws_BOX$iws_nlcd2006_pct_43
PADUS_12_lulc_iws_BOX$TotalAg2011_pct <- PADUS_12_lulc_iws_BOX$iws_nlcd2006_pct_81 + PADUS_12_lulc_iws_BOX$iws_nlcd2006_pct_82

par(mfrow=c(2,3))
boxplot(iws_nlcd2011_pct_42 ~ PADUS_gap, data=PADUS_12_lulc_iws_BOX, las=1, main='Coniferous forest', col=c('darkgreen','white'), ylab='% cover')
boxplot(TotalForest2011_pct ~ PADUS_gap, data=PADUS_12_lulc_iws_BOX, las=1, main='Total forest', col=c('darkgreen','white'), ylab='% cover')
boxplot(iws_roaddensity_density_mperha ~ PADUS_gap, data=PADUS_12_lulc_iws_BOX, las=1, ylim=c(0,100), main='Road density', col=c('darkgreen','white'), ylab='mperha')
boxplot(TotalAg2011_pct ~ PADUS_gap, data=PADUS_12_lulc_iws_BOX, las=1, ylim=c(), main='Total ag', col=c('darkgreen','white'), ylab='% cover')
boxplot(iws_tri_mean ~ PADUS_gap, data=PADUS_12_lulc_iws_BOX, las=1, ylim=c(), main='Mean TRI', col=c('darkgreen','white'))

## Gaps 1-3
PADUS_123_lulc_iws <- subset(dt$iws.lulc, lagoslakeid %in% PADUS_123_lakes_ID)
PADUS_123_lulc_iws$PADUS_gap <- 'Gaps1-3'
nonPADUS_123_lulc_iws <- subset(dt$iws.lulc, !(lagoslakeid %in% PADUS_123_lakes_ID))
nonPADUS_123_lulc_iws$PADUS_gap <- 'nonGaps1-3'
PADUS_123_lulc_iws_BOX <- rbind.data.frame(PADUS_123_lulc_iws, nonPADUS_123_lulc_iws)
PADUS_123_lulc_iws_BOX$TotalForest2011_pct <- PADUS_123_lulc_iws_BOX$iws_nlcd2006_pct_41 + PADUS_123_lulc_iws_BOX$iws_nlcd2006_pct_42 + PADUS_123_lulc_iws_BOX$iws_nlcd2006_pct_43
PADUS_123_lulc_iws_BOX$TotalAg2011_pct <- PADUS_123_lulc_iws_BOX$iws_nlcd2006_pct_81 + PADUS_123_lulc_iws_BOX$iws_nlcd2006_pct_82

par(mfrow=c(2,3))
boxplot(iws_nlcd2011_pct_42 ~ PADUS_gap, data=PADUS_123_lulc_iws_BOX, las=1, main='Coniferous forest', col=c('orange','white'), ylab='% cover')
boxplot(TotalForest2011_pct ~ PADUS_gap, data=PADUS_123_lulc_iws_BOX, las=1, main='Total forest', col=c('orange','white'), ylab='% cover')
boxplot(iws_roaddensity_density_mperha ~ PADUS_gap, data=PADUS_12_lulc_iws_BOX, las=1, ylim=c(0,100), main='Road density', col=c('orange','white'), ylab='mperha')
boxplot(TotalAg2011_pct ~ PADUS_gap, data=PADUS_123_lulc_iws_BOX, las=1, ylim=c(), main='Total ag', col=c('orange','white'), ylab='% cover')
boxplot(iws_tri_mean ~ PADUS_gap, data=PADUS_123_lulc_iws_BOX, las=1, ylim=c(), main='Mean TRI', col=c('orange','white'))

# IWS morphometry variables
PADUS_12_iws <- subset(dt$iws, lagoslakeid %in% PADUS_12_lakes_ID)
PADUS_12_iws$PADUS_gap <- 'Gaps1-2'
nonPADUS_12_iws <- subset(dt$iws, !(lagoslakeid %in% PADUS_12_lakes_ID))
nonPADUS_12_iws$PADUS_gap <- 'nonGaps1-2'
PADUS_12_iws_BOX <- rbind.data.frame(PADUS_12_iws, nonPADUS_12_iws)
PADUS_12_iws_BOX$WRT <- PADUS_12_iws_BOX$iws_ha/PADUS_12_iws_BOX$iws_lakeareaha

par(mfrow=c(2,3))
boxplot(iws_ha ~ PADUS_gap, data=PADUS_12_iws_BOX, las=1, main='Watershed area', ylim=c(0,2000), ylab='hectares', col=c('darkgreen','white'))
boxplot(iws_lakeareaha ~ PADUS_gap, data=PADUS_12_iws_BOX, las=1, main='Lake area', ylim=c(0,100), ylab='hectares', col=c('darkgreen','white'))
boxplot(WRT ~ PADUS_gap, data=PADUS_12_iws_BOX, las=1, ylim=c(0,100), main='WRT', col=c('darkgreen','white'))

## Gaps 1-3
PADUS_123_iws <- subset(dt$iws, lagoslakeid %in% PADUS_123_lakes_ID)
PADUS_123_iws$PADUS_gap <- 'Gaps1-3'
nonPADUS_123_iws <- subset(dt$iws, !(lagoslakeid %in% PADUS_123_lakes_ID))
nonPADUS_123_iws$PADUS_gap <- 'nonGaps1-3'
PADUS_123_iws_BOX <- rbind.data.frame(PADUS_123_iws, nonPADUS_123_iws)
PADUS_123_iws_BOX$WRT <- PADUS_123_iws_BOX$iws_ha/PADUS_123_iws_BOX$iws_lakeareaha

boxplot(iws_ha ~ PADUS_gap, data=PADUS_123_iws_BOX, las=1, ylim=c(0,2000), ylab='hectares', col=c('orange','white'))
boxplot(iws_lakeareaha ~ PADUS_gap, data=PADUS_123_iws_BOX, las=1, ylim=c(0,100), ylab='hectares', col=c('orange','white'))
boxplot(WRT ~ PADUS_gap, data=PADUS_123_iws_BOX, las=1, ylim=c(0,100), col=c('orange','white'))

# climate (slow to load)
# from:
# Collins, S. M., et al. 2018. LAGOS-NE Annual, seasonal, and monthly climate data for lakes 
# and watersheds in a 17-state region of the U.S.. 
# Environmental Data Initiative. http://dx.doi:10.6073/pasta/4abe86a2c00dc9a628924aa149d7bf34. 
# Dataset accessed 6/19/2018.
big_climate_df <- read.csv('lagoslakeid_PRISM_Normals_1981_2010.csv')
big_climate_df[,1] <- NULL #delete useless first column
clim_var_colnames <- big_climate_df$Var #create vector of clim variable names

big_climate_df <- as.data.frame(t(big_climate_df)) #transpose to get lagoslakeid as a column
colnames(big_climate_df) <- clim_var_colnames #rename columns by clim var name vector above
big_climate_df <- big_climate_df[-1,] #delete useless first row

#create column of lagoslakeid
lagoslakeids <- rownames(big_climate_df)
lagoslakeids <- gsub("X","", lagoslakeids)
big_climate_df$lagoslakeid <- lagoslakeids
big_climate_df <- big_climate_df[,c(length(big_climate_df),1,3:length(big_climate_df)-1)]

# convert factors to numeric
w <- which( sapply(big_climate_df, class ) == 'factor' )
big_climate_df[w] <- lapply(big_climate_df[w], function(x) as.numeric(as.character(x)) )

PADUS_12_climate <- subset(big_climate_df, lagoslakeid %in% PADUS_12_lakes_ID)
PADUS_12_climate$PADUS_gap <- 'Gaps1-2'
nonPADUS_12_climate <- subset(big_climate_df, !(lagoslakeid %in% PADUS_12_lakes_ID))
nonPADUS_12_climate$PADUS_gap <- 'nonGaps1-2'
PADUS_12_climate_BOX <- rbind.data.frame(PADUS_12_climate, nonPADUS_12_climate)

PADUS_123_climate <- subset(big_climate_df, lagoslakeid %in% PADUS_123_lakes_ID)
PADUS_123_climate$PADUS_gap <- 'Gaps1-3'
nonPADUS_123_climate <- subset(big_climate_df, !(lagoslakeid %in% PADUS_123_lakes_ID))
nonPADUS_123_climate$PADUS_gap <- 'nonGaps1-3'
PADUS_123_climate_BOX <- rbind.data.frame(PADUS_123_climate, nonPADUS_123_climate)

par(mfrow=c(1,2))
boxplot(wytmean ~ PADUS_gap, data=PADUS_12_climate_BOX, col=c('darkgreen','white'), main='Mean annual temp', ylab='deg C', ylim=c(), las=1)
boxplot(wytmean ~ PADUS_gap, data=PADUS_123_climate_BOX, col=c('orange','white'), main='Mean annual temp', ylab='deg C', ylim=c(), las=1)

boxplot(wyppt ~ PADUS_gap, data=PADUS_12_climate_BOX, col=c('darkgreen','white'), main='Mean annual ppt', ylab='mm', ylim=c(), las=1)
boxplot(wyppt ~ PADUS_gap, data=PADUS_123_climate_BOX, col=c('orange','white'), main='Mean annual ppt', ylab='mm', ylim=c(), las=1)



########### compare % protected to different variables ############
yvar <- (c('TotalGaps12','TotalGaps123'))
xvar <- 'Elevation'
plot(Buff1500m_pct_protected_shp@data[,yvar[1]] ~ Buff1500m_pct_protected_shp@data[,xvar], pch=20,
     ylim=c(0,1), ylab=yvar[1], xlab=xvar)
plot_cor <- cor.test(Buff1500m_pct_protected_shp@data[,yvar[1]], Buff1500m_pct_protected_shp@data[,xvar], 
                     method='pearson',alternative='two.sided', conf.level=0.95)
mtext(side=3, paste0('r = ', round(plot_cor$estimate,2),', p = ', round(plot_cor$p.value,4)))
plot(Buff1500m_pct_protected_shp@data[,yvar[2]] ~ Buff1500m_pct_protected_shp@data[,xvar], pch=20,
     ylim=c(0,1), ylab=yvar[2], xlab=xvar)
plot_cor <- cor.test(Buff1500m_pct_protected_shp@data[,yvar[2]], Buff1500m_pct_protected_shp@data[,xvar], 
                     method='pearson',alternative='two.sided', conf.level=0.95)
mtext(side=3, paste0('r = ', round(plot_cor$estimate,2),', p = ', round(plot_cor$p.value,4)))

# log transform elevation
elev_temp_df <- data.frame(TotalGaps12=Buff1500m_pct_protected_shp@data$TotalGaps12,
                           TotalGaps123=Buff1500m_pct_protected_shp@data$TotalGaps123,
                           lnElev=log(Buff1500m_pct_protected_shp@data$Elevation))
elev_temp_df <- subset(elev_temp_df, lnElev > 0)
plot(elev_temp_df$TotalGaps12 ~ elev_temp_df$lnElev, pch=20,
     ylim=c(0,1), ylab=yvar[1], xlab='lnElevation')
plot_cor <- cor.test(elev_temp_df$TotalGaps12, elev_temp_df$lnElev, 
                     method='pearson',alternative='two.sided', conf.level=0.95)
mtext(side=3, paste0('r = ', round(plot_cor$estimate,2),', p = ', round(plot_cor$p.value,4)))
plot(elev_temp_df$TotalGaps123 ~ elev_temp_df$lnElev, pch=20,
     ylim=c(0,1), ylab=yvar[2], xlab='lnElevation')
plot_cor <- cor.test(elev_temp_df$TotalGaps123, elev_temp_df$lnElev, 
                     method='pearson',alternative='two.sided', conf.level=0.95)
mtext(side=3, paste0('r = ', round(plot_cor$estimate,2),', p = ', round(plot_cor$p.value,4)))


xvar <- 'Hectares'
plot(Buff1500m_pct_protected_shp@data[,yvar[1]] ~ Buff1500m_pct_protected_shp@data[,xvar], pch=20,
     ylim=c(0,1), ylab=yvar[1], xlab=xvar)
plot_cor <- cor.test(Buff1500m_pct_protected_shp@data[,yvar[1]], Buff1500m_pct_protected_shp@data[,xvar], 
                     method='pearson',alternative='two.sided', conf.level=0.95)
mtext(side=3, paste0('r = ', round(plot_cor$estimate,2),', p = ', round(plot_cor$p.value,4)))
plot(Buff1500m_pct_protected_shp@data[,yvar[2]] ~ Buff1500m_pct_protected_shp@data[,xvar], pch=20,
     ylim=c(0,1), ylab=yvar[2], xlab=xvar)
plot_cor <- cor.test(Buff1500m_pct_protected_shp@data[,yvar[2]], Buff1500m_pct_protected_shp@data[,xvar], 
                     method='pearson',alternative='two.sided', conf.level=0.95)
mtext(side=3, paste0('r = ', round(plot_cor$estimate,2),', p = ', round(plot_cor$p.value,4)))

# log transform lake area?
xvar <- 'Hectares'
plot(Buff1500m_pct_protected_shp@data[,yvar[1]] ~ log(Buff1500m_pct_protected_shp@data[,xvar]), pch=20,
     ylim=c(0,1), ylab=yvar[1], xlab=xvar)
plot_cor <- cor.test(Buff1500m_pct_protected_shp@data[,yvar[1]], log(Buff1500m_pct_protected_shp@data[,xvar]), 
                     method='pearson',alternative='two.sided', conf.level=0.95)
mtext(side=3, paste0('r = ', round(plot_cor$estimate,2),', p = ', round(plot_cor$p.value,4)))
plot(Buff1500m_pct_protected_shp@data[,yvar[2]] ~ log(Buff1500m_pct_protected_shp@data[,xvar]), pch=20,
     ylim=c(0,1), ylab=yvar[2], xlab=xvar)
plot_cor <- cor.test(Buff1500m_pct_protected_shp@data[,yvar[2]], log(Buff1500m_pct_protected_shp@data[,xvar]), 
                     method='pearson',alternative='two.sided', conf.level=0.95)
mtext(side=3, paste0('r = ', round(plot_cor$estimate,2),', p = ', round(plot_cor$p.value,4)))

# Connectivity, LULC and terrain data (recognizing 1500m buffs not really IWS AND that only have LAGOS NE lakes)
# first, create data frame of selected variables
iws_conn <- dt$iws.conn
iws_lulc <- dt$iws.lulc

iws_lulc_sub <- iws_lulc[,c('lagoslakeid','iws_roaddensity_density_mperha','iws_tri_mean','iws_nlcd2011_pct_41',
                            'iws_nlcd2011_pct_42','iws_nlcd2011_pct_43','iws_nlcd2011_pct_81','iws_nlcd2011_pct_82',
                            'iws_nlcd2011_pct_22','iws_nlcd2011_pct_23','iws_nlcd2011_pct_24')]

names(iws_lulc_sub) <- c('lagoslakeid','iws_roaddensity_density_mperha','iws_tri_mean','iws_deciduous_pct',
                         'iws_coniferous_pct','iws_mixed_pct','iws_pasture_pct','iws_cultivated_pct',
                         'iws_developed_low_pct','iws_developed_med_pct','iws_developed_high_pct') 

iws_df <- iws_conn[,c('lagoslakeid','iws_lakes_overlapping_area_pct','iws_streamdensity_streams_density_mperha','iws_wl_allwetlandsdissolved_overlapping_area_pct')]
iws_df <- merge(iws_df, iws_lulc_sub, by='lagoslakeid')
iws_df$TotalForest_pct <- iws_df$iws_coniferous_pct + iws_df$iws_deciduous_pct + iws_df$iws_mixed_pct
iws_df$TotalAg_pct <- iws_df$iws_pasture_pct + iws_df$iws_cultivated_pct

# merge data frame of selected variables to protected watersheds
iws_df <- merge(iws_df, Buff1500m_pct_protected_shp@data, by.x='lagoslakeid', by.y='lagoslakei')

xvars <- names(iws_df)[2:16]
par(mfrow=c(1,2))
for (i in 1:length(xvars)){
  plot(iws_df[,yvar[1]] ~ iws_df[,xvars[i]], pch=20,ylim=c(0,1), ylab=yvar[1], xlab=xvars[i], las=1)
  plot_cor <- cor.test(iws_df[,yvar[1]], iws_df[,xvars[i]], 
                       method='pearson',alternative='two.sided', conf.level=0.95)
  mtext(side=3, paste0('r = ', round(plot_cor$estimate,2),', p = ', round(plot_cor$p.value,4)))
  plot(iws_df[,yvar[2]] ~ iws_df[,xvars[i]], pch=20,ylim=c(0,1), ylab=yvar[2], xlab=xvars[i], las=1)
  plot_cor <- cor.test(iws_df[,yvar[2]], iws_df[,xvars[i]], 
                       method='pearson',alternative='two.sided', conf.level=0.95)
  mtext(side=3, paste0('r = ', round(plot_cor$estimate,2),', p = ', round(plot_cor$p.value,4)))
  plot_cor=NULL
}

# how many lakes have completely protected watersheds? (in LAGOS NE)
nrow(subset(iws_df, TotalGaps12 >= 1))
nrow(subset(iws_df, TotalGaps123 >= 1))

############### Map fully protected lakes ##################
# all
FullyGaps12 <- subset(lakes_4ha_pts, lagoslakei %in% PADUS_12_lakes_ID)
plot(states_shp, main='Fully protected, gaps 1-2')
plot(FullyGaps12, add=T, pch=20, col='darkgreen')

FullyGaps123 <- subset(lakes_4ha_pts, lagoslakei %in% PADUS_123_lakes_ID)
plot(states_shp, main='Fully protected, gaps 1-3')
plot(FullyGaps123, add=T, pch=20, col='orange')

# headwaters
par(mfrow=c(1,1))
FullyGaps12_HW <- subset(FullyGaps12, lagoslakei %in% headwater_df$lagoslakei)
FullyGaps123_HW <- subset(FullyGaps123, lagoslakei %in% headwater_df$lagoslakei)
all_HW_lakes <- subset(lakes_4ha_pts, lagoslakei %in% headwater_df$lagoslakei)
plot(states_shp, main='Fully protected headwater lakes')
plot(all_HW_lakes, add=T, pch=20, col='lightgray')
plot(FullyGaps123_HW, add=T, pch=20, col='orange')
plot(FullyGaps12_HW, add=T, pch=20, col='darkgreen')
mtext(side=3, paste0('Gaps 1-2: ',nrow(FullyGaps12_HW@data), ' lakes, ','Gaps 1-3: ',nrow(FullyGaps123_HW@data), ' lakes'))
legend('bottomleft', legend=c('Gaps 1-2','Gaps 1-3','All headwater'), col=c('darkgreen','orange','lightgray'), pch=c(16,16,16))

# isolated lakes
FullyGaps12_IS <- subset(FullyGaps12, lagoslakei %in% isolated_df$lagoslakei)
FullyGaps123_IS <- subset(FullyGaps123, lagoslakei %in% isolated_df$lagoslakei)
all_IS_lakes <- subset(lakes_4ha_pts, lagoslakei %in% isolated_df$lagoslakei)
plot(states_shp, main='Fully protected isolated lakes')
plot(all_IS_lakes, add=T, pch=20, col='lightgray')
plot(FullyGaps123_IS, add=T, pch=20, col='orange')
plot(FullyGaps12_IS, add=T, pch=20, col='darkgreen')
mtext(side=3, paste0('Gaps 1-2: ',nrow(FullyGaps12_IS@data), ' lakes, ','Gaps 1-3: ',nrow(FullyGaps123_IS@data), ' lakes'))
legend('bottomleft', legend=c('Gaps 1-2','Gaps 1-3','All isolated'), col=c('darkgreen','orange','lightgray'), pch=c(16,16,16))

# DR lakestream
FullyGaps12_DRLS <- subset(FullyGaps12, lagoslakei %in% DR_lakestream_df$lagoslakei)
FullyGaps123_DRLS <- subset(FullyGaps123, lagoslakei %in% DR_lakestream_df$lagoslakei)
all_DRLS_lakes <- subset(lakes_4ha_pts, lagoslakei %in% DR_lakestream_df$lagoslakei)
plot(states_shp, main='Fully protected DR lakestream lakes')
plot(all_DRLS_lakes, add=T, pch=20, col='lightgray')
plot(FullyGaps123_DRLS, add=T, pch=20, col='orange')
plot(FullyGaps12_DRLS, add=T, pch=20, col='darkgreen')
mtext(side=3, paste0('Gaps 1-2: ',nrow(FullyGaps12_DRLS@data), ' lakes, ','Gaps 1-3: ',nrow(FullyGaps123_DRLS@data), ' lakes'))
legend('bottomleft', legend=c('Gaps 1-2','Gaps 1-3','All DR lakestream'), col=c('darkgreen','orange','lightgray'), pch=c(16,16,16))

# DR stream lakes
FullyGaps12_DRS <- subset(FullyGaps12, lagoslakei %in% DR_stream_df$lagoslakei)
FullyGaps123_DRS <- subset(FullyGaps123, lagoslakei %in% DR_stream_df$lagoslakei)
all_DRS_lakes <- subset(lakes_4ha_pts, lagoslakei %in% DR_stream_df$lagoslakei)
plot(states_shp, main='Fully protected DR stream lakes')
plot(all_DRS_lakes, add=T, pch=20, col='lightgray')
plot(FullyGaps123_DRS, add=T, pch=20, col='orange')
plot(FullyGaps12_DRS, add=T, pch=20, col='darkgreen')
mtext(side=3, paste0('Gaps 1-2: ',nrow(FullyGaps12_DRS@data), ' lakes, ','Gaps 1-3: ',nrow(FullyGaps123_DRS@data), ' lakes'))
legend('bottomleft', legend=c('Gaps 1-2','Gaps 1-3','All DR stream'), col=c('darkgreen','orange','lightgray'), pch=c(16,16,16))

########################### end ############################