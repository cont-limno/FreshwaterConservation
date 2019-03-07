####################### Lake protection by LAGOS lake connectivity class #######################
# Date: 2-7-19
# updated: 3-7-19
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(dplyr)
library(reshape2)
library(ggplot2)

#### Input data ####
setwd('C:/Users/FWL/Documents/FreshwaterConservation')

PADUS_table <- read.csv("Data/PADUS.csv")

NHD_pts <- shapefile("C:/Ian_GIS/NHD/NHD_waterbody_pts/NHD_waterbody_pts.shp")
NHD_pts <- subset(NHD_pts, FTYPE=='LakePond' | FTYPE=='Reservoir')

# X-walk table for linking NHD to LAGOS lakes
xwalk <- read.csv("C:/Ian_GIS/FreshwaterConservation/LAGOS_Lake_Link_v1.csv") #too large for github

# LAGOS-US lakes >= 1ha with connectivity classes
LAGOS_1ha <- shapefile("C:/Ian_GIS/LAGOS_US_GIS/LAGOS_US_All_lakes_1ha_Conn.shp")

#### Main program ####
# add in lake connectivity classes from LAGOS
xwalk_reduced <- xwalk[,c('lagoslakeid','nhdplusv2_reachcode','nhdplusv2_comid')] #keep only desired columns in xwalk table for joining
xwalk_reduced <- xwalk_reduced[!duplicated(xwalk_reduced$lagoslakeid),] #first remove duplicate lagoslakeids
LAGOS_1ha_xwalk <- merge(LAGOS_1ha@data[,c('lagoslakei','LakeConnec')], xwalk_reduced, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
LAGOS_1ha_xwalk <- LAGOS_1ha_xwalk[!duplicated(LAGOS_1ha_xwalk$nhdplusv2_comid),] #remove dup COMID
#write.csv(LAGOS_1ha_xwalk, "Data/LakeConn_1ha_LAGOS_NHD_Xwalk.csv")

NHD_pts_LAGOS1ha_conn <- merge(NHD_pts@data, LAGOS_1ha_xwalk, by.x='COMID',by.y='nhdplusv2_comid', all.x=F)

LakeConn_countz <- as.data.frame(NHD_pts_LAGOS1ha_conn %>%
                                   group_by(LakeConnec) %>%
                                   tally())

barplot(LakeConn_countz$n, names.arg=LakeConn_countz$LakeConnec, las=1)

# Compare lake protection across connectivity classes
PADUS_LakeCat <- PADUS_table
PADUS_LakeCat$PctGAP_Status12Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat
PADUS_LakeCat$PctGAP_Status123Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat + PADUS_LakeCat$PctGAP_Status3Cat

# create columns for protected vs. unprotected (based on different % catchment protected thresholds)
# for 75, 90 and 100 % catchment protection (individually for GAP Status 1-2 and 1-3)
PADUS_LakeCat$ProtectGAP12Cat_75 <- ifelse(PADUS_LakeCat$PctGAP_Status12Cat >= 75, "Protected75", "Unprotected75")
PADUS_LakeCat$ProtectGAP123Cat_75 <- ifelse(PADUS_LakeCat$PctGAP_Status123Cat >= 75, "Protected75", "Unprotected75")

PADUS_LakeCat_conn <- merge(PADUS_LakeCat, NHD_pts_LAGOS1ha_conn, by='COMID', all.x=F)

PADUS_LakeCat_conn <- PADUS_LakeCat_conn[!is.na(PADUS_LakeCat_conn$ProtectGAP12Cat_75),] #get rid of lakes with NA in protection
PADUS_LakeCat_conn <- PADUS_LakeCat_conn[!is.na(PADUS_LakeCat_conn$ProtectGAP123Cat_75),]

protected_GAP12_df <- subset(PADUS_LakeCat_conn, ProtectGAP12Cat_75 =='Protected75')
unprotected_GAP12_df <- subset(PADUS_LakeCat_conn, ProtectGAP12Cat_75 =='Unprotected75')

protected_GAP123_df <- subset(PADUS_LakeCat_conn, ProtectGAP123Cat_75 =='Protected75')
unprotected_GAP123_df <- subset(PADUS_LakeCat_conn, ProtectGAP123Cat_75 =='Unprotected75')

protected_GAP12_counts <- as.data.frame(protected_GAP12_df %>%
  group_by(LakeConnec,ProtectGAP12Cat_75) %>%
  tally())

protected_GAP123_counts <- as.data.frame(protected_GAP123_df %>%
                                          group_by(LakeConnec,ProtectGAP123Cat_75) %>%
                                          tally())

unprotected_GAP12_counts <- as.data.frame(unprotected_GAP12_df %>%
                                           group_by(LakeConnec,ProtectGAP12Cat_75) %>%
                                           tally())

unprotected_GAP123_counts <- as.data.frame(unprotected_GAP123_df %>%
                                            group_by(LakeConnec,ProtectGAP123Cat_75) %>%
                                            tally())


summary_df <- data.frame(LakeConnec=LakeConn_countz$LakeConnec, nLakes=LakeConn_countz$n)
summary_df$nProtected_strict <- protected_GAP12_counts$n
summary_df$nProtected_multi <- protected_GAP123_counts$n
summary_df$nUnprotected_strict <- unprotected_GAP12_counts$n
summary_df$nUnprotected_multi <- unprotected_GAP123_counts$n
summary_df$PropProtected_strict <- summary_df$nProtected_strict/summary_df$nLakes
summary_df$PropProtected_multi <- summary_df$nProtected_multi/summary_df$nLakes

barplot(summary_df$PropProtected_strict, names.arg=summary_df$LakeConnec)
barplot(summary_df$PropProtected_multi, names.arg=summary_df$LakeConnec)

barplot_df <- melt(summary_df[,c(1,7,8)], id.vars='LakeConnec')


ggplot(barplot_df, aes(x = LakeConnec, y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Proportion of lakes protected by LAGOS connectivity class") +
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  scale_y_continuous(limits=c(0,0.3), breaks=seq(0,0.3,0.05)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  #scale_fill_manual("legend", values = c("PP75" = "gray82", "PP90" = "gray50", "PP100" = "black"),
  #                  labels=c('75%','90%','100%'))+
  scale_fill_manual("legend", values = c("PropProtected_strict" = "olivedrab3", "PropProtected_multi" = "navajowhite2"),
                    labels=c('Strict','Multi-use'))+
  geom_hline(yintercept=0.17, linetype='dashed', color='black')+
  theme(legend.position=c(0.87,0.9))+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title
