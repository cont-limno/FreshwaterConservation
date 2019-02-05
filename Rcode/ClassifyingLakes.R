####################### Classifying US lakes based on LAGOS and LakeCat ########################
# Date: 1-7-19
# updated: 2-1-19
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(dplyr)
library(raster)
library(cluster)
library(fpc)
library(ggplot2)
library(clustMixType)
#library(NbClust)
library(reshape2)
library(gridExtra)
library(rgdal)

#### input data ####
setwd("C:/Users/FWL/Documents/FreshwaterConservation")

# NHD waterbodies (converted to points in ArcGIS) (NHDPlusV2 National dataset; downloaded November 2018)
# used Extract Values to Points in ArcGIS to get USGS landform as attribute
# Cress, J.J., Sayre, Roger, Comer, Patrick, and Warner, Harumi, 2009, Terrestrial Ecosystems-Land Surface Forms of the Conterminous United States: U.S. Geological Survey Scientific Investigations Map 3085, scale 1:5,000,000, 1 sheet.
NHD_pts <- shapefile("C:/Ian_GIS/FreshwaterConservation/USGS_Landforms/NHD_waterbody_pts_landform/NHD_waterbody_pts_landform.shp")

# lower 48 states
lower48 <- shapefile("C:/Ian_GIS/cb_2016_us_state_500k/lower48.shp") #same crs as NHD_pts

# LAGOS-US lakes >= 1ha with connectivity classes
LAGOS_1ha <- shapefile("C:/Ian_GIS/LAGOS_US_GIS/LAGOS_US_All_lakes_1ha_Conn.shp")

# Protected land by GAP status by local catchments and network watersheds (LakeCat)
# From US Protected Areas Database (PADUS) v 1.4
# https://gapanalysis.usgs.gov/padus/data/download/
PADUS_LakeCat <- read.csv("Data/PADUS.csv")

# LakeCat data downloaded November 2018 (some files too large for github repo, so stored all locally)
# ftp://newftp.epa.gov/EPADataCommons/ORD/NHDPlusLandscapeAttributes/LakeCat/FinalTables/
elevation <- read.csv("C:/Ian_GIS/LakeCat/Elevation.csv")
NLCD_2011 <- read.csv("C:/Ian_GIS/LakeCat/NLCD2011.csv")
#RoadDensity <- read.csv("C:/Ian_GIS/LakeCat/RoadDensity.csv")
PRISM <- read.csv("C:/Ian_GIS/LakeCat/PRISM_1981_2010.csv")
WetIndex <- read.csv("C:/Ian_GIS/LakeCat/WetIndx.csv")
#Mines <- read.csv("C:/Ian_GIS/LakeCat/Mines.csv")
#Dams <- read.csv("C:/Ian_GIS/LakeCat/NABD.csv")
#Toxic <- read.csv("C:/Ian_GIS/LakeCat/EPA_FRS.csv")
#ForestLoss <- read.csv("C:/Ian_GIS/LakeCat/ForestLossByYear0013.csv")
#Fahr <- read.csv("C:/Ian_GIS/LakeCat/FirePerimeters.csv")
#Deposition <- read.csv("C:/Ian_GIS/LakeCat/NADP.csv")
#Impervious <- read.csv("C:/Ian_GIS/LakeCat/ImperviousSurfaces2011.csv")
Runoff <- read.csv("C:/Ian_GIS/LakeCat/Runoff.csv")
Baseflow <- read.csv("C:/Ian_GIS/LakeCat/BFI.csv")

# X-walk table for linking NHD to LAGOS lakes
xwalk <- read.csv("C:/Ian_GIS/FreshwaterConservation/LAGOS_Lake_Link_v1.csv")

#### D-fine constants ####
min_protected <- 0 # keep data points (lake catchments) with percent protected above this value
lake_sqkm_cutoff <- 0.01 #=1ha

#### D-fine functions ####
# cluster boxplots function without legend (amended clustMixType::clprofiles)
clprofiles_no_legend <- function (object, x, vars = NULL, col = NULL) {
  if (length(object$cluster) != nrow(x)) 
    stop("Size of x does not match cluster result!")
  if (is.null(vars)) 
    vars <- 1:ncol(x)
  if (!is.numeric(vars)) 
    vars <- sapply(vars, function(z) return(which(colnames(x) == 
                                                    z)))
  if (length(vars) < 1) 
    stop("Specified variable names do not match x!")
  if (is.null(col)) {
    k <- max(unique(object$cluster))
    if (k > 2) 
      col <- brewer.pal(k, "Set3")
    if (k == 2) 
      col <- c("lightblue", "orange")
    if (k == 1) 
      col <- "lightblue"
  }
  clusids <- sort(unique(object$cluster))
  if (length(col) != max(clusids)) 
    warning("Length of col should match number of clusters!")
  par(ask = TRUE)
  for (i in vars) {
    if (is.numeric(x[, i])) {
      boxplot(x[, i] ~ object$cluster, col = col, main = colnames(x)[i])
      #legend("topright", legend = clusids, fill = col)
    }
    if (is.factor(x[, i])) {
      tab <- table(x[, i], object$cluster)
      for (j in 1:length(object$size)) tab[, j] <- tab[, 
                                                       j]/object$size[j]
      barplot(t(tab), beside = TRUE, main = colnames(x)[i], 
              col = col)
    }
  }
  par(ask = "FALSE")
  invisible()
}

############## Main program #############

# # Number of lakes with maxdepth
# NHD_1ha <- subset(NHD_pts, AREASQKM >= lake_sqkm_cutoff)
# NHD_1ha_wDepth <- subset(NHD_1ha, MaxDepth >0)
# 
# # compare depths to LAGOSNE depths
# library(LAGOSNE)
# dt <- lagosne_load(version = '1.087.1') #returns list of data.frame objects
# 
# LAGOSNE_wDepth <- subset(dt$lakes_limno, maxdepth>0)
# LAGOSNE_wDepth <- LAGOSNE_wDepth[,c('lagoslakeid','maxdepth')]
# colnames(LAGOSNE_wDepth) <- c('lagoslakeid','maxdepth_LAGOS')
# 
# reach1ha_df <- data.frame(lagoslakeid=LAGOS_1ha@data$lagoslakei, ReachCode=LAGOS_1ha@data$ReachCode)
# 
# test <- merge(LAGOSNE_wDepth, reach1ha_df, by='lagoslakeid', all.x=F)
# test <- merge(test, NHD_1ha_wDepth, by.x='ReachCode', by.y='REACHCODE', all.x=F)
# test <- merge(test, LAGOSNE_wDepth, by='lagoslakeid', all.x=F)
# 
# plot(test$MaxDepth ~ test$maxdepth_LAGOS, pch=20, xlab='MaxDepth LAGOS', ylab='MaxDepth NHD', 
#      xlim=c(0,80), ylim=c(0,80))
# abline(0,1)
# corplot <- cor(test$MaxDepth, test$maxdepth_LAGOS, use='pairwise.complete.obs', method='pearson')
# legend('bottomright', paste0("r = ", round(corplot, 3)), bty='n')
# mtext(side=3, paste0("n lakes = ", nrow(test)))

# Lake area
NHD_pts@data$COMID <- as.numeric(NHD_pts@data$COMID) #COMID was character in NHD; must convert
NHD_pts@data$SDF <- NHD_pts@data$Shape_Leng/(2*sqrt(pi*NHD_pts@data$Shape_Area))
PADUS_NHD <- left_join(PADUS_LakeCat, NHD_pts@data, by='COMID')
PADUS_NHD <- PADUS_NHD[!duplicated(PADUS_NHD$COMID),] #remove duplicate COMID (6 for some reason)
PADUS_NHD <- subset(PADUS_NHD, AREASQKM >= lake_sqkm_cutoff)
COMID_1ha_above <- PADUS_NHD$COMID #get string of COMIDs >= 1ha for further subsetting below
PADUS_NHD <- subset(PADUS_NHD, COMID %in% COMID_1ha_above) #overall table

# Elevation
PADUS_elevation <- full_join(PADUS_LakeCat, elevation, by='COMID')
PADUS_elevation <- subset(PADUS_elevation, COMID %in% COMID_1ha_above)

# Topographic wetness index
PADUS_WetIndex <- full_join(PADUS_LakeCat, WetIndex, by='COMID')
PADUS_WetIndex <- subset(PADUS_WetIndex, COMID %in% COMID_1ha_above)

# Land use/cover: calculate some new "total" variables
NLCD_2011$PctTotalForest2011Cat <- NLCD_2011$PctConif2011Cat + NLCD_2011$PctDecid2011Cat + NLCD_2011$PctMxFst2011Cat
NLCD_2011$PctTotalAg2011Cat <- NLCD_2011$PctCrop2011Cat + NLCD_2011$PctHay2011Cat
NLCD_2011$PctTotalWetland2011Cat <- NLCD_2011$PctWdWet2011Cat + NLCD_2011$PctHbWet2011Cat
NLCD_2011$PctTotalForest2011Ws <- NLCD_2011$PctConif2011Ws + NLCD_2011$PctDecid2011Ws + NLCD_2011$PctMxFst2011Ws
NLCD_2011$PctTotalAg2011Ws <- NLCD_2011$PctCrop2011Ws + NLCD_2011$PctHay2011Ws
NLCD_2011$PctTotalWetland2011Ws <- NLCD_2011$PctWdWet2011Ws + NLCD_2011$PctHbWet2011Ws
PADUS_NLCD2011 <- full_join(PADUS_LakeCat, NLCD_2011, by='COMID')
PADUS_NLCD2011 <- subset(PADUS_NLCD2011, COMID %in% COMID_1ha_above)

# Runoff
PADUS_runoff <- full_join(PADUS_LakeCat, Runoff, by='COMID')
PADUS_runoff <- subset(PADUS_runoff, COMID %in% COMID_1ha_above)

# Baseflow
PADUS_baseflow <- full_join(PADUS_LakeCat, Baseflow, by='COMID')
PADUS_baseflow <- subset(PADUS_baseflow, COMID %in% COMID_1ha_above)

# Climate
PADUS_PRISM <- full_join(PADUS_LakeCat, PRISM, by='COMID') 
PADUS_PRISM <- subset(PADUS_PRISM, COMID %in% COMID_1ha_above)

# LAGOS connectivity class
LAGOS_1ha@data$COMID <- as.numeric(LAGOS_1ha@data$ComID)
PADUS_LAGOS <- full_join(PADUS_LakeCat, LAGOS_1ha@data, by='COMID')
PADUS_LAGOS <- subset(PADUS_LAGOS, COMID %in% COMID_1ha_above)

### Create data frame of desired variables for cluster analysis or classification trees ##
a <- PADUS_NHD[,c('COMID','CatAreaSqKm','AREASQKM','RASTERVALU','SDF')]
b <- PADUS_elevation[,c('COMID','ElevCat')]
c <- PADUS_WetIndex[,c('COMID','WetIndexCat')]
d <- PADUS_NLCD2011[,c('COMID','PctConif2011Cat','PctTotalWetland2011Cat')]
e <- PADUS_runoff[,c('COMID','RunoffCat')]
f <- PADUS_baseflow[,c('COMID','BFICat')]
g <- PADUS_PRISM[,c('COMID','Precip8110Cat','Tmean8110Cat')]
#h <- PADUS_LAGOS[,c('COMID','LakeConnec')]
big_mama_table <- Reduce(function(...) merge(..., all=T), list(a,b,c,d,e,f,g))

# Create categorical var for Landform class
big_mama_table$LandForm <- NA
big_mama_table$LandForm[big_mama_table$RASTERVALU==1] <- "FlatPlains"
big_mama_table$LandForm[big_mama_table$RASTERVALU==2] <- "SmoothPlains"
big_mama_table$LandForm[big_mama_table$RASTERVALU==3] <- "IrregularPlains"
big_mama_table$LandForm[big_mama_table$RASTERVALU==4] <- "Escarpments"
big_mama_table$LandForm[big_mama_table$RASTERVALU==5] <- "LowHills"
big_mama_table$LandForm[big_mama_table$RASTERVALU==6] <- "Hills"
big_mama_table$LandForm[big_mama_table$RASTERVALU==7] <- "BreaksFoothills"
big_mama_table$LandForm[big_mama_table$RASTERVALU==8] <- "LowMtns"
big_mama_table$LandForm[big_mama_table$RASTERVALU==9] <- "HighMtns"
big_mama_table$LandForm[big_mama_table$RASTERVALU==10] <- "DrainageChannels"
big_mama_table$LandForm <- as.factor(big_mama_table$LandForm)

big_mama_table <- big_mama_table[complete.cases(big_mama_table),] #trivial number of rows with NAs; remove

### Different method/package: k-prototypes clustering
# with help from: https://www.kaggle.com/rahultej/k-prototypes-correlation-randomforest 

# subset overall dataset to test cluster method
#set.seed(87)
#baby <- sample_n(big_mama_table, size=100000, replace=F) #create random sample subset for testing tree
var_list <- c('COMID','CatAreaSqKm','AREASQKM','ElevCat','WetIndexCat','PctConif2011Cat','PctTotalWetland2011Cat',
              'RunoffCat','BFICat','Precip8110Cat','Tmean8110Cat','SDF','LandForm')
#baby <- baby[,c(var_list)]
full_cluster_data <- big_mama_table[,c(var_list)]

# add in lake connectivity classes from LAGOS



#cor(baby[,2:(ncol(baby)-1)], method='pearson')
cor(full_cluster_data[,2:(ncol(full_cluster_data)-1)], method='pearson')
#gower_dist <- daisy(baby[,2:ncol(baby)], metric='gower')

# need to log-transform any variables?

par(mfrow=c(2,3))
for (i in 2:(length(var_list)-2)){
   hist(full_cluster_data[,i], main=names(full_cluster_data[i]), las=1)
 }

par(mfrow=c(2,3))
for (i in 2:(length(var_list)-2)){
  hist(log(full_cluster_data[,i]), main=names(full_cluster_data[i]), las=1)
}

par(mfrow=c(1,2))
for (i in 2:(length(var_list)-2)){
  hist(full_cluster_data[,i], main=names(full_cluster_data[i]), las=1)
  hist(log(full_cluster_data[,i]), main=names(full_cluster_data[i]), las=1)
}

# could try logging: total wetland, coniferous, elevation, cat area, lake area, runoff
log_cluster_data <- full_cluster_data
log_cluster_data$CatAreaSqKm_log <- log(log_cluster_data$CatAreaSqKm)
log_cluster_data$AREASQKM_log <- log(log_cluster_data$AREASQKM)
#log_cluster_data$PctTotalWetland2011Cat_log <- log(log_cluster_data$PctTotalWetland2011Cat)
#log_cluster_data$PctConif2011Cat_log <- log(log_cluster_data$PctConif2011Cat)
#log_cluster_data$ElevCat_log <- log(log_cluster_data$ElevCat)
#log_cluster_data$RunoffCat_log <- log(log_cluster_data$RunoffCat)
log_cluster_data$SDF_log <- log(log_cluster_data$SDF)
log_cluster_data <- log_cluster_data[complete.cases(log_cluster_data),]
#log_cluster_data[,2:(length(var_list)-2)] <- log_cluster_data[,2:(length(var_list)-2)][!is.infinite(rowSums(log_cluster_data[,2:(length(var_list)-2)])),]

var_list <- c('COMID','CatAreaSqKm_log','AREASQKM_log','ElevCat','WetIndexCat','PctConif2011Cat','PctTotalWetland2011Cat',
              'RunoffCat','BFICat','Precip8110Cat','Tmean8110Cat','SDF_log','LandForm')
var_list <- c('COMID','ElevCat','WetIndexCat','PctConif2011Cat','PctTotalWetland2011Cat',
              'RunoffCat','BFICat','Precip8110Cat','Tmean8110Cat')
log_cluster_data <- log_cluster_data[,c(var_list)]

# check optimal number of clusters
#baby <- sample_n(log_cluster_data, size=50000, replace=F) #if need a subset
wss<-vector()
#for (i in 2:15){ wss[i] <- sum(kproto(baby[,2:ncol(baby)], i)$withinss)}
#for (i in 2:15){ wss[i] <- sum(kproto(full_cluster_data[,2:ncol(full_cluster_data)], i)$withinss)}
#for (i in 2:15){ wss[i] <- sum(kproto(log_cluster_data[,2:ncol(log_cluster_data)], i)$withinss)}
for (i in 2:15){ wss[i] <- sum(kmeans(baby[,2:ncol(baby)], i)$withinss)}
par(mfrow=c(1,1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Elbow Method",
     pch=20)

# too much memory for full dataset
#library(factoextra)
#fviz_nbclust(log_cluster_data[,2:ncol(log_cluster_data)], kproto, method = "silhouette")

#library(NbClust)
# but dissimilarity matrix is too large with large datasets (memory overload)
#NbClust(data=baby[,2:ncol(baby)], diss=gower_dist, distance=NULL, min.nc=2, max.nc=15, method='ward.D', index='ratkowsky')

# set number of clusters
k=10
#k=7
#kpres <- kproto(baby[,2:ncol(baby)], k=k)
#kpres <- kproto(full_cluster_data[,2:ncol(full_cluster_data)], k=k)
#kpres <- kproto(log_cluster_data[,2:ncol(log_cluster_data)], k=k)
kpres <- kmeans(baby[,2:ncol(baby)], centers=k)
hist(kpres$cluster)
cluster_means <- as.data.frame(kpres$centers)
cluster_means$cluster <- seq(1,k,1)
cluster_means <- cluster_means[,c(ncol(cluster_means),1:(ncol(cluster_means)-1))]

# Create loop to store cluster analysis outputs (cluster values and means) for different numbers of clusters
kpres_list <- list()
cluster_means_list <- list()
for (i in 3:12){
  #kprez <- kproto(baby[,2:ncol(baby)], k=i)# testing loop on small sample of data
  kprez <- kproto(log_cluster_data[,2:ncol(log_cluster_data)], k=i)
  cluster_meanz <- kprez$centers
  cluster_meanz$cluster <- seq(1,i,1)
  cluster_meanz <- cluster_meanz[,c(ncol(cluster_meanz),1:(ncol(cluster_meanz)-1))]
  kpres_list[[i]] <- kprez$cluster
  cluster_means_list[[i]] <- cluster_meanz
  kprez <- NULL
  cluster_means <- NULL
}


# barplots of cluster means by explanatory variable
# for (i in 1:(length(var_list)-2)){
#   barplot(cluster_means[,i], main=names(cluster_means[i]), names.arg=seq(1,k,1), xlab='Cluster', las=1)
# }

cluster_summary <- summary(kpres)
cluster_members <- baby
#cluster_members <- full_cluster_data
cluster_members$cluster <- kpres$cluster
cluster_members$clusterFac <- as.factor(cluster_members$cluster)

# for (i in 1:(length(var_list)-2)){
#   boxplot(cluster_members[,i] ~ cluster_members$clusterFac, las=1, xlab='cluster', main=names(cluster_members[i]))
# }

# map clusters
# mapping by cluster number
kdf <- data.frame(cluster=kpres$cluster,COMID=baby$COMID)
#kdf <- data.frame(cluster=kpres$cluster,COMID=full_cluster_data$COMID)
#kdf <- data.frame(cluster=kpres$cluster,COMID=log_cluster_data$COMID)

# append data to shapefile first
k_shp <- merge(NHD_pts, kdf, by='COMID', all.x=F)
k_shp@data$clusterFac <- as.factor(k_shp@data$cluster)
k_map_df <- as.data.frame(k_shp@data)
k_map_df$xCor <- k_shp@coords[,1]
k_map_df$yCor <- k_shp@coords[,2]

# this ggplot is imperfect, but it gets the job done for exploration
#map_colors <- c('firebrick','dodgerblue','orange','darkgreen','gray60','gold','black','hotpink','springgreen2','slateblue2')
map_colors <- c('lightblue','steelblue','palegreen','forestgreen','salmon','red2','tan1','darkorange1','thistle2','purple4')
k.point3<-ggplot(k_map_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=k_map_df$clusterFac), size=2) +
  ggtitle('K-prototypes clustering')
k.point3$labels$colour = 'Cluster' # change legend title
k.point3 + geom_path(data=lower48,aes(long,lat,group=group),colour='black') + coord_equal()+
  #scale_colour_brewer(palette = 'Set1') +
  scale_colour_manual(values = map_colors) +
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())

dsnname <- "C:/Ian_GIS/LakeCat/Clusters"
layername <- "kproto_LakeCat_clusters_k10"
#writeOGR(k_shp, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

# oh wow, there are already built-in functions for the boxplots...
par(mfrow=c(2,4))
clprofiles_no_legend(kpres, baby[,2:ncol(baby)], col=map_colors)
#clprofiles_no_legend(kpres, full_cluster_data[,2:ncol(full_cluster_data)], col=map_colors)
#clprofiles_no_legend(kpres, log_cluster_data[,2:ncol(log_cluster_data)], col=map_colors)

## How do clusters align with protected lakes?
# Identify protected vs. unprotected lakes based on % thresholds
# Calculate total catchment and watershed protection for GAPS 1-3
PADUS_LakeCat$PctGAP_Status12Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat
PADUS_LakeCat$PctGAP_Status123Cat <- PADUS_LakeCat$PctGAP_Status1Cat + PADUS_LakeCat$PctGAP_Status2Cat + PADUS_LakeCat$PctGAP_Status3Cat

# create columns for protected vs. unprotected (based on different % catchment protected thresholds)
# for 75, 90 and 100 % catchment protection (individually for GAP Status 1-2 and 1-3)
PADUS_LakeCat$ProtectGAP12Cat_75 <- ifelse(PADUS_LakeCat$PctGAP_Status12Cat >= 75, "Protected75", "Unprotected75")
PADUS_LakeCat$ProtectGAP123Cat_75 <- ifelse(PADUS_LakeCat$PctGAP_Status123Cat >= 75, "Protected75", "Unprotected75")
PADUS_LakeCat$ProtectGAP12Cat_90 <- ifelse(PADUS_LakeCat$PctGAP_Status12Cat >= 90, "Protected90", "Unprotected90")
PADUS_LakeCat$ProtectGAP123Cat_90 <- ifelse(PADUS_LakeCat$PctGAP_Status123Cat >= 90, "Protected90", "Unprotected90")
PADUS_LakeCat$ProtectGAP12Cat_100 <- ifelse(PADUS_LakeCat$PctGAP_Status12Cat >= 100, "Protected100", "Unprotected100")
PADUS_LakeCat$ProtectGAP123Cat_100 <- ifelse(PADUS_LakeCat$PctGAP_Status123Cat >= 100, "Protected100", "Unprotected100")

# join in cluster number
PADUS_LakeCat_cluster <- merge(PADUS_LakeCat, kdf, by='COMID', all.x=F)
PADUS_LakeCat_cluster$clusterFac <- as.factor(PADUS_LakeCat_cluster$cluster)

#### Barplots of lake protection by cluster
# 75% protection threshold
GAP12_Cat75 <- PADUS_LakeCat_cluster %>% 
  group_by(ProtectGAP12Cat_75, clusterFac) %>%
  summarise(number = n())
GAP12_Cat75 <- GAP12_Cat75[complete.cases(GAP12_Cat75),] #trivial number of rows with NAs; remove

GAP123_Cat75 <- PADUS_LakeCat_cluster %>% 
  group_by(ProtectGAP123Cat_75, clusterFac) %>%
  summarise(number = n())
GAP123_Cat75 <- GAP123_Cat75[complete.cases(GAP123_Cat75),] #trivial number of rows with NAs; remove

p1 <- ggplot(GAP12_Cat75, aes(x = clusterFac, y = number, fill = ProtectGAP12Cat_75)) + 
  geom_bar(stat = "identity") +
  xlab("Cluster") +
  ylab("Lakes") +
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  ggtitle('GAPS 1-2, 75% Cat')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_fill_manual("legend", values = c("Protected75" = "forestgreen", "Unprotected75" = "lightgoldenrod"),
                    labels=c('Protected','Unprotected'))+
  #theme(legend.position=c(0.9,0.85))+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

p2 <- ggplot(GAP123_Cat75, aes(x = clusterFac, y = number, fill = ProtectGAP123Cat_75)) + 
  geom_bar(stat = "identity") +
  xlab("Cluster") +
  ylab("Lakes") +
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  ggtitle('GAPS 1-3, 75% Cat')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_fill_manual("legend", values = c("Protected75" = "forestgreen", "Unprotected75" = "lightgoldenrod"),
                    labels=c('Protected','Unprotected'))+
  #theme(legend.position=c(0.9,0.85))+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

grid.arrange(p1, p2, nrow=1)

## 100% protection threshold
GAP12_Cat100 <- PADUS_LakeCat_cluster %>% 
  group_by(ProtectGAP12Cat_100, clusterFac) %>%
  summarise(number = n())
GAP12_Cat100 <- GAP12_Cat100[complete.cases(GAP12_Cat100),] #trivial number of rows with NAs; remove

GAP123_Cat100 <- PADUS_LakeCat_cluster %>% 
  group_by(ProtectGAP123Cat_100, clusterFac) %>%
  summarise(number = n())
GAP123_Cat100 <- GAP123_Cat100[complete.cases(GAP123_Cat100),] #trivial number of rows with NAs; remove

p3 <- ggplot(GAP12_Cat100, aes(x = clusterFac, y = number, fill = ProtectGAP12Cat_100)) + 
  geom_bar(stat = "identity") +
  xlab("Cluster") +
  ylab("Lakes") +
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  ggtitle('GAPS 1-2, 100% Cat')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_fill_manual("legend", values = c("Protected100" = "forestgreen", "Unprotected100" = "lightgoldenrod"),
                    labels=c('Protected','Unprotected'))+
  #theme(legend.position=c(0.9,0.85))+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

p4 <- ggplot(GAP123_Cat100, aes(x = clusterFac, y = number, fill = ProtectGAP123Cat_100)) + 
  geom_bar(stat = "identity") +
  xlab("Cluster") +
  ylab("Lakes") +
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  ggtitle('GAPS 1-3, 100% Cat')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_fill_manual("legend", values = c("Protected100" = "forestgreen", "Unprotected100" = "lightgoldenrod"),
                    labels=c('Protected','Unprotected'))+
  #theme(legend.position=c(0.9,0.85))+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

grid.arrange(p3, p4, nrow=1)
grid.arrange(p1,p2,p3,p4, nrow=2)

####### scratch #####
### Clustering
# with help from: https://medium.com/@anastasia.reusova/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995
# https://www.r-bloggers.com/clustering-mixed-data-types-in-r/ 
# set.seed(87)
# baby <- sample_n(big_mama_table, size=1000, replace=F) #create random sample subset for testing tree
# var_list <- c('COMID','CatAreaSqKm','AREASQKM','ElevCat','WetIndexCat','PctConif2011Cat','PctTotalWetland2011Cat',
#               'RunoffCat','BFICat','Precip8110Cat','Tmean8110Cat','SDF','LandForm')
# baby <- baby[,c(var_list)]
# 
# gower_dist <- daisy(baby[,2:ncol(baby)], metric='gower')
# 
# # divisive clustering (break down full dataset into smaller groups;probably better with very large dataset)
# div_clust <- diana(as.matrix(gower_dist), diss=T, keep.diss=F)
# plot(div_clust, main='Divisive')
# 
# # agglomerative clustering (start with each row as own cluster and aggregate)
# aggl_clust <- hclust(gower_dist, method="ward.D")
# plot(aggl_clust, main="Agglomerative, Ward's D")
# 
# ## Assess number of clusters
# cstats.table <- function(dist, tree, k) {
#   clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
#                     "wb.ratio","dunn2","avg.silwidth")
#   clust.size <- c("cluster.size")
#   stats.names <- c()
#   row.clust <- c()
#   
#   output.stats <- matrix(ncol = k, nrow = length(clust.assess))
#   cluster.sizes <- matrix(ncol = k, nrow = k)
#   
#   for(i in c(1:k)){
#     row.clust[i] <- paste("Cluster-", i, " size")
#   }
#   
#   for(i in c(2:k)){
#     stats.names[i] <- paste("Test", i-1)
#     
#     for(j in seq_along(clust.assess)){
#       output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
#       
#     }
#     
#     for(d in 1:k) {
#       cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
#       dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
#       cluster.sizes[d, i]
#       
#     }
#   }
#   
#   output.stats.df <- data.frame(output.stats)
#   
#   cluster.sizes <- data.frame(cluster.sizes)
#   cluster.sizes[is.na(cluster.sizes)] <- 0
#   
#   rows.all <- c(clust.assess, row.clust)
#   # rownames(output.stats.df) <- clust.assess
#   output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
#   colnames(output) <- stats.names[2:k]
#   rownames(output) <- rows.all
#   
#   is.num <- sapply(output, is.numeric)
#   output[is.num] <- lapply(output[is.num], round, 2)
#   
#   output
# }
# 
# stats.df.divisive <- cstats.table(gower_dist, div_clust, 20)
# stats.df.agglom <- cstats.table(gower_dist, aggl_clust, 20)
# 
# # elbow method
# ggplot(data = data.frame(t(cstats.table(gower_dist, div_clust, 20))), 
#        aes(x=cluster.number, y=within.cluster.ss)) + 
#   geom_point()+
#   geom_line()+
#   ggtitle("Divisive clustering") +
#   labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ggplot(data = data.frame(t(cstats.table(gower_dist, aggl_clust, 20))), 
#        aes(x=cluster.number, y=within.cluster.ss)) + 
#   geom_point()+
#   geom_line()+
#   ggtitle("Agglomerative clustering") +
#   labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# # silhouette method
# ggplot(data = data.frame(t(cstats.table(gower_dist, div_clust, 15))), 
#        aes(x=cluster.number, y=avg.silwidth)) + 
#   geom_point()+
#   geom_line()+
#   ggtitle("Divisive clustering") +
#   labs(x = "Num.of clusters", y = "Average silhouette width") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# 
# # look at results with specific number of clusters
# div_k10 <- cutree(div_clust, k=10)
# div_k7 <- cutree(div_clust, k=7)
# hist(div_k10)
# hist(div_k7)