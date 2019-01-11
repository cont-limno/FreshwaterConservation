############### Terrestrial and hydrologic lake connectivity indices ###########################
# Date: 1-3-19
# updated: 
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(vegan)
library(factoextra)
library(rgdal)

#### input data ####
setwd("C:/Users/FWL/Documents/FreshwaterConservation")
hydro_conn_df <- read.csv("Data/Michigan_LAGOS_conn_metrics.csv")
terr_conn_df <- read.csv("Data/LakeWetlandPatchStats_2020mBuff.csv")
patch_statz <- read.csv("Data/MichiganLakePatchStats_wBorderStates.csv")
cost_dist <- read.csv("Data/CostDist_Mich_4ha_lakes_2020mBuff.csv")

# Michigan shapefile
mich_shp <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/STATE/Michigan.shp")

# LAGOS NE lakes
lakes_4ha_poly <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/LAGOS_NE_All_Lakes_4ha/LAGOS_NE_All_Lakes_4ha.shp")
lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/LAGOS_NE_All_Lakes_4ha_POINTS/LAGOS_NE_All_Lakes_4ha_POINTS.shp")

# load color palette function
#source("Rcode/functions/divPalette.R") #from: https://quantdev.ssri.psu.edu/sites/qdev/files/Tutorial_ColorR_2.html 

# Protected area (PADUS) data, calculated for LAGOS IWS and lake buffers in ArcGIS (tabluate area)
PADUS_IWS <- read.csv("Data/PADUS_MI_IWS_pct.csv")
PADUS_buff <- read.csv("Data/PADUS_MI_Buff2020m_pct.csv")

##################### Main program ######################
# identify Mich lagoslakeids (focal lakes)
mich_lakes_4ha <- subset(lakes_4ha_poly, STATE=='MI')
mich_lagoslakeids <- unique(mich_lakes_4ha@data$lagoslakei)

# drop useless index columns from conn dfs
terr_conn_df <- terr_conn_df[,-1]
hydro_conn_df <- hydro_conn_df[,-1]

# get shape index from patch_statz, join to terrestrial df
patch_statz <- data.frame(lagoslakeid=patch_statz$patchID, shape_index=patch_statz$shape.index)
terr_conn_df <- merge(terr_conn_df, patch_statz, by='lagoslakeid', all.x=F)
terr_conn_df <- merge(terr_conn_df, cost_dist, by.x='lagoslakeid', by.y='lagoslakei', all.x=F)

# keep only some terrestrial conn vars
terr_conn_df <- terr_conn_df[,c('lagoslakeid','nLakePatches','LakeEdgeArea_pct','nWetlandPatches','WetlandArea_pct','shape_index','MIN')]
colnames(terr_conn_df) <- c('lagoslakeid','nLakePatches','LakeEdgeArea_pct','nWetlandPatches','WetlandArea_pct','shape_index','min_cost_dist')

# rename hydro conn vars and drop some
colnames(hydro_conn_df) <- c('lagoslakeid','lake_area_pct','stream_density_mperha','wetland_pct','connwetland_pct','stream_density_500mbuff_mperha','shoreline_wetlands_pct','damdensity_ptsperha')
hydro_conn_df <- hydro_conn_df[,c('lagoslakeid','lake_area_pct','stream_density_mperha','wetland_pct','connwetland_pct','shoreline_wetlands_pct','damdensity_ptsperha')]

# convert hydro variable percentages to proportions (already are for terrestrial variables)
hydro_conn_df$wetland_pct <- hydro_conn_df$wetland_pct/100
hydro_conn_df$connwetland_pct <- hydro_conn_df$connwetland_pct/100
hydro_conn_df$lake_area_pct <- hydro_conn_df$lake_area_pct/100
hydro_conn_df$shoreline_wetlands_pct <- hydro_conn_df$shoreline_wetlands_pct/100

# likely due to raster processing/topology issues, some proportions slightly exceeded 1; adjust these to 1
terr_conn_df$WetlandArea_pct <- ifelse(terr_conn_df$WetlandArea_pct > 1, 1, terr_conn_df$WetlandArea_pct)

# a few NA values in hydro conn, but these can be treated as 0
hydro_conn_df[is.na(hydro_conn_df)] <- 0

## how correlated are hydro and terr conn vars?
cor(hydro_conn_df[,2:ncol(hydro_conn_df)], use='pairwise.complete.obs', method='pearson')

### PCA
## Hydro variables
summary(hydro_conn_df[,2:ncol(hydro_conn_df)])
par(mfrow=c(2,3))
for (i in 2:ncol(hydro_conn_df)){
  hist(hydro_conn_df[,i], main=colnames(hydro_conn_df[i]))
}

# using correlation matrix due to different variable scales; otherwise vars with large values would dominate
# even though many vars are percents, some are still very low (e.g., shoreline_wetlands_pct; mostly 0-1%)
# originally also used lake_area_pct, damdensity_ptsperha, but both vars so 0 heavy, contained little information 
# princomp internally centers and scale data; done logically in prcomp (checked this using both functions; same output)
pca_hydro <- princomp(~ stream_density_mperha + wetland_pct + connwetland_pct + shoreline_wetlands_pct, 
                      data=hydro_conn_df, cor=T, scores=T)
par(mfrow=c(1,1))
screeplot(pca_hydro, type='l')
summary(pca_hydro)
loadings(pca_hydro)
eigenvals(pca_hydro)

#help from: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
fviz_pca_var(pca_hydro,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# To get a composite of first 2 components, what if did pythagorean on scores for PCs 1 and 2?
pca_hydro_scores <- as.data.frame(scores(pca_hydro))
pca_hydro_scores$PC1and2 <- sqrt((pca_hydro_scores$Comp.1 ^2) + (pca_hydro_scores$Comp.2 ^2))
hist(pca_hydro_scores$PC1and2)
pca_hydro_scores$lagoslakeid <- hydro_conn_df$lagoslakeid

pca_hydro_scores_shp <- merge(lakes_4ha_pts, pca_hydro_scores, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
pca_hydro_scores_shp_df <- as.data.frame(pca_hydro_scores_shp@data)
pca_hydro_scores_shp_df$xCor <- pca_hydro_scores_shp@coords[,1]
pca_hydro_scores_shp_df$yCor <- pca_hydro_scores_shp@coords[,2]

# this ggplot is imperfect, but it gets the job done for exploration
pca_hydro_scores.point3<-ggplot(pca_hydro_scores_shp_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=pca_hydro_scores_shp_df$PC1and2), size=2) +
  ggtitle('Hydro conn index')
pca_hydro_scores.point3$labels$colour = 'Hydro' # change legend title
pca_hydro_scores.point3 + geom_path(data=mich_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  #scale_colour_brewer(palette = 'Set1') +
  scale_color_continuous(low='firebrick', high='dodgerblue')+
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())

## Terrestrial variables
cor(terr_conn_df[,2:ncol(terr_conn_df)], use='pairwise.complete.obs', method='pearson')
summary(terr_conn_df[,2:ncol(terr_conn_df)])
# correct least cost distance such that larger distances are worse (lower numbers)
terr_conn_df$min_cost_dist_corrected <- (1/terr_conn_df$min_cost_dist)
par(mfrow=c(2,3))
for (i in 2:ncol(terr_conn_df)){
  hist(terr_conn_df[,i], main=colnames(terr_conn_df[i]))
}

pca_terr <- princomp(~ nLakePatches + LakeEdgeArea_pct + nWetlandPatches + WetlandArea_pct + shape_index + 
                       min_cost_dist_corrected, data=terr_conn_df, cor=T, scores=T)
par(mfrow=c(1,1))
screeplot(pca_terr, type='l')
summary(pca_terr)
loadings(pca_terr)
eigenvals(pca_terr)

fviz_pca_var(pca_terr,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# To get a composite of first 2 components, what if did pythagorean on scores for PCs 1 and 2?
pca_terr_scores <- as.data.frame(scores(pca_terr))
pca_terr_scores$PC1and2 <- sqrt((pca_terr_scores$Comp.1 ^2) + (pca_terr_scores$Comp.2 ^2))
pca_terr_scores$PC1and2and3 <- sqrt((pca_terr_scores$Comp.1 ^2) + (pca_terr_scores$Comp.2 ^2) + (pca_terr_scores$Comp.3 ^2))
hist(pca_terr_scores$PC1and2)
hist(pca_terr_scores$PC1and2and3)
pca_terr_scores$lagoslakeid <- terr_conn_df$lagoslakeid

pca_terr_scores_shp <- merge(lakes_4ha_pts, pca_terr_scores, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
pca_terr_scores_shp_df <- as.data.frame(pca_terr_scores_shp@data)
pca_terr_scores_shp_df$xCor <- pca_terr_scores_shp@coords[,1]
pca_terr_scores_shp_df$yCor <- pca_terr_scores_shp@coords[,2]

# this ggplot is imperfect, but it gets the job done for exploration
pca_terr_scores.point3<-ggplot(pca_terr_scores_shp_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=pca_terr_scores_shp_df$PC1and2), size=2) +
  ggtitle('terr conn index')
pca_terr_scores.point3$labels$colour = 'terr' # change legend title
pca_terr_scores.point3 + geom_path(data=mich_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  #scale_colour_brewer(palette = 'Set1') +
  scale_color_continuous(low='firebrick', high='dodgerblue')+
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())


# this ggplot is imperfect, but it gets the job done for exploration
pca_terr_scores.point3<-ggplot(pca_terr_scores_shp_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=pca_terr_scores_shp_df$PC1and2and3), size=2) +
  ggtitle('terr conn index')
pca_terr_scores.point3$labels$colour = 'terr' # change legend title
pca_terr_scores.point3 + geom_path(data=mich_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  #scale_colour_brewer(palette = 'Set1') +
  scale_color_continuous(low='firebrick', high='dodgerblue')+
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())

#### Compare hydro conn and terr conn variables for lakes
hydro_terr_conn_df <- merge(pca_hydro_scores, pca_terr_scores, by='lagoslakeid', all.x=F)
hydro_terr_conn_df <- hydro_terr_conn_df[,c('lagoslakeid','PC1and2.x','PC1and2.y','PC1and2and3')]
colnames(hydro_terr_conn_df) <- c('lagoslakeid','PC1and2_hydro','PC1and2_terr','PC1and2and3_terr')

#library(RColorBrewer)
#colorz <- brewer.pal(9, name='GnBu')[round(hydro_terr_conn_df$hydro_terr)]
#colorz <- divPalette(10, "RdYlBu")[round(hydro_terr_conn_df$hydro_terr)]

dev.new(width=4, height=4)
plot(PC1and2_hydro ~ PC1and2and3_terr, data=hydro_terr_conn_df, pch=16, xlim=c(0,10), ylim=c(0,10))
abline(0,1)
corplot <- cor(hydro_terr_conn_df$PC1and2_hydro, hydro_terr_conn_df$PC1and2and3_terr, method='pearson', use='pairwise.complete.obs')
legend('bottomright', legend=paste0("r = ", round(corplot, 3)), bty='n')
#legend('topleft', legend=c("high","low"), pch=c(16,16), col=c(divPalette(10, "RdYlBu")[10], divPalette(10, "RdYlBu")[1]))

# Map: pythagorean theorem on hydro and terr PCs
hydro_terr_conn_df$hydro_terr <- sqrt((hydro_terr_conn_df$PC1and2_hydro ^2) + (hydro_terr_conn_df$PC1and2and3_terr ^2))
hist(hydro_terr_conn_df$hydro_terr)

hydro_terr_conn_shp <- merge(lakes_4ha_pts, hydro_terr_conn_df, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
hydro_terr_conn_shp_df <- as.data.frame(hydro_terr_conn_shp@data)
hydro_terr_conn_shp_df$xCor <- hydro_terr_conn_shp@coords[,1]
hydro_terr_conn_shp_df$yCor <- hydro_terr_conn_shp@coords[,2]

# this ggplot is imperfect, but it gets the job done for exploration
hydro_terr_scores.point3<-ggplot(hydro_terr_conn_shp_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=hydro_terr_conn_shp_df$hydro_terr), size=2) +
  ggtitle('hydro/terr conn index')
hydro_terr_scores.point3$labels$colour = 'terr' # change legend title
hydro_terr_scores.point3 + geom_path(data=mich_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  #scale_color_gradient2(low='firebrick',mid='yellow',high='dodgerblue') +
  scale_color_continuous(low='firebrick', high='dodgerblue')+
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())

# save to disk for better mapping in ArcGIS
dsnname <- "C:/Ian_GIS/FreshwaterConservation/ConnIndices"
layername <- "hydro_terr_conn_index"
#writeOGR(hydro_terr_conn_shp, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

###### Compare conn indices to % IWS and lake buffer protected
# Note, TabulateArea calculations in ArcGIS do not have lake area subtracted from buffers or IWS; may need to correct this later, but would be pain 
# some initial wrangling (standardize column names, get rid of unnecessary columns)
PADUS_IWS <- PADUS_IWS[,c('LAGOSLAKEI','GAP12_IWS_pct','GAP123_IWS_pct','IWS_area_ha','GAP12_IWS_ha','GAP123_IWS_ha')]
colnames(PADUS_IWS) <- c('lagoslakeid','GAP12_IWS_pct','GAP123_IWS_pct','IWS_area_ha','GAP12_IWS_ha','GAP123_IWS_ha')

PADUS_buff <- PADUS_buff[,c('LAGOSLAKEI','GAP12_buff_pct','GAP123_buff_pct','buff_area_ha','GAP12_buff_ha','GAP123_buff_ha')]
colnames(PADUS_buff) <- c('lagoslakeid','GAP12_buff_pct','GAP123_buff_pct','buff_area_ha','GAP12_buff_ha','GAP123_buff_ha')

# need dataframe of unprotected lakes with 0% protected (otherwise only include lakes with nonzero protection) 
PADUS_IWS_conn <- merge(PADUS_IWS, hydro_terr_conn_df, by='lagoslakeid')
PADUS_buff_conn <- merge(PADUS_buff, hydro_terr_conn_df, by='lagoslakeid')

# create empty data frames of unprotected lakes
MI_lakes_IWS_unprotected <- subset(mich_lakes_4ha, !(lagoslakei %in% PADUS_IWS_conn$lagoslakeid))@data$lagoslakei
MI_lakes_IWS_unprotected <- data.frame(lagoslakeid=MI_lakes_IWS_unprotected, GAP12_IWS_pct=0,GAP123_IWS_pct=0,IWS_area_ha=0,GAP12_IWS_ha=0,GAP123_IWS_ha=0,PC1and2_hydro=0,PC1and2_terr=0,PC1and2and3_terr=0,hydro_terr=0)
MI_lakes_IWS_unprotected$lagoslakeid <- as.numeric(levels(MI_lakes_IWS_unprotected$lagoslakeid))[MI_lakes_IWS_unprotected$lagoslakeid]

MI_lakes_buff_unprotected <- subset(mich_lakes_4ha, !(lagoslakei %in% PADUS_buff_conn$lagoslakeid))@data$lagoslakei
MI_lakes_buff_unprotected <- data.frame(lagoslakeid=MI_lakes_buff_unprotected, GAP12_buff_pct=0,GAP123_buff_pct=0,buff_area_ha=0,GAP12_buff_ha=0,GAP123_buff_ha=0,PC1and2_hydro=0,PC1and2_terr=0,PC1and2and3_terr=0,hydro_terr=0)
MI_lakes_buff_unprotected$lagoslakeid <- as.numeric(levels(MI_lakes_buff_unprotected$lagoslakeid))[MI_lakes_buff_unprotected$lagoslakeid]

# append to tables of protected lakes
PADUS_IWS_conn <- rbind.data.frame(PADUS_IWS_conn, MI_lakes_IWS_unprotected)
PADUS_buff_conn <- rbind.data.frame(PADUS_buff_conn, MI_lakes_buff_unprotected)

par(mfrow=c(2,2))
plot(GAP12_IWS_pct ~ PC1and2_hydro, data=PADUS_IWS_conn, pch=16, las=1, ylab='Proportion IWS protected (GAPS 1-2)',
     xlab='Hydro conn index', main='Hydro')
corplot <- round(cor(PADUS_IWS_conn$GAP12_IWS_pct, PADUS_IWS_conn$PC1and2_hydro, method='pearson', use='pairwise.complete.obs'),3)
legend('topright', legend=paste0("r = ", corplot), bty='n')

plot(GAP123_IWS_pct ~ PC1and2_hydro, data=PADUS_IWS_conn, pch=16, las=1, ylab='Proportion IWS protected (GAPS 1-3)',
     xlab='Hydro conn index', main='Hydro')
corplot <- round(cor(PADUS_IWS_conn$GAP123_IWS_pct, PADUS_IWS_conn$PC1and2_hydro, method='pearson', use='pairwise.complete.obs'),3)
legend('topright', legend=paste0("r = ", corplot), bty='n')


plot(GAP12_buff_pct ~ PC1and2and3_terr, data=PADUS_buff_conn, pch=16, las=1, ylab='Proportion buff protected (GAPS 1-2)',
     xlab='Terr conn index', main='Terrestrial')
corplot <- round(cor(PADUS_buff_conn$GAP12_buff_pct, PADUS_buff_conn$PC1and2and3_terr, method='pearson', use='pairwise.complete.obs'),3)
legend('topright', legend=paste0("r = ", corplot), bty='n')

plot(GAP123_buff_pct ~ PC1and2_hydro, data=PADUS_buff_conn, pch=16, las=1, ylab='Proportion buff protected (GAPS 1-3)',
     xlab='Terr conn index', main='Terrestrial')
corplot <- round(cor(PADUS_buff_conn$GAP123_buff_pct, PADUS_buff_conn$PC1and2and3_terr, method='pearson', use='pairwise.complete.obs'),3)
legend('topright', legend=paste0("r = ", corplot), bty='n')

################## Scratch ###########################
#### Some other approach? Non-metric multidimensional scaling
# library(ecodist)
# distmat <- vegdist(hydro_terr_conn_df[,2:ncol(hydro_terr_conn_df)], method='bray')
# nmds_test <- nmds(distmat, mindim=2, maxdim=2) #warning: slow
# stressplot(nmds_test, distmat)