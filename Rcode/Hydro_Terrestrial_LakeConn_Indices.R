############### Terrestrial and hydrologic lake connectivity indices ###########################
# Date: 1-3-19
# updated: 1-17-19
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(vegan)
library(ggplot2)
library(factoextra)
library(rgdal)
library(plot3D)
library(dplyr)

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

#get inverse of dam density (so that higher value represents more connectivity)
# forget it; too little variability in this variable to make it worth using
#hydro_conn_df$damdensity_ptsperha <- (1/hydro_conn_df$damdensity_ptsperha)

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
pca_hydro <- princomp(~ stream_density_mperha + connwetland_pct + shoreline_wetlands_pct, 
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
pca_hydro_scores$PChydroall <- sqrt((pca_hydro_scores$Comp.1 ^2) + (pca_hydro_scores$Comp.2 ^2) + (pca_hydro_scores$Comp.3 ^2))
hist(pca_hydro_scores$PChydroall)
pca_hydro_scores$lagoslakeid <- hydro_conn_df$lagoslakeid

pca_hydro_scores_shp <- merge(lakes_4ha_pts, pca_hydro_scores, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
pca_hydro_scores_shp_df <- as.data.frame(pca_hydro_scores_shp@data)
pca_hydro_scores_shp_df$xCor <- pca_hydro_scores_shp@coords[,1]
pca_hydro_scores_shp_df$yCor <- pca_hydro_scores_shp@coords[,2]

# this ggplot is imperfect, but it gets the job done for exploration
pca_hydro_scores.point3<-ggplot(pca_hydro_scores_shp_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=pca_hydro_scores_shp_df$PChydroall), size=2) +
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
#terr_conn_df$min_cost_dist_corrected <- (1/terr_conn_df$min_cost_dist)
#terr_conn_df$min_cost_dist_corrected <- (1-terr_conn_df$min_cost_dist)
terr_conn_df$min_cost_dist_corrected <- scales::rescale(terr_conn_df$min_cost_dist, to=c(100,1))

par(mfrow=c(2,3))
for (i in 2:ncol(terr_conn_df)){
  hist(terr_conn_df[,i], main=colnames(terr_conn_df[i]))
}

pca_terr <- princomp(~ nLakePatches + LakeEdgeArea_pct + nWetlandPatches + WetlandArea_pct + 
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
pca_terr_scores$PCterrall <- sqrt((pca_terr_scores$Comp.1 ^2) + (pca_terr_scores$Comp.2 ^2) + (pca_terr_scores$Comp.3 ^2) + (pca_terr_scores$Comp.4 ^2) + (pca_terr_scores$Comp.5 ^2))
hist(pca_terr_scores$PCterrall)
pca_terr_scores$lagoslakeid <- terr_conn_df$lagoslakeid

pca_terr_scores_shp <- merge(lakes_4ha_pts, pca_terr_scores, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
pca_terr_scores_shp_df <- as.data.frame(pca_terr_scores_shp@data)
pca_terr_scores_shp_df$xCor <- pca_terr_scores_shp@coords[,1]
pca_terr_scores_shp_df$yCor <- pca_terr_scores_shp@coords[,2]

# this ggplot is imperfect, but it gets the job done for exploration
pca_terr_scores.point3<-ggplot(pca_terr_scores_shp_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=pca_terr_scores_shp_df$PCterrall), size=2) +
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
hydro_terr_conn_df <- hydro_terr_conn_df[,c('lagoslakeid','PChydroall','PCterrall')]
hydro_terr_conn_df$hydro_terr <- sqrt((hydro_terr_conn_df$PCterrall ^2) + (hydro_terr_conn_df$PChydroall ^2)) 
hydro_terr_conn_df <- hydro_terr_conn_df[complete.cases(hydro_terr_conn_df), ]

#dev.new(width=4, height=4)
par(mfrow=c(1,1))
plot(PChydroall ~ PCterrall, data=hydro_terr_conn_df, pch=16, xlim=c(), ylim=c(), col='black',
     xlab='Terrestrial', ylab='Hydrologic')
abline(0,1)
corplot <- cor(hydro_terr_conn_df$PChydroall, hydro_terr_conn_df$PCterrall, method='pearson', use='pairwise.complete.obs')
legend('bottomright', legend=paste0("r = ", round(corplot, 3)), bty='n')

library(scatterD3)
tooltips = paste("lagoslakeid:", hydro_terr_conn_df$lagoslakei,"</strong><br />HYDRO:",round(hydro_terr_conn_df$PChydroall,3),
                 "</strong><br />TERR:",round(hydro_terr_conn_df$PCterrall,3), "</strong><br />COMBINED:",round(hydro_terr_conn_df$hydro_terr,3))
scatterD3(x = hydro_terr_conn_df$PCterrall, y = hydro_terr_conn_df$PChydroall, xlim=c(0,20), ylim=c(0,20),
          xlab='Terrestrial',ylab='Hydrologic',col_var=hydro_terr_conn_df$hydro_terr,
          col_lab='Conn score',tooltip_text = tooltips)

# ggplot with dots colored by combined hydro/terr conn score and outliers >= 10 removed
gg_sub <- subset(hydro_terr_conn_df, hydro_terr <= 10)
#jpeg('C:/Ian_GIS/FreshwaterConservation/Exports/Figs/colored_ggplot_conn_scores.jpeg',width = 4,height = 4,units = 'in',res=600)
combined_scores.point3<-ggplot(gg_sub, aes(x=PCterrall, y=PChydroall))+
  geom_point(aes(colour=gg_sub$hydro_terr), size=1) +
  #geom_abline(intercept=0, slope=1, color='black', size=1) + #1:1 fit line
  geom_hline(yintercept=5, color='black', linetype='dashed', size=1) +
  geom_vline(xintercept=5, color='black', linetype='dashed', size=1) +
  annotate("text", x=0, y=10, label='A)', size=4)+
  annotate("text", x=0, y=4.6, label='C)', size=4)+
  annotate("text", x=5.5, y=10, label='B)', size=4)+
  annotate("text", x=5.5, y=4.6, label='D)', size=4)+
  ggtitle('Combined hydrologic/terrestrial connectivity score')
combined_scores.point3$labels$colour = 'Combined score' # change legend title
combined_scores.point3 +
  scale_x_continuous(name="Terrestrial", limits=c(0, 10)) +
  scale_y_continuous(name="Hydrologic", limits=c(0, 10)) +
  scale_color_gradient(low='firebrick1', high='dodgerblue')+
  theme_classic() +
  theme(legend.position=c(0.9,0.75))+
  theme(legend.key.size=unit(0.15,"in"))+
  theme(legend.text=element_text(size=7))+
  theme(legend.title=element_text(color='black', size=8))+
  theme(plot.title=element_text(size=9, face='bold'))
#dev.off()


# Map: pythagorean theorem on hydro and terr PCs
hist(hydro_terr_conn_df$hydro_terr)

hydro_terr_conn_shp <- merge(lakes_4ha_pts, hydro_terr_conn_df, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
hydro_terr_conn_shp_df <- as.data.frame(hydro_terr_conn_shp@data)
hydro_terr_conn_shp_df$xCor <- hydro_terr_conn_shp@coords[,1]
hydro_terr_conn_shp_df$yCor <- hydro_terr_conn_shp@coords[,2]

# this ggplot is imperfect, but it gets the job done for exploration
hydro_terr_scores.point3<-ggplot(hydro_terr_conn_shp_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=hydro_terr_conn_shp_df$hydro_terr), size=2) +
  ggtitle('hydro/terr conn index')
hydro_terr_scores.point3$labels$colour = 'Score' # change legend title
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
MI_lakes_IWS_unprotected <- data.frame(lagoslakeid=MI_lakes_IWS_unprotected, GAP12_IWS_pct=0,GAP123_IWS_pct=0,IWS_area_ha=0,GAP12_IWS_ha=0,GAP123_IWS_ha=0,PChydroall=0,PCterrall=0,hydro_terr=0)
MI_lakes_IWS_unprotected$lagoslakeid <- as.numeric(levels(MI_lakes_IWS_unprotected$lagoslakeid))[MI_lakes_IWS_unprotected$lagoslakeid]

MI_lakes_buff_unprotected <- subset(mich_lakes_4ha, !(lagoslakei %in% PADUS_buff_conn$lagoslakeid))@data$lagoslakei
MI_lakes_buff_unprotected <- data.frame(lagoslakeid=MI_lakes_buff_unprotected, GAP12_buff_pct=0,GAP123_buff_pct=0,buff_area_ha=0,GAP12_buff_ha=0,GAP123_buff_ha=0,PChydroall=0,PCterrall=0,hydro_terr=0)
MI_lakes_buff_unprotected$lagoslakeid <- as.numeric(levels(MI_lakes_buff_unprotected$lagoslakeid))[MI_lakes_buff_unprotected$lagoslakeid]

# append to tables of protected lakes
PADUS_IWS_conn <- rbind.data.frame(PADUS_IWS_conn, MI_lakes_IWS_unprotected)
PADUS_buff_conn <- rbind.data.frame(PADUS_buff_conn, MI_lakes_buff_unprotected)


#jpeg('C:/Ian_GIS/FreshwaterConservation/Exports/Figs/panel_scatter_conn_scores.jpeg',width = 6,height = 6,units = 'in',res=600)
  par(mfrow=c(2,2))
  # PLOT A
  par(mar=c(4,4,2,0.5)) #bot,left,top,right
  plot(GAP12_IWS_pct ~ PChydroall, data=PADUS_IWS_conn, pch=20, las=1, ylab='Proportion watershed protected',
     xlab='Hydrologic conn index', main='A) GAPS 1-2', xlim=c(0,10))
  corplot <- round(cor(PADUS_IWS_conn$GAP12_IWS_pct, PADUS_IWS_conn$PChydroall, method='pearson', use='pairwise.complete.obs'),2)
  legend('topright', legend=paste0("r = ", corplot), bty='n', cex=0.8)
  # PLOT B
  par(mar=c(4,0.5,2,4)) #bot,left,top,right
  plot(GAP123_IWS_pct ~ PChydroall, data=PADUS_IWS_conn, pch=20, las=1, ylab='',
     xlab='Hydrologic conn index', main='B) GAPS 1-3', xlim=c(0,10), yaxt='n')
  corplot <- round(cor(PADUS_IWS_conn$GAP123_IWS_pct, PADUS_IWS_conn$PChydroall, method='pearson', use='pairwise.complete.obs'),2)
  legend('topright', legend=paste0("r = ", corplot), bty='n', cex=0.8)
  # PLOT C
  par(mar=c(4,4,2,0.5)) #bot,left,top,right
  plot(GAP12_buff_pct ~ PCterrall, data=PADUS_buff_conn, pch=20, las=1, ylab='Proportion buffer protected',
     xlab='Terrestrial conn index', main='C) GAPS 1-2', xlim=c(0,10))
  corplot <- round(cor(PADUS_buff_conn$GAP12_buff_pct, PADUS_buff_conn$PCterrall, method='pearson', use='pairwise.complete.obs'),2)
  legend('topright', legend=paste0("r = ", corplot), bty='n', cex=0.8)
  # PLOT D
  par(mar=c(4,0.5,2,4)) #bot,left,top,right
  plot(GAP123_buff_pct ~ PCterrall, data=PADUS_buff_conn, pch=20, las=1, ylab='',
     xlab='Terrestrial conn index', main='D) GAPS 1-3', xlim=c(0,10), yaxt='n')
  corplot <- round(cor(PADUS_buff_conn$GAP123_buff_pct, PADUS_buff_conn$PCterrall, method='pearson', use='pairwise.complete.obs'),2)
  legend('topright', legend=paste0("r = ", corplot), bty='n', cex=0.8)
#dev.off()

## panel histograms of conn scores
jpeg('C:/Ian_GIS/FreshwaterConservation/Exports/Figs/panel_hist_conn_scores.jpeg',width = 6,height = 3,units = 'in',res=600)
  par(mfrow=c(1,3))
  # First plot
  par(mar=c(2.5,3,1,0.5)) #bot,left,top,right
  hist(hydro_terr_conn_df$PChydroall, ylab='', xlab='', xlim=c(0,13), ylim=c(0,3500), breaks=seq(0,13,1),
       main='Hydologic', las=1)
  # second plot
  #par(mar=c(2.5,0.25,1,1)) #bot,left,top,right
  hist(hydro_terr_conn_df$PCterrall, ylab='', xlab='', xlim=c(0,13), ylim=c(0,3500), breaks=seq(0,13,1),
       main='Terrestrial', las=1)
  # third plot
  #par(mar=c(2.5,0.5,1,0.5)) #bot,left,top,right
  hist(hydro_terr_conn_df$hydro_terr,ylab='', xlab='', xlim=c(0,13), ylim=c(0,3500), breaks=seq(0,13,1),
       main='Combined', las=1)
dev.off()

# single histogram
#jpeg('C:/Ian_GIS/FreshwaterConservation/Exports/Figs/hist_conn_scores.jpeg',width = 4.26,height = 3.85,units = 'in',res=600)
  par(mfrow=c(1,1))
  par(mar=c(2.5,3,1,0.5)) #bot,left,top,right
  hist(hydro_terr_conn_df$PChydroall, main='', ylab='', xlab='', xlim=c(0,13), ylim=c(0,3500), breaks=seq(0,13,1))
#dev.off()
  
  
  
#hydro_terr_conn_df$hydroQ <- dplyr::ntile(hydro_terr_conn_df$PC1and2_hydro, 4)
#hydro_terr_conn_df$terrQ <- dplyr::ntile(hydro_terr_conn_df$PC1and2and3_terr, 4)
#hydro_terr_conn_df$hydroterrQ <- dplyr::ntile(hydro_terr_conn_df$hydro_terr, 4)

## 3D scatterplot of combined hydro/terr conn score, %buffer protected, %IWS protected
# library(scatterplot3d)
scatter3d_df <- merge(PADUS_buff_conn, PADUS_IWS_conn, by='lagoslakeid')
# attach(scatter3d_df)
# par(mfrow=c(1,1))
# scatterplot3d(hydro_terr.x, GAP12_buff_pct, GAP12_IWS_pct, xlab='Hydro/terr conn score', ylab='Buffer % protected', 
#               zlab='IWS % protected', main='GAPS 1-2')
# scatterplot3d(hydro_terr.x, GAP123_buff_pct, GAP123_IWS_pct, xlab='Hydro/terr conn score', ylab='Buffer % protected', 
#               zlab='IWS % protected', main='GAPS 1-3')

# with help from: http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization 

#jpeg('C:/Ian_GIS/FreshwaterConservation/Exports/Figs/conn_scores_PADUS_3d.jpeg',width = 6,height = 4,units = 'in',res=600)
  par(mfrow=c(1,2))
  par(mar=c(1,1,2,2)) #bot,left,top,right
  attach(scatter3d_df)
  scatter3D(hydro_terr.x, GAP12_buff_pct, GAP12_IWS_pct, bty='g', phi=20, theta=20, xlab='Hydro/terr conn score', ylab='Buffer % protected', 
          zlab='IWS % protected', main='GAPS 1-2', colvar=hydro_terr.x, pch=20,
          col=ramp.col(c("firebrick","tan","dodgerblue")),
          clab='Conn score', colkey=list(plot=F))
  
  scatter3D(hydro_terr.x, GAP123_buff_pct, GAP123_IWS_pct, bty='g', phi=20, theta=20, xlab='Hydro/terr conn score', ylab='Buffer % protected', 
          zlab='IWS % protected', main='GAPS 1-3', colvar=hydro_terr.x, pch=20,
          col=ramp.col(c("firebrick","tan","dodgerblue")),
          clab='Conn score')
#dev.off()

cor(scatter3d_df$GAP12_buff_pct, scatter3d_df$GAP12_IWS_pct, method='pearson')
cor(scatter3d_df$GAP123_buff_pct, scatter3d_df$GAP123_IWS_pct, method='pearson')

#### What are the characteristics of lakes with different connectivity scores? ##
# Hydrological
dev.off()
hydro_conn_char <- merge(pca_hydro_scores, hydro_conn_df, by='lagoslakeid')
hydro_conn_char <- hydro_conn_char[,c(1,5,7:10)]
#hydro_conn_char$ScoreGroup <- cut(hydro_conn_char$PChydroall, breaks=c(seq(0,10,1),20))
hydro_conn_char$ScoreGroup <- cut(hydro_conn_char$PChydroall, breaks=c(0,2,5,13))

# number of lakes per conn score group
hydro_conn_char %>% 
  group_by(ScoreGroup) %>%
  summarise(nLakes=length(ScoreGroup))

par(mfrow=c(2,2))
boxplot(hydro_conn_char$stream_density_mperha ~ hydro_conn_char$ScoreGroup, las=1, 
        xlab='Hydro conn score', main='Stream density')

#boxplot(hydro_conn_char$wetland_pct ~ hydro_conn_char$ScoreGroup, las=1, 
#        xlab='Hydro conn score', main='Wetland prop')

boxplot(hydro_conn_char$connwetland_pct ~ hydro_conn_char$ScoreGroup, las=1, 
        xlab='Hydro conn score', main='Conn wetland prop')

boxplot(hydro_conn_char$shoreline_wetlands_pct ~ hydro_conn_char$ScoreGroup, las=1, 
        xlab='Hydro conn score', main='Shoreline wetland prop')

# Terrestrial
terr_conn_char <- merge(pca_terr_scores, terr_conn_df, by='lagoslakeid')
terr_conn_char <- terr_conn_char[,c(1,7:11,14)]
#terr_conn_char$ScoreGroup <- cut(terr_conn_char$PCterrall, breaks=c(seq(0,10,1),25))
terr_conn_char$ScoreGroup <- cut(terr_conn_char$PCterrall, breaks=c(0,2,5,13))

# number of lakes per conn score group
terr_conn_char %>% 
  group_by(ScoreGroup) %>%
  summarise(nLakes=length(ScoreGroup))

par(mfrow=c(2,3))
boxplot(terr_conn_char$nLakePatches ~ terr_conn_char$ScoreGroup, las=1, 
        xlab='Terr conn score', main='Lake patches')

boxplot(terr_conn_char$LakeEdgeArea_pct ~ terr_conn_char$ScoreGroup, las=1, 
        xlab='Terr conn score', main='Lake edge area prop')

boxplot(terr_conn_char$nWetlandPatches ~ terr_conn_char$ScoreGroup, las=1, 
        xlab='Terr conn score', main='Wetland patches')

boxplot(terr_conn_char$WetlandArea_pct ~ terr_conn_char$ScoreGroup, las=1, 
        xlab='Terr conn score', main='Wetland prop')

# boxplot(terr_conn_char$shape_index ~ terr_conn_char$ScoreGroup, las=1, 
#         xlab='Terr conn score', main='Shape index')

boxplot(terr_conn_char$min_cost_dist_corrected ~ terr_conn_char$ScoreGroup, las=1, 
        xlab='Terr conn score', main='Cost distance')

# Combined hydro/terrestrial
hydro_terr_conn_char <- merge(hydro_terr_conn_df, terr_conn_char[,c(1:8)], by='lagoslakeid')
hydro_terr_conn_char <- merge(hydro_terr_conn_char, hydro_conn_char[,c(1:6)], by='lagoslakeid')
hydro_terr_conn_char <- hydro_terr_conn_char[,c(1:4,6:10,13:16)]
colnames(hydro_terr_conn_char)[2:3] <- c('PChydroall','PCterrall')
#hydro_terr_conn_char$ScoreGroup <- cut(hydro_terr_conn_char$hydro_terr, breaks=c(seq(0,10,2),24))
#hydro_terr_conn_char$ScoreGroup <- cut(hydro_terr_conn_char$hydro_terr, breaks=c(0,2,5,13))
# Create new column that divides data into quadrants per conceptual model, using 5 as the cutoff
score_cutoff <- 5
hydro_terr_conn_char$Quadrant <- ifelse(hydro_terr_conn_char$PChydroall > score_cutoff & hydro_terr_conn_char$PCterrall < score_cutoff, 'QA',NA)
hydro_terr_conn_char$Quadrant <- ifelse(hydro_terr_conn_char$PChydroall < score_cutoff & hydro_terr_conn_char$PCterrall < score_cutoff, 'QC',hydro_terr_conn_char$Quadrant)
hydro_terr_conn_char$Quadrant <- ifelse(hydro_terr_conn_char$PChydroall > score_cutoff & hydro_terr_conn_char$PCterrall > score_cutoff, 'QB',hydro_terr_conn_char$Quadrant)
hydro_terr_conn_char$Quadrant <- ifelse(hydro_terr_conn_char$PChydroall < score_cutoff & hydro_terr_conn_char$PCterrall > score_cutoff, 'QD',hydro_terr_conn_char$Quadrant)

# number of lakes per conn score group
hydro_terr_conn_char %>% 
  group_by(Quadrant) %>%
  summarise(nLakes=length(Quadrant))

dev.off()



#jpeg('C:/Ian_GIS/FreshwaterConservation/Exports/Figs/panel_boxplot_conn_scores.jpeg',width = 8,height = 5,units = 'in',res=600)
par(mfrow=c(2,4))
# top four
par(mar=c(2,4,2,1)) #bot,left,top,right
boxplot(hydro_terr_conn_char$nLakePatches ~ hydro_terr_conn_char$Quadrant, las=1, 
        xlab='', main='Lake patches', ylab='Number of patches in buffer')

boxplot(hydro_terr_conn_char$LakeEdgeArea_pct ~ hydro_terr_conn_char$Quadrant, las=1, 
        xlab='', main='Lake edge area', ylab='Proportion of buffer')

boxplot(hydro_terr_conn_char$nWetlandPatches ~ hydro_terr_conn_char$Quadrant, las=1, 
        xlab='', main='Wetland patches', ylab='Number of patches in buffer')

boxplot(hydro_terr_conn_char$min_cost_dist_corrected ~ hydro_terr_conn_char$Quadrant, las=1, 
        xlab='', main='Permeability', ylab='Low to high in buffer')

# bottom four
par(mar=c(4,4,2,1)) #bot,left,top,right
boxplot(hydro_terr_conn_char$stream_density_mperha ~ hydro_terr_conn_char$Quadrant, las=1, 
        xlab='Connectivity quadrant', main='Stream density', ylab='m/ha (watershed)')

boxplot(hydro_terr_conn_char$wetland_pct ~ hydro_terr_conn_char$Quadrant, las=1, 
        xlab='Connectivity quadrant', main='Wetland area', ylab='Proportion of watershed')

boxplot(hydro_terr_conn_char$connwetland_pct ~ hydro_terr_conn_char$Quadrant, las=1, 
        xlab='Connectivity quadrant', main='Stream-connected wetlands', ylab='Proportion of watershed')

boxplot(hydro_terr_conn_char$shoreline_wetlands_pct ~ hydro_terr_conn_char$Quadrant, las=1, 
        xlab='Connectivity quadrant', main='Shoreline wetlands', ylab='Proportion of lake perimeter')
#dev.off()

# Prepare shapefile for exploratory mapping in ArcGIS
mich_lakes_4ha_export <- merge(lakes_4ha_pts, hydro_terr_conn_char, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)

mich_lakes_4ha_export@data <- mich_lakes_4ha_export@data %>% 
  select(1, 6, 12, 13, 33:45)

mich_lakes_4ha_export <- merge(mich_lakes_4ha_export, hydro_terr_conn_df[,c(1:3)], by.x='lagoslakei', by.y='lagoslakeid')

# save to disk for better mapping in ArcGIS
dsnname <- "C:/Ian_GIS/FreshwaterConservation/ConnIndices"
layername <- "hydro_terr_conn_index"
#writeOGR(mich_lakes_4ha_export, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

# Conn score quadrant by % protection?
lagoslakeid_quadrant <- data.frame(lagoslakeid=hydro_terr_conn_char$lagoslakeid, Quadrant=hydro_terr_conn_char$Quadrant)
PADUS_buff_quadrant <- merge(lagoslakeid_quadrant, PADUS_buff_conn[,1:3], by='lagoslakeid', all.x=F)
PADUS_IWS_quadrant <- merge(lagoslakeid_quadrant, PADUS_IWS_conn[,1:3], by='lagoslakeid', all.x=F)

# paneled boxplots of % protection by conn quadrant
#jpeg('C:/Ian_GIS/FreshwaterConservation/Exports/Figs/panel_boxplot_PADUS.jpeg',width = 6,height = 6,units = 'in',res=600)
par(mfrow=c(2,2))
par(mar=c(2,4,2,1)) #bot,left,top,right
boxplot(PADUS_IWS_quadrant$GAP12_IWS_pct ~ PADUS_IWS_quadrant$Quadrant, las=1, main='GAPS 1-2',
        ylab='Proportion watershed protected', xaxt='n')

par(mar=c(2,0.5,2,4.5)) #bot,left,top,right
boxplot(PADUS_IWS_quadrant$GAP123_IWS_pct ~ PADUS_IWS_quadrant$Quadrant, las=1, main='GAPS 1-3',
        ylab='', yaxt='n', xaxt='n')

par(mar=c(3.5,4,0.5,1)) #bot,left,top,right
boxplot(PADUS_buff_quadrant$GAP12_buff_pct ~ PADUS_buff_quadrant$Quadrant, las=1, main='',
        ylab='Proportion buffer protected')

par(mar=c(3.5,0.5,0.5,4.5)) #bot,left,top,right
boxplot(PADUS_buff_quadrant$GAP123_buff_pct ~ PADUS_buff_quadrant$Quadrant, las=1, main='',
        ylab='', yaxt='n')
#dev.off()


## Pariwise comparisons of % protection by quadrants
library(lattice)
# Check distributions for normality
histogram(~ GAP12_IWS_pct | Quadrant,data=PADUS_IWS_quadrant,layout=c(1,4))
histogram(~ GAP123_IWS_pct | Quadrant,data=PADUS_IWS_quadrant,layout=c(1,4))
histogram(~ GAP12_buff_pct | Quadrant,data=PADUS_buff_quadrant,layout=c(1,4))
histogram(~ GAP123_buff_pct | Quadrant,data=PADUS_buff_quadrant,layout=c(1,4))

# not normal (many low values, so use Bartlett's test for homoskedasticity; can use Kruskal-Wallis if yes)
bartlett.test(GAP12_IWS_pct ~ Quadrant,data=PADUS_IWS_quadrant)
bartlett.test(GAP123_IWS_pct ~ Quadrant,data=PADUS_IWS_quadrant)
bartlett.test(GAP12_buff_pct ~ Quadrant,data=PADUS_buff_quadrant)
bartlett.test(GAP123_buff_pct ~ Quadrant,data=PADUS_buff_quadrant)

# Global Welch's one-way ANOVA (OK with heteroskedasticity)
oneway.test(GAP12_IWS_pct ~ Quadrant,data=PADUS_IWS_quadrant)
oneway.test(GAP123_IWS_pct ~ Quadrant,data=PADUS_IWS_quadrant)
oneway.test(GAP12_buff_pct ~ Quadrant,data=PADUS_buff_quadrant)
oneway.test(GAP123_buff_pct ~ Quadrant,data=PADUS_buff_quadrant)

# By setting pool.sd to F, turning off homoskedasticity assumption
# but sample sizes in group B very small (and very unequal in general, except A and D)
pairwise.t.test(PADUS_IWS_quadrant$GAP12_IWS_pct, PADUS_IWS_quadrant$Quadrant, p.adjust.method='BH', pool.sd=F)
pairwise.t.test(PADUS_IWS_quadrant$GAP123_IWS_pct, PADUS_IWS_quadrant$Quadrant, p.adjust.method='BH', pool.sd=F)
pairwise.t.test(PADUS_buff_quadrant$GAP12_buff_pct, PADUS_buff_quadrant$Quadrant, p.adjust.method='BH', pool.sd=F)
pairwise.t.test(PADUS_buff_quadrant$GAP123_buff_pct, PADUS_buff_quadrant$Quadrant, p.adjust.method='BH', pool.sd=F)

# kruskal.test(PADUS_IWS_quadrant$GAP12_IWS_pct ~ PADUS_IWS_quadrant$Quadrant)
# kruskal.test(PADUS_IWS_quadrant$GAP123_IWS_pct ~ PADUS_IWS_quadrant$Quadrant)
# kruskal.test(PADUS_buff_quadrant$GAP12_buff_pct ~ PADUS_IWS_quadrant$Quadrant)
# kruskal.test(PADUS_buff_quadrant$GAP123_buff_pct ~ PADUS_IWS_quadrant$Quadrant)

# Median protection by quadrant (group)
PADUS_IWS_quadrant %>%
  group_by(Quadrant) %>%
  summarize(Median=median(GAP12_IWS_pct))

PADUS_IWS_quadrant %>%
  group_by(Quadrant) %>%
  summarize(Median=median(GAP123_IWS_pct))

PADUS_buff_quadrant %>%
  group_by(Quadrant) %>%
  summarize(Median=median(GAP12_buff_pct))

PADUS_buff_quadrant %>%
  group_by(Quadrant) %>%
  summarize(Median=median(GAP123_buff_pct))


# Conn score by LAGOS lake conn type?
par(mfrow=c(1,3))
boxplot(mich_lakes_4ha_export@data$hydro_terr ~ as.factor(mich_lakes_4ha_export@data$LakeConnec), las=1,
        ylab='Combined hydro/terrestrial conn score', main='Combined', ylim=c(0,13), names=c('DRLS','DRS','HW','ISOL'))

boxplot(mich_lakes_4ha_export@data$PChydroall ~ as.factor(mich_lakes_4ha_export@data$LakeConnec), las=1,
        ylab='Hydro conn score', main='Hydrologic', ylim=c(0,13), names=c('DRLS','DRS','HW','ISOL'))

boxplot(mich_lakes_4ha_export@data$PCterrall ~ as.factor(mich_lakes_4ha_export@data$LakeConnec), las=1,
        ylab='Terrestrial conn score', main='Terrestrial', ylim=c(0,13), names=c('DRLS','DRS','HW','ISOL'))

# normal distribution may be violated...
TukeyHSD(aov(mich_lakes_4ha_export@data$hydro_terr ~ as.factor(mich_lakes_4ha_export@data$LakeConnec)))
TukeyHSD(aov(mich_lakes_4ha_export@data$PChydroall ~ as.factor(mich_lakes_4ha_export@data$LakeConnec)))
TukeyHSD(aov(mich_lakes_4ha_export@data$PCterrall ~ as.factor(mich_lakes_4ha_export@data$LakeConnec)))


