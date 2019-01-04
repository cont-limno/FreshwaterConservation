############### Terrestrial and hydrologic lake connectivity indices ###########################
# Date: 1-3-19
# updated: 
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(vegan)
library(factoextra)

#### input data ####
setwd("C:/Users/FWL/Documents/FreshwaterConservation")
hydro_conn_df <- read.csv("Data/Michigan_LAGOS_conn_metrics.csv")
terr_conn_df <- read.csv("Data/LakeWetlandPatchStats_2020mBuff.csv")
patch_statz <- read.csv("Data/MichiganLakePatchStats_wBorderStates.csv")

# Michigan shapefile
mich_shp <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/STATE/Michigan.shp")

# LAGOS NE lakes
lakes_4ha_poly <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/LAGOS_NE_All_Lakes_4ha/LAGOS_NE_All_Lakes_4ha.shp")
lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/LAGOS_NE_All_Lakes_4ha_POINTS/LAGOS_NE_All_Lakes_4ha_POINTS.shp")

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

# keep only some terrestrial conn vars
terr_conn_df <- terr_conn_df[,c('lagoslakeid','nLakePatches','LakeEdgeArea_pct','nWetlandPatches','WetlandArea_pct','shape_index')]

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
cor(terr_conn_df[,2:ncol(terr_conn_df)], use='pairwise.complete.obs', method='pearson')

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
summary(terr_conn_df[,2:ncol(terr_conn_df)])
par(mfrow=c(2,3))
for (i in 2:ncol(terr_conn_df)){
  hist(terr_conn_df[,i], main=colnames(terr_conn_df[i]))
}

pca_terr <- princomp(~ nLakePatches + LakeEdgeArea_pct + nWetlandPatches + WetlandArea_pct + shape_index, 
                     data=terr_conn_df, cor=T, scores=T)
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
hist(pca_terr_scores$PC1and2)
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


#### Some other approach? Non-metric multidimensional scaling
# library(ecodist)
# distmat <- vegdist(hydro_terr_conn_df[,2:ncol(hydro_terr_conn_df)], method='bray')
# nmds_test <- nmds(distmat, mindim=2, maxdim=2) #warning: slow
# stressplot(nmds_test, distmat)