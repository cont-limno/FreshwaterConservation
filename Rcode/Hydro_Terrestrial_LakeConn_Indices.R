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

# a few NA values in hydro conn, but these can be treated as 0
hydro_conn_df[is.na(hydro_conn_df)] <- 0

## how correlated are hydro and terr conn vars?
cor(hydro_conn_df[,2:ncol(hydro_conn_df)], use='pairwise.complete.obs', method='pearson')
cor(terr_conn_df[,2:ncol(terr_conn_df)], use='pairwise.complete.obs', method='pearson')

### PCA
hydro_terr_conn_df <- merge(hydro_conn_df, terr_conn_df, by='lagoslakeid', all=F)

#pca <- rda(hydro_terr_conn_df[,2:ncol(hydro_terr_conn_df)], scale=T)
pca <- princomp(hydro_terr_conn_df[,2:ncol(hydro_terr_conn_df)], cor=T, scores=T)
screeplot(pca, type='l')
summary(pca)
pca$loadings

#help from: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


# PCA results for variables
pca_results_var <- get_pca_var(pca)
pca_results_var$contrib
pca_results_var$cos2

## What if do individual PCAs on just hydro and just terr variables?
pca_hydro <- princomp(hydro_conn_df[,2:ncol(hydro_conn_df)], cor=T, scores=T)
screeplot(pca_hydro, type='l')
summary(pca_hydro)

pca_terr <- princomp(terr_conn_df[,2:ncol(terr_conn_df)], cor=T, scores=T)
screeplot(pca_terr, type='l')
summary(pca_terr)

#### Some other approach? Non-metric multidimensional scaling
# library(ecodist)
# distmat <- vegdist(hydro_terr_conn_df[,2:ncol(hydro_terr_conn_df)], method='bray')
# nmds_test <- nmds(distmat, mindim=2, maxdim=2) #warning: slow
# stressplot(nmds_test, distmat)