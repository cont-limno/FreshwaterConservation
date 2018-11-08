####################### Landscape connectivity of lakes ########################################
# Date: 11-8-18
# updated:
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(grainscape)
library(ggplot2)

#### input data ####
setwd("C:/Users/FWL/Documents/FreshwaterConservation")

# cost raster from ArcGIS
mich_cost <- raster("C:/Ian_GIS/FreshwaterConservation/Exports/MI_Cost.tif")

# michigan shapefile
mich_shp <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/STATE/Michigan.shp")

# LAGOS NE lakes
lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS_US_4ha_lakes/LAGOS_US_All_Lakes_4ha_pts/LAGOS_US_All_Lakes_4ha_pts.shp")

# Zonal statistics table of Cost Distance surface for each lake (ArcGIS output)
zonal_cost_dist <- read.csv("C:/Ian_GIS/FreshwaterConservation/Exports/MI_CostDist_lagoslakeid.csv")

# PADUS raster (protected areas database)
PADUS <- raster("C:/Ian_GIS/FreshwaterConservation/Exports/MI_PADUS.tif")

############################ Main program ############################
# from grainscape...haven't gotten to work yet
#patchyMPG <- MPG(mich_cost, patch = (mich_cost == 9))

### exploratory analysis of lake zonal cost data
hist(zonal_cost_dist$MEAN, breaks=seq(0,30000,100))
hist(zonal_cost_dist$MEAN, breaks=seq(0,30000,1000), xlim=c(0,10000))
lower_uppers <- quantile(zonal_cost_dist$MEAN, c(0.05, 0.95))

# how many really isolated lakes? (based on arbitrary cutoff of 95th percentile)
nrow(subset(zonal_cost_dist, MEAN >= lower_uppers[2]))

lakes_4ha_pts_ZN <- merge(lakes_4ha_pts, zonal_cost_dist, by='lagoslakei', all.x=F)
lakes_4ha_pts_ZN_isol <- subset(lakes_4ha_pts_ZN, MEAN >= lower_uppers[2])


plot(mich_shp)
plot(PADUS, add=T, col=c('darkgreen','darkolivegreen1','gray45'))
plot(lakes_4ha_pts_ZN_isol, col='orange', pch=20, add=T)
title('Terrestrially isolated lakes, protected areas')
mtext(side=3, paste0('n lakes: ', nrow(lakes_4ha_pts_ZN_isol@data)))

hist(zonal_cost_dist$MEAN, breaks=seq(0,30000,1000), xlim=c(0,10000), xlab=paste0('Low to High'),
     main='Relative cost distance', ylab='')
