####################### Size distribution of US protected areas ################################
# Date: 3-12-19
# updated:
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(raster)
library(rgeos)

#### input data ####
PADUS <- shapefile("C:/Ian_GIS/PADUS/PADUS_Gaps123/PADUS_Gaps123.shp")

############### Main program ##############
# subset by GAP status
GAP12 <- subset(PADUS, GAP_Sts == 1 | GAP_Sts==2)
GAP3 <- subset(PADUS, GAP_Sts==3)

# calculate areas
GAP12_areas <- gArea(GAP12, byid=T)/10000 #divide by 10000 to convert sq m to hectares
GAP3_areas <- gArea(GAP3, byid=T)/10000
summary(GAP12_areas)
summary(GAP3_areas)

# plotz
hist(GAP12_areas, col='forestgreen', xlim=c(0,1000), breaks=seq(0,2500000,10), xlab='hectares')
hist(GAP3_areas, col='moccasin', xlim=c(0,1000), breaks=seq(0,2500000,10), xlab='hectares')
