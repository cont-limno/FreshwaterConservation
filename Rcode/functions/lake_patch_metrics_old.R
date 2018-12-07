## Function to calculate lake patch metrics based on specified dispersal buffer (based on a species or hypothetical species)
# output table columns:
# lagoslakeid: focal lake in LAGOS
# nLakePatches: number of lakes within buffer
# FullLakeArea_ha: total lake area of lakes intersecting with buffer (including area outside buffer)
# FullLakePerimeter_km: total lake perimeter of lakes intersecting with buffer (including lengths outside buffer)
# EdgeLengthInBuff_km: total edge length of lakes in buffer
# EdgeLengthInBuff_ha: total edge area of lakes in buffer (area of lake margin cells; 1 cell deep around lake)
# EdgeLengthInBuff_pct: proportion of buffer area that is edge area
# LakeAreaInBuff_ha: total lake area of lakes in buffer only
# LakeAreaInBuff_pct: proportion of buffer area that is lake area
# mean.shape.index.full.lake: mean shape index of all lakes in buffer zone, including lake portions outside buffer
# BuffArea_ha: area of buffer zone
library(raster)
library(rgeos)
library(SDMTools)

lake_patch_metrics <- function(lagoslakeid, lake_shp, dispersal_buff, cellsize){
  #lagoslakeid: unique lake ID in LAGOS
  #lake_shp: shapefile of lake polygons
  #dispersal_buff: maximum overland dispersal distance, meters
  #cellsize: pixel width of analysis
  
  # extract focal lake, buffer it by dispersal width, erase focal lake from buffer zone
  focal_lake <- lagoslakeid
  focal_lake_shp <- subset(lake_shp, lagoslakei %in% lagoslakeid)
  focal_lake_buff <- gBuffer(focal_lake_shp, byid=T, width=dispersal_buff)
  focal_lake_buff_erased <- erase(focal_lake_buff, focal_lake_shp)
  focal_lake_buff_erased_area <- gArea(focal_lake_buff_erased)
  
  # identify lakes that intersect with buffer (only lake sections within buffer)
  # build in conditional statements:
  #1) if intersecting lakes do not exist, return empty output
  #2) if intersecting lakes do exist, but area of intersection is less than 1 pixel area, return empty output; otherwise, carry on
  intersecting_lakes <- raster::intersect(lake_shp, focal_lake_buff_erased)
  
  if (is.null(intersecting_lakes) == T) {
    combined <- data.frame(lagoslakeid=lagoslakeid, nLakePatches=0, FullLakeArea_ha=0, FullLakePerimeter_km=0, EdgeLengthInBuff_km=0, EdgeAreaInBuff_ha=0,
                           EdgeAreaInBuff_pct=0, LakeAreaInBuff_ha=0, LakeAreaInBuff_pct=0, mean.shape.index.full.lake=NA,
                           BuffArea_ha=focal_lake_buff_erased_area/10000, stringsAsFactors=F) #stringsAsFactors prevents factor level from being returned over character value
    rownames(combined) <- lagoslakeid
  } else {
    #intersecting_lakes <- intersecting_lakes
    if (gArea(intersecting_lakes) < cellsize^2) {
      combined <- data.frame(lagoslakeid=lagoslakeid, nLakePatches=0, FullLakeArea_ha=0, FullLakePerimeter_km=0, EdgeLengthInBuff_km=0, EdgeAreaInBuff_ha=0,
                             EdgeAreaInBuff_pct=0, LakeAreaInBuff_ha=0, LakeAreaInBuff_pct=0, mean.shape.index.full.lake=NA,
                             BuffArea_ha=focal_lake_buff_erased_area/10000, stringsAsFactors=F) #stringsAsFactors prevents factor level from being returned over character value
      rownames(combined) <- lagoslakeid
    } else {
      
      rtemp <- raster(extent(intersecting_lakes), res=cellsize)#create blank raster before rasterizing lakes based on lagoslakeid
      intersecting_lakes_ras <- rasterize(intersecting_lakes , rtemp, field=as.numeric(intersecting_lakes@data$lagoslakei.1))
      
      # identify lakes that intersect with buffer (FULL lake polygons)
      intersecting_lakes_full <- subset(lake_shp, lagoslakei %in% unique(intersecting_lakes@data$lagoslakei.1))
      rtemp <- raster(extent(intersecting_lakes_full), res=cellsize)
      intersecting_lakes_full_ras <- rasterize(intersecting_lakes_full, rtemp, field=as.numeric(intersecting_lakes_full@data$lagoslakei))
      
      # patch statistics for portions of lakes that fall within buffer (only interested in perimeter, which is edge/entry into lakes)
      BuffPatchStatz <- PatchStat(intersecting_lakes_ras, cellsize=cellsize, latlon=F) #verified by Ian: does not perform on whole lake; just intersecting part
      BuffPatchStatz <- BuffPatchStatz[,c('patchID','area','perimeter','n.edges.perimeter')] #keep only desired columns
      
      # patch statistics for full lakes intersecting with buffer, including portions of those lakes outside buffer
      FullLakePatchStatz <- PatchStat(intersecting_lakes_full_ras, cellsize=cellsize, latlon=F)
      newcolnames <- c(names(FullLakePatchStatz)[1], paste0(names(FullLakePatchStatz)[2:ncol(FullLakePatchStatz)],'.full.lake'))
      colnames(FullLakePatchStatz) <- newcolnames
      
      combined <- merge(BuffPatchStatz, FullLakePatchStatz, by='patchID', all.y=F) #may have lakes slightly in buffer, but lost in 2nd raster conversion (drop those in merge...assume negligible contribution)
      combined <- combined[,c('area','perimeter','n.edges.perimeter','shape.index.full.lake','area.full.lake','perimeter.full.lake')]
      
      combined$EdgeLengthInBuff_m <- combined$perimeter
      combined$EdgeAreaInBuff_sqm <- combined$n.edges.perimeter * (cellsize^2) #defining edge area as 1 pixel into lake
      combined$EdgeAreaInBuff_pct <- combined$EdgeAreaInBuff_sqm/focal_lake_buff_erased_area
      combined$LakeAreaInBuff_sqm <- combined$area
      combined$LakeAreaInBuff_pct <- combined$LakeAreaInBuff_sqm/focal_lake_buff_erased_area
      combined <- combined[,c('area.full.lake','perimeter.full.lake','EdgeLengthInBuff_m','EdgeAreaInBuff_sqm','EdgeAreaInBuff_pct','LakeAreaInBuff_sqm','LakeAreaInBuff_pct','shape.index.full.lake')]
      # NEED TO CALCULATE TOTALS FOR ALL OF THESE EXCEPT SHAPE INDEX (get mean)
      abcdefg <- colSums(combined[,1:7], na.rm=T)
      h <- mean(combined$shape.index.full.lake, na.rm=T)
      nLakePatches <- nrow(combined) 
      # put everything in output dataframe and convert sq meters to hectares
      combined <- data.frame(lagoslakeid=lagoslakeid, nLakePatches=nLakePatches, FullLakeArea_ha=abcdefg[1]/10000, FullLakePerimeter_km=abcdefg[2]/1000, EdgeLengthInBuff_km=abcdefg[3]/1000, EdgeAreaInBuff_ha=abcdefg[4]/10000,
                             EdgeAreaInBuff_pct=abcdefg[5], LakeAreaInBuff_ha=abcdefg[6]/10000, LakeAreaInBuff_pct=abcdefg[7], mean.shape.index.full.lake=h,
                             BuffArea_ha=focal_lake_buff_erased_area/10000, stringsAsFactors=F)#stringsAsFactors prevents factor level from being returned over character value
      rownames(combined) <- lagoslakeid
    }
  }
  return(combined)
  
  # clean up
  FullLakePatchStatz <- NULL
  newcolnames <- NULL
  intersecting_lakes_full <- NULL
  intersecting_lakes_full_ras <- NULL
  intersecting_lakes <- NULL
  intersecting_lakes_ras <- NULL
  BuffPatchStatz <- NULL
  focal_lake <- NULL
  focal_lake_buff <- NULL
  focal_lake_buff_erased <- NULL
  focal_lake_buff_erased_area <- NULL
  focal_lake_shp <- NULL
  rtemp <- NULL
  output <- NULL
  abcdefg <- NULL
  h <- NULL
  nLakePatches <- NULL
  combined <- NULL
}
