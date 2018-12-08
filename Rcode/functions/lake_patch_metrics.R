## Function to calculate lake patch metrics based on specified dispersal buffer (based on a species or hypothetical species)
# output table columns:
# lagoslakeid: focal lake in LAGOS
# FullLakeArea_ha: total lake area of lakes intersecting with buffer (including area outside buffer)
# FullLakePerimeter_km: total lake perimeter of lakes intersecting with buffer (including lengths outside buffer)
# FullLakeEdgeArea_ha: total edge area of lakes intersecting with buffer (including lake area outside buffer)
# AvgFullLakeShapeIndex: average shape index of lakes intersecting with buffer (including lake area outside buffer)
# FullLakeCoreArea_ha: total core area of lakes intersecting with buffer (including lake area outside buffer)
# AvgFullLakeCoreAreaIndex: average core area index of lakes intersecting with buffer (including lake area outside buffer)
# nLakePatches: number of lakes (patches) within buffer
# BuffArea_ha: area of buffer zone
# EdgeLengthInBuff_km: total edge length (inside buffer only) of lakes intersecting with buffer
# EdgeAreaInBuff_ha: total area of edge habitat (inside buffer only) of lakes intersecting with buffer (1-cell deep in lake polygons, calculated using cellsize argument)
# EdgeAreaInBuff_pct: proportion of buffer area covered by edge habitat
# BufferWidth_m: taken from dispersal_buff argument (as record in output)
library(raster)
library(rgeos)
library(spatialEco)

lake_patch_metrics <- function(lagoslakeid, lake_shp, dispersal_buff, cellsize, patchmetric_df){
  #lagoslakeid: unique lake ID in LAGOS
  #lake_shp: shapefile of lake polygons
  #dispersal_buff: maximum overland dispersal distance, meters
  #cellsize: pixel width of analysis
  #patchmetric_df: output of SDMTools::PatchStat for lake_shp lakes (or subset)
  
  # extract focal lake, buffer it by dispersal width, get buffer area as buffer area minus focal lake area
  focal_lake <- lagoslakeid
  focal_lake_shp <- subset(lake_shp, lagoslakei %in% lagoslakeid)
  focal_lake_buff <- gBuffer(focal_lake_shp, byid=T, width=dispersal_buff)
  focal_lake_buff_area_ha <- (gArea(focal_lake_buff) - gArea(focal_lake_shp))/10000
  
  # identify lakes that intersect with buffer, excluding focal lake (length=1 if buffer only contains focal lake)
  intersecting_lakes <- raster::intersect(lake_shp, focal_lake_buff)
  
  if (length(intersecting_lakes) <= 1) {
    output_df <- data.frame(lagoslakeid=lagoslakeid, FullLakeArea_ha=0, FullLakePerimeter_km=0,
                            FullLakeEdgeArea_ha=0, AvgFullLakeShapeIndex=NA, FullLakeCoreArea_ha=0, AvgFullLakeCoreAreaIndex=NA,
                            nLakePatches=0, BuffArea_ha=focal_lake_buff_area_ha, EdgeLengthInBuff_km=0, EdgeAreaInBuff_ha=0, EdgeAreaInBuff_pct=0,
                            BufferWidth_m=dispersal_buff, stringsAsFactors=F) #stringsAsFactors prevents factor level from being returned over character value
    rownames(output_df) <- lagoslakeid
    
  } else {
    # Subtract perimeter of focal lake from total perimeter of intersecting lakes (round to make tiny numbers 0)
    EdgeLength_km <- round(((sum(spatialEco::polyPerimeter(intersecting_lakes), na.rm=T) - spatialEco::polyPerimeter(focal_lake_shp))/1000),4)
    # get edge habitat area by getting shoreline length in meters, divided by cell size to get number of cells, which then gets multiplied by cell area
    EdgeArea_ha <- (((EdgeLength_km*1000)/cellsize) * (cellsize^2))/10000

    # Bring in full lake patch data for intersecting lakes from other df (output of SDMTools::PatchStat)
    intersecting_lakes_IDs <- unique(intersecting_lakes$lagoslakei.1)
    intersecting_lakes_IDs <- intersecting_lakes_IDs[ intersecting_lakes_IDs != lagoslakeid ]#remove focal lake
    intersecting_lakes_metrics <- subset(patchmetric_df, patchID %in% intersecting_lakes_IDs)
    intersecting_lakes_metrics <- intersecting_lakes_metrics[,c('patchID','area','perimeter','edge.area','shape.index','core.area','core.area.index')]
    colnames(intersecting_lakes_metrics) <- c('lagoslakeid','FullLakeArea_sqm','FullLakePerimeter_m','FullLakeEdgeArea_sqm','AvgFullLakeShapeIndex','FullLakeCoreArea_ha','AvgFullLakeCoreAreaIndex')
    bcde <- colSums(intersecting_lakes_metrics[,2:5], na.rm=T)
    fg <- colMeans(intersecting_lakes_metrics[,6:7], na.rm=T)
    nLakePatches <- nrow(intersecting_lakes_metrics)
    
    output_df <- data.frame(lagoslakeid=lagoslakeid, FullLakeArea_ha=bcde[1]/10000, FullLakePerimeter_km=bcde[2]/1000,
                            FullLakeEdgeArea_ha=bcde[3]/10000, AvgFullLakeShapeIndex=bcde[4], FullLakeCoreArea_ha=fg[1], AvgFullLakeCoreAreaIndex=fg[2],
                            nLakePatches=nLakePatches, BuffArea_ha=focal_lake_buff_area_ha, stringsAsFactors=F) #stringsAsFactors prevents factor level from being returned over character value)
    output_df$EdgeLengthInBuff_km <- sum(EdgeLength_km, na.rm=T)
    output_df$EdgeAreaInBuff_ha <- sum(EdgeArea_ha, na.rm=T)
    output_df$EdgeAreaInBuff_pct <- output_df$EdgeAreaInBuff_ha/output_df$BuffArea_ha
    output_df$BufferWidth_m <- dispersal_buff
    rownames(output_df) <- lagoslakeid
    
  }
  return(output_df)
  # clean up
  intersecting_lakes_IDs <- NULL
  intersecting_lakes <- NULL
  intersecting_lakes_metrics <- NULL
  bcde <- NULL
  fg <- NULL
  nLakePatches <- NULL
  focal_lake <- NULL
  focal_lake_shp <- NULL
  focal_lake_buff <- NULL
  focal_lake_buff_area_ha <- NULL
  EdgeLength_km <- NULL
  EdgeArea_ha <- NULL
  output_df <- NULL
}
