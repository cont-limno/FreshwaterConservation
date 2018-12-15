# Function to calculate stats on lake and wetland patches within specified buffers (diserpsal buffers for wildlife)
# Date: 12-15-18
# output table description
# lagoslakeid: focal lake ID
# nLakePatches: number of lake patches within buffer (any part of lake intersects with buffer)
# LakeEdge_km: length of lake patches within buffer (only lake parts in buffer; analogous to entry points to other waterbodies from focal lake)
# LakeEdgeArea_ha: amount of lake edge habitat, function of LakeEdge_km and assumption that 30m of perimeter=1 30x30m cell area (900sqm)
# LakeEdgeArea_pct: proportion of lake buffer covered by lake edge habitat (focal lake removed)
# nWetlandPatches: number of wetland patches within buffer (from NWI)
# WetlandEdge_km: length of wetland patches within buffer (only wetland parts in buffer; analogous to entry points to other waterbodies from focal lake)
# WetlandArea_ha: amount of wetland habitat within buffer
# WetlandArea_pct: proportion of lake buffer covered by wetland habitat (focal lake removed)
# BufferWidth_m: length of buffer used in analysis

library(raster)
library(rgeos)
library(spatialEco)

lake_dispersal_buffer_patch_metrics <- function(lagoslakeid, LAGOS_shp, NHD_shp, wetland_shp, dispersal_buff){
  #all GIS data assumed to be in same coordinate reference system (CRS)
  #lagoslakeid: unique ID of focal lake from LAGOS (>=4ha)
  #LAGOS_shp: LAGOS lake polygons (>=4ha)
  #NHD_shp: polygons from NHD containing LAGOS lakes and others >=0.1 ha
  #wetland_shp: Natl Wetland Inventory (NWI) polygons; WETLAND_TYPE: Freshwater Emergent Wetland' OR 'Freshwater Forested/Shrub Wetland' OR 'Other'
  #dispersal_buff: dispersal buffer (radius) around focal lake (meters; reflecting input data CRS): represents area available for wildlife dispersing/moving from focal lake
  
  focal_lake <- lagoslakeid
  focal_lake_shp <- subset(LAGOS_shp, lagoslakei %in% lagoslakeid) #focal LAGOS lake polygon only
  focal_lake_buff <- gBuffer(focal_lake_shp, byid=T, width=dispersal_buff)
  focal_lake_buff_area_ha <- (gArea(focal_lake_buff) - gArea(focal_lake_shp))/10000 #area of focal lake buffer, minus area of focal lake itself
  
  # identify lakes that intersect with buffer, excluding focal lake (length=1 if buffer only contains focal lake)
  intersecting_lakes <- raster::intersect(NHD_shp, focal_lake_buff)
  if (length(intersecting_lakes) <= 1) {
    lake_output_df <- data.frame(lagoslakeid=lagoslakeid, nLakePatches=0, LakeEdge_km=0, LakeEdgeArea_ha=0, LakeEdgeArea_pct=0, stringsAsFactors=F) #stringsAsFactors prevents factor level from being returned over character value
    
  } else {
    lake_output_df <- data.frame(lagoslakeid=lagoslakeid, nLakePatches=(length(intersecting_lakes)-1), LakeEdge_km=NA, LakeEdgeArea_ha=NA, LakeEdgeArea_pct=NA, stringsAsFactors=F)
    # LakeEdge_km: perimeter of intersecting lakes in buffer, minus perimeter of focal lake
    lake_output_df$LakeEdge_km <- (sum(spatialEco::polyPerimeter(intersecting_lakes), na.rm=T)/1000) - (spatialEco::polyPerimeter(focal_lake_shp)/1000)
    # LakeEdgeArea_ha: assumes edge area is analogous to 30m pixels of shoreline (30m=res of other rasters in analysis)
    lake_output_df$LakeEdgeArea_ha <- (((lake_output_df$LakeEdge_km*1000)/30) * 900)/10000
    # LakeEdgeArea_pct: divide edge area by focal_lake_buffer_area_ha
    lake_output_df$LakeEdgeArea_pct <- lake_output_df$LakeEdgeArea_ha/focal_lake_buff_area_ha
  }
  # similar analysis for wetlands
  intersecting_wetlands <- raster::intersect(wetland_shp, focal_lake_buff)#much slower than lakes; more polygons
  if (length(intersecting_wetlands) <= 1) {
    wetland_output_df <- data.frame(lagoslakeid=lagoslakeid, nWetlandPatches=0, WetlandEdge_km=0, WetlandArea_ha=0, WetlandArea_pct=0, BufferWidth_m=dispersal_buff, stringsAsFactors=F) #stringsAsFactors prevents factor level from being returned over character value
    
  } else {
    wetland_output_df <- data.frame(lagoslakeid=lagoslakeid, nWetlandPatches=length(intersecting_wetlands), WetlandEdge_km=NA, WetlandArea_ha=NA, WetlandArea_pct=NA, BufferWidth_m=dispersal_buff, stringsAsFactors=F)
    # WetlandEdge_km: perimeter of intersecting lakes in buffer
    wetland_output_df$WetlandEdge_km <- sum(spatialEco::polyPerimeter(intersecting_wetlands), na.rm=T)/1000 
    # WetlandArea_ha: assumes edge area is analogous to 30m pixels of shoreline (30m=res of other rasters in analysis)
    wetland_output_df$WetlandArea_ha <- sum(gArea(intersecting_wetlands), na.rm=T)/10000
    # WetlandArea_pct: divide edge area by focal_lake_buffer_area_ha
    wetland_output_df$WetlandArea_pct <- wetland_output_df$WetlandArea_ha/focal_lake_buff_area_ha
  }
  
  # combine lake and wetland variables into single data frame as the output
  output_df <- merge(lake_output_df, wetland_output_df, by='lagoslakeid')
  rownames(output_df) <- lagoslakeid
  return(output_df)
  
  # clean up
  wetland_output_df <- NULL
  lake_output_df <- NULL
  focal_lake <- NULL
  focal_lake_shp <- NULL
  focal_lake_buff <- NULL
  focal_lake_buff_area_ha <- NULL
  intersecting_lakes <- NULL
  intersecting_wetlands <- NULL
  output_df <- NULL
}
