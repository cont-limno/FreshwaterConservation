# get inland lakes in Michigan not upstream of or directly connected to the 
# Great Lakes

library(LAGOSNEgis)
library(nhdR)
library(sf)
library(dplyr)
library(units)

# ---- load_gis_data ----

# st_layers(lagosnegis_path())
# gdalUtils::ogrinfo(lagosnegis_path(), "LAGOS_NE_All_Lakes_4ha", so = TRUE)
# gdalUtils::ogrinfo(lagosnegis_path(), "Stream_Polylines", so = TRUE)

mich_shp       <- query_gis("STATE", "State_Name", "Michigan")
mich_lakes_4ha <- query_gis_(query = "SELECT * FROM LAGOS_NE_All_Lakes_4ha WHERE STATE LIKE 'MI'")

# xwalk          <- load_xwalk()
# mich_lakes_4ha <- left_join(mich_lakes_4ha, 
#                             dplyr::select(xwalk, "lagoslakeid", 
#                                           "NHDHR_PermanentIdentifier"))

# ---- find_bad_lakes -----

## Houghton Lake is the largest MI inland lake
## use it to generate a polyline selection buffer
# 
# coords <- lake_info(name = "Houghton Lake", state = "Michigan")
# 
# units::set_units(
#   units::set_units(
#     lake_info(name = "Houghton Lake", state = "Michigan")$lake_area_ha, 
#     "ha"), "dd")
# 
# greatlakes_lines <- nhdR::nhd_plus_query(lat = coords$nhd_lat,
#                                          lon = coords$nhd_long,
#                                          dsn = "NHDFlowLine",
#                                          buffer_dist = 0.1)$sp$NHDFlowLine

is_bad_lake <- function(llid){
  llid        <- 2639 # should be a bad lake
  coords      <- lake_info(lagoslakeid = llid)[,c("nhd_long", "nhd_lat")]
  focal_poly  <- dplyr::filter(mich_lakes_4ha, lagoslakeid == llid)
  
  # get intersecting flowlines, are they all FTYPE coastal?
  focal_lines <- nhdR::nhd_plus_query(lat = coords$nhd_lat,
                                           lon = coords$nhd_long,
                                           dsn = "NHDFlowLine",
                                           buffer_dist = 0.1)$sp$NHDFlowLine
  focal_lines <- st_transform(focal_lines, st_crs(focal_poly))
  focal_lines <- focal_lines[
    unlist(lapply(
      st_intersects(focal_lines, focal_poly),
      function(x) length(x) > 0)),]
  all_coastal <- all(focal_lines$FTYPE == "Coastline")
  
  if(!all_coastal){
    # walk upstream
    # bad lake if we get FTYPE coastal reaches
    nhdR::terminal_reaches(lon = coords$nhd_long, lat = coords$nhd_lat)
    
  }
  
  mapview::mapview(focal_poly) + mapview::mapview(focal_lines)
  
  
  
}

#Check out the following possible bad lakes:
# lagoslakeid != 2227,
# lagoslakeid != 2197,
#lagoslakeid != 4930

# get lakes that only contain flowlines with a "Coastal" FTYPE
# get flowlines that intersect the boundary of the Michigan landmass 
