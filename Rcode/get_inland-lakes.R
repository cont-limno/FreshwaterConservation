# get inland lakes in Michigan not upstream of or directly connected to the 
# Great Lakes

library(LAGOSNEgis)
library(nhdR)
library(sf)
library(dplyr)
library(units)
library(mapview) # optional

# ---- load_gis_data ----

# st_layers(lagosnegis_path())
# gdalUtils::ogrinfo(lagosnegis_path(), "LAGOS_NE_All_Lakes_4ha", so = TRUE)
# gdalUtils::ogrinfo(lagosnegis_path(), "Stream_Polylines", so = TRUE)

mich_shp       <- query_gis("STATE", "State_Name", "Michigan")
mich_lakes_4ha <- query_gis_(query = "SELECT * FROM LAGOS_NE_All_Lakes_4ha WHERE STATE LIKE 'MI'")

# remove lakes that intersect the Great Lakes
great_lakes <- great_lakes(spatial = TRUE)
bad_lakes <- mich_lakes_4ha[
  unlist(lapply(
    st_intersects(mich_lakes_4ha, st_buffer(gl, 20)), 
    function(x) length(x) > 0)),]

# remove lakes in LAGOS that are not in NHDPlus
# llid <- 2227
# TODO

# xwalk          <- load_xwalk()
# mich_lakes_4ha <- left_join(mich_lakes_4ha, 
#                             dplyr::select(xwalk, "lagoslakeid", 
#                                           "NHDHR_PermanentIdentifier"))

# ---- find_bad_lakes -----

# Check out the following possible bad lakes:
# llid <- 2639
# llid <- 2197

is_bad_lake <- function(llid, buffer_size){
  coords      <- lake_info(lagoslakeid = llid)
  focal_poly  <- dplyr::filter(mich_lakes_4ha, lagoslakeid == llid)
  buffer_size <- units::set_units(
    sqrt(coords$lake_area_ha * 10000) * 1.2,
    "m")
  
  # get intersecting flowlines, bad lake if they are all FTYPE coastal
  focal_lines <- nhdR::nhd_plus_query(lat = coords$nhd_lat,
                                    lon = coords$nhd_long,
                                    dsn = "NHDFlowLine",
                                    buffer_dist = buffer_size)$sp$NHDFlowLine
  focal_lines <- st_transform(focal_lines, st_crs(focal_poly))
  focal_lines <- focal_lines[
    unlist(lapply(
      st_intersects(focal_lines, focal_poly),
      function(x) length(x) > 0)),]
  all_coastal <- all(focal_lines$FTYPE == "Coastline")
  
  if(!all_coastal){
    # walk upstream, bad lake if we get any FTYPE coastal reaches
    test <- nhdR::extract_network(lon = coords$nhd_long, lat = coords$nhd_lat)
    
    
    
  }
  
  mapview::mapview(focal_poly) + mapview::mapview(focal_lines)
  
  
  
}



# get lakes that only contain flowlines with a "Coastal" FTYPE
# get flowlines that intersect the boundary of the Michigan landmass 
