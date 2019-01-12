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
# llid <- 2639
# llid <- 2197
gl <- great_lakes(spatial = TRUE)
bad_lakes <- mich_lakes_4ha[
  unlist(lapply(
    st_intersects(mich_lakes_4ha, st_buffer(gl, 20)), 
    function(x) length(x) > 0)),]
bad_lakes$reason <- "intersect GL"

# remove lakes in LAGOS that are not in NHDPlus
# llid <- 2227
xwalk <- load_xwalk()
not_in_nhdp <- left_join(mich_lakes_4ha, 
                         dplyr::select(xwalk, -GNIS_Name, -GNIS_ID), 
                         by = "lagoslakeid") %>% 
  dplyr::filter(is.na(NHDPlusv2_COMID)) %>%
  dplyr::select(names(bad_lakes)[-which(names(bad_lakes) == "reason")]) %>%
  dplyr::filter(!(lagoslakeid %in% bad_lakes$lagoslakeid))
not_in_nhdp$reason <- "not in NHDp"
bad_lakes <- rbind(bad_lakes, not_in_nhdp)

# remove isolated lakes
# llid <- 2227
isolated_lakes <- dplyr::filter(mich_lakes_4ha, 
                                LakeConnectivity == "Isolated")
isolated_lakes$reason <- "isolated"
bad_lakes <- rbind(bad_lakes, isolated_lakes)
bad_lakes_llids <- bad_lakes$lagoslakeid

test <- dplyr::filter(mich_lakes_4ha, 
                      !(lagoslakeid %in% bad_lakes$lagoslakeid))

llid <- test$lagoslakeid[5]
# ---- find_bad_lakes -----
is_bad_lake <- function(llid){
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
  if(nrow(focal_lines) > 0){
    focal_lines <- st_transform(focal_lines, st_crs(focal_poly))
    focal_lines <- focal_lines[
      unlist(lapply(
        st_intersects(focal_lines, focal_poly),
        function(x) length(x) > 0)),]
    all_coastal <- all(focal_lines$FTYPE == "Coastline")
  }else{
    print("Lake is not connected to NHDPlus streams")
    all_coastal <- TRUE # lake not connected to nhdplus streams
  }
  
  if(!all_coastal){
    # walk upstream, bad lake if we get any FTYPE coastal reaches
    up_network <- nhdR::extract_network(
      lon = coords$nhd_long, 
      lat = coords$nhd_lat)
    if(!is.null(nrow(up_network))){
      downstream_of_gl <- any(up_network$ftype == "Coastal")
    }else{
      print("No upnetwork streams found in the NHDPlus")
      downstream_of_gl <- TRUE
    }
  }else{
    downstream_of_gl <- FALSE
  }
  
  if(!all_coastal & !downstream_of_gl){
    FALSE # not bad lake
  }else{
    TRUE # bad lake
  }
}

is_bad_lake(llid)

test2 <- sapply(test$lagoslakeid[1:5], function(x){
  print(x) 
  is_bad_lake(x)
}) 

# get lakes that only contain flowlines with a "Coastal" FTYPE
# get flowlines that intersect the boundary of the Michigan landmass 
