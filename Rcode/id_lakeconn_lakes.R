# Identify lakes for stream network analysis
#
# Lakes are potentially disqualified if they:
#   * intersect the Great Lakes
#   * have no entry for NHDPlus in the x_walk table
#   * are Isolated
# 
#   * are not connected to NHDPlus flowlines
#   * are a Headwater lake 
#   * are on the Great Lakes coast
#   * are downstream of the Great Lakes

library(LAGOSNEgis)
library(nhdR)
library(sf)
library(dplyr)
library(units)
library(ggplot2) # optional
library(mapview) # optional

# ---- load_gis_data ----

# st_layers(lagosnegis_path())
# gdalUtils::ogrinfo(lagosnegis_path(), "LAGOS_NE_All_Lakes_4ha", so = TRUE)
# gdalUtils::ogrinfo(lagosnegis_path(), "Stream_Polylines", so = TRUE)

mich_shp       <- query_gis("STATE", "State_Name", "Michigan")
mich_lakes_4ha <- query_gis_(query = "SELECT * FROM LAGOS_NE_All_Lakes_4ha WHERE STATE LIKE 'MI'")
gl             <- great_lakes(spatial = TRUE)

# ---- find_obviously_bad_lakes ----
# remove lakes that intersect the Great Lakes
# llid <- 2639
# llid <- 2197

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

# ---- map_obviously_bad_lakes ----

lake_points <- query_gis("LAGOS_NE_All_Lakes_4ha_POINTS", 
                              "lagoslakeid", 
                              mich_lakes_4ha$lagoslakeid)
lake_points <- lake_points %>%
  left_join(
    dplyr::select(
      st_drop_geometry(bad_lakes), "lagoslakeid", "reason"), 
    by = "lagoslakeid")

ggplot() + 
  geom_sf(data = mich_shp) +
  geom_sf(data = 
            dplyr::filter(lake_points, !is.na(reason)), 
          aes(color = reason), size = 1) +
  ggtitle(paste0("Total of ", 
                 nrow(dplyr::filter(lake_points, is.na(reason))), 
          " 'good' lakes"))
ggsave("Figures/bad_lakes.png")

# ---- find_more_subtle_bad_lakes -----
test <- dplyr::filter(mich_lakes_4ha, 
                      !(lagoslakeid %in% bad_lakes$lagoslakeid))

llid <- test$lagoslakeid[5]
llid <- 69665
llid <- 2339
llid <- lake_info(name = "Duck Lake", state = "Michigan")$lagoslakeid

is_bad_lake <- function(llid){
  reason <- NA
  is_bad <- FALSE
  
  coords      <- lake_info(lagoslakeid = llid)
  focal_poly  <- dplyr::filter(mich_lakes_4ha, lagoslakeid == llid)
  
  # get intersecting flowlines, bad lake if they are all FTYPE coastal
  focal_lines <- nhdR::nhd_plus_query(poly = focal_poly,
                                    dsn = "NHDFlowLine")$sp$NHDFlowLine
  if(nrow(focal_lines) > 0){
    all_coastal <- all(focal_lines$FTYPE == "Coastline")
  }else{
    all_coastal <- FALSE
    is_bad      <- TRUE
    reason      <- "not connected to nhdp streams"
  }
  
  if(all_coastal){
    is_bad <- TRUE
    reason <- "on GL coast"
  }
  
  if(nrow(focal_lines) > 0){
    up_network <- nhdR::extract_network(
      lon = coords$nhd_long, 
      lat = coords$nhd_lat)
    if(is.null(nrow(up_network))){
        # set FALSE for now bc this might not mean its a bad lake
        is_bad <- FALSE
        reason <- "no upnetwork streams"
    }else{
      if(any(up_network$ftype == "Coastal")){
        is_bad <- TRUE
        reason <- "downstream of GL"
      }
    }
  }
    
  data.frame("llid" = llid, "is_bad" = is_bad, "reason" = reason, 
             stringsAsFactors = FALSE)
  
}

# is_bad_lake(llid)
# is_bad_lake(2339)

sapply(test$lagoslakeid, function(x){
  print(x) 
  res <- suppressMessages(suppressWarnings(
            is_bad_lake(x)))
  if(x == test$lagoslakeid[1]){
    write.table(res, file = "Data/connectivity_lakes.csv", append = TRUE, 
                sep = ",", row.names = FALSE, col.names = TRUE)
  }else{
    write.table(res, file = "Data/connectivity_lakes.csv", append = TRUE, 
                sep = ",", row.names = FALSE, col.names = FALSE)  
  }
}) 

res <- read.csv("Data/connectivity_lakes.csv", stringsAsFactors = FALSE)
# unlink("Data/connectivity_lakes.csv")