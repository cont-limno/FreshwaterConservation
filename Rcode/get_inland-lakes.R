# get inland lakes in Michigan not upstream of or directly connected to the 
# Great Lakes

library(LAGOSNEgis)
library(sf)
library(dplyr)

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

# Houghton Lake is the largest MI inland lake
# use it to generate a polyline selection buffer

# Betsie Lake (llid 2639) should be excluded
lake_info(name = "Betsie Lake", state = "Michigan")

is_bad_lake <- function(llid){
  llid <- 2639
  
  # skip if llid not in mich_lakes_4ha
  # any(mich_lakes_4ha$lagoslakeid %in% llid)
  
  focal_poly <- dplyr::filter(mich_lakes_4ha, lagoslakeid == llid)
  
  # get intersecting flowlines
  # are they all FTYPE coastal?
  
  # walk upstream
  # do we get FTYPE coastal reaches?
  
}

#Check out the following possible bad lakes:
# lagoslakeid != 2227,
# lagoslakeid != 2197,
#lagoslakeid != 4930

# get lakes that only contain flowlines with a "Coastal" FTYPE
# get flowlines that intersect the boundary of the Michigan landmass 
