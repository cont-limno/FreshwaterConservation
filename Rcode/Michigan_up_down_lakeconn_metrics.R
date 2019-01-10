############### Hydrologic lake connectivity indices not available in LAGOS ###########################
# Date started: 8-Nov-18 
# updated: 7-Jan-19
# Author: Katelyn King
################################################################################################

#### R libraries ####
library(dplyr)
library(hydrolinks)
library(sf)
library(mapview)

#### input data ####
setwd("C:/Users/FWL/Documents/FreshwaterConservation")
#Note: eventually use MI LAGOS lakes >4ha 

#set a path for data from hydrolinks package #note don't commit files to github
cache_set_dir(path = 'Data/')

####### Main program: calc downstream and upstream lake areas, area of closest lake, distance to closest lake ###### 

#get shapefiles from nhd high-res by using the permanent identifiers from LAGOS 
#start by testing Duck Lake connected to Lake Superior
test = get_shape_by_id(123397648, dataset = 'nhdh', feature_type = 'waterbody')

#set up loop to be used for all lakes eventually 
output <- data.frame(permanent_=NA, up_lakes_area=NA, down_lakes_area=NA, closest_lake_area=NA, stream_distance_lake=NA)            

for (permanent_ in test) {
  # traverse upstream
  upstream = traverse_flowlines(Inf, test$permanent_, direction = 'in')
  upstream_lakes = get_shape_by_id(upstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
  
  if(!is.na(upstream_lakes)) { #if there are upstream lakes 
    up_lakes_sum = sum(upstream_lakes$areasqkm) #return total upstream lake area (sqkm)
    conn_lakes<-as.data.frame(upstream_lakes)
    lakes<-dplyr::left_join(conn_lakes, upstream, by = "permanent_")
    lakes$LENGTHnum<-as.numeric(lakes$LENGTHKM)  #need numeric to find closest lake
    up_stream_dist_lake = lakes[which.min(lakes$LENGTHnum),19] # stream distance to the closest lake
    up_closest_lake_area=lakes[which.min(lakes$LENGTHnum),6]  #lake areasqkm of closest lake
  }   
  else{ #if no upstream lakes, write 0
    up_lakes_sum    = 0
    up_stream_dist_lake = 0
    up_closest_lake_area  = 0
  }
  
  #traverse downstream 
  downstream = traverse_flowlines(Inf, test$permanent_, direction = 'out')
  ### CROP OUT GREAT LAKES 
  downstream_streams = get_shape_by_id(downstream$permanent_, dataset = 'nhdh', feature_type = 'flowline')
  downstream_lakes = get_shape_by_id(downstream$permanent_, dataset='nhdh', feature_type = 'waterbody')
  GL_streams<-dplyr::filter(downstream_streams, fcode == 56600 ) #coastline fcode
  GL_lakes<-dplyr::filter(downstream_lakes, fcode==39004 | fcode==39010 | fcode==39000) #Lake Superior, Erie, Huron, Ontario  fcodes
  network<-dplyr::anti_join(downstream, GL_streams, by="permanent_") %>%
    dplyr::anti_join(GL_lakes, by="permanent_")
  network2<-dplyr::filter(network, !grepl("{", permanent_, fixed = TRUE)) #get rid of other polylines in GL
  network2$LENGTHnum<-as.numeric(network2$LENGTHKM) #need numeric to find distances from focal lake 
  
  #traverse back upstream from the terminal reach. This will get all of the water bodies  
  return_network = traverse_flowlines(Inf, network2[which.max(network2$LENGTHnum),1], direction='in',  max_steps = 100000)
  connected_lakes = get_shape_by_id(return_network$permanent_, dataset='nhdh', feature_type = 'waterbody')
  
  
#need to figure out how to get closest lake downstream 
  #stream_distance_lake <- which is closer up_stream_dist_lake or down_stream_dist_lake
  #closest_lake_area<-  area if the closest lake 
  
  down_lakes_sum <- sum(connected_lakes$areasqkm) - up_lakes_sum - test$areasqkm #return downstream lake area (sqkm)= all lakes - upstream lakes - the focal lake area
  
  output[,1]=test$permanent_
  output[,2]=up_lakes_sum
  output[,3]=down_lakes_sum
  output[,4]=closest_lake_area
  output[,5]=stream_distance_lake 
} 

mapview(connected_lakes)

