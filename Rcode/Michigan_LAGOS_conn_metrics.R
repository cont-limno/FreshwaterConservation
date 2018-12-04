####################### Extract/wrangle LAGOS freshwater connectivity metrics ##################
# Date: 12-4-18
# updated:
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(LAGOSNE)

#### Input data ####
dt <- lagosne_load(version = '1.087.1') #returns list of data.frame objects

setwd('C:/Users/FWL/Documents/FreshwaterConservation')

################ Main program #####################
# subset of michigan lakes
mich <- subset(dt$locus, state_zoneid=="State_3")
mich_lagoslakeids <- unique(mich$lagoslakeid)

### IWS conn variables
# create string of variables to keep
iws_conn_var <- c('lagoslakeid','iws_lakes_overlapping_area_pct','iws_streamdensity_streams_density_mperha',
                  'iws_wl_allwetlandsdissolved_overlapping_area_pct','iws_wl_connectedwetlandsundissolved_overlapping_area_pct')
iws_conn_df <- dt$iws.conn[,iws_conn_var] #subsetting
iws_conn_df <- subset(iws_conn_df, lagoslakeid %in% mich_lagoslakeids) #keep only michigan lakes

### 500m buffer conn variables
buff500_conn_var <- c('lagoslakeid','buffer500m_streamdensity_streams_density_mperha')
buff500_conn_df <- dt$buffer500m.conn[,buff500_conn_var]
buff500_conn_df <- subset(buff500_conn_df, lagoslakeid %in% mich_lagoslakeids)

### percent of shoreline adjacent to wetlands
perim_df <- data.frame(lagoslakeid=dt$locus$lagoslakeid, perimeter_m=dt$locus$lake_perim_meters)
perim_df <- subset(perim_df, lagoslakeid %in% mich_lagoslakeids)
shoreline_wetlands_df <- data.frame(lagoslakeid=dt$lakes.geo$lagoslakeid, shoreline_wetlands_km=dt$lakes.geo$wlconnections_allwetlands_shoreline_km)
shoreline_wetlands_df <- subset(shoreline_wetlands_df, lagoslakeid %in% mich_lagoslakeids)
shoreline_wetlands_df <- merge(shoreline_wetlands_df, perim_df, by='lagoslakeid')
shoreline_wetlands_df$shoreline_wetlands_pct <- (shoreline_wetlands_df$shoreline_wetlands_km*1000)/shoreline_wetlands_df$perimeter_m

##### create overall merged table #####
big_mama_df <- merge(iws_conn_df, buff500_conn_df, by='lagoslakeid')
big_mama_df <- merge(big_mama_df, shoreline_wetlands_df[,c('lagoslakeid','shoreline_wetlands_pct')], by='lagoslakeid')
#write.csv(big_mama_df, "Data/Michigan_LAGOS_conn_metrics.csv")

#### basic plots
par(mfrow=c(2,3))
hist(big_mama_df$iws_lakes_overlapping_area_pct, main='IWS lakes', xlab='')
hist(big_mama_df$iws_streamdensity_streams_density_mperha, main='IWS stream density', xlab='')
hist(big_mama_df$iws_wl_allwetlandsdissolved_overlapping_area_pct, main='IWS wetland pct', xlab='')
hist(big_mama_df$iws_wl_connectedwetlandsundissolved_overlapping_area_pct, main='IWS connected wetlands pct', xlab='')
hist(big_mama_df$buffer500m_streamdensity_streams_density_mperha, main='Buff 500m stream density', xlab='')
hist(big_mama_df$shoreline_wetlands_pct, main='Shoreline wetland %', xlab='')
