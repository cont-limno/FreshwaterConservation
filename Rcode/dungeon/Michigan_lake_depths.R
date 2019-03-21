library(LAGOSNE)
library(raster)
library(ggplot2)

dt <- lagosne_load(version = '1.087.1') #returns list of data.frame objects

# subset of michigan lakes
mich <- subset(dt$locus, state_zoneid=="State_3")
mich_lagoslakeids <- unique(mich$lagoslakeid)

mich_depth <- subset(dt$lakes_limno, lagoslakeid %in% mich_lagoslakeids)
mich_depth <- subset(mich_depth, maxdepth >= 0)

# Sam Oliver depth
oliver_depth <- lagos_load_oliver_2015()
oliver_depth_mich <- subset(oliver_depth, lagoslakeid %in% mich_lagoslakeids)

# plotz
par(mfrow=c(3,1))
breaks=seq(0,90,2)
hist(mich_depth$maxdepth, main='Michigan LAGOSNE', xlim=c(0,40), xlab='', breaks=breaks)
hist(oliver_depth_mich$zmaxobs, main='Oliver Michigan observed', xlim=c(0,40), xlab='', breaks=breaks)
hist(oliver_depth_mich$zmaxpredict, main='Oliver Michigan predicted', xlim=c(0,40), xlab='', breaks=breaks)

# Michigan shapefile
mich_shp <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/STATE/Michigan.shp")  
par(mfrow=c(1,1))
plot(mich_shp)

# lake points
lakes_4ha_pts <- shapefile("C:/Ian_GIS/LAGOS-NE-GISv1.0/LAGOS_NE_All_Lakes_4ha_POINTS/LAGOS_NE_All_Lakes_4ha_POINTS.shp")

# merge lake depths to lake points
lakes_4ha_pts_depth <- merge(lakes_4ha_pts, mich_depth, by.x='lagoslakei', by.y='lagoslakeid', all.x=F)
plot(lakes_4ha_pts_depth, add=T, col='dodgerblue', pch=20)

dmaps_map_df <- as.data.frame(lakes_4ha_pts_depth@data)
dmaps_map_df$xCor <- lakes_4ha_pts_depth@coords[,1]
dmaps_map_df$yCor <- lakes_4ha_pts_depth@coords[,2]

# this ggplot is imperfect, but it gets the job done for exploration
dmap.point3<-ggplot(dmaps_map_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=dmaps_map_df$maxdepth), size=2) +
  ggtitle('Maxdepth')
dmap.point3$labels$colour = 'Maxdepth' # change legend title
dmap.point3 + geom_path(data=mich_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())

lakes_4ha_pts_depth_shallow <- subset(lakes_4ha_pts_depth, maxdepth <= 10)

dmaps_map_df2 <- as.data.frame(lakes_4ha_pts_depth_shallow@data)
dmaps_map_df2$xCor <- lakes_4ha_pts_depth_shallow@coords[,1]
dmaps_map_df2$yCor <- lakes_4ha_pts_depth_shallow@coords[,2]

# this ggplot is imperfect, but it gets the job done for exploration
dmap.point4<-ggplot(dmaps_map_df2, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=dmaps_map_df2$maxdepth), size=2) +
  ggtitle('Maxdepth')
dmap.point4$labels$colour = 'Maxdepth' # change legend title
dmap.point4 + geom_path(data=mich_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())
