### Custom function to calculate protected lakes in each NARS ecoregion

protected_lakes_by_NARS_80pct <- function(NHD_pts_lakes_PADUS, NARS_regions){
  #NHD_pts_lakes_PADUS: NHD lake centroids, merged with PADUS data
  #NARS_regions: NARS ecoregions polygons, same crs as NHD
  NARS_names <- NARS_regions@data$WSA9
  # data fraome of lake IDs
  NHD_pts_lakes_PADUS@data$rowID <- rownames(NHD_pts_lakes_PADUS@data)
  rowid_NHD_df <- data.frame(rowID=NHD_pts_lakes_PADUS@data$rowID, COMID=NHD_pts_lakes_PADUS@data$COMID)
  # number of lakes per state
  # subset points that fall in each state polygon
  # sp::over doesn't retain attribute data from points, so create data frame to join those data back later based on rowid
  NARS_COMID <- sp::over(NHD_pts_lakes_PADUS, NARS_regions, returnList = F)#warning: slow with large polygons/lots of pts
  NARS_COMID$joinID <- rownames(NARS_COMID)
  NARS_COMID <- merge(NARS_COMID, rowid_NHD_df, by.x='joinID', by.y='rowID')
  
  # get rid of factor; would cause problems later
  NARS_COMID$COMID <- as.numeric(levels(NARS_COMID$COMID))[NARS_COMID$COMID]
  
  # define protected lakes based on % Ws and Cat protected
  protected_lakes_gap12_ctr <- subset(NHD_pts_lakes_PADUS, COMID %in% protected_GAPS12_COMIDs_ctr)
  protected_lakes_gap3_ctr <- subset(NHD_pts_lakes_PADUS, COMID %in% protected_GAP3only_COMIDS_ctr)
  protected_lakes_gap12_Cat80 <- subset(NHD_pts_lakes_PADUS, COMID %in% protected_GAPS12_COMIDs_80)
  protected_lakes_gap3_Cat80 <- subset(NHD_pts_lakes_PADUS, COMID %in% protected_GAP3only_COMIDs_80)
  unprotected_lakes <- subset(NHD_pts_lakes_PADUS, COMID %in% unprotected_COMIDs)
  
  # number of protected lakes by ecoregion
  NARS_lakes_byNARS_gap12_ctr <- colSums(gContains(NARS_regions, protected_lakes_gap12_ctr, byid = T))
  NARS_lakes_byNARS_gap3_ctr <- colSums(gContains(NARS_regions, protected_lakes_gap3_ctr, byid = T))
  NARS_lakes_byNARS_gap12_Cat80 <- colSums(gContains(NARS_regions, protected_lakes_gap12_Cat80, byid = T))
  NARS_lakes_byNARS_gap3_Cat80 <- colSums(gContains(NARS_regions, protected_lakes_gap3_Cat80, byid = T))
  NARS_lakes_byNARS_unprotected <- colSums(gContains(NARS_regions, unprotected_lakes, byid=T))
  #setNames(NARS_lakes_byNARS, NARS_names)
  NARS_protected_lakes_DF <- data.frame(Ecoregion=NARS_names, ProtectedLakes_gap12_ctr=NARS_lakes_byNARS_gap12_ctr,
                                           ProtectedLakes_gap3_ctr=NARS_lakes_byNARS_gap3_ctr,
                                           ProtectedLakes_gap12_Cat80=NARS_lakes_byNARS_gap12_Cat80,
                                           ProtectedLakes_gap3_Cat80=NARS_lakes_byNARS_gap3_Cat80,
                                           unprotected_lakes=NARS_lakes_byNARS_unprotected)
  
  # proportion of protected lakes by ecoregion (out of total lakes in each ecoregion)
  # count number of rows (COMIDs, therefore lakes) per unique ecoregion
  lake_countz_NARS <- NARS_COMID %>%
    group_by(WSA9) %>%
    tally()
  colnames(lake_countz_NARS) <- c("Ecoregion","nLakes")
  lake_countz_protected_NARS <- merge(lake_countz_NARS, NARS_protected_lakes_DF, by="Ecoregion", all.x=F)
  lake_countz_protected_NARS$PropProtected_gap12_ctr <- lake_countz_protected_NARS$ProtectedLakes_gap12_ctr/lake_countz_protected_NARS$nLakes
  lake_countz_protected_NARS$PropProtected_gap3_ctr <- lake_countz_protected_NARS$ProtectedLakes_gap3_ctr/lake_countz_protected_NARS$nLakes
  lake_countz_protected_NARS$PropProtected_gap12_Cat80 <- lake_countz_protected_NARS$ProtectedLakes_gap12_Cat80/lake_countz_protected_NARS$nLakes
  lake_countz_protected_NARS$PropProtected_gap3_Cat80 <- lake_countz_protected_NARS$ProtectedLakes_gap3_Cat80/lake_countz_protected_NARS$nLakes
  lake_countz_protected_NARS$PropUnprotected <- lake_countz_protected_NARS$unprotected_lakes/lake_countz_protected_NARS$nLakes
  # clean up for next iteration
  protected_lakes_gap12_ctr <- NULL
  protected_lakes_gap3_ctr <- NULL
  protected_lakes_gap12_Cat80 <- NULL
  protected_lakes_gap3_Cat80 <- NULL
  unprotected_lakes <- NULL
  lake_countz_NARS <- NULL
  NARS_protected_lakes_DF <- NULL
  NARS_lakes_byNARS_gap12_ctr <- NULL
  NARS_lakes_byNARS_gap3_ctr <- NULL
  NARS_lakes_byNARS_gap12_Cat80 <- NULL
  NARS_lakes_byNARS_gap3_Cat80 <- NULL
  NARS_lakes_byNARS_unprotected <- NULL
  NARS_COMID <- NULL
  rowid_NHD_df <- NULL
  NARS_names <- NULL
  rowid_NHD_df <- NULL
  return(lake_countz_protected_NARS)
}
