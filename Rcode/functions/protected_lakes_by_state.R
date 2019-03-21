### Custom function to calculate protected lakes in each US state (lower 48)

protected_lakes_by_state <- function(NHD_pts_lakes_PADUS, lower48){
  #NHD_pts_lakes_PADUS: NHD lake centroids, merged with PADUS data
  #lower48: lower48 state polygons, same crs as NHD
  lower48_names <- lower48@data$NAME
  # data fraome of lake IDs
  NHD_pts_lakes_PADUS@data$rowID <- rownames(NHD_pts_lakes_PADUS@data)
  rowid_NHD_df <- data.frame(rowID=NHD_pts_lakes_PADUS@data$rowID, COMID=NHD_pts_lakes_PADUS@data$COMID)
  # number of lakes per state
  # subset points that fall in each state polygon
  # sp::over doesn't retain attribute data from points, so create data frame to join those data back later based on rowid
  lower48_COMID <- sp::over(NHD_pts_lakes_PADUS, lower48, returnList = F)
  lower48_COMID$joinID <- rownames(lower48_COMID)
  lower48_COMID <- merge(lower48_COMID, rowid_NHD_df, by.x='joinID', by.y='rowID')
  
  # get rid of factor; would cause problems later
  lower48_COMID$COMID <- as.numeric(levels(lower48_COMID$COMID))[lower48_COMID$COMID]
  
  # define protected lakes based on % Ws and Cat protected
  protected_lakes_gap12_ctr <- subset(NHD_pts_lakes_PADUS, COMID %in% protected_GAPS12_COMIDs_ctr)
  protected_lakes_gap3_ctr <- subset(NHD_pts_lakes_PADUS, COMID %in% protected_GAP3only_COMIDS_ctr)
  protected_lakes_gap12_Cat100 <- subset(NHD_pts_lakes_PADUS, COMID %in% protected_GAPS12_COMIDs_100)
  protected_lakes_gap3_Cat100 <- subset(NHD_pts_lakes_PADUS, COMID %in% protected_GAP3only_COMIDs_100)
  unprotected_lakes <- subset(NHD_pts_lakes_PADUS, COMID %in% unprotected_COMIDs)
  
  # number of protected lakes by state
  lower48_lakes_bystate_gap12_ctr <- colSums(gContains(lower48, protected_lakes_gap12_ctr, byid = T))
  lower48_lakes_bystate_gap3_ctr <- colSums(gContains(lower48, protected_lakes_gap3_ctr, byid = T))
  lower48_lakes_bystate_gap12_Cat100 <- colSums(gContains(lower48, protected_lakes_gap12_Cat100, byid = T))
  lower48_lakes_bystate_gap3_Cat100 <- colSums(gContains(lower48, protected_lakes_gap3_Cat100, byid = T))
  lower48_lakes_bystate_unprotected <- colSums(gContains(lower48, unprotected_lakes, byid=T))
  #setNames(lower48_lakes_bystate, lower48_names)
  lower48_protected_lakes_DF <- data.frame(State=lower48_names, ProtectedLakes_gap12_ctr=lower48_lakes_bystate_gap12_ctr,
                                           ProtectedLakes_gap3_ctr=lower48_lakes_bystate_gap3_ctr,
                                           ProtectedLakes_gap12_Cat100=lower48_lakes_bystate_gap12_Cat100,
                                           ProtectedLakes_gap3_Cat100=lower48_lakes_bystate_gap3_Cat100,
                                           unprotected_lakes=lower48_lakes_bystate_unprotected)
  
  # proportion of protected lakes by state (out of total lakes in each state)
  # count number of rows (COMIDs, therefore lakes) per unique state
  lake_countz_state <- lower48_COMID %>%
    group_by(NAME) %>%
    tally()
  colnames(lake_countz_state) <- c("State","nLakes")
  lake_countz_protected_lower48 <- merge(lake_countz_state, lower48_protected_lakes_DF, by="State", all.x=F)
  lake_countz_protected_lower48$PropProtected_gap12_ctr <- lake_countz_protected_lower48$ProtectedLakes_gap12_ctr/lake_countz_protected_lower48$nLakes
  lake_countz_protected_lower48$PropProtected_gap3_ctr <- lake_countz_protected_lower48$ProtectedLakes_gap3_ctr/lake_countz_protected_lower48$nLakes
  lake_countz_protected_lower48$PropProtected_gap12_Cat100 <- lake_countz_protected_lower48$ProtectedLakes_gap12_Cat100/lake_countz_protected_lower48$nLakes
  lake_countz_protected_lower48$PropProtected_gap3_Cat100 <- lake_countz_protected_lower48$ProtectedLakes_gap3_Cat100/lake_countz_protected_lower48$nLakes
  lake_countz_protected_lower48$PropUnprotected <- lake_countz_protected_lower48$unprotected_lakes/lake_countz_protected_lower48$nLakes
  # clean up for next iteration
  protected_lakes_gap12_ctr <- NULL
  protected_lakes_gap3_ctr <- NULL
  protected_lakes_gap12_Cat100 <- NULL
  protected_lakes_gap3_Cat100 <- NULL
  unprotected_lakes <- NULL
  lake_countz_state <- NULL
  lower48_protected_lakes_DF <- NULL
  lower48_lakes_bystate_gap12_ctr <- NULL
  lower48_lakes_bystate_gap3_ctr <- NULL
  lower48_lakes_bystate_gap12_Cat100 <- NULL
  lower48_lakes_bystate_gap3_Cat100 <- NULL
  lower48_lakes_bystate_unprotected <- NULL
  lower48_COMID <- NULL
  rowid_NHD_df <- NULL
  lower48_names <- NULL
  rowid_NHD_df <- NULL
  return(lake_countz_protected_lower48)
}
