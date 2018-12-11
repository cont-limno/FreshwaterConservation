## this function contrasts protected vs. unprotected lakes
# depends on input data frame with prior manipulation and some calculations
# the 100 means 100% watershed or catchment protected to qualify as a protected lake
# another cutoff could be used by changing the 100s in the function

protected_unprotected_percentiles100 <- function(dataframe, var_name, cat_var_name, ws_var_name, percentiles, round_digits=2){
  #dataframe: pre-joined data frame containing PADUS and LakeCat data
  #var_name: generic variable name that functions similarly to a rowname/data label, in quotes
  #cat_var_name: catchment variable name, must match column name in dataframe, in quotes
  #ws_var_name: watershed variable name, must match column name in dataframe, in quotes
  #percentiles: length=3 string of percentiles to calculate using quantile function
  #round_digits: number of decimals to round off output
  output <- data.frame(Variable=c(var_name), Cat_GAP12=NA, Ws_GAP12=NA, Cat_GAP123=NA, Ws_GAP123=NA)
  # Catchment, GAPS1-2
  pro1 <- subset(dataframe, ProtectGAP12Cat_100 =='Protected100')
  unpro1 <- subset(dataframe, ProtectGAP12Cat_100 =='Unprotected100')
  pro1 <- round(quantile(pro1[,cat_var_name], percentiles, na.rm=T), digits=round_digits)  
  unpro1 <- round(quantile(unpro1[,cat_var_name], percentiles, na.rm=T), digits=round_digits)
  output[1,2] <- paste0('(',pro1[1],', ',pro1[2],', ',pro1[3],')',', (',unpro1[1],', ',unpro1[2],', ',unpro1[3],')')
  
  # Watershed, GAPS1-2
  pro2 <- subset(dataframe, ProtectGAP12Ws_100 =='Protected100')
  unpro2 <- subset(dataframe, ProtectGAP12Ws_100 =='Unprotected100')
  pro2 <- round(quantile(pro2[,ws_var_name], percentiles, na.rm=T), digits=round_digits)  
  unpro2 <- round(quantile(unpro2[,ws_var_name], percentiles, na.rm=T), digits=round_digits)
  output[1,3] <- paste0('(',pro2[1],', ',pro2[2],', ',pro2[3],')',', (',unpro2[1],', ',unpro2[2],', ',unpro2[3],')')
  
  # Catchment, GAPS1-3
  pro3 <- subset(dataframe, ProtectGAP123Cat_100 =='Protected100')
  unpro3 <- subset(dataframe, ProtectGAP123Cat_100 =='Unprotected100')
  pro3 <- round(quantile(pro3[,cat_var_name], percentiles, na.rm=T), digits=round_digits)  
  unpro3 <- round(quantile(unpro3[,cat_var_name], percentiles, na.rm=T), digits=round_digits)
  output[1,4] <- paste0('(',pro3[1],', ',pro3[2],', ',pro3[3],')',', (',unpro3[1],', ',unpro3[2],', ',unpro3[3],')')
  
  # Watershed, GAPS1-2
  pro4 <- subset(dataframe, ProtectGAP123Ws_100 =='Protected100')
  unpro4 <- subset(dataframe, ProtectGAP123Ws_100 =='Unprotected100')
  pro4 <- round(quantile(pro4[,ws_var_name], percentiles, na.rm=T), digits=round_digits)  
  unpro4 <- round(quantile(unpro4[,ws_var_name], percentiles, na.rm=T), digits=round_digits)
  output[1,5] <- paste0('(',pro4[1],', ',pro4[2],', ',pro4[3],')',', (',unpro4[1],', ',unpro4[2],', ',unpro4[3],')')
  
  #cleanup
  pro1 <- NULL
  unpro1 <- NULL
  pro2 <- NULL
  unpro2 <- NULL
  pro3 <- NULL
  unpro3 <- NULL
  pro4 <- NULL
  unpro4 <- NULL
  
  return(output)
}