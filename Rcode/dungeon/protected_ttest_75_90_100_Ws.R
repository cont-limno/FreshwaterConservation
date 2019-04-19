# function to perform t tests at network watershed protection thresholds of 75,90 and 100% (to be considered protected)
protected_ttest_75_90_100_Ws <- function(dataframe, yvar){
  #dataframe: dataframe containing PADUS and other data to plot, with % protected cutoffs of 75, 90 and 100
  #yvar: y axis variable (for which comparing protected vs. unprotected)
  # with help from: https://www.r-bloggers.com/two-sample-students-t-test-2/
  # 75% threshold for counting as protected
  pro <- subset(dataframe, ProtectGAP12Ws_75=='Protected75')[,yvar]
  unpro <- subset(dataframe, ProtectGAP12Ws_75=='Unprotected75')[,yvar]
  var1 <- var.test(pro,unpro) #if p > 0.05, have homoskedasticity
  
  if (var1$p.value > 0.05) {
    T1 <- t.test(pro,unpro, var.equal=F, paired=F)# perform's Welch's test with unequal variances and unpaired data
    T1_p <- T1$p.value
    T1_est <- T1$estimate
    T1_stat <- T1$statistic
    
  } else {
    T1 <- t.test(pro,unpro, var.equal=T, paired=F) #standard t test with equal variances
    T1_p <- T1$p.value
    T1_est <- T1$estimate
    T1_stat <- T1$statistic
  }
  result1 <- c(T1_est[1], T1_est[2], T1_stat, T1_p)
  
  # 90% threshold for counting as protected
  pro <- subset(dataframe, ProtectGAP12Ws_90=='Protected90')[,yvar]
  unpro <- subset(dataframe, ProtectGAP12Ws_90=='Unprotected90')[,yvar]
  var2 <- var.test(pro,unpro) #if p > 0.05, have homoskedasticity
  
  if (var2$p.value > 0.05) {
    T2 <- t.test(pro,unpro, var.equal=F, paired=F)# perform's Welch's test with unequal variances and unpaired data
    T2_p <- T2$p.value
    T2_est <- T2$estimate
    T2_stat <- T2$statistic
    
  } else {
    T2 <- t.test(pro,unpro, var.equal=T, paired=F) #standard t test with equal variances
    T2_p <- T2$p.value
    T2_est <- T2$estimate
    T2_stat <- T2$statistic
  }
  result2 <- c(T2_est[1], T2_est[2], T2_stat, T2_p)
  
  # 100% threshold for counting as protected
  pro <- subset(dataframe, ProtectGAP12Ws_100=='Protected100')[,yvar]
  unpro <- subset(dataframe, ProtectGAP12Ws_100=='Unprotected100')[,yvar]
  var3 <- var.test(pro,unpro) #if p > 0.05, have homoskedasticity
  if (var3$p.value > 0.05) {
    T3 <- t.test(pro,unpro, var.equal=F, paired=F)# perform's Welch's test with unequal variances and unpaired data
    T3_p <- T3$p.value
    T3_est <- T3$estimate
    T3_stat <- T3$statistic
    
  } else {
    T3 <- t.test(pro,unpro, var.equal=T, paired=F) #standard t test with equal variances
    T3_p <- T3$p.value
    T3_est <- T3$estimate
    T3_stat <- T3$statistic
  }
  result3 <- c(T3_est[1], T3_est[2], T3_stat, T3_p)
  
  ##### same analysis for GAPS 1-3
  # 75% threshold for counting as protected
  pro <- subset(dataframe, ProtectGAP123Ws_75=='Protected75')[,yvar]
  unpro <- subset(dataframe, ProtectGAP123Ws_75=='Unprotected75')[,yvar]
  var1 <- var.test(pro,unpro) #if p > 0.05, have homoskedasticity
  
  if (var1$p.value > 0.05) {
    T4 <- t.test(pro,unpro, var.equal=F, paired=F)# perform's Welch's test with unequal variances and unpaired data
    T4_p <- T4$p.value
    T4_est <- T4$estimate
    T4_stat <- T4$statistic
    
  } else {
    T4 <- t.test(pro,unpro, var.equal=T, paired=F) #standard t test with equal variances
    T4_p <- T4$p.value
    T4_est <- T4$estimate
    T4_stat <- T4$statistic
  }
  result4 <- c(T4_est[1], T4_est[2], T4_stat, T4_p)
  
  # 90% threshold for counting as protected
  pro <- subset(dataframe, ProtectGAP123Ws_90=='Protected90')[,yvar]
  unpro <- subset(dataframe, ProtectGAP123Ws_90=='Unprotected90')[,yvar]
  var2 <- var.test(pro,unpro) #if p > 0.05, have homoskedasticity
  
  if (var2$p.value > 0.05) {
    T5 <- t.test(pro,unpro, var.equal=F, paired=F)# perform's Welch's test with unequal variances and unpaired data
    T5_p <- T5$p.value
    T5_est <- T5$estimate
    T5_stat <- T5$statistic
    
  } else {
    T5 <- t.test(pro,unpro, var.equal=T, paired=F) #standard t test with equal variances
    T5_p <- T5$p.value
    T5_est <- T5$estimate
    T5_stat <- T5$statistic
  }
  result5 <- c(T5_est[1], T5_est[2], T5_stat, T5_p)
  
  # 100% threshold for counting as protected
  pro <- subset(dataframe, ProtectGAP123Ws_100=='Protected100')[,yvar]
  unpro <- subset(dataframe, ProtectGAP123Ws_100=='Unprotected100')[,yvar]
  var3 <- var.test(pro,unpro) #if p > 0.05, have homoskedasticity
  if (var3$p.value > 0.05) {
    T6 <- t.test(pro,unpro, var.equal=F, paired=F)# perform's Welch's test with unequal variances and unpaired data
    T6_p <- T6$p.value
    T6_est <- T6$estimate
    T6_stat <- T6$statistic
    
  } else {
    T6 <- t.test(pro,unpro, var.equal=T, paired=F) #standard t test with equal variances
    T6_p <- T6$p.value
    T6_est <- T6$estimate
    T6_stat <- T6$statistic
  }
  result6 <- c(T6_est[1], T6_est[2], T6_stat, T6_p)
  
  # generate output to return based on results above from 3 t-tests
  outputz <- data.frame(pro_est=NA, unpro_est=NA, t_stat=NA, t_pval=NA)
  outputz[1,] <- result1
  outputz[2,] <- result2
  outputz[3,] <- result3
  outputz[4,] <- result4
  outputz[5,] <- result5
  outputz[6,] <- result6
  rownames(outputz) <- c('GAPS12_75pct','GAPS12_90pct','GAPS12_100pct','GAPS123_75pct','GAPS123_90pct','GAPS123_100pct')
  outputz$var <- yvar #add in variable name for reference
  outputz$var_p <- c(var1$p.value, var2$p.value, var3$p.value)
  outputz$equal_var <- ifelse(outputz$var_p < 0.05, 'FALSE', 'TRUE')
  outputz$t_test <- ifelse(outputz$var_p < 0.05, 'Welchs', 'Standard')
  return(outputz)
}