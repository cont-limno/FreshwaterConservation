# 6 boxplots for desired variable GAPS 1-2 and 1-3
six_boxplot <- function(dataframe, yvar, ylimits){
  #dataframe: dataframe containing PADUS and other data to plot, with % protected cutoffs of 75, 90 and 100
  #yvar: y axis variable (for which comparing protected vs. unprotected)
  #ylimits: y axis limits for easy manual adjustment for all plots
  par(mfrow=c(2,3))
  melt_colnames <- c('level','variable','value')
  
  # GAPS 1-2 (strict protection)
  test <- melt(dataframe, id.vars='ProtectGAP12Cat_75',measure.vars=yvar,na.rm=T)
  colnames(test) <- melt_colnames
  test$level <- as.factor(test[,1])
  test2 <- melt(dataframe, id.vars='ProtectGAP12Cat_90',measure.vars=yvar,na.rm=T)
  colnames(test2) <- melt_colnames
  test2$level <- as.factor(test2[,1])
  test3 <- melt(dataframe, id.vars='ProtectGAP12Cat_100',measure.vars=yvar,na.rm=T)
  colnames(test3) <- melt_colnames
  test3$level <- as.factor(test3[,1])
  
  # GAPS 1-3 (strict protection + multi-use landscapes)
  test4 <- melt(dataframe, id.vars='ProtectGAP123Cat_75',measure.vars=yvar,na.rm=T)
  colnames(test4) <- melt_colnames
  test4$level <- as.factor(test4[,1])
  test5 <- melt(dataframe, id.vars='ProtectGAP123Cat_90',measure.vars=yvar,na.rm=T)
  colnames(test5) <- melt_colnames
  test5$level <- as.factor(test5[,1])
  test6 <- melt(dataframe, id.vars='ProtectGAP123Cat_100',measure.vars=yvar,na.rm=T)
  colnames(test6) <- melt_colnames
  test6$level <- as.factor(test6[,1])
  
  # plotting
  xlabz <- c('Protected','Unprotected')
  titlez <- c('Threshold: 75%', 'Threshold: 90%', 'Threshold: 100%')
  boxplot(value ~ level, data=test, las=1, col=c('green','gray'), ylim=ylimits, names=xlabz, outline=T, main=titlez[1], ylab=yvar, xlab='GAPS 1-2')
  boxplot(value ~ level, data=test2, las=1, col=c('green','gray'), ylim=ylimits, names=xlabz, outline=T, main=titlez[2], xlab='GAPS 1-2')
  boxplot(value ~ level, data=test3, las=1, col=c('green','gray'), ylim=ylimits, names=xlabz, outline=T, main=titlez[3], xlab='GAPS 1-2')
  boxplot(value ~ level, data=test4, las=1, col=c('dodgerblue','gray'), ylim=ylimits, names=xlabz, outline=T, main=titlez[1], ylab=yvar, xlab='GAPS 1-3')
  boxplot(value ~ level, data=test5, las=1, col=c('dodgerblue','gray'), ylim=ylimits, names=xlabz, outline=T, main=titlez[2], xlab='GAPS 1-3')
  boxplot(value ~ level, data=test6, las=1, col=c('dodgerblue','gray'), ylim=ylimits, names=xlabz, outline=T, main=titlez[3], xlab='GAPS 1-3')
  
  # clear crap
  test=NULL
  test2=NULL
  test3=NULL
  test4=NULL
  test5=NULL
  test6=NULL
  xlabz=NULL
  titlez=NULL
  melt_colnames=NULL
}
