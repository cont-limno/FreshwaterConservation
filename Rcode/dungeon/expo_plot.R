# exploratory scatter plot with correlation coefficient (used for percent catchment protected vs. some variable)
expo_plot <- function(xvar, yvar, dataframe, min_protected){
  #xvar: explanatory variable
  #yvar: protected status variable
  #dataframe: dataframe containing data
  #min_protected: keep data points with percent protected above this value
  data_sub <- dataframe[dataframe[,yvar] > min_protected, ]
  plot(data_sub[,yvar] ~ data_sub[,xvar], data=data_sub, pch=20, xlab=xvar, ylab=yvar)
  plot_cor <- round(cor(data_sub[,yvar], data_sub[,xvar], use='pairwise.complete.obs'),3)
  plot_lm <- lm(data_sub[,yvar] ~ data_sub[,xvar], data=data_sub)
  plot_p <- round(anova(plot_lm)$'Pr(>F)'[1],3) 
  mtext(side=3, paste0("r = ", plot_cor, ', p = ', plot_p))
  plot_cor = NULL
  data_sub = NULL
  plot_p <- NULL
  plot_lm <- NULL
}