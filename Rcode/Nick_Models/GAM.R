###gam models
library(ggplot2)
library(mgcv)
library(dplyr)

catch_dat<-read.csv("Data/Nick/catchment_table.csv", header=T)

shed_dat<-read.csv("Data/Nick/watershed_table.csv", header=T)

dat<-full_join(catch_dat[,c(2:33)], shed_dat[,c(2,11,14:33)], by="COMID")

#max depth has negative values, maybe nonsense
dat$MaxDepth[dat$MaxDepth<0]<-NA


#removing NA values from all rows. 
dat_gam<-dat[complete.cases(dat[,c(4:length(dat))]),][,4:length(dat)]

#GAM for each covariate and response, and filling in a table reporting the results
gam_results<-data.frame()
for (j in 1:6){
  for ( i in 7:50){
    
    #it appears that some predictors don't have enough unique values to run a gam model on. I'm skipping over them here, but you could create a categorical predictor as well.
    gam_model<-  tryCatch({
   gam(dat_gam[,j]~ s(scale(log(dat_gam[,i]+(min(dat_gam[,i])+1)),center = T, scale=T), k=5),family=binomial(link='logit'),data=dat_gam )
    }, error=function(e){ })
    
  if(is.null(gam_model)){  
    sum<-NA
    covariate<-names(dat_gam[i])
    response<-names(dat_gam[j])
    p<-NA
    dev<-NA
    r2<-NA
    edf<-NA
  }else{
    
    sum<-summary(gam_model)
    covariate<-names(dat_gam[i])
    response<-names(dat_gam[j])
    p<-sum$s.pv
    dev_expl<-sum$dev.expl
    r2<-sum$r.sq
    edf<-sum$edf
    
    png(filename=paste("Data/Nick/Results/GAM/",paste(response,covariate, sep="_") ,".png", sep=""))
    plot(gam_model, xlab=paste(covariate,"Scaled", sep=" "), ylab="Log Odds Not Protected", main=response)
    dev.off()
  }
    
    gam_results<-rbind(gam_results,data.frame(response=response,covariate=covariate, p=p, dev_expl=dev_expl, r2=r2, edf=edf))
    print(i)
  }
  print(j)
}


#plotting the fit for each model
responses<-as.character(unique(gam_results$response))
for (j in 1:length(responses)){
  
  plot_dat_Ws<-gam_results[gam_results$response==responses[j] ,]
  plot_dat_Cat<-gam_results[gam_results$response==responses[j],]
  
  covs_order = factor(plot_dat$covariate, levels=unique(plot_dat$covariate[rev(order(plot_dat$r2))]), ordered=TRUE)
  
  plot_dat$covs_order<-covs_order
  
  plot_Ws<-ggplot(data=plot_dat[grep("Ws",plot_dat$covariate),]) + geom_point(aes(x=covs_order, y=r2), size=2)   + theme_classic() + scale_y_continuous("Adjusted R Squared", limits=c(0,.23))+ scale_x_discrete("")+ theme(axis.text.x = element_text(angle = 70, hjust = 1)) + ggtitle(responses[j])
  
  ggsave(filename=paste(responses[j],"_Ws",".png", sep=""),plot=plot_Ws, path="Data/Nick/Results/GAM/fit_plots",device="png",width=6.5, height=4.5, dpi=300, units="in" )
  
  plot_Cat<-ggplot(data=plot_dat[grep("Cat",plot_dat$covariate),]) + geom_point(aes(x=covs_order, y=r2), size=2)   + theme_classic() + scale_y_continuous("Adjusted R Squared", limits=c(0,.23))+ scale_x_discrete("")+ theme(axis.text.x = element_text(angle = 70, hjust = 1)) + ggtitle(responses[j])
  
  ggsave(filename=paste(responses[j],"_Cat",".png", sep=""),plot=plot_Cat, path="Data/Nick/Results/GAM/fit_plots",device="png",width=6.5, height=4.5, dpi=300, units="in" )
  
  plot<-ggplot(data=plot_dat) + geom_point(aes(x=covs_order, y=r2), size=2)   + theme_classic() + scale_y_continuous("Adjusted R Squared", limits=c(0,.23))+ scale_x_discrete("")+ theme(axis.text.x = element_text(angle = 70, hjust = 1)) + ggtitle(responses[j])
  
  ggsave(filename=paste(responses[j],"_Ws_Cat",".png", sep=""),plot=plot, path="Data/Nick/Results/GAM/fit_plots",device="png",width=6.5, height=4.5, dpi=300, units="in" )
  
  
}


