## logistic regression model
library(ggplot2)
library(dplyr)
library(cowplot)

catch_dat<-read.csv("Data/Nick/catchment_table.csv", header=T)

#shed_dat<-read.csv("Data/Nick/watershed_table.csv", header=T)

dat<-catch_dat
#dat<-full_join(catch_dat[,c(2:33)], shed_dat[,c(2,11,14:33)], by="COMID")

#max depth has negative values, maybe nonsense
#dat$MaxDepth[dat$MaxDepth<0]<-NA


#removing NA values from all rows. 
#dat_log<-dat[complete.cases(dat[,c(6:length(dat))]),][,6:length(dat)]
dat_log<-dat
  
dat_log$ProtectGAP12_ctr_fac<-factor(dat_log$ProtectGAP12_ctr, levels=c("UnprotectedCtr","ProtectedCtr"))

dat_log$ProtectGAP3_ctr_fac<-factor(dat_log$ProtectGAP3_ctr, levels=c("UnprotectedCtr","ProtectedCtr"))

dat_log$ProtectGAP12Cat_80_fac<-factor(dat_log$ProtectGAP12Cat_80, levels=c("Unprotected80","Protected80"))

dat_log$ProtectGAP3Cat_80_fac<-factor(dat_log$ProtectGAP3Cat_80, levels=c("Unprotected80","Protected80"))

dat_log$Unprotected_fac<-factor(dat_log$Unprotected, levels=c("Unprotected","Protected"))



#logistic regression for each covariate and response, and filling in a table reporting the results
logistic_coef<-data.frame()
regions<-unique(dat_log$WSA9[!is.na(dat_log$WSA9)])
for (k in 1:length(regions)){
  #difference responses
  for (j in 32:35){
    #different covariates
    for ( i in 12:28){
  
      #determining number of unique values for each covariate for each category of the response
num_unique_val<-aggregate(dat_log[dat_log$WSA9==regions[k],i]~dat_log[dat_log$WSA9==regions[k],j], FUN=function(x){length(unique(x))})
  
  
  if (length(unique(dat_log[dat_log$WSA9==regions[k],i]))>1 & num_unique_val[1,2]>1 &num_unique_val[2,2]>1){
    tryCatch({
log_model<- glm(dat_log[dat_log$WSA9==regions[k],j]~ scale(log(dat_log[dat_log$WSA9==regions[k],i]+(abs(min(dat_log[dat_log$WSA9==regions[k],i], na.rm=T))+.01)),center = T, scale=T),family=binomial(link='logit'))
    }, warning=function(w) print(c(j,k,i)))
  

sum<-summary(log_model)
coef=sum$coefficients[2,1]
covariate=names(dat_log[i])
response<-names(dat_log[j])
p=sum$coefficients[2,4]
se=sum$coefficients[2,2]
dev_expl=1-sum$deviance/sum$null.deviance

logistic_coef<-rbind(logistic_coef,data.frame(response=response,covariate=covariate,coef=coef, se=se, p=p, dev_expl=dev_expl,n=nrow(dat_log[dat_log$WSA9==regions[k],]) ,region=regions[k]))
  }
  else{
    covariate=names(dat_log[i])
    response<-names(dat_log[j])
    
    logistic_coef<-rbind(logistic_coef,data.frame(response=response,covariate=covariate,coef=NA, se=NA, p=NA, dev_expl=NA,n=nrow(dat_log[dat_log$WSA9==regions[k],]) ,region=regions[k]))
  }
  print(c(k))
}
}
}

#plotting the coefficient estimates for each logistic model
# responses<-as.character(unique(logistic_coef$response))
# for (j in 1:length(responses)){
#   
# plot_dat<-logistic_coef[logistic_coef$response==responses[j],]
# 
# covs_order = factor(plot_dat$covariate, levels=unique(plot_dat$covariate[order(plot_dat$coef)]), ordered=TRUE)
# 
# plot_dat$covs_order<-covs_order
# 
# plot<-ggplot(data=plot_dat) + geom_point(aes(x=covs_order, y=coef)) + geom_errorbar(aes(x=covs_order, ymin=coef-se, ymax=coef+se)) + geom_hline(yintercept=0) + theme_classic() + scale_y_continuous("Scaled Coefficient")+ scale_x_discrete("")+ theme(axis.text.x = element_text(angle = 70, hjust = 1)) + ggtitle(responses[j])
# 
# ggsave(filename=paste(responses[j],".png", sep=""),plot=plot, path="~/Documents/Grad School/Projects/FreshwaterConservation/Data/Nick/Results/Logistic_Regression",device="png",width=6.5, height=4.5, dpi=300, units="in" )
# 
# }
# 


###plotting heat maps of coefficients by region
#removing some covariates with low variability
logistic_coef$region_fac<-factor(logistic_coef$region, levels=rev(c("NAP","CPL", "SAP", "TPL","UMW", "SPL", "NPL", "XER","WMT")))

cov_names<-data.frame(covariate_fac=unique(logistic_coef$covariate),full_cov_name=c("Lake Area (km²)", "Catchment Area (km²)", "Drainage Ratio", "Elevation (m)", "Topo. Wetness Index", "% Total Forest", "% Agriculture", "% Wetland", "% Conif. Forest", "Road Density (km/km²)", "% Impervious", "Runoff (mm)", "% Base Flow", "S+N Dep. (kg/ha/yr)", "% Forest Loss", "Mean Precip. (mm)","Mean Temp. (C°)" ))

logistic_coef<-left_join(logistic_coef, cov_names, by=c("covariate"="covariate_fac"))
#figuring out the mean coeficient value for each covariate across all protection types so I can order them in the plot.


ordering_coef<-logistic_coef %>% group_by(covariate) %>% dplyr::summarize(mean_coef=mean(coef, na.rm=T), full_cov_name=first(full_cov_name))
ordering_coef$type=c("lake","lake","lake","ele","ele","land","land","land","land","land","land","ele","ele","clim","land","clim","clim")
#setting the covariate order
# logistic_coef$covariate_fac<-factor(logistic_coef$covariate,levels=(data.frame(ordering_coef[order(ordering_coef$mean_coef),"covariate"]))[,1])

logistic_coef$full_cov_name_fac<-factor(logistic_coef$full_cov_name,levels=(data.frame(ordering_coef[order(ordering_coef$mean_coef),"full_cov_name"]))[,1])


#logistic_coef$covariate_fac<-factor(logistic_coef$covariate,levels=(data.frame(ordering_coef[order(ordering_coef$type),"covariate"]))[,1])

##setting the region order from west to east






#plotting the heatmap. removing some covariates that didn't have a enough unique values to get good estimates from the logistic regression
ProtectGAP12_ctr_plot<-ggplot() +
  geom_tile(data=logistic_coef[logistic_coef$response=="ProtectGAP12_ctr_fac" ,], aes(x=region_fac, y=full_cov_name_fac, fill=coef)) + 
  #scale_fill_distiller("Coef.",palette = "RdBu", limits=c(-2.16,1.78)) + 
  scale_fill_gradient2( "β", low = "blue", mid = "white",high = "red", midpoint = 0, limits=c(-2.05  , 1.9)) +
  scale_y_discrete("") + scale_x_discrete("", position="bottom") +
  geom_point(data=logistic_coef[logistic_coef$response=="ProtectGAP12_ctr_fac" & logistic_coef$p>=.05 ,], aes(x=region_fac, y=full_cov_name, z=NULL), size=.5)+
  theme(axis.text.y = element_text(size=12))
# ggtitle("ProtectGAP12_ctr")

ProtectGAP3_ctr_plot<-ggplot()+
  geom_tile(data=logistic_coef[logistic_coef$response=="ProtectGAP3_ctr_fac" ,], aes(x=region_fac, y=full_cov_name_fac, fill=coef)) + 
  scale_fill_gradient2( "β", low = "blue", mid = "white",high = "red", midpoint = 0, limits=c(-2.05  , 1.9)) +
  scale_y_discrete("") + scale_x_discrete("", position="bottom") +
  geom_point(data=logistic_coef[logistic_coef$response=="ProtectGAP3_ctr_fac" & logistic_coef$p>=.05 ,], aes(x=region_fac, y=full_cov_name, z=NULL), size=.5)+
  theme(axis.text.y = element_text(size=12))

ProtectGAP12Cat_80_plot<-ggplot()+
  geom_tile(data=logistic_coef[logistic_coef$response=="ProtectGAP12Cat_80_fac" ,], aes(x=region_fac, y=full_cov_name_fac, fill=coef)) + 
  scale_fill_gradient2( "β",low = "blue", mid = "white",high = "red", midpoint = 0, limits=c(-2.05  , 1.9)) +
  scale_y_discrete("") + scale_x_discrete("", position="bottom") +
  geom_point(data=logistic_coef[logistic_coef$response=="ProtectGAP12Cat_80_fac" & logistic_coef$p>=.05 ,], aes(x=region_fac, y=full_cov_name, z=NULL), size=.5)+
  theme(axis.text.y = element_text(size=12))

ProtectGAP3Cat_80_plot<-ggplot()+
  geom_tile(data=logistic_coef[logistic_coef$response=="ProtectGAP3Cat_80_fac" ,], aes(x=region_fac, y=full_cov_name_fac, fill=coef)) + 
  scale_fill_gradient2(  "β",low = "blue", mid = "white",high = "red", midpoint = 0, limits=c(-2.05  , 1.9)) +
  scale_y_discrete("") + scale_x_discrete("", position="bottom") +
  geom_point(data=logistic_coef[logistic_coef$response=="ProtectGAP3Cat_80_fac" & logistic_coef$p>=.05 ,], aes(x=region_fac, y=full_cov_name, z=NULL), size=.5)+
  theme(axis.text.y = element_text(size=12))


Unprotected_plot<-ggplot()+
  geom_tile(data=logistic_coef[logistic_coef$response=="Unprotected_fac" ,], aes(x=region_fac, y=full_cov_name_fac, fill=coef)) + 
  scale_fill_gradient2( "β", low = "blue", mid = "white",high = "red", midpoint = 0, limits=c(-2.05  , 1.9)) +
  scale_y_discrete("") + scale_x_discrete("", position="bottom") +
  geom_point(data=logistic_coef[logistic_coef$response=="Unprotected_fac" & logistic_coef$p>=.05 ,], aes(x=region_fac, y=full_cov_name, z=NULL), size=.5)+
  theme(axis.text.y = element_text(size=12))

ProtectGAP12_ctr_plot_noleg<-ProtectGAP12_ctr_plot + guides(fill=F)
ProtectGAP3_ctr_plot_noleg<-ProtectGAP3_ctr_plot + guides(fill=F)
ProtectGAP12Cat_80_plot_noleg<-ProtectGAP12Cat_80_plot + guides(fill=F)
ProtectGAP3Cat_80_plot_noleg<-ProtectGAP3Cat_80_plot + guides(fill=F)
legend<-get_legend(ProtectGAP12_ctr_plot+ theme(legend.position="right",legend.justification="center",legend.key.height=unit(1,"cm")))

plots_2_2<-cowplot::plot_grid(cowplot::plot_grid(ProtectGAP12Cat_80_plot_noleg,ProtectGAP3Cat_80_plot_noleg, ncol = 1, labels="auto"),legend, rel_widths =c(1,.1) )

#plot_w_legend<-plot_grid(plots_2_2, legend, ncol=1, rel_heights = c(1,.1))

ggsave(plot=plots_2_2, filename="Protect_panels_maintext_8_6.png",path="~/Documents/Grad School/Projects/FreshwaterConservation/Data/Nick/Results/Logistic Regression/Heatmaps/", units="in", width=7, height=9)


plots_2_sup<-cowplot::plot_grid(cowplot::plot_grid(ProtectGAP12_ctr_plot_noleg,ProtectGAP3_ctr_plot_noleg, ncol = 1, labels="auto"),legend, rel_widths =c(1,.1) )

#plot_w_legend<-plot_grid(plots_2_2, legend, ncol=1, rel_heights = c(1,.1))

ggsave(plot=plots_2_sup, filename="Protect_panels_supp_8_6.png",path="~/Documents/Grad School/Projects/FreshwaterConservation/Data/Nick/Results/Logistic Regression/Heatmaps/", units="in", width=7, height=9)

# ggsave(filename="ProtectGAP12_ctr.png",plot=ProtectGAP12_ctr_plot,device="png", path="~/Documents/Grad School/Projects/FreshwaterConservation/Data/Nick/Results/Logistic Regression/Heatmaps/", units="in", width=8.5, height=6)
# 
# ggsave(filename="ProtectGAP3_ctr.png",plot=ProtectGAP3_ctr_plot,device="png", path="~/Documents/Grad School/Projects/FreshwaterConservation/Data/Nick/Results/Logistic Regression/Heatmaps/", units="in", width=8.5, height=6)
# 
# 
# ggsave(filename="ProtectGAP3Cat_100.png",plot=ProtectGAP3Cat_100_plot,device="png", path="~/Documents/Grad School/Projects/FreshwaterConservation/Data/Nick/Results/Logistic Regression/Heatmaps/", units="in", width=8.5, height=6)
# 
# 
# ggsave(filename="ProtectGAP12Cat_100.png",plot=ProtectGAP12Cat_100_plot,device="png", path="~/Documents/Grad School/Projects/FreshwaterConservation/Data/Nick/Results/Logistic Regression/Heatmaps/", units="in", width=8.5, height=6)

ggsave(filename="Unprotected_7_30.png",plot=Unprotected_plot+ggtitle(""),device="png", path="~/Documents/Grad School/Projects/FreshwaterConservation/Data/Nick/Results/Logistic Regression/Heatmaps/", units="in", width=8.5, height=6)

