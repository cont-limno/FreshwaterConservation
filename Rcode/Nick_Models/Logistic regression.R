## logistic regression model
library(sjPlot)
library(ggplot2)

catch_dat<-read.csv("Data/Nick/catchment_table.csv", header=T)

shed_dat<-read.csv("Data/Nick/watershed_table.csv", header=T)

dat<-full_join(catch_dat[,c(2:33)], shed_dat[,c(2,11,14:33)], by="COMID")

#max depth has negative values, maybe nonsense
dat$MaxDepth[dat$MaxDepth<0]<-NA


#removing NA values from all rows. 
dat_log<-dat[complete.cases(dat[,c(4:length(dat))]),][,4:length(dat)]

#logistic regression for each covariate and response, and filling in a table reporting the results
logistic_coef<-data.frame()
for (j in 1:6){
for ( i in 7:50){
log_model<- glm(dat_log[,j]~ scale(log(dat_log[,i]+(min(dat_log[,i])+1)),center = T, scale=T),family=binomial(link='logit'),data=dat_log)

sum<-summary(log_model)
coef=sum$coefficients[2,1]
covariate=names(dat_log[i])
response<-names(dat_log[j])
p=sum$coefficients[2,4]
se=sum$coefficients[2,2]
dev_expl=1-sum$deviance/sum$null.deviance

logistic_coef<-rbind(logistic_coef,data.frame(response=response,covariate=covariate,coef=coef, se=se, p=p, dev_expl=dev_expl ))
}
}

#plotting the coefficient estimates for each logistic model
responses<-as.character(unique(logistic_coef$response))
for (j in 1:length(responses)){
  
plot_dat<-logistic_coef[logistic_coef$response==responses[j],]

covs_order = factor(plot_dat$covariate, levels=unique(plot_dat$covariate[order(plot_dat$coef)]), ordered=TRUE)

plot_dat$covs_order<-covs_order

plot<-ggplot(data=plot_dat) + geom_point(aes(x=covs_order, y=coef)) + geom_errorbar(aes(x=covs_order, ymin=coef-se, ymax=coef+se)) + geom_hline(yintercept=0) + theme_classic() + scale_y_continuous("Scaled Coefficient")+ scale_x_discrete("")+ theme(axis.text.x = element_text(angle = 70, hjust = 1)) + ggtitle(responses[j])

ggsave(filename=paste(responses[j],".png", sep=""),plot=plot, path="~/Documents/Grad School/Projects/FreshwaterConservation/Data/Nick/Results/Logistic_Regression",device="png",width=6.5, height=4.5, dpi=300, units="in" )

}


