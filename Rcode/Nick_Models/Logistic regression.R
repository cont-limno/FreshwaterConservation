## logistic regression model

catch_dat<-read.csv("Data/Nick/catchment_table.csv", header=T)

shed_dat<-read.csv("Data/Nick/watershed_table.csv", header=T)

dat<-full_join(catch_dat[,c(2:33)], shed_dat[,c(2,11,14:33)], by="COMID")

#max depth has negative values, maybe nonsense
dat$MaxDepth[dat$MaxDepth<0]<-NA


#removing NA values from all rows. 
dat_log<-dat[complete.cases(dat[,c(4:length(dat))]),][,4:length(dat)]


log_ProtectGAP12Cat_75 <- glm(ProtectGAP12Cat_75 ~PctTotalAg2011Cat,family=binomial(link='logit'),data=dat_log)

summary(model)
plot(dat_log$PctTotalAg2011Cat,dat_log$ProtectGAP12Cat_75,xlab="PctTotalAg2011Cat",ylab="Probability of protected",col = rgb(red = 1, green = 1, blue = 1, alpha = 0.05))
curve(predict(log_ProtectGAP12Cat_75,data.frame(PctTotalAg2011Cat=x),type="resp"),add=TRUE)
