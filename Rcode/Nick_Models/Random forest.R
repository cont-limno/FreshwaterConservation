####Random forest model
library(dplyr)
library(randomForest)
library(pROC)
library(forestFloor)

catch_dat<-read.csv("Data/Nick/catchment_table.csv", header=T)

shed_dat<-read.csv("Data/Nick/watershed_table.csv", header=T)

dat<-full_join(catch_dat[,c(2:33)], shed_dat[,c(2,11,14:33)], by="COMID")

#max depth has negative values, maybe nonsense
dat$MaxDepth[dat$MaxDepth<0]<-NA


#removing NA values from all rows. 
dat_rf<-dat[complete.cases(dat[,c(4:length(dat))]),][,4:length(dat)]

#running random forest, correcting class imbalance
RF_ProtectGAP12Cat_75<-randomForest(x = dat_rf[,7:50], y = dat_rf[,4],keep.inbag = T, keep.forest = T, ntree=100, importance=T,strata=dat_rf[,4],sampsize=c(10000,10000)) 

RF_ProtectGAP12Cat_75
varImpPlot(RF_ProtectGAP12Cat_75)

rf.roc<-roc(dat_rf$ProtectGAP12Cat_75,RF_ProtectGAP12Cat_75$votes[,2])
plot(rf.roc, main=paste("AUC = ", round(auc(rf.roc),2), sep=""))
auc(rf.roc)

ff_ProtectGAP12Cat_75 = forestFloor(
  rf.fit = RF_ProtectGAP12Cat_75,       # mandatory
  X=dat_rf[,7:50],
  y=dat_rf[,4],
  calc_np = FALSE,    # TRUE or FALSE both works, makes no difference
  binary_reg = T #takes no effect here when rfo$type="regression"
)

Col=fcol(ff_ProtectGAP12Cat_75,1,orderByImportance=T, alpha=.3)

plot(ff_ProtectGAP12Cat_75,plot_seq = c(1:12),col=Col,plot_GOF=F,orderByImportance=TRUE, limitY=F) 

