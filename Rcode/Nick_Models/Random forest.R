####Random forest model
library(dplyr)
library(randomForest)
library(pROC)
library(forestFloor)
library(caret)

catch_dat<-read.csv("Data/Nick/catchment_table.csv", header=T)

shed_dat<-read.csv("Data/Nick/watershed_table.csv", header=T)

dat<-full_join(catch_dat[,c(2:33)], shed_dat[,c(2,11,14:33)], by="COMID")

#max depth has negative values, maybe nonsense
dat$MaxDepth[dat$MaxDepth<0]<-NA


#removing NA values from all rows. 
dat_rf<-dat[complete.cases(dat[,c(4:length(dat))]),][,4:length(dat)]

RF_list<-list()
FF_list<-list()
for (j in 1:6){
#running random forest, correcting class imbalance
  RF_list[[j]]<-randomForest(x = dat_rf[,7:50], y = dat_rf[,j],keep.inbag = T, keep.forest = T, ntree=500, importance=T,strata=dat_rf[,j],sampsize=c(5000,5000)) 
  
  FF_list[[j]]<-forestFloor(
    rf.fit = RF_list[[j]],       # mandatory
    X=dat_rf[,7:50],
    y=dat_rf[,j],
    calc_np = FALSE,    # TRUE or FALSE both works, makes no difference
    binary_reg = T #takes no effect here when rfo$type="regression"
  )
  print(j)
}

save(RF_list,  file = "Data/Nick/Results/Random Forest/rf_model_list.RData")

save(FF_list,  file = "Data/Nick/Results/Random Forest/frst_floor_list.RData")
  
#exporting variable importance plots
 for (j in 1:6){
   png(paste("~/Documents/Grad School/Projects/FreshwaterConservation/Data/Nick/Results/Random Forest/Variable_Importance/",names(dat_rf[j]), ".png", sep=""), width = 7, height=7, res = 300, units = "in")
   varImpPlot(RF_list[[j]], type = 1, main=names(dat_rf[j]))
  dev.off()
 } 
  


#exporting feature contributions (marginal effects)
for (j in 1:6){
  png(paste("~/Documents/Grad School/Projects/FreshwaterConservation/Data/Nick/Results/Random Forest/Marginal_Effects/",names(dat_rf[j]), ".png", sep=""), width = 8, height=7, res = 300, units = "in")
  Col=fcol(FF_list[[j]],1,orderByImportance=T, alpha=.05)
  plot( FF_list[[j]],1:9,plot_GOF=F,orderByImportance=TRUE, limitY=F, col=Col, ylab="Change in probability")
  dev.off()
} 


rf_predictions<-data.frame()

for (j in 1:6){
  rf.roc<-roc(dat_rf[,j],RF_list[[j]]$votes[,2])
  positive_cat<-as.character(unique(dat_rf[,j])[grep("Unprotected",unique(dat_rf[,j]))])
  conf<-caret::confusionMatrix(predict(RF_list[[j]]),dat_rf[,j], positive=positive_cat)
  rf_predictions<-rbind(rf_predictions, data.frame(response=names(dat_rf[j]),auc=auc(rf.roc),accuracy=conf$overall[1],sensitivity=conf$byClass[1], specificity=conf$byClass[2]))
}

write.csv(rf_predictions, "~/Documents/Grad School/Projects/FreshwaterConservation/Data/Nick/Results/Random Forest/rf_fit_table_12_18.csv", row.names=F)



