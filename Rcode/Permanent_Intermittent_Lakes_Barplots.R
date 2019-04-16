####################### Permanent vs. intermittent lake protection in US #######################
# Date: 4-16-19
# updated:
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#### R libraries ####
library(reshape2)
library(ggplot2)
library(gridExtra)

#### Input data ####
# precalculated in LakeProtectionByState.R
setwd('C:/Users/FWL/Documents/FreshwaterConservation')
NARS_perm <- read.csv("Data/LakeProtection_byNARS_perm.csv")
NARS_inter <- read.csv("Data/LakeProtection_byNARS_inter.csv")
NARS_all <- read.csv("Data/LakeProtection_byNARS.csv")

############################ Main program ##################################
# Initial data wrangling: lump multi-use and strict
NARS_perm$Total_perm_ctr <- NARS_perm$ProtectedLakes_gap12_ctr + NARS_perm$ProtectedLakes_gap3_ctr
NARS_perm$Total_perm_Cat100 <- NARS_perm$ProtectedLakes_gap12_Cat100 + NARS_perm$ProtectedLakes_gap3_Cat100
NARS_inter$Total_inter_ctr <- NARS_inter$ProtectedLakes_gap12_ctr + NARS_inter$ProtectedLakes_gap3_ctr
NARS_inter$Total_inter_Cat100 <- NARS_inter$ProtectedLakes_gap12_Cat100 + NARS_inter$ProtectedLakes_gap3_Cat100

# Pull out new total columns, divide by # of lakes in ecoregion for % intermittent, % permanent protected
NARS_full_table <- merge(NARS_all[,c('Ecoregion','nLakes')], NARS_perm[,c('Ecoregion','Total_perm_ctr','Total_perm_Cat100')],
                         by='Ecoregion', all=F)
NARS_full_table <- merge(NARS_full_table, NARS_inter[,c('Ecoregion','Total_inter_ctr','Total_inter_Cat100')], 
                         by='Ecoregion', all=F)

NARS_full_table$Pct_perm_ctr <- (NARS_full_table$Total_perm_ctr/NARS_full_table$nLakes)*100
NARS_full_table$Pct_inter_ctr <- (NARS_full_table$Total_inter_ctr/NARS_full_table$nLakes)*100
NARS_full_table$Pct_perm_Cat100 <- (NARS_full_table$Total_perm_Cat100/NARS_full_table$nLakes)*100
NARS_full_table$Pct_inter_Cat100 <- (NARS_full_table$Total_inter_Cat100/NARS_full_table$nLakes)*100
NARS_full_table <- NARS_full_table[,c(1,7:10)]
NARS_full_melted <- melt(NARS_full_table, id.vars='Ecoregion')

NARS_ctr_melted <- NARS_full_melted[c(1:18),] #get rows with ctr protection for first barplot
NARS_ctr_melted$PlotOrder <- c(8,9,3,7,4,6,5,2,1,8,9,3,7,4,6,5,2,1) #west-east ordering of ecoregions
NARS_Cat100_melted <- NARS_full_melted[c(19:36),] #get rows with Cat100 protection for 2nd barplot
NARS_Cat100_melted$PlotOrder <- c(8,9,3,7,4,6,5,2,1,8,9,3,7,4,6,5,2,1) #west-east ordering of ecoregions

## barplot
ecoregion_ctr_plot <- ggplot(NARS_ctr_melted, aes(x = reorder(Ecoregion, PlotOrder), y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  ggtitle('a) Protected lake = lake center in protected area')+
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  scale_y_continuous(limits=c(0,70), breaks=seq(0,70,10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
  theme(axis.text.y=element_text(color='black'))+
  scale_fill_manual("legend", values = c("Pct_perm_ctr" = "dodgerblue", "Pct_inter_ctr" = "gray50"),
                    labels=c('Permanent','Intermittent'))+
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(legend.position=c(0.89,0.9))+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

ecoregion_Cat100_plot <- ggplot(NARS_Cat100_melted, aes(x = reorder(Ecoregion, PlotOrder), y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  ggtitle('b) Protected lake = 100% catchment protected')+
  guides(fill = guide_legend(reverse=T)) +
  #theme_bw() +
  scale_y_continuous(limits=c(0,70), breaks=seq(0,70,10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x=element_text(angle=50, hjust=1, color='black'))+ #tilt axis labels
  theme(axis.text.y=element_text(color='black'))+
  scale_fill_manual("legend", values = c("Pct_perm_Cat100" = "dodgerblue", "Pct_inter_Cat100" = "gray50"),
                    labels=c('Permanent','Intermittent'))+
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(legend.position="none")+ #manually reposition legend inside plot
  theme(legend.title=element_blank()) #remove legend title

png('Figures/LakeProtectionByNARS_perm_inter.png',width = 4.5,height = 6,units = 'in',res=600)
  grid.arrange(ecoregion_ctr_plot, ecoregion_Cat100_plot, nrow=2)
dev.off()