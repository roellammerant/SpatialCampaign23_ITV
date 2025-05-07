##%######################################################%##
#                                                          #
####               Species level R:S ratio              ####
#                                                          #
##%######################################################%##
Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")
TraitData_exp <- Spatial_Campaign_data_exp[,c(1:4, 29,30)]
TraitData_exp$Ratio <- TraitData_exp$`Total belowground dry mass (mg)`/TraitData_exp$`Total aboveground dry mass (mg)`
names(TraitData_exp)[7]<-paste(c("Ratio"))
names(TraitData_exp)[2]<-paste(c("Site_number"))
TraitData_exp[is.na(TraitData_exp)] = 0

Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")
TraitData_semi <- Spatial_Campaign_data_semi[,c(1:4, 30,31)]
TraitData_semi[is.na(TraitData_semi)] = 0
TraitData_semi$Ratio <- TraitData_semi$`Total belowground dry mass (mg)`/ TraitData_semi$`Total aboveground dry mass (mg)`  
names(TraitData_semi)[7]<-paste(c("Ratio"))
names(TraitData_semi)[2]<-paste(c("Site_number"))

Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")
TraitData_shel <- Spatial_Campaign_data_shel[,c(1:4, 34,35)]
TraitData_shel[is.na(TraitData_shel)] = 0
TraitData_shel$Ratio <- TraitData_shel$`Total belowground dry mass (mg)` / TraitData_shel$`Total aboveground dry mass (mg)`
names(TraitData_shel)[7]<-paste(c("Ratio"))
names(TraitData_shel)[2]<-paste(c("Site_number"))

Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")
TraitData_pojo <- Spatial_Campaign_data_pojo[-c(1,4,6,7),c(1:4, 32,33)]
TraitData_pojo[is.na(TraitData_pojo)] = 0
TraitData_pojo$Ratio <- TraitData_pojo$`Total belowground dry mass (mg)`/TraitData_pojo$`Total aboveground dry mass (mg)`
names(TraitData_pojo)[7]<-paste(c("Ratio"))
names(TraitData_pojo)[2]<-paste(c("Site_number"))
TraitData_pojo <- TraitData_pojo[-c(1,4,6,7),]
TraitData_pojo[is.na(TraitData_pojo)] = 0

############################################
###         Combine dataframes           ###
############################################

TraitData_exp$Site_group <- c(rep("exposed",136))
TraitData_semi$Site_group <- c(rep("semi",118))
TraitData_shel$Site_group <- c(rep("sheltered",70))
TraitData_pojo$Site_group <- c(rep("Pojo",123))
Ratio_all <- rbind(TraitData_exp, TraitData_semi, TraitData_shel, TraitData_pojo)
Ratio_Shallow <- subset(Ratio_all, Depth=="S")
Ratiotrait_MYRSPI <- subset(Ratio_Shallow, Species=="MYRSPI")
Ratiotrait_STUPEC <- subset(Ratio_Shallow, Species=="STUPEC")
Ratiotrait_CERDEM <- subset(Ratio_Shallow, Species=="CERDEM")
Ratiotrait_POTPER <- subset(Ratio_Shallow, Species=="POTPER")
RatioS <- rbind(Ratiotrait_MYRSPI,Ratiotrait_STUPEC,Ratiotrait_CERDEM, Ratiotrait_POTPER)
RatioS$Exposure <- factor(paste(RatioS$Site_group, 
                                RatioS$Species, sep = "_"),
                         levels = c(
                           "exposed_MYRSPI", "exposed_STUPEC","exposed_POTPER",
                           "semi_MYRSPI", "semi_STUPEC","semi_CERDEM", "semi_POTPER",
                           "Pojo_MYRSPI", "Pojo_CERDEM", 
                           "sheltered_MYRSPI", "sheltered_STUPEC", "sheltered_CERDEM", "sheltered_POTPER"
                         ))


##%######################################################%##
#                                                          #
####                    Intra R:S ratio                 ####
#                                                          #
##%######################################################%##

RatioS$Exposure <- factor(RatioS$Exposure,
                         levels=c(
                           "exposed_MYRSPI", "exposed_STUPEC","exposed_POTPER",
                           "semi_MYRSPI", "semi_STUPEC", "semi_POTPER", "semi_CERDEM",
                           "sheltered_MYRSPI", "sheltered_STUPEC", "sheltered_POTPER",  "sheltered_CERDEM",
                           "Pojo_MYRSPI", "Pojo_CERDEM"))







par(mar = c(3, 5, 2, 3))
boxplot(Ratio ~ Exposure, data = RatioS,
        boxwex = 0.5, col = c("cyan3","burlywood","grey","cyan3","burlywood","grey","darkorange","cyan3","burlywood","grey","darkorange", "cyan3", "darkorange"),
        main = NA,
        xlab = NA, ylab = "R:S ratio",at = c(1, 2, 3, 5, 6, 7,8, 10,11,12,13, 15,16),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(Ratio ~ Exposure,
           data = RatioS,
           method = "jitter",
           at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5, 7.5, 9.5,10.5,11.5,12.5, 14.5,15.5),
           pch = 17,
           col = c("cyan3","burlywood","grey","cyan3","burlywood","grey","darkorange","cyan3","burlywood","grey","darkorange", "cyan3", "darkorange"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")


axis(1, 
     at = seq(1.8 , 16 , 4.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topleft", legend = c(expression(italic("M. spicatum")), expression(italic("S. pectinata")), expression(italic("p. perfoliatus")), expression(italic("C. demersum"))), 
       col=c("cyan3","burlywood","grey","darkorange"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)


##%######################################################%##
#                                                          #
####                 Stats R:S ratio                    ####
#                                                          #
##%######################################################%##


##### Normality #####

library(rstatix)

### Exposed
RatioS[c(1:8),c(9)]
mshapiro_test(RatioS[c(1:8),c(7)]) # MYRSPI non-significant
RatioS[c(40:60),c(9)]
mshapiro_test(RatioS[c(40:60),c(7)]) # STUPEC non-significant
RatioS[c(138:152),c(9)]
mshapiro_test(RatioS[c(138:152),c(7)]) # POTPER non-significant

### Semi
RatioS[c(9:24),c(9)]
mshapiro_test(RatioS[c(9:24),c(7)]) # MYRSPI significant
RatioS[c(61:75),c(9)]
mshapiro_test(RatioS[c(61:75),c(7)]) # STUPEC significant
RatioS[c(85:88),c(9)]
mshapiro_test(RatioS[c(85:88),c(7)]) # CERDEM all zero
RatioS[c(153:169),c(9)]
mshapiro_test(RatioS[c(153:169),c(7)]) # POTPER significant

### sheltered
RatioS[c(25:33),c(9)]
mshapiro_test(RatioS[c(25:33),c(7)]) # MYRSPI non-significant
RatioS[c(76:84),c(9)]
mshapiro_test(RatioS[c(76:84),c(7)]) # STUPEC non-significant
RatioS[c(91:112),c(9)]
mshapiro_test(RatioS[c(91:112),c(7)]) # CERDEM all zero
RatioS[c(170:175),c(9)]
mshapiro_test(RatioS[c(170:175),c(7)]) # POTPER Non-significant

### Pojo
RatioS[c(34:39),c(9)]
mshapiro_test(RatioS[c(34:39),c(7)]) # MYRSPI non-significant
RatioS[c(113:137),c(9)]
mshapiro_test(RatioS[c(113:137),c(7)]) # CERDEM all zero


#### For every species, not all data is normal distributed

##### Homogeneity #####

RatioS$Site_Exposure <- paste(RatioS$Site_number, RatioS$Exposure, sep="_")

R_Intra_Myrspi <- RatioS[ which(RatioS$Species=='MYRSPI'), ]
R_Intra_STUPEC <- RatioS[ which(RatioS$Species=='STUPEC'), ]
R_Intra_CERDEM <- RatioS[ which(RatioS$Species=='CERDEM'), ]
R_Intra_POTPER <- RatioS[ which(RatioS$Species=='POTPER'), ]

fligner.test(R_Intra_Myrspi$Ratio ~ R_Intra_Myrspi$Site_group) # significant
fligner.test(R_Intra_STUPEC$Ratio ~ R_Intra_STUPEC$Site_group) # significant
fligner.test(R_Intra_CERDEM$Ratio ~ R_Intra_CERDEM$Site_group) # all zero
fligner.test(R_Intra_POTPER$Ratio ~ R_Intra_POTPER$Site_group) # non-significant


##%######################################################%##
#                                                          #
####      Non-parametric test + heteroscedasticity     #####
#                                                          #
##%######################################################%##

library(tidyverse)
library(vegan)

##%######################################################%##
#                                                          #
####            Non parametric permanova                ####   
####                                                    ####
#                                                          #
##%######################################################%##


######### Shallow sites #########
# https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/permanova/
# Doesn´t assume normality or homogeneity of variance

MYRSPI_R <- adonis2(R_Intra_Myrspi$Ratio ~ Exposure + Site_Exposure, data=R_Intra_Myrspi, perm=999)
MYRSPI_R # significant difference at exposure level

R_Intra_STUPEC$Ratio[R_Intra_STUPEC$Ratio == 0] <- 0.00001 
STUPEC_R <- adonis2(R_Intra_STUPEC$Ratio ~ Exposure + Site_Exposure, data=R_Intra_STUPEC, perm=999)
STUPEC_R # significant difference at exposure and site level

CERDEM_R <- adonis2(R_Intra_CERDEM$Ratio ~ Exposure + Site_Exposure, data=R_Intra_CERDEM, perm=999)
CERDEM_R # No significant differences, all zero values

POTPER_R <- adonis2(R_Intra_POTPER$Ratio ~ Exposure + Site_Exposure, data=R_Intra_POTPER, perm=999)
POTPER_R # significant difference at exposure level

##%######################################################%##
#                                                          #
####                 Post-hoc adonis test               ####   
#                                                          #
##%######################################################%##
library(devtools)
library(pairwiseAdonis)

######### Across exposure categories #########

dist_matrixM=vegdist(R_Intra_Myrspi$Ratio,method="manhattan")
pairwise.adonis2(dist_matrixM ~ Exposure + Site_Exposure, p.adj = "hochberg", data = R_Intra_Myrspi)

dist_matrixS=vegdist(R_Intra_STUPEC$Ratio,method="manhattan")
pairwise.adonis2(dist_matrixS ~ Exposure + Site_Exposure, p.adj = "hochberg", data = R_Intra_STUPEC)

dist_matrixC=vegdist(R_Intra_CERDEM$Ratio,method="manhattan")
pairwise.adonis2(dist_matrixC ~ Exposure + Site_Exposure, p.adj = "hochberg", data = R_Intra_CERDEM)

dist_matrixP=vegdist(R_Intra_POTPER$Ratio,method="manhattan")
pairwise.adonis2(dist_matrixP ~ Exposure + Site_Exposure, p.adj = "hochberg", data = R_Intra_POTPER)





