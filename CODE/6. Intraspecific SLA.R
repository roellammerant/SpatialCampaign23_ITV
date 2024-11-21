##%######################################################%##
#                                                          #
####               Species level SLA                    ####
#                                                          #
##%######################################################%##
Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")
TraitData_exp <- Spatial_Campaign_data_exp[,c(1:4, 35,36)]
TraitData_exp$SLA <- (TraitData_exp$`Specific leaf area 1 (mm^2/mg)` +TraitData_exp$`Specific leaf area 2 (mm^2/mg)`)/2
names(TraitData_exp)[2]<-paste(c("Site_number"))
TraitData_exp[is.na(TraitData_exp)] = 0

Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")
TraitData_semi <- Spatial_Campaign_data_semi[,c(1:4, 36,37)]
TraitData_semi$SLA <- (TraitData_semi$`Specific leaf area 1 (mm^2/mg)` +TraitData_semi$`Specific leaf area 2 (mm^2/mg)`)/2  
names(TraitData_semi)[2]<-paste(c("Site_number"))

Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")
TraitData_shel <- Spatial_Campaign_data_shel[,c(1:4, 40,41)]
TraitData_shel$SLA <- (TraitData_shel$`Specific leaf area 1 (cm^2/mg)` +TraitData_shel$`Specific leaf area 2 (cm^2/mg)`)/2  
names(TraitData_shel)[2]<-paste(c("Site_number"))

Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")
TraitData_pojo <- Spatial_Campaign_data_pojo[,c(1:4, 38,39)]
TraitData_pojo$SLA <- (TraitData_pojo$`Specific leaf area 1 (cm^2/g)` +TraitData_pojo$`Specific leaf area 2 (cm^2/g)`)/2  
names(TraitData_pojo)[2]<-paste(c("Site_number"))
TraitData_pojo <- TraitData_pojo[-c(1,4,6,7),]
TraitData_pojo[is.na(TraitData_pojo)] = 0

############################################
###         Combine dataframes           ###
############################################

TraitData_exp$Site_group <- c(rep("exposed",136))
TraitData_semi$Site_group <- c(rep("semi",118))
TraitData_shel$Site_group <- c(rep("sheltered",70))
TraitData_pojo$Site_group <- c(rep("Pojo",127))

TraitData_exp <- TraitData_exp[,c(1:4, 7,8)]
TraitData_semi <- TraitData_semi[,c(1:4, 7,8)]
TraitData_shel <- TraitData_shel[,c(1:4, 7,8)]
TraitData_pojo <- TraitData_pojo[,c(1:4, 7,8)]

SLA_all <- rbind(TraitData_exp, TraitData_semi, TraitData_shel, TraitData_pojo)
SLA_Shallow <- subset(SLA_all, Depth=="S")
SLAtrait_MYRSPI <- subset(SLA_Shallow, Species=="MYRSPI")
SLAtrait_STUPEC <- subset(SLA_Shallow, Species=="STUPEC")
SLAtrait_CERDEM <- subset(SLA_Shallow, Species=="CERDEM")
SLAtrait_POTPER <- subset(SLA_Shallow, Species=="POTPER")
SLAS <- rbind(SLAtrait_MYRSPI,SLAtrait_STUPEC,SLAtrait_CERDEM, SLAtrait_POTPER)
SLAS$Exposure <- factor(paste(SLAS$Site_group, 
                              SLAS$Species, sep = "_"),
                          levels = c(
                            "exposed_MYRSPI", "exposed_STUPEC","exposed_POTPER",
                            "semi_MYRSPI", "semi_STUPEC","semi_CERDEM", "semi_POTPER",
                            "Pojo_MYRSPI", "Pojo_CERDEM", 
                            "sheltered_MYRSPI", "sheltered_STUPEC", "sheltered_CERDEM", "sheltered_POTPER"
                          ))


##%######################################################%##
#                                                          #
####                      Intra SLA                     ####
#                                                          #
##%######################################################%##

SLAS$Exposure <- factor(SLAS$Exposure,
                          levels=c(
                            "exposed_MYRSPI", "exposed_STUPEC","exposed_POTPER",
                            "semi_MYRSPI", "semi_STUPEC", "semi_POTPER", "semi_CERDEM",
                            "sheltered_MYRSPI", "sheltered_STUPEC", "sheltered_POTPER",  "sheltered_CERDEM",
                            "Pojo_MYRSPI", "Pojo_CERDEM"))







par(mar = c(3, 5, 2, 3))
boxplot(SLA ~ Exposure, data = SLAS,
        boxwex = 0.5, col = c("cyan3","burlywood","grey","cyan3","burlywood","grey","darkorange","cyan3","burlywood","grey","darkorange", "cyan3", "darkorange"),
        main = NA,
        xlab = NA, ylab = "SLA",at = c(1, 2, 3, 5, 6, 7,8, 10,11,12,13, 15,16),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(SLA ~ Exposure,
           data = SLAS,
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
####                    Stats SLA                       ####
#                                                          #
##%######################################################%##


##### Normality #####

library(rstatix)

### Exposed
SLAS[c(1:8),c(7)]
mshapiro_test(SLAS[c(1:8),c(5)]) # MYRSPI non-significant
SLAS[c(40:60),c(7)]
mshapiro_test(SLAS[c(40:60),c(5)]) # STUPEC non-significant
SLAS[c(138:152),c(7)]
mshapiro_test(SLAS[c(138:152),c(5)]) # POTPER significant

### Semi
SLAS[c(9:24),c(7)]
mshapiro_test(SLAS[c(9:24),c(5)]) # MYRSPI non-significant
SLAS[c(61:75),c(7)]
mshapiro_test(SLAS[c(61:75),c(5)]) # STUPEC non-significant
SLAS[c(85:88),c(7)]
mshapiro_test(SLAS[c(85:88),c(5)]) # CERDEM non-significant
SLAS[c(153:169),c(7)]
mshapiro_test(SLAS[c(153:169),c(5)]) # POTPER significant

### sheltered
SLAS[c(25:33),c(7)]
mshapiro_test(SLAS[c(25:33),c(5)]) # MYRSPI non-significant
SLAS[c(76:84),c(7)]
mshapiro_test(SLAS[c(76:84),c(5)]) # STUPEC non-significant
SLAS[c(91:112),c(7)]
mshapiro_test(SLAS[c(91:112),c(5)]) # CERDEM significant
SLAS[c(170:175),c(7)]
mshapiro_test(SLAS[c(170:175),c(5)]) # POTPER Non-significant

### Pojo
SLAS[c(34:39),c(7)]
mshapiro_test(SLAS[c(34:39),c(5)]) # MYRSPI significant
SLAS[c(113:137),c(7)]
mshapiro_test(SLAS[c(113:137),c(5)]) # CERDEM significant


#### For all species except STUPEC, not all data is normal distributed

##### Homogeneity #####
SLAS$Site_Exposure <- paste(SLAS$Site_number, SLAS$Exposure, sep="_")
S_Intra_Myrspi <- SLAS[ which(SLAS$Species=='MYRSPI'), ]
S_Intra_STUPEC <- SLAS[ which(SLAS$Species=='STUPEC'), ]
S_Intra_CERDEM <- SLAS[ which(SLAS$Species=='CERDEM'), ]
S_Intra_POTPER <- SLAS[ which(SLAS$Species=='POTPER'), ]

fligner.test(S_Intra_Myrspi$SLA ~ S_Intra_Myrspi$Site_group) # non-significant
fligner.test(S_Intra_STUPEC$SLA ~ S_Intra_STUPEC$Site_group) # non-significant
fligner.test(S_Intra_CERDEM$SLA ~ S_Intra_CERDEM$Site_group) # significant
fligner.test(S_Intra_POTPER$SLA ~ S_Intra_POTPER$Site_group) # non-significant

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

MYRSPI_S <- adonis2(S_Intra_Myrspi$SLA ~ Exposure + Site_Exposure, data=S_Intra_Myrspi, perm=999)
MYRSPI_S # significant difference at exposure and site level

STUPEC_S <- adonis2(S_Intra_STUPEC$SLA ~ Exposure + Site_Exposure, data=S_Intra_STUPEC, perm=999)
STUPEC_S # no differences

CERDEM_S <- adonis2(S_Intra_CERDEM$SLA ~ Exposure + Site_Exposure, data=S_Intra_CERDEM, perm=999)
CERDEM_S # significant difference at site level

POTPER_S <- adonis2(S_Intra_POTPER$SLA ~ Exposure + Site_Exposure, data=S_Intra_POTPER, perm=999)
POTPER_S # no differences

##%######################################################%##
#                                                          #
####                 Post-hoc adonis test               ####   
#                                                          #
##%######################################################%##
library(devtools)
library(pairwiseAdonis)

######### Across exposure categories #########

dist_matrixM=vegdist(S_Intra_Myrspi$SLA,method="manhattan")
pairwise.adonis2(dist_matrixM ~ Exposure + Site_Exposure, p.adj = "hochberg", data = S_Intra_Myrspi)

dist_matrixM=vegdist(S_Intra_STUPEC$SLA,method="manhattan")
pairwise.adonis2(dist_matrixM ~ Exposure + Site_Exposure, p.adj = "hochberg", data = S_Intra_STUPEC)

dist_matrixM=vegdist(S_Intra_CERDEM$SLA,method="manhattan")
pairwise.adonis2(dist_matrixM ~ Exposure + Site_Exposure, p.adj = "hochberg", data = S_Intra_CERDEM)

dist_matrixM=vegdist(S_Intra_POTPER$SLA,method="manhattan")
pairwise.adonis2(dist_matrixM ~ Exposure + Site_Exposure, p.adj = "hochberg", data = S_Intra_POTPER)



