##%######################################################%##
#                                                          #
####               Species level height                 ####
#                                                          #
##%######################################################%##
Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")
TraitData_exp <- Spatial_Campaign_data_exp[,c(1:5)]
names(TraitData_exp)[5]<-paste(c("Height"))
names(TraitData_exp)[2]<-paste(c("Site_number"))

Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")
TraitData_semi <- Spatial_Campaign_data_semi[,c(1:5)]
names(TraitData_semi)[5]<-paste(c("Height"))
names(TraitData_semi)[2]<-paste(c("Site_number"))


Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")
TraitData_shel <- Spatial_Campaign_data_shel[,c(1:5)]
names(TraitData_shel)[5]<-paste(c("Height"))
names(TraitData_shel)[2]<-paste(c("Site_number"))
TraitData_shel[is.na(TraitData_shel)] = 0


Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")
TraitData_pojo <- Spatial_Campaign_data_pojo[,c(1:5)]
names(TraitData_pojo)[5]<-paste(c("Height"))
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
Heighttrait_all <- rbind(TraitData_exp, TraitData_semi, TraitData_shel, TraitData_pojo)
Heighttrait_Shallow <- subset(Heighttrait_all, Depth=="S")
Heighttrait_MYRSPI <- subset(Heighttrait_Shallow, Species=="MYRSPI")
Heighttrait_STUPEC <- subset(Heighttrait_Shallow, Species=="STUPEC")
Heighttrait_CERDEM <- subset(Heighttrait_Shallow, Species=="CERDEM")
Heighttrait_POTPER <- subset(Heighttrait_Shallow, Species=="POTPER")
HeightS <- rbind(Heighttrait_MYRSPI,Heighttrait_STUPEC,Heighttrait_CERDEM, Heighttrait_POTPER)
HeightS$Exposure <- factor(paste(HeightS$Site_group, 
                                 HeightS$Species, sep = "_"),
                                                 levels = c(
                                                   "exposed_MYRSPI", "exposed_STUPEC","exposed_POTPER",
                                                   "semi_MYRSPI", "semi_STUPEC","semi_CERDEM", "semi_POTPER",
                                                   "Pojo_MYRSPI", "Pojo_CERDEM", 
                                                   "sheltered_MYRSPI", "sheltered_STUPEC", "sheltered_CERDEM", "sheltered_POTPER"
                                                 ))

png(
  "output_plot/Height_SpeciesLevel.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(1,1))

par(mar = c(3, 5, 2, 3))

##%######################################################%##
#                                                          #
####                    Intra height                    ####
#                                                          #
##%######################################################%##

HeightS$Exposure <- factor(HeightS$Exposure,
                                            levels=c(
                                              "exposed_MYRSPI", "exposed_STUPEC","exposed_POTPER",
                                              "semi_MYRSPI", "semi_STUPEC", "semi_POTPER", "semi_CERDEM",
                                              "sheltered_MYRSPI", "sheltered_STUPEC", "sheltered_POTPER",  "sheltered_CERDEM",
                                              "Pojo_MYRSPI", "Pojo_CERDEM"))







par(mar = c(3, 5, 2, 3))
boxplot(Height ~ Exposure, data = HeightS,
        boxwex = 0.5, col = c("cyan3","burlywood","grey","cyan3","burlywood","grey","darkorange","cyan3","burlywood","grey","darkorange", "cyan3", "darkorange"),
        main = NA,
        xlab = NA, ylab = "Height (cm)",at = c(1, 2, 3, 5, 6, 7,8, 10,11,12,13, 15,16),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(Height ~ Exposure,
           data = HeightS,
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
####                    Stats height                    ####
#                                                          #
##%######################################################%##


##### Normality #####

library(rstatix)

### Exposed
HeightS[c(1:8),c(7)]
mshapiro_test(HeightS[c(1:8),c(5)]) # MYRSPI significant
HeightS[c(40:60),c(7)]
mshapiro_test(HeightS[c(40:60),c(5)]) # STUPEC significant
HeightS[c(138:152),c(7)]
mshapiro_test(HeightS[c(138:152),c(5)]) # POTPER significant

### Semi
HeightS[c(9:24),c(7)]
mshapiro_test(HeightS[c(9:24),c(5)]) # MYRSPI non-significant
HeightS[c(61:75),c(7)]
mshapiro_test(HeightS[c(61:75),c(5)]) # STUPEC non-significant
HeightS[c(85:88),c(7)]
mshapiro_test(HeightS[c(85:88),c(5)]) # CERDEM non-significant
HeightS[c(153:169),c(7)]
mshapiro_test(HeightS[c(153:169),c(5)]) # POTPER significant

### sheltered
HeightS[c(25:33),c(7)]
mshapiro_test(HeightS[c(25:33),c(5)]) # MYRSPI significant
HeightS[c(76:84),c(7)]
mshapiro_test(HeightS[c(76:84),c(5)]) # STUPEC non-significant
HeightS[c(91:112),c(7)]
mshapiro_test(HeightS[c(91:112),c(5)]) # CERDEM significant
HeightS[c(170:175),c(7)]
mshapiro_test(HeightS[c(170:175),c(5)]) # POTPER Non-significant

### Pojo
HeightS[c(34:39),c(7)]
mshapiro_test(HeightS[c(34:39),c(5)]) # MYRSPI non-significant
HeightS[c(113:137),c(7)]
mshapiro_test(HeightS[c(113:137),c(5)]) # CERDEM significant


#### For every species, not all data is normal distributed

##### Homogeneity #####

HeightS$Site_Exposure <- paste(HeightS$Site_number, HeightS$Exposure, sep="_")

H_Intra_Myrspi <- HeightS[ which(HeightS$Species=='MYRSPI'), ]
H_Intra_STUPEC <- HeightS[ which(HeightS$Species=='STUPEC'), ]
H_Intra_CERDEM <- HeightS[ which(HeightS$Species=='CERDEM'), ]
H_Intra_POTPER <- HeightS[ which(HeightS$Species=='POTPER'), ]

fligner.test(H_Intra_Myrspi$Height ~ H_Intra_Myrspi$Site_group) # non-significant
fligner.test(H_Intra_STUPEC$Height ~ H_Intra_STUPEC$Site_group) # non-significant
fligner.test(H_Intra_CERDEM$Height ~ H_Intra_CERDEM$Site_group) # non-significant
fligner.test(H_Intra_POTPER$Height ~ H_Intra_POTPER$Site_group) # significant


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

MYRSPI_H <- adonis2(H_Intra_Myrspi$Height ~ Exposure + Site_Exposure, data=H_Intra_Myrspi, perm=999)
MYRSPI_H # significant difference at exposure and site level

STUPEC_H <- adonis2(H_Intra_STUPEC$Height ~ Exposure + Site_Exposure, data=H_Intra_STUPEC, perm=999)
STUPEC_H # significant difference at site level

CERDEM_H <- adonis2(H_Intra_CERDEM$Height ~ Exposure + Site_Exposure, data=H_Intra_CERDEM, perm=999)
CERDEM_H # significant difference at site level

POTPER_H <- adonis2(H_Intra_POTPER$Height ~ Exposure + Site_Exposure, data=H_Intra_POTPER, perm=999)
POTPER_H # significant difference at exposure and site level

##%######################################################%##
#                                                          #
####                 Post-hoc adonis test               ####   
#                                                          #
##%######################################################%##
library(devtools)
library(pairwiseAdonis)

######### Across exposure categories #########

dist_matrixM=vegdist(H_Intra_Myrspi$Height,method="manhattan")
pairwise.adonis2(dist_matrixM ~ Exposure + Site_Exposure, p.adj = "hochberg", data = H_Intra_Myrspi)

dist_matrixS=vegdist(H_Intra_STUPEC$Height,method="manhattan")
pairwise.adonis2(dist_matrixS ~ Exposure + Site_Exposure, p.adj = "hochberg", data = H_Intra_STUPEC)

dist_matrixC=vegdist(H_Intra_CERDEM$Height,method="manhattan")
pairwise.adonis2(dist_matrixC ~ Exposure + Site_Exposure, p.adj = "hochberg", data = H_Intra_CERDEM)

dist_matrixC=vegdist(H_Intra_POTPER$Height,method="manhattan")
pairwise.adonis2(dist_matrixC ~ Exposure + Site_Exposure, p.adj = "hochberg", data = H_Intra_POTPER)








