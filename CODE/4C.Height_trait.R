##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##
library(rstatix)

### Exposed_Shallow
Indices_height_Allsites[c(1:31),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(1:31),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(1:31),c(6)])) # marginally significant

### Exposed_Deep
Indices_height_Allsites[c(31:57),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(31:57),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(31:57),c(6)])) # significant

### Semi_Shallow
Indices_height_Allsites[c(58:87),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(58:87),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(58:87),c(6)])) # significant

### Semi_Deep
Indices_height_Allsites[c(88:117),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(88:117),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(88:117),c(6)])) # significant

### Pojo_Deep
Indices_height_Allsites[c(118:125),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(118:125),c(5)])) # non-significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(118:125),c(6)])) # significant

### Pojo_Shallow
Indices_height_Allsites[c(126:155),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(126:155),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(126:155),c(6)])) # significant

### Sheltered_Shallow
Indices_height_Allsites[c(156:185),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(156:185),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(156:185),c(6)])) # significant


##%######################################################%##
#                                                          #
####                Homogeneity tests                   ####
#                                                          #
##%######################################################%##
library(stats)
Indices_height_Allsites$CWM_Height<- as.numeric(Indices_height_Allsites$CWM_Height) 
Indices_height_Allsites$FDis_Height<- as.numeric(Indices_height_Allsites$FDis_Height) 
Indices_height_Allsites$Exposure<- as.factor(Indices_height_Allsites$Exposure) 

fligner.test(Indices_height_Allsites$CWM_Height ~ Indices_height_Allsites$Exposure_Depth) # signifcant
fligner.test(Indices_height_Allsites$FDis_Height ~ Indices_height_Allsites$Exposure_Depth) # signifcant

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

# Permanova can´t handle zero`s
Indices_height_Allsites$FDis_Height[Indices_height_Allsites$FDis_Height == 0] <- 0.00001 
Shallow_Height <- Indices_height_Allsites[ which(Indices_height_Allsites$Depth=='Shallow'), ]
Deep_Height <- Indices_height_Allsites[ which(Indices_height_Allsites$Depth=='Deep'), ]
######### Shallow sites #########
# https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/permanova/
# Doesn´t assume normality or homogeneity of variance

CWMHeight_S <- adonis2(Shallow_Height$CWM_Height ~ Exposure + Site_Exposure, data=Shallow_Height, perm=999)
CWMHeight_S # significant difference at exposure and site level

FDisHeight_S <- adonis2(Shallow_Height$FDis_Height ~ Exposure + Site_Exposure , data=Shallow_Height, perm=999)
FDisHeight_S # significant difference at exposure and site level

######### Deep sites #########

CWMHeight_D <- adonis2(Deep_Height$CWM_Height ~ Exposure + Site_Exposure, data=Deep_Height, perm=999)
CWMHeight_D # significant difference at exposure and site level

FDisHeight_D <- adonis2(Deep_Height$FDis_Height ~ Exposure + Site_Exposure , data=Deep_Height, perm=999)
FDisHeight_D # significant difference at exposure and site level

##%######################################################%##
#                                                          #
####                 Post-hoc adonis test               ####   
#                                                          #
##%######################################################%##
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

######### Shallow sites #########

Shallow_HeightA <- Shallow_Height[c(1:31, 32:61),] #Exposed and Semi
Shallow_HeightB <- Shallow_Height[c(32:61, 92:121),] #Semi and sheltered
Shallow_HeightC <- Shallow_Height[c(92:121,62:91),] #Sheltered and Pojo
Shallow_HeightD <- Shallow_Height[c(1:31, 92:121),] #Exposed and sheltered

dist_matrixA=vegdist(Shallow_HeightA$CWM_Height,method="bray")
pairwise.adonis2(dist_matrixA ~ Exposure + Site_Exposure, data = Shallow_HeightA)

dist_matrixB=vegdist(Shallow_HeightB$CWM_Height,method="bray")
pairwise.adonis2(dist_matrixB ~ Exposure + Site_Exposure, data = Shallow_HeightB)

dist_matrixC=vegdist(Shallow_HeightC $CWM_Height,method="bray")
pairwise.adonis2(dist_matrixC ~ Exposure + Site_Exposure, data = Shallow_HeightC)


dist_matrixA2=vegdist(Shallow_HeightA$FDis_Height,method="bray")
pairwise.adonis2(dist_matrixA2 ~ Exposure + Site_Exposure, data = Shallow_HeightA)

dist_matrixB2=vegdist(Shallow_HeightB$FDis_Height,method="bray")
pairwise.adonis2(dist_matrixB2 ~ Exposure + Site_Exposure, data = Shallow_HeightB)

dist_matrixC2=vegdist(Shallow_HeightC $FDis_Height,method="bray")
pairwise.adonis2(dist_matrixC2 ~ Exposure + Site_Exposure, data = Shallow_HeightC)







