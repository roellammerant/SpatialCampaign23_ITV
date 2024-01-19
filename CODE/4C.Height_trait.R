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
library(rstatix)
library(ggpubr)

##%######################################################%##
#                                                          #
####            Non parametric permanova                ####   
####                                                    ####
#                                                          #
##%######################################################%##
Shallow_Height <- Indices_height_Allsites[ which(Indices_height_Allsites$Depth=='Shallow'), ]
Deep_Height <- Indices_height_Allsites[ which(Indices_height_Allsites$Depth=='Deep'), ]

######### Shallow sites #########
library(lme4)
library(predictmeans)
# https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/permanova/
# Doesn´t assume normality or homogeneity of variance

fm <- lmer(FDis_Height ~ Exposure +(1|Site_Exposure), data=Shallow_Height)
permanova.lmer(fm) # non-significant

fm2 <- lmer(CWM_Height ~ Exposure +(1|Site_Exposure), data=Shallow_Height)
permanova.lmer(fm2) # significant

######### Deep sites #########

fmD <- lmer(FDis_Height ~ Exposure +(1|Site_Exposure), data=Deep_Height)
permanova.lmer(fmD) # non-significant

fmD2 <- lmer(CWM_Height ~ Exposure +(1|Site_Exposure), data=Deep_Height)
permanova.lmer(fmD2) # significant


##%######################################################%##
#                                                          #
####                 Mann-Whitney U test                ####   
#                                                          #
##%######################################################%##

######### Shallow sites #########

Shallow_HeightA <- Shallow_Height[c(1:31, 32:61),] #Exposed and Semi
Shallow_HeightB <- Shallow_Height[c(32:61, 92:121),] #Semi and sheltered
Shallow_HeightC <- Shallow_Height[c(92:121,62:91),] #Sheltered and Pojo
Shallow_HeightD <- Shallow_Height[c(1:31, 92:121),] 
Shallow_HeightE <- Shallow_Height[c(11:30),]

library(coin)
library(rcompanion)
rcompanion::pairwisePermutationTest(CWM_Height ~ Exposure +(1|Site_Exposure), data=Deep_Height)
