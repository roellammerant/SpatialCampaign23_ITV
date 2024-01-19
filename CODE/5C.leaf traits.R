##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##
library(rstatix)

### Exposed_Shallow
Indices_leaf_Allsites[c(1:31),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(1:31),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(1:31),c(3)])) # significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(1:31),c(6)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(1:31),c(7)])) # significant

### Exposed_Deep
Indices_leaf_Allsites[c(31:57),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(31:57),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(31:57),c(3)])) # significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(31:57),c(6)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(31:57),c(7)])) # significant

### Semi_Shallow
Indices_leaf_Allsites[c(58:87),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(58:87),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(58:87),c(3)])) # significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(58:87),c(6)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(58:87),c(7)])) # significant

### Semi_Deep
Indices_leaf_Allsites[c(88:117),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(88:117),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(88:117),c(3)])) # significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(88:117),c(6)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(88:117),c(7)])) # significant

### Pojo_Deep
Indices_leaf_Allsites[c(118:125),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(118:125),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(118:125),c(3)])) # non-significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(118:125),c(6)])) # non-significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(118:125),c(7)])) # non-significant

### Pojo_Shallow
Indices_leaf_Allsites[c(126:155),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(126:155),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(126:155),c(3)])) # significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(126:155),c(6)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(126:155),c(7)])) # significant

### Sheltered_Shallow
Indices_leaf_Allsites[c(156:185),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(156:185),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(156:185),c(3)])) # significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(156:185),c(6)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(156:185),c(7)])) # significant


##%######################################################%##
#                                                          #
####                Homogeneity tests                   ####
#                                                          #
##%######################################################%##
library(stats)
Indices_leaf_Allsites$CWM_SLA <- as.numeric(Indices_leaf_Allsites$CWM_SLA) 
Indices_leaf_Allsites$FDis_SLA<- as.numeric(Indices_leaf_Allsites$FDis_SLA) 
Indices_leaf_Allsites$CWM_LAP <- as.numeric(Indices_leaf_Allsites$CWM_LAP) 
Indices_leaf_Allsites$FDis_LAP<- as.numeric(Indices_leaf_Allsites$FDis_LAP) 
Indices_leaf_Allsites$Exposure<- as.factor(Indices_leaf_Allsites$Exposure) 

fligner.test(Indices_leaf_Allsites$CWM_SLA ~ Indices_height_Allsites$Exposure_Depth) # signifcant
fligner.test(Indices_leaf_Allsites$FDis_SLA ~ Indices_height_Allsites$Exposure_Depth) # signifcant

fligner.test(Indices_leaf_Allsites$CWM_LAP ~ Indices_height_Allsites$Exposure_Depth) # signifcant
fligner.test(Indices_leaf_Allsites$FDis_LAP ~ Indices_height_Allsites$Exposure_Depth) # signifcant

##%######################################################%##
#                                                          #
####      Non-parametric test + heteroscedasticity     #####
#                                                          #
##%######################################################%##


##%######################################################%##
#                                                          #
####            Non parametric permanova                ####   
####                                                    ####
#                                                          #
##%######################################################%##
Shallow_Leaf <- Indices_leaf_Allsites[ which(Indices_leaf_Allsites$Depth=='Shallow'), ]
Deep_Leaf <- Indices_leaf_Allsites[ which(Indices_leaf_Allsites$Depth=='Deep'), ]

######### Shallow sites #########
library(lme4)
library(predictmeans)
# https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/permanova/
# Doesn´t assume normality or homogeneity of variance

fm <- lmer(FDis_SLA ~ Exposure +(1|Site_Exposure), data=Shallow_Leaf)
permanova.lmer(fm) # non-significant

fm2 <- lmer(CWM_SLA ~ Exposure +(1|Site_Exposure), data=Shallow_Leaf)
permanova.lmer(fm2) # marginally significant

fm <- lmer(FDis_LAP ~ Exposure +(1|Site_Exposure), data=Shallow_Leaf)
permanova.lmer(fm) # significant

fm2 <- lmer(CWM_LAP ~ Exposure +(1|Site_Exposure), data=Shallow_Leaf)
permanova.lmer(fm2) # non-significant


######### Deep sites #########

fmD <- lmer(FDis_SLA ~ Exposure +(1|Site_Exposure), data=Deep_Leaf)
permanova.lmer(fmD) # non-significant

fmD2 <- lmer(CWM_SLA ~ Exposure +(1|Site_Exposure), data=Deep_Leaf)
permanova.lmer(fmD2) # significant

fmD <- lmer(FDis_LAP ~ Exposure +(1|Site_Exposure), data=Deep_Leaf)
permanova.lmer(fmD) # non-significant

fmD2 <- lmer(CWM_LAP ~ Exposure +(1|Site_Exposure), data=Deep_Leaf)
permanova.lmer(fmD2) # significant
