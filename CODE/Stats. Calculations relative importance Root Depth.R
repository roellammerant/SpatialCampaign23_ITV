##%######################################################%##
#                                                          #
####              adjust data for tests                 ####
#                                                          #
##%######################################################%##
library(stats)
Indices_RootDepth_Fixed$CWM_RootDepth<- as.numeric(Indices_RootDepth_Fixed$CWM_RootDepth) 
Indices_RootDepth_SpecificMinusFixedA$CWM_RootDepth<- as.numeric(Indices_RootDepth_SpecificMinusFixedA$CWM_RootDepth) 
Indices_RootDepth_SpecificMinusFixedB$CWM_RootDepth<- as.numeric(Indices_RootDepth_SpecificMinusFixedB$CWM_RootDepth) 

Indices_RootDepth_Fixed$FDis_RootDepth<- as.numeric(Indices_RootDepth_Fixed$FDis_RootDepth) 
Indices_RootDepth_SpecificMinusFixedA$FDis_RootDepth<- as.numeric(Indices_RootDepth_SpecificMinusFixedA$FDis_RootDepth) 
Indices_RootDepth_SpecificMinusFixedB$FDis_RootDepth<- as.numeric(Indices_RootDepth_SpecificMinusFixedB$FDis_RootDepth) 

Indices_RootDepth_Fixed$Exposure<- as.factor(Indices_RootDepth_Fixed$Exposure) 
Indices_RootDepth_SpecificMinusFixedA$Exposure<- as.factor(Indices_RootDepth_SpecificMinusFixedA$Exposure)
Indices_RootDepth_SpecificMinusFixedB$Exposure<- as.factor(Indices_RootDepth_SpecificMinusFixedB$Exposure)

Indices_RootDepth_Allsites$CWM_RootDepth<- as.numeric(Indices_RootDepth_Allsites$CWM_RootDepth) 
Indices_RootDepth_Allsites$FDis_RootDepth<- as.numeric(Indices_RootDepth_Allsites$FDis_RootDepth) 
Indices_RootDepth_Allsites$Exposure<- as.factor(Indices_RootDepth_Allsites$Exposure) 

Indices_RootDepth_AllsitesB$CWM_RootDepth<- as.numeric(Indices_RootDepth_AllsitesB$CWM_RootDepth) 
Indices_RootDepth_AllsitesB$FDis_RootDepth<- as.numeric(Indices_RootDepth_AllsitesB$FDis_RootDepth) 
Indices_RootDepth_AllsitesB$Exposure<- as.factor(Indices_RootDepth_AllsitesB$Exposure) 

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
Indices_RootDepth_Fixed$FDis_RootDepth[Indices_RootDepth_Fixed$FDis_RootDepth == 0] <- 0.00001 
Indices_RootDepth_Fixed$CWM_RootDepth[Indices_RootDepth_Fixed$CWM_RootDepth == 0] <- 0.00001 
Shallow_RootDepth_Fixed <- Indices_RootDepth_Fixed[ which(Indices_RootDepth_Fixed$Depth=='Shallow'), ]

Indices_RootDepth_SpecificMinusFixedA$FDis_RootDepth[Indices_RootDepth_SpecificMinusFixedA$FDis_RootDepth == 0] <- 0.00001 
Indices_RootDepth_SpecificMinusFixedA$CWM_RootDepth <- abs(Indices_RootDepth_SpecificMinusFixedA$CWM_RootDepth) 
Indices_RootDepth_SpecificMinusFixedA$CWM_RootDepth[Indices_RootDepth_SpecificMinusFixedA$CWM_RootDepth == 0] <- 0.00001 
Shallow_RootDepth_IntraA <- Indices_RootDepth_SpecificMinusFixedA[ which(Indices_RootDepth_SpecificMinusFixedA$Depth=='Shallow'), ]

Indices_RootDepth_SpecificMinusFixedB$FDis_RootDepth[Indices_RootDepth_SpecificMinusFixedB$FDis_RootDepth == 0] <- 0.00001 
Indices_RootDepth_SpecificMinusFixedB$CWM_RootDepth <- abs(Indices_RootDepth_SpecificMinusFixedB$CWM_RootDepth)
Indices_RootDepth_SpecificMinusFixedB$CWM_RootDepth[Indices_RootDepth_SpecificMinusFixedB$CWM_RootDepth == 0] <- 0.00001 
Shallow_RootDepth_IntraB <- Indices_RootDepth_SpecificMinusFixedB[ which(Indices_RootDepth_SpecificMinusFixedB$Depth=='Shallow'), ]

Indices_RootDepth_Allsites$FDis_RootDepth[Indices_RootDepth_Allsites$FDis_RootDepth == 0] <- 0.00001 
Indices_RootDepth_Allsites$CWM_RootDepth[Indices_RootDepth_Allsites$CWM_RootDepth == 0] <- 0.00001
Shallow_RootDepth_SpecificA <- Indices_RootDepth_Allsites[ which(Indices_RootDepth_Allsites$Depth=='Shallow'), ]

Indices_RootDepth_AllsitesB$FDis_RootDepth[Indices_RootDepth_AllsitesB$FDis_RootDepth == 0] <- 0.00001 
Indices_RootDepth_AllsitesB$CWM_RootDepth[Indices_RootDepth_AllsitesB$CWM_RootDepth == 0] <- 0.00001 
Shallow_RootDepth_SpecificB <- Indices_RootDepth_AllsitesB[ which(Indices_RootDepth_AllsitesB$Depth=='Shallow'), ]


#### FIXED ####

CWMRootDepth_Fixed <- adonis2(Shallow_RootDepth_Fixed$CWM_RootDepth ~ Exposure + Site_Exposure, data=Shallow_RootDepth_Fixed, perm=999)
CWMRootDepth_Fixed # significant difference at exposure and site level

FDisRootDepth_Fixed <- adonis2(Shallow_RootDepth_Fixed$FDis_RootDepth ~ Exposure + Site_Exposure , data=Shallow_RootDepth_Fixed, perm=999)
FDisRootDepth_Fixed # significant difference at exposure and site level

#### Intra A ####

CWMRootDepth_IntraA <- adonis2(Shallow_RootDepth_IntraA$CWM_RootDepth ~ Exposure + Site_Exposure, data=Shallow_RootDepth_IntraA, perm=999)
CWMRootDepth_IntraA # significant difference at exposure and site level

FDisRootDepth_IntraA <- adonis2(Shallow_RootDepth_IntraA$FDis_RootDepth ~ Exposure + Site_Exposure , data=Shallow_RootDepth_IntraA, perm=999)
FDisRootDepth_IntraA # significant difference at exposure and site level

#### Intra B ####

CWMRootDepth_IntraB <- adonis2(Shallow_RootDepth_IntraB$CWM_RootDepth ~ Exposure + Site_Exposure, data=Shallow_RootDepth_IntraB, perm=999)
CWMRootDepth_IntraB # significant difference at exposure and site level

FDisRootDepth_IntraB <- adonis2(Shallow_RootDepth_IntraB$FDis_RootDepth ~ Exposure + Site_Exposure , data=Shallow_RootDepth_IntraB, perm=999)
FDisRootDepth_IntraB # significant difference at exposure and site level

#### Specific A ####

CWMRootDepth_SpecificA <- adonis2(Shallow_RootDepth_SpecificA$CWM_RootDepth ~ Exposure + Site_Exposure, data=Shallow_RootDepth_SpecificA, perm=999)
CWMRootDepth_SpecificA # significant difference at exposure and site level

FDisRootDepth_SpecificA <- adonis2(Shallow_RootDepth_SpecificA$FDis_RootDepth ~ Exposure + Site_Exposure , data=Shallow_RootDepth_SpecificA, perm=999)
FDisRootDepth_SpecificA # significant difference at exposure and site level

#### Specific B ####

CWMRootDepth_SpecificB <- adonis2(Shallow_RootDepth_SpecificB$CWM_RootDepth ~ Exposure + Site_Exposure, data=Shallow_RootDepth_SpecificB, perm=999)
CWMRootDepth_SpecificB # significant difference at exposure and site level

FDisRootDepth_SpecificB <- adonis2(Shallow_RootDepth_SpecificB$FDis_RootDepth ~ Exposure + Site_Exposure , data=Shallow_RootDepth_SpecificB, perm=999)
FDisRootDepth_SpecificB # significant difference at exposure and site level
