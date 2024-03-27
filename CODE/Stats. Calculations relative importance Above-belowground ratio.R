##%######################################################%##
#                                                          #
####              adjust data for tests                 ####
#                                                          #
##%######################################################%##
library(stats)
Indices_Ratio_Fixed$CWM_Ratio<- as.numeric(Indices_Ratio_Fixed$CWM_Ratio) 
Indices_Ratio_SpecificMinusFixedA$CWM_Ratio<- as.numeric(Indices_Ratio_SpecificMinusFixedA$CWM_Ratio) 
Indices_Ratio_SpecificMinusFixedB$CWM_Ratio<- as.numeric(Indices_Ratio_SpecificMinusFixedB$CWM_Ratio) 

Indices_Ratio_Fixed$FDis_Ratio<- as.numeric(Indices_Ratio_Fixed$FDis_Ratio) 
Indices_Ratio_SpecificMinusFixedA$FDis_Ratio<- as.numeric(Indices_Ratio_SpecificMinusFixedA$FDis_Ratio) 
Indices_Ratio_SpecificMinusFixedB$FDis_Ratio<- as.numeric(Indices_Ratio_SpecificMinusFixedB$FDis_Ratio) 

Indices_Ratio_Fixed$Exposure<- as.factor(Indices_Ratio_Fixed$Exposure) 
Indices_Ratio_SpecificMinusFixedA$Exposure<- as.factor(Indices_Ratio_SpecificMinusFixedA$Exposure)
Indices_Ratio_SpecificMinusFixedB$Exposure<- as.factor(Indices_Ratio_SpecificMinusFixedB$Exposure)

Indices_Ratio_Allsites$CWM_Ratio<- as.numeric(Indices_Ratio_Allsites$CWM_Ratio) 
Indices_Ratio_Allsites$FDis_Ratio<- as.numeric(Indices_Ratio_Allsites$FDis_Ratio) 
Indices_Ratio_Allsites$Exposure<- as.factor(Indices_Ratio_Allsites$Exposure) 

Indices_Ratio_AllsitesB$CWM_Ratio<- as.numeric(Indices_Ratio_AllsitesB$CWM_Ratio) 
Indices_Ratio_AllsitesB$FDis_Ratio<- as.numeric(Indices_Ratio_AllsitesB$FDis_Ratio) 
Indices_Ratio_AllsitesB$Exposure<- as.factor(Indices_Ratio_AllsitesB$Exposure) 

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
Indices_Ratio_Fixed$FDis_Ratio[Indices_Ratio_Fixed$FDis_Ratio == 0] <- 0.00001 
Indices_Ratio_Fixed$CWM_Ratio[Indices_Ratio_Fixed$CWM_Ratio == 0] <- 0.00001 
Shallow_Ratio_Fixed <- Indices_Ratio_Fixed[ which(Indices_Ratio_Fixed$Depth=='Shallow'), ]

Indices_Ratio_SpecificMinusFixedA$FDis_Ratio[Indices_Ratio_SpecificMinusFixedA$FDis_Ratio == 0] <- 0.00001 
Indices_Ratio_SpecificMinusFixedA$CWM_Ratio <- Indices_Ratio_SpecificMinusFixedA$CWM_Ratio + 0.195 
Shallow_Ratio_IntraA <- Indices_Ratio_SpecificMinusFixedA[ which(Indices_Ratio_SpecificMinusFixedA$Depth=='Shallow'), ]

Indices_Ratio_SpecificMinusFixedB$FDis_Ratio[Indices_Ratio_SpecificMinusFixedB$FDis_Ratio == 0] <- 0.00001 
Indices_Ratio_SpecificMinusFixedB$CWM_Ratio <- Indices_Ratio_SpecificMinusFixedB$CWM_Ratio + 0.723 
Shallow_Ratio_IntraB <- Indices_Ratio_SpecificMinusFixedB[ which(Indices_Ratio_SpecificMinusFixedB$Depth=='Shallow'), ]

Indices_Ratio_Allsites$FDis_Ratio[Indices_Ratio_Allsites$FDis_Ratio == 0] <- 0.00001 
Indices_Ratio_Allsites$CWM_Ratio[Indices_Ratio_Allsites$CWM_Ratio == 0] <- 0.00001
Shallow_Ratio_SpecificA <- Indices_Ratio_Allsites[ which(Indices_Ratio_Allsites$Depth=='Shallow'), ]

Indices_Ratio_AllsitesB$FDis_Ratio[Indices_Ratio_AllsitesB$FDis_Ratio == 0] <- 0.00001 
Indices_Ratio_AllsitesB$CWM_Ratio[Indices_Ratio_AllsitesB$CWM_Ratio == 0] <- 0.00001 
Shallow_Ratio_SpecificB <- Indices_Ratio_AllsitesB[ which(Indices_Ratio_AllsitesB$Depth=='Shallow'), ]


#### FIXED ####

CWMRatio_Fixed <- adonis2(Shallow_Ratio_Fixed$CWM_Ratio ~ Exposure + Site_Exposure, data=Shallow_Ratio_Fixed, perm=999)
CWMRatio_Fixed # significant difference at exposure and site level

FDisRatio_Fixed <- adonis2(Shallow_RootDepth_Fixed$FDis_Ratio ~ Exposure + Site_Exposure , data=Shallow_Ratio_Fixed, perm=999)
FDisRatio_Fixed # significant difference at exposure and site level

#### Intra A ####

CWMRatio_IntraA <- adonis2(Shallow_Ratio_IntraA$CWM_Ratio ~ Exposure + Site_Exposure, data=Shallow_Ratio_IntraA, perm=999)
CWMRatio_IntraA # significant difference at exposure and site level

FDisRatio_IntraA <- adonis2(Shallow_Ratio_IntraA$FDis_Ratio ~ Exposure + Site_Exposure , data=Shallow_Ratio_IntraA, perm=999)
FDisRatio_IntraA # significant difference at exposure and site level

#### Intra B ####

CWMRatio_IntraB <- adonis2(Shallow_Ratio_IntraB$CWM_Ratio ~ Exposure + Site_Exposure, data=Shallow_Ratio_IntraB, perm=999)
CWMRatio_IntraB # significant difference at exposure and site level

FDisRatio_IntraB <- adonis2(Shallow_Ratio_IntraB$FDis_Ratio ~ Exposure + Site_Exposure , data=Shallow_Ratio_IntraB, perm=999)
FDisRatio_IntraB # significant difference at exposure and site level

#### Specific A ####

CWMRatio_SpecificA <- adonis2(Shallow_Ratio_SpecificA$CWM_Ratio ~ Exposure + Site_Exposure, data=Shallow_Ratio_SpecificA, perm=999)
CWMRatio_SpecificA # significant difference at exposure and site level

FDisRatio_SpecificA <- adonis2(Shallow_Ratio_SpecificA$FDis_Ratio ~ Exposure + Site_Exposure , data=Shallow_Ratio_SpecificA, perm=999)
FDisRatio_SpecificA # significant difference at exposure and site level

#### Specific B ####

CWMRatio_SpecificB <- adonis2(Shallow_Ratio_SpecificB$CWM_Ratio ~ Exposure + Site_Exposure, data=Shallow_Ratio_SpecificB, perm=999)
CWMRatio_SpecificB # significant difference at exposure and site level

FDisRatio_SpecificB <- adonis2(Shallow_Ratio_SpecificB$FDis_Ratio ~ Exposure + Site_Exposure , data=Shallow_Ratio_SpecificB, perm=999)
FDisRatio_SpecificB # significant difference at exposure and site level
