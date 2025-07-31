##%######################################################%##
#                                                          #
####              adjust data for tests                 ####
#                                                          #
##%######################################################%##
library(stats)
Indices_Ratio_Fixed$CWM_Ratio<- as.numeric(Indices_Ratio_Fixed$CWM_Ratio) 
Indices_Ratio_SpecificMinusFixedB$CWM_Ratio<- as.numeric(Indices_Ratio_SpecificMinusFixedB$CWM_Ratio) 
<<<<<<< HEAD

Indices_Ratio_Fixed$FDis_Ratio<- as.numeric(Indices_Ratio_Fixed$FDis_Ratio) 
Indices_Ratio_SpecificMinusFixedB$FDis_Ratio<- as.numeric(Indices_Ratio_SpecificMinusFixedB$FDis_Ratio) 

Indices_Ratio_Fixed$Exposure<- as.factor(Indices_Ratio_Fixed$Exposure) 
Indices_Ratio_SpecificMinusFixedB$Exposure<- as.factor(Indices_Ratio_SpecificMinusFixedB$Exposure)

Indices_Ratio_AllsitesB$CWM_Ratio<- as.numeric(Indices_Ratio_AllsitesB$CWM_Ratio) 
Indices_Ratio_AllsitesB$FDis_Ratio<- as.numeric(Indices_Ratio_AllsitesB$FDis_Ratio) 
=======
Indices_Ratio_AllsitesB$CWM_Ratio<- as.numeric(Indices_Ratio_AllsitesB$CWM_Ratio) 

Indices_Ratio_Fixed$Exposure<- as.factor(Indices_Ratio_Fixed$Exposure) 
Indices_Ratio_SpecificMinusFixedB$Exposure<- as.factor(Indices_Ratio_SpecificMinusFixedB$Exposure)
>>>>>>> 7634eecad9e86c76a8fd357bbe1b1edf32e0c4cc
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
<<<<<<< HEAD
Indices_Ratio_Fixed$FDis_Ratio[Indices_Ratio_Fixed$FDis_Ratio == 0] <- 0.00001 
Indices_Ratio_Fixed$CWM_Ratio[Indices_Ratio_Fixed$CWM_Ratio == 0] <- 0.00001 
Shallow_Ratio_Fixed <- Indices_Ratio_Fixed[ which(Indices_Ratio_Fixed$Depth=='Shallow'), ]

Indices_Ratio_SpecificMinusFixedB$FDis_Ratio[Indices_Ratio_SpecificMinusFixedB$FDis_Ratio == 0] <- 0.00001 
Indices_Ratio_SpecificMinusFixedB$CWM_Ratio <- Indices_Ratio_SpecificMinusFixedB$CWM_Ratio + 0.723 
Shallow_Ratio_IntraB <- Indices_Ratio_SpecificMinusFixedB[ which(Indices_Ratio_SpecificMinusFixedB$Depth=='Shallow'), ]

Indices_Ratio_AllsitesB$FDis_Ratio[Indices_Ratio_AllsitesB$FDis_Ratio == 0] <- 0.00001 
=======

Indices_Ratio_Fixed$CWM_Ratio[Indices_Ratio_Fixed$CWM_Ratio == 0] <- 0.00001 
Shallow_Ratio_Fixed <- Indices_Ratio_Fixed[ which(Indices_Ratio_Fixed$Depth=='Shallow'), ]

Indices_Ratio_SpecificMinusFixedB$CWM_Ratio <- Indices_Ratio_SpecificMinusFixedB$CWM_Ratio + 0.723 
Shallow_Ratio_IntraB <- Indices_Ratio_SpecificMinusFixedB[ which(Indices_Ratio_SpecificMinusFixedB$Depth=='Shallow'), ]

>>>>>>> 7634eecad9e86c76a8fd357bbe1b1edf32e0c4cc
Indices_Ratio_AllsitesB$CWM_Ratio[Indices_Ratio_AllsitesB$CWM_Ratio == 0] <- 0.00001 
Shallow_Ratio_SpecificB <- Indices_Ratio_AllsitesB[ which(Indices_Ratio_AllsitesB$Depth=='Shallow'), ]


#### FIXED ####
<<<<<<< HEAD

CWMRatio_Fixed <- adonis2(Shallow_Ratio_Fixed$CWM_Ratio ~ Exposure + Site_Exposure, data=Shallow_Ratio_Fixed, perm=999)
CWMRatio_Fixed # significant difference at exposure and site level

FDisRatio_Fixed <- adonis2(Shallow_Ratio_Fixed$FDis_Ratio ~ Exposure + Site_Exposure , data=Shallow_Ratio_Fixed, perm=999)
FDisRatio_Fixed # significant difference at exposure and site level

#### Intra B ####

CWMRatio_IntraB <- adonis2(Shallow_Ratio_IntraB$CWM_Ratio ~ Exposure + Site_Exposure, data=Shallow_Ratio_IntraB, perm=999)
CWMRatio_IntraB # significant difference at exposure and site level

FDisRatio_IntraB <- adonis2(Shallow_Ratio_IntraB$FDis_Ratio ~ Exposure + Site_Exposure , data=Shallow_Ratio_IntraB, perm=999)
FDisRatio_IntraB # significant difference at exposure and site level

#### Specific B ####

CWMRatio_SpecificB <- adonis2(Shallow_Ratio_SpecificB$CWM_Ratio ~ Exposure + Site_Exposure, data=Shallow_Ratio_SpecificB, perm=999)
CWMRatio_SpecificB # significant difference at exposure and site level

FDisRatio_SpecificB <- adonis2(Shallow_Ratio_SpecificB$FDis_Ratio ~ Exposure + Site_Exposure , data=Shallow_Ratio_SpecificB, perm=999)
FDisRatio_SpecificB # significant difference at exposure and site level
=======
CWMRatio_Fixed <- adonis2(Shallow_Ratio_Fixed$CWM_Ratio ~ Exposure + Site_Exposure, data=Shallow_Ratio_Fixed, perm=999)
CWMRatio_Fixed # significant difference at exposure and site level

#### Intra B ####
CWMRatio_IntraB <- adonis2(Shallow_Ratio_IntraB$CWM_Ratio ~ Exposure + Site_Exposure, data=Shallow_Ratio_IntraB, perm=999)
CWMRatio_IntraB # significant difference at exposure and site level

#### Specific B ####
CWMRatio_SpecificB <- adonis2(Shallow_Ratio_SpecificB$CWM_Ratio ~ Exposure + Site_Exposure, data=Shallow_Ratio_SpecificB, perm=999)
CWMRatio_SpecificB # significant difference at exposure and site level
>>>>>>> 7634eecad9e86c76a8fd357bbe1b1edf32e0c4cc
