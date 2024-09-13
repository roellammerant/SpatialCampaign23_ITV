##%######################################################%##
#                                                          #
####                       PC1                          ####
#                                                          #
##%######################################################%##


##%######################################################%##
#                                                          #
####              adjust data for tests                 ####
#                                                          #
##%######################################################%##
library(stats)
Indices_PCA_Fixed$CWM_PC1<- as.numeric(Indices_PCA_Fixed$CWM_PC1) 
Indices_PCA_SpecificMinusFixedB$CWM_PC1<- as.numeric(Indices_PCA_SpecificMinusFixedB$CWM_PC1) 

Indices_PCA_Fixed$FDis_PC1<- as.numeric(Indices_PCA_Fixed$FDis_PC1) 
Indices_PCA_SpecificMinusFixedB$FDis_PC1<- as.numeric(Indices_PCA_SpecificMinusFixedB$FDis_PC1) 

Indices_PCA_Fixed$Exposure<- as.factor(Indices_PCA_Fixed$Exposure) 
Indices_PCA_SpecificMinusFixedB$Exposure<- as.factor(Indices_PCA_SpecificMinusFixedB$Exposure)

Indices_PCA_AllsitesB$CWM_PC1<- as.numeric(Indices_PCA_AllsitesB$CWM_PC1) 
Indices_PCA_AllsitesB$FDis_PC1<- as.numeric(Indices_PCA_AllsitesB$FDis_PC1) 
Indices_PCA_AllsitesB$Exposure<- as.factor(Indices_PCA_AllsitesB$Exposure) 

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
Indices_PCA_Fixed$FDis_PC1[Indices_PCA_Fixed$FDis_PC1 == 0] <- 0.00001 
Indices_PCA_Fixed$CWM_PC1 <- Indices_PCA_Fixed$CWM_PC1 + 1.8510

Indices_PCA_SpecificMinusFixedB$FDis_PC1[Indices_PCA_SpecificMinusFixedB$FDis_PC1 == 0] <- 0.00001 
Indices_PCA_SpecificMinusFixedB$CWM_PC1 <- Indices_PCA_SpecificMinusFixedB$CWM_PC1 + 1.93

Indices_PCA_AllsitesB$FDis_PC1[Indices_PCA_AllsitesB$FDis_PC1 == 0] <- 0.00001 
Indices_PCA_AllsitesB$CWM_PC1 <- Indices_PCA_AllsitesB$CWM_PC1 + 3.1392

#### FIXED ####

CWMPC1_Fixed <- adonis2(Indices_PCA_Fixed$CWM_PC1 ~ Exposure + Site_Exposure, data=Indices_PCA_Fixed, perm=999)
CWMPC1_Fixed # significant difference at exposure and site level

FDisPC1_Fixed <- adonis2(Indices_PCA_Fixed$FDis_PC1 ~ Exposure + Site_Exposure , data=Indices_PCA_Fixed, perm=999)
FDisPC1_Fixed # significant difference at exposure and site level

#### Intra B ####

CWMPC1_IntraB <- adonis2(Indices_PCA_SpecificMinusFixedB$CWM_PC1 ~ Exposure + Site_Exposure, data=Indices_PCA_SpecificMinusFixedB, perm=999)
CWMPC1_IntraB # significant difference at exposure and site level

FDisPC1_IntraB <- adonis2(Indices_PCA_SpecificMinusFixedB$FDis_PC1 ~ Exposure + Site_Exposure , data=Indices_PCA_SpecificMinusFixedB, perm=999)
FDisPC1_IntraB # significant difference at exposure and site level

#### Specific B ####

CWMPC1_SpecificB <- adonis2(Indices_PCA_AllsitesB$CWM_PC1 ~ Exposure + Site_Exposure, data=Indices_PCA_AllsitesB, perm=999)
CWMPC1_SpecificB # significant difference at exposure and site level

FDisPC1_SpecificB <- adonis2(Indices_PCA_AllsitesB$CWM_PC1 ~ Exposure + Site_Exposure , data=Indices_PCA_AllsitesB, perm=999)
FDisPC1_SpecificB # significant difference at exposure and site level

##%######################################################%##
#                                                          #
####                       PC2                          ####
#                                                          #
##%######################################################%##




##%######################################################%##
#                                                          #
####              adjust data for tests                 ####
#                                                          #
##%######################################################%##
Indices_PCA_Fixed$CWM_PC2<- as.numeric(Indices_PCA_Fixed$CWM_PC2) 
Indices_PCA_SpecificMinusFixedB$CWM_PC2<- as.numeric(Indices_PCA_SpecificMinusFixedB$CWM_PC2) 

Indices_PCA_Fixed$FDis_PC2<- as.numeric(Indices_PCA_Fixed$FDis_PC2) 
Indices_PCA_SpecificMinusFixedB$FDis_PC2<- as.numeric(Indices_PCA_SpecificMinusFixedB$FDis_PC2) 

Indices_PCA_Fixed$Exposure<- as.factor(Indices_PCA_Fixed$Exposure) 
Indices_PCA_SpecificMinusFixedB$Exposure<- as.factor(Indices_PCA_SpecificMinusFixedB$Exposure)

Indices_PCA_AllsitesB$CWM_PC2<- as.numeric(Indices_PCA_AllsitesB$CWM_PC2) 
Indices_PCA_AllsitesB$FDis_PC2<- as.numeric(Indices_PCA_AllsitesB$FDis_PC2) 
Indices_PCA_AllsitesB$Exposure<- as.factor(Indices_PCA_AllsitesB$Exposure) 

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
Indices_PCA_Fixed$FDis_PC2[Indices_PCA_Fixed$FDis_PC2 == 0] <- 0.00001 
Indices_PCA_Fixed$CWM_PC2 <- Indices_PCA_Fixed$CWM_PC2 + 1.0640

Indices_PCA_SpecificMinusFixedB$FDis_PC2[Indices_PCA_SpecificMinusFixedB$FDis_PC2 == 0] <- 0.00001 
Indices_PCA_SpecificMinusFixedB$CWM_PC2 <- Indices_PCA_SpecificMinusFixedB$CWM_PC2 + 0.952

Indices_PCA_AllsitesB$FDis_PC2[Indices_PCA_AllsitesB$FDis_PC2 == 0] <- 0.00001 
Indices_PCA_AllsitesB$CWM_PC2 <- Indices_PCA_AllsitesB$CWM_PC2 + 1.254

#### FIXED ####

CWMPC1_Fixed <- adonis2(Indices_PCA_Fixed$CWM_PC2 ~ Exposure + Site_Exposure, data=Indices_PCA_Fixed, perm=999)
CWMPC1_Fixed # significant difference at exposure and site level

FDisPC1_Fixed <- adonis2(Indices_PCA_Fixed$FDis_PC2 ~ Exposure + Site_Exposure , data=Indices_PCA_Fixed, perm=999)
FDisPC1_Fixed # significant difference at exposure and site level


#### Intra B ####

CWMPC1_IntraB <- adonis2(Indices_PCA_SpecificMinusFixedB$CWM_PC2 ~ Exposure + Site_Exposure, data=Indices_PCA_SpecificMinusFixedB, perm=999)
CWMPC1_IntraB # significant difference at exposure and site level

FDisPC1_IntraB <- adonis2(Indices_PCA_SpecificMinusFixedB$FDis_PC2 ~ Exposure + Site_Exposure , data=Indices_PCA_SpecificMinusFixedB, perm=999)
FDisPC1_IntraB # significant difference at exposure and site level

#### Specific B ####

CWMPC1_SpecificB <- adonis2(Indices_PCA_AllsitesB$CWM_PC2 ~ Exposure + Site_Exposure, data=Indices_PCA_AllsitesB, perm=999)
CWMPC1_SpecificB # significant difference at exposure and site level

FDisPC1_SpecificB <- adonis2(Indices_PCA_AllsitesB$CWM_PC2 ~ Exposure + Site_Exposure , data=Indices_PCA_AllsitesB, perm=999)
FDisPC1_SpecificB # significant difference at exposure and site level
