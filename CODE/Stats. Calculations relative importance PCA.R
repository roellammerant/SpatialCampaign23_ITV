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
Indices_PCA_SpecificMinusFixedA$CWM_PC1<- as.numeric(Indices_PCA_SpecificMinusFixedA$CWM_PC1) 
Indices_PCA_SpecificMinusFixedB$CWM_PC1<- as.numeric(Indices_PCA_SpecificMinusFixedB$CWM_PC1) 

Indices_PCA_Fixed$FDis_PC1<- as.numeric(Indices_PCA_Fixed$FDis_PC1) 
Indices_PCA_SpecificMinusFixedA$FDis_PC1<- as.numeric(Indices_PCA_SpecificMinusFixedA$FDis_PC1) 
Indices_PCA_SpecificMinusFixedB$FDis_PC1<- as.numeric(Indices_PCA_SpecificMinusFixedB$FDis_PC1) 

Indices_PCA_Fixed$Exposure<- as.factor(Indices_PCA_Fixed$Exposure) 
Indices_PCA_SpecificMinusFixedA$Exposure<- as.factor(Indices_PCA_SpecificMinusFixedA$Exposure)
Indices_PCA_SpecificMinusFixedB$Exposure<- as.factor(Indices_PCA_SpecificMinusFixedB$Exposure)

Indices_PCA_Allsites$CWM_PC1<- as.numeric(Indices_PCA_Allsites$CWM_PC1) 
Indices_PCA_Allsites$FDis_PC1<- as.numeric(Indices_PCA_Allsites$FDis_PC1) 
Indices_PCA_Allsites$Exposure<- as.factor(Indices_PCA_Allsites$Exposure) 

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
Indices_PCA_Fixed$CWM_PC1 <- Indices_PCA_Fixed$CWM_PC1 + 1.42
Shallow_PCA_Fixed <- Indices_PCA_Fixed[ which(Indices_PCA_Fixed$Depth=='Shallow'), ]

Indices_PCA_SpecificMinusFixedA$FDis_PC1[Indices_PCA_SpecificMinusFixedA$FDis_PC1 == 0] <- 0.00001 
Indices_PCA_SpecificMinusFixedA$CWM_PC1 <- Indices_PCA_SpecificMinusFixedA$CWM_PC1 + 0.75785
Shallow_PCA_IntraA <- Indices_PCA_SpecificMinusFixedA[ which(Indices_PCA_SpecificMinusFixedA$Depth=='Shallow'), ]

Indices_PCA_SpecificMinusFixedB$FDis_PC1[Indices_PCA_SpecificMinusFixedB$FDis_PC1 == 0] <- 0.00001 
Indices_PCA_SpecificMinusFixedB$CWM_PC1 <- Indices_PCA_SpecificMinusFixedB$CWM_PC1 + 0.985
Shallow_PCA_IntraB <- Indices_PCA_SpecificMinusFixedB[ which(Indices_PCA_SpecificMinusFixedB$Depth=='Shallow'), ]

Indices_PCA_Allsites$FDis_PC1[Indices_PCA_Allsites$FDis_PC1 == 0] <- 0.00001 
Indices_PCA_Allsites$CWM_PC1 <- Indices_PCA_Allsites$CWM_PC1 + 1.425
Shallow_PCA_SpecificA <- Indices_PCA_Allsites[ which(Indices_PCA_Allsites$Depth=='Shallow'), ]

Indices_PCA_AllsitesB$FDis_PC1[Indices_PCA_AllsitesB$FDis_PC1 == 0] <- 0.00001 
Indices_PCA_AllsitesB$CWM_PC1 <- Indices_PCA_AllsitesB$CWM_PC1 + 1.425
Shallow_PCA_SpecificB <- Indices_PCA_AllsitesB[ which(Indices_PCA_AllsitesB$Depth=='Shallow'), ]


#### FIXED ####

CWMPC1_Fixed <- adonis2(Shallow_PCA_Fixed$CWM_PC1 ~ Exposure + Site_Exposure, data=Shallow_PCA_Fixed, perm=999)
CWMPC1_Fixed # significant difference at exposure and site level

FDisPC1_Fixed <- adonis2(Shallow_PCA_Fixed$FDis_PC1 ~ Exposure + Site_Exposure , data=Shallow_PCA_Fixed, perm=999)
FDisPC1_Fixed # significant difference at exposure and site level

#### Intra A ####

CWMPC1_IntraA <- adonis2(Shallow_PCA_IntraA$CWM_PC1 ~ Exposure + Site_Exposure, data=Shallow_PCA_IntraA, perm=999)
CWMPC1_IntraA # significant difference at exposure and site level

FDisPC1_IntraA <- adonis2(Shallow_PCA_IntraA$FDis_PC1 ~ Exposure + Site_Exposure , data=Shallow_PCA_IntraA, perm=999)
FDisPC1_IntraA # significant difference at exposure and site level

#### Intra B ####

CWMPC1_IntraB <- adonis2(Shallow_PCA_IntraB$CWM_PC1 ~ Exposure + Site_Exposure, data=Shallow_PCA_IntraB, perm=999)
CWMPC1_IntraB # significant difference at exposure and site level

FDisPC1_IntraB <- adonis2(Shallow_PCA_IntraB$FDis_PC1 ~ Exposure + Site_Exposure , data=Shallow_PCA_IntraB, perm=999)
FDisPC1_IntraB # significant difference at exposure and site level

#### Specific A ####

CWMPC1_SpecificA <- adonis2(Shallow_PCA_SpecificA$CWM_PC1 ~ Exposure + Site_Exposure, data=Shallow_PCA_SpecificA, perm=999)
CWMPC1_SpecificA # significant difference at exposure and site level

FDisPC1_SpecificA <- adonis2(Shallow_PCA_SpecificA$FDis_PC1 ~ Exposure + Site_Exposure , data=Shallow_PCA_SpecificA, perm=999)
FDisPC1_SpecificA # significant difference at exposure and site level

#### Specific B ####

CWMPC1_SpecificB <- adonis2(Shallow_PCA_SpecificB$CWM_PC1 ~ Exposure + Site_Exposure, data=Shallow_PCA_SpecificB, perm=999)
CWMPC1_SpecificB # significant difference at exposure and site level

FDisPC1_SpecificB <- adonis2(Shallow_PCA_SpecificB$CWM_PC1 ~ Exposure + Site_Exposure , data=Shallow_PCA_SpecificB, perm=999)
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
library(stats)
Indices_PCA_Fixed$CWM_PC2<- as.numeric(Indices_PCA_Fixed$CWM_PC2) 
Indices_PCA_SpecificMinusFixedA$CWM_PC2<- as.numeric(Indices_PCA_SpecificMinusFixedA$CWM_PC2) 
Indices_PCA_SpecificMinusFixedB$CWM_PC2<- as.numeric(Indices_PCA_SpecificMinusFixedB$CWM_PC2) 

Indices_PCA_Fixed$FDis_PC2<- as.numeric(Indices_PCA_Fixed$FDis_PC2) 
Indices_PCA_SpecificMinusFixedA$FDis_PC2<- as.numeric(Indices_PCA_SpecificMinusFixedA$FDis_PC2) 
Indices_PCA_SpecificMinusFixedB$FDis_PC2<- as.numeric(Indices_PCA_SpecificMinusFixedB$FDis_PC2) 

Indices_PCA_Fixed$Exposure<- as.factor(Indices_PCA_Fixed$Exposure) 
Indices_PCA_SpecificMinusFixedA$Exposure<- as.factor(Indices_PCA_SpecificMinusFixedA$Exposure)
Indices_PCA_SpecificMinusFixedB$Exposure<- as.factor(Indices_PCA_SpecificMinusFixedB$Exposure)

Indices_PCA_Allsites$CWM_PC2<- as.numeric(Indices_PCA_Allsites$CWM_PC2) 
Indices_PCA_Allsites$FDis_PC2<- as.numeric(Indices_PCA_Allsites$FDis_PC2) 
Indices_PCA_Allsites$Exposure<- as.factor(Indices_PCA_Allsites$Exposure) 

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
Indices_PCA_Fixed$CWM_PC2 <- Indices_PCA_Fixed$CWM_PC2 + 0.841
Shallow_PCA_Fixed <- Indices_PCA_Fixed[ which(Indices_PCA_Fixed$Depth=='Shallow'), ]

Indices_PCA_SpecificMinusFixedA$FDis_PC2[Indices_PCA_SpecificMinusFixedA$FDis_PC2 == 0] <- 0.00001 
Indices_PCA_SpecificMinusFixedA$CWM_PC2 <- Indices_PCA_SpecificMinusFixedA$CWM_PC2 + 1.35
Shallow_PCA_IntraA <- Indices_PCA_SpecificMinusFixedA[ which(Indices_PCA_SpecificMinusFixedA$Depth=='Shallow'), ]

Indices_PCA_SpecificMinusFixedB$FDis_PC2[Indices_PCA_SpecificMinusFixedB$FDis_PC2 == 0] <- 0.00001 
Indices_PCA_SpecificMinusFixedB$CWM_PC2 <- Indices_PCA_SpecificMinusFixedB$CWM_PC2 + 1.14
Shallow_PCA_IntraB <- Indices_PCA_SpecificMinusFixedB[ which(Indices_PCA_SpecificMinusFixedB$Depth=='Shallow'), ]

Indices_PCA_Allsites$FDis_PC2[Indices_PCA_Allsites$FDis_PC2 == 0] <- 0.00001 
Indices_PCA_Allsites$CWM_PC2 <- Indices_PCA_Allsites$CWM_PC2 + 1.16
Shallow_PCA_SpecificA <- Indices_PCA_Allsites[ which(Indices_PCA_Allsites$Depth=='Shallow'), ]

Indices_PCA_AllsitesB$FDis_PC2[Indices_PCA_AllsitesB$FDis_PC2 == 0] <- 0.00001 
Indices_PCA_AllsitesB$CWM_PC2 <- Indices_PCA_AllsitesB$CWM_PC2 + 1.20
Shallow_PCA_SpecificB <- Indices_PCA_AllsitesB[ which(Indices_PCA_AllsitesB$Depth=='Shallow'), ]


#### FIXED ####

CWMPC1_Fixed <- adonis2(Shallow_PCA_Fixed$CWM_PC2 ~ Exposure + Site_Exposure, data=Shallow_PCA_Fixed, perm=999)
CWMPC1_Fixed # significant difference at exposure and site level

FDisPC1_Fixed <- adonis2(Shallow_PCA_Fixed$FDis_PC2 ~ Exposure + Site_Exposure , data=Shallow_PCA_Fixed, perm=999)
FDisPC1_Fixed # significant difference at exposure and site level

#### Intra A ####

CWMPC1_IntraA <- adonis2(Shallow_PCA_IntraA$CWM_PC2 ~ Exposure + Site_Exposure, data=Shallow_PCA_IntraA, perm=999)
CWMPC1_IntraA # significant difference at exposure and site level

FDisPC1_IntraA <- adonis2(Shallow_PCA_IntraA$FDis_PC2 ~ Exposure + Site_Exposure , data=Shallow_PCA_IntraA, perm=999)
FDisPC1_IntraA # significant difference at exposure and site level

#### Intra B ####

CWMPC1_IntraB <- adonis2(Shallow_PCA_IntraB$CWM_PC2 ~ Exposure + Site_Exposure, data=Shallow_PCA_IntraB, perm=999)
CWMPC1_IntraB # significant difference at exposure and site level

FDisPC1_IntraB <- adonis2(Shallow_PCA_IntraB$FDis_PC2 ~ Exposure + Site_Exposure , data=Shallow_PCA_IntraB, perm=999)
FDisPC1_IntraB # significant difference at exposure and site level

#### Specific A ####

CWMPC1_SpecificA <- adonis2(Shallow_PCA_SpecificA$CWM_PC2 ~ Exposure + Site_Exposure, data=Shallow_PCA_SpecificA, perm=999)
CWMPC1_SpecificA # significant difference at exposure and site level

FDisPC1_SpecificA <- adonis2(Shallow_PCA_SpecificA$FDis_PC2 ~ Exposure + Site_Exposure , data=Shallow_PCA_SpecificA, perm=999)
FDisPC1_SpecificA # significant difference at exposure and site level

#### Specific B ####

CWMPC1_SpecificB <- adonis2(Shallow_PCA_SpecificB$CWM_PC2 ~ Exposure + Site_Exposure, data=Shallow_PCA_SpecificB, perm=999)
CWMPC1_SpecificB # significant difference at exposure and site level

FDisPC1_SpecificB <- adonis2(Shallow_PCA_SpecificB$CWM_PC2 ~ Exposure + Site_Exposure , data=Shallow_PCA_SpecificB, perm=999)
FDisPC1_SpecificB # significant difference at exposure and site level
