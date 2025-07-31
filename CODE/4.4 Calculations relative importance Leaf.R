##%######################################################%##
#                                                          #
####                       SLA                          ####
#                                                          #
##%######################################################%##


##%######################################################%##
#                                                          #
####              adjust data for tests                 ####
#                                                          #
##%######################################################%##
library(stats)
Indices_Leaf_Fixed$CWM_SLA<- as.numeric(Indices_Leaf_Fixed$CWM_SLA) 
Indices_Leaf_SpecificMinusFixedB$CWM_SLA<- as.numeric(Indices_Leaf_SpecificMinusFixedB$CWM_SLA)
Indices_leaf_AllsitesB$CWM_SLA<- as.numeric(Indices_leaf_AllsitesB$CWM_SLA) 

Indices_Leaf_Fixed$Exposure<- as.factor(Indices_Leaf_Fixed$Exposure) 
Indices_Leaf_SpecificMinusFixedB$Exposure<- as.factor(Indices_Leaf_SpecificMinusFixedB$Exposure)
Indices_leaf_AllsitesB$Exposure<- as.factor(Indices_leaf_AllsitesB$Exposure) 

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
Shallow_Leaf_Fixed <- Indices_Leaf_Fixed[ which(Indices_Leaf_Fixed$Depth=='Shallow'), ]

Indices_Leaf_SpecificMinusFixedB$CWM_SLA <- Indices_Leaf_SpecificMinusFixedB$CWM_SLA + 11.6
Shallow_Leaf_IntraB <- Indices_Leaf_SpecificMinusFixedB[ which(Indices_Leaf_SpecificMinusFixedB$Depth=='Shallow'), ]

Shallow_Leaf_SpecificB <- Indices_leaf_AllsitesB[ which(Indices_leaf_AllsitesB$Depth=='Shallow'), ]


#### FIXED ####
CWMSLA_Fixed <- adonis2(Shallow_Leaf_Fixed$CWM_SLA ~ Exposure + Site_Exposure, data=Shallow_Leaf_Fixed, perm=999)
CWMSLA_Fixed # significant difference at exposure and site level

#### Intra B ####
CWMSLA_IntraB <- adonis2(Shallow_Leaf_IntraB$CWM_SLA ~ Exposure + Site_Exposure, data=Shallow_Leaf_IntraB, perm=999)
CWMSLA_IntraB # significant difference at exposure and site level

#### Specific B ####
CWMSLA_SpecificB <- adonis2(Shallow_Leaf_SpecificB$CWM_SLA ~ Exposure + Site_Exposure, data=Shallow_Leaf_SpecificB, perm=999)
CWMSLA_SpecificB # significant difference at exposure and site level