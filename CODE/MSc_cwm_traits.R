##########################################
###########          #####################
########### EXPOSED ######################
###########         ######################
##########################################


TraitData_exp <- Exposed_data[,c(1:5, 19, 31:35)]
TraitData_exp[, c(5:11)] <- sapply(TraitData_exp[, c(5:11)], as.numeric)
names(TraitData_exp)[5:11]<-paste(c("Height", "MaxRootingDepth", "SLA1", "SLA2",
                                "LAP_Ratio1", "LAP_Ratio2", "SpecificRhizomeLength"))
names(TraitData_exp)[2]<-paste(c("Site_number"))

AbundanceData_exp <- Exposed_data[,c(1:4, 26:30)]
AbundanceData_exp[, c(5:9)] <- sapply(AbundanceData_exp[, c(5:9)], as.numeric)
names(AbundanceData_exp)[5:9]<-paste(c("AbovegroundDryMass", "BelowgroundDrymass", "RhizomeDryMass", "RootDryMass", "TotalDryMass"))
names(AbundanceData_exp)[2]<-paste(c("Site_number"))


# Compute and add species richness
library(vegan)
library(data.table)
plot_comm_exp <- as.matrix(as.data.frame.matrix(table(AbundanceData_exp$Site, AbundanceData_exp$Species)))
SpeciesRichness_exp <- as.data.frame (rowSums(plot_comm_exp))
setDT(SpeciesRichness_exp, keep.rownames = "Site")
names(SpeciesRichness_exp)[2]<-paste("SpRich")
TraitData_exp<- merge(SpeciesRichness_exp,TraitData_exp,by="Site")
AbundanceData_exp<- merge(SpeciesRichness_exp,AbundanceData_exp,by="Site")

# Species accumulation curve
sp1 <- specaccum(plot_comm_exp, method = "random", permutations = 10000)
plot(sp1, ci.type="poly", col="red", lwd=2, ci.lty=0, ci.col="lightblue")

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass
Plot_Abun_exp <- with(AbundanceData_exp, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_exp[is.na(Plot_Abun_exp)] = 0

##### Abundance per site
Plot_Abun_exp1_deep <- Plot_Abun_exp[c(1:8),]
Plot_Abun_exp1_shallow <- Plot_Abun_exp[c(27:34),]
Plot_Abun_exp1_deep <- Plot_Abun_exp1_deep[,c(2,4,5)]
Plot_Abun_exp1_shallow <- Plot_Abun_exp1_shallow[,c(1,2,4,5)]

Plot_Abun_exp2_deep <- Plot_Abun_exp[c(9:14),]
Plot_Abun_exp2_shallow <- Plot_Abun_exp[c(35:39),]
Plot_Abun_exp2_deep <- data.matrix(Plot_Abun_exp2_deep[,c(6)])
colnames(Plot_Abun_exp2_deep) <- c("ZOSMAR")
Plot_Abun_exp2_shallow <- Plot_Abun_exp2_shallow[,c(3:6)]

Plot_Abun_exp3_deep <- Plot_Abun_exp[c(15:20),]
Plot_Abun_exp3_shallow <- Plot_Abun_exp[c(40:45),]
Plot_Abun_exp3_deep <- Plot_Abun_exp3_deep[,c(2,4:6)]
Plot_Abun_exp3_shallow <- Plot_Abun_exp3_shallow[,c(2:5)]

Plot_Abun_exp4_deep <- Plot_Abun_exp[c(21:26),]
Plot_Abun_exp4_shallow <- Plot_Abun_exp[c(46:51),]
Plot_Abun_exp4_deep <- Plot_Abun_exp4_deep[,c(1,4,6)]

Plot_Abun_exp5_shallow <- Plot_Abun_exp[c(52:57),]
Plot_Abun_exp5_shallow <- Plot_Abun_exp5_shallow[,c(1,3:5)]


# Species richness vs biomass relationship
TotalBiomass_exp <- as.data.frame (rowSums(Plot_Abun_exp))
setDT(TotalBiomass_exp, keep.rownames = "Site")
names(TotalBiomass_exp)[2]<-paste("Biomass")
TotalBiomass_exp<- merge(SpeciesRichness_exp,TotalBiomass_exp,by="Site")

cor.test(TotalBiomass_exp$SpRich, TotalBiomass_exp$Biomass)
plot(TotalBiomass_exp$SpRich, TotalBiomass_exp$Biomass)




##%##########################################
#########    Trait matrix      ########
##%#########################################

####Height trait 
Heighttrait_exp <- TraitData_exp[,c(1:6)]
Heighttrait_exp <- na.omit(Heighttrait_exp)

#### Species Max values

Sites_exp <-aggregate(x=Heighttrait_exp$Height, by=list(Heighttrait_exp$Site_number,Heighttrait_exp$Depth, Heighttrait_exp$Species), FUN=max)
names(Sites_exp)[1:4]<-paste(c("Site","Depth","Species", "Height"))
Site1_exp_deep <- subset(Sites_exp, Site==1 & Depth=="D")
Site1_exp_shallow <- subset(Sites_exp, Site==1 & Depth=="S")
Site2_exp_deep <- subset(Sites_exp, Site==2 & Depth=="D")
Site2_exp_shallow <- subset(Sites_exp, Site==2 & Depth=="S")
Site3_exp_deep <- subset(Sites_exp, Site==3 & Depth=="D")
Site3_exp_shallow <- subset(Sites_exp, Site==3 & Depth=="S")
Site4_exp_deep <- subset(Sites_exp, Site==4 & Depth=="D")
Site4_exp_shallow <- subset(Sites_exp, Site==4 & Depth=="S")
Site5_exp_shallow <- subset(Sites_exp, Site==5 & Depth=="S")

## Leaves

SLA <- c(TraitData_exp$SLA1, TraitData_exp$SLA2)
Species <- c(TraitData_exp$Species, TraitData_exp$Species)
Site_number <- c(TraitData_exp$Site_number,TraitData_exp$Site_number)
Depth <- c(TraitData_exp$Depth,TraitData_exp$Depth)
Leaftraits_exp <- data.frame(Species,Site_number, Depth,SLA)
Leaftraits_exp <- na.omit(Leaftraits_exp)
Leaftraits_exp <- subset(Leaftraits_exp, SLA <5)

###Max and median

SLA_B_exp <- aggregate(x= Leaftraits_exp$SLA, by = list(Leaftraits_exp$Depth, Leaftraits_exp$Species), FUN = median)
names(SLA_B_exp)[1:3]<-paste(c("Depth","Species", "SLA"))
SLA_B_exp_deep <- subset(SLA_B_exp, Depth=="D")
SLA_B_exp_shallow <- subset(SLA_B_exp, Depth=="S")
SLA_B_exp1_deep <- SLA_B_exp_deep[-c(1,5),]
SLA_B_exp1_shallow <- SLA_B_exp_shallow[-c(3,6),]
SLA_B_exp2_deep <- SLA_B_exp_deep[-c(1:4),]
SLA_B_exp2_shallow <- SLA_B_exp_shallow[-c(1,2),]
SLA_B_exp3_deep <- SLA_B_exp_deep[-c(1),]
SLA_B_exp3_shallow <- SLA_B_exp_shallow[-c(1,6),]
SLA_B_exp4_deep <- SLA_B_exp_deep[-c(2,4),]
SLA_B_exp4_shallow <- SLA_B_exp_shallow
SLA_B_exp5_shallow <- SLA_B_exp_shallow[-c(2,6),]

# Species max and median value trait matrix 

Traits_exp1_deep <- cbind(Site1_exp_deep$Species,Site1_exp_deep$Height,SLA_B_exp1_deep$SLA)
Traits_exp1_deep <- as.data.frame(Traits_exp1_deep)
names(Traits_exp1_deep)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_exp1_deep) <- Traits_exp1_deep$Species
Traits_exp1_deep$Height <- as.numeric(Traits_exp1_deep$Height)
Traits_exp1_deep$SLA <- as.numeric(Traits_exp1_deep$SLA)

Traits_exp1_shallow <- cbind(Site1_exp_shallow$Species,Site1_exp_shallow$Height,SLA_B_exp1_shallow$SLA)
Traits_exp1_shallow <- as.data.frame(Traits_exp1_shallow)
names(Traits_exp1_shallow)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_exp1_shallow) <- Traits_exp1_shallow$Species
Traits_exp1_shallow$Height <- as.numeric(Traits_exp1_shallow$Height)
Traits_exp1_shallow$SLA <- as.numeric(Traits_exp1_shallow$SLA)

Traits_exp2_deep <- cbind(Site2_exp_deep$Species,Site2_exp_deep$Height,SLA_B_exp2_deep$SLA)
Traits_exp2_deep <- as.data.frame(Traits_exp2_deep)
names(Traits_exp2_deep)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_exp2_deep) <- Traits_exp2_deep$Species
Traits_exp2_deep$Height <- as.numeric(Traits_exp2_deep$Height)
Traits_exp2_deep$SLA <- as.numeric(Traits_exp2_deep$SLA)

Traits_exp2_shallow <- cbind(Site2_exp_shallow$Species,Site2_exp_shallow$Height,SLA_B_exp2_shallow$SLA)
Traits_exp2_shallow <- as.data.frame(Traits_exp2_shallow)
names(Traits_exp2_shallow)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_exp2_shallow) <- Traits_exp2_shallow$Species
Traits_exp2_shallow$Height <- as.numeric(Traits_exp2_shallow$Height)
Traits_exp2_shallow$SLA <- as.numeric(Traits_exp2_shallow$SLA)

Traits_exp3_deep <- cbind(Site3_exp_deep$Species,Site3_exp_deep$Height,SLA_B_exp3_deep$SLA)
Traits_exp3_deep <- as.data.frame(Traits_exp3_deep)
names(Traits_exp3_deep)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_exp3_deep) <- Traits_exp3_deep$Species
Traits_exp3_deep$Height <- as.numeric(Traits_exp3_deep$Height)
Traits_exp3_deep$SLA <- as.numeric(Traits_exp3_deep$SLA)

Traits_exp3_shallow <- cbind(Site3_exp_shallow$Species,Site3_exp_shallow$Height,SLA_B_exp3_shallow$SLA)
Traits_exp3_shallow <- as.data.frame(Traits_exp3_shallow)
names(Traits_exp3_shallow)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_exp3_shallow) <- Traits_exp3_shallow$Species
Traits_exp3_shallow$Height <- as.numeric(Traits_exp3_shallow$Height)
Traits_exp3_shallow$SLA <- as.numeric(Traits_exp3_shallow$SLA)

Traits_exp4_deep <- cbind(Site4_exp_deep$Species,Site4_exp_deep$Height,SLA_B_exp4_deep$SLA)
Traits_exp4_deep <- as.data.frame(Traits_exp4_deep)
names(Traits_exp4_deep)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_exp4_deep) <- Traits_exp4_deep$Species
Traits_exp4_deep$Height <- as.numeric(Traits_exp4_deep$Height)
Traits_exp4_deep$SLA <- as.numeric(Traits_exp4_deep$SLA)

Traits_exp4_shallow <- cbind(Site4_exp_shallow$Species,Site4_exp_shallow$Height,SLA_B_exp4_shallow$SLA)
Traits_exp4_shallow <- as.data.frame(Traits_exp4_shallow)
names(Traits_exp4_shallow)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_exp4_shallow) <- Traits_exp4_shallow$Species
Traits_exp4_shallow$Height <- as.numeric(Traits_exp4_shallow$Height)
Traits_exp4_shallow$SLA <- as.numeric(Traits_exp4_shallow$SLA)

Traits_exp5_shallow <- cbind(Site5_exp_shallow$Species,Site5_exp_shallow$Height,SLA_B_exp5_shallow$SLA)
Traits_exp5_shallow <- as.data.frame(Traits_exp5_shallow)
names(Traits_exp5_shallow)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_exp5_shallow) <- Traits_exp5_shallow$Species
Traits_exp5_shallow$Height <- as.numeric(Traits_exp5_shallow$Height)
Traits_exp5_shallow$SLA <- as.numeric(Traits_exp5_shallow$SLA)

########################################################
### Community weighted mean value for height trait######
########################################################


##site 1
cwm_median_exp1_deep <-FD::functcomp(Traits_exp1_deep[,c("Height",
                                                   "SLA")], Plot_Abun_exp1_deep)
colnames(cwm_median_exp1_deep) <- paste0("cwm_", colnames(cwm_median_exp1_deep))

cwm_median_exp1_shallow <-FD::functcomp(Traits_exp1_shallow[,c("Height",
                                                         "SLA")], Plot_Abun_exp1_shallow)
colnames(cwm_median_exp1_shallow) <- paste0("cwm_", colnames(cwm_median_exp1_shallow))

##Site 2

cwm_median_exp2_deep <-FD::functcomp(Traits_exp2_deep[,c("Height",
                                               "SLA")], Plot_Abun_exp2_deep)
colnames(cwm_median_exp2_deep) <- paste0("cwm_", colnames(cwm_median_exp2_deep))

cwm_median_exp2_shallow <-FD::functcomp(Traits_exp2_shallow[,c("Height",
                                                         "SLA")], Plot_Abun_exp2_shallow)
colnames(cwm_median_exp2_shallow) <- paste0("cwm_", colnames(cwm_median_exp2_shallow))

##Site 3

cwm_median_exp3_deep <-FD::functcomp(Traits_exp3_deep[,c("Height",
                                               "SLA")], Plot_Abun_exp3_deep)
colnames(cwm_median_exp3_deep) <- paste0("cwm_", colnames(cwm_median_exp3_deep))

cwm_median_exp3_shallow <-FD::functcomp(Traits_exp3_shallow[,c("Height",
                                                         "SLA")], Plot_Abun_exp3_shallow)
colnames(cwm_median_exp3_shallow) <- paste0("cwm_", colnames(cwm_median_exp3_shallow))

##Site4 

cwm_median_exp4_deep <-FD::functcomp(Traits_exp4_deep[,c("Height",
                                               "SLA")], Plot_Abun_exp4_deep)
colnames(cwm_median_exp4_deep) <- paste0("cwm_", colnames(cwm_median_exp4_deep))

cwm_median_exp4_shallow <-FD::functcomp(Traits_exp4_shallow[,c("Height",
                                                         "SLA")], Plot_Abun_exp4_shallow)
colnames(cwm_median_exp4_shallow) <- paste0("cwm_", colnames(cwm_median_exp4_shallow))

##Site 5

cwm_median_exp5_shallow <-FD::functcomp(Traits_exp5_shallow[,c("Height",
                                                         "SLA")], Plot_Abun_exp5_shallow)
colnames(cwm_median_exp5_shallow) <- paste0("cwm_", colnames(cwm_median_exp5_shallow))

cwm_median_expsites <-rbind(cwm_median_exp1_deep,cwm_median_exp2_deep,cwm_median_exp3_deep,
                            cwm_median_exp4_deep,cwm_median_exp1_shallow,cwm_median_exp2_shallow,cwm_median_exp3_shallow,
                            cwm_median_exp4_shallow, cwm_median_exp5_shallow)
Site_group <- c(rep("exposed",57))
cwm_median_expsites <-cbind(cwm_median_expsites,Site_group)
cwm_median_expsites$Site <- rownames(cwm_median_expsites)
cwm_median_expsites$Depth <- c(rep("Deep",26),rep("Shallow",31))

##########################################
###########          #####################
###########   SEMI   ######################
###########          ######################
##########################################

TraitData_semi <- Semi_data[,c(1:5, 19, 32:36)]
TraitData_semi[, c(5:11)] <- sapply(TraitData_semi[, c(5:11)], as.numeric)
names(TraitData_semi)[5:11]<-paste(c("Height", "MaxRootingDepth", "SLA1", "SLA2",
                                     "LAP_Ratio1", "LAP_Ratio2", "SpecificRhizomeLength"))
names(TraitData_semi)[2]<-paste(c("Site_number"))

AbundanceData_semi <- Semi_data[,c(1:4, 27:31)]
AbundanceData_semi[, c(5:9)] <- sapply(AbundanceData_semi[, c(5:9)], as.numeric)
names(AbundanceData_semi)[5:9]<-paste(c("AbovegroundDryMass", "BelowgroundDrymass", "RhizomeDryMass", "RootDryMass", "TotalDryMass"))
names(AbundanceData_semi)[2]<-paste(c("Site_number"))


# Compute and add species richness
library(vegan)
library(data.table)
plot_comm_semi <- as.matrix(as.data.frame.matrix(table(AbundanceData_semi$Site, AbundanceData_semi$Species)))
SpeciesRichness_semi <- as.data.frame (rowSums(plot_comm_semi))
setDT(SpeciesRichness_semi, keep.rownames = "Site")
names(SpeciesRichness_semi)[2]<-paste("SpRich")
TraitData_semi<- merge(SpeciesRichness_semi,TraitData_semi,by="Site")
AbundanceData_semi<- merge(SpeciesRichness_semi,AbundanceData_semi,by="Site")

# Species accumulation curve
sp1 <- specaccum(plot_comm_semi, method = "random", permutations = 10000)
plot(sp1, ci.type="poly", col="red", lwd=2, ci.lty=0, ci.col="lightblue")

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass, can be whatever
Plot_Abun_semi <- with(AbundanceData_semi, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_semi[is.na(Plot_Abun_semi)] = 0
##### Abundance per site
Plot_Abun_semi1_deep <- Plot_Abun_semi[c(1:6),]
Plot_Abun_semi1_shallow <- Plot_Abun_semi[c(31:36),]
Plot_Abun_semi1_deep <- Plot_Abun_semi1_deep[,c(2,6:8)]
Plot_Abun_semi1_shallow <- Plot_Abun_semi1_shallow[,c(2,3,6:8)]

Plot_Abun_semi2_deep <- Plot_Abun_semi[c(7:12),]
Plot_Abun_semi2_shallow <- Plot_Abun_semi[c(37:42),]
Plot_Abun_semi2_deep <- Plot_Abun_semi2_deep[,c(2,3,7:8)]
Plot_Abun_semi2_shallow <- Plot_Abun_semi2_shallow[,c(3,6:8)]

Plot_Abun_semi3_deep <- Plot_Abun_semi[c(13:18),]
Plot_Abun_semi3_shallow <- Plot_Abun_semi[c(43:48),]
Plot_Abun_semi3_deep <- Plot_Abun_semi3_deep[,c(1:2)]
Plot_Abun_semi3_shallow <- Plot_Abun_semi3_shallow[,c(1:3,7)]

Plot_Abun_semi4_deep <- Plot_Abun_semi[c(19:24),]
Plot_Abun_semi4_shallow <- Plot_Abun_semi[c(49:54),]
Plot_Abun_semi4_deep <- Plot_Abun_semi4_deep[,c(1:2,6)]
Plot_Abun_semi4_shallow <- Plot_Abun_semi4_shallow[,c(1:3,5,7)]

Plot_Abun_semi5_deep <- Plot_Abun_semi[c(25:30),]
Plot_Abun_semi5_shallow <- Plot_Abun_semi[c(55:60),]
Plot_Abun_semi5_deep <- Plot_Abun_semi5_deep[,c(1:3,7:9)]
Plot_Abun_semi5_shallow <- Plot_Abun_semi5_shallow[,c(2:4,7:8)]


# Species richness vs biomass relationship
TotalBiomass_semi <- as.data.frame (rowSums(Plot_Abun_semi))
setDT(TotalBiomass_semi, keep.rownames = "Site")
names(TotalBiomass_semi)[2]<-paste("Biomass")
TotalBiomass_semi<- merge(SpeciesRichness_semi,TotalBiomass_semi,by="Site")

cor.test(TotalBiomass_semi$SpRich, TotalBiomass_semi$Biomass)
plot(TotalBiomass_semi$SpRich, TotalBiomass_semi$Biomass)




##%##########################################
#########    Trait matrix      ########
##%#########################################

####Height trait 
Heighttrait_semi <- TraitData_semi[,c(1:6)]
Heighttrait_semi <- na.omit(Heighttrait_semi)

#### Species Max values

Sites_semi <-aggregate(x=Heighttrait_semi$Height, by=list(Heighttrait_semi$Site_number,Heighttrait_semi$Depth, Heighttrait_semi$Species), FUN=max)
names(Sites_semi)[1:4]<-paste(c("Site","Depth","Species", "Height"))
Site1_semi_deep <- subset(Sites_semi, Site==1 & Depth=="D")
Site1_semi_shallow <- subset(Sites_semi, Site==1 & Depth=="S")
Site2_semi_deep <- subset(Sites_semi, Site==2 & Depth=="D")
Site2_semi_shallow <- subset(Sites_semi, Site==2 & Depth=="S")
Site3_semi_deep <- subset(Sites_semi, Site==3 & Depth=="D")
Site3_semi_shallow <- subset(Sites_semi, Site==3 & Depth=="S")
Site4_semi_deep <- subset(Sites_semi, Site==4 & Depth=="D")
Site4_semi_shallow <- subset(Sites_semi, Site==4 & Depth=="S")
Site5_semi_deep <- subset(Sites_semi, Site==5 & Depth=="D")
Site5_semi_shallow <- subset(Sites_semi, Site==5 & Depth=="S")



## Other traits

SLA <- c(TraitData_semi$SLA1, TraitData_semi$SLA2)
Species <- c(TraitData_semi$Species, TraitData_semi$Species)
Site_number <- c(TraitData_semi$Site_number,TraitData_semi$Site_number)
Depth <- c(TraitData_semi$Depth, TraitData_semi$Depth)
Leaftraits_semi <- data.frame(Species, Site_number,Depth,SLA)
Leaftraits_semi <- na.omit(Leaftraits_semi)
Leaftraits_semi <- subset(Leaftraits_semi, SLA <5)

###Max and median

SLA_B_semi <- aggregate(x= Leaftraits_semi$SLA, by = list(Leaftraits_semi$Species, Leaftraits_semi$Depth), FUN = median)
names(SLA_B_semi)[1:3]<-paste(c("Species","Depth" ,"SLA"))
SLA_B_semi1_deep <- SLA_B_semi[c(2,4:6),]
SLA_B_semi1_shallow <- SLA_B_semi[c(9,10,13:15),]
SLA_B_semi2_deep <- SLA_B_semi[c(2,3,5,6),]
SLA_B_semi2_shallow <- SLA_B_semi[c(10,13:15),]
SLA_B_semi3_deep <- SLA_B_semi[c(1,2),]
SLA_B_semi3_shallow <- SLA_B_semi[c(8:10,14),]
SLA_B_semi4_deep <- SLA_B_semi[c(1,2,4),]
SLA_B_semi4_shallow <- SLA_B_semi[c(8:10,12,14),]
SLA_B_semi5_deep <- SLA_B_semi[c(1:3,5:7),]
SLA_B_semi5_shallow <- SLA_B_semi[c(9:11,14,15),]

# Species max and median value trait matrix 

Traits_semi1_deep <- cbind(Site1_semi_deep$Species,Site1_semi_deep$Height,SLA_B_semi1_deep$SLA)
Traits_semi1_deep <- as.data.frame(Traits_semi1_deep)
names(Traits_semi1_deep)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_semi1_deep) <- Traits_semi1_deep$Species
Traits_semi1_deep$Height <- as.numeric(Traits_semi1_deep$Height)
Traits_semi1_deep$SLA <- as.numeric(Traits_semi1_deep$SLA)

Traits_semi1_shallow <- cbind(Site1_semi_shallow$Species,Site1_semi_shallow$Height,SLA_B_semi1_shallow$SLA)
Traits_semi1_shallow <- as.data.frame(Traits_semi1_shallow)
names(Traits_semi1_shallow)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_semi1_shallow) <- Traits_semi1_shallow$Species
Traits_semi1_shallow$Height <- as.numeric(Traits_semi1_shallow$Height)
Traits_semi1_shallow$SLA <- as.numeric(Traits_semi1_shallow$SLA)

Traits_semi2_deep <- cbind(Site2_semi_deep$Species,Site2_semi_deep$Height,SLA_B_semi2_deep$SLA)
Traits_semi2_deep <- as.data.frame(Traits_semi2_deep)
names(Traits_semi2_deep)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_semi2_deep) <- Traits_semi2_deep$Species
Traits_semi2_deep$Height <- as.numeric(Traits_semi2_deep$Height)
Traits_semi2_deep$SLA <- as.numeric(Traits_semi2_deep$SLA)

Traits_semi2_shallow <- cbind(Site2_semi_shallow$Species,Site2_semi_shallow$Height,SLA_B_semi2_shallow$SLA)
Traits_semi2_shallow <- as.data.frame(Traits_semi2_shallow)
names(Traits_semi2_shallow)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_semi2_shallow) <- Traits_semi2_shallow$Species
Traits_semi2_shallow$Height <- as.numeric(Traits_semi2_shallow$Height)
Traits_semi2_shallow$SLA <- as.numeric(Traits_semi2_shallow$SLA)

Traits_semi3_deep <- cbind(Site3_semi_deep$Species,Site3_semi_deep$Height,SLA_B_semi3_deep$SLA)
Traits_semi3_deep <- as.data.frame(Traits_semi3_deep)
names(Traits_semi3_deep)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_semi3_deep) <- Traits_semi3_deep$Species
Traits_semi3_deep$Height <- as.numeric(Traits_semi3_deep$Height)
Traits_semi3_deep$SLA <- as.numeric(Traits_semi3_deep$SLA)

Traits_semi3_shallow <- cbind(Site3_semi_shallow$Species,Site3_semi_shallow$Height,SLA_B_semi3_shallow$SLA)
Traits_semi3_shallow <- as.data.frame(Traits_semi3_shallow)
names(Traits_semi3_shallow)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_semi3_shallow) <- Traits_semi3_shallow$Species
Traits_semi3_shallow$Height <- as.numeric(Traits_semi3_shallow$Height)
Traits_semi3_shallow$SLA <- as.numeric(Traits_semi3_shallow$SLA)

Traits_semi4_deep <- cbind(Site4_semi_deep$Species,Site4_semi_deep$Height,SLA_B_semi4_deep$SLA)
Traits_semi4_deep <- as.data.frame(Traits_semi4_deep)
names(Traits_semi4_deep)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_semi4_deep) <- Traits_semi4_deep$Species
Traits_semi4_deep$Height <- as.numeric(Traits_semi4_deep$Height)
Traits_semi4_deep$SLA <- as.numeric(Traits_semi4_deep$SLA)

Traits_semi4_shallow <- cbind(Site4_semi_shallow$Species,Site4_semi_shallow$Height,SLA_B_semi4_shallow$SLA)
Traits_semi4_shallow <- as.data.frame(Traits_semi4_shallow)
names(Traits_semi4_shallow)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_semi4_shallow) <- Traits_semi4_shallow$Species
Traits_semi4_shallow$Height <- as.numeric(Traits_semi4_shallow$Height)
Traits_semi4_shallow$SLA <- as.numeric(Traits_semi4_shallow$SLA)

Traits_semi5_deep <- cbind(Site5_semi_deep$Species,Site5_semi_deep$Height,SLA_B_semi5_deep$SLA)
Traits_semi5_deep <- as.data.frame(Traits_semi5_deep)
names(Traits_semi5_deep)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_semi5_deep) <- Traits_semi5_deep$Species
Traits_semi5_deep$Height <- as.numeric(Traits_semi5_deep$Height)
Traits_semi5_deep$SLA <- as.numeric(Traits_semi5_deep$SLA)

Traits_semi5_shallow <- cbind(Site5_semi_shallow$Species,Site5_semi_shallow$Height,SLA_B_semi5_shallow$SLA)
Traits_semi5_shallow <- as.data.frame(Traits_semi5_shallow)
names(Traits_semi5_shallow)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_semi5_shallow) <- Traits_semi5_shallow$Species
Traits_semi5_shallow$Height <- as.numeric(Traits_semi5_shallow$Height)
Traits_semi5_shallow$SLA <- as.numeric(Traits_semi5_shallow$SLA)


########################################################
### Community weighted mean value for height trait######
########################################################



cwm_median_semi1_deep <-FD::functcomp(Traits_semi1_deep[,c("Height",
                                                 "SLA")], Plot_Abun_semi1_deep)
colnames(cwm_median_semi1_deep) <- paste0("cwm_", colnames(cwm_median_semi1_deep))

cwm_median_semi1_shallow <-FD::functcomp(Traits_semi1_shallow[,c("Height",
                                                           "SLA")], Plot_Abun_semi1_shallow)
colnames(cwm_median_semi1_shallow) <- paste0("cwm_", colnames(cwm_median_semi1_shallow))


cwm_median_semi2_deep <-FD::functcomp(Traits_semi2_deep[,c("Height",
                                                           "SLA")], Plot_Abun_semi2_deep)
colnames(cwm_median_semi2_deep) <- paste0("cwm_", colnames(cwm_median_semi2_deep))

cwm_median_semi2_shallow <-FD::functcomp(Traits_semi2_shallow[,c("Height",
                                                                 "SLA")], Plot_Abun_semi2_shallow)
colnames(cwm_median_semi2_shallow) <- paste0("cwm_", colnames(cwm_median_semi2_shallow))


cwm_median_semi3_deep <-FD::functcomp(Traits_semi3_deep[,c("Height",
                                                           "SLA")], Plot_Abun_semi3_deep)
colnames(cwm_median_semi3_deep) <- paste0("cwm_", colnames(cwm_median_semi3_deep))

cwm_median_semi3_shallow <-FD::functcomp(Traits_semi3_shallow[,c("Height",
                                                                 "SLA")], Plot_Abun_semi3_shallow)
colnames(cwm_median_semi3_shallow) <- paste0("cwm_", colnames(cwm_median_semi3_shallow))


cwm_median_semi4_deep <-FD::functcomp(Traits_semi4_deep[,c("Height",
                                                           "SLA")], Plot_Abun_semi4_deep)
colnames(cwm_median_semi4_deep) <- paste0("cwm_", colnames(cwm_median_semi4_deep))

cwm_median_semi4_shallow <-FD::functcomp(Traits_semi4_shallow[,c("Height",
                                                                 "SLA")], Plot_Abun_semi4_shallow)
colnames(cwm_median_semi4_shallow) <- paste0("cwm_", colnames(cwm_median_semi4_shallow))


cwm_median_semi5_deep <-FD::functcomp(Traits_semi5_deep[,c("Height",
                                                           "SLA")], Plot_Abun_semi5_deep)
colnames(cwm_median_semi5_deep) <- paste0("cwm_", colnames(cwm_median_semi5_deep))

cwm_median_semi5_shallow <-FD::functcomp(Traits_semi5_shallow[,c("Height",
                                                                 "SLA")], Plot_Abun_semi5_shallow)
colnames(cwm_median_semi5_shallow) <- paste0("cwm_", colnames(cwm_median_semi5_shallow))


cwm_median_semisites <-rbind(cwm_median_semi1_deep,cwm_median_semi2_deep,cwm_median_semi3_deep,
                             cwm_median_semi4_deep, cwm_median_semi5_deep,
                             cwm_median_semi1_shallow,cwm_median_semi2_shallow,cwm_median_semi3_shallow,
                             cwm_median_semi4_shallow,cwm_median_semi5_shallow)
Site_group <- c(rep("semi",60))
cwm_median_semisites <-cbind(cwm_median_semisites,Site_group)
cwm_median_semisites$Site <- rownames(cwm_median_semisites)
cwm_median_semisites$Depth <- c(rep("Deep",30),rep("Shallow",30))

##########################################
###########           ####################
########### SHELTERED ####################
###########           ####################
##########################################

TraitData_shel <- Shel_data[,c(1:5, 19, 36:40)]
TraitData_shel[, c(5:11)] <- sapply(TraitData_shel[, c(5:11)], as.numeric)
names(TraitData_shel)[5:11]<-paste(c("Height", "MaxRootingDepth", "SLA1", "SLA2",
                                     "LAP_Ratio1", "LAP_Ratio2", "SpecificRhizomeLength"))
names(TraitData_shel)[2]<-paste(c("Site_number"))

AbundanceData_shel <- Shel_data[,c(1:4, 31:35)]
AbundanceData_shel[, c(5:9)] <- sapply(AbundanceData_shel[, c(5:9)], as.numeric)
names(AbundanceData_shel)[5:9]<-paste(c("AbovegroundDryMass", "BelowgroundDrymass", "RhizomeDryMass", "RootDryMass", "TotalDryMass"))
names(AbundanceData_shel)[2]<-paste(c("Site_number"))


# Compute and add species richness
library(vegan)
library(data.table)
plot_comm_shel <- as.matrix(as.data.frame.matrix(table(AbundanceData_shel$Site, AbundanceData_shel$Species)))
SpeciesRichness_shel <- as.data.frame (rowSums(plot_comm_shel))
setDT(SpeciesRichness_shel, keep.rownames = "Site")
names(SpeciesRichness_shel)[2]<-paste("SpRich")
TraitData_shel<- merge(SpeciesRichness_shel,TraitData_shel,by="Site")
AbundanceData_shel<- merge(SpeciesRichness_shel,AbundanceData_shel,by="Site")

# Species accumulation curve
sp1 <- specaccum(plot_comm_shel, method = "random", permutations = 10000)
plot(sp1, ci.type="poly", col="red", lwd=2, ci.lty=0, ci.col="lightblue")

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass, can be whatever
Plot_Abun_shel <- with(AbundanceData_shel, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_shel[is.na(Plot_Abun_shel)] = 0
##### Abundance per site
Plot_Abun_shel1 <- Plot_Abun_shel[c(1:6),]
Plot_Abun_shel1 <- Plot_Abun_shel1[,c(2:4,6,8,9)]
Plot_Abun_shel2 <- Plot_Abun_shel[c(7:12),]
Plot_Abun_shel2 <- Plot_Abun_shel2[,c(1,2,4,5,7:9)]
Plot_Abun_shel3 <- Plot_Abun_shel[c(13:18),]
Plot_Abun_shel3 <- Plot_Abun_shel3[,c(2,5,7)]
Plot_Abun_shel4 <- Plot_Abun_shel[c(19:24),]
Plot_Abun_shel4 <- Plot_Abun_shel4[,c(2,7,9)]
Plot_Abun_shel5 <- Plot_Abun_shel[c(25:30),]
Plot_Abun_shel5 <- Plot_Abun_shel5[,c(1,2,5,9)]


# Species richness vs biomass relationship
TotalBiomass_shel <- as.data.frame (rowSums(Plot_Abun_shel))
setDT(TotalBiomass_shel, keep.rownames = "Site")
names(TotalBiomass_shel)[2]<-paste("Biomass")
TotalBiomass_shel<- merge(SpeciesRichness_shel,TotalBiomass_shel,by="Site")

cor.test(TotalBiomass_shel$SpRich, TotalBiomass_shel$Biomass)
plot(TotalBiomass_shel$SpRich, TotalBiomass_shel$Biomass)




##%##########################################
#########    Trait matrix      ########
##%#########################################

####Height trait 
Heighttrait_shel <- TraitData_shel[,c(1:6)]
Heighttrait_shel <- na.omit(Heighttrait_shel)

#### Species Max values
Sites_shel <-aggregate(x=Heighttrait_shel$Height, by=list(Heighttrait_shel$Site_number,Heighttrait_shel$Depth, Heighttrait_shel$Species), FUN=max)
names(Sites_shel)[1:4]<-paste(c("Site","Depth","Species", "Height"))
Site1_shel <- Sites_shel[c(3,8,9,14,18,20),]
Site2_shel <- Sites_shel[c(1,4,10,11,15,19,21),]
Site3_shel <- Sites_shel[c(5,12,16),]
Site4_shel <- Sites_shel[c(6,17,22),]
Site5_shel <- Sites_shel[c(2,7,13,23),]

## Other traits

SLA <- c(TraitData_shel$SLA1, TraitData_shel$SLA2)
Species <- c(TraitData_shel$Species, TraitData_shel$Species)
Site_number <- c(TraitData_shel$Site_number,TraitData_shel$Site_number)
Depth <- c(TraitData_shel$Depth, TraitData_shel$Depth)
Leaftraits_shel <- data.frame(Species, Site_number,Depth, SLA)
Leaftraits_shel <- na.omit(Leaftraits_shel)
Leaftraits_shel <- subset(Leaftraits_shel, SLA <5)

###Max and median

SLA_B_shel <- aggregate(x= Leaftraits_shel$SLA, by = list(Leaftraits_shel$Species), FUN = median)
names(SLA_B_shel)[1:2]<-paste(c("Species", "SLA"))
SLA_B_shel1 <- SLA_B_shel[c(2:4,6,8,9),]
SLA_B_shel2 <- SLA_B_shel[c(1,2,3,5,7:9),]
SLA_B_shel3 <- SLA_B_shel[c(2,5,7),]
SLA_B_shel4 <- SLA_B_shel[c(2,7,9),]
SLA_B_shel5 <- SLA_B_shel[c(1,2,5,9),]

# Species max and median value trait matrix 

Traits_shel1 <- cbind(Site1_shel$Species,Site1_shel$Height,SLA_B_shel1$SLA)
Traits_shel1 <- as.data.frame(Traits_shel1)
names(Traits_shel1)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_shel1) <- Traits_shel1$Species
Traits_shel1$Height <- as.numeric(Traits_shel1$Height)
Traits_shel1$SLA <- as.numeric(Traits_shel1$SLA)

Traits_shel2 <- cbind(Site2_shel$Species,Site2_shel$Height,SLA_B_shel2$SLA)
Traits_shel2 <- as.data.frame(Traits_shel2)
names(Traits_shel2)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_shel2) <- Traits_shel2$Species
Traits_shel2$Height <- as.numeric(Traits_shel2$Height)
Traits_shel2$SLA <- as.numeric(Traits_shel2$SLA)

Traits_shel3 <- cbind(Site3_shel$Species,Site3_shel$Height,SLA_B_shel3$SLA)
Traits_shel3 <- as.data.frame(Traits_shel3)
names(Traits_shel3)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_shel3) <- Traits_shel3$Species
Traits_shel3$Height <- as.numeric(Traits_shel3$Height)
Traits_shel3$SLA <- as.numeric(Traits_shel3$SLA)

Traits_shel4 <- cbind(Site4_shel$Species,Site4_shel$Height,SLA_B_shel4$SLA)
Traits_shel4 <- as.data.frame(Traits_shel4)
names(Traits_shel4)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_shel4) <- Traits_shel4$Species
Traits_shel4$Height <- as.numeric(Traits_shel4$Height)
Traits_shel4$SLA <- as.numeric(Traits_shel4$SLA)

Traits_shel5 <- cbind(Site5_shel$Species,Site5_shel$Height,SLA_B_shel5$SLA)
Traits_shel5 <- as.data.frame(Traits_shel5)
names(Traits_shel5)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_shel5) <- Traits_shel5$Species
Traits_shel5$Height <- as.numeric(Traits_shel5$Height)
Traits_shel5$SLA <- as.numeric(Traits_shel5$SLA)

########################################################
### Community weighted mean value for height trait######
########################################################


cwm_median_shel1_shallow <-FD::functcomp(Traits_shel1[,c("Height",
                                                 "SLA")], Plot_Abun_shel1)
colnames(cwm_median_shel1_shallow) <- paste0("cwm_", colnames(cwm_median_shel1_shallow))

cwm_median_shel2_shallow <-FD::functcomp(Traits_shel2[,c("Height",
                                                 "SLA")], Plot_Abun_shel2)
colnames(cwm_median_shel2_shallow) <- paste0("cwm_", colnames(cwm_median_shel2_shallow))

cwm_median_shel3_shallow <-FD::functcomp(Traits_shel3[,c("Height",
                                                 "SLA")], Plot_Abun_shel3)
colnames(cwm_median_shel3_shallow) <- paste0("cwm_", colnames(cwm_median_shel3_shallow))

cwm_median_shel4_shallow <-FD::functcomp(Traits_shel4[,c("Height",
                                                 "SLA")], Plot_Abun_shel4)
colnames(cwm_median_shel4_shallow) <- paste0("cwm_", colnames(cwm_median_shel4_shallow))

cwm_median_shel5_shallow <-FD::functcomp(Traits_shel5[,c("Height",
                                                 "SLA")], Plot_Abun_shel5)
colnames(cwm_median_shel5_shallow) <- paste0("cwm_", colnames(cwm_median_shel5_shallow))

cwm_median_shelsites <-rbind(cwm_median_shel1_shallow,cwm_median_shel2_shallow,cwm_median_shel3_shallow,
                             cwm_median_shel4_shallow, cwm_median_shel5_shallow)
Site_group <- c(rep("sheltered",30))
cwm_median_shelsites <-cbind(cwm_median_shelsites,Site_group)
cwm_median_shelsites$Site <- rownames(cwm_median_shelsites)
cwm_median_shelsites$Depth <- c(rep("Shallow",30))

##########################################
###########          #####################
###########   POJO   ######################
###########          ######################
##########################################

TraitData_pojo <- Pojo_data[,c(1:5, 19, 34:38)]
TraitData_pojo[, c(5:11)] <- sapply(TraitData_pojo[, c(5:11)], as.numeric)
names(TraitData_pojo)[5:11]<-paste(c("Height", "MaxRootingDepth", "SLA1", "SLA2",
                                     "LAP_Ratio1", "LAP_Ratio2", "SpecificRhizomeLength"))
names(TraitData_pojo)[2]<-paste(c("Site_number"))

AbundanceData_pojo <- Pojo_data[,c(1:4, 29:33)]
AbundanceData_pojo[, c(5:9)] <- sapply(AbundanceData_pojo[, c(5:9)], as.numeric)
names(AbundanceData_pojo)[5:9]<-paste(c("AbovegroundDryMass", "BelowgroundDrymass", "RhizomeDryMass", "RootDryMass", "TotalDryMass"))
names(AbundanceData_pojo)[2]<-paste(c("Site_number"))


# Compute and add species richness
library(vegan)
library(data.table)
plot_comm_pojo <- as.matrix(as.data.frame.matrix(table(AbundanceData_pojo$Site, AbundanceData_pojo$Species)))
SpeciesRichness_pojo <- as.data.frame (rowSums(plot_comm_pojo))
setDT(SpeciesRichness_pojo, keep.rownames = "Site")
names(SpeciesRichness_pojo)[2]<-paste("SpRich")
TraitData_pojo<- merge(SpeciesRichness_pojo,TraitData_pojo,by="Site")
AbundanceData_pojo<- merge(SpeciesRichness_pojo,AbundanceData_pojo,by="Site")

# Species accumulation curve
sp1 <- specaccum(plot_comm_pojo, method = "random", permutations = 10000)
plot(sp1, ci.type="poly", col="red", lwd=2, ci.lty=0, ci.col="lightblue")

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass, can be whatever
Plot_Abun_pojo <- with(AbundanceData_pojo, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_pojo[is.na(Plot_Abun_pojo)] = 0
Plot_Abun_pojo1 <- Plot_Abun_pojo[c(13:18),]
Plot_Abun_pojo1 <- Plot_Abun_pojo1[,c(2:4,7:12)]

Plot_Abun_pojo2_deep <- Plot_Abun_pojo[c(1:6),]
Plot_Abun_pojo2_shallow <- Plot_Abun_pojo[c(19:24),]
Plot_Abun_pojo2_deep <- Plot_Abun_pojo2_deep[,c(2,9)]
Plot_Abun_pojo2_shallow <- Plot_Abun_pojo2_shallow[,c(2:4,7,11)]

Plot_Abun_pojo3_deep <- Plot_Abun_pojo[c(7:12),]
Plot_Abun_pojo3_shallow <- Plot_Abun_pojo[c(25:30),]
Plot_Abun_pojo3_deep <- Plot_Abun_pojo3_deep[,c(1,2,4,7,9,11)]
Plot_Abun_pojo3_shallow <- Plot_Abun_pojo3_shallow[,c(2:5,11)]

Plot_Abun_pojo4 <- Plot_Abun_pojo[c(31:36),]
Plot_Abun_pojo4 <- Plot_Abun_pojo4[,c(2:5,7,11)]

Plot_Abun_pojo5 <- Plot_Abun_pojo[c(37:42),]
Plot_Abun_pojo5 <- Plot_Abun_pojo5[,c(2,3,6,11)]


# Species richness vs biomass relationship
TotalBiomass_pojo <- as.data.frame (rowSums(Plot_Abun_pojo))
setDT(TotalBiomass_pojo, keep.rownames = "Site")
names(TotalBiomass_pojo)[2]<-paste("Biomass")
TotalBiomass_pojo<- merge(SpeciesRichness_pojo,TotalBiomass_pojo,by="Site")

cor.test(TotalBiomass_pojo$SpRich, TotalBiomass_pojo$Biomass)
plot(TotalBiomass_pojo$SpRich, TotalBiomass_pojo$Biomass)




##%##########################################
#########    Trait matrix      ########
##%#########################################

####Height trait 
Heighttrait_pojo <- TraitData_pojo[,c(1:6)]
Heighttrait_pojo <- na.omit(Heighttrait_pojo)

#### Species Max values
Sites_pojo <-aggregate(x=Heighttrait_pojo$Height, by=list(Heighttrait_pojo$Site_number,Heighttrait_pojo$Depth, Heighttrait_pojo$Species), FUN=max)
names(Sites_pojo)[1:4]<-paste(c("Site","Depth","Species", "Height"))
Site1_pojo <- subset(Sites_pojo, Site==1)
Site2_pojo_deep <- subset(Sites_pojo, Site==2 & Depth=="D")
Site2_pojo_shallow <- subset(Sites_pojo, Site==2 & Depth=="S")
Site3_pojo_deep <- subset(Sites_pojo, Site==3 & Depth=="D")
Site3_pojo_shallow <- subset(Sites_pojo, Site==3 & Depth=="S")
Site4_pojo <- subset(Sites_pojo, Site==4)
Site5_pojo <- subset(Sites_pojo, Site==5)

Species <- split(Heighttrait_pojo,Heighttrait_pojo$Species)
NAJMAR <- Species[["NAJMAR"]]
NAJMAR2 <- subset(NAJMAR, Height >25)
NAJMAR_mean <- aggregate(x= NAJMAR2$Height, by = list(NAJMAR2$Species), FUN = mean)
names(NAJMAR_mean)[1:2]<-paste(c("Species","Height"))

Site1_pojo <- Site1_pojo[-c(4),]
NAJMAR_mean1 <- NAJMAR_mean
NAJMAR_mean1$Site <- c(1)
NAJMAR_mean1$Depth <- c("S")
Site1_pojo <- rbind(Site1_pojo,NAJMAR_mean1)
Site1_pojo <- Site1_pojo[order(Site1_pojo$Species),]

Site2_pojo_shallow <- Site2_pojo_shallow[-c(4),]
NAJMAR_mean2 <- NAJMAR_mean
NAJMAR_mean2$Site <- c(2)
NAJMAR_mean2$Depth <- c("S")
Site2_pojo_shallow <- rbind(Site2_pojo_shallow,NAJMAR_mean2)
Site2_pojo_shallow <- Site2_pojo_shallow[order(Site2_pojo_shallow$Species),]
 
Site3_pojo_deep <- Site3_pojo_deep[-c(4),]
NAJMAR_mean3 <- NAJMAR_mean
NAJMAR_mean3$Site <- c(3)
NAJMAR_mean3$Depth <- c("D")
Site3_pojo_deep <- rbind(Site3_pojo_deep,NAJMAR_mean3)
Site3_pojo_deep <- Site3_pojo_deep[order(Site3_pojo_deep$Species),]

Site4_pojo <- Site4_pojo[-c(5),]
NAJMAR_mean4 <- NAJMAR_mean
NAJMAR_mean4$Site <- c(4)
NAJMAR_mean4$Depth <- c("S")
Site4_pojo <- rbind(Site4_pojo,NAJMAR_mean4)
Site4_pojo <- Site4_pojo[order(Site4_pojo$Species),]



## Other traits

SLA <- c(TraitData_pojo$SLA1, TraitData_pojo$SLA2)
Species <- c(TraitData_pojo$Species, TraitData_pojo$Species)
Site_number <- c(TraitData_pojo$Site_number,TraitData_pojo$Site_number)
Depth <- c(TraitData_pojo$Depth, TraitData_pojo$Depth)
Leaftraits_pojo <- data.frame(Species, Site_number,Depth,SLA)
Leaftraits_pojo <- na.omit(Leaftraits_pojo)
Leaftraits_pojo <- subset(Leaftraits_pojo, SLA <5)

###Max and median

SLA_B_pojo <- aggregate(x= Leaftraits_pojo$SLA, by = list(Leaftraits_pojo$Species,Leaftraits_pojo$Depth), FUN = median)
names(SLA_B_pojo)[1:3]<-paste(c("Species","Depth", "SLA"))
SLA_B_pojo1 <- SLA_B_pojo[c(7:9,12:17),]
SLA_B_pojo2_deep <- SLA_B_pojo[c(2,5),]
SLA_B_pojo2_shallow <- SLA_B_pojo[c(7:9,12,16),]
SLA_B_pojo3_deep <- SLA_B_pojo[c(1:6),]
SLA_B_pojo3_shallow <- SLA_B_pojo[c(7:10,16),]
SLA_B_pojo4 <- SLA_B_pojo[c(7:10,12,16),]
SLA_B_pojo5 <- SLA_B_pojo[c(7:8,11,16),]

# Species max and median value trait matrix 

Traits_pojo1 <- cbind(Site1_pojo$Species,Site1_pojo$Height,SLA_B_pojo1$SLA)
Traits_pojo1 <- as.data.frame(Traits_pojo1)
names(Traits_pojo1)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_pojo1) <- Traits_pojo1$Species
Traits_pojo1$Height <- as.numeric(Traits_pojo1$Height)
Traits_pojo1$SLA <- as.numeric(Traits_pojo1$SLA)

Traits_pojo2_deep <- cbind(Site2_pojo_deep$Species,Site2_pojo_deep$Height,SLA_B_pojo2_deep$SLA)
Traits_pojo2_deep <- as.data.frame(Traits_pojo2_deep)
names(Traits_pojo2_deep)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_pojo2_deep) <- Traits_pojo2_deep$Species
Traits_pojo2_deep$Height <- as.numeric(Traits_pojo2_deep$Height)
Traits_pojo2_deep$SLA <- as.numeric(Traits_pojo2_deep$SLA)

Traits_pojo2_shallow <- cbind(Site2_pojo_shallow$Species,Site2_pojo_shallow$Height,SLA_B_pojo2_shallow$SLA)
Traits_pojo2_shallow <- as.data.frame(Traits_pojo2_shallow)
names(Traits_pojo2_shallow)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_pojo2_shallow) <- Traits_pojo2_shallow$Species
Traits_pojo2_shallow$Height <- as.numeric(Traits_pojo2_shallow$Height)
Traits_pojo2_shallow$SLA <- as.numeric(Traits_pojo2_shallow$SLA)

Traits_pojo3_deep <- cbind(Site3_pojo_deep$Species,Site3_pojo_deep$Height,SLA_B_pojo3_deep$SLA)
Traits_pojo3_deep <- as.data.frame(Traits_pojo3_deep)
names(Traits_pojo3_deep)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_pojo3_deep) <- Traits_pojo3_deep$Species
Traits_pojo3_deep$Height <- as.numeric(Traits_pojo3_deep$Height)
Traits_pojo3_deep$SLA <- as.numeric(Traits_pojo3_deep$SLA)

Traits_pojo3_shallow <- cbind(Site3_pojo_shallow$Species,Site3_pojo_shallow$Height,SLA_B_pojo3_shallow$SLA)
Traits_pojo3_shallow <- as.data.frame(Traits_pojo3_shallow)
names(Traits_pojo3_shallow)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_pojo3_shallow) <- Traits_pojo3_shallow$Species
Traits_pojo3_shallow$Height <- as.numeric(Traits_pojo3_shallow$Height)
Traits_pojo3_shallow$SLA <- as.numeric(Traits_pojo3_shallow$SLA)

Traits_pojo4 <- cbind(Site4_pojo$Species,Site4_pojo$Height,SLA_B_pojo4$SLA)
Traits_pojo4 <- as.data.frame(Traits_pojo4)
names(Traits_pojo4)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_pojo4) <- Traits_pojo4$Species
Traits_pojo4$Height <- as.numeric(Traits_pojo4$Height)
Traits_pojo4$SLA <- as.numeric(Traits_pojo4$SLA)

Traits_pojo5 <- cbind(Site5_pojo$Species,Site5_pojo$Height,SLA_B_pojo5$SLA)
Traits_pojo5 <- as.data.frame(Traits_pojo5)
names(Traits_pojo5)[1:3]<-paste(c("Species", "Height", "SLA"))
rownames(Traits_pojo5) <- Traits_pojo5$Species
Traits_pojo5$Height <- as.numeric(Traits_pojo5$Height)
Traits_pojo5$SLA <- as.numeric(Traits_pojo5$SLA)

########################################################
### Community weighted mean value for height trait######
########################################################


cwm_median_pojo1 <-FD::functcomp(Traits_pojo1[,c("Height",
                                                 "SLA")], Plot_Abun_pojo1)
colnames(cwm_median_pojo1) <- paste0("cwm_", colnames(cwm_median_pojo1))


cwm_median_pojo2_deep <-FD::functcomp(Traits_pojo2_deep[,c("Height",
                                                 "SLA")], Plot_Abun_pojo2_deep)
colnames(cwm_median_pojo2_deep) <- paste0("cwm_", colnames(cwm_median_pojo2_deep))

cwm_median_pojo2_shallow <-FD::functcomp(Traits_pojo2_shallow[,c("Height",
                                                                 "SLA")], Plot_Abun_pojo2_shallow)
colnames(cwm_median_pojo2_shallow) <- paste0("cwm_", colnames(cwm_median_pojo2_shallow))


cwm_median_pojo3_deep <-FD::functcomp(Traits_pojo3_deep[,c("Height",
                                                           "SLA")], Plot_Abun_pojo3_deep)
colnames(cwm_median_pojo3_deep) <- paste0("cwm_", colnames(cwm_median_pojo3_deep))

cwm_median_pojo3_shallow <-FD::functcomp(Traits_pojo3_shallow[,c("Height",
                                                                 "SLA")], Plot_Abun_pojo3_shallow)
colnames(cwm_median_pojo3_shallow) <- paste0("cwm_", colnames(cwm_median_pojo3_shallow))


cwm_median_pojo4 <-FD::functcomp(Traits_pojo4[,c("Height",
                                                 "SLA")], Plot_Abun_pojo4)
colnames(cwm_median_pojo4) <- paste0("cwm_", colnames(cwm_median_pojo4))

cwm_median_pojo5 <-FD::functcomp(Traits_pojo5[,c("Height",
                                                 "SLA")], Plot_Abun_pojo5)
colnames(cwm_median_pojo5) <- paste0("cwm_", colnames(cwm_median_pojo5))

cwm_median_pojosites <-rbind(cwm_median_pojo2_deep,cwm_median_pojo3_deep,cwm_median_pojo1,cwm_median_pojo2_shallow,cwm_median_pojo3_shallow,
                             cwm_median_pojo4, cwm_median_pojo5)
Site_group <- c(rep("Pojo",42))
cwm_median_pojosites <-cbind(cwm_median_pojosites,Site_group)
cwm_median_pojosites$Site <- rownames(cwm_median_pojosites)
cwm_median_pojosites <- na.omit(cwm_median_pojosites)
cwm_median_pojosites$Depth <- c(rep("Deep",8),rep("Shallow",30))

#########################################################
######## CWM all groups#################################
#######################################################

cwm_allsitedepthspefic <- rbind(cwm_median_expsites, cwm_median_semisites, cwm_median_shelsites, cwm_median_pojosites)
cwm_alldeep <- subset(cwm_allsitedepthspefic,Depth=="Deep")
cwm_allshallow <- subset(cwm_allsitedepthspefic,Depth=="Shallow")


#######################################################
############## Boxplots ################################
######################################################

##Both depths together
cwm_allsitedepthspefic$Site_group <- factor(cwm_allsitedepthspefic$Site_group, 
                                           levels=c("exposed", "semi","sheltered", "Pojo" ))
boxplot(cwm_Height ~ Site_group, data = cwm_allsitedepthspefic, main ="Cwm of max height across the exposure gradient", 
        col = c("green","magenta","cyan","blue"), ylab="cwm of max height (cm)", xlab ="Exposure")

boxplot(cwm_SLA ~ Site_group, data = cwm_allsitedepthspefic, main ="Cwm of Specific Leaf Area across the exposure gradient", 
        col = c("green","magenta","cyan","blue"),ylab="cwm of SLA (cm2/mg)", xlab ="Exposure")

####depths separately

cwm_alldeep$Site_group <- factor(cwm_alldeep$Site_group, 
                                            levels=c("exposed", "semi", "Pojo" ))
boxplot(cwm_Height ~ Site_group, data = cwm_alldeep, main ="Cwm of max height at deep sites", 
        col = c("green","magenta","cyan","blue"), ylab="cwm of max height (cm)", xlab ="Exposure")

boxplot(cwm_SLA ~ Site_group, data = cwm_alldeep, main ="Cwm of Specific Leaf Area at deep sites", 
        col = c("green","magenta","cyan","blue"),ylab="cwm of SLA (cm2/mg)", xlab ="Exposure")


cwm_allshallow$Site_group <- factor(cwm_allshallow$Site_group, 
                                 levels=c("exposed", "semi","sheltered", "Pojo" ))
boxplot(cwm_Height ~ Site_group, data = cwm_allshallow, main ="Cwm of max height at shallow sites", 
        col = c("green","magenta","cyan","blue"), ylab="cwm of max height (cm)", xlab ="Exposure")

boxplot(cwm_SLA ~ Site_group, data = cwm_allshallow, main ="Cwm of Specific Leaf Area at shallow sites", 
        col = c("green","magenta","cyan","blue"),ylab="cwm of SLA (cm2/mg)", xlab ="Exposure")


####Colors by depth max height (all data points)

cwm_allsitedepthspefic$Site_group <- factor(cwm_allsitedepthspefic$Site_group, 
                                            levels=c("exposed", "semi","sheltered", "Pojo" ))
cwm_allsitedepthspefic <- cwm_allsitedepthspefic[order(cwm_allsitedepthspefic$Site_group), ]
cwm_allsitedepthspefic$Depth <- factor(cwm_allsitedepthspefic$Depth, levels = c("Deep", "Shallow"))
cwm_allsitedepthspefic$Site_Depth <- factor(
  paste(cwm_allsitedepthspefic$Site_group, cwm_allsitedepthspefic$Depth, sep = "_"),
  levels = c(
    "exposed_Deep", "exposed_Shallow",
    "semi_Deep", "semi_Shallow", "sheltered_Shallow",
    "Pojo_Deep", "Pojo_Shallow"
  )
)
# Boxplot colors and legend colors
boxplot_colors <- c("lightblue","bisque","lightblue","bisque","lightblue","bisque","lightblue","bisque")
scatterplot_colors <- c("lightblue","bisque","lightblue","bisque","bisque","lightblue","bisque")
legend_colors <- c("lightblue","bisque")

# Create the initial boxplot without stripchart
boxplot(cwm_Height ~ Site_group * Depth, data = cwm_allsitedepthspefic,
        main = "Cwm of maximum height at deep and shallow sites", boxwex = 0.5, col = boxplot_colors,
        xlab = NA, ylab = "cwm of max height (cm)", lex.order = TRUE, xaxt = "n", cex.lab = 1.1, cex.main =1.5)

# Add the stripchart to the existing plot
stripchart(cwm_Height ~ Site_Depth,
           data = cwm_allsitedepthspefic,
           method = "jitter",
           at = c(1.4, 2.4, 3.4, 4.4, 6.4, 7.4, 8.4),
           pch = 17,
           col = scatterplot_colors,
           vertical = TRUE,
           add = TRUE)

# Labels for the x-axis
label = c("Exposed", "Semi-sheltered", "Sheltered", "Pojo Bay")

# Add custom x-axis
axis(1, at = seq(1.5, 7.5, 2), labels = label, tick = FALSE, cex.axis = 1)

# Add legend
legend("topleft", legend = c("Deep","Shallow"), fill = legend_colors)



####Colors by depth SLA


# Create the initial boxplot without stripchart
boxplot(cwm_SLA ~ Site_group * Depth, data = cwm_allsitedepthspefic,
        main = "Cwm of Specific Leaf Area at deep and shallow sites", boxwex = 0.5, col = boxplot_colors,
        xlab = NA, ylab = "cwm of SLA (cm2/mg)", lex.order = TRUE, xaxt = "n", cex.lab = 1.1, cex.main =1.5)

# Add the stripchart to the existing plot
stripchart(cwm_SLA ~ Site_Depth,
           data = cwm_allsitedepthspefic,
           method = "jitter",
           at = c(1.4, 2.4, 3.4, 4.4, 6.4, 7.4, 8.4),
           pch = 17,
           col = scatterplot_colors,
           vertical = TRUE,
           add = TRUE)

# Labels for the x-axis
label = c("Exposed", "Semi-sheltered", "Sheltered", "Pojo Bay")

# Add custom x-axis
axis(1, at = seq(1.5, 7.5, 2), labels = label, tick = FALSE, cex.axis = 1)

# Add legend
legend("topleft", legend = c("Deep","Shallow"), fill = legend_colors)




###boxplot with deep and shallow no scatterplot

## max height
boxplot(cwm_Height ~ Site_group * Depth, data = cwm_allsitedepthspefic,
        main = "Cwm of maximum height at deep and shallow sites", boxwex = 0.7, col = boxplot_colors,
        xlab = NA, ylab = "cwm of max height (cm)", lex.order = TRUE, xaxt = "n", cex.lab = 1.1, cex.main =1.5)
label = c("Exposed", "Semi-sheltered", "Sheltered", "Pojo Bay")

axis(1, at = seq(1.5, 7.5, 2), labels = label, tick = FALSE, cex.axis = 1)
legend("topleft", legend = c("Deep","Shallow"), fill = legend_colors)

##SLA
boxplot(cwm_SLA ~ Site_group * Depth, data = cwm_allsitedepthspefic,
        main = "Cwm of Specific Leaf Area at deep and shallow sites", boxwex = 0.7, col = boxplot_colors,
        xlab = NA, ylab = "cwm of SLA (cm2/mg)", lex.order = TRUE, xaxt = "n", cex.lab = 1.1, cex.main =1.5)
label = c("Exposed", "Semi-sheltered", "Sheltered", "Pojo Bay")

axis(1, at = seq(1.5, 7.5, 2), labels = label, tick = FALSE, cex.axis = 1)
legend("topleft", legend = c("Deep","Shallow"), fill = legend_colors)



####Colors by depth max height (site means)

character_position <- 2
split_characters <- strsplit(as.character(cwm_allsitedepthspefic$Site), "")
cwm_allsitedepthspefic$Site_number <- sapply(split_characters, function(x) ifelse(length(x) >= character_position, x[character_position], NA))
View(cwm_allsitedepthspefic)


cwm_allsitemeans_Height <- aggregate(x= cwm_allsitedepthspefic$cwm_Height, by = list(cwm_allsitedepthspefic$Site_number,
                                                                              cwm_allsitedepthspefic$Depth, cwm_allsitedepthspefic$Site_group, 
                                                                              cwm_allsitedepthspefic$Site_Depth), FUN = mean)
colnames(cwm_allsitemeans_Height) <-c("Site_number", "Depth","Site_group","Site_depth","cwm_Height")

cwm_allsitemeans_Height$Site_group <- factor(cwm_allsitemeans_Height$Site_group, 
                                            levels=c("exposed", "semi","sheltered", "Pojo" ))
cwm_allsitemeans_Height <- cwm_allsitemeans_Height[order(cwm_allsitemeans_Height$Site_group), ]
cwm_allsitemeans_Height$Depth <- factor(cwm_allsitemeans_Height$Depth, levels = c("Deep", "Shallow"))

# Boxplot colors and legend colors
boxplot_colors <- c("lightblue","bisque","lightblue","bisque","lightblue","bisque","lightblue","bisque")
scatterplot_colors <- c("lightblue","bisque","lightblue","bisque","bisque","lightblue","bisque")
legend_colors <- c("lightblue","bisque")

# Create the initial boxplot without stripchart
boxplot(cwm_Height ~ Site_group * Depth, data = cwm_allsitemeans_Height,
        main = "Cwm of maximum height at deep and shallow sites", boxwex = 0.5, col = boxplot_colors,
        xlab = NA, ylab = "Average cwm of max height (cm)", lex.order = TRUE, xaxt = "n", cex.lab = 1.1, cex.main =1.5)

# Add the stripchart to the existing plot
stripchart(cwm_Height ~ Site_depth,
           data = cwm_allsitemeans_Height,
           method = "jitter",
           at = c(1.4, 2.4, 3.4, 4.4, 6.4, 7.4, 8.4),
           pch = 17,
           col = scatterplot_colors,
           vertical = TRUE,
           add = TRUE)

# Labels for the x-axis
label = c("Exposed", "Semi-sheltered", "Sheltered", "Pojo Bay")

# Add custom x-axis
axis(1, at = seq(1.5, 7.5, 2), labels = label, tick = FALSE, cex.axis = 1)

# Add legend
legend("topleft", legend = c("Deep","Shallow"), fill = legend_colors)



####Colors by depth SLA (site means)

cwm_allsitemeans_SLA <- aggregate(x= cwm_allsitedepthspefic$cwm_SLA, by = list(cwm_allsitedepthspefic$Site_number,
                                                                                     cwm_allsitedepthspefic$Depth, cwm_allsitedepthspefic$Site_group, 
                                                                                     cwm_allsitedepthspefic$Site_Depth), FUN = mean)
colnames(cwm_allsitemeans_SLA) <-c("Site_number", "Depth","Site_group","Site_depth","cwm_SLA")

cwm_allsitemeans_SLA$Site_group <- factor(cwm_allsitemeans_SLA$Site_group, 
                                             levels=c("exposed", "semi","sheltered", "Pojo" ))
cwm_allsitemeans_SLA <- cwm_allsitemeans_SLA[order(cwm_allsitemeans_SLA$Site_group), ]
cwm_allsitemeans_SLA$Depth <- factor(cwm_allsitemeans_SLA$Depth, levels = c("Deep", "Shallow"))


# Create the initial boxplot without stripchart
boxplot(cwm_SLA ~ Site_group * Depth, data = cwm_allsitemeans_SLA,
        main = "Cwm of Specific Leaf Area at deep and shallow sites", boxwex = 0.5, col = boxplot_colors,
        xlab = NA, ylab = "Average cwm of SLA (cm2/mg)", lex.order = TRUE, xaxt = "n", cex.lab = 1.1, cex.main =1.5)

# Add the stripchart to the existing plot
stripchart(cwm_SLA ~ Site_depth,
           data = cwm_allsitemeans_SLA,
           method = "jitter",
           at = c(1.4, 2.4, 3.4, 4.4, 6.4, 7.4, 8.4),
           pch = 17,
           col = scatterplot_colors,
           vertical = TRUE,
           add = TRUE)

# Labels for the x-axis
label = c("Exposed", "Semi-sheltered", "Sheltered", "Pojo Bay")

# Add custom x-axis
axis(1, at = seq(1.5, 7.5, 2), labels = label, tick = FALSE, cex.axis = 1)

# Add legend
legend("topleft", legend = c("Deep","Shallow"), fill = legend_colors)

