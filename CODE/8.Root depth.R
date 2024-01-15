##########################################
###########          #####################
########### EXPOSED ######################
###########         ######################
##########################################
Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")

TraitData_expA <- Spatial_Campaign_data_exp[-c(43),c(1:4, 20)]
names(TraitData_expA)[5]<-paste(c("RootDepth"))
TraitData_expA[is.na(TraitData_expA)] = 0


AbundanceData_exp <- Spatial_Campaign_data_exp[,c(1:4, 33)]
names(AbundanceData_exp)[5]<-paste(c("TotalDryMass"))
names(AbundanceData_exp)[2]<-paste(c("Site_number"))

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass
Plot_Abun_exp <- with(AbundanceData_exp, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_exp[is.na(Plot_Abun_exp)] = 0

##### Abundance per depth
Plot_Abun_exp_deep <- Plot_Abun_exp[c(1:26),-c(1,3)]
Plot_Abun_exp_shallow <- Plot_Abun_exp[c(27:57),]

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_exp_RootDepth <- aggregate(x=TraitData_expA$RootDepth, by=list(TraitData_expA$Depth, TraitData_expA$Species), FUN=max)
names(Sites_exp_RootDepth)[1:3]<-paste(c("Depth","Species", "RootDepth"))

Sites_exp_RootDepth_D <- subset(Sites_exp_RootDepth, Depth=="D")
Sites_exp_RootDepth_S <- subset(Sites_exp_RootDepth, Depth=="S")

Sites_exp_RootDepth_DB <- as.data.frame(Sites_exp_RootDepth_D[,c(3)])
rownames(Sites_exp_RootDepth_DB) <- Sites_exp_RootDepth_D$Species
names(Sites_exp_RootDepth_DB)[1]<-paste(c("RootDepth"))

Sites_exp_RootDepth_SB <- as.data.frame(Sites_exp_RootDepth_S[,c(3)])
rownames(Sites_exp_RootDepth_SB) <- Sites_exp_RootDepth_S$Species
names(Sites_exp_RootDepth_SB)[1]<-paste(c("RootDepth"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_exp_RootDepth_D <- FD::dbFD(Sites_exp_RootDepth_DB,Plot_Abun_exp_deep, w.abun = TRUE)
Indices_exp_RootDepth_S <- FD::dbFD(Sites_exp_RootDepth_SB,Plot_Abun_exp_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMexp_RootDepth_D <-as.data.frame(Indices_exp_RootDepth_D$CWM)
CWMexp_RootDepth_S <-as.data.frame(Indices_exp_RootDepth_S$CWM)

Indices_RootDepth_expsites <- rbind(CWMexp_RootDepth_D,CWMexp_RootDepth_S)
colnames(Indices_RootDepth_expsites) <- c("CWM_RootDepth")
Indices_RootDepth_expsites$Depth <- c(rep("Deep",26),rep("Shallow",31))
Indices_RootDepth_expsites$Exposure <- c(rep("Exposed",57))

###### Functional Dispersion
FDisexp_RootDepth_D <-as.data.frame(Indices_exp_RootDepth_D$FDis)
colnames(FDisexp_RootDepth_D) <- c("FDis")
FDisexp_RootDepth_S <-as.data.frame(Indices_exp_RootDepth_S$FDis)
colnames(FDisexp_RootDepth_S) <- c("FDis")

Indices_RootDepth_expsites$FDis_RootDepth <- rbind(FDisexp_RootDepth_D,FDisexp_RootDepth_S)
Indices_RootDepth_expsites <- tibble::rownames_to_column(Indices_RootDepth_expsites,"Site")
rownames(Indices_RootDepth_expsites)<-c(1:57)


##########################################
###########          #####################
########### SEMI    ######################
###########         ######################
##########################################

Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")

TraitData_semiA <- Spatial_Campaign_data_semi[,c(1:4, 20)]
TraitData_semiA[is.na(TraitData_semiA)] = 0
names(TraitData_semiA)[5]<-paste(c("RootDepth")) 

AbundanceData_semi <- Spatial_Campaign_data_semi[,c(1:4, 34)]
names(AbundanceData_semi)[5]<-paste(c("TotalDryMass"))
names(AbundanceData_semi)[2]<-paste(c("Site_number"))

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass
Plot_Abun_semi <- with(AbundanceData_semi, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_semi[is.na(Plot_Abun_semi)] = 0

##### Abundance per depth
Plot_Abun_semi_deep <- Plot_Abun_semi[c(1:30),-c(4,5)]
Plot_Abun_semi_shallow <- Plot_Abun_semi[c(31:60),-c(9)]

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_semi_Ratio <- aggregate(x=TraitData_semiA$Ratio, by=list(TraitData_semiA$Depth, TraitData_semiA$Species), FUN=median)
names(Sites_semi_Ratio)[1:3]<-paste(c("Depth","Species", "Ratio"))

Sites_semi_RatioD <- subset(Sites_semi_Ratio, Depth=="D")
Sites_semi_RatioS <- subset(Sites_semi_Ratio, Depth=="S")


Sites_semi_RatioDB <- as.data.frame(Sites_semi_RatioD[,c(3)])
rownames(Sites_semi_RatioDB) <- Sites_semi_RatioD$Species
names(Sites_semi_RatioDB)[1]<-paste(c("Ratio"))

Sites_semi_RatioSB <- as.data.frame(Sites_semi_RatioS[,c(3)])
rownames(Sites_semi_RatioSB) <- Sites_semi_RatioS$Species
names(Sites_semi_RatioSB)[1]<-paste(c("Ratio"))


########################################################
### Community weighted mean value for height trait######
########################################################

Indices_semi_Ratio_D <- FD::dbFD(Sites_semi_RatioDB,Plot_Abun_semi_deep, w.abun = TRUE)
Indices_semi_Ratio_S <- FD::dbFD(Sites_semi_RatioSB,Plot_Abun_semi_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMsemi_Ratio_D <-as.data.frame(Indices_semi_Ratio_D$CWM)
CWMsemi_Ratio_S <-as.data.frame(Indices_semi_Ratio_S$CWM)

Indices_Ratio_semisites <- rbind(CWMsemi_Ratio_D,CWMsemi_Ratio_S)
colnames(Indices_Ratio_semisites) <- c("CWM_Ratio")
Indices_Ratio_semisites$Depth <- c(rep("Deep",30),rep("Shallow",30))
Indices_Ratio_semisites$Exposure <- c(rep("semi",30))

###### Functional Dispersion
FDissemi_ratio_D <-as.data.frame(Indices_semi_Ratio_D$FDis)
colnames(FDissemi_ratio_D) <- c("FDis")
FDissemi_Ratio_S <-as.data.frame(Indices_semi_Ratio_S$FDis)
colnames(FDissemi_Ratio_S) <- c("FDis")

Indices_Ratio_semisites$FDis_Ratio <- rbind(FDissemi_ratio_D,FDissemi_Ratio_S)

Indices_Ratio_semisites <- tibble::rownames_to_column(Indices_Ratio_semisites,"Site")
rownames(Indices_Ratio_semisites)<-c(1:60)

##############################################
###########              #####################
###########  Sheltered   #####################
###########              #####################
##############################################

Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")

TraitData_shelA <- Spatial_Campaign_data_shel[,c(1:4, 34,35)]
TraitData_shelA[is.na(TraitData_shelA)] = 0
TraitData_shelA$Ratio <- TraitData_shelA$`Total belowground dry mass (mg)` / TraitData_shelA$`Total aboveground dry mass (mg)`

AbundanceData_shel <- Spatial_Campaign_data_shel[,c(1:4, 38)]
names(AbundanceData_shel)[5]<-paste(c("TotalDryMass"))
names(AbundanceData_shel)[2]<-paste(c("Site_number"))

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass
Plot_Abun_shel <- with(AbundanceData_shel, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_shel[is.na(Plot_Abun_shel)] = 0

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_shel_Ratio <- aggregate(x=TraitData_shelA$Ratio, by=list(TraitData_shelA$Depth, TraitData_shelA$Species), FUN=median)
names(Sites_shel_Ratio)[1:3]<-paste(c("Depth","Species", "Ratio"))

Sites_shel_RatioB <- as.data.frame(Sites_shel_Ratio[,c(3)])
rownames(Sites_shel_RatioB) <- Sites_shel_Ratio$Species
names(Sites_shel_RatioB)[1]<-paste(c("Ratio"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_shel_Ratio <- FD::dbFD(Sites_shel_RatioB,Plot_Abun_shel, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMshel_Ratio <-as.data.frame(Indices_shel_Ratio$CWM)

Indices_Ratio_shelsites <- CWMshel_Ratio
colnames(Indices_Ratio_shelsites) <- c("CWM_Ratio")
Indices_Ratio_shelsites$Depth <- c(rep("Shallow",30))
Indices_Ratio_shelsites$Exposure <- c(rep("Sheltered",30))

###### Functional Dispersion
FDisshel_Ratio <-as.data.frame(Indices_shel_Ratio$FDis)
colnames(FDisshel_Ratio) <- c("FDis")

Indices_Ratio_shelsites$FDis_Ratio <- FDisshel_Ratio

Indices_Ratio_shelsites <- tibble::rownames_to_column(Indices_Ratio_shelsites,"Site")
rownames(Indices_Ratio_shelsites)<-c(1:30)

##########################################
###########          #####################
########### POJO    ######################
###########         ######################
##########################################

Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")

TraitData_pojoA <- Spatial_Campaign_data_pojo[-c(1,4,6,7),c(1:4, 32,33)]
TraitData_pojoA[is.na(TraitData_pojoA)] = 0
TraitData_pojoA$Ratio <- TraitData_pojoA$`Total belowground dry mass (mg)`/TraitData_pojoA$`Total aboveground dry mass (mg)`

AbundanceData_pojo <- Spatial_Campaign_data_pojo[-c(1,4,6,7),c(1:4, 36)]
names(AbundanceData_pojo)[5]<-paste(c("TotalDryMass"))
names(AbundanceData_pojo)[2]<-paste(c("Site_number"))

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass
Plot_Abun_pojo <- with(AbundanceData_pojo, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_pojo[is.na(Plot_Abun_pojo)] = 0

##### Abundance per depth
Plot_Abun_pojo_deep <- Plot_Abun_pojo[c(1:8),-c(3,5,6,8,10,12)]
Plot_Abun_pojo_shallow <- Plot_Abun_pojo[c(9:38),-c(1)]

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_pojo_Ratio <- aggregate(x=TraitData_pojoA$Ratio, by=list(TraitData_pojoA$Depth, TraitData_pojoA$Species), FUN=median)
names(Sites_pojo_Ratio)[1:3]<-paste(c("Depth","Species", "Ratio"))

Sites_pojo_Ratio_D <- subset(Sites_pojo_Ratio, Depth=="D")
Sites_pojo_Ratio_S <- subset(Sites_pojo_Ratio, Depth=="S")

Sites_pojo_Ratio_DB <- as.data.frame(Sites_pojo_Ratio_D[,c(3)])
rownames(Sites_pojo_Ratio_DB) <- Sites_pojo_Ratio_D$Species
names(Sites_pojo_Ratio_DB)[1]<-paste(c("Ratio"))

Sites_pojo_Ratio_SB <- as.data.frame(Sites_pojo_Ratio_S[,c(3)])
rownames(Sites_pojo_Ratio_SB) <- Sites_pojo_Ratio_S$Species
names(Sites_pojo_Ratio_SB)[1]<-paste(c("Ratio"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_pojo_Ratio_D <- FD::dbFD(Sites_pojo_Ratio_DB,Plot_Abun_pojo_deep, w.abun = TRUE)
Indices_pojo_Ratio_S <- FD::dbFD(Sites_pojo_Ratio_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMpojo_Ratio_D <-as.data.frame(Indices_pojo_Ratio_D$CWM)
CWMpojo_Ratio_S <-as.data.frame(Indices_pojo_Ratio_S$CWM)

Indices_Ratio_pojosites <- rbind(CWMpojo_Ratio_D,CWMpojo_Ratio_S)
colnames(Indices_Ratio_pojosites) <- c("CWM_Ratio")
Indices_Ratio_pojosites$Depth <- c(rep("Deep",8),rep("Shallow",30))
Indices_Ratio_pojosites$Exposure <- c(rep("Pojo",38))

###### Functional Dispersion
FDispojo_Ratio_D <-as.data.frame(Indices_pojo_Ratio_D$FDis)
colnames(FDispojo_Ratio_D) <- c("FDis")
FDispojo_Ratio_S <-as.data.frame(Indices_pojo_Ratio_S$FDis)
colnames(FDispojo_Ratio_S) <- c("FDis")

Indices_Ratio_pojosites$FDis_Ratio <- rbind(FDispojo_Ratio_D,FDispojo_Ratio_S)

Indices_Ratio_pojosites <- tibble::rownames_to_column(Indices_Ratio_pojosites,"Site")
rownames(Indices_Ratio_pojosites)<-c(1:38)


##################################################
###########                 ######################
###########    Combined     ######################
###########                 ######################
##################################################

Indices_Ratio_expsites <- as.matrix.data.frame(Indices_Ratio_expsites)
Indices_Ratio_pojosites <- as.matrix.data.frame(Indices_Ratio_pojosites)
Indices_Ratio_semisites <- as.matrix.data.frame(Indices_Ratio_semisites)
Indices_Ratio_shelsites <- as.matrix.data.frame(Indices_Ratio_shelsites)
Indices_Ratio_expsites <- as.data.frame(Indices_Ratio_expsites)
Indices_Ratio_pojosites <- as.data.frame(Indices_Ratio_pojosites)
Indices_Ratio_semisites <- as.data.frame(Indices_Ratio_semisites)
Indices_Ratio_shelsites <- as.data.frame(Indices_Ratio_shelsites)

Indices_Ratio_Allsites<- rbind.data.frame(Indices_Ratio_expsites,Indices_Ratio_semisites,Indices_Ratio_pojosites,Indices_Ratio_shelsites)


Indices_Ratio_Allsites$Exposure_Depth <- factor(paste(Indices_Ratio_Allsites$Exposure, 
                                                      Indices_Ratio_Allsites$Depth, sep = "_"),
                                                levels = c(
                                                  "Exposed_Shallow", "Exposed_Deep",
                                                  "semi_Shallow", "semi_Deep",
                                                  "Pojo_Shallow", "Pojo_Deep",
                                                  "Sheltered_Shallow"
                                                ))

