##%######################################################%##
#                                                          #
####      Fixed community indices for PCA               ####
#                                                          #
##%######################################################%##

Fixed_PC1 <- aggregate(x=All_PCA$PC1, by=list(All_PCA$Species), FUN=median)
Fixed_PC2 <- aggregate(x=All_PCA$PC2, by=list(All_PCA$Species), FUN=median)

##%######################################################%##
#                                                          #
####                 Community indices                  ####
#                                                          #
##%######################################################%##

##########################################
###########          #####################
########### EXPOSED ######################
###########         ######################
##########################################

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

Sites_exp_PC1 <- aggregate(x=TraitData_expP$PC1, by=list(TraitData_expP$Depth, TraitData_expP$Species), FUN=median)
Sites_exp_PC2 <- aggregate(x=TraitData_expP$PC2, by=list(TraitData_expP$Depth, TraitData_expP$Species), FUN=median)
names(Sites_exp_PC1)[1:3]<-paste(c("Depth","Species", "PC1"))
names(Sites_exp_PC2)[1:3]<-paste(c("Depth","Species", "PC2"))
Sites_exp_PC1_S <- subset(Sites_exp_PC1, Depth=="S")
Sites_exp_PC2_S <- subset(Sites_exp_PC2, Depth=="S")

Sites_exp_PC1_S$PC1 <- as.numeric(c("-1.217","-0.117", "-0.427","-0.365", "-0.936", "-0.545"))
Sites_exp_PC1_S$PC2 <- as.numeric(c("0.755","-0.263", "-0.675","0.198", "-1.063", "-0.432"))



Sites_exp_PC1_SB <- as.data.frame(Sites_exp_PC1_S[,c(3)])
rownames(Sites_exp_PC1_SB) <- Sites_exp_PC1_S$Species
names(Sites_exp_PC1_SB)[1]<-paste(c("PC1"))

Sites_exp_PC2_SB <- as.data.frame(Sites_exp_PC2_S[,c(3)])
rownames(Sites_exp_PC2_SB) <- Sites_exp_PC2_S$Species
names(Sites_exp_PC2_SB)[1]<-paste(c("PC2"))


########################################################
### Community weighted mean value for height trait######
########################################################

Indices_exp_PC1_S <- FD::dbFD(Sites_exp_PC1_SB,Plot_Abun_exp_shallow, w.abun = TRUE)
Indices_exp_PC2_S <- FD::dbFD(Sites_exp_PC2_SB,Plot_Abun_exp_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMexp_PC1_S <-as.data.frame(Indices_exp_PC1_S$CWM)
CWMexp_PC2_S <-as.data.frame(Indices_exp_PC2_S$CWM)

Indices_PCA_expsites <- CWMexp_PC1_S
colnames(Indices_PCA_expsites) <- c("CWM_PC1")
Indices_PCA_expsites$CWM_PC2 <- CWMexp_PC2_S
Indices_PCA_expsites$Depth <- c(rep("Shallow",31))
Indices_PCA_expsites$Exposure <- c(rep("Exposed",31))

###### Functional Dispersion
FDisexp_PC1_S <-as.data.frame(Indices_exp_PC1_S$FDis)
colnames(FDisexp_PC1_S) <- c("FDis")
FDisexp_PC2_S <-as.data.frame(Indices_exp_PC2_S$FDis)
colnames(FDisexp_PC2_S) <- c("FDis")

Indices_PCA_expsites$FDis_PC1 <- FDisexp_PC1_S
Indices_PCA_expsites$FDis_PC2 <- FDisexp_PC2_S

Indices_PCA_expsites <- tibble::rownames_to_column(Indices_PCA_expsites,"Site")
rownames(Indices_PCA_expsites)<-c(1:31)

##########################################
###########          #####################
########### SEMI    ######################
###########         ######################
##########################################

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
Plot_Abun_semi_shallow <- Plot_Abun_semi[c(31:60),-c(9)]

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_semi_PC1 <- aggregate(x=TraitData_semiP$PC1, by=list(TraitData_semiP$Depth, TraitData_semiP$Species), FUN=median)
Sites_semi_PC2 <- aggregate(x=TraitData_semiP$PC2, by=list(TraitData_semiP$Depth, TraitData_semiP$Species), FUN=median)
names(Sites_semi_PC1)[1:3]<-paste(c("Depth","Species", "PC1"))
names(Sites_semi_PC2)[1:3]<-paste(c("Depth","Species", "PC2"))
Site_semi_PC1_S <- subset(Sites_semi_PC1, Depth=="S")
Site_semi_PC2_S <- subset(Sites_semi_PC2, Depth=="S")

Site_semi_PC1_S$PC1 <- as.numeric(c("0.915","-1.218", "-0.117","-2.777", "0.465","-0.427", "-0.365", "-0.936"))
Site_semi_PC2_S$PC2 <- as.numeric(c("0.416","0.755", "-0.262","2.454", "0.154","-0.675", "0.198", "-1.062"))



Site_semi_PC1_SB <- as.data.frame(Site_semi_PC1_S[,c(3)])
rownames(Site_semi_PC1_SB) <- Site_semi_PC1_S$Species
names(Site_semi_PC1_SB)[1]<-paste(c("PC1"))

Site_semi_PC2_SB <- as.data.frame(Site_semi_PC2_S[,c(3)])
rownames(Site_semi_PC2_SB) <- Site_semi_PC2_S$Species
names(Site_semi_PC2_SB)[1]<-paste(c("PC2"))


########################################################
### Community weighted mean value for height trait######
########################################################

Indices_semi_PC1_S <- FD::dbFD(Site_semi_PC1_SB,Plot_Abun_semi_shallow, w.abun = TRUE)
Indices_semi_PC2_S <- FD::dbFD(Site_semi_PC2_SB,Plot_Abun_semi_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMsemi_PC1_S <-as.data.frame(Indices_semi_PC1_S$CWM)
CWMsemi_PC2_S <-as.data.frame(Indices_semi_PC2_S$CWM)

Indices_PCA_semisites <- CWMsemi_PC1_S
colnames(Indices_PCA_semisites) <- c("CWM_PC1")
Indices_PCA_semisites$CWM_PC2 <- CWMsemi_PC2_S
Indices_PCA_semisites$Depth <- c(rep("Shallow",30))
Indices_PCA_semisites$Exposure <- c(rep("semi",30))

###### Functional Dispersion
FDissemi_PC1_S <-as.data.frame(Indices_semi_PC1_S$FDis)
colnames(FDissemi_PC1_S) <- c("FDis")
FDissemi_PC2_S <-as.data.frame(Indices_semi_PC2_S$FDis)
colnames(FDissemi_PC2_S) <- c("FDis")

Indices_PCA_semisites$FDis_PC1 <- rbind(FDissemi_PC1_S)
Indices_PCA_semisites$FDis_PC2 <- rbind(FDissemi_PC2_S)

Indices_PCA_semisites <- tibble::rownames_to_column(Indices_PCA_semisites,"Site")
rownames(Indices_PCA_semisites)<-c(1:30)

##############################################
###########              #####################
###########  Sheltered   #####################
###########              #####################
##############################################

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

Sites_shel_PC1 <- aggregate(x=TraitData_shelP$SLA, by=list(TraitData_shelP$Depth, TraitData_shelP$Species), FUN=median)
Sites_shel_PC2 <- aggregate(x=TraitData_shelP$LAP, by=list(TraitData_shelP$Depth, TraitData_shelP$Species), FUN=median)
names(Sites_shel_PC1)[1:3]<-paste(c("Depth","Species", "PC1"))
names(Sites_shel_PC2)[1:3]<-paste(c("Depth","Species", "PC2"))
Sites_shel_PC1$PC1 <- as.numeric(c("1.047","0.915", "1.516","-0.996", "-1.218", "1.015", "-0.117", "0.465", 
                                   "-0.365"))
Sites_shel_PC2$PC2 <- as.numeric(c("-0.00001","0.416", "0.075","0.654", "0.755", "0.382", "-0.263", "0.154", 
                                   "0.198"))


Sites_shel_PC1B <- as.data.frame(Sites_shel_PC1[,c(3)])
rownames(Sites_shel_PC1B) <- Sites_shel_PC1$Species
names(Sites_shel_PC1B)[1]<-paste(c("PC1"))

Sites_shel_PC2B <- as.data.frame(Sites_shel_PC2[,c(3)])
rownames(Sites_shel_PC2B) <- Sites_shel_PC2$Species
names(Sites_shel_PC2B)[1]<-paste(c("PC2"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_shel_PC1 <- FD::dbFD(Sites_shel_PC1B,Plot_Abun_shel, w.abun = TRUE)
Indices_shel_PC2 <- FD::dbFD(Sites_shel_PC2B,Plot_Abun_shel, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMshel_PC1 <-as.data.frame(Indices_shel_PC1$CWM)
CWMshel_PC2 <-as.data.frame(Indices_shel_PC2$CWM)

Indices_PCA_shelsites <- CWMshel_PC1
colnames(Indices_PCA_shelsites) <- c("CWM_PC1")
Indices_PCA_shelsites$CWM_PC2 <- CWMshel_PC2
Indices_PCA_shelsites$Depth <- c(rep("Shallow",30))
Indices_PCA_shelsites$Exposure <- c(rep("Sheltered",30))

###### Functional Dispersion
FDisshel_PC1 <-as.data.frame(Indices_shel_PC1$FDis)
colnames(FDisshel_PC1) <- c("FDis")
FDisshel_PC2 <-as.data.frame(Indices_shel_PC2$FDis)
colnames(FDisshel_PC2) <- c("FDis")

Indices_PCA_shelsites$FDis_PC1 <- FDisshel_PC1
Indices_PCA_shelsites$FDis_PC2 <- FDisshel_PC2

Indices_PCA_shelsites <- tibble::rownames_to_column(Indices_PCA_shelsites,"Site")
rownames(Indices_PCA_shelsites)<-c(1:30)
##########################################
###########          #####################
########### POJO    ######################
###########         ######################
##########################################

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
Plot_Abun_pojo_shallow <- Plot_Abun_pojo[c(9:38),-c(1)]

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_pojo_PC1 <- aggregate(x=TraitData_pojoP$PC1, by=list(TraitData_pojoP$Depth, TraitData_pojoP$Species), FUN=median)
Sites_pojo_PC2 <- aggregate(x=TraitData_pojoP$PC2, by=list(TraitData_pojoP$Depth, TraitData_pojoP$Species), FUN=median)
names(Sites_pojo_PC1)[1:3]<-paste(c("Depth","Species", "PC1"))
names(Sites_pojo_PC2)[1:3]<-paste(c("Depth","Species", "PC12"))

Site_pojo_PC1_S <- subset(Sites_pojo_PC1, Depth=="S")
Site_pojo_PC2_S <- subset(Sites_pojo_PC2, Depth=="S")

Site_pojo_PC1_S$PC1 <- as.numeric(c("0.915","2.597", "1.516","-0.997", "-1.218", "1.015", "-0.713", "0.285", 
                                   "0.136","0.464","0.984"))
Site_pojo_PC2_S$PC2 <- as.numeric(c("0.416","-0.294", "0.075","0.654", "0.755", "0.382", "-1.147", "0.223", 
                                   "-0.051","0.154","-0.365"))



Site_pojo_PC1_SB <- as.data.frame(Site_pojo_PC1_S[,c(3)])
rownames(Site_pojo_PC1_SB) <- Site_pojo_PC1_S$Species
names(Site_pojo_PC1_SB)[1]<-paste(c("PC1"))

Site_pojo_PC2_SB <- as.data.frame(Site_pojo_PC2_S[,c(3)])
rownames(Site_pojo_PC2_SB) <- Site_pojo_PC2_S$Species
names(Site_pojo_PC2_SB)[1]<-paste(c("PC2"))


########################################################
### Community weighted mean value for height trait######
########################################################

Indices_pojo_PC1_S <- FD::dbFD(Site_pojo_PC1_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)
Indices_pojo_PC2_S <- FD::dbFD(Site_pojo_PC2_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMpojo_PC1_S <-as.data.frame(Indices_pojo_PC1_S$CWM)
CWMpojo_PC2_S <-as.data.frame(Indices_pojo_PC2_S$CWM)

Indices_PCA_pojosites <- CWMpojo_PC1_S
colnames(Indices_PCA_pojosites) <- c("CWM_PC1")
Indices_PCA_pojosites$CWM_PC2<- CWMpojo_PC2_S
Indices_PCA_pojosites$Depth <- c(rep("Shallow",30))
Indices_PCA_pojosites$Exposure <- c(rep("Pojo",30))

###### Functional Dispersion
FDispojo_PC1_S <-as.data.frame(Indices_pojo_PC1_S$FDis)
colnames(FDispojo_PC1_S) <- c("FDis")
FDispojo_PC2_S <-as.data.frame(Indices_pojo_PC2_S$FDis)
colnames(FDispojo_PC2_S) <- c("FDis")

Indices_PCA_pojosites$FDis_PC1 <- rbind(FDispojo_PC1_S)
Indices_PCA_pojosites$FDis_PC2 <- rbind(FDispojo_PC2_S)

Indices_PCA_pojosites <- tibble::rownames_to_column(Indices_PCA_pojosites,"Site")
rownames(Indices_PCA_pojosites)<-c(1:30)
##################################################
###########                 ######################
###########    Combined     ######################
###########                 ######################
##################################################
Indices_PCA_expsites <- as.matrix.data.frame(Indices_PCA_expsites)
Indices_PCA_pojosites <- as.matrix.data.frame(Indices_PCA_pojosites)
Indices_PCA_semisites <- as.matrix.data.frame(Indices_PCA_semisites)
Indices_PCA_shelsites <- as.matrix.data.frame(Indices_PCA_shelsites)
Indices_PCA_expsites <- as.data.frame(Indices_PCA_expsites)
Indices_PCA_pojosites <- as.data.frame(Indices_PCA_pojosites)
Indices_PCA_semisites <- as.data.frame(Indices_PCA_semisites)
Indices_PCA_shelsites <- as.data.frame(Indices_PCA_shelsites)

Indices_PCA_Fixed<- rbind.data.frame(Indices_PCA_expsites,Indices_PCA_semisites,Indices_PCA_pojosites,Indices_PCA_shelsites)
Indices_PCA_Fixed$Site_number<- c(rep("1S",8),rep("2S",5),rep("3S",6),rep("4S",6),rep("5S",6),
                                   rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),
                                   rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),
                                   rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6))

Indices_PCA_Fixed$Exposure_Depth <- factor(paste(Indices_PCA_Fixed$Exposure, 
                                                 Indices_PCA_Fixed$Depth, sep = "_"),
                                            levels = c(
                                              "Exposed_Shallow",
                                              "semi_Shallow", 
                                              "Pojo_Shallow", 
                                              "Sheltered_Shallow"
                                            ))


Indices_PCA_Fixed$Site_Exposure <- factor(paste(Indices_PCA_Fixed$Site_number, 
                                                Indices_PCA_Fixed$Exposure, sep = "_"),
                                           levels = c(
                                             "1S_Exposed", "2S_Exposed", "3S_Exposed", "4S_Exposed", "5S_Exposed", 
                                             "1S_semi", "2S_semi", "3S_semi", "4S_semi", "5S_semi", 
                                             "1S_Pojo", "2S_Pojo", "3S_Pojo", "4S_Pojo", "5S_Pojo", 
                                             "1S_Sheltered", "2S_Sheltered", "3S_Sheltered", "4S_Sheltered", "5S_Sheltered"
                                           ))

