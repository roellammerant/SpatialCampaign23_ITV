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

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_exp_PC1 <- aggregate(x=TraitData_expP$PC1, by=list(TraitData_expP$Depth, TraitData_expP$Species), FUN=median)
Sites_exp_PC2 <- aggregate(x=TraitData_expP$PC2, by=list(TraitData_expP$Depth, TraitData_expP$Species), FUN=median)
names(Sites_exp_PC1)[1:3]<-paste(c("Depth","Species", "PC1"))
names(Sites_exp_PC2)[1:3]<-paste(c("Depth","Species", "PC2"))
Sites_exp_PC1$PC1<- Sites_exp_PC1$PC1- as.numeric(c("1.12","0.19", "0.19","0.34", "0.09", "0.09", "0.93", 
                                                      "0.93", "0.40","0.40"))
Sites_exp_PC2$PC2 <- Sites_exp_PC2$PC2- as.numeric(c("0.57","-0.28", "-0.28","-0.59", "0.18", "0.18", "-0.84", 
                                                     "-0.84", "0.11","0.11"))

Sites_exp_PC1_D <- subset(Sites_exp_PC1, Depth=="D")
Sites_exp_PC1_S <- subset(Sites_exp_PC1, Depth=="S")

Sites_exp_PC2_D <- subset(Sites_exp_PC2, Depth=="D")
Sites_exp_PC2_S <- subset(Sites_exp_PC2, Depth=="S")

Sites_exp_PC1_DB <- as.data.frame(Sites_exp_PC1_D[,c(3)])
rownames(Sites_exp_PC1_DB) <- Sites_exp_PC1_D$Species
names(Sites_exp_PC1_DB)[1]<-paste(c("PC1"))

Sites_exp_PC1_SB <- as.data.frame(Sites_exp_PC1_S[,c(3)])
rownames(Sites_exp_PC1_SB) <- Sites_exp_PC1_S$Species
names(Sites_exp_PC1_SB)[1]<-paste(c("PC1"))

Sites_exp_PC2_DB <- as.data.frame(Sites_exp_PC2_D[,c(3)])
rownames(Sites_exp_PC2_DB) <- Sites_exp_PC2_D$Species
names(Sites_exp_PC2_DB)[1]<-paste(c("PC2"))

Sites_exp_PC2_SB <- as.data.frame(Sites_exp_PC2_S[,c(3)])
rownames(Sites_exp_PC2_SB) <- Sites_exp_PC2_S$Species
names(Sites_exp_PC2_SB)[1]<-paste(c("PC2"))


########################################################
### Community weighted mean value for height trait######
########################################################

Indices_exp_PC1_D <- FD::dbFD(Sites_exp_PC1_DB,Plot_Abun_exp_deep, w.abun = TRUE)
Indices_exp_PC1_S <- FD::dbFD(Sites_exp_PC1_SB,Plot_Abun_exp_shallow, w.abun = TRUE)
Indices_exp_PC2_D <- FD::dbFD(Sites_exp_PC2_DB,Plot_Abun_exp_deep, w.abun = TRUE)
Indices_exp_PC2_S <- FD::dbFD(Sites_exp_PC2_SB,Plot_Abun_exp_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMexp_PC1_D <-as.data.frame(Indices_exp_PC1_D$CWM)
CWMexp_PC1_S <-as.data.frame(Indices_exp_PC1_S$CWM)
CWMexp_PC2_D <-as.data.frame(Indices_exp_PC2_D$CWM)
CWMexp_PC2_S <-as.data.frame(Indices_exp_PC2_S$CWM)

Indices_PCA_expsites <- rbind(CWMexp_PC1_S,CWMexp_PC1_D)
colnames(Indices_PCA_expsites) <- c("CWM_PC1")
Indices_PCA_expsites$CWM_PC2 <- rbind(CWMexp_PC2_S, CWMexp_PC2_D)
Indices_PCA_expsites$Depth <- c(rep("Shallow",31),rep("Deep",26))
Indices_PCA_expsites$Exposure <- c(rep("Exposed",57))

###### Functional Dispersion
FDisexp_PC1_D <-as.data.frame(Indices_exp_PC1_D$FDis)
colnames(FDisexp_PC1_D) <- c("FDis")
FDisexp_PC1_S <-as.data.frame(Indices_exp_PC1_S$FDis)
colnames(FDisexp_PC1_S) <- c("FDis")
FDisexp_PC2_D <-as.data.frame(Indices_exp_PC2_D$FDis)
colnames(FDisexp_PC2_D) <- c("FDis")
FDisexp_PC2_S <-as.data.frame(Indices_exp_PC2_S$FDis)
colnames(FDisexp_PC2_S) <- c("FDis")

Indices_PCA_expsites$FDis_PC1 <- rbind(FDisexp_PC1_S, FDisexp_PC1_D)
Indices_PCA_expsites$FDis_PC2 <- rbind(FDisexp_PC2_S, FDisexp_PC2_D)

Indices_PCA_expsites <- tibble::rownames_to_column(Indices_PCA_expsites,"Site")
rownames(Indices_PCA_expsites)<-c(1:57)

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
Plot_Abun_semi_deep <- Plot_Abun_semi[c(1:30),-c(4,5)]
Plot_Abun_semi_shallow <- Plot_Abun_semi[c(31:60),-c(9)]

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_semi_PC1 <- aggregate(x=TraitData_semiP$PC1, by=list(TraitData_semiP$Depth, TraitData_semiP$Species), FUN=median)
Sites_semi_PC2 <- aggregate(x=TraitData_semiP$PC2, by=list(TraitData_semiP$Depth, TraitData_semiP$Species), FUN=median)
names(Sites_semi_PC1)[1:3]<-paste(c("Depth","Species", "PC1"))
names(Sites_semi_PC2)[1:3]<-paste(c("Depth","Species", "PC2"))
Sites_semi_PC1$PC1 <- Sites_semi_PC1$PC1 - as.numeric(c("-1.29","-1.29", "1.12","1.12", "0.19","0.19", "2.62", "-0.65", "0.34", 
                                   "0.34","0.09","0.09","0.93","0.93","0.40"))
Sites_semi_PC2$PC2 <-  Sites_semi_PC2$PC2 - as.numeric(c("0.22","0.22", "0.57","0.57", "-0.28","-0.28", "2.81", "-0.06", "0.59", 
                                   "0.59","0.18","0.18","-0.84","-0.84","0.11"))


Site_semi_PC1_D <- subset(Sites_semi_PC1, Depth=="D")
Site_semi_PC1_S <- subset(Sites_semi_PC1, Depth=="S")

Site_semi_PC2_D <- subset(Sites_semi_PC2, Depth=="D")
Site_semi_PC2_S <- subset(Sites_semi_PC2, Depth=="S")

Site_semi_PC1_DB <- as.data.frame(Site_semi_PC1_D[,c(3)])
rownames(Site_semi_PC1_DB) <- Site_semi_PC1_D$Species
names(Site_semi_PC1_DB)[1]<-paste(c("PC1"))

Site_semi_PC1_SB <- as.data.frame(Site_semi_PC1_S[,c(3)])
rownames(Site_semi_PC1_SB) <- Site_semi_PC1_S$Species
names(Site_semi_PC1_SB)[1]<-paste(c("PC1"))

Site_semi_PC2_DB <- as.data.frame(Site_semi_PC2_D[,c(3)])
rownames(Site_semi_PC2_DB) <- Site_semi_PC2_D$Species
names(Site_semi_PC2_DB)[1]<-paste(c("PC2"))

Site_semi_PC2_SB <- as.data.frame(Site_semi_PC2_S[,c(3)])
rownames(Site_semi_PC2_SB) <- Site_semi_PC2_S$Species
names(Site_semi_PC2_SB)[1]<-paste(c("PC2"))


########################################################
### Community weighted mean value for height trait######
########################################################

Indices_semi_PC1_D <- FD::dbFD(Site_semi_PC1_DB,Plot_Abun_semi_deep, w.abun = TRUE)
Indices_semi_PC1_S <- FD::dbFD(Site_semi_PC1_SB,Plot_Abun_semi_shallow, w.abun = TRUE)
Indices_semi_PC2_D <- FD::dbFD(Site_semi_PC2_DB,Plot_Abun_semi_deep, w.abun = TRUE)
Indices_semi_PC2_S <- FD::dbFD(Site_semi_PC2_SB,Plot_Abun_semi_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMsemi_PC1_D <-as.data.frame(Indices_semi_PC1_D$CWM)
CWMsemi_PC1_S <-as.data.frame(Indices_semi_PC1_S$CWM)
CWMsemi_PC2_D <-as.data.frame(Indices_semi_PC2_D$CWM)
CWMsemi_PC2_S <-as.data.frame(Indices_semi_PC2_S$CWM)

Indices_PCA_semisites <- rbind(CWMsemi_PC1_S,CWMsemi_PC1_D)
colnames(Indices_PCA_semisites) <- c("CWM_PC1")
Indices_PCA_semisites$CWM_PC2 <- rbind(CWMsemi_PC2_S,CWMsemi_PC2_D)
Indices_PCA_semisites$Depth <- c(rep("Shallow",30),rep("Deep",30))
Indices_PCA_semisites$Exposure <- c(rep("semi",60))

###### Functional Dispersion
FDissemi_PC1_D <-as.data.frame(Indices_semi_PC1_D$FDis)
colnames(FDissemi_PC1_D) <- c("FDis")
FDissemi_PC1_S <-as.data.frame(Indices_semi_PC1_S$FDis)
colnames(FDissemi_PC1_S) <- c("FDis")
FDissemi_PC2_D <-as.data.frame(Indices_semi_PC2_D$FDis)
colnames(FDissemi_PC2_D) <- c("FDis")
FDissemi_PC2_S <-as.data.frame(Indices_semi_PC2_S$FDis)
colnames(FDissemi_PC2_S) <- c("FDis")

Indices_PCA_semisites$FDis_PC1 <- rbind(FDissemi_PC1_S,FDissemi_PC1_D)
Indices_PCA_semisites$FDis_PC2 <- rbind(FDissemi_PC2_S,FDissemi_PC2_D)

Indices_PCA_semisites <- tibble::rownames_to_column(Indices_PCA_semisites,"Site")
rownames(Indices_PCA_semisites)<-c(1:60)

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
Sites_shel_PC1$PC1 <- Sites_shel_PC1$PC1 - as.numeric(c("-0.92","-1.29", "-1.45","0.88", "1.12", "-1.29", "0.19", "-0.65", 
                                   "0.09"))
Sites_shel_PC2$PC2 <- Sites_shel_PC2$PC2 - as.numeric(c("0.01","0.22", "0.04","0.74", "0.57", "0.29", "-0.28", "0.06", 
                                   "0.18"))


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
Plot_Abun_pojo_deep <- Plot_Abun_pojo[c(1:8),-c(3,5,6,8,10,12)]
Plot_Abun_pojo_shallow <- Plot_Abun_pojo[c(9:38),-c(1)]

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_pojo_PC1 <- aggregate(x=TraitData_pojoP$PC1, by=list(TraitData_pojoP$Depth, TraitData_pojoP$Species), FUN=median)
Sites_pojo_PC2 <- aggregate(x=TraitData_pojoP$PC2, by=list(TraitData_pojoP$Depth, TraitData_pojoP$Species), FUN=median)
names(Sites_pojo_PC1)[1:3]<-paste(c("Depth","Species", "PC1"))
names(Sites_pojo_PC2)[1:3]<-paste(c("Depth","Species", "PC2"))
Sites_pojo_PC1$PC1 <- Sites_pojo_PC1$PC1 - as.numeric(c("-0.92","-1.29", "-1.29","-1.47", "-1.45", "-1.45", "0.88", "1.12", 
                                   "-1.29","-1.29","1.44","-0.67", "-0.67","0.09", "-0.65", "-0.65", "-0.05"))
Sites_pojo_PC2$PC2 <- Sites_pojo_PC2$PC2 - as.numeric(c("0.01","0.22", "0.22","0.10", "0.04", "0.04", "0.74", "0.57", 
                                   "0.29","0.29","-1.05", "0.25","0.25", "-0.24", "0.06", "0.06", "-0.11"))

Site_pojo_PC1_D <- subset(Sites_pojo_PC1, Depth=="D")
Site_pojo_PC1_S <- subset(Sites_pojo_PC1, Depth=="S")

Site_pojo_PC2_D <- subset(Sites_pojo_PC2, Depth=="D")
Site_pojo_PC2_S <- subset(Sites_pojo_PC2, Depth=="S")

Site_pojo_PC1_DB <- as.data.frame(Site_pojo_PC1_D[,c(3)])
rownames(Site_pojo_PC1_DB) <- Site_pojo_PC1_D$Species
names(Site_pojo_PC1_DB)[1]<-paste(c("PC1"))

Site_pojo_PC1_SB <- as.data.frame(Site_pojo_PC1_S[,c(3)])
rownames(Site_pojo_PC1_SB) <- Site_pojo_PC1_S$Species
names(Site_pojo_PC1_SB)[1]<-paste(c("PC1"))

Site_pojo_PC2_DB <- as.data.frame(Site_pojo_PC2_D[,c(3)])
rownames(Site_pojo_PC2_DB) <- Site_pojo_PC2_D$Species
names(Site_pojo_PC2_DB)[1]<-paste(c("PC2"))

Site_pojo_PC2_SB <- as.data.frame(Site_pojo_PC2_S[,c(3)])
rownames(Site_pojo_PC2_SB) <- Site_pojo_PC2_S$Species
names(Site_pojo_PC2_SB)[1]<-paste(c("PC2"))


########################################################
### Community weighted mean value for height trait######
########################################################

Indices_pojo_PC1_D <- FD::dbFD(Site_pojo_PC1_DB,Plot_Abun_pojo_deep, w.abun = TRUE)
Indices_pojo_PC1_S <- FD::dbFD(Site_pojo_PC1_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)
Indices_pojo_PC2_D <- FD::dbFD(Site_pojo_PC2_DB,Plot_Abun_pojo_deep, w.abun = TRUE)
Indices_pojo_PC2_S <- FD::dbFD(Site_pojo_PC2_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMpojo_PC1_D <-as.data.frame(Indices_pojo_PC1_D$CWM)
CWMpojo_PC1_S <-as.data.frame(Indices_pojo_PC1_S$CWM)
CWMpojo_PC2_D <-as.data.frame(Indices_pojo_PC2_D$CWM)
CWMpojo_PC2_S <-as.data.frame(Indices_pojo_PC2_S$CWM)

Indices_PCA_pojosites <- rbind(CWMpojo_PC1_D,CWMpojo_PC1_S)
colnames(Indices_PCA_pojosites) <- c("CWM_PC1")
Indices_PCA_pojosites$CWM_PC2 <- rbind(CWMpojo_PC2_D,CWMpojo_PC2_S)
Indices_PCA_pojosites$Depth <- c(rep("Deep",8),rep("Shallow",30))
Indices_PCA_pojosites$Exposure <- c(rep("Pojo",38))

###### Functional Dispersion
FDispojo_PC1_D <-as.data.frame(Indices_pojo_PC1_D$FDis)
colnames(FDispojo_PC1_D) <- c("FDis")
FDispojo_PC1_S <-as.data.frame(Indices_pojo_PC1_S$FDis)
colnames(FDispojo_PC1_S) <- c("FDis")
FDispojo_PC2_D <-as.data.frame(Indices_pojo_PC2_D$FDis)
colnames(FDispojo_PC2_D) <- c("FDis")
FDispojo_PC2_S <-as.data.frame(Indices_pojo_PC2_S$FDis)
colnames(FDispojo_PC2_S) <- c("FDis")

Indices_PCA_pojosites$FDis_PC1 <- rbind(FDispojo_PC1_D,FDispojo_PC1_S)
Indices_PCA_pojosites$FDis_PC2 <- rbind(FDispojo_PC2_D,FDispojo_PC2_S)

Indices_PCA_pojosites <- tibble::rownames_to_column(Indices_PCA_pojosites,"Site")
rownames(Indices_PCA_pojosites)<-c(1:38)
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
Indices_PCA_Fixed$Site_number<- c(rep("1S",8),rep("2S",5),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",8),rep("2D",6),rep("3D",6),rep("4D",6),
                                  rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",6),rep("2D",6),rep("3D",6),rep("4D",6),rep("5D",6),
                                  rep("2D",2),rep("3D",6),rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),
                                  rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6))

Indices_PCA_Fixed$Exposure_Depth <- factor(paste(Indices_PCA_Fixed$Exposure, 
                                                 Indices_PCA_Fixed$Depth, sep = "_"),
                                           levels = c(
                                             "Exposed_Shallow", "Exposed_Deep",
                                             "semi_Shallow", "semi_Deep",
                                             "Pojo_Shallow", "Pojo_Deep",
                                             "Sheltered_Shallow"
                                           ))


Indices_PCA_Fixed$Site_Exposure <- factor(paste(Indices_PCA_Fixed$Site_number, 
                                                Indices_PCA_Fixed$Exposure, sep = "_"),
                                          levels = c(
                                            "1S_Exposed", "2S_Exposed", "3S_Exposed", "4S_Exposed", "5S_Exposed", "1D_Exposed", "2D_Exposed", "3D_Exposed", "4D_Exposed",
                                            "1S_semi", "2S_semi", "3S_semi", "4S_semi", "5S_semi", "1D_semi", "2D_semi", "3D_semi", "4D_semi", "5D_semi",
                                            "1S_Pojo", "2S_Pojo", "3S_Pojo", "4S_Pojo", "5S_Pojo", "2D_Pojo", "3D_Pojo",
                                            "1S_Sheltered", "2S_Sheltered", "3S_Sheltered", "4S_Sheltered", "5S_Sheltered"
                                          ))

