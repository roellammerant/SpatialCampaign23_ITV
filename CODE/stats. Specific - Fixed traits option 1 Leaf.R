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

Sites_exp_SLA <- aggregate(x=TraitData_exp$SLA, by=list(TraitData_exp$Depth, TraitData_exp$Species), FUN=median)
Sites_exp_LAP <- aggregate(x=TraitData_exp$LAP, by=list(TraitData_exp$Depth, TraitData_exp$Species), FUN=median)
names(Sites_exp_SLA)[1:3]<-paste(c("Depth","Species", "SLA"))
names(Sites_exp_LAP)[1:3]<-paste(c("Depth","Species", "LAP"))
Sites_exp_SLA$SLA <- Sites_exp_SLA$SLA - as.numeric(c("0.18","0.47", "0.47","0.19", "0.13", "0.13", "0.12", 
                                  "0.12", "0.31","0.31"))
Sites_exp_LAP$LAP <- Sites_exp_LAP$LAP - as.numeric(c("0.09","0.52", "0.52","0.13", "0.06", "0.06", "0.18", 
                                  "0.18", "0.22","0.22"))

Site_exp_SLA_D <- subset(Sites_exp_SLA, Depth=="D")
Site_exp_SLA_S <- subset(Sites_exp_SLA, Depth=="S")

Site_exp_LAP_D <- subset(Sites_exp_LAP, Depth=="D")
Site_exp_LAP_S <- subset(Sites_exp_LAP, Depth=="S")

Site_exp_SLA_DB <- as.data.frame(Site_exp_SLA_D[,c(3)])
rownames(Site_exp_SLA_DB) <- Site_exp_SLA_D$Species
names(Site_exp_SLA_DB)[1]<-paste(c("SLA"))

Site_exp_SLA_SB <- as.data.frame(Site_exp_SLA_S[,c(3)])
rownames(Site_exp_SLA_SB) <- Site_exp_SLA_S$Species
names(Site_exp_SLA_SB)[1]<-paste(c("SLA"))

Site_exp_LAP_DB <- as.data.frame(Site_exp_LAP_D[,c(3)])
rownames(Site_exp_LAP_DB) <- Site_exp_LAP_D$Species
names(Site_exp_LAP_DB)[1]<-paste(c("LAP"))

Site_exp_LAP_SB <- as.data.frame(Site_exp_LAP_S[,c(3)])
rownames(Site_exp_LAP_SB) <- Site_exp_LAP_S$Species
names(Site_exp_LAP_SB)[1]<-paste(c("LAP"))


########################################################
### Community weighted mean value for height trait######
########################################################

Indices_exp_SLA_D <- FD::dbFD(Site_exp_SLA_DB,Plot_Abun_exp_deep, w.abun = TRUE)
Indices_exp_SLA_S <- FD::dbFD(Site_exp_SLA_SB,Plot_Abun_exp_shallow, w.abun = TRUE)
Indices_exp_LAP_D <- FD::dbFD(Site_exp_LAP_DB,Plot_Abun_exp_deep, w.abun = TRUE)
Indices_exp_LAP_S <- FD::dbFD(Site_exp_LAP_SB,Plot_Abun_exp_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMexp_SLA_D <-as.data.frame(Indices_exp_SLA_D$CWM)
CWMexp_SLA_S <-as.data.frame(Indices_exp_SLA_S$CWM)
CWMexp_LAP_D <-as.data.frame(Indices_exp_LAP_D$CWM)
CWMexp_LAP_S <-as.data.frame(Indices_exp_LAP_S$CWM)

Indices_Leaf_expsites <- rbind(CWMexp_SLA_S,CWMexp_SLA_D)
colnames(Indices_Leaf_expsites) <- c("CWM_SLA")
Indices_Leaf_expsites$CWM_LAP <- rbind(CWMexp_LAP_S, CWMexp_LAP_D)
Indices_Leaf_expsites$Depth <- c(rep("Shallow",31),rep("Deep",26))
Indices_Leaf_expsites$Exposure <- c(rep("Exposed",57))

###### Functional Dispersion
FDisexp_SLA_D <-as.data.frame(Indices_exp_SLA_D$FDis)
colnames(FDisexp_SLA_D) <- c("FDis")
FDisexp_SLA_S <-as.data.frame(Indices_exp_SLA_S$FDis)
colnames(FDisexp_SLA_S) <- c("FDis")
FDisexp_LAP_D <-as.data.frame(Indices_exp_LAP_D$FDis)
colnames(FDisexp_LAP_D) <- c("FDis")
FDisexp_LAP_S <-as.data.frame(Indices_exp_LAP_S$FDis)
colnames(FDisexp_LAP_S) <- c("FDis")

Indices_Leaf_expsites$FDis_SLA <- rbind(FDisexp_SLA_S, FDisexp_SLA_D)
Indices_Leaf_expsites$FDis_LAP <- rbind(FDisexp_LAP_S, FDisexp_LAP_D)

Indices_Leaf_expsites <- tibble::rownames_to_column(Indices_Leaf_expsites,"Site")
rownames(Indices_Leaf_expsites)<-c(1:57)

##########################################
###########          #####################
########### SEMI    ######################
###########         ######################
##########################################

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_semi_SLA <- aggregate(x=TraitData_semi$SLA, by=list(TraitData_semi$Depth, TraitData_semi$Species), FUN=median)
Sites_semi_LAP <- aggregate(x=TraitData_semi$LAP, by=list(TraitData_semi$Depth, TraitData_semi$Species), FUN=median)
names(Sites_semi_SLA)[1:3]<-paste(c("Depth","Species", "SLA"))
names(Sites_semi_LAP)[1:3]<-paste(c("Depth","Species", "LAP"))
Sites_semi_SLA$SLA <- Sites_semi_SLA$SLA - as.numeric(c("0.22","0.22", "0.18","0.18", "0.47","0.47", "0.10", "0.06", "0.19", 
                                   "0.19","0.13","0.13","0.12","0.12","0.31"))
Sites_semi_LAP$LAP <- Sites_semi_LAP$LAP - as.numeric(c("0.08","0.08", "0.09","0.09", "0.52","0.52", "0.11", "0.07", "0.13", 
                                   "0.13","0.06","0.06","0.18","0.18","0.22"))


Site_semi_SLA_D <- subset(Sites_semi_SLA, Depth=="D")
Site_semi_SLA_S <- subset(Sites_semi_SLA, Depth=="S")

Site_semi_LAP_D <- subset(Sites_semi_LAP, Depth=="D")
Site_semi_LAP_S <- subset(Sites_semi_LAP, Depth=="S")

Site_semi_SLA_DB <- as.data.frame(Site_semi_SLA_D[,c(3)])
rownames(Site_semi_SLA_DB) <- Site_semi_SLA_D$Species
names(Site_semi_SLA_DB)[1]<-paste(c("SLA"))

Site_semi_SLA_SB <- as.data.frame(Site_semi_SLA_S[,c(3)])
rownames(Site_semi_SLA_SB) <- Site_semi_SLA_S$Species
names(Site_semi_SLA_SB)[1]<-paste(c("SLA"))

Site_semi_LAP_DB <- as.data.frame(Site_semi_LAP_D[,c(3)])
rownames(Site_semi_LAP_DB) <- Site_semi_LAP_D$Species
names(Site_semi_LAP_DB)[1]<-paste(c("LAP"))

Site_semi_LAP_SB <- as.data.frame(Site_semi_LAP_S[,c(3)])
rownames(Site_semi_LAP_SB) <- Site_semi_LAP_S$Species
names(Site_semi_LAP_SB)[1]<-paste(c("LAP"))


########################################################
### Community weighted mean value for height trait######
########################################################

Indices_semi_SLA_D <- FD::dbFD(Site_semi_SLA_DB,Plot_Abun_semi_deep, w.abun = TRUE)
Indices_semi_SLA_S <- FD::dbFD(Site_semi_SLA_SB,Plot_Abun_semi_shallow, w.abun = TRUE)
Indices_semi_LAP_D <- FD::dbFD(Site_semi_LAP_DB,Plot_Abun_semi_deep, w.abun = TRUE)
Indices_semi_LAP_S <- FD::dbFD(Site_semi_LAP_SB,Plot_Abun_semi_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMsemi_SLA_D <-as.data.frame(Indices_semi_SLA_D$CWM)
CWMsemi_SLA_S <-as.data.frame(Indices_semi_SLA_S$CWM)
CWMsemi_LAP_D <-as.data.frame(Indices_semi_LAP_D$CWM)
CWMsemi_LAP_S <-as.data.frame(Indices_semi_LAP_S$CWM)

Indices_Leaf_semisites <- rbind(CWMsemi_SLA_S,CWMsemi_SLA_D)
colnames(Indices_Leaf_semisites) <- c("CWM_SLA")
Indices_Leaf_semisites$CWM_LAP <- rbind(CWMsemi_LAP_S,CWMsemi_LAP_D)
Indices_Leaf_semisites$Depth <- c(rep("Shallow",30),rep("Deep",30))
Indices_Leaf_semisites$Exposure <- c(rep("semi",60))

###### Functional Dispersion
FDissemi_SLA_D <-as.data.frame(Indices_semi_SLA_D$FDis)
colnames(FDissemi_SLA_D) <- c("FDis")
FDissemi_SLA_S <-as.data.frame(Indices_semi_SLA_S$FDis)
colnames(FDissemi_SLA_S) <- c("FDis")
FDissemi_LAP_D <-as.data.frame(Indices_semi_LAP_D$FDis)
colnames(FDissemi_LAP_D) <- c("FDis")
FDissemi_LAP_S <-as.data.frame(Indices_semi_LAP_S$FDis)
colnames(FDissemi_LAP_S) <- c("FDis")

Indices_Leaf_semisites$FDis_SLA <- rbind(FDissemi_SLA_S,FDissemi_SLA_D)
Indices_Leaf_semisites$FDis_LAP <- rbind(FDissemi_LAP_S,FDissemi_LAP_D)

Indices_Leaf_semisites <- tibble::rownames_to_column(Indices_Leaf_semisites,"Site")
rownames(Indices_Leaf_semisites)<-c(1:60)

##############################################
###########              #####################
###########  Sheltered   #####################
###########              #####################
##############################################

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_shel_SLA <- aggregate(x=TraitData_shel$SLA, by=list(TraitData_shel$Depth, TraitData_shel$Species), FUN=median)
Sites_shel_LAP <- aggregate(x=TraitData_shel$LAP, by=list(TraitData_shel$Depth, TraitData_shel$Species), FUN=median)
names(Sites_shel_SLA)[1:3]<-paste(c("Depth","Species", "SLA"))
names(Sites_shel_LAP)[1:3]<-paste(c("Depth","Species", "LAP"))
Sites_shel_SLA$SLA <- Sites_shel_SLA$SLA - as.numeric(c("1.16","0.22", "0.61","0.08", "0.18", "0.14", "0.47", "0.06", 
                                   "0.13"))
Sites_shel_LAP$LAP <- Sites_shel_LAP$LAP - as.numeric(c("0.45","0.08", "0.28","0.10", "0.09", "0.10", "0.52", "0.07", 
                                   "0.06"))


Sites_shel_SLAB <- as.data.frame(Sites_shel_SLA[,c(3)])
rownames(Sites_shel_SLAB) <- Sites_shel_SLA$Species
names(Sites_shel_SLAB)[1]<-paste(c("SLA"))

Sites_shel_LAPB <- as.data.frame(Sites_shel_LAP[,c(3)])
rownames(Sites_shel_LAPB) <- Sites_shel_LAP$Species
names(Sites_shel_LAPB)[1]<-paste(c("LAP"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_shel_SLA <- FD::dbFD(Sites_shel_SLAB,Plot_Abun_shel, w.abun = TRUE)
Indices_shel_LAP <- FD::dbFD(Sites_shel_LAPB,Plot_Abun_shel, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMshel_SLA <-as.data.frame(Indices_shel_SLA$CWM)
CWMshel_LAP <-as.data.frame(Indices_shel_LAP$CWM)

Indices_Leaf_shelsites <- CWMshel_SLA
colnames(Indices_Leaf_shelsites) <- c("CWM_SLA")
Indices_Leaf_shelsites$CWM_LAP <- CWMshel_LAP
Indices_Leaf_shelsites$Depth <- c(rep("Shallow",30))
Indices_Leaf_shelsites$Exposure <- c(rep("Sheltered",30))

###### Functional Dispersion
FDisshel_SLA <-as.data.frame(Indices_shel_SLA$FDis)
colnames(FDisshel_SLA) <- c("FDis")
FDisshel_LAP <-as.data.frame(Indices_shel_LAP$FDis)
colnames(FDisshel_LAP) <- c("FDis")

Indices_Leaf_shelsites$FDis_SLA <- FDisshel_SLA
Indices_Leaf_shelsites$FDis_LAP <- FDisshel_LAP

Indices_Leaf_shelsites <- tibble::rownames_to_column(Indices_Leaf_shelsites,"Site")
rownames(Indices_Leaf_shelsites)<-c(1:30)
##########################################
###########          #####################
########### POJO    ######################
###########         ######################
##########################################

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_pojo_SLA <- aggregate(x=TraitData_pojo$SLA, by=list(TraitData_pojo$Depth, TraitData_pojo$Species), FUN=median)
Sites_pojo_LAP <- aggregate(x=TraitData_pojo$LAP, by=list(TraitData_pojo$Depth, TraitData_pojo$Species), FUN=median)
names(Sites_pojo_SLA)[1:3]<-paste(c("Depth","Species", "SLA"))
names(Sites_pojo_LAP)[1:3]<-paste(c("Depth","Species", "LAP"))
Sites_pojo_SLA$SLA <- Sites_pojo_SLA$SLA - as.numeric(c("1.16","0.22", "0.22","0.75", "0.61", "0.61", "0.08", "0.18", 
                                   "0.14","0.14","0.72","0.80", "0.80","0.39", "0.06", "0.06", "0.97"))
Sites_pojo_LAP$LAP <- Sites_pojo_LAP$LAP - as.numeric(c("0.45","0.08", "0.08","0.11", "0.28", "0.28", "0.10", "0.09", 
                                   "0.10","0.10","1.91", "0.63","0.63", "0.44", "0.07", "0.07", "1.15"))

Site_pojo_SLA_D <- subset(Sites_pojo_SLA, Depth=="D")
Site_pojo_SLA_S <- subset(Sites_pojo_SLA, Depth=="S")

Site_pojo_LAP_D <- subset(Sites_pojo_LAP, Depth=="D")
Site_pojo_LAP_S <- subset(Sites_pojo_LAP, Depth=="S")

Site_pojo_SLA_DB <- as.data.frame(Site_pojo_SLA_D[,c(3)])
rownames(Site_pojo_SLA_DB) <- Site_pojo_SLA_D$Species
names(Site_pojo_SLA_DB)[1]<-paste(c("SLA"))

Site_pojo_SLA_SB <- as.data.frame(Site_pojo_SLA_S[,c(3)])
rownames(Site_pojo_SLA_SB) <- Site_pojo_SLA_S$Species
names(Site_pojo_SLA_SB)[1]<-paste(c("SLA"))

Site_pojo_LAP_DB <- as.data.frame(Site_pojo_LAP_D[,c(3)])
rownames(Site_pojo_LAP_DB) <- Site_pojo_LAP_D$Species
names(Site_pojo_LAP_DB)[1]<-paste(c("LAP"))

Site_pojo_LAP_SB <- as.data.frame(Site_pojo_LAP_S[,c(3)])
rownames(Site_pojo_LAP_SB) <- Site_pojo_LAP_S$Species
names(Site_pojo_LAP_SB)[1]<-paste(c("LAP"))


########################################################
### Community weighted mean value for height trait######
########################################################

Indices_pojo_SLA_D <- FD::dbFD(Site_pojo_SLA_DB,Plot_Abun_pojo_deep, w.abun = TRUE)
Indices_pojo_SLA_S <- FD::dbFD(Site_pojo_SLA_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)
Indices_pojo_LAP_D <- FD::dbFD(Site_pojo_LAP_DB,Plot_Abun_pojo_deep, w.abun = TRUE)
Indices_pojo_LAP_S <- FD::dbFD(Site_pojo_LAP_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMpojo_SLA_D <-as.data.frame(Indices_pojo_SLA_D$CWM)
CWMpojo_SLA_S <-as.data.frame(Indices_pojo_SLA_S$CWM)
CWMpojo_LAP_D <-as.data.frame(Indices_pojo_LAP_D$CWM)
CWMpojo_LAP_S <-as.data.frame(Indices_pojo_LAP_S$CWM)

Indices_Leaf_pojosites <- rbind(CWMpojo_SLA_D,CWMpojo_SLA_S)
colnames(Indices_Leaf_pojosites) <- c("CWM_SLA")
Indices_Leaf_pojosites$CWM_LAP <- rbind(CWMpojo_LAP_D,CWMpojo_LAP_S)
Indices_Leaf_pojosites$Depth <- c(rep("Deep",8),rep("Shallow",30))
Indices_Leaf_pojosites$Exposure <- c(rep("Pojo",38))

###### Functional Dispersion
FDispojo_SLA_D <-as.data.frame(Indices_pojo_SLA_D$FDis)
colnames(FDispojo_SLA_D) <- c("FDis")
FDispojo_SLA_S <-as.data.frame(Indices_pojo_SLA_S$FDis)
colnames(FDispojo_SLA_S) <- c("FDis")
FDispojo_LAP_D <-as.data.frame(Indices_pojo_LAP_D$FDis)
colnames(FDispojo_LAP_D) <- c("FDis")
FDispojo_LAP_S <-as.data.frame(Indices_pojo_LAP_S$FDis)
colnames(FDispojo_LAP_S) <- c("FDis")

Indices_Leaf_pojosites$FDis_SLA <- rbind(FDispojo_SLA_D,FDispojo_SLA_S)
Indices_Leaf_pojosites$FDis_LAP <- rbind(FDispojo_LAP_D,FDispojo_LAP_S)

Indices_Leaf_pojosites <- tibble::rownames_to_column(Indices_Leaf_pojosites,"Site")
rownames(Indices_Leaf_pojosites)<-c(1:38)
##################################################
###########                 ######################
###########    Combined     ######################
###########                 ######################
##################################################
Indices_Leaf_expsites <- as.matrix.data.frame(Indices_Leaf_expsites)
Indices_Leaf_pojosites <- as.matrix.data.frame(Indices_Leaf_pojosites)
Indices_Leaf_semisites <- as.matrix.data.frame(Indices_Leaf_semisites)
Indices_Leaf_shelsites <- as.matrix.data.frame(Indices_Leaf_shelsites)
Indices_Leaf_expsites <- as.data.frame(Indices_Leaf_expsites)
Indices_Leaf_pojosites <- as.data.frame(Indices_Leaf_pojosites)
Indices_Leaf_semisites <- as.data.frame(Indices_Leaf_semisites)
Indices_Leaf_shelsites <- as.data.frame(Indices_Leaf_shelsites)

Indices_Leaf_SpecificMinusFixedA<- rbind.data.frame(Indices_Leaf_expsites,Indices_Leaf_semisites,Indices_Leaf_pojosites,Indices_Leaf_shelsites)
Indices_Leaf_SpecificMinusFixedA$Site_number<- c(rep("1S",8),rep("2S",5),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",8),rep("2D",6),rep("3D",6),rep("4D",6),
                                   rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",6),rep("2D",6),rep("3D",6),rep("4D",6),rep("5D",6),
                                   rep("2D",2),rep("3D",6),rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),
                                   rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6))

Indices_Leaf_SpecificMinusFixedA$Exposure_Depth <- factor(paste(Indices_Leaf_SpecificMinusFixedA$Exposure, 
                                                                Indices_Leaf_SpecificMinusFixedA$Depth, sep = "_"),
                                            levels = c(
                                              "Exposed_Shallow", "Exposed_Deep",
                                              "semi_Shallow", "semi_Deep",
                                              "Pojo_Shallow", "Pojo_Deep",
                                              "Sheltered_Shallow"
                                            ))


Indices_Leaf_SpecificMinusFixedA$Site_Exposure <- factor(paste(Indices_Leaf_SpecificMinusFixedA$Site_number, 
                                                 Indices_Leaf_SpecificMinusFixedA$Exposure, sep = "_"),
                                           levels = c(
                                             "1S_Exposed", "2S_Exposed", "3S_Exposed", "4S_Exposed", "5S_Exposed", "1D_Exposed", "2D_Exposed", "3D_Exposed", "4D_Exposed",
                                             "1S_semi", "2S_semi", "3S_semi", "4S_semi", "5S_semi", "1D_semi", "2D_semi", "3D_semi", "4D_semi", "5D_semi",
                                             "1S_Pojo", "2S_Pojo", "3S_Pojo", "4S_Pojo", "5S_Pojo", "2D_Pojo", "3D_Pojo",
                                             "1S_Sheltered", "2S_Sheltered", "3S_Sheltered", "4S_Sheltered", "5S_Sheltered"
                                           ))
