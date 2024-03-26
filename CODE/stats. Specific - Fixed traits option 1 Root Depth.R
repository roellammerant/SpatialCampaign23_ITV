##%######################################################%##
#                                                          #
####            Community indices RootDepth             ####
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

Sites_exp_RootDepth <- aggregate(x=TraitData_expA$RootDepth, by=list(TraitData_expA$Depth, TraitData_expA$Species), FUN=max)
names(Sites_exp_RootDepth)[1:3]<-paste(c("Depth","Species", "RootDepth"))
Sites_exp_RootDepth$RootDepth <- Sites_exp_RootDepth$RootDepth - as.numeric(c("33.8","26.9", 
                                                                              "26.9","24.6", "21.8", "21.8", 
                                                                              "40.3", "40.3", "21.1","21.1"))

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

Indices_RootDepth_expsites <- rbind(CWMexp_RootDepth_S,CWMexp_RootDepth_D)
colnames(Indices_RootDepth_expsites) <- c("CWM_RootDepth")
Indices_RootDepth_expsites$Depth <- c(rep("Shallow",31),rep("Deep",26))
Indices_RootDepth_expsites$Exposure <- c(rep("Exposed",57))

###### Functional Dispersion
FDisexp_RootDepth_D <-as.data.frame(Indices_exp_RootDepth_D$FDis)
colnames(FDisexp_RootDepth_D) <- c("FDis")
FDisexp_RootDepth_S <-as.data.frame(Indices_exp_RootDepth_S$FDis)
colnames(FDisexp_RootDepth_S) <- c("FDis")

Indices_RootDepth_expsites$FDis_RootDepth <- rbind(FDisexp_RootDepth_S,FDisexp_RootDepth_D)
Indices_RootDepth_expsites <- tibble::rownames_to_column(Indices_RootDepth_expsites,"Site")
rownames(Indices_RootDepth_expsites)<-c(1:57)


##########################################
###########          #####################
########### SEMI    ######################
###########         ######################
##########################################

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_semi_RootDepth <- aggregate(x=TraitData_semiA$RootDepth, by=list(TraitData_semiA$Depth, TraitData_semiA$Species), FUN=max)
names(Sites_semi_RootDepth)[1:3]<-paste(c("Depth","Species", "RootDepth"))
Sites_semi_RootDepth$RootDepth <- Sites_semi_RootDepth$RootDepth - 
                                              as.numeric(c("0.0","0.0", "33.8","33.8", "26.9", 
                                               "26.9", "16.5", "31.5", "24.6","24.6",
                                               "21.8", "21.8","40.3", "40.3", "21.1"))

Sites_semi_RootDepthD <- subset(Sites_semi_RootDepth, Depth=="D")
Sites_semi_RootDepthS <- subset(Sites_semi_RootDepth, Depth=="S")

Sites_semi_RootDepthDB <- as.data.frame(Sites_semi_RootDepthD[,c(3)])
rownames(Sites_semi_RootDepthDB) <- Sites_semi_RootDepthD$Species
names(Sites_semi_RootDepthDB)[1]<-paste(c("RootDepth"))

Sites_semi_RootDepthSB <- as.data.frame(Sites_semi_RootDepthS[,c(3)])
rownames(Sites_semi_RootDepthSB) <- Sites_semi_RootDepthS$Species
names(Sites_semi_RootDepthSB)[1]<-paste(c("RootDepth"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_semi_RootDepth_D <- FD::dbFD(Sites_semi_RootDepthDB,Plot_Abun_semi_deep, w.abun = TRUE)
Indices_semi_RootDepth_S <- FD::dbFD(Sites_semi_RootDepthSB,Plot_Abun_semi_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMsemi_RootDepth_D <-as.data.frame(Indices_semi_RootDepth_D$CWM)
CWMsemi_RootDepth_S <-as.data.frame(Indices_semi_RootDepth_S$CWM)

Indices_RootDepth_semisites <- rbind(CWMsemi_RootDepth_S,CWMsemi_RootDepth_D)
colnames(Indices_RootDepth_semisites) <- c("CWM_RootDepth")
Indices_RootDepth_semisites$Depth <- c(rep("Shallow",30),rep("Deep",30))
Indices_RootDepth_semisites$Exposure <- c(rep("semi",30))

###### Functional Dispersion
FDissemi_RootDepth_D <-as.data.frame(Indices_semi_RootDepth_D$FDis)
colnames(FDissemi_RootDepth_D) <- c("FDis")
FDissemi_RootDepth_S <-as.data.frame(Indices_semi_RootDepth_S$FDis)
colnames(FDissemi_RootDepth_S) <- c("FDis")

Indices_RootDepth_semisites$FDis_RootDepth <- rbind(FDissemi_RootDepth_S,FDissemi_RootDepth_D)

Indices_RootDepth_semisites <- tibble::rownames_to_column(Indices_RootDepth_semisites,"Site")
rownames(Indices_RootDepth_semisites)<-c(1:60)

##############################################
###########              #####################
###########  Sheltered   #####################
###########              #####################

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_shel_RootDepth <- aggregate(x=TraitData_shelA$RootDepth, by=list(TraitData_shelA$Depth, TraitData_shelA$Species), FUN=max)
names(Sites_shel_RootDepth)[1:3]<-paste(c("Depth","Species", "RootDepth"))
Sites_shel_RootDepth$RootDepth <- Sites_shel_RootDepth$RootDepth - 
                                               as.numeric(c("6.7","0.0", "0.0","47.4", "33.8", 
                                               "14.6", "26.9", "31.5", "21.8"))

Sites_shel_RootDepthB <- as.data.frame(Sites_shel_RootDepth[,c(3)])
rownames(Sites_shel_RootDepthB) <- Sites_shel_RootDepth$Species
names(Sites_shel_RootDepthB)[1]<-paste(c("RootDepth"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_shel_RootDepth <- FD::dbFD(Sites_shel_RootDepthB,Plot_Abun_shel, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMshel_RootDepth <-as.data.frame(Indices_shel_RootDepth$CWM)

Indices_RootDepth_shelsites <- CWMshel_RootDepth
colnames(Indices_RootDepth_shelsites) <- c("CWM_RootDepth")
Indices_RootDepth_shelsites$Depth <- c(rep("Shallow",30))
Indices_RootDepth_shelsites$Exposure <- c(rep("Sheltered",30))

###### Functional Dispersion
FDisshel_RootDepth <-as.data.frame(Indices_shel_RootDepth$FDis)
colnames(FDisshel_RootDepth) <- c("FDis")

Indices_RootDepth_shelsites$FDis_RootDepth <- FDisshel_RootDepth

Indices_RootDepth_shelsites <- tibble::rownames_to_column(Indices_RootDepth_shelsites,"Site")
rownames(Indices_RootDepth_shelsites)<-c(1:30)

##########################################
###########          #####################
########### POJO    ######################
###########         ######################
##########################################

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_pojo_RootDepth <- aggregate(x=TraitData_pojoA$RootDepth, by=list(TraitData_pojoA$Depth, TraitData_pojoA$Species), FUN=max)
names(Sites_pojo_RootDepth)[1:3]<-paste(c("Depth","Species", "RootDepth"))
Sites_pojo_RootDepth$RootDepth <- Sites_pojo_RootDepth$RootDepth -
                                                as.numeric(c("6.7","0.0", "0.0","0.0", "0.0", 
                                               "0.0", "47.4", "33.8", "14.6","14.6",
                                               "20.4", "13.0","13.0", "14.7", "31.5", 
                                               "31.5", "9.1"))

Sites_pojo_RootDepth_D <- subset(Sites_pojo_RootDepth, Depth=="D")
Sites_pojo_RootDepth_S <- subset(Sites_pojo_RootDepth, Depth=="S")

Sites_pojo_RootDepth_DB <- as.data.frame(Sites_pojo_RootDepth_D[,c(3)])
rownames(Sites_pojo_RootDepth_DB) <- Sites_pojo_RootDepth_D$Species
names(Sites_pojo_RootDepth_DB)[1]<-paste(c("RootDepth"))

Sites_pojo_RootDepth_SB <- as.data.frame(Sites_pojo_RootDepth_S[,c(3)])
rownames(Sites_pojo_RootDepth_SB) <- Sites_pojo_RootDepth_S$Species
names(Sites_pojo_RootDepth_SB)[1]<-paste(c("RootDepth"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_pojo_RootDepth_D <- FD::dbFD(Sites_pojo_RootDepth_DB,Plot_Abun_pojo_deep, w.abun = TRUE)
Indices_pojo_RootDepth_S <- FD::dbFD(Sites_pojo_RootDepth_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMpojo_RootDepth_D <-as.data.frame(Indices_pojo_RootDepth_D$CWM)
CWMpojo_RootDepth_S <-as.data.frame(Indices_pojo_RootDepth_S$CWM)

Indices_RootDepth_pojosites <- rbind(CWMpojo_RootDepth_D,CWMpojo_RootDepth_S)
colnames(Indices_RootDepth_pojosites) <- c("CWM_RootDepth")
Indices_RootDepth_pojosites$Depth <- c(rep("Deep",8),rep("Shallow",30))
Indices_RootDepth_pojosites$Exposure <- c(rep("Pojo",38))

###### Functional Dispersion
FDispojo_RootDepth_D <-as.data.frame(Indices_pojo_RootDepth_D$FDis)
colnames(FDispojo_RootDepth_D) <- c("FDis")
FDispojo_RootDepth_S <-as.data.frame(Indices_pojo_RootDepth_S$FDis)
colnames(FDispojo_RootDepth_S) <- c("FDis")

Indices_RootDepth_pojosites$FDis_RootDepth <- rbind(FDispojo_RootDepth_D,FDispojo_RootDepth_S)

Indices_RootDepth_pojosites <- tibble::rownames_to_column(Indices_RootDepth_pojosites,"Site")
rownames(Indices_RootDepth_pojosites)<-c(1:38)


##################################################
###########                 ######################
###########    Combined     ######################
###########                 ######################
##################################################

Indices_RootDepth_expsites <- as.matrix.data.frame(Indices_RootDepth_expsites)
Indices_RootDepth_pojosites <- as.matrix.data.frame(Indices_RootDepth_pojosites)
Indices_RootDepth_semisites <- as.matrix.data.frame(Indices_RootDepth_semisites)
Indices_RootDepth_shelsites <- as.matrix.data.frame(Indices_RootDepth_shelsites)
Indices_RootDepth_expsites <- as.data.frame(Indices_RootDepth_expsites)
Indices_RootDepth_pojosites <- as.data.frame(Indices_RootDepth_pojosites)
Indices_RootDepth_semisites <- as.data.frame(Indices_RootDepth_semisites)
Indices_RootDepth_shelsites <- as.data.frame(Indices_RootDepth_shelsites)

Indices_RootDepth_SpecificMinusFixedA<- rbind.data.frame(Indices_RootDepth_expsites,Indices_RootDepth_semisites,Indices_RootDepth_pojosites,Indices_RootDepth_shelsites)

Indices_RootDepth_SpecificMinusFixedA$Site_number<- c(rep("1S",8),rep("2S",5),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",8),rep("2D",6),rep("3D",6),rep("4D",6),
                                     rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",6),rep("2D",6),rep("3D",6),rep("4D",6),rep("5D",6),
                                     rep("2D",2),rep("3D",6),rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),
                                     rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6))

Indices_RootDepth_SpecificMinusFixedA$Exposure_Depth <- factor(paste(Indices_RootDepth_SpecificMinusFixedA$Exposure, 
                                                                     Indices_RootDepth_SpecificMinusFixedA$Depth, sep = "_"),
                                                 levels = c(
                                                   "Exposed_Shallow", "Exposed_Deep",
                                                   "semi_Shallow", "semi_Deep",
                                                   "Pojo_Shallow", "Pojo_Deep",
                                                   "Sheltered_Shallow"
                                                 ))

Indices_RootDepth_SpecificMinusFixedA$Site_Exposure <- factor(paste(Indices_RootDepth_SpecificMinusFixedA$Site_number, 
                                                      Indices_RootDepth_SpecificMinusFixedA$Exposure, sep = "_"),
                                                levels = c(
                                                  "1S_Exposed", "2S_Exposed", "3S_Exposed", "4S_Exposed", "5S_Exposed", "1D_Exposed", "2D_Exposed", "3D_Exposed", "4D_Exposed",
                                                  "1S_semi", "2S_semi", "3S_semi", "4S_semi", "5S_semi", "1D_semi", "2D_semi", "3D_semi", "4D_semi", "5D_semi",
                                                  "1S_Pojo", "2S_Pojo", "3S_Pojo", "4S_Pojo", "5S_Pojo", "2D_Pojo", "3D_Pojo",
                                                  "1S_Sheltered", "2S_Sheltered", "3S_Sheltered", "4S_Sheltered", "5S_Sheltered"
                                                ))

