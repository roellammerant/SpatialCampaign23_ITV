##%######################################################%##
#                                                          #
####            Community indices R-S ratio             ####
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

Sites_exp_Ratio <- aggregate(x=TraitData_expA$Ratio, by=list(TraitData_expA$Depth, TraitData_expA$Species), FUN=median)
names(Sites_exp_Ratio)[1:3]<-paste(c("Depth","Species", "Ratio"))
### change for fixed R-S ratio
Sites_exp_Ratio$Ratio <- Sites_exp_Ratio$Ratio - as.numeric(c("0.30","0.46", "0.46","0.75", "0.26", "0.26", "0.83", "0.83", "0.55","0.55"))

Sites_exp_Ratio_D <- subset(Sites_exp_Ratio, Depth=="D")
Sites_exp_Ratio_S <- subset(Sites_exp_Ratio, Depth=="S")

Sites_exp_Ratio_DB <- as.data.frame(Sites_exp_Ratio_D[,c(3)])
rownames(Sites_exp_Ratio_DB) <- Sites_exp_Ratio_D$Species
names(Sites_exp_Ratio_DB)[1]<-paste(c("Ratio"))

Sites_exp_Ratio_SB <- as.data.frame(Sites_exp_Ratio_S[,c(3)])
rownames(Sites_exp_Ratio_SB) <- Sites_exp_Ratio_S$Species
names(Sites_exp_Ratio_SB)[1]<-paste(c("Ratio"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_exp_Ratio_D <- FD::dbFD(Sites_exp_Ratio_DB,Plot_Abun_exp_deep, w.abun = TRUE)
Indices_exp_Ratio_S <- FD::dbFD(Sites_exp_Ratio_SB,Plot_Abun_exp_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMexp_Ratio_D <-as.data.frame(Indices_exp_Ratio_D$CWM)
CWMexp_Ratio_S <-as.data.frame(Indices_exp_Ratio_S$CWM)

Indices_Ratio_expsites <- rbind(CWMexp_Ratio_S,CWMexp_Ratio_D)
colnames(Indices_Ratio_expsites) <- c("CWM_Ratio")
Indices_Ratio_expsites$Depth <- c(rep("Shallow",31),rep("Deep",26))
Indices_Ratio_expsites$Exposure <- c(rep("Exposed",57))

###### Functional Dispersion
FDisexp_Ratio_D <-as.data.frame(Indices_exp_Ratio_D$FDis)
colnames(FDisexp_Ratio_D) <- c("FDis")
FDisexp_Ratio_S <-as.data.frame(Indices_exp_Ratio_S$FDis)
colnames(FDisexp_Ratio_S) <- c("FDis")

Indices_Ratio_expsites$FDis_Ratio <- rbind(FDisexp_Ratio_S, FDisexp_Ratio_D)
Indices_Ratio_expsites <- tibble::rownames_to_column(Indices_Ratio_expsites,"Site")
rownames(Indices_Ratio_expsites)<-c(1:57)


##########################################
###########          #####################
########### SEMI    ######################
###########         ######################
##########################################

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_semi_Ratio <- aggregate(x=TraitData_semiA$Ratio, by=list(TraitData_semiA$Depth, TraitData_semiA$Species), FUN=median)
names(Sites_semi_Ratio)[1:3]<-paste(c("Depth","Species", "Ratio"))
Sites_semi_Ratio$Ratio <- Sites_semi_Ratio$Ratio -  as.numeric(c("0.00","0.00", "0.30","0.30", "0.46", "0.46", "0.28", "0.09", "0.75","0.75",
                                       "0.26", "0.26","0.83","0.83","0.55"))

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

Indices_Ratio_semisites <- rbind(CWMsemi_Ratio_S,CWMsemi_Ratio_D)
colnames(Indices_Ratio_semisites) <- c("CWM_Ratio")
Indices_Ratio_semisites$Depth <- c(rep("Shallow",30),rep("Deep",30))
Indices_Ratio_semisites$Exposure <- c(rep("semi",60))

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

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_shel_Ratio <- aggregate(x=TraitData_shelA$Ratio, by=list(TraitData_shelA$Depth, TraitData_shelA$Species), FUN=median)
names(Sites_shel_Ratio)[1:3]<-paste(c("Depth","Species", "Ratio"))
Sites_shel_Ratio$Ratio <- Sites_shel_Ratio$Ratio - as.numeric(c("0.09","0.00", "0.00","0.16", "0.30", "0.00", "0.46", "0.09", "0.26"))

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

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_pojo_Ratio <- aggregate(x=TraitData_pojoA$Ratio, by=list(TraitData_pojoA$Depth, TraitData_pojoA$Species), FUN=median)
names(Sites_pojo_Ratio)[1:3]<-paste(c("Depth","Species", "Ratio"))
Sites_pojo_Ratio$Ratio <- Sites_pojo_Ratio$Ratio - as.numeric(c("0.09","0.00", "0.00","0.00", "0.00", "0.00", "0.16", "0.30", "0.00","0.00",
                                       "1.13","0.13", "0.13","0.07", "0.09", "0.09", "0.47"))


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

Indices_Ratio_SpecificMinusFixedA<- rbind.data.frame(Indices_Ratio_expsites,Indices_Ratio_semisites,Indices_Ratio_pojosites,Indices_Ratio_shelsites)

Indices_Ratio_SpecificMinusFixedA$Site_number<- c(rep("1S",8),rep("2S",5),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",8),rep("2D",6),rep("3D",6),rep("4D",6),
                                    rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",6),rep("2D",6),rep("3D",6),rep("4D",6),rep("5D",6),
                                    rep("2D",2),rep("3D",6),rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),
                                    rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6))

Indices_Ratio_SpecificMinusFixedA$Exposure_Depth <- factor(paste(Indices_Ratio_SpecificMinusFixedA$Exposure, 
                                                                 Indices_Ratio_SpecificMinusFixedA$Depth, sep = "_"),
                                             levels = c(
                                               "Exposed_Shallow", "Exposed_Deep",
                                               "semi_Shallow", "semi_Deep",
                                               "Pojo_Shallow", "Pojo_Deep",
                                               "Sheltered_Shallow"
                                             ))

Indices_Ratio_SpecificMinusFixedA$Site_Exposure <- factor(paste(Indices_Ratio_SpecificMinusFixedA$Site_number, 
                                                  Indices_Ratio_SpecificMinusFixedA$Exposure, sep = "_"),
                                            levels = c(
                                              "1S_Exposed", "2S_Exposed", "3S_Exposed", "4S_Exposed", "5S_Exposed", "1D_Exposed", "2D_Exposed", "3D_Exposed", "4D_Exposed",
                                              "1S_semi", "2S_semi", "3S_semi", "4S_semi", "5S_semi", "1D_semi", "2D_semi", "3D_semi", "4D_semi", "5D_semi",
                                              "1S_Pojo", "2S_Pojo", "3S_Pojo", "4S_Pojo", "5S_Pojo", "2D_Pojo", "3D_Pojo",
                                              "1S_Sheltered", "2S_Sheltered", "3S_Sheltered", "4S_Sheltered", "5S_Sheltered"
                                            ))

