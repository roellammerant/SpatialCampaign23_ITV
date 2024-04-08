##%######################################################%##
#                                                          #
####            Community indices PCA                   ####
#                                                          #
##%######################################################%##

##########################################
###########          #####################
########### EXPOSED ######################
###########         ######################
##########################################

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
Plot_Abun_exp4_deep <- Plot_Abun_exp4_deep[,c(4,6)]

Plot_Abun_exp5_shallow <- Plot_Abun_exp[c(52:57),]
Plot_Abun_exp5_shallow <- Plot_Abun_exp5_shallow[,c(1,3:5)]


##%##########################################
#########    Trait matrix           ########
##%#########################################

Sites_exp_PC1 <- aggregate(x=TraitData_expP$PC1, by=list(TraitData_expP$Site_number, TraitData_expP$Depth, TraitData_expP$Species), FUN=median)
Sites_exp_PC2 <- aggregate(x=TraitData_expP$PC2, by=list(TraitData_expP$Site_number, TraitData_expP$Depth, TraitData_expP$Species), FUN=median)
names(Sites_exp_PC1)[1:4]<-paste(c("Site","Depth","Species", "PC1"))
names(Sites_exp_PC2)[1:4]<-paste(c("Site","Depth","Species", "PC2"))

############ PC1  ############ 
Sites1_exp_PC1D <- subset(Sites_exp_PC1, Site==1 & Depth=="D")
Sites1_exp_PC1DB <- as.data.frame(Sites1_exp_PC1D[,c(4)])
rownames(Sites1_exp_PC1DB) <- Sites1_exp_PC1D$Species
names(Sites1_exp_PC1DB)[1]<-paste(c("PC1"))
Sites1_exp_PC1DB$PC1 <- Sites1_exp_PC1DB$PC1 - as.numeric(c("0.19","0.09", "0.93"))

Sites1_exp_PC1S <- subset(Sites_exp_PC1, Site==1 & Depth=="S")
Sites1_exp_PC1SB <- as.data.frame(Sites1_exp_PC1S[,c(4)])
rownames(Sites1_exp_PC1SB) <- Sites1_exp_PC1S$Species
names(Sites1_exp_PC1SB)[1]<-paste(c("PC1"))
Sites1_exp_PC1SB$PC1 <- Sites1_exp_PC1SB$PC1 - as.numeric(c("1.12","0.19", "0.09", "0.93"))

Sites2_exp_PC1D <- subset(Sites_exp_PC1, Site==2 & Depth=="D")
Sites2_exp_PC1DB <- as.data.frame(Sites2_exp_PC1D[,c(4)])
rownames(Sites2_exp_PC1DB) <- Sites2_exp_PC1D$Species
names(Sites2_exp_PC1DB)[1]<-paste(c("PC1"))
Sites2_exp_PC1DB$PC1 <- Sites2_exp_PC1DB$PC1 - as.numeric(c("0.40"))

Sites2_exp_PC1S <- subset(Sites_exp_PC1, Site==2 & Depth=="S")
Sites2_exp_PC1SB <- as.data.frame(Sites2_exp_PC1S[,c(4)])
rownames(Sites2_exp_PC1SB) <- Sites2_exp_PC1S$Species
names(Sites2_exp_PC1SB)[1]<-paste(c("PC1"))
Sites2_exp_PC1SB$PC1 <- Sites2_exp_PC1SB$PC1 - as.numeric(c("0.33","0.09", "0.93", "0.40"))

Sites3_exp_PC1D <- subset(Sites_exp_PC1, Site==3 & Depth=="D")
Sites3_exp_PC1DB <- as.data.frame(Sites3_exp_PC1D[,c(4)])
rownames(Sites3_exp_PC1DB) <- Sites3_exp_PC1D$Species
names(Sites3_exp_PC1DB)[1]<-paste(c("PC1"))
Sites3_exp_PC1DB$PC1 <- Sites3_exp_PC1DB$PC1 - as.numeric(c("0.19", "0.09", "0.93", "0.40"))

Sites3_exp_PC1S <- subset(Sites_exp_PC1, Site==3 & Depth=="S")
Sites3_exp_PC1SB <- as.data.frame(Sites3_exp_PC1S[,c(4)])
rownames(Sites3_exp_PC1SB) <- Sites3_exp_PC1S$Species
names(Sites3_exp_PC1SB)[1]<-paste(c("PC1"))
Sites3_exp_PC1SB$PC1 <-Sites3_exp_PC1SB$PC1 - as.numeric(c("0.19","0.33","0.09", "0.93"))

Sites4_exp_PC1D <- subset(Sites_exp_PC1, Site==4 & Depth=="D")
Sites4_exp_PC1DB <- as.data.frame(Sites4_exp_PC1D[,c(4)])
rownames(Sites4_exp_PC1DB) <- Sites4_exp_PC1D$Species
names(Sites4_exp_PC1DB)[1]<-paste(c("PC1"))
Sites4_exp_PC1DB$PC1 <- Sites4_exp_PC1DB$PC1 - as.numeric(c("0.09", "0.40"))

Sites4_exp_PC1S <- subset(Sites_exp_PC1, Site==4 & Depth=="S")
Sites4_exp_PC1SB <- as.data.frame(Sites4_exp_PC1S[,c(4)])
rownames(Sites4_exp_PC1SB) <- Sites4_exp_PC1S$Species
names(Sites4_exp_PC1SB)[1]<-paste(c("PC1"))
Sites4_exp_PC1SB$PC1 <- Sites4_exp_PC1SB$PC1 - as.numeric(c("1.12","0.19","0.34","0.09", "0.93", 
                                                          "0.40"))

Sites5_exp_PC1S <- subset(Sites_exp_PC1, Site==5 & Depth=="S")
Sites5_exp_PC1SB <- as.data.frame(Sites5_exp_PC1S[,c(4)])
rownames(Sites5_exp_PC1SB) <- Sites5_exp_PC1S$Species
names(Sites5_exp_PC1SB)[1]<-paste(c("PC1"))
Sites5_exp_PC1SB$PC1 <- Sites5_exp_PC1SB$PC1 - as.numeric(c("1.12","0.34", "0.09", "0.93"))

############ PC2  ############ 
Sites1_exp_PC2D <- subset(Sites_exp_PC2, Site==1 & Depth=="D")
Sites1_exp_PC2DB <- as.data.frame(Sites1_exp_PC2D[,c(4)])
rownames(Sites1_exp_PC2DB) <- Sites1_exp_PC2D$Species
names(Sites1_exp_PC2DB)[1]<-paste(c("PC2"))
Sites1_exp_PC2DB$PC2 <- Sites1_exp_PC2DB$PC2 - as.numeric(c("-0.28","0.18", "-0.84"))

Sites1_exp_PC2S <- subset(Sites_exp_PC2, Site==1 & Depth=="S")
Sites1_exp_PC2SB <- as.data.frame(Sites1_exp_PC2S[,c(4)])
rownames(Sites1_exp_PC2SB) <- Sites1_exp_PC2S$Species
names(Sites1_exp_PC2SB)[1]<-paste(c("PC2"))
Sites1_exp_PC2SB$PC2 <- Sites1_exp_PC2SB$PC2 - as.numeric(c("0.57","-0.28","0.18", "-0.84"))

Sites2_exp_PC2D <- subset(Sites_exp_PC2, Site==2 & Depth=="D")
Sites2_exp_PC2DB <- as.data.frame(Sites2_exp_PC2D[,c(4)])
rownames(Sites2_exp_PC2DB) <- Sites2_exp_PC2D$Species
names(Sites2_exp_PC2DB)[1]<-paste(c("PC2"))
Sites2_exp_PC2DB$PC2 <- Sites2_exp_PC2DB$PC2 - as.numeric(c("0.11"))

Sites2_exp_PC2S <- subset(Sites_exp_PC2, Site==2 & Depth=="S")
Sites2_exp_PC2SB <- as.data.frame(Sites2_exp_PC2S[,c(4)])
rownames(Sites2_exp_PC2SB) <- Sites2_exp_PC2S$Species
names(Sites2_exp_PC2SB)[1]<-paste(c("PC2"))
Sites2_exp_PC2SB$PC2 <- Sites2_exp_PC2SB$PC2 - as.numeric(c("-0.59","0.18", "-0.84", "0.11"))

Sites3_exp_PC2D <- subset(Sites_exp_PC2, Site==3 & Depth=="D")
Sites3_exp_PC2DB <- as.data.frame(Sites3_exp_PC2D[,c(4)])
rownames(Sites3_exp_PC2DB) <- Sites3_exp_PC2D$Species
names(Sites3_exp_PC2DB)[1]<-paste(c("PC2"))
Sites3_exp_PC2DB$PC2 <- Sites3_exp_PC2DB$PC2 - as.numeric(c("-0.28","0.18", "-0.84", "0.11"))

Sites3_exp_PC2S <- subset(Sites_exp_PC2, Site==3 & Depth=="S")
Sites3_exp_PC2SB <- as.data.frame(Sites3_exp_PC2S[,c(4)])
rownames(Sites3_exp_PC2SB) <- Sites3_exp_PC2S$Species
names(Sites3_exp_PC2SB)[1]<-paste(c("PC2"))
Sites3_exp_PC2SB$PC2 <- Sites3_exp_PC2SB$PC2 - as.numeric(c("-0.28","-0.59", "0.18", "-0.84"))

Sites4_exp_PC2D <- subset(Sites_exp_PC2, Site==4 & Depth=="D")
Sites4_exp_PC2DB <- as.data.frame(Sites4_exp_PC2D[,c(4)])
rownames(Sites4_exp_PC2DB) <- Sites4_exp_PC2D$Species
names(Sites4_exp_PC2DB)[1]<-paste(c("PC2"))
Sites4_exp_PC2DB$PC2 <-Sites4_exp_PC2DB$PC2 - as.numeric(c("0.18","0.11"))

Sites4_exp_PC2S <- subset(Sites_exp_PC2, Site==4 & Depth=="S")
Sites4_exp_PC2SB <- as.data.frame(Sites4_exp_PC2S[,c(4)])
rownames(Sites4_exp_PC2SB) <- Sites4_exp_PC2S$Species
names(Sites4_exp_PC2SB)[1]<-paste(c("PC2"))
Sites4_exp_PC2SB$PC2 <- Sites4_exp_PC2SB$PC2 - as.numeric(c("0.57","-0.28", "-0.59", "0.18", "-0.84", "0.11"))

Sites5_exp_PC2S <- subset(Sites_exp_PC2, Site==5 & Depth=="S")
Sites5_exp_PC2SB <- as.data.frame(Sites5_exp_PC2S[,c(4)])
rownames(Sites5_exp_PC2SB) <- Sites5_exp_PC2S$Species
names(Sites5_exp_PC2SB)[1]<-paste(c("PC2"))
Sites5_exp_PC2SB$PC2 <- Sites5_exp_PC2SB$PC2 - as.numeric(c("0.57","-0.59", "0.18", "-0.84"))

########################################################
### Community weighted mean value for leaf traits ######
########################################################

############ PC1  ############ 
Indices_exp1_PC1D <- FD::dbFD(Sites1_exp_PC1DB,Plot_Abun_exp1_deep, w.abun = TRUE)
Indices_exp1_PC1S <- FD::dbFD(Sites1_exp_PC1SB,Plot_Abun_exp1_shallow, w.abun = TRUE)

Indices_exp2_PC1S <- FD::dbFD(Sites2_exp_PC1SB,Plot_Abun_exp2_shallow, w.abun = TRUE)
Indices_exp2_PC1D <- Indices_exp1_PC1S
Indices_exp2_PC1D$FDis <- c(rep("0",8))
Indices_exp2_PC1D$CWM <- c(rep("0.03609",8))

Indices_exp3_PC1D <- FD::dbFD(Sites3_exp_PC1DB,Plot_Abun_exp3_deep, w.abun = TRUE)
Indices_exp3_PC1S <- FD::dbFD(Sites3_exp_PC1SB,Plot_Abun_exp3_shallow, w.abun = TRUE)

Indices_exp4_PC1D <- FD::dbFD(Sites4_exp_PC1DB,Plot_Abun_exp4_deep, w.abun = TRUE)
Indices_exp4_PC1S <- FD::dbFD(Sites4_exp_PC1SB,Plot_Abun_exp4_shallow, w.abun = TRUE)

Indices_exp5_PC1S <- FD::dbFD(Sites5_exp_PC1SB,Plot_Abun_exp5_shallow, w.abun = TRUE)

############ PC2  ############ 
Indices_exp1_PC2D <- FD::dbFD(Sites1_exp_PC2DB,Plot_Abun_exp1_deep, w.abun = TRUE)
Indices_exp1_PC2S <- FD::dbFD(Sites1_exp_PC2SB,Plot_Abun_exp1_shallow, w.abun = TRUE)

Indices_exp2_PC2S <- FD::dbFD(Sites2_exp_PC2SB,Plot_Abun_exp2_shallow, w.abun = TRUE)
Indices_exp2_PC2D <- Indices_exp1_LAPS
Indices_exp2_PC2D$FDis <- c(rep("0",8))
Indices_exp2_PC2D$CWM <- c(rep("0.0344133",8))

Indices_exp3_PC2D <- FD::dbFD(Sites3_exp_PC2DB,Plot_Abun_exp3_deep, w.abun = TRUE)
Indices_exp3_PC2S <- FD::dbFD(Sites3_exp_PC2SB,Plot_Abun_exp3_shallow, w.abun = TRUE)

Indices_exp4_PC2D <- FD::dbFD(Sites4_exp_PC2DB,Plot_Abun_exp4_deep, w.abun = TRUE)
Indices_exp4_PC2S <- FD::dbFD(Sites4_exp_PC2SB,Plot_Abun_exp4_shallow, w.abun = TRUE)

Indices_exp5_PC2S <- FD::dbFD(Sites5_exp_PC2SB,Plot_Abun_exp5_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean

############ PC1  ############ 
CWMExp1PC1S <-as.data.frame(Indices_exp1_PC1S$CWM)
CWMExp1PC1D <-as.data.frame(Indices_exp1_PC1D$CWM)

CWMExp2PC1S <-as.data.frame(Indices_exp2_PC1S$CWM)
colnames(CWMExp2PC1S) <- c("PC1")
CWMExp2PC1D <-as.data.frame(Indices_exp2_PC1D$CWM)
CWMExp2PC1D <- as.data.frame(CWMExp2PC1D[c(1:6),]) 
colnames(CWMExp2PC1D) <- c("PC1")

CWMExp3PC1S <-as.data.frame(Indices_exp3_PC1S$CWM)
CWMExp3PC1D <-as.data.frame(Indices_exp3_PC1D$CWM)

CWMExp4PC1S <-as.data.frame(Indices_exp4_PC1S$CWM)
CWMExp4PC1D <-as.data.frame(Indices_exp4_PC1D$CWM)

CWMExp5PC1S <-as.data.frame(Indices_exp5_PC1S$CWM)

Indices_PCA_Expsites <-rbind.data.frame(CWMExp1PC1S,CWMExp2PC1S,CWMExp3PC1S,CWMExp4PC1S,CWMExp5PC1S,
                                        CWMExp1PC1D,CWMExp2PC1D,CWMExp3PC1D,CWMExp4PC1D)

colnames(Indices_PCA_Expsites) <- c("CWM_PC1")
Indices_PCA_Expsites$Depth <- c(rep("Shallow",31),rep("Deep",26))
Indices_PCA_Expsites$Exposure <- c(rep("Exposed",57))

############ PC2  ############ 
CWMExp1PC2S <-as.data.frame(Indices_exp1_PC2S$CWM)
CWMExp1PC2D <-as.data.frame(Indices_exp1_PC2D$CWM)

CWMExp2PC2S <-as.data.frame(Indices_exp2_PC2S$CWM)
colnames(CWMExp2PC2S) <- c("PC2")
CWMExp2PC2D <-as.data.frame(Indices_exp2_PC2D$CWM)
CWMExp2PC2D <- as.data.frame(CWMExp2PC2D[c(1:6),]) 
colnames(CWMExp2PC2D) <- c("PC2")

CWMExp3PC2S <-as.data.frame(Indices_exp3_PC2S$CWM)
CWMExp3PC2D <-as.data.frame(Indices_exp3_PC2D$CWM)

CWMExp4PC2S <-as.data.frame(Indices_exp4_PC2S$CWM)
CWMExp4PC2D <-as.data.frame(Indices_exp4_PC2D$CWM)

CWMExp5PC2S <-as.data.frame(Indices_exp5_PC2S$CWM)

Indices_PCA_Expsites$CWM_PC2 <-rbind.data.frame(CWMExp1PC2S,CWMExp2PC2S,CWMExp3PC2S,CWMExp4PC2S,CWMExp5PC2S,
                                                 CWMExp1PC2D,CWMExp2PC2D,CWMExp3PC2D,CWMExp4PC2D)
###### Functional Dispersion

############ PC1  ############ 
FDisExp1PC1S <-as.data.frame(Indices_exp1_PC1S$FDis)
colnames(FDisExp1PC1S) <- c("FDis")
FDisExp1PC1D <-as.data.frame(Indices_exp1_PC1D$FDis)
colnames(FDisExp1PC1D) <- c("FDis")

FDisExp2PC1S <-as.data.frame(Indices_exp2_PC1S$FDis)
colnames(FDisExp2PC1S) <- c("FDis")
FDisExp2PC1D <-as.data.frame(Indices_exp2_PC1D$FDis)
FDisExp2PC1D <- as.data.frame(FDisExp2PC1D[c(1:6),]) 
colnames(FDisExp2PC1D) <- c("FDis")

FDisExp3PC1S <-as.data.frame(Indices_exp3_PC1S$FDis)
colnames(FDisExp3PC1S) <- c("FDis")
FDisExp3PC1D <-as.data.frame(Indices_exp3_PC1D$FDis)
colnames(FDisExp3PC1D) <- c("FDis")

FDisExp4PC1S <-as.data.frame(Indices_exp4_PC1S$FDis)
colnames(FDisExp4PC1S) <- c("FDis")
FDisExp4PC1D <-as.data.frame(Indices_exp4_PC1D$FDis)
colnames(FDisExp4PC1D) <- c("FDis")

FDisExp5PC1S <-as.data.frame(Indices_exp5_PC1S$FDis)
colnames(FDisExp5PC1S) <- c("FDis")

Indices_PCA_Expsites$FDis_PC1 <-rbind.data.frame(FDisExp1PC1S,FDisExp2PC1S,FDisExp3PC1S,FDisExp4PC1S,FDisExp5PC1S,
                                                  FDisExp1PC1D,FDisExp2PC1D,FDisExp3PC1D,FDisExp4PC1D)
############ PC2  ############ 
FDisExp1PC2S <-as.data.frame(Indices_exp1_PC2S$FDis)
colnames(FDisExp1PC2S) <- c("FDis")
FDisExp1PC2D <-as.data.frame(Indices_exp1_PC2D$FDis)
colnames(FDisExp1PC2D) <- c("FDis")

FDisExp2PC2S <-as.data.frame(Indices_exp2_PC2S$FDis)
colnames(FDisExp2PC2S) <- c("FDis")
FDisExp2PC2D <-as.data.frame(Indices_exp2_PC2D$FDis)
FDisExp2PC2D <- as.data.frame(FDisExp2PC2D[c(1:6),]) 
colnames(FDisExp2PC2D) <- c("FDis")

FDisExp3PC2S <-as.data.frame(Indices_exp3_PC2S$FDis)
colnames(FDisExp3PC2S) <- c("FDis")
FDisExp3PC2D <-as.data.frame(Indices_exp3_PC2D$FDis)
colnames(FDisExp3PC2D) <- c("FDis")

FDisExp4PC2S <-as.data.frame(Indices_exp4_PC2S$FDis)
colnames(FDisExp4PC2S) <- c("FDis")
FDisExp4PC2D <-as.data.frame(Indices_exp4_PC2D$FDis)
colnames(FDisExp4PC2D) <- c("FDis")

FDisExp5PC2S <-as.data.frame(Indices_exp5_PC2S$FDis)
colnames(FDisExp5PC2S) <- c("FDis")

Indices_PCA_Expsites$FDis_PC2 <-rbind.data.frame(FDisExp1PC2S,FDisExp2PC2S,FDisExp3PC2S,FDisExp4PC2S,FDisExp5PC2S,
                                                  FDisExp1PC2D,FDisExp2PC2D,FDisExp3PC2D,FDisExp4PC2D)
Indices_PCA_Expsites <- tibble::rownames_to_column(Indices_PCA_Expsites,"Site")
rownames(Indices_PCA_Expsites)<-c(1:57)

##########################################
###########          #####################
###########   SEMI  ######################
###########         ######################
##########################################


############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass
Plot_Abun_semi <- with(AbundanceData_semi, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_semi[is.na(Plot_Abun_semi)] = 0

##### Abundance per site
Plot_Abun_semi1_deep <- Plot_Abun_semi[c(1:6),]
Plot_Abun_semi1_shallow <- Plot_Abun_semi[c(31:36),]
Plot_Abun_semi1_deep <- Plot_Abun_semi1_deep[,c(2,6,7,8)]
Plot_Abun_semi1_shallow <- Plot_Abun_semi1_shallow[,c(2,3,6,7,8)]

Plot_Abun_semi2_deep <- Plot_Abun_semi[c(7:12),]
Plot_Abun_semi2_shallow <- Plot_Abun_semi[c(37:42),]
Plot_Abun_semi2_deep <- Plot_Abun_semi2_deep[,c(2,3,7,8)]
Plot_Abun_semi2_shallow <- Plot_Abun_semi2_shallow[,c(3,6,7,8)]

Plot_Abun_semi3_deep <- Plot_Abun_semi[c(13:18),]
Plot_Abun_semi3_shallow <- Plot_Abun_semi[c(43:48),]
Plot_Abun_semi3_deep <- Plot_Abun_semi3_deep[,c(1,2)]
Plot_Abun_semi3_shallow <- Plot_Abun_semi3_shallow[,c(1,2,3,7)]

Plot_Abun_semi4_deep <- Plot_Abun_semi[c(19:24),]
Plot_Abun_semi4_shallow <- Plot_Abun_semi[c(49:54),]
Plot_Abun_semi4_deep <- Plot_Abun_semi4_deep[,c(1,2,6)]
Plot_Abun_semi4_shallow <- Plot_Abun_semi4_shallow[,c(1,2,3,5,7)]

Plot_Abun_semi5_deep <- Plot_Abun_semi[c(25:30),]
Plot_Abun_semi5_shallow <- Plot_Abun_semi[c(55:60),]
Plot_Abun_semi5_deep <- Plot_Abun_semi5_deep[,c(1,2,3,7,8,9)]
Plot_Abun_semi5_shallow <- Plot_Abun_semi5_shallow[,c(2,3,4,7,8)]


##%##########################################
#########    Trait matrix      ########
##%#########################################

Sites_semi_PC1 <- aggregate(x=TraitData_semiP$PC1, by=list(TraitData_semiP$Site_number,TraitData_semiP$Depth, TraitData_semiP$Species), FUN=median)
Sites_semi_PC2 <- aggregate(x=TraitData_semiP$PC2, by=list(TraitData_semiP$Site_number,TraitData_semiP$Depth, TraitData_semiP$Species), FUN=median)
names(Sites_semi_PC1)[1:4]<-paste(c("Site","Depth","Species", "PC1"))
names(Sites_semi_PC2)[1:4]<-paste(c("Site", "Depth","Species", "PC2"))

############ PC1  ############ 
Sites1_semi_PC1D<- subset(Sites_semi_PC1, Site==1 & Depth=="D")
Sites1_semi_PC1DB <- as.data.frame(Sites1_semi_PC1D[,c(4)])
rownames(Sites1_semi_PC1DB) <- Sites1_semi_PC1D$Species
names(Sites1_semi_PC1DB)[1]<-paste(c("PC1"))
Sites1_semi_PC1DB$PC1 <- Sites1_semi_PC1DB$PC1 - as.numeric(c("1.12","0.34", "0.09", "0.93"))

Sites1_semi_PC1S <- subset(Sites_semi_PC1, Site==1 & Depth=="S")
Sites1_semi_PC1SB <- as.data.frame(Sites1_semi_PC1S[,c(4)])
rownames(Sites1_semi_PC1SB) <- Sites1_semi_PC1S$Species
names(Sites1_semi_PC1SB)[1]<-paste(c("PC1"))
Sites1_semi_PC1SB$PC1 <- Sites1_semi_PC1SB$PC1 - as.numeric(c("1.12","0.19", "0.34", "0.09", "0.93"))

Sites2_semi_PC1D <- subset(Sites_semi_PC1, Site==2 & Depth=="D")
Sites2_semi_PC1DB <- as.data.frame(Sites2_semi_PC1D[,c(4)])
rownames(Sites2_semi_PC1DB) <- Sites2_semi_PC1D$Species
names(Sites2_semi_PC1DB)[1]<-paste(c("PC1"))
Sites2_semi_PC1DB$PC1 <- Sites2_semi_PC1DB$PC1 - as.numeric(c("1.12","0.19", "0.09", "0.93"))

Sites2_semi_PC1S <- subset(Sites_semi_PC1, Site==2 & Depth=="S")
Sites2_semi_PC1SB <- as.data.frame(Sites2_semi_PC1S[,c(4)])
rownames(Sites2_semi_PC1SB) <- Sites2_semi_PC1S$Species
names(Sites2_semi_PC1SB)[1]<-paste(c("PC1"))
Sites2_semi_PC1SB$PC1 <- Sites2_semi_PC1SB$PC1 - as.numeric(c("0.19", "0.34", "0.09", "0.93"))

Sites3_semi_PC1D <- subset(Sites_semi_PC1, Site==3 & Depth=="D")
Sites3_semi_PC1DB <- as.data.frame(Sites3_semi_PC1D[,c(4)])
rownames(Sites3_semi_PC1DB) <- Sites3_semi_PC1D$Species
names(Sites3_semi_PC1DB)[1]<-paste(c("PC1"))
Sites3_semi_PC1DB$PC1 <- Sites3_semi_PC1DB$PC1 - as.numeric(c("-1.29","1.12"))

Sites3_semi_PC1S <- subset(Sites_semi_PC1, Site==3 & Depth=="S")
Sites3_semi_PC1SB <- as.data.frame(Sites3_semi_PC1S[,c(4)])
rownames(Sites3_semi_PC1SB) <- Sites3_semi_PC1S$Species
names(Sites3_semi_PC1SB)[1]<-paste(c("PC1"))
Sites3_semi_PC1SB$PC1 <- Sites3_semi_PC1SB$PC1 - as.numeric(c("-1.29","1.12", "0.19", "0.09"))

Sites4_semi_PC1D <- subset(Sites_semi_PC1, Site==4 & Depth=="D")
Sites4_semi_PC1DB <- as.data.frame(Sites4_semi_PC1D[,c(4)])
rownames(Sites4_semi_PC1DB) <- Sites4_semi_PC1D$Species
names(Sites4_semi_PC1DB)[1]<-paste(c("PC1"))
Sites4_semi_PC1DB$PC1 <- Sites4_semi_PC1DB$PC1 - as.numeric(c("-1.29","1.12","0.34"))

Sites4_semi_PC1S <- subset(Sites_semi_PC1, Site==4 & Depth=="S")
Sites4_semi_PC1SB <- as.data.frame(Sites4_semi_PC1S[,c(4)])
rownames(Sites4_semi_PC1SB) <- Sites4_semi_PC1S$Species
names(Sites4_semi_PC1SB)[1]<-paste(c("PC1"))
Sites4_semi_PC1SB$PC1 <- Sites4_semi_PC1SB$PC1 - as.numeric(c("-1.29","1.12", "0.19", "-0.65", "0.09"))

Sites5_semi_PC1D <- subset(Sites_semi_PC1, Site==5 & Depth=="D")
Sites5_semi_PC1DB <- as.data.frame(Sites5_semi_PC1D[,c(4)])
rownames(Sites5_semi_PC1DB) <- Sites5_semi_PC1D$Species
names(Sites5_semi_PC1DB)[1]<-paste(c("PC1"))
Sites5_semi_PC1DB$PC1 <- Sites5_semi_PC1DB$PC1 - as.numeric(c("-1.29","1.12", "0.19","0.09","0.93","0.40"))

Sites5_semi_PC1S <- subset(Sites_semi_PC1, Site==5 & Depth=="S")
Sites5_semi_PC1SB <- as.data.frame(Sites5_semi_PC1S[,c(4)])
rownames(Sites5_semi_PC1SB) <- Sites5_semi_PC1S$Species
names(Sites5_semi_PC1SB)[1]<-paste(c("PC1"))
Sites5_semi_PC1SB$PC1 <- Sites5_semi_PC1SB$PC1 - as.numeric(c("1.12", "0.19", "2.625", "0.09","0.93"))

############ PC2  ############ 
Sites1_semi_PC2D <- subset(Sites_semi_PC2, Site==1 & Depth=="D")
Sites1_semi_PC2DB <- as.data.frame(Sites1_semi_PC2D[,c(4)])
rownames(Sites1_semi_PC2DB) <- Sites1_semi_PC2D$Species
names(Sites1_semi_PC2DB)[1]<-paste(c("PC2"))
Sites1_semi_PC2DB$PC2 <- Sites1_semi_PC2DB$PC2 - as.numeric(c("0.57","-0.28", "0.18", "-0.84"))

Sites1_semi_PC2S <- subset(Sites_semi_PC2, Site==1 & Depth=="S")
Sites1_semi_PC2SB <- as.data.frame(Sites1_semi_PC2S[,c(4)])
rownames(Sites1_semi_PC2SB) <- Sites1_semi_PC2S$Species
names(Sites1_semi_PC2SB)[1]<-paste(c("PC2"))
Sites1_semi_PC2SB$PC2 <- Sites1_semi_PC2SB$PC2 - as.numeric(c("0.57","-0.28","-0.59", "0.18", "-0.84"))

Sites2_semi_PC2D <- subset(Sites_semi_PC2, Site==2 & Depth=="D")
Sites2_semi_PC2DB <- as.data.frame(Sites2_semi_PC2D[,c(4)])
rownames(Sites2_semi_PC2DB) <- Sites2_semi_PC2D$Species
names(Sites2_semi_PC2DB)[1]<-paste(c("PC2"))
Sites2_semi_PC2DB$PC2 <- Sites2_semi_PC2DB$PC2 - as.numeric(c("0.57","-0.28","0.18", "-0.84"))

Sites2_semi_PC2S <- subset(Sites_semi_PC2, Site==2 & Depth=="S")
Sites2_semi_PC2SB <- as.data.frame(Sites2_semi_PC2S[,c(4)])
rownames(Sites2_semi_PC2SB) <- Sites2_semi_PC2S$Species
names(Sites2_semi_PC2SB)[1]<-paste(c("PC2"))
Sites2_semi_PC2SB$PC2 <- Sites2_semi_PC2SB$PC2 - as.numeric(c("-0.28","-0.59", "0.18", "-0.84"))

Sites3_semi_PC2D <- subset(Sites_semi_PC2, Site==3 & Depth=="D")
Sites3_semi_PC2DB <- as.data.frame(Sites3_semi_PC2D[,c(4)])
rownames(Sites3_semi_PC2DB) <- Sites3_semi_PC2D$Species
names(Sites3_semi_PC2DB)[1]<-paste(c("PC2"))
Sites3_semi_PC2DB$PC2 <- Sites3_semi_PC2DB$PC2 - as.numeric(c("0.22","0.57"))

Sites3_semi_PC2S <- subset(Sites_semi_PC2, Site==3 & Depth=="S")
Sites3_semi_PC2SB <- as.data.frame(Sites3_semi_PC2S[,c(4)])
rownames(Sites3_semi_PC2SB) <- Sites3_semi_PC2S$Species
names(Sites3_semi_PC2SB)[1]<-paste(c("PC2"))
Sites3_semi_PC2SB$PC2  <- Sites3_semi_PC2SB$PC2  - as.numeric(c("0.22","0.57", "-0.28", "0.18"))

Sites4_semi_PC2D <- subset(Sites_semi_PC2, Site==4 & Depth=="D")
Sites4_semi_PC2DB <- as.data.frame(Sites4_semi_PC2D[,c(4)])
rownames(Sites4_semi_PC2DB) <- Sites4_semi_PC2D$Species
names(Sites4_semi_PC2DB)[1]<-paste(c("PC2"))
Sites4_semi_PC2DB$PC2  <- Sites4_semi_PC2DB$PC2  - as.numeric(c("0.22","0.57", "-0.59"))

Sites4_semi_PC2S <- subset(Sites_semi_PC2, Site==4 & Depth=="S")
Sites4_semi_PC2SB <- as.data.frame(Sites4_semi_PC2S[,c(4)])
rownames(Sites4_semi_PC2SB) <- Sites4_semi_PC2S$Species
names(Sites4_semi_PC2SB)[1]<-paste(c("PC2"))
Sites4_semi_PC2SB$PC2  <- Sites4_semi_PC2SB$PC2  - as.numeric(c("0.22","0.57", "-0.28", "0.06","0.18"))

Sites5_semi_PC2D <- subset(Sites_semi_PC2, Site==5 & Depth=="D")
Sites5_semi_PC2DB <- as.data.frame(Sites5_semi_PC2D[,c(4)])
rownames(Sites5_semi_PC2DB) <- Sites5_semi_PC2D$Species
names(Sites5_semi_PC2DB)[1]<-paste(c("PC2"))
Sites5_semi_PC2DB$PC2  <- Sites5_semi_PC2DB$PC2  - as.numeric(c("0.22","0.57", "-0.28", "0.18", "-0.84", "0.11"))

Sites5_semi_PC2S <- subset(Sites_semi_PC2, Site==5 & Depth=="S")
Sites5_semi_PC2SB <- as.data.frame(Sites5_semi_PC2S[,c(4)])
rownames(Sites5_semi_PC2SB) <- Sites5_semi_PC2S$Species
names(Sites5_semi_PC2SB)[1]<-paste(c("PC2"))
Sites5_semi_PC2SB$PC2 <- Sites5_semi_PC2SB$PC2  - as.numeric(c("0.57", "-0.28", "2.809", "0.18", "-0.84"))

########################################################
### Community weighted mean value for leaf traits ######
########################################################

############ PC1  ############ 
Indices_semi1_PC1D <- FD::dbFD(Sites1_semi_PC1DB,Plot_Abun_semi1_deep, w.abun = TRUE)
Indices_semi1_PC1S <- FD::dbFD(Sites1_semi_PC1SB,Plot_Abun_semi1_shallow, w.abun = TRUE)

Indices_semi2_PC1D <- FD::dbFD(Sites2_semi_PC1DB,Plot_Abun_semi2_deep, w.abun = TRUE)
Indices_semi2_PC1S <- FD::dbFD(Sites2_semi_PC1SB,Plot_Abun_semi2_shallow, w.abun = TRUE)

Indices_semi3_PC1D <- FD::dbFD(Sites3_semi_PC1DB,Plot_Abun_semi3_deep, w.abun = TRUE)
Indices_semi3_PC1S <- FD::dbFD(Sites3_semi_PC1SB,Plot_Abun_semi3_shallow, w.abun = TRUE)

Indices_semi4_PC1D <- FD::dbFD(Sites4_semi_PC1DB,Plot_Abun_semi4_deep, w.abun = TRUE)
Indices_semi4_PC1S <- FD::dbFD(Sites4_semi_PC1SB,Plot_Abun_semi4_shallow, w.abun = TRUE)

Indices_semi5_PC1D <- FD::dbFD(Sites5_semi_PC1DB,Plot_Abun_semi5_deep, w.abun = TRUE)
Indices_semi5_PC1S <- FD::dbFD(Sites5_semi_PC1SB,Plot_Abun_semi5_shallow, w.abun = TRUE)

############ PC2 ############ 

Indices_semi1_PC2D <- FD::dbFD(Sites1_semi_PC2DB,Plot_Abun_semi1_deep, w.abun = TRUE)
Indices_semi1_PC2S <- FD::dbFD(Sites1_semi_PC2SB,Plot_Abun_semi1_shallow, w.abun = TRUE)

Indices_semi2_PC2D <- FD::dbFD(Sites2_semi_PC2DB,Plot_Abun_semi2_deep, w.abun = TRUE)
Indices_semi2_PC2S <- FD::dbFD(Sites2_semi_PC2SB,Plot_Abun_semi2_shallow, w.abun = TRUE)

Indices_semi3_PC2D <- FD::dbFD(Sites3_semi_PC2DB,Plot_Abun_semi3_deep, w.abun = TRUE)
Indices_semi3_PC2S <- FD::dbFD(Sites3_semi_PC2SB,Plot_Abun_semi3_shallow, w.abun = TRUE)

Indices_semi4_PC2D <- FD::dbFD(Sites4_semi_PC2DB,Plot_Abun_semi4_deep, w.abun = TRUE)
Indices_semi4_PC2S <- FD::dbFD(Sites4_semi_PC2SB,Plot_Abun_semi4_shallow, w.abun = TRUE)

Indices_semi5_PC2D <- FD::dbFD(Sites5_semi_PC2DB,Plot_Abun_semi5_deep, w.abun = TRUE)
Indices_semi5_PC2S <- FD::dbFD(Sites5_semi_PC2SB,Plot_Abun_semi5_shallow, w.abun = TRUE)


########################################################
###           Combine semi site indices           ######
########################################################

###### Community weighted mean

############ PC1  ############
CWMSemi1PC1S <-as.data.frame(Indices_semi1_PC1S$CWM)
CWMSemi1PC1D <-as.data.frame(Indices_semi1_PC1D$CWM)

CWMSemi2PC1S <-as.data.frame(Indices_semi2_PC1S$CWM)
CWMSemi2PC1D <-as.data.frame(Indices_semi2_PC1D$CWM)

CWMSemi3PC1S <-as.data.frame(Indices_semi3_PC1S$CWM)
CWMSemi3PC1D <-as.data.frame(Indices_semi3_PC1D$CWM)

CWMSemi4PC1S <-as.data.frame(Indices_semi4_PC1S$CWM)
CWMSemi4PC1D <-as.data.frame(Indices_semi4_PC1D$CWM)

CWMSemi5PC1S <-as.data.frame(Indices_semi5_PC1S$CWM)
CWMSemi5PC1D <-as.data.frame(Indices_semi5_PC1D$CWM)

Indices_PCA_Semisites <-rbind.data.frame(CWMSemi1PC1S,CWMSemi2PC1S,CWMSemi3PC1S,CWMSemi4PC1S,CWMSemi5PC1S,
                                          CWMSemi1PC1D,CWMSemi2PC1D,CWMSemi3PC1D,CWMSemi4PC1D,CWMSemi5PC1D)
colnames(Indices_PCA_Semisites) <- c("CWM_PC1")
Indices_PCA_Semisites$Depth <- c(rep("Shallow",30),rep("Deep",30))
Indices_PCA_Semisites$Exposure <- c(rep("Semi",60))

############ PC2  ############

CWMSemi1PC2S <-as.data.frame(Indices_semi1_PC2S$CWM)
CWMSemi1PC2D <-as.data.frame(Indices_semi1_PC2D$CWM)

CWMSemi2PC2S <-as.data.frame(Indices_semi2_PC2S$CWM)
CWMSemi2PC2D <-as.data.frame(Indices_semi2_PC2D$CWM)

CWMSemi3PC2S <-as.data.frame(Indices_semi3_PC2S$CWM)
CWMSemi3PC2D <-as.data.frame(Indices_semi3_PC2D$CWM)

CWMSemi4PC2S <-as.data.frame(Indices_semi4_PC2S$CWM)
CWMSemi4PC2D <-as.data.frame(Indices_semi4_PC2D$CWM)

CWMSemi5PC2S <-as.data.frame(Indices_semi5_PC2S$CWM)
CWMSemi5PC2D <-as.data.frame(Indices_semi5_PC2D$CWM)

Indices_PCA_Semisites$CWM_PC2 <-rbind.data.frame(CWMSemi1PC2S,CWMSemi2PC2S,CWMSemi3PC2S,CWMSemi4PC2S,CWMSemi5PC2S,
                                                  CWMSemi1PC2D,CWMSemi2PC2D,CWMSemi3PC2D,CWMSemi4PC2D,CWMSemi5PC2D)

###### Functional Dispersion
############ PC1  ############
FDisSemi1PC1S <-as.data.frame(Indices_semi1_PC1S$FDis)
colnames(FDisSemi1PC1S) <- c("FDis")
FDisSemi1PC1D <-as.data.frame(Indices_semi1_PC1D$FDis)
colnames(FDisSemi1PC1D) <- c("FDis")

FDisSemi2PC1S <-as.data.frame(Indices_semi2_PC1S$FDis)
colnames(FDisSemi2PC1S) <- c("FDis")
FDisSemi2PC1D <-as.data.frame(Indices_semi2_PC1D$FDis)
colnames(FDisSemi2PC1D) <- c("FDis")

FDisSemi3PC1S <-as.data.frame(Indices_semi3_PC1S$FDis)
colnames(FDisSemi3PC1S) <- c("FDis")
FDisSemi3PC1D <-as.data.frame(Indices_semi3_PC1D$FDis)
colnames(FDisSemi3PC1D) <- c("FDis")

FDisSemi4PC1S <-as.data.frame(Indices_semi4_PC1S$FDis)
colnames(FDisSemi4PC1S) <- c("FDis")
FDisSemi4PC1D <-as.data.frame(Indices_semi4_PC1D$FDis)
colnames(FDisSemi4PC1D) <- c("FDis")

FDisSemi5PC1S <-as.data.frame(Indices_semi5_PC1S$FDis)
colnames(FDisSemi5PC1S) <- c("FDis")
FDisSemi5PC1D <-as.data.frame(Indices_semi5_PC1D$FDis)
colnames(FDisSemi5PC1D) <- c("FDis")

Indices_PCA_Semisites$FDis_PC1 <-rbind.data.frame(FDisSemi1PC1S,FDisSemi2PC1S,FDisSemi3PC1S,FDisSemi4PC1S,FDisSemi5PC1S,
                                                   FDisSemi1PC1D,FDisSemi2PC1D,FDisSemi3PC1D,FDisSemi4PC1D, FDisSemi5PC1D)
############ PC2  ############

FDisSemi1PC2S <-as.data.frame(Indices_semi1_PC2S$FDis)
colnames(FDisSemi1PC2S) <- c("FDis")
FDisSemi1PC2D <-as.data.frame(Indices_semi1_PC2D$FDis)
colnames(FDisSemi1PC2D) <- c("FDis")

FDisSemi2PC2S <-as.data.frame(Indices_semi2_PC2S$FDis)
colnames(FDisSemi2PC2S) <- c("FDis")
FDisSemi2PC2D <-as.data.frame(Indices_semi2_PC2D$FDis)
colnames(FDisSemi2PC2D) <- c("FDis")

FDisSemi3PC2S <-as.data.frame(Indices_semi3_PC2S$FDis)
colnames(FDisSemi3PC2S) <- c("FDis")
FDisSemi3PC2D <-as.data.frame(Indices_semi3_PC2D$FDis)
colnames(FDisSemi3PC2D) <- c("FDis")

FDisSemi4PC2S <-as.data.frame(Indices_semi4_PC2S$FDis)
colnames(FDisSemi4PC2S) <- c("FDis")
FDisSemi4PC2D <-as.data.frame(Indices_semi4_PC2D$FDis)
colnames(FDisSemi4PC2D) <- c("FDis")

FDisSemi5PC2S <-as.data.frame(Indices_semi5_PC2S$FDis)
colnames(FDisSemi5PC2S) <- c("FDis")
FDisSemi5PC2D <-as.data.frame(Indices_semi5_PC2D$FDis)
colnames(FDisSemi5PC2D) <- c("FDis")

Indices_PCA_Semisites$FDis_PC2 <-rbind.data.frame(FDisSemi1PC2S,FDisSemi2PC2S,FDisSemi3PC2S,FDisSemi4PC2S,FDisSemi5PC2S,
                                                   FDisSemi1PC2D,FDisSemi2PC2D,FDisSemi3PC2D,FDisSemi4PC2D, FDisSemi5PC2D)
Indices_PCA_Semisites <- tibble::rownames_to_column(Indices_PCA_Semisites,"Site")
rownames(Indices_PCA_Semisites)<-c(1:60)

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
Indices_PCA_Expsites <- as.matrix.data.frame(Indices_PCA_Expsites)
Indices_PCA_pojosites <- as.matrix.data.frame(Indices_PCA_pojosites)
Indices_PCA_Semisites <- as.matrix.data.frame(Indices_PCA_Semisites)
Indices_PCA_shelsites <- as.matrix.data.frame(Indices_PCA_shelsites)
Indices_PCA_Expsites <- as.data.frame(Indices_PCA_Expsites)
Indices_PCA_pojosites <- as.data.frame(Indices_PCA_pojosites)
Indices_PCA_Semisites <- as.data.frame(Indices_PCA_Semisites)
Indices_PCA_shelsites <- as.data.frame(Indices_PCA_shelsites)

Indices_PCA_SpecificMinusFixedB<- rbind.data.frame(Indices_PCA_Expsites,Indices_PCA_Semisites,Indices_PCA_pojosites,Indices_PCA_shelsites)
Indices_PCA_SpecificMinusFixedB$Site_number<- c(rep("1S",8),rep("2S",5),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",8),rep("2D",6),rep("3D",6),rep("4D",6),
                                                 rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",6),rep("2D",6),rep("3D",6),rep("4D",6),rep("5D",6),
                                                 rep("2D",2),rep("3D",6),rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),
                                                 rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6))

Indices_PCA_SpecificMinusFixedB$Exposure_Depth <- factor(paste(Indices_PCA_SpecificMinusFixedB$Exposure, 
                                                               Indices_PCA_SpecificMinusFixedB$Depth, sep = "_"),
                                                          levels = c(
                                                            "Exposed_Shallow", "Exposed_Deep",
                                                            "Semi_Shallow", "Semi_Deep",
                                                            "Pojo_Shallow", "Pojo_Deep",
                                                            "Sheltered_Shallow"
                                                          ))


Indices_PCA_SpecificMinusFixedB$Site_Exposure <- factor(paste(Indices_PCA_SpecificMinusFixedB$Site_number, 
                                                              Indices_PCA_SpecificMinusFixedB$Exposure, sep = "_"),
                                                         levels = c(
                                                           "1S_Exposed", "2S_Exposed", "3S_Exposed", "4S_Exposed", "5S_Exposed", "1D_Exposed", "2D_Exposed", "3D_Exposed", "4D_Exposed",
                                                           "1S_Semi", "2S_Semi", "3S_Semi", "4S_Semi", "5S_Semi", "1D_Semi", "2D_Semi", "3D_Semi", "4D_Semi", "5D_Semi",
                                                           "1S_Pojo", "2S_Pojo", "3S_Pojo", "4S_Pojo", "5S_Pojo", "2D_Pojo", "3D_Pojo",
                                                           "1S_Sheltered", "2S_Sheltered", "3S_Sheltered", "4S_Sheltered", "5S_Sheltered"
                                                         ))

