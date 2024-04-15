##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##
library(rstatix)

### Exposed_Shallow
Indices_PCA_AllsitesB[c(1:31),c(10)]

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(1:31),c(2)])) # non-significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(1:31),c(6)])) # non-significant

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(1:31),c(5)])) # non-significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(1:31),c(7)])) # non-significant

### Exposed_Deep
Indices_PCA_AllsitesB[c(32:57),c(10)]

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(32:57),c(2)])) # significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(32:57),c(6)])) # significant

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(32:57),c(5)])) # significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(32:57),c(7)])) # significant

### Semi_Shallow
Indices_PCA_AllsitesB[c(58:87),c(10)]

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(58:87),c(2)])) # non-significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(58:87),c(6)])) # significant

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(58:87),c(5)])) # significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(58:87),c(7)])) # significant

### Semi_Deep
Indices_PCA_AllsitesB[c(88:117),c(10)]

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(88:117),c(2)])) # significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(88:117),c(6)])) # significant

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(88:117),c(5)])) # significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(88:117),c(7)])) # significant

### Pojo_Deep
Indices_PCA_AllsitesB[c(118:125),c(10)]

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(118:125),c(2)])) # non-significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(118:125),c(6)])) # marginally significant

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(118:125),c(5)])) # non-significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(118:125),c(7)])) # non-significant

### Pojo_Shallow
Indices_PCA_AllsitesB[c(126:155),c(10)]

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(126:155),c(2)])) # significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(126:155),c(6)])) # significant

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(126:155),c(5)])) # significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(126:155),c(7)])) # significant

### Sheltered_Shallow
Indices_PCA_AllsitesB[c(156:185),c(10)]

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(156:185),c(2)])) # significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(156:185),c(6)])) # significant

mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(156:185),c(5)])) # significant
mshapiro_test(as.numeric(Indices_PCA_AllsitesB[c(156:185),c(7)])) # significant

### FDis and CWM are overall non-normal distributes for both PC1 and PC2

##%######################################################%##
#                                                          #
####                Homogeneity tests                   ####
#                                                          #
##%######################################################%##
library(stats)
Indices_PCA_AllsitesB$CWM_PC1 <- as.numeric(Indices_PCA_AllsitesB$CWM_PC1) 
Indices_PCA_AllsitesB$FDis_PC1<- as.numeric(Indices_PCA_AllsitesB$FDis_PC1) 
Indices_PCA_AllsitesB$CWM_PC2 <- as.numeric(Indices_PCA_AllsitesB$CWM_PC2) 
Indices_PCA_AllsitesB$FDis_PC2<- as.numeric(Indices_PCA_AllsitesB$FDis_PC2) 
Indices_PCA_AllsitesB$Exposure<- as.factor(Indices_PCA_AllsitesB$Exposure) 

fligner.test(Indices_PCA_AllsitesB$CWM_PC1 ~ Indices_PCA_AllsitesB$Exposure_Depth) # signifcant
fligner.test(Indices_PCA_AllsitesB$FDis_PC1 ~ Indices_PCA_AllsitesB$Exposure_Depth) # signifcant

fligner.test(Indices_PCA_AllsitesB$CWM_PC2 ~ Indices_PCA_AllsitesB$Exposure_Depth) # signifcant
fligner.test(Indices_PCA_AllsitesB$FDis_PC2 ~ Indices_PCA_AllsitesB$Exposure_Depth) # signifcant

### FDis and CWM show heteroscedasticity for both SLA and LAP

##%######################################################%##
#                                                          #
####      Non-parametric test + heteroscedasticity     #####
#                                                          #
##%######################################################%##


##%######################################################%##
#                                                          #
####            Non parametric permanova                ####   
####                                                    ####
#                                                          #
##%######################################################%##

Indices_PCA_AllsitesB$FDis_PC1[Indices_PCA_AllsitesB$FDis_PC1 == 0] <- 0.00001 
Indices_PCA_AllsitesB$FDis_PC2[Indices_PCA_AllsitesB$FDis_PC2 == 0] <- 0.00001 
Indices_PCA_AllsitesB$CWM_PC1 <- Indices_PCA_AllsitesB$CWM_PC1 + 3.0756
Indices_PCA_AllsitesB$CWM_PC2 <- Indices_PCA_AllsitesB$CWM_PC2 + 5.2013
Shallow_PCA <- Indices_PCA_AllsitesB[ which(Indices_PCA_AllsitesB$Depth=='Shallow'), ]
Deep_PCA <- Indices_PCA_AllsitesB[ which(Indices_PCA_AllsitesB$Depth=='Deep'), ]

##%######################################################%##
#                                                          #
####                       PC1                         #####
#                                                          #
##%######################################################%##

######### Shallow sites #########
CWMPC1_S <- adonis2(Shallow_PCA$CWM_PC1 ~ Exposure + Site_Exposure, data=Shallow_PCA, perm=999)
CWMPC1_S # significant difference at exposure and site level

FDisPC1_S <- adonis2(Shallow_PCA$FDis_PC1 ~ Exposure + Site_Exposure , data=Shallow_PCA, perm=999)
FDisPC1_S # significant difference at exposure and site level

######### Deep sites #########
CWMPC1_D <- adonis2(Deep_PCA$CWM_PC1 ~ Exposure + Site_Exposure, data=Deep_PCA, perm=999)
CWMPC1_D # significant difference at exposure and site level

FDisPC1_D <- adonis2(Deep_PCA$FDis_PC1 ~ Exposure + Site_Exposure , data=Deep_PCA, perm=999)
FDisPC1_D # significant difference at exposure and site level

######### Between depths #########

CWMPC1_Depths <- adonis2(Indices_leaf_AllsitesB$CWM_PC1 ~ Exposure_Depth + Site_Exposure, 
                         data=Indices_leaf_AllsitesB, perm=999)
CWMPC1_Depths # significant difference among exposure-depth and site level

FDisPC1_Depths <- adonis2(Indices_leaf_AllsitesB$FDis_PC1 ~ Exposure_Depth + Site_Exposure, 
                          data=Indices_leaf_AllsitesB, perm=999)
FDisPC1_Depths # significant difference among exposure-depth and site level


##%######################################################%##
#                                                          #
####                       PC2                         #####
#                                                          #
##%######################################################%##

######### Shallow sites #########
CWMPC2_S <- adonis2(Shallow_PCA$CWM_PC2 ~ Exposure + Site_Exposure, data=Shallow_PCA, perm=999)
CWMPC2_S # significant difference at exposure and site level

FDisPC2_S <- adonis2(Shallow_PCA$FDis_PC2 ~ Exposure + Site_Exposure , data=Shallow_PCA, perm=999)
FDisPC2_S # significant difference at exposure and site level

######### Deep sites #########
CWMPC2_D <- adonis2(Deep_PCA$CWM_PC2 ~ Exposure + Site_Exposure, data=Deep_PCA, perm=999)
CWMPC2_D # significant difference at exposure and site level

FDisPC2_D <- adonis2(Deep_PCA$FDis_PC2 ~ Exposure + Site_Exposure , data=Deep_PCA, perm=999)
FDisPC2_D # significant difference at exposure and site level

######### Between depths #########
CWMPC2_Depths <- adonis2(Indices_leaf_AllsitesB$CWM_PC2 ~ Exposure_Depth + Site_Exposure, 
                         data=Indices_leaf_AllsitesB, perm=999)
CWMPC2_Depths # significant difference among exposure-depth and site level

FDisPC2_Depths <- adonis2(Indices_leaf_AllsitesB$FDis_PC2 ~ Exposure_Depth + Site_Exposure, 
                          data=Indices_leaf_AllsitesB, perm=999)
FDisPC2_Depths # significant difference among exposure-depth and site level


##%######################################################%##
#                                                          #
####                 Post-hoc adonis test               ####   
#                                                          #
##%######################################################%##

##%######################################################%##
#                                                          #
####                       PC1                         #####
#                                                          #
##%######################################################%##

######### Shallow sites #########

######### CWM
table(Shallow_PCA$Exposure_Depth[c(1:61)])
dist_matrixA=vegdist(Shallow_PCA$CWM_PC1[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(1:61),])

table(Shallow_PCA$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB=vegdist(Shallow_PCA$CWM_PC1[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(32:61, 92:121),])

table(Shallow_PCA$Exposure_Depth[c(92:121, 62:91)])
dist_matrixC=vegdist(Shallow_PCA$CWM_PC1[c(92:121, 62:91)],method="manhattan")
pairwise.adonis2(dist_matrixC ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(92:121, 62:91),])

######### FDis

table(Shallow_PCA$Exposure_Depth[c(1:61)])
dist_matrixA2=vegdist(Shallow_PCA$FDis_PC1[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(1:61),])

table(Shallow_PCA$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB2=vegdist(Shallow_PCA$FDis_PC1[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(32:61, 92:121),])

table(Shallow_PCA$Exposure_Depth[c(92:121, 62:91)])
dist_matrixC2=vegdist(Shallow_PCA$FDis_PC1[c(92:121, 62:91)],method="manhattan")
pairwise.adonis2(dist_matrixC2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(92:121, 62:91),])

table(Shallow_PCA$Exposure_Depth[c(1:31, 92:121)])
dist_matrixD2=vegdist(Shallow_PCA$FDis_PC1[c(1:31, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(1:31, 92:121),])

######### Deep sites #########

######### CWM
table(Deep_PCA$Exposure_Depth[c(1:26,27:56)])
dist_matrixAD=vegdist(Deep_PCA$CWM_PC1[c(1:26,27:56)],method="manhattan")
pairwise.adonis2(dist_matrixAD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_PCA[c(1:26,27:56),])

table(Deep_PCA$Exposure_Depth[c(27:64)])
dist_matrixBD=vegdist(Deep_PCA$CWM_PC1[c(27:64)],method="manhattan")
pairwise.adonis2(dist_matrixBD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_PCA[c(27:64),])

######### FDis
table(Deep_PCA$Exposure_Depth[c(1:26,27:56)])
dist_matrixBD=vegdist(Deep_PCA$FDis_PC1[c(1:26,27:56)],method="manhattan")
pairwise.adonis2(dist_matrixBD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_PCA[c(1:26,27:56),])

table(Deep_PCA$Exposure_Depth[c(27:64)])
dist_matrixBD2=vegdist(Deep_PCA$FDis_PC1[c(27:64)],method="manhattan")
pairwise.adonis2(dist_matrixBD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_PCA[c(27:64),])


######### Between depths #########
ExposedDepths <-Indices_PCA_AllsitesB[c(1:57),]
SemiDepths <-Indices_PCA_AllsitesB[c(58:117),]
PojoDepths <-Indices_PCA_AllsitesB[c(118:155),]

###Exposed
dist_matrixA_ExposedDepths=vegdist(ExposedDepths$CWM_PC1,method="manhattan")
pairwise.adonis2(dist_matrixA_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant site level

dist_matrixB_ExposedDepths=vegdist(ExposedDepths$FDis_PC1,method="manhattan")
pairwise.adonis2(dist_matrixB_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant depth and site level

###Semi
dist_matrixA_SemiDepths=vegdist(SemiDepths$CWM_PC1,method="manhattan")
pairwise.adonis2(dist_matrixA_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant depth and site level

dist_matrixB_SemiDepths=vegdist(SemiDepths$FDis_PC1,method="manhattan")
pairwise.adonis2(dist_matrixB_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant depth and site level (marginally)

###Pojo
dist_matrixA_PojoDepths=vegdist(PojoDepths$CWM_PC1,method="manhattan")
pairwise.adonis2(dist_matrixA_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #significant depth and site level (marginally)

dist_matrixB_PojoDepths=vegdist(PojoDepths$FDis_PC1,method="manhattan")
pairwise.adonis2(dist_matrixB_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #non-significant

##%######################################################%##
#                                                          #
####                       PC2                         #####
#                                                          #
##%######################################################%##

######### Shallow sites #########

######### CWM
table(Shallow_PCA$Exposure_Depth[c(1:61)])
dist_matrixA=vegdist(Shallow_PCA$CWM_PC2[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(1:61),])

table(Shallow_PCA$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB=vegdist(Shallow_PCA$CWM_PC2[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(32:61, 92:121),])

table(Shallow_PCA$Exposure_Depth[c(92:121, 62:91)])
dist_matrixC=vegdist(Shallow_PCA$CWM_PC2[c(92:121, 62:91)],method="manhattan")
pairwise.adonis2(dist_matrixC ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(92:121, 62:91),])

######### FDis

table(Shallow_PCA$Exposure_Depth[c(1:61)])
dist_matrixA2=vegdist(Shallow_PCA$FDis_PC2[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(1:61),])

table(Shallow_PCA$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB2=vegdist(Shallow_PCA$FDis_PC2[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(32:61, 92:121),])

table(Shallow_PCA$Exposure_Depth[c(92:121, 62:91)])
dist_matrixC2=vegdist(Shallow_PCA$FDis_PC2[c(92:121, 62:91)],method="manhattan")
pairwise.adonis2(dist_matrixC2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(92:121, 62:91),])

table(Shallow_PCA$Exposure_Depth[c(1:31, 92:121)])
dist_matrixD2=vegdist(Shallow_PCA$FDis_PC2[c(1:31, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_PCA[c(1:31, 92:121),])

######### Deep sites #########

######### CWM
table(Deep_PCA$Exposure_Depth[c(1:26,27:56)])
dist_matrixAD=vegdist(Deep_PCA$CWM_PC2[c(1:26,27:56)],method="manhattan")
pairwise.adonis2(dist_matrixAD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_PCA[c(1:26,27:56),])

table(Deep_PCA$Exposure_Depth[c(27:64)])
dist_matrixBD=vegdist(Deep_PCA$CWM_PC2[c(27:64)],method="manhattan")
pairwise.adonis2(dist_matrixBD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_PCA[c(27:64),])

######### FDis
table(Deep_PCA$Exposure_Depth[c(1:26,27:56)])
dist_matrixBD=vegdist(Deep_PCA$FDis_PC2[c(1:26,27:56)],method="manhattan")
pairwise.adonis2(dist_matrixBD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_PCA[c(1:26,27:56),])

table(Deep_PCA$Exposure_Depth[c(27:64)])
dist_matrixBD2=vegdist(Deep_PCA$FDis_PC2[c(27:64)],method="manhattan")
pairwise.adonis2(dist_matrixBD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_PCA[c(27:64),])

######### Between depths #########
ExposedDepths <-Indices_PCA_AllsitesB[c(1:57),]
SemiDepths <-Indices_PCA_AllsitesB[c(58:117),]
PojoDepths <-Indices_PCA_AllsitesB[c(118:155),]

#Exposed
dist_matrixC_ExposedDepths=vegdist(ExposedDepths$CWM_PC2,method="manhattan")
pairwise.adonis2(dist_matrixC_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant depth and site level

dist_matrixD_ExposedDepths=vegdist(ExposedDepths$FDis_PC2,method="manhattan")
pairwise.adonis2(dist_matrixD_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant depth and site level

###Semi
dist_matrixC_SemiDepths=vegdist(SemiDepths$CWM_PC2,method="manhattan")
pairwise.adonis2(dist_matrixC_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant site level

dist_matrixD_SemiDepths=vegdist(SemiDepths$FDis_PC2,method="manhattan")
pairwise.adonis2(dist_matrixD_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant depth and site level 

###Pojo
dist_matrixC_PojoDepths=vegdist(PojoDepths$CWM_PC2,method="manhattan")
pairwise.adonis2(dist_matrixC_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #significant site level (marginally)

dist_matrixD_PojoDepths=vegdist(PojoDepths$FDis_PC2,method="manhattan")
pairwise.adonis2(dist_matrixD_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #significant site level (marginally)

