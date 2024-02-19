##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##
library(rstatix)

### Exposed_Shallow
Indices_Ratio_AllsitesB[c(1:31),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(1:31),c(2)])) # non-significant
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(1:31),c(5)])) # significant

### Exposed_Deep
Indices_Ratio_AllsitesB[c(32:57),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(32:57),c(2)])) # significant
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(32:57),c(5)])) # significant

### Semi_shallow
Indices_Ratio_AllsitesB[c(58:87),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(58:87),c(2)])) # non-significant
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(58:87),c(5)])) # significant

### Semi_Deep
Indices_Ratio_AllsitesB[c(88:117),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(88:117),c(2)])) # significant
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(88:117),c(5)])) # significant

### Pojo_Deep
Indices_Ratio_AllsitesB[c(118:125),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(118:125),c(2)])) # non-significant
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(118:125),c(5)])) # non-significant

### Pojo_Shallow
Indices_Ratio_AllsitesB[c(126:155),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(126:155),c(2)])) # significant
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(126:155),c(5)])) # significant

### Sheltered_Shallow
Indices_Ratio_AllsitesB[c(156:185),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(156:185),c(2)])) # significant
mshapiro_test(as.numeric(Indices_Ratio_AllsitesB[c(156:185),c(5)])) # significant

### FDis and CWM are overall non-normal distributed

##%######################################################%##
#                                                          #
####                Homogeneity tests                   ####
#                                                          #
##%######################################################%##
library(stats)
Indices_Ratio_AllsitesB$CWM_Ratio<- as.numeric(Indices_Ratio_AllsitesB$CWM_Ratio) 
Indices_Ratio_AllsitesB$FDis_Ratio<- as.numeric(Indices_Ratio_AllsitesB$FDis_Ratio) 

fligner.test(Indices_Ratio_AllsitesB$CWM_Ratio ~ Indices_Ratio_AllsitesB$Exposure_Depth) # signifcant
fligner.test(Indices_Ratio_AllsitesB$FDis_Ratio ~ Indices_Ratio_AllsitesB$Exposure_Depth) # signifcant

### FDis and CWM show heteroscedasticity

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

# Permanova can´t handle zero`s
Indices_Ratio_AllsitesB$FDis_Ratio[Indices_Ratio_AllsitesB$FDis_Ratio == 0] <- 0.00001 
Indices_Ratio_AllsitesB$CWM_Ratio <- Indices_Ratio_AllsitesB$CWM_Ratio + 1 # small values between 0-1 give problems for the function
Shallow_Ratio <- Indices_Ratio_AllsitesB[ which(Indices_Ratio_AllsitesB$Depth=='Shallow'), ]
Deep_Ratio <- Indices_Ratio_AllsitesB[ which(Indices_Ratio_AllsitesB$Depth=='Deep'), ]

######### Shallow sites #########
CWMRatio_S <- adonis2(Shallow_Ratio$CWM_Ratio ~ Exposure + Site_Exposure, data=Shallow_Ratio, perm=999)
CWMRatio_S # significant difference at exposure and site level

FDisRatio_S <- adonis2(Shallow_Ratio$FDis_Ratio ~ Exposure + Site_Exposure , data=Shallow_Ratio, perm=999)
FDisRatio_S # significant difference at exposure and site level

######### Deep sites #########
CWMRatio_D <- adonis2(Deep_Ratio$CWM_Ratio ~ Exposure + Site_Exposure, data=Deep_Ratio, perm=999)
CWMRatio_D # significant difference at exposure and site level

FDisRatio_D <- adonis2(Deep_Ratio$FDis_Ratio ~ Exposure + Site_Exposure , data=Deep_Ratio, perm=999)
FDisRatio_D # significant difference at exposure and site level

######### Between depths #########
CWMRatio_Depths <- adonis2(Indices_Ratio_AllsitesB$CWM_Ratio ~ Exposure_Depth + Site_Exposure, 
                            data=Indices_Ratio_AllsitesB, perm=999)
CWMRatio_Depths # significant difference among exposure-depth and site level

FDisRatio_Depths <- adonis2(Indices_Ratio_AllsitesB$FDis_Ratio ~ Exposure_Depth + Site_Exposure, 
                             data=Indices_Ratio_AllsitesB, perm=999)
FDisRatio_Depths # significant difference among exposure-depth and site level

##%######################################################%##
#                                                          #
####                 Post-hoc adonis test               ####   
#                                                          #
##%######################################################%##

######### Shallow sites #########
dist_matrixA=vegdist(Shallow_Ratio$CWM_Ratio,method="manhattan")
pairwise.adonis2(dist_matrixA ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Ratio)

dist_matrixB=vegdist(Shallow_Ratio$FDis_Ratio,method="manhattan")
pairwise.adonis2(dist_matrixB ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Ratio)

######### Deep sites #########
dist_matrixAD=vegdist(Deep_Ratio$CWM_Ratio,method="manhattan")
pairwise.adonis2(dist_matrixAD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Ratio)

dist_matrixBD=vegdist(Deep_Ratio$FDis_Ratio,method="manhattan")
pairwise.adonis2(dist_matrixBD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Ratio)

######### Between depths #########
ExposedDepths <-Indices_Ratio_AllsitesB[c(1:57),]
SemiDepths <-Indices_Ratio_AllsitesB[c(58:117),]
PojoDepths <-Indices_Ratio_AllsitesB[c(118:155),]

dist_matrixA_ExposedDepths=vegdist(ExposedDepths$CWM_Ratio,method="manhattan")
pairwise.adonis2(dist_matrixA_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant depth and site level

dist_matrixB_ExposedDepths=vegdist(ExposedDepths$FDis_Ratio,method="manhattan")
pairwise.adonis2(dist_matrixB_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant depth and site level

dist_matrixA_SemiDepths=vegdist(SemiDepths$CWM_Ratio,method="manhattan")
pairwise.adonis2(dist_matrixA_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant depth and site level

dist_matrixB_SemiDepths=vegdist(SemiDepths$FDis_Ratio,method="manhattan")
pairwise.adonis2(dist_matrixB_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant depth 

dist_matrixA_PojoDepths=vegdist(PojoDepths$CWM_Ratio,method="manhattan")
pairwise.adonis2(dist_matrixA_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #non-significant

dist_matrixB_PojoDepths=vegdist(PojoDepths$FDis_Ratio,method="manhattan")
pairwise.adonis2(dist_matrixB_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #marginally significant depth
