######################################################
#          Stats for differences in                  #
#          Turnover and total values                 #
######################################################

library(rstatix)
library(stats)
library(pairwiseAdonis)
###########################
#          Height         #
###########################
Height_Stats <- rbind(Indices_height_Allsites_Shallow,Indices_height_Fixed_Shallow)

########## Normality test ##########
#Mostly non-normal distributed
# Exposed
mshapiro_test(as.numeric(Height_Stats[c(1:31),c(5)])) #significant
mshapiro_test(as.numeric(Height_Stats[c(122:152),c(5)])) #significant
# Semi
mshapiro_test(as.numeric(Height_Stats[c(32:61),c(5)])) #significant
mshapiro_test(as.numeric(Height_Stats[c(153:182),c(5)])) #significant
# Pojo
mshapiro_test(as.numeric(Height_Stats[c(62:91),c(5)])) #significant
mshapiro_test(as.numeric(Height_Stats[c(183:212),c(5)])) #non-significant
# Sheltered
mshapiro_test(as.numeric(Height_Stats[c(92:121),c(5)])) #significant
mshapiro_test(as.numeric(Height_Stats[c(213:242),c(5)])) #significant

########## Homogeneity test ##########

fligner.test(Height_Stats$CWM_Height ~ Height_Stats$Exposure_Depth) # significant

######### Adonis test  ##########

Total_H <- adonis2(Height_Stats$CWM_Height[c(1:121)] ~ Exposure_Depth + Site_Exposure, data=Height_Stats[c(1:121),], perm=999)
Total_H 

Turnover_H <- adonis2(Height_Stats$CWM_Height[c(122:242)] ~ Exposure_Depth + Site_Exposure, 
                      data=Height_Stats[c(122:242),], perm=999)
Turnover_H 

######### Post hoc Adonis test  ##########

### Total variation

table(Height_Stats$Exposure_Depth[c(1:61)])
dist_matrixA=vegdist(Height_Stats$CWM_Height[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Height_Stats[c(1:61),]) # sig

table(Height_Stats$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB=vegdist(Height_Stats$CWM_Height[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Height_Stats[c(32:61, 92:121),]) # sig

table(Height_Stats$Exposure_Depth[c(62:121)])
dist_matrixC=vegdist(Height_Stats$CWM_Height[c(62:121)],method="manhattan")
pairwise.adonis2(dist_matrixC ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Height_Stats[c(62:121),]) #sig


### Species Turnover

table(Height_Stats$Exposure_Depth[c(122:182)])
dist_matrixAT=vegdist(Height_Stats$CWM_Height[c(122:182)],method="manhattan")
pairwise.adonis2(dist_matrixAT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Height_Stats[c(122:182),]) #sig

table(Height_Stats$Exposure_Depth[c(213:242, 153:182)])
dist_matrixBT=vegdist(Height_Stats$CWM_Height[c(213:242, 153:182)],method="manhattan")
pairwise.adonis2(dist_matrixBT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Height_Stats[c(213:242, 153:182),])# Marg sig

table(Height_Stats$Exposure_Depth[c(183:242)])
dist_matrixCT=vegdist(Height_Stats$CWM_Height[c(183:242)],method="manhattan")
pairwise.adonis2(dist_matrixCT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Height_Stats[c(183:242),]) # sig

### Total variation - Species Turnover

table(Height_Stats$Exposure_Depth[c(1:31, 122:152)])
dist_matrixATS=vegdist(Height_Stats$CWM_Height[c(1:31, 122:152)],method="manhattan")
pairwise.adonis2(dist_matrixATS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Height_Stats[c(1:31, 122:152),]) #sig

table(Height_Stats$Exposure_Depth[c(32:61, 153:182)])
dist_matrixBTS=vegdist(Height_Stats$CWM_Height[c(32:61, 153:182)],method="manhattan")
pairwise.adonis2(dist_matrixBTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Height_Stats[c(32:61, 153:182),])# Marg sig

table(Height_Stats$Exposure_Depth[c(92:121, 213:242)])
dist_matrixCTS=vegdist(Height_Stats$CWM_Height[c(92:121, 213:242)],method="manhattan")
pairwise.adonis2(dist_matrixCTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Height_Stats[c(92:121, 213:242),]) # sig

table(Height_Stats$Exposure_Depth[c(62:91, 183:212)])
dist_matrixDTS=vegdist(Height_Stats$CWM_Height[c(62:91, 183:212)],method="manhattan")
pairwise.adonis2(dist_matrixDTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Height_Stats[c(62:91, 183:212),]) # sig

###########################
#      Root depth         #
###########################
RootDepth_Stats <- rbind(Indices_RootDepth_Allsites_Shallow,Indices_RootDepth_Fixed_Shallow)

########## Normality test ##########
#All non-normal distributed
# Exposed
mshapiro_test(as.numeric(RootDepth_Stats[c(1:31),c(5)])) #significant
mshapiro_test(as.numeric(RootDepth_Stats[c(122:152),c(5)])) #significant
# Semi
mshapiro_test(as.numeric(RootDepth_Stats[c(32:61),c(5)])) #significant
mshapiro_test(as.numeric(RootDepth_Stats[c(153:182),c(5)])) #significant
# Sheltered
mshapiro_test(as.numeric(RootDepth_Stats[c(62:91),c(5)])) #significant
mshapiro_test(as.numeric(RootDepth_Stats[c(183:212),c(5)])) #significant
# Pojo
mshapiro_test(as.numeric(RootDepth_Stats[c(92:121),c(5)])) #significant
mshapiro_test(as.numeric(RootDepth_Stats[c(213:242),c(5)])) #significant

########## Homogeneity test ##########

fligner.test(RootDepth_Stats$CWM_RootDepth ~ RootDepth_Stats$Exposure_Depth) # significant

######### Adonis test  ##########

RootDepth_Stats$CWM_RootDepth[RootDepth_Stats$CWM_RootDepth == 0] <- 0.00001 ### Permanova can´t handle zeros

Total_R <- adonis2(RootDepth_Stats$CWM_RootDepth[c(1:121)] ~ Exposure_Depth + Site_Exposure, 
                   data=RootDepth_Stats[c(1:121),], perm=999)
Total_R 

Turnover_R <- adonis2(RootDepth_Stats$CWM_RootDepth[c(122:242)] ~ Exposure_Depth + Site_Exposure, 
                      data=RootDepth_Stats[c(122:242),], perm=999)
Turnover_R 

######### Post hoc Adonis test  ##########

### Total variation

table(RootDepth_Stats$Exposure_Depth[c(1:61)])
dist_matrixRA=vegdist(RootDepth_Stats$CWM_RootDepth[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixRA ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = RootDepth_Stats[c(1:61),]) # sig

table(RootDepth_Stats$Exposure_Depth[c(32:61, 92:121)])
dist_matrixRB=vegdist(RootDepth_Stats$CWM_RootDepth[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixRB ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = RootDepth_Stats[c(32:61, 92:121),])#  non sig

table(RootDepth_Stats$Exposure_Depth[c(62:121)])
dist_matrixRC=vegdist(RootDepth_Stats$CWM_RootDepth[c(62:121)],method="manhattan")
pairwise.adonis2(dist_matrixRC ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = RootDepth_Stats[c(62:121),]) # non sig


### Species Turnover

table(RootDepth_Stats$Exposure_Depth[c(122:182)])
dist_matrixRAT=vegdist(RootDepth_Stats$CWM_RootDepth[c(122:182)],method="manhattan")
pairwise.adonis2(dist_matrixRAT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = RootDepth_Stats[c(122:182),])# non sig

table(RootDepth_Stats$Exposure_Depth[c(213:242, 153:182)])
dist_matrixRBT=vegdist(RootDepth_Stats$CWM_RootDepth[c(213:242, 153:182)],method="manhattan")
pairwise.adonis2(dist_matrixRBT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = RootDepth_Stats[c(213:242, 153:182),])# sig

table(RootDepth_Stats$Exposure_Depth[c(183:242)])
dist_matrixRCT=vegdist(RootDepth_Stats$CWM_RootDepth[c(183:242)],method="manhattan")
pairwise.adonis2(dist_matrixRCT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = RootDepth_Stats[c(183:242),]) #  non sig

### Total variation - Species Turnover

table(RootDepth_Stats$Exposure_Depth[c(1:31, 122:152)])
dist_matrixARTS=vegdist(RootDepth_Stats$CWM_RootDepth[c(1:31, 122:152)],method="manhattan")
pairwise.adonis2(dist_matrixARTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = RootDepth_Stats[c(1:31, 122:152),]) 

table(RootDepth_Stats$Exposure_Depth[c(32:61, 153:182)])
dist_matrixBRTS=vegdist(RootDepth_Stats$CWM_RootDepth[c(32:61, 153:182)],method="manhattan")
pairwise.adonis2(dist_matrixBRTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = RootDepth_Stats[c(32:61, 153:182),])

table(RootDepth_Stats$Exposure_Depth[c(92:121, 213:242)])
dist_matrixCRTS=vegdist(RootDepth_Stats$CWM_RootDepth[c(92:121, 213:242)],method="manhattan")
pairwise.adonis2(dist_matrixCRTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = RootDepth_Stats[c(92:121, 213:242),]) 

table(RootDepth_Stats$Exposure_Depth[c(62:91, 183:212)])
dist_matrixDRTS=vegdist(RootDepth_Stats$CWM_RootDepth[c(62:91, 183:212)],method="manhattan")
pairwise.adonis2(dist_matrixDRTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = RootDepth_Stats[c(62:91, 183:212),]) 

###########################
#       R:s ratio         #
###########################
Ratio_Stats <- rbind(Indices_Ratio_Allsites_Shallow,Indices_Ratio_Fixed_Shallow)

########## Normality test ##########
#All non-normal distributed
# Exposed
mshapiro_test(as.numeric(Ratio_Stats[c(1:31),c(5)])) #significant
mshapiro_test(as.numeric(Ratio_Stats[c(122:152),c(5)])) #significant
# Semi
mshapiro_test(as.numeric(Ratio_Stats[c(32:61),c(5)])) #significant
mshapiro_test(as.numeric(Ratio_Stats[c(153:182),c(5)])) #significant
# Sheltered
mshapiro_test(as.numeric(Ratio_Stats[c(62:91),c(5)])) #significant
mshapiro_test(as.numeric(Ratio_Stats[c(183:212),c(5)])) #significant
# Pojo
mshapiro_test(as.numeric(Ratio_Stats[c(92:121),c(5)])) #significant
mshapiro_test(as.numeric(Ratio_Stats[c(213:242),c(5)])) #significant

########## Homogeneity test ##########

fligner.test(Ratio_Stats$CWM_Ratio ~ Ratio_Stats$Exposure_Depth) # significant

######### Adonis test  ##########

Ratio_Stats$CWM_Ratio[Ratio_Stats$CWM_Ratio == 0] <- 0.00001 ### Permanova can´t handle zeros

Total_Ratio <- adonis2(Ratio_Stats$CWM_Ratio[c(1:121)] ~ Exposure_Depth + Site_Exposure, data=Ratio_Stats[c(1:121),], 
                       perm=999)
Total_Ratio 

Turnover_Ratio <- adonis2(Ratio_Stats$CWM_Ratio[c(122:242)] ~ Exposure_Depth + Site_Exposure, 
                      data=Ratio_Stats[c(122:242),], perm=999)
Turnover_Ratio

######### Post hoc Adonis test  ##########

### Total variation

table(Ratio_Stats$Exposure_Depth[c(1:61)])
dist_matrixRA=vegdist(Ratio_Stats$CWM_Ratio[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixRA ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = Ratio_Stats[c(1:61),]) # sig

table(Ratio_Stats$Exposure_Depth[c(32:61, 92:121)])
dist_matrixRB=vegdist(Ratio_Stats$CWM_Ratio[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixRB ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = Ratio_Stats[c(32:61, 92:121),])#sig

table(Ratio_Stats$Exposure_Depth[c(62:121)])
dist_matrixRC=vegdist(Ratio_Stats$CWM_Ratio[c(62:121)],method="manhattan")
pairwise.adonis2(dist_matrixRC ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = Ratio_Stats[c(62:121),]) # non sig


### Species Turnover

table(Ratio_Stats$Exposure_Depth[c(122:182)])
dist_matrixRAT=vegdist(Ratio_Stats$CWM_Ratio[c(122:182)],method="manhattan")
pairwise.adonis2(dist_matrixRAT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = Ratio_Stats[c(122:182),])# sig

table(Ratio_Stats$Exposure_Depth[c(213:242, 153:182)])
dist_matrixRBT=vegdist(Ratio_Stats$CWM_Ratio[c(213:242, 153:182)],method="manhattan")
pairwise.adonis2(dist_matrixRBT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = Ratio_Stats[c(213:242, 153:182),])# sig

table(Ratio_Stats$Exposure_Depth[c(183:242)])
dist_matrixRCT=vegdist(Ratio_Stats$CWM_Ratio[c(183:242)],method="manhattan")
pairwise.adonis2(dist_matrixRCT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = Ratio_Stats[c(183:242),]) #  non sig

### Total variation - Species Turnover

table(Ratio_Stats$Exposure_Depth[c(1:31, 122:152)])
dist_matrixARTS=vegdist(Ratio_Stats$CWM_Ratio[c(1:31, 122:152)],method="manhattan")
pairwise.adonis2(dist_matrixARTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Ratio_Stats[c(1:31, 122:152),]) 

table(Ratio_Stats$Exposure_Depth[c(32:61, 153:182)])
dist_matrixBRTS=vegdist(Ratio_Stats$CWM_Ratio[c(32:61, 153:182)],method="manhattan")
pairwise.adonis2(dist_matrixBRTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Ratio_Stats[c(32:61, 153:182),])

table(Ratio_Stats$Exposure_Depth[c(92:121, 213:242)])
dist_matrixCRTS=vegdist(Ratio_Stats$CWM_Ratio[c(92:121, 213:242)],method="manhattan")
pairwise.adonis2(dist_matrixCRTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Ratio_Stats[c(92:121, 213:242),]) 

table(Ratio_Stats$Exposure_Depth[c(62:91, 183:212)])
dist_matrixDRTS=vegdist(Ratio_Stats$CWM_Ratio[c(62:91, 183:212)],method="manhattan")
pairwise.adonis2(dist_matrixDRTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Ratio_Stats[c(62:91, 183:212),]) 


###########################
#          SLA            #
###########################
Leaf_Stats <- rbind(Indices_Leaf_Allsites_Shallow,Indices_Leaf_Fixed_Shallow)

########## Normality test ##########
#Mostly non-normal distributed
# Exposed
mshapiro_test(as.numeric(Leaf_Stats[c(1:31),c(2)])) #non-significant
mshapiro_test(as.numeric(Leaf_Stats[c(122:152),c(2)])) #significant
# Semi
mshapiro_test(as.numeric(Leaf_Stats[c(32:61),c(2)])) #significant
mshapiro_test(as.numeric(Leaf_Stats[c(153:182),c(2)])) #significant
# Sheltered
mshapiro_test(as.numeric(Leaf_Stats[c(62:91),c(2)])) #significant
mshapiro_test(as.numeric(Leaf_Stats[c(183:212),c(2)])) #significant
# Pojo
mshapiro_test(as.numeric(Leaf_Stats[c(92:121),c(2)])) #significant
mshapiro_test(as.numeric(Leaf_Stats[c(213:242),c(2)])) #significant

########## Homogeneity test ##########

fligner.test(Leaf_Stats$CWM_SLA ~ Leaf_Stats$Exposure_Depth) # significant

######### Adonis test  ##########

Total_L <- adonis2(Leaf_Stats$CWM_SLA[c(1:121)] ~ Exposure_Depth + Site_Exposure, data=Leaf_Stats[c(1:121),], 
                       perm=999)
Total_L 

Turnover_L <- adonis2(Leaf_Stats$CWM_SLA[c(122:242)] ~ Exposure_Depth + Site_Exposure, 
                          data=Leaf_Stats[c(122:242),], perm=999)
Turnover_L

######### Post hoc Adonis test  ##########

### Total variation

table(Leaf_Stats$Exposure_Depth[c(1:61)])
dist_matrixSA=vegdist(Leaf_Stats$CWM_SLA[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixSA ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = Leaf_Stats[c(1:61),]) # non sig

table(Leaf_Stats$Exposure_Depth[c(32:61, 92:121)])
dist_matrixSB=vegdist(Leaf_Stats$CWM_SLA[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixSB ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = Leaf_Stats[c(32:61, 92:121),])# non sig

table(Leaf_Stats$Exposure_Depth[c(62:121)])
dist_matrixSC=vegdist(Leaf_Stats$CWM_SLA[c(62:121)],method="manhattan")
pairwise.adonis2(dist_matrixSC ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = Leaf_Stats[c(62:121),]) # sig


### Species Turnover

table(Leaf_Stats$Exposure_Depth[c(122:182)])
dist_matrixSAT=vegdist(Leaf_Stats$CWM_SLA[c(122:182)],method="manhattan")
pairwise.adonis2(dist_matrixSAT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = Leaf_Stats[c(122:182),])# non sig

table(Leaf_Stats$Exposure_Depth[c(213:242, 153:182)])
dist_matrixSBT=vegdist(Leaf_Stats$CWM_SLA[c(213:242, 153:182)],method="manhattan")
pairwise.adonis2(dist_matrixSBT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = Leaf_Stats[c(213:242, 153:182),])# non sig

table(Leaf_Stats$Exposure_Depth[c(183:242)])
dist_matrixSCT=vegdist(Leaf_Stats$CWM_SLA[c(183:242)],method="manhattan")
pairwise.adonis2(dist_matrixSCT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = Leaf_Stats[c(183:242),]) # sig

### Total variation - Species Turnover

table(Leaf_Stats$Exposure_Depth[c(1:31, 122:152)])
dist_matrixALTS=vegdist(Leaf_Stats$CWM_SLA[c(1:31, 122:152)],method="manhattan")
pairwise.adonis2(dist_matrixALTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Leaf_Stats[c(1:31, 122:152),]) 

table(Leaf_Stats$Exposure_Depth[c(32:61, 153:182)])
dist_matrixBLTS=vegdist(Leaf_Stats$CWM_SLA[c(32:61, 153:182)],method="manhattan")
pairwise.adonis2(dist_matrixBLTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Leaf_Stats[c(32:61, 153:182),])

table(Leaf_Stats$Exposure_Depth[c(92:121, 213:242)])
dist_matrixCLTS=vegdist(Leaf_Stats$CWM_SLA[c(92:121, 213:242)],method="manhattan")
pairwise.adonis2(dist_matrixCLTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Leaf_Stats[c(92:121, 213:242),]) 

table(Leaf_Stats$Exposure_Depth[c(62:91, 183:212)])
dist_matrixDLTS=vegdist(Leaf_Stats$CWM_SLA[c(62:91, 183:212)],method="manhattan")
pairwise.adonis2(dist_matrixDLTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = Leaf_Stats[c(62:91, 183:212),]) 

###########################
#          PC1            #
###########################
PCA_Stats <- rbind(Indices_PCA_AllsitesB, Indices_PCA_Fixed)

########## Normality test ##########
#Mostly non-normal distributed
# Exposed
mshapiro_test(as.numeric(PCA_Stats[c(1:31),c(2)])) #non-significant
mshapiro_test(as.numeric(PCA_Stats[c(122:152),c(2)])) #significant
# Semi
mshapiro_test(as.numeric(PCA_Stats[c(32:61),c(2)])) #non-significant
mshapiro_test(as.numeric(PCA_Stats[c(153:182),c(2)])) #marginally significant
# Sheltered
mshapiro_test(as.numeric(PCA_Stats[c(62:91),c(2)])) #significant
mshapiro_test(as.numeric(PCA_Stats[c(183:212),c(2)])) #non-significant
# Pojo
mshapiro_test(as.numeric(PCA_Stats[c(92:121),c(2)])) #significant
mshapiro_test(as.numeric(PCA_Stats[c(213:242),c(2)])) #significant

########## Homogeneity test ##########

fligner.test(PCA_Stats$CWM_PC1 ~ PCA_Stats$Exposure_Depth) # significant

######### Adonis test  ##########

#PCA_Stats$CWM_PC1 <- PCA_Stats$CWM_PC1 + 3.14
#PCA_Stats$CWM_PC2 <- PCA_Stats$CWM_PC2 + 1.26

Total_PC1 <- adonis2(PCA_Stats$CWM_PC1[c(1:121)] ~ Exposure_Depth + Site_Exposure, data=PCA_Stats[c(1:121),], 
                   perm=999)
Total_PC1 

Turnover_PC1 <- adonis2(PCA_Stats$CWM_PC1[c(122:242)] ~ Exposure_Depth + Site_Exposure, 
                      data=PCA_Stats[c(122:242),], perm=999)
Turnover_PC1

######### Post hoc Adonis test  ##########

### Total variation

table(PCA_Stats$Exposure_Depth[c(1:61)])
dist_matrixPA=vegdist(PCA_Stats$CWM_PC1[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixPA ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PCA_Stats[c(1:61),]) # non sig

table(PCA_Stats$Exposure_Depth[c(32:61, 92:121)])
dist_matrixPB=vegdist(PCA_Stats$CWM_PC1[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixPB ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PCA_Stats[c(32:61, 92:121),])# non sig

table(PCA_Stats$Exposure_Depth[c(62:121)])
dist_matrixPC=vegdist(PCA_Stats$CWM_PC1[c(62:121)],method="manhattan")
pairwise.adonis2(dist_matrixPC ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PCA_Stats[c(62:121),]) # sig

### Species Turnover

table(PCA_Stats$Exposure_Depth[c(122:182)])
dist_matrixPAT=vegdist(PCA_Stats$CWM_PC1[c(122:182)],method="manhattan")
pairwise.adonis2(dist_matrixPAT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PCA_Stats[c(122:182),])# non sig

table(PCA_Stats$Exposure_Depth[c(213:242, 153:182)])
dist_matrixPBT=vegdist(PCA_Stats$CWM_PC1[c(213:242, 153:182)],method="manhattan")
pairwise.adonis2(dist_matrixPBT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PCA_Stats[c(213:242, 153:182),])# sig

table(PCA_Stats$Exposure_Depth[c(183:242)])
dist_matrixPCT=vegdist(PCA_Stats$CWM_PC1[c(183:242)],method="manhattan")
pairwise.adonis2(dist_matrixPCT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PCA_Stats[c(183:242),]) # sig

### Total variation - Species Turnover

table(PCA_Stats$Exposure_Depth[c(1:31, 122:152)])
dist_matrixAPTS=vegdist(PCA_Stats$CWM_PC1[c(1:31, 122:152)],method="manhattan")
pairwise.adonis2(dist_matrixAPTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = PCA_Stats[c(1:31, 122:152),]) 

table(PCA_Stats$Exposure_Depth[c(32:61, 153:182)])
dist_matrixBPTS=vegdist(PCA_Stats$CWM_PC1[c(32:61, 153:182)],method="manhattan")
pairwise.adonis2(dist_matrixBPTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = PCA_Stats[c(32:61, 153:182),])

table(PCA_Stats$Exposure_Depth[c(92:121, 213:242)])
dist_matrixCPTS=vegdist(PCA_Stats$CWM_PC1[c(92:121, 213:242)],method="manhattan")
pairwise.adonis2(dist_matrixCPTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = PCA_Stats[c(92:121, 213:242),]) 

table(PCA_Stats$Exposure_Depth[c(62:91, 183:212)])
dist_matrixDPTS=vegdist(PCA_Stats$CWM_PC1[c(62:91, 183:212)],method="manhattan")
pairwise.adonis2(dist_matrixDPTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = PCA_Stats[c(62:91, 183:212),]) 

###########################
#          PC2            #
###########################
PCA_Stats

########## Normality test ##########
#Mostly non-normal distributed
# Exposed
mshapiro_test(as.numeric(PCA_Stats[c(1:31),c(5)])) #significant
mshapiro_test(as.numeric(PCA_Stats[c(122:152),c(5)])) #non-significant
# Semi
mshapiro_test(as.numeric(PCA_Stats[c(32:61),c(5)])) #non-significant
mshapiro_test(as.numeric(PCA_Stats[c(153:182),c(5)])) #non-significant
# Sheltered
mshapiro_test(as.numeric(PCA_Stats[c(62:91),c(5)])) #significant
mshapiro_test(as.numeric(PCA_Stats[c(183:212),c(5)])) #significant
# Pojo
mshapiro_test(as.numeric(PCA_Stats[c(92:121),c(5)])) #significant
mshapiro_test(as.numeric(PCA_Stats[c(213:242),c(5)])) #significant

########## Homogeneity test ##########

fligner.test(PCA_Stats$CWM_PC2 ~ PCA_Stats$Exposure_Depth) # significant

######### Adonis test  ##########

Total_PC2 <- adonis2(PCA_Stats$CWM_PC2[c(1:121)] ~ Exposure_Depth + Site_Exposure, data=PCA_Stats[c(1:121),], 
                     perm=999)
Total_PC2

Turnover_PC2 <- adonis2(PCA_Stats$CWM_PC2[c(122:242)] ~ Exposure_Depth + Site_Exposure, 
                        data=PCA_Stats[c(122:242),], perm=999)
Turnover_PC2

######### Post hoc Adonis test  ##########

### Total variation

table(PCA_Stats$Exposure_Depth[c(1:61)])
dist_matrixPA=vegdist(PCA_Stats$CWM_PC2[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixPA ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PCA_Stats[c(1:61),]) # sig

table(PCA_Stats$Exposure_Depth[c(32:61, 92:121)])
dist_matrixPB=vegdist(PCA_Stats$CWM_PC2[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixPB ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PCA_Stats[c(32:61, 92:121),])# sig

table(PCA_Stats$Exposure_Depth[c(62:121)])
dist_matrixPC=vegdist(PCA_Stats$CWM_PC2[c(62:121)],method="manhattan")
pairwise.adonis2(dist_matrixPC ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PCA_Stats[c(62:121),]) # sig


### Species Turnover

table(PCA_Stats$Exposure_Depth[c(122:182)])
dist_matrixPAT=vegdist(PCA_Stats$CWM_PC2[c(122:182)],method="manhattan")
pairwise.adonis2(dist_matrixPAT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PCA_Stats[c(122:182),])# sig

table(PCA_Stats$Exposure_Depth[c(213:242, 153:182)])
dist_matrixPBT=vegdist(PCA_Stats$CWM_PC2[c(213:242, 153:182)],method="manhattan")
pairwise.adonis2(dist_matrixPBT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PCA_Stats[c(213:242, 153:182),])# marg sig

table(PCA_Stats$Exposure_Depth[c(183:242)])
dist_matrixPCT=vegdist(PCA_Stats$CWM_PC2[c(183:242)],method="manhattan")
pairwise.adonis2(dist_matrixPCT ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PCA_Stats[c(183:242),]) # non sig

### Total variation - Species Turnover

table(PCA_Stats$Exposure_Depth[c(1:31, 122:152)])
dist_matrixAPTS=vegdist(PCA_Stats$CWM_PC2[c(1:31, 122:152)],method="manhattan")
pairwise.adonis2(dist_matrixAPTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = PCA_Stats[c(1:31, 122:152),]) 

table(PCA_Stats$Exposure_Depth[c(32:61, 153:182)])
dist_matrixBPTS=vegdist(PCA_Stats$CWM_PC2[c(32:61, 153:182)],method="manhattan")
pairwise.adonis2(dist_matrixBPTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = PCA_Stats[c(32:61, 153:182),])

table(PCA_Stats$Exposure_Depth[c(92:121, 213:242)])
dist_matrixCPTS=vegdist(PCA_Stats$CWM_PC2[c(92:121, 213:242)],method="manhattan")
pairwise.adonis2(dist_matrixCPTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = PCA_Stats[c(92:121, 213:242),]) 

table(PCA_Stats$Exposure_Depth[c(62:91, 183:212)])
dist_matrixDPTS=vegdist(PCA_Stats$CWM_PC2[c(62:91, 183:212)],method="manhattan")
pairwise.adonis2(dist_matrixDPTS ~ Exposure_Depth + Site_Exposure, p.adj = "hochberg", data = PCA_Stats[c(62:91, 183:212),]) 
