##%######################################################%##
#                                                          #
####                   PCA all traits                   ####
#                                                          #
##%######################################################%##

##########################################
###########          #####################
########### EXPOSED ######################
###########         ######################
##########################################
Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")
TraitData_exp <- Spatial_Campaign_data_exp[-c(43),c(1:5, 29,30, 20, 35:38)]
###Height
names(TraitData_exp)[5]<-paste(c("Height"))
names(TraitData_exp)[2]<-paste(c("Site_number"))
###Ratio
TraitData_exp$Ratio <- TraitData_exp$`Total belowground dry mass (mg)`/TraitData_exp$`Total aboveground dry mass (mg)`
###Rootdepth
names(TraitData_exp)[8]<-paste(c("RootDepth"))
TraitData_exp[is.na(TraitData_exp)] = 0
###SLA
TraitData_exp$SLA <- (TraitData_exp$`Specific leaf area 1 (mm^2/mg)`+ TraitData_exp$`Specific leaf area 2 (mm^2/mg)`)/2
###LAP
TraitData_exp$LAP <- (TraitData_exp$`Leaf area - perimeter ratio 1`+ TraitData_exp$`Leaf area - perimeter ratio 2`)/2

TraitData_exp <- TraitData_exp[,c(1:5, 8, 13:15)]
TraitData_exp$Exposure <- "Exposed"

##########################################
###########          #####################
###########   SEMI  ######################
###########         ######################
##########################################

Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")
TraitData_semi <- Spatial_Campaign_data_semi[,c(1:5, 30,31, 20, 36:39)]

###Height
names(TraitData_semi)[5]<-paste(c("Height"))
names(TraitData_semi)[2]<-paste(c("Site_number"))
###Ratio
TraitData_semi$`Total belowground dry mass (mg)`[is.na(TraitData_semi$`Total belowground dry mass (mg)`)] = 0
TraitData_semi$Ratio <- TraitData_semi$`Total belowground dry mass (mg)`/TraitData_semi$`Total aboveground dry mass (mg)`
###Rootdepth
names(TraitData_semi)[8]<-paste(c("RootDepth"))
TraitData_semi$RootDepth[is.na(TraitData_semi$RootDepth)] = 0
###SLA
TraitData_semi$SLA <- (TraitData_semi$`Specific leaf area 1 (mm^2/mg)`+TraitData_semi$`Specific leaf area 2 (mm^2/mg)`)/2
###LAP
TraitData_semi$LAP <- (TraitData_semi$`Leaf area - perimeter ratio 1`+TraitData_semi$`Leaf area - perimeter ratio 2`)/2

TraitData_semi <- TraitData_semi[,c(1:5, 8, 13:15)]
TraitData_semi$Exposure <- "Semi"


##############################################
###########             ######################
###########   SHELTERED ######################
###########             ######################
##############################################

Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")
TraitData_shel <- Spatial_Campaign_data_shel[-c(2),c(1:5, 34,35, 20, 40:43)]

###Height
names(TraitData_shel)[5]<-paste(c("Height"))
names(TraitData_shel)[2]<-paste(c("Site_number"))
###Ratio
TraitData_shel$`Total belowground dry mass (mg)`[is.na(TraitData_shel$`Total belowground dry mass (mg)`)] = 0
TraitData_shel$Ratio <- TraitData_shel$`Total belowground dry mass (mg)`/TraitData_shel$`Total aboveground dry mass (mg)`
###Rootdepth
names(TraitData_shel)[8]<-paste(c("RootDepth"))
TraitData_shel$RootDepth[is.na(TraitData_shel$RootDepth)] = 0
###SLA
TraitData_shel$SLA <- (TraitData_shel$`Specific leaf area 1 (cm^2/mg)`+TraitData_shel$`Specific leaf area 2 (cm^2/mg)`)/2
###LAP
TraitData_shel$LAP <- (TraitData_shel$`Leaf area - perimeter ratio 1`+TraitData_shel$`Leaf area - perimeter ratio 2`)/2

TraitData_shel <- TraitData_shel[,c(1:5, 8, 13:15)]
TraitData_shel$Exposure <- "Shel"

##############################################
###########             ######################
###########    Pojo     ######################
###########             ######################
##############################################

Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")
TraitData_pojo <- Spatial_Campaign_data_pojo[-c(1,4,6,7,60),c(1:5, 32,33, 20, 38:41)]

###Height
names(TraitData_pojo)[5]<-paste(c("Height"))
names(TraitData_pojo)[2]<-paste(c("Site_number"))
###Ratio
TraitData_pojo$`Total belowground dry mass (mg)`[is.na(TraitData_pojo$`Total belowground dry mass (mg)`)] = 0
TraitData_pojo$Ratio <- TraitData_pojo$`Total belowground dry mass (mg)`/TraitData_pojo$`Total aboveground dry mass (mg)`
###Rootdepth
names(TraitData_pojo)[8]<-paste(c("RootDepth"))
TraitData_pojo$RootDepth[is.na(TraitData_pojo$RootDepth)] = 0
###SLA
TraitData_pojo$SLA <- (TraitData_pojo$`Specific leaf area 1 (cm^2/g)`+ TraitData_pojo$`Specific leaf area 2 (cm^2/g)`)/2
###LAP
TraitData_pojo$LAP <- (TraitData_pojo$`Leaf area - perimeter ratio 1`+ TraitData_pojo$`Leaf area - perimeter ratio 2`)/2

TraitData_pojo <- TraitData_pojo[,c(1:5, 8, 13:15)]
TraitData_pojo$Exposure <- "Pojo"

#################################################
###########                ######################
###########    All triats  ######################
###########                ######################
#################################################

All_PCA <- rbind (TraitData_exp, TraitData_semi, TraitData_shel, TraitData_pojo)
All_PCA <- subset(All_PCA,Depth=="S")
#################################################
###########                ######################
###########       PCA      ######################
###########                ######################
#################################################

ord <- prcomp(~ All_PCA$Height +
                All_PCA$RootDepth +
                All_PCA$Ratio +
                All_PCA$SLA 
              , center = TRUE, scale = TRUE, data=All_PCA)

summary(ord) # percent of variance explained by each PC AXIS

data.scores = as.data.frame(ord$x[,1:2])
All_PCA$PC1 <- data.scores$PC1
All_PCA$PC2 <- data.scores$PC2

#################################################
###########                ######################
###########   Plot PCA     ######################
###########                ######################
#################################################
colors <- c("burlywood3","orange2","palegreen","plum1")
library(car)

png(
  "output_plot/PCA plant traits.jpg",
  width = 16,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(1,2))

par(mar = c(3, 5, 2, 3))


plot(ord$x[,1:2], pch=16, 
     cex=0.90, asp=1, ylim=c(-4,4), xlim=c(-4,4),col=colors[as.factor(All_PCA$Exposure)])

legend("topleft", legend = c("Exposed", "Semi-exposed","Sheltered","Pojo bay"), 
       col=c("burlywood3","palegreen","plum1","orange2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

dataEllipse(All_PCA$PC1, All_PCA$PC2, groups = as.factor(All_PCA$Exposure), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)

plot(NULL, xlim=c(-4,4), ylim=c(-4,4), ylab="PC2", xlab="PC1")

dataEllipse(All_PCA$PC1, All_PCA$PC2, groups = as.factor(All_PCA$Exposure), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)

l.x <- ord$rotation[,1]*4
l.y <- ord$rotation[,2]*4

arrows(rep(0,11), rep(0,11), l.x, l.y, len=0.1, lwd=2, col = "red")

text(-2.2, 2.4, expression("Height"))
text(-3.3, -0.9, expression("Root depth"))
text(-2.0, -3.3, expression("R:S ratio"))
text(2.0, -1.2, expression("SLA"))


dev.off()

