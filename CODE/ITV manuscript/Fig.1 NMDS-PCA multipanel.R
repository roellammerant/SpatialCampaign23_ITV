##%######################################################%##
#                                                          #
####               Prepare data tait PCA                ####
#                                                          #
##%######################################################%##
library(vegan)
library(car)
library(tidyr)
library(ellipse)
library(imager)

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
All_PCA <- subset(All_PCA <- subset(All_PCA, Depth=="S"))

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

##%######################################################%##
#                                                          #
####                 environmental PCA                  ####
#                                                          #
##%######################################################%##

Site_environmental <- read_excel("DATA/Site environmental.xlsx")

ordB <- prcomp(~ Site_environmental$Salinity +
                Site_environmental$LOI +
                Site_environmental$`Percent Sand` +
                Site_environmental$`Percent Clay` 
              , center = TRUE, scale = TRUE, data=Site_environmental)

summary(ordB) # percent of variance explained by each PC AXIS

data.scores = as.data.frame(ordB$x[,1:2])
data.scores$Exposure <- Site_environmental$Exposure

##%######################################################%##
#                                                          #
####      Prepare data Community composition NMDS       ####
#                                                          #
##%######################################################%##

Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")
Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")
Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")
Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")

Exposed_data <- Spatial_Campaign_data_exp[,c(1:4,29:34)]
Semi_data <-Spatial_Campaign_data_semi[,c(1:4,30:35)]
Shel_data <-Spatial_Campaign_data_shel[,c(1:4,34:39)]
Pojo_data <-Spatial_Campaign_data_pojo[,c(1:4,32:37)]

Exposed_dry_mass <- aggregate(`Total  dry mass (mg)` ~ Site*Species,
                              FUN = sum,  
                              data = Exposed_data)

Semi_dry_mass <- aggregate(`Total  dry mass (mg)` ~ Site*Species,
                           FUN = sum,  
                           data = Semi_data)

Shel_dry_mass <- aggregate(`Total  dry mass (mg)` ~ Site*Species,
                           FUN = sum,  
                           data = Shel_data)

Pojo_dry_mass <- aggregate(`Total  dry mass (mg)` ~ Site*Species,
                           FUN = sum,  
                           data = Pojo_data)



Exposed<- pivot_wider(Exposed_dry_mass, names_from = Species, values_from = `Total  dry mass (mg)`)
Exposed[is.na(Exposed)] <- 0
Exposed$Site_group <- c("Exposed")
Exposed <-Exposed[,c(2:8)]

Semi<- pivot_wider(Semi_dry_mass, names_from = Species, values_from = `Total  dry mass (mg)`)
Semi[is.na(Semi)] <- 0
Semi$Site_group <- c("Semi")
Semi <-Semi[,c(2:11)]

Sheltered<- pivot_wider(Shel_dry_mass, names_from = Species, values_from = `Total  dry mass (mg)`)
Sheltered[is.na(Sheltered)] <- 0
Sheltered$Site_group <- c("Shel")
Sheltered <-Sheltered[,c(2:11)]

Pojo<- pivot_wider(Pojo_dry_mass, names_from = Species, values_from = `Total  dry mass (mg)`)
Pojo[is.na(Pojo)] <- 0
Pojo$Site_group <- c("Pojo")
Pojo <-Pojo[,c(2:14)]

Exposed$CALHER <-0
Exposed$CERDEM <-0
Exposed$Fontinalis <-0
Exposed$LEMTRI <-0
Exposed$MYRSIB <-0
Exposed$NAJMAR <- 0
Exposed$NUPLUT <- 0
Exposed$POTNIT <- 0
Exposed$POTOBT <-0
Exposed$RANBAU <-0
Exposed$RANCIR <-0
Exposed$Sparganium <-0
Exposed <-Exposed[,c(7,2,5,4,6,1,3,8:19)]

Semi$CALHER <-0
Semi$Fontinalis <-0
Semi$LEMTRI <-0
Semi$MYRSIB <-0
Semi$NAJMAR <- 0
Semi$NUPLUT <- 0
Semi$POTNIT <- 0
Semi$POTOBT <-0
Semi$Sparganium <-0
Semi <-Semi[,c(10,3,8,7,9,2,6,11,1,12:18,4,5,19)]

Sheltered$Fontinalis <-0
Sheltered$NUPLUT <- 0
Sheltered$POTNIT <- 0
Sheltered$POTOBT <-0
Sheltered$RANBAU <-0
Sheltered$RUPCIR <-0
Sheltered$Sparganium <-0
Sheltered$ZANMAJ <- 0
Sheltered$ZOSMAR <-0
Sheltered <-Sheltered[,c(10,7,18,9,19,5,16,1,2,11,3,4,6,12,13,14,15,8,17)]

Pojo$POTPER <-0
Pojo$RANBAU <-0
Pojo$RUPCIR <-0
Pojo$STUPEC <-0
Pojo$ZANMAJ <-0
Pojo$ZOSMAR <-0
Pojo <-Pojo[,c(13,14,18,17,19,6,16,1,2,3,4,5,7,8,9,10,15,11,12)]

All_sites_drymass <-rbind(Exposed,Semi)
All_sites_drymass <-rbind(All_sites_drymass,Sheltered)
All_sites_drymass <-rbind(All_sites_drymass,Pojo)

comB = All_sites_drymass[,2:19]
m_comB = as.matrix(comB)
set.seed(123)
nmdsB = metaMDS(m_comB, distance = "bray")
nmdsB
plot(nmdsB)

data.scoresB = as.data.frame(scores(nmdsB)$sites)
data.scoresB$Site_group <- All_sites_drymass$Site_group


##%######################################################%##
#                                                          #
####           Plot 4 panel figure of sites             ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Figure_Intra1(Sites_Species_traits).jpg",
  width = 16,
  height = 11,
  units = 'in',
  res = 600
)

par(mfcol = c(2, 2), mar = c(0.5, 0.5, 0, 0))  

#######################
### FIGURE OF sites ###
#######################

image_data <- load.image("C:/Users/roell/OneDrive/Bureaublad/Github/Coastclim Spatial/Spatial-23-CoastClim/output_plot/Map_sitegroup_colors.png")
plot(image_data, axes = FALSE)

mtext("A", 3, 0.25, adj=0.00, font=2)

#######################
### Species NMDS    ###
#######################
par(mar = c(4, 4, 1.5, 0.5))
colors <- c("burlywood3","orange2","palegreen","plum1")

ordiplot(nmdsB, display = "species", cex = 1.5, xlim = c(-2.3,3.5), ylab="NMDS2", xlab="NMDS1")
dataEllipse(data.scoresB$NMDS1, data.scoresB$NMDS2, groups = as.factor(data.scoresB$Site_group), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)

text(1.064122079, 0.25366121, expression("C. demersum"))
text(0.000725299, -0.930, expression("M. spicatum"))
text(-0.845397347, 0.770, expression("R. cirrhosa"))
text(-0.874367214, -1, expression("R. baudotti"))
text(-1.074251583, 0.364, expression("Z. major"))
text(-0.316448835 , 1, expression("S. pectinata"))
text(-1.8, -0.45444449, expression("Z. marina"))
text(2.15,-0.63773488,expression("L. trisulca"))
text(1.495955489,0.00655396, expression("M. sibiricum"))
text(1.532480937,0.6, expression("N. lutea"))
text(-0.750562000, -0.36740262, expression("P. perfoliatus"))
text(2.27517475, -0.80189288, expression("Fontinalis Sp."))
text(2.35, -0.98839380, expression("Sparganium Sp."))
text( 1.0051941252, -0.65978410, expression("R. circunatus"))
text(3.240411228,  0.75350624, expression("P. nitens"))
text(1.946877033, -1.19134342, expression("P. obtusifolius"))
text(2.082468368, -0.27188921, expression("N. marina"))
text(2.913322221, -0.07091259, expression("C. hermaprhoditica"))

mtext("C", 3, 0.25, adj=0.00, font=2)

#######################
### Environmental   ###
#######################
plot(ordB$x[,1:2], pch=16, col=c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                                "palegreen","palegreen","palegreen","palegreen","palegreen",
                                "plum1","plum1","plum1","plum1","plum1",
                                "orange2","orange2","orange2","orange2","orange2"), 
     cex=0.75,ylim = c(-3,3), xlim = c(-2,4), ylab="PC2 (12.00%)", xlab="PC1 (86.35%)")

dataEllipse(data.scores$PC1, data.scores$PC2, groups = as.factor(data.scores$Exposure), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)

legend("topleft", legend = c("Exposed", "Semi-sheltered","Sheltered","Pojo bay"), 
       col=c("burlywood3","palegreen","plum1","orange2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

mtext("B", 3, 0.25, adj=0.0, font=2)
abline(v=0, h=0, lty=3)

l.x <- ordB$rotation[,1]*1.5
l.y <- ordB$rotation[,2]*1.5

arrows(rep(0,11), rep(0,11), l.x, l.y, len=0.1, lwd=1.25)


text(-0.82, -1.4, expression("Salinity"))
text(0.90, -0.72, expression("LOI"))
text(-0.99, 0.37, expression("Sand %"))
text(1.00, -0.33, expression("Clay %"))

#######################
### Species traits  ###
#######################



plot(ord$x[,1:2], pch=16, 
     cex=0.90, asp=1, ylim=c(-4,4), xlim=c(-4,4),col=colors[as.factor(All_PCA$Exposure)],
     ylab="PC2 (27.79%)", xlab="PC1 (40.35%)")


dataEllipse(All_PCA$PC1, All_PCA$PC2, groups = as.factor(All_PCA$Exposure), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)

l.x <- ord$rotation[,1]*4
l.y <- ord$rotation[,2]*4

arrows(rep(0,11), rep(0,11), l.x, l.y, len=0.1, lwd=2, col = "red")

text(-2.2, 2.4, expression("Height"))
text(-3.3, -0.6, expression("Root depth"))
text(-2.0, -3.3, expression("R:S ratio"))
text(2.0, -0.7, expression("SLA"))

abline(v=0, h=0, lty=3)

mtext("D", 3, 0.25, adj=0.00, font=2)

dev.off()

