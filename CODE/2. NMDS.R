##%######################################################%##
#                                                          #
####          Prepare data environmental NMDS           ####
#                                                          #
##%######################################################%##
library(vegan)
library(car)
library(tidyr)
library(ellipse)

Site_environmental <- read_excel("DATA/Site environmental.xlsx")

#make community matrix - extract columns with site variables
com = Site_environmental[,3:6]
#turn data frame into a matrix
m_com = as.matrix(com)
set.seed(123)
nmds = metaMDS(m_com, distance = "bray")
nmds
par(mfcol=c(1,1))
plot(nmds)

#extract NMDS scores (x and y coordinates)

data.scores = as.data.frame(scores(nmds)$sites)
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
####                Plot 4 panel NMDS                   ####
#                                                          #
##%######################################################%##

png(
  "output_plot/NMDSAllNames.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)

par(mfcol=c(2,2))

par(mar = c(4, 4, 3, 3))

###########################################
#####  NMDS Environmental variables  ######
###########################################

colors <- c("burlywood3","orange2","palegreen","plum1")
ordiplot(nmds, display = "species", cex = 1.5,xlim = c(-0.7,0.7), ylim = c(-0.2,0.2), , ylab="NMDS2", xlab="NMDS1")
dataEllipse(data.scores$NMDS1, data.scores$NMDS2, groups = as.factor(data.scores$Exposure), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)

text(-0.2567360, -0.12, expression("Salinity"))
text(0.37, 0.018, expression("LOI"))
text(-0.29, 0.13, expression("Sand %"))
text(0.695, 0.026, expression("Clay %"))

legend("topleft",
       legend = unique(data.scores$Exposure),
       pch = 19,
       col = c("burlywood3","palegreen","plum1","orange2"))


plot(data.scores$NMDS1, data.scores$NMDS2, xlim = c(-0.7,0.7), ylim = c(-0.2,0.2),
     col=colors[as.factor(data.scores$Exposure)], pch = 19, ylab="NMDS2", xlab="NMDS1",)
dataEllipse(data.scores$NMDS1, data.scores$NMDS2, groups = as.factor(data.scores$Exposure), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)


###########################################
#####  NMDS community composition    ######
###########################################

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

plot(data.scoresB$NMDS1, data.scoresB$NMDS2,  col = colors[as.factor(data.scoresB$Site_group)],xlim=c(-2,3), ylab="NMDS2", xlab="NMDS1", pch = 19)
dataEllipse(data.scoresB$NMDS1, data.scoresB$NMDS2, groups = as.factor(data.scoresB$Site_group), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)

dev.off()

