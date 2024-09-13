##%######################################################%##
#                                                          #
####               Species level root depth             ####
#                                                          #
##%######################################################%##
Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")
TraitData_exp <- Spatial_Campaign_data_exp[,c(1:4,20)]
names(TraitData_exp)[5]<-paste(c("RootDepth"))
names(TraitData_exp)[2]<-paste(c("Site_number"))
TraitData_exp[is.na(TraitData_exp)] = 0

Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")
TraitData_semi <- Spatial_Campaign_data_semi[,c(1:4,20)]
names(TraitData_semi)[5]<-paste(c("RootDepth"))
names(TraitData_semi)[2]<-paste(c("Site_number"))
TraitData_semi[is.na(TraitData_semi)] = 0


Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")
TraitData_shel <- Spatial_Campaign_data_shel[,c(1:4,20)]
names(TraitData_shel)[5]<-paste(c("RootDepth"))
names(TraitData_shel)[2]<-paste(c("Site_number"))
TraitData_shel[is.na(TraitData_shel)] = 0


Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")
TraitData_pojo <- Spatial_Campaign_data_pojo[,c(1:4,20)]
names(TraitData_pojo)[5]<-paste(c("RootDepth"))
names(TraitData_pojo)[2]<-paste(c("Site_number"))
TraitData_pojo <- TraitData_pojo[-c(1,4,6,7),]
TraitData_pojo[is.na(TraitData_pojo)] = 0

############################################
###         Combine dataframes           ###
############################################

TraitData_exp$Site_group <- c(rep("exposed",136))
TraitData_semi$Site_group <- c(rep("semi",118))
TraitData_shel$Site_group <- c(rep("sheltered",70))
TraitData_pojo$Site_group <- c(rep("Pojo",127))
RootDepth_all <- rbind(TraitData_exp, TraitData_semi, TraitData_shel, TraitData_pojo)
Roottrait_Shallow <- subset(RootDepth_all, Depth=="S")
Roottrait_MYRSPI <- subset(Roottrait_Shallow, Species=="MYRSPI")
Roottrait_STUPEC <- subset(Roottrait_Shallow, Species=="STUPEC")
Roottrait_CERDEM <- subset(Roottrait_Shallow, Species=="CERDEM")
Roottrait_POTPER <- subset(Roottrait_Shallow, Species=="POTPER")
RootS <- rbind(Roottrait_MYRSPI,Roottrait_STUPEC,Roottrait_CERDEM, Roottrait_POTPER)
RootS$Exposure <- factor(paste(RootS$Site_group, 
                               RootS$Species, sep = "_"),
                           levels = c(
                             "exposed_MYRSPI", "exposed_STUPEC","exposed_POTPER",
                             "semi_MYRSPI", "semi_STUPEC","semi_CERDEM", "semi_POTPER",
                             "Pojo_MYRSPI", "Pojo_CERDEM", 
                             "sheltered_MYRSPI", "sheltered_STUPEC", "sheltered_CERDEM", "sheltered_POTPER"
                           ))

png(
  "output_plot/Height_SpeciesLevel.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(1,1))

par(mar = c(3, 5, 2, 3))

##%######################################################%##
#                                                          #
####                    Intra height                    ####
#                                                          #
##%######################################################%##

RootS$Exposure <- factor(RootS$Exposure,
                           levels=c(
                             "exposed_MYRSPI", "exposed_STUPEC","exposed_POTPER",
                             "semi_MYRSPI", "semi_STUPEC", "semi_POTPER", "semi_CERDEM",
                             "sheltered_MYRSPI", "sheltered_STUPEC", "sheltered_POTPER",  "sheltered_CERDEM",
                             "Pojo_MYRSPI", "Pojo_CERDEM"))







par(mar = c(3, 5, 2, 3))
boxplot(RootDepth ~ Exposure, data = RootS,
        boxwex = 0.5, col = c("cyan3","burlywood","grey","cyan3","burlywood","grey","darkorange","cyan3","burlywood","grey","darkorange", "cyan3", "darkorange"),
        main = NA,
        xlab = NA, ylab = "Root depth (cm)",at = c(1, 2, 3, 5, 6, 7,8, 10,11,12,13, 15,16),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(RootDepth ~ Exposure,
           data = RootS,
           method = "jitter",
           at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5, 7.5, 9.5,10.5,11.5,12.5, 14.5,15.5),
           pch = 17,
           col = c("cyan3","burlywood","grey","cyan3","burlywood","grey","darkorange","cyan3","burlywood","grey","darkorange", "cyan3", "darkorange"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")


axis(1, 
     at = seq(1.8 , 16 , 4.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topleft", legend = c(expression(italic("M. spicatum")), expression(italic("S. pectinata")), expression(italic("p. perfoliatus")), expression(italic("C. demersum"))), 
       col=c("cyan3","burlywood","grey","darkorange"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

