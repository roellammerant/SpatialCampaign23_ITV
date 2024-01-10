##%######################################################%##
#                                                          #
####               Species level height                 ####
#                                                          #
##%######################################################%##
Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")
TraitData_exp <- Spatial_Campaign_data_exp[,c(1:5)]
names(TraitData_exp)[5]<-paste(c("Height"))
names(TraitData_exp)[2]<-paste(c("Site_number"))

Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")
TraitData_semi <- Spatial_Campaign_data_semi[,c(1:5)]
names(TraitData_semi)[5]<-paste(c("Height"))
names(TraitData_semi)[2]<-paste(c("Site_number"))


Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")
TraitData_shel <- Spatial_Campaign_data_shel[,c(1:5)]
names(TraitData_shel)[5]<-paste(c("Height"))
names(TraitData_shel)[2]<-paste(c("Site_number"))
TraitData_shel[is.na(TraitData_shel)] = 0


Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")
TraitData_pojo <- Spatial_Campaign_data_pojo[,c(1:5)]
names(TraitData_pojo)[5]<-paste(c("Height"))
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
Heighttrait_all <- rbind(TraitData_exp, TraitData_semi, TraitData_shel, TraitData_pojo)

Heighttrait_all$Exposure_Depth <- factor(paste(Heighttrait_all$Site_group, 
                                               Heighttrait_all$Depth, sep = "_"),
                                                 levels = c(
                                                   "exposed_S", "exposed_D",
                                                   "semi_S", "semi_D",
                                                   "Pojo_S", "Pojo_D",
                                                   "sheltered_S"
                                                 ))

png(
  "output_plot/Height_SpeciesLevel.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(2,2))

par(mar = c(3, 5, 2, 3))

##%######################################################%##
#                                                          #
####                    MYRSPI height                   ####
#                                                          #
##%######################################################%##

Heighttrait_all_MYRSPI <- subset(Heighttrait_all,Species=="MYRSPI")
Heighttrait_all_MYRSPI$Exposure_Depth <- factor(Heighttrait_all_MYRSPI$Exposure_Depth, 
                                            levels=c(
                                              "exposed_S", "exposed_D",
                                              "semi_S", "semi_D", 
                                              "sheltered_S",
                                              "Pojo_S"))

par(mar = c(3, 5, 2, 3))
boxplot(Height ~ Exposure_Depth, data = Heighttrait_all_MYRSPI,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque"),
        main = expression(italic("M. spicatum")),
        xlab = NA, ylab = "Height (cm)",at = c(1, 2, 4, 5, 7, 9),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(Height ~ Exposure_Depth,
           data = Heighttrait_all_MYRSPI,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-exposed", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.55 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topleft", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

##%######################################################%##
#                                                          #
####                    STUPEC height                   ####
#                                                          #
##%######################################################%##

Heighttrait_all_STUPEC <- subset(Heighttrait_all,Species=="STUPEC")
Heighttrait_all_STUPEC$Exposure_Depth <- factor(Heighttrait_all_STUPEC$Exposure_Depth, 
                                                levels=c(
                                                  "exposed_S", "exposed_D",
                                                  "semi_S", "semi_D", 
                                                  "sheltered_S"))

par(mar = c(3, 5, 2, 3))
boxplot(Height ~ Exposure_Depth, data = Heighttrait_all_STUPEC,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque"),
        main = expression(italic("S. pectinata")),
        xlab = NA, ylab = "Height (cm)",at = c(1, 2, 4, 5, 7),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(Height ~ Exposure_Depth,
           data = Heighttrait_all_STUPEC,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-exposed", "Sheltered")

axis(1, 
     at = seq(1.5 , 8 , 2.7),
     labels = label , 
     tick=FALSE , cex=1.2)

##%######################################################%##
#                                                          #
####                    RANCIR height                   ####
#                                                          #
##%######################################################%##

Heighttrait_all_RANCIR <- subset(Heighttrait_all,Species=="RANCIR")
Heighttrait_all_RANCIR$Exposure_Depth <- factor(Heighttrait_all_RANCIR$Exposure_Depth, 
                                                levels=c(
                                                  "semi_S", 
                                                  "sheltered_S",
                                                  "Pojo_S", "Pojo_D"))

par(mar = c(3, 5, 2, 3))
boxplot(Height ~ Exposure_Depth, data = Heighttrait_all_RANCIR,
        boxwex = 0.5, col = c("bisque","bisque","bisque","azure2"),
        main = expression(italic("R. circinatus")),
        xlab = NA, ylab = "Height (cm)",at = c(1, 3, 5, 6),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(Height ~ Exposure_Depth,
           data = Heighttrait_all_RANCIR,
           method = "jitter",
           at = c(0.5, 2.5, 4.5, 5.5),
           pch = 17,
           col = c("bisque","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Semi-exposed", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1 , 6 , 2),
     labels = label , 
     tick=FALSE , cex=1.2)

##%######################################################%##
#                                                          #
####                    POTPER height                   ####
#                                                          #
##%######################################################%##

Heighttrait_all_POTPER <- subset(Heighttrait_all,Species=="POTPER")
Heighttrait_all_POTPER$Exposure_Depth <- factor(Heighttrait_all_POTPER$Exposure_Depth, 
                                                levels=c(
                                                  "exposed_S", "exposed_D",
                                                  "semi_S", "semi_D", 
                                                  "sheltered_S"))

par(mar = c(3, 5, 2, 3))
boxplot(Height ~ Exposure_Depth, data = Heighttrait_all_POTPER,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque"),
        main = expression(italic("P. perfoliatus")),
        xlab = NA, ylab = "Height (cm)",at = c(1, 2, 4, 5, 7),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(Height ~ Exposure_Depth,
           data = Heighttrait_all_POTPER,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-exposed", "Sheltered")

axis(1, 
     at = seq(1.5 , 8 , 2.7),
     labels = label , 
     tick=FALSE , cex=1.2)





dev.off()

