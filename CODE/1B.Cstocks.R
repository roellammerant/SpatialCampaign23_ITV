##%######################################################%##
#                                                          #
####           Plant Quadrat C stocks per site          ####
#                                                          #
##%######################################################%##

################################
##      Shallow quadrats      ##
################################
Cstock_exp_quad_Shallow$Exposure <-  c(rep("Exposed",31))
Cstock_exp_quad_Shallow$Exposure_Site <- factor(paste(Cstock_exp_quad_Shallow$Exposure, 
                                                      Cstock_exp_quad_Shallow$Site_number, sep = "_"),
                                                 levels = c("Exposed_1", "Exposed_2","Exposed_3","Exposed_4",
                                                   "Exposed_5" ))
Cstock_semi_quad_Shallow$Exposure <-  c(rep("Semi",30))
Cstock_semi_quad_Shallow$Exposure_Site <- factor(paste(Cstock_semi_quad_Shallow$Exposure, 
                                                       Cstock_semi_quad_Shallow$Site_number, sep = "_"),
                                                levels = c("Semi_1", "Semi_2","Semi_3","Semi_4",
                                                           "Semi_5" ))
Cstock_shel_quad_Shallow$Exposure <-  c(rep("Sheltered",30))
Cstock_shel_quad_Shallow$Exposure_Site <- factor(paste(Cstock_shel_quad_Shallow$Exposure, 
                                                       Cstock_shel_quad_Shallow$Site_number, sep = "_"),
                                                levels = c("Sheltered_1", "Sheltered_2","Sheltered_3","Sheltered_4",
                                                           "Sheltered_5" ))
Cstock_pojo_quad_Shallow$Exposure <-  c(rep("Pojo",30))
Cstock_pojo_quad_Shallow$Exposure_Site <- factor(paste(Cstock_pojo_quad_Shallow$Exposure, 
                                                       Cstock_pojo_quad_Shallow$Site_number, sep = "_"),
                                                levels = c("Pojo_1", "Pojo_2","Pojo_3","Pojo_4",
                                                           "Pojo_5" ))
Cstock_all_plant_shallow <-rbind(Cstock_exp_quad_Shallow,Cstock_semi_quad_Shallow,Cstock_shel_quad_Shallow,
                                 Cstock_pojo_quad_Shallow)

################################
##       Deep quadrats        ##
################################
Cstock_exp_quad_Deep$Exposure <-  c(rep("Exposed",26))
Cstock_exp_quad_Deep$Exposure_Site <- factor(paste(Cstock_exp_quad_Deep$Exposure, 
                                                   Cstock_exp_quad_Deep$Site_number, sep = "_"),
                                                levels = c("Exposed_1", "Exposed_2","Exposed_3","Exposed_4"))
Cstock_exp_quad_Deep <- rbind(Cstock_exp_quad_Deep, Cstock_exp_quad_Shallow[c(26:31),])
Cstock_exp_quad_Deep[27:32, 3] = 0
Cstock_exp_quad_Deep[27:32, 1] = c("D5.1", "D5.2", "D5.3", "D5.4", "D5.5", "D5.6")

Cstock_semi_quad_Deep$Exposure <-    c(rep("Semi",30))
Cstock_semi_quad_Deep$Exposure_Site <- factor(paste(Cstock_semi_quad_Deep$Exposure, 
                                                    Cstock_semi_quad_Deep$Site_number, sep = "_"),
                                                 levels = c("Semi_1", "Semi_2","Semi_3","Semi_4",
                                                            "Semi_5" ))
Cstock_shel_quad_Deep <- Cstock_shel_quad_Shallow
Cstock_shel_quad_Deep$Exposure <- c(rep("Sheltered",30))
Cstock_shel_quad_Deep$Cstock_gm2 <- c(rep(0,30))
Cstock_shel_quad_Deep$Exposure_Site <- factor(paste(Cstock_shel_quad_Deep$Exposure, 
                                                    Cstock_shel_quad_Deep$Site_number, sep = "_"),
                                                 levels = c("Sheltered_1", "Sheltered_2","Sheltered_3","Sheltered_4",
                                                            "Sheltered_5" ))
Cstock_shel_quad_Deep[c(1:30), 1] = c("D1.1", "D1.2", "D1.3", "D1.4", "D1.5", "D1.6",
                                            "D2.1", "D2.2", "D2.3", "D2.4", "D2.5", "D2.6",
                                            "D3.1", "D3.2", "D3.3", "D3.4", "D3.5", "D3.6",
                                            "D4.1", "D4.2", "D4.3", "D4.4", "D4.5", "D4.6",
                                            "D5.1", "D5.2", "D5.3", "D5.4", "D5.5", "D5.6")

Cstock_pojo_quad_Deep$Exposure <-  c(rep("Pojo",12))
Cstock_pojo_quad_Deep$Exposure_Site <- factor(paste(Cstock_pojo_quad_Deep$Exposure, 
                                                    Cstock_pojo_quad_Deep$Site_number, sep = "_"),
                                              levels = c("Pojo_2","Pojo_3" ))
Cstock_pojo_quad_Deep <- rbind(Cstock_pojo_quad_Shallow[c(1:6),],Cstock_pojo_quad_Deep, Cstock_pojo_quad_Shallow[c(19:30),])
Cstock_pojo_quad_Deep[c(1:6, 19:30), 3] = 0
Cstock_pojo_quad_Deep[c(1:6, 19:30), 1] = c("D1.1", "D1.2", "D1.3", "D1.4", "D1.5", "D1.6",
                                            "D4.1", "D4.2", "D4.3", "D4.4", "D4.5", "D4.6",
                                            "D5.1", "D5.2", "D5.3", "D5.4", "D5.5", "D5.6")

Cstock_all_plant_deep <-rbind(Cstock_exp_quad_Deep,Cstock_semi_quad_Deep,Cstock_shel_quad_Deep,
                                 Cstock_pojo_quad_Deep)

##%######################################################%##
#                                                          #
####           Algae Quadrat C stocks per site          ####
#                                                          #
##%######################################################%##

################################
##      Shallow quadrats      ##
################################
Cstock_exp_Alg_quad_Shallow$Exposure <-  c(rep("Exposed",31))
Cstock_exp_Alg_quad_Shallow$Exposure_Site <- factor(paste(Cstock_exp_Alg_quad_Shallow$Exposure, 
                                                          Cstock_exp_Alg_quad_Shallow$Site_number, sep = "_"),
                                                levels = c("Exposed_1", "Exposed_2","Exposed_3","Exposed_4",
                                                           "Exposed_5" ))

Cstock_semi_Alg_quad_Shallow$Exposure <-  c(rep("Semi",30))
Cstock_semi_Alg_quad_Shallow$Exposure_Site <- factor(paste(Cstock_semi_Alg_quad_Shallow$Exposure, 
                                                           Cstock_semi_Alg_quad_Shallow$Site_number, sep = "_"),
                                                 levels = c("Semi_1", "Semi_2","Semi_3","Semi_4",
                                                            "Semi_5" ))

Cstock_shel_Alg_quad_Shallow$Exposure <-  c(rep("Sheltered",30))
Cstock_shel_Alg_quad_Shallow$Exposure_Site <- factor(paste(Cstock_shel_Alg_quad_Shallow$Exposure, 
                                                           Cstock_shel_Alg_quad_Shallow$Site_number, sep = "_"),
                                                 levels = c("Sheltered_1", "Sheltered_2","Sheltered_3","Sheltered_4",
                                                            "Sheltered_5" ))

Cstock_pojo_Alg_quad_Shallow$Exposure <-  c(rep("Pojo",30))
Cstock_pojo_Alg_quad_Shallow$Exposure_Site <- factor(paste(Cstock_pojo_Alg_quad_Shallow$Exposure, 
                                                           Cstock_pojo_Alg_quad_Shallow$Site_number, sep = "_"),
                                                 levels = c("Pojo_1", "Pojo_2","Pojo_3","Pojo_4",
                                                            "Pojo_5" ))

Cstock_all_algae_shallow <-rbind(Cstock_exp_Alg_quad_Shallow,Cstock_semi_Alg_quad_Shallow,Cstock_shel_Alg_quad_Shallow,
                                 Cstock_pojo_Alg_quad_Shallow)
################################
##       Deep quadrats        ##
################################
Cstock_exp_Alg_quad_Deep$Exposure <-  c(rep("Exposed",26))
Cstock_exp_Alg_quad_Deep$Exposure_Site <- factor(paste(Cstock_exp_Alg_quad_Deep$Exposure, 
                                                       Cstock_exp_Alg_quad_Deep$Site_number, sep = "_"),
                                             levels = c("Exposed_1", "Exposed_2","Exposed_3","Exposed_4"))
Cstock_exp_Alg_quad_Deep <- rbind(Cstock_exp_Alg_quad_Deep, Cstock_exp_Alg_quad_Shallow[c(26:31),])
Cstock_exp_Alg_quad_Deep[27:32, 3] = 0
Cstock_exp_Alg_quad_Deep[27:32, 1] = c("D5.1", "D5.2", "D5.3", "D5.4", "D5.5", "D5.6")

Cstock_semi_Alg_quad_Deep$Exposure <-   c(rep("Semi",30))
Cstock_semi_Alg_quad_Deep$Exposure_Site <- factor(paste(Cstock_semi_Alg_quad_Deep$Exposure, 
                                                        Cstock_semi_Alg_quad_Deep$Site_number, sep = "_"),
                                              levels = c("Semi_1", "Semi_2","Semi_3","Semi_4",
                                                         "Semi_5" ))

Cstock_shel_Alg_quad_Deep <- Cstock_shel_Alg_quad_Shallow
Cstock_shel_Alg_quad_Deep$Exposure <- c(rep("Sheltered",30))
Cstock_shel_Alg_quad_Deep$Algae_Cstock_gm2 <- c(rep(0,30))
Cstock_shel_Alg_quad_Deep$Exposure_Site <- factor(paste(Cstock_shel_Alg_quad_Deep$Exposure, 
                                                        Cstock_shel_Alg_quad_Deep$Site_number, sep = "_"),
                                              levels = c("Sheltered_1", "Sheltered_2","Sheltered_3","Sheltered_4",
                                                         "Sheltered_5" ))
Cstock_shel_Alg_quad_Deep[c(1:30), 1] = c("D1.1", "D1.2", "D1.3", "D1.4", "D1.5", "D1.6",
                                      "D2.1", "D2.2", "D2.3", "D2.4", "D2.5", "D2.6",
                                      "D3.1", "D3.2", "D3.3", "D3.4", "D3.5", "D3.6",
                                      "D4.1", "D4.2", "D4.3", "D4.4", "D4.5", "D4.6",
                                      "D5.1", "D5.2", "D5.3", "D5.4", "D5.5", "D5.6")


Cstock_pojo_Alg_quad_Deep$Exposure <-  c(rep("Pojo",12))
Cstock_pojo_Alg_quad_Deep$Exposure_Site <- factor(paste(Cstock_pojo_Alg_quad_Deep$Exposure, 
                                                        Cstock_pojo_Alg_quad_Deep$Site_number, sep = "_"),
                                              levels = c("Pojo_2","Pojo_3" ))
Cstock_pojo_Alg_quad_Deep <- rbind(Cstock_pojo_Alg_quad_Shallow[c(1:6),],Cstock_pojo_Alg_quad_Deep, Cstock_pojo_Alg_quad_Shallow[c(19:30),])
Cstock_pojo_Alg_quad_Deep[c(1:6, 19:30), 3] = 0
Cstock_pojo_Alg_quad_Deep[c(1:6, 19:30), 1] = c("D1.1", "D1.2", "D1.3", "D1.4", "D1.5", "D1.6",
                                            "D4.1", "D4.2", "D4.3", "D4.4", "D4.5", "D4.6",
                                            "D5.1", "D5.2", "D5.3", "D5.4", "D5.5", "D5.6")

Cstock_all_algae_deep <-rbind(Cstock_exp_Alg_quad_Deep,Cstock_semi_Alg_quad_Deep,Cstock_shel_Alg_quad_Deep,
                                 Cstock_pojo_Alg_quad_Deep)


##%######################################################%##
#                                                          #
####      Figure Plant Quadrat C stocks per site        ####
#                                                          #
##%######################################################%##

CPlant_Shallow <- aggregate(Cstock_all_plant_shallow$Cstock_gm2,
                    by = list(Cstock_all_plant_shallow$Exposure_Site),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(x)))
CPlant_Shallow <- do.call(data.frame, CPlant_Shallow) 
CPlant_Shallow$se <- CPlant_Shallow$x.sd / sqrt(CPlant_Shallow$x.n)
colnames(CPlant_Shallow) <- c("Site", "mean", "sd", "n", "se")

CPlant_Deep <- aggregate(Cstock_all_plant_deep$Cstock_gm2,
                            by = list(Cstock_all_plant_deep$Exposure_Site),
                            FUN = function(x) c(mean = mean(x), sd = sd(x),
                                                n = length(x)))
CPlant_Deep <- do.call(data.frame, CPlant_Deep) 
CPlant_Deep$se <- CPlant_Deep$x.sd / sqrt(CPlant_Deep$x.n)
colnames(CPlant_Deep) <- c("Site", "mean", "sd", "n", "se")

png(
  "output_plot/SupplCstockPlantSite.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)

par(mfcol=c(2,1))

par(mar = c(3, 5, 2, 3))

barCenters <- barplot(height = CPlant_Shallow$mean,
                      beside = true, las = 2,
                      ylim = c(0, 150),
                      cex.axis = 1.25, cex.lab =1.25, xaxt = "n",
                      ylab = "g C m-2 (1-2m)",
                      border = "black", axes = TRUE, 
                      col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                              "palegreen","palegreen","palegreen","palegreen","palegreen",
                              "plum1","plum1","plum1","plum1","plum1",
                              "orange2","orange2","orange2","orange2","orange2"))

segments(barCenters, CPlant_Shallow$mean - CPlant_Shallow$se, barCenters,
         CPlant_Shallow$mean + CPlant_Shallow$se, lwd = 1.5)
arrows(barCenters, CPlant_Shallow$mean - CPlant_Shallow$se, barCenters,
       CPlant_Shallow$mean + CPlant_Shallow$se, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
legend("topleft", legend = c("Exposed", "Semi-exposed","Sheltered","Pojo bay"), 
       col=c("burlywood3","palegreen","plum1","orange2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

barCenters <- barplot(height = CPlant_Deep$mean,
                      beside = true, las = 2,
                      ylim = c(0, 150),
                      cex.axis = 1.25, cex.lab =1.25, xaxt = "n",
                      ylab = "g C m-2 (3-4m)",
                      border = "black", axes = TRUE, 
                      col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                              "palegreen","palegreen","palegreen","palegreen","palegreen",
                              "plum1","plum1","plum1","plum1","plum1",
                              "orange2","orange2","orange2","orange2","orange2"))

segments(barCenters, CPlant_Deep$mean - CPlant_Deep$se, barCenters,
         CPlant_Deep$mean + CPlant_Deep$se, lwd = 1.5)
arrows(barCenters, CPlant_Deep$mean - CPlant_Deep$se, barCenters,
       CPlant_Deep$mean + CPlant_Deep$se, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

dev.off()

##%######################################################%##
#                                                          #
####      Figure algae Quadrat C stocks per site        ####
#                                                          #
##%######################################################%##

CAlgae_Shallow <- aggregate(Cstock_all_algae_shallow$Algae_Cstock_gm2,
                            by = list(Cstock_all_algae_shallow$Exposure_Site),
                            FUN = function(x) c(mean = mean(x), sd = sd(x),
                                                n = length(x)))
CAlgae_Shallow <- do.call(data.frame, CAlgae_Shallow) 
CAlgae_Shallow$se <- CAlgae_Shallow$x.sd / sqrt(CAlgae_Shallow$x.n)
colnames(CAlgae_Shallow) <- c("Site", "mean", "sd", "n", "se")

CAlgae_Deep <- aggregate(Cstock_all_algae_deep$Algae_Cstock_gm2,
                            by = list(Cstock_all_algae_deep$Exposure_Site),
                            FUN = function(x) c(mean = mean(x), sd = sd(x),
                                                n = length(x)))
CAlgae_Deep <- do.call(data.frame, CAlgae_Deep) 
CAlgae_Deep$se <- CAlgae_Deep$x.sd / sqrt(CAlgae_Deep$x.n)
colnames(CAlgae_Deep) <- c("Site", "mean", "sd", "n", "se")

png(
  "output_plot/SupplCstockAlgaeSite.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)

par(mfcol=c(2,1))

par(mar = c(3, 5, 2, 3))

barCenters <- barplot(height = CAlgae_Shallow$mean,
                      beside = true, las = 2,
                      ylim = c(0, 350),
                      cex.axis = 1.25, cex.lab =1.25, xaxt = "n",
                      ylab = "g C m-2 (1-2m)",
                      border = "black", axes = TRUE, 
                      col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                              "palegreen","palegreen","palegreen","palegreen","palegreen",
                              "plum1","plum1","plum1","plum1","plum1",
                              "orange2","orange2","orange2","orange2","orange2"))

segments(barCenters, CAlgae_Shallow$mean - CAlgae_Shallow$se, barCenters,
         CAlgae_Shallow$mean + CAlgae_Shallow$se, lwd = 1.5)
arrows(barCenters, CAlgae_Shallow$mean - CAlgae_Shallow$se, barCenters,
       CAlgae_Shallow$mean + CAlgae_Shallow$se, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
legend("topleft", legend = c("Exposed", "Semi-exposed","Sheltered","Pojo bay"), 
       col=c("burlywood3","palegreen","plum1","orange2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

barCenters <- barplot(height = CAlgae_Deep$mean,
                      beside = true, las = 2,
                      ylim = c(0, 350),
                      cex.axis = 1.25, cex.lab =1.25, xaxt = "n",
                      ylab = "g C m-2 (3-4m)",
                      border = "black", axes = TRUE, 
                      col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                              "palegreen","palegreen","palegreen","palegreen","palegreen",
                              "plum1","plum1","plum1","plum1","plum1",
                              "orange2","orange2","orange2","orange2","orange2"))

segments(barCenters, CAlgae_Deep$mean - CAlgae_Deep$se, barCenters,
         CAlgae_Deep$mean + CAlgae_Deep$se, lwd = 1.5)
arrows(barCenters, CAlgae_Deep$mean - CAlgae_Deep$se , barCenters,
       CAlgae_Deep$mean + CAlgae_Deep$se , lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

dev.off()

