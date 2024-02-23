##%######################################################%##
#                                                          #
####            Community indices height                ####
#                                                          #
##%######################################################%##

CategoryLevel_Height<- Indices_height_AllsitesB[c(1:117),]
SiteLevel_Height<- Indices_height_Allsites[c(1:117),]

CategoryLevel_Height$Exposure_Depth <- factor(CategoryLevel_Height$Exposure_Depth, # Reorder factor levels
                        c("Exposed_Shallow", "Exposed_Deep", "Semi_Shallow", "Semi_Deep"))

SiteLevel_Height$Exposure_Depth <- factor(SiteLevel_Height$Exposure_Depth,  # Reorder factor levels
                                          c("Exposed_Shallow", "Exposed_Deep", "Semi_Shallow", "Semi_Deep"))

CategoryLevel_Height$CWM_Height <- as.numeric(CategoryLevel_Height$CWM_Height)
SiteLevel_Height$CWM_Height <- as.numeric(SiteLevel_Height$CWM_Height)
CategoryLevel_Height$FDis_Height <- as.numeric(CategoryLevel_Height$FDis_Height)
SiteLevel_Height$FDis_Height <- as.numeric(SiteLevel_Height$FDis_Height)

plot(CategoryLevel_Height$CWM_Height ~ SiteLevel_Height$CWM_Height, ylim = c(0, 150), xlim = c(0, 150),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(2, 150, "Height", cex = 1.5)
text(2, 138, " r = 0.63", cex = 1.5)
cor.test(CategoryLevel_Height$CWM_Height,SiteLevel_Height$CWM_Height) # 0.62

plot(CategoryLevel_Height$FDis_Height ~ SiteLevel_Height$FDis_Height, ylim = c(0,1.5), xlim = c(0,1.5),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.05, 1.5, "Height", cex = 1.5)
text(0.05, 1.4, " r = 0.78", cex = 1.5)
cor.test(CategoryLevel_Height$FDis_Height,SiteLevel_Height$FDis_Height) # 0.78


##%######################################################%##
#                                                          #
####    Community indices Above-belowground ratio       ####
#                                                          #
##%######################################################%##

CategoryLevel_Ratio<- Indices_Ratio_Allsites[c(1:117),]
SiteLevel_Ratio<- Indices_Ratio_AllsitesB[c(1:117),]

CategoryLevel_Ratio$CWM_Ratio <- as.numeric(CategoryLevel_Ratio$CWM_Ratio)
SiteLevel_Ratio$CWM_Ratio <- as.numeric(SiteLevel_Ratio$CWM_Ratio)
CategoryLevel_Ratio$FDis_Ratio <- as.numeric(CategoryLevel_Ratio$FDis_Ratio)
SiteLevel_Ratio$FDis_Ratio <- as.numeric(SiteLevel_Ratio$FDis_Ratio)

plot(CategoryLevel_Ratio$CWM_Ratio ~ SiteLevel_Ratio$CWM_Ratio, ylim = c(0, 1.5), xlim = c(0, 1.5),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.17, 1.5, "Root to shoot ratio", cex = 1.5)
text(0.05, 1.4, " r = 0.81", cex = 1.5)
cor.test(CategoryLevel_Ratio$CWM_Ratio,SiteLevel_Ratio$CWM_Ratio) # 0.81

plot(CategoryLevel_Ratio$FDis_Ratio ~ SiteLevel_Ratio$FDis_Ratio, ylim = c(0,1.5), xlim = c(0,1.5),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.17, 1.5, "Root to shoot ratio", cex = 1.5)
text(0.05, 1.4, " r = 0.51", cex = 1.5)
cor.test(CategoryLevel_Ratio$FDis_Ratio,SiteLevel_Ratio$FDis_Ratio) # 0.51

##%######################################################%##
#                                                          #
####             Community indices Root depth           ####
#                                                          #
##%######################################################%##

CategoryLevel_RootDepth <- Indices_RootDepth_Allsites[c(1:117),]
SiteLevel_RootDepth <- Indices_RootDepth_AllsitesB[c(1:117),]

CategoryLevel_RootDepth$CWM_RootDepth <- as.numeric(CategoryLevel_RootDepth$CWM_RootDepth)
SiteLevel_RootDepth$CWM_RootDepth <- as.numeric(SiteLevel_RootDepth$CWM_RootDepth)
CategoryLevel_RootDepth$FDis_RootDepth <- as.numeric(CategoryLevel_RootDepth$FDis_RootDepth)
SiteLevel_RootDepth$FDis_RootDepth <- as.numeric(SiteLevel_RootDepth$FDis_RootDepth)

plot(CategoryLevel_RootDepth$CWM_RootDepth ~ SiteLevel_RootDepth$CWM_RootDepth, ylim = c(0, 50), xlim = c(0, 50),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(3, 50, "Root depth", cex = 1.5)
text(1.7, 46, "r = 0.77", cex = 1.5)
cor.test(CategoryLevel_RootDepth$CWM_RootDepth,SiteLevel_RootDepth$CWM_RootDepth) # 0.77

plot(CategoryLevel_RootDepth$FDis_RootDepth ~ SiteLevel_RootDepth$FDis_RootDepth, ylim = c(0,1.5), xlim = c(0,1.5),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.09, 1.5, "Root depth", cex = 1.5)
text(0.05, 1.4, " r = 0.72", cex = 1.5)
cor.test(CategoryLevel_RootDepth$FDis_RootDepth,SiteLevel_RootDepth$FDis_RootDepth) # 0.72

##%######################################################%##
#                                                          #
####              Community indices SLA                 ####
#                                                          #
##%######################################################%##

CategoryLevel_SLA <- Indices_leaf_Allsites[c(1:117),]
SiteLevel_SLA <- Indices_leaf_AllsitesB[c(1:117),]

CategoryLevel_SLA$CWM_SLA <- as.numeric(CategoryLevel_SLA$CWM_SLA)
SiteLevel_SLA$CWM_SLA <- as.numeric(SiteLevel_SLA$CWM_SLA)
CategoryLevel_SLA$FDis_SLA <- as.numeric(CategoryLevel_SLA$FDis_SLA)
SiteLevel_SLA$FDis_SLA <- as.numeric(SiteLevel_SLA$FDis_SLA)

plot(CategoryLevel_SLA$CWM_SLA ~ SiteLevel_SLA$CWM_SLA, ylim = c(0, 1.2), xlim = c(0, 1.2),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.02, 1.2, "SLA", cex = 1.5)
text(0.05, 1.1, " r = 0.25", cex = 1.5)
cor.test(CategoryLevel_SLA$CWM_SLA,SiteLevel_SLA$CWM_SLA) # 0.25

plot(CategoryLevel_SLA$FDis_SLA ~ SiteLevel_SLA$FDis_SLA, ylim = c(0,1.5), xlim = c(0,1.5),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.02, 1.5, "SLA", cex = 1.5)
text(0.05, 1.4, " r = 0.76", cex = 1.5)
cor.test(CategoryLevel_SLA$FDis_SLA,SiteLevel_SLA$FDis_SLA) # 0.76

##%######################################################%##
#                                                          #
####              Community indices LAP                 ####
#                                                          #
##%######################################################%##

CategoryLevel_LAP <- Indices_leaf_Allsites[c(1:117),]
SiteLevel_LAP <- Indices_leaf_AllsitesB[c(1:117),]

CategoryLevel_LAP$CWM_LAP <- as.numeric(CategoryLevel_LAP$CWM_LAP)
SiteLevel_LAP$CWM_LAP <- as.numeric(SiteLevel_LAP$CWM_LAP)
CategoryLevel_LAP$FDis_LAP <- as.numeric(CategoryLevel_LAP$FDis_LAP)
SiteLevel_LAP$FDis_LAP <- as.numeric(SiteLevel_LAP$FDis_LAP)

plot(CategoryLevel_LAP$CWM_LAP ~ SiteLevel_LAP$CWM_LAP, ylim = c(0, 1), xlim = c(0, 1),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.02, 1, "LAP", cex = 1.5)
text(0.04, 0.92, " r = 0.93", cex = 1.5)
cor.test(CategoryLevel_LAP$CWM_LAP,SiteLevel_LAP$CWM_LAP) # 0.93

plot(CategoryLevel_LAP$FDis_LAP ~ SiteLevel_LAP$FDis_LAP, ylim = c(0,1.5), xlim = c(0,1.5),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.02, 1.5, "LAP", cex = 1.5)
text(0.05, 1.4, " r = 0.92", cex = 1.5)
cor.test(CategoryLevel_LAP$FDis_LAP,SiteLevel_LAP$FDis_LAP) # 0.92



##%######################################################%##
#                                                          #
####        Plotting Community weighted mean            ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Category versus site trait values for CWM.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(2,3))

par(mar = c(5, 5, 3, 2))

plot(CategoryLevel_Height$CWM_Height ~ SiteLevel_Height$CWM_Height, ylim = c(0, 150), xlim = c(0, 150),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(8, 150, "Height", cex = 1.4)
text(8, 138, " r = 0.63", cex = 1.4)

plot(CategoryLevel_Ratio$CWM_Ratio ~ SiteLevel_Ratio$CWM_Ratio, ylim = c(0, 1.5), xlim = c(0, 1.5),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.29, 1.5, "Root to shoot ratio", cex = 1.4)
text(0.09, 1.4, " r = 0.81", cex = 1.4)

plot(CategoryLevel_RootDepth$CWM_RootDepth ~ SiteLevel_RootDepth$CWM_RootDepth, ylim = c(0, 50), xlim = c(0, 50),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(4.9, 50, "Root depth", cex = 1.4)
text(3.4, 46, "r = 0.77", cex = 1.4)

plot(CategoryLevel_SLA$CWM_SLA ~ SiteLevel_SLA$CWM_SLA, ylim = c(0, 1.2), xlim = c(0, 1.2),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.04, 1.2, "SLA", cex = 1.4)
text(0.07, 1.1, " r = 0.25", cex = 1.4)

plot(CategoryLevel_LAP$CWM_LAP ~ SiteLevel_LAP$CWM_LAP, ylim = c(0, 1), xlim = c(0, 1),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.04, 1, "LAP", cex = 1.4)
text(0.06, 0.92, " r = 0.93", cex = 1.4)

dev.off()



##%######################################################%##
#                                                          #
####          Plotting Functional dispersion            ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Category versus site trait values for FDis.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(2,3))

par(mar = c(5, 5, 3, 2))

plot(CategoryLevel_Height$FDis_Height ~ SiteLevel_Height$FDis_Height, ylim = c(0,1.5), xlim = c(0,1.5),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.10, 1.5, "Height", cex = 1.5)
text(0.10, 1.4, " r = 0.78", cex = 1.5)

plot(CategoryLevel_Ratio$FDis_Ratio ~ SiteLevel_Ratio$FDis_Ratio, ylim = c(0,1.5), xlim = c(0,1.5),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.29, 1.5, "Root to shoot ratio", cex = 1.5)
text(0.09, 1.4, " r = 0.51", cex = 1.5)

plot(CategoryLevel_RootDepth$FDis_RootDepth ~ SiteLevel_RootDepth$FDis_RootDepth, ylim = c(0,1.5), xlim = c(0,1.5),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.17, 1.5, "Root depth", cex = 1.5)
text(0.11, 1.4, " r = 0.72", cex = 1.5)

plot(CategoryLevel_SLA$FDis_SLA ~ SiteLevel_SLA$FDis_SLA, ylim = c(0,1.5), xlim = c(0,1.5),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.06, 1.5, "SLA", cex = 1.5)
text(0.11, 1.4, " r = 0.76", cex = 1.5)

plot(CategoryLevel_LAP$FDis_LAP ~ SiteLevel_LAP$FDis_LAP, ylim = c(0,1.5), xlim = c(0,1.5),
     ylab = "Trait values at category level", xlab = "Trait values at site level", cex.lab=1.4)
abline(a = 0, b = 1, col = "blue", lwd = 2)
text(0.06, 1.5, "LAP", cex = 1.5)
text(0.11, 1.4, " r = 0.92", cex = 1.5)

dev.off()


