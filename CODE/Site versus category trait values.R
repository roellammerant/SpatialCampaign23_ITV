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

plot(CategoryLevel_Height$CWM_Height ~ SiteLevel_Height$CWM_Height, ylim = c(0, 150), xlim = c(0, 150))
abline(a = 0, b = 1, col = "blue")
cor.test(CategoryLevel_Height$CWM_Height,SiteLevel_Height$CWM_Height) # 0.62

plot(CategoryLevel_Height$FDis_Height ~ SiteLevel_Height$FDis_Height, ylim = c(0,1.5), xlim = c(0,1.5))
abline(a = 0, b = 1, col = "blue")
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

plot(CategoryLevel_Ratio$CWM_Ratio ~ SiteLevel_Ratio$CWM_Ratio, ylim = c(0, 1.5), xlim = c(0, 1.5))
abline(a = 0, b = 1, col = "blue")
cor.test(CategoryLevel_Ratio$CWM_Ratio,SiteLevel_Ratio$CWM_Ratio) # 0.81

plot(CategoryLevel_Ratio$FDis_Ratio ~ SiteLevel_Ratio$FDis_Ratio, ylim = c(0,1.5), xlim = c(0,1.5))
abline(a = 0, b = 1, col = "blue")
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

plot(CategoryLevel_RootDepth$CWM_RootDepth ~ SiteLevel_RootDepth$CWM_RootDepth, ylim = c(0, 50), xlim = c(0, 50))
abline(a = 0, b = 1, col = "blue")
cor.test(CategoryLevel_RootDepth$CWM_RootDepth,SiteLevel_RootDepth$CWM_RootDepth) # 0.77

plot(CategoryLevel_RootDepth$FDis_RootDepth ~ SiteLevel_RootDepth$FDis_RootDepth, ylim = c(0,1.5), xlim = c(0,1.5))
abline(a = 0, b = 1, col = "blue")
cor.test(CategoryLevel_RootDepth$FDis_RootDepth,SiteLevel_RootDepth$FDis_RootDepth) # 0.71

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

plot(CategoryLevel_SLA$CWM_SLA ~ SiteLevel_SLA$CWM_SLA, ylim = c(0, 1.2), xlim = c(0, 1.2))
abline(a = 0, b = 1, col = "blue")
cor.test(CategoryLevel_SLA$CWM_SLA,SiteLevel_SLA$CWM_SLA) # 0.25

plot(CategoryLevel_SLA$FDis_SLA ~ SiteLevel_SLA$FDis_SLA, ylim = c(0,1.5), xlim = c(0,1.5))
abline(a = 0, b = 1, col = "blue")
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

plot(CategoryLevel_LAP$CWM_LAP ~ SiteLevel_LAP$CWM_LAP, ylim = c(0, 1), xlim = c(0, 1))
abline(a = 0, b = 1, col = "blue")
cor.test(CategoryLevel_LAP$CWM_LAP,SiteLevel_LAP$CWM_LAP) # 0.93

plot(CategoryLevel_LAP$FDis_LAP ~ SiteLevel_LAP$FDis_LAP, ylim = c(0,1.5), xlim = c(0,1.5))
abline(a = 0, b = 1, col = "blue")
cor.test(CategoryLevel_LAP$FDis_LAP,SiteLevel_LAP$FDis_LAP) # 0.92




