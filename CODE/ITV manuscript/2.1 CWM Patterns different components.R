###########################
#          Height         #
###########################
Indices_height_Allsites_Shallow <- subset(Indices_height_Allsites, Depth=="Shallow")
Indices_height_Allsites_Shallow$CWM_Height <- as.numeric(Indices_height_Allsites_Shallow$CWM_Height)
library(plyr)
Indices_height_Allsites_Shallow$Exposure_Depth <- 
  revalue(Indices_height_Allsites_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_All", 
                                                            "Semi_Shallow" = "Semi_Shallow_All",
                                                            "Sheltered_Shallow" = "Sheltered_Shallow_All", 
                                                            "Pojo_Shallow" = "Pojo_Shallow_All"))


Indices_height_Fixed_Shallow <- subset(Indices_height_Fixed, Depth=="Shallow")
Indices_height_Fixed_Shallow$CWM_Height <- as.numeric(Indices_height_Fixed_Shallow$CWM_Height)
Indices_height_Fixed_Shallow$Exposure_Depth <- 
  revalue(Indices_height_Fixed_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Turnover", 
                                                         "Semi_Shallow" = "Semi_Shallow_Turnover",
                                                         "Sheltered_Shallow" = "Sheltered_Shallow_Turnover", 
                                                         "Pojo_Shallow" = "Pojo_Shallow_Turnover"))


Indices_height_SpecificMinusFixedA_Shallow <- subset(Indices_height_SpecificMinusFixedA, Depth=="Shallow")
Indices_height_SpecificMinusFixedA_Shallow$CWM_Height <- as.numeric(Indices_height_SpecificMinusFixedA_Shallow$CWM_Height)
Indices_height_SpecificMinusFixedA_Shallow$Exposure_Depth <- 
  revalue(Indices_height_SpecificMinusFixedA_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Intra", 
                                                                       "Semi_Shallow" = "Semi_Shallow_Intra",
                                                                       "Sheltered_Shallow" = "Sheltered_Shallow_Intra", 
                                                                       "Pojo_Shallow" = "Pojo_Shallow_Intra"))

Shallow_All <- rbind(Indices_height_Allsites_Shallow,Indices_height_SpecificMinusFixedA_Shallow,Indices_height_Fixed_Shallow)
Shallow_All$Exposure_Depth <- factor(Shallow_All$Exposure_Depth , # Reorder factor levels
                                     c("Exposed_Shallow_All", "Exposed_Shallow_Turnover", "Exposed_Shallow_Intra",
                                       "Semi_Shallow_All", "Semi_Shallow_Turnover", "Semi_Shallow_Intra", 
                                       "Sheltered_Shallow_All","Sheltered_Shallow_Turnover", "Sheltered_Shallow_Intra",
                                       "Pojo_Shallow_All","Pojo_Shallow_Turnover","Pojo_Shallow_Intra"))

###########################
#      Root depth         #
###########################

Indices_RootDepth_Allsites_Shallow <- subset(Indices_RootDepth_AllsitesB, Depth=="Shallow")
Indices_RootDepth_Allsites_Shallow$CWM_RootDepth <- as.numeric(Indices_RootDepth_Allsites_Shallow$CWM_RootDepth)
library(plyr)
Indices_RootDepth_Allsites_Shallow$Exposure_Depth <- 
  revalue(Indices_RootDepth_Allsites_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_All", 
                                                               "Semi_Shallow" = "Semi_Shallow_All",
                                                               "Sheltered_Shallow" = "Sheltered_Shallow_All", 
                                                               "Pojo_Shallow" = "Pojo_Shallow_All"))


Indices_RootDepth_Fixed_Shallow <- subset(Indices_RootDepth_Fixed, Depth=="Shallow")
Indices_RootDepth_Fixed_Shallow$CWM_RootDepth <- as.numeric(Indices_RootDepth_Fixed_Shallow$CWM_RootDepth)
Indices_RootDepth_Fixed_Shallow$Exposure_Depth <- 
  revalue(Indices_RootDepth_Fixed_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Turnover", 
                                                            "semi_Shallow" = "Semi_Shallow_Turnover",
                                                            "Sheltered_Shallow" = "Sheltered_Shallow_Turnover", 
                                                            "Pojo_Shallow" = "Pojo_Shallow_Turnover"))


Indices_RootDepth_SpecificMinusFixedA_Shallow <- subset(Indices_RootDepth_SpecificMinusFixedB, Depth=="Shallow")
Indices_RootDepth_SpecificMinusFixedA_Shallow$CWM_RootDepth <- as.numeric(Indices_RootDepth_SpecificMinusFixedA_Shallow$CWM_RootDepth)
Indices_RootDepth_SpecificMinusFixedA_Shallow$Exposure_Depth <- 
  revalue(Indices_RootDepth_SpecificMinusFixedA_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Intra", 
                                                                          "Semi_Shallow" = "Semi_Shallow_Intra",
                                                                          "Sheltered_Shallow" = "Sheltered_Shallow_Intra", 
                                                                          "Pojo_Shallow" = "Pojo_Shallow_Intra"))

Shallow_All_RootDepth <- rbind(Indices_RootDepth_Allsites_Shallow,Indices_RootDepth_SpecificMinusFixedA_Shallow,Indices_RootDepth_Fixed_Shallow)
Shallow_All_RootDepth$Exposure_Depth <- factor(Shallow_All_RootDepth$Exposure_Depth , # Reorder factor levels
                                               c("Exposed_Shallow_All", "Exposed_Shallow_Turnover", "Exposed_Shallow_Intra",
                                                 "Semi_Shallow_All", "Semi_Shallow_Turnover", "Semi_Shallow_Intra", 
                                                 "Sheltered_Shallow_All","Sheltered_Shallow_Turnover", "Sheltered_Shallow_Intra",
                                                 "Pojo_Shallow_All","Pojo_Shallow_Turnover","Pojo_Shallow_Intra"))

###########################
#       R:s ratio         #
###########################
Indices_Ratio_Allsites_Shallow <- subset(Indices_Ratio_AllsitesB, Depth=="Shallow")
Indices_Ratio_Allsites_Shallow$CWM_Ratio <- as.numeric(Indices_Ratio_Allsites_Shallow$CWM_Ratio)
Indices_Ratio_Allsites_Shallow$Exposure_Depth <- 
  revalue(Indices_Ratio_Allsites_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_All", 
                                                           "Semi_Shallow" = "Semi_Shallow_All",
                                                           "Sheltered_Shallow" = "Sheltered_Shallow_All", 
                                                           "Pojo_Shallow" = "Pojo_Shallow_All"))


Indices_Ratio_Fixed_Shallow <- subset(Indices_Ratio_Fixed, Depth=="Shallow")
Indices_Ratio_Fixed_Shallow$CWM_Ratio <- as.numeric(Indices_Ratio_Fixed_Shallow$CWM_Ratio)
Indices_Ratio_Fixed_Shallow$Exposure_Depth <- 
  revalue(Indices_Ratio_Fixed_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Turnover", 
                                                        "semi_Shallow" = "Semi_Shallow_Turnover",
                                                        "Sheltered_Shallow" = "Sheltered_Shallow_Turnover", 
                                                        "Pojo_Shallow" = "Pojo_Shallow_Turnover"))


Indices_Ratio_SpecificMinusFixedA_Shallow <- subset(Indices_Ratio_SpecificMinusFixedB, Depth=="Shallow")
Indices_Ratio_SpecificMinusFixedA_Shallow$CWM_Ratio <- as.numeric(Indices_Ratio_SpecificMinusFixedA_Shallow$CWM_Ratio)
Indices_Ratio_SpecificMinusFixedA_Shallow$Exposure_Depth <- 
  revalue(Indices_Ratio_SpecificMinusFixedA_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Intra", 
                                                                      "Semi_Shallow" = "Semi_Shallow_Intra",
                                                                      "Sheltered_Shallow" = "Sheltered_Shallow_Intra", 
                                                                      "Pojo_Shallow" = "Pojo_Shallow_Intra"))

Shallow_All_Ratio <- rbind(Indices_Ratio_Allsites_Shallow,Indices_Ratio_SpecificMinusFixedA_Shallow,Indices_Ratio_Fixed_Shallow)
Shallow_All_Ratio$Exposure_Depth <- factor(Shallow_All_Ratio$Exposure_Depth , # Reorder factor levels
                                           c("Exposed_Shallow_All", "Exposed_Shallow_Turnover", "Exposed_Shallow_Intra",
                                             "Semi_Shallow_All", "Semi_Shallow_Turnover", "Semi_Shallow_Intra", 
                                             "Sheltered_Shallow_All","Sheltered_Shallow_Turnover", "Sheltered_Shallow_Intra",
                                             "Pojo_Shallow_All","Pojo_Shallow_Turnover","Pojo_Shallow_Intra"))

###########################
#          SLA            #
###########################

Indices_Leaf_Allsites_Shallow <- subset(Indices_leaf_AllsitesB, Depth=="Shallow")
Indices_Leaf_Allsites_Shallow$CWM_SLA <- as.numeric(Indices_Leaf_Allsites_Shallow$CWM_SLA)
Indices_Leaf_Allsites_Shallow$Exposure_Depth <- 
  revalue(Indices_Leaf_Allsites_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_All", 
                                                          "Semi_Shallow" = "Semi_Shallow_All",
                                                          "Sheltered_Shallow" = "Sheltered_Shallow_All", 
                                                          "Pojo_Shallow" = "Pojo_Shallow_All"))


Indices_Leaf_Fixed_Shallow <- subset(Indices_Leaf_Fixed, Depth=="Shallow")
Indices_Leaf_Fixed_Shallow$CWM_SLA <- as.numeric(Indices_Leaf_Fixed_Shallow$CWM_SLA)
Indices_Leaf_Fixed_Shallow$Exposure_Depth <- 
  revalue(Indices_Leaf_Fixed_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Turnover", 
                                                       "semi_Shallow" = "Semi_Shallow_Turnover",
                                                       "Sheltered_Shallow" = "Sheltered_Shallow_Turnover", 
                                                       "Pojo_Shallow" = "Pojo_Shallow_Turnover"))


Indices_Leaf_SpecificMinusFixedA_Shallow <- subset(Indices_Leaf_SpecificMinusFixedB, Depth=="Shallow")
Indices_Leaf_SpecificMinusFixedA_Shallow$CWM_SLA <- as.numeric(Indices_Leaf_SpecificMinusFixedA_Shallow$CWM_SLA)
Indices_Leaf_SpecificMinusFixedA_Shallow$Exposure_Depth <- 
  revalue(Indices_Leaf_SpecificMinusFixedA_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Intra", 
                                                                     "Semi_Shallow" = "Semi_Shallow_Intra",
                                                                     "Sheltered_Shallow" = "Sheltered_Shallow_Intra", 
                                                                     "Pojo_Shallow" = "Pojo_Shallow_Intra"))

Shallow_All_Leaf <- rbind(Indices_Leaf_Allsites_Shallow,Indices_Leaf_SpecificMinusFixedA_Shallow,Indices_Leaf_Fixed_Shallow)
Shallow_All_Leaf$Exposure_Depth <- factor(Shallow_All_Leaf$Exposure_Depth , # Reorder factor levels
                                          c("Exposed_Shallow_All", "Exposed_Shallow_Turnover", "Exposed_Shallow_Intra",
                                            "Semi_Shallow_All", "Semi_Shallow_Turnover", "Semi_Shallow_Intra", 
                                            "Sheltered_Shallow_All","Sheltered_Shallow_Turnover", "Sheltered_Shallow_Intra",
                                            "Pojo_Shallow_All","Pojo_Shallow_Turnover","Pojo_Shallow_Intra"))

##%######################################################%##
#                                                          #
####                       PCA                          ####
#                                                          #
##%######################################################%##

Indices_PCA_AllsitesB$CWM_PC1 <- as.numeric(Indices_PCA_AllsitesB$CWM_PC1)
Indices_PCA_AllsitesB$CWM_PC2 <- as.numeric(Indices_PCA_AllsitesB$CWM_PC2)
library(plyr)
Indices_PCA_AllsitesB$Exposure_Depth <- 
  revalue(Indices_PCA_AllsitesB$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_All", 
                                                  "Semi_Shallow" = "Semi_Shallow_All",
                                                  "Sheltered_Shallow" = "Sheltered_Shallow_All", 
                                                  "Pojo_Shallow" = "Pojo_Shallow_All"))

Indices_PCA_Fixed$CWM_PC1 <- as.numeric(Indices_PCA_Fixed$CWM_PC1)
Indices_PCA_Fixed$CWM_PC2 <- as.numeric(Indices_PCA_Fixed$CWM_PC2)
Indices_PCA_Fixed$Exposure_Depth <- 
  revalue(Indices_PCA_Fixed$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Turnover", 
                                              "semi_Shallow" = "Semi_Shallow_Turnover",
                                              "Sheltered_Shallow" = "Sheltered_Shallow_Turnover", 
                                              "Pojo_Shallow" = "Pojo_Shallow_Turnover"))


Indices_PCA_SpecificMinusFixedB$CWM_PC1 <- as.numeric(Indices_PCA_SpecificMinusFixedB$CWM_PC1)
Indices_PCA_SpecificMinusFixedB$CWM_PC2 <- as.numeric(Indices_PCA_SpecificMinusFixedB$CWM_PC2)
Indices_PCA_SpecificMinusFixedB$Exposure_Depth <- 
  revalue(Indices_PCA_SpecificMinusFixedB$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Intra", 
                                                            "Semi_Shallow" = "Semi_Shallow_Intra",
                                                            "Sheltered_Shallow" = "Sheltered_Shallow_Intra", 
                                                            "Pojo_Shallow" = "Pojo_Shallow_Intra"))

Shallow_All_PCA <- rbind(Indices_PCA_AllsitesB,Indices_PCA_SpecificMinusFixedB,Indices_PCA_Fixed)
Shallow_All_PCA$Exposure_Depth <- factor(Shallow_All_PCA$Exposure_Depth , # Reorder factor levels
                                         c("Exposed_Shallow_All", "Exposed_Shallow_Turnover", "Exposed_Shallow_Intra",
                                           "Semi_Shallow_All", "Semi_Shallow_Turnover", "Semi_Shallow_Intra", 
                                           "Sheltered_Shallow_All","Sheltered_Shallow_Turnover", "Sheltered_Shallow_Intra",
                                           "Pojo_Shallow_All","Pojo_Shallow_Turnover","Pojo_Shallow_Intra"))


png(
  "output_plot/MainFigure_CWM_TakePatternsApart.jpg",
  width = 15,
  height = 25,
  units = 'in',
  res = 600
)

par(mfcol=c(6,1))
par(mar = c(3, 5, 2, 3), cex = c(1.4))
boxplot(CWM_Height ~ Exposure_Depth, data = Shallow_All,
        boxwex = 0.5, col = c("lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque"),
        at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15),
        main = NA,
        xlab = NA, ylab = "CWM Height",
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

abline(h=c(0), col=c("blue"), lty=c(2), lwd=c(1))

stripchart(CWM_Height ~ Exposure_Depth,
           data = Shallow_All,
           method = "jitter",
           at = c(0.5,1.5,2.5, 4.5,5.5,6.5, 8.5,9.5,10.5, 12.5,13.5,14.5),
           pch = 17,
           col = c("lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(2 , 15 , 4), 
     labels = label , 
     tick=FALSE , cex=1.2)



boxplot(CWM_RootDepth ~ Exposure_Depth, data = Shallow_All_RootDepth,
        boxwex = 0.5, col = c("lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque"),
        at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15),
        main = NA,
        xlab = NA, ylab = "CWM Root Depth",
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

abline(h=c(0), col=c("blue"), lty=c(2), lwd=c(1))

stripchart(CWM_RootDepth ~ Exposure_Depth,
           data = Shallow_All_RootDepth,
           method = "jitter",
           at = c(0.5,1.5,2.5, 4.5,5.5,6.5, 8.5,9.5,10.5, 12.5,13.5,14.5),
           pch = 17,
           col = c("lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(2 , 15 , 4), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("bottomleft", legend = c("Total variation", "Species turnover", "Intra-specific variation"), 
       col=c("lightsalmon","azure2","bisque"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

boxplot(CWM_Ratio ~ Exposure_Depth, data = Shallow_All_Ratio,
        boxwex = 0.5, col = c("lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque"),
        at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15),
        main = NA,
        xlab = NA, ylab = "CWM R:S ratio",
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

abline(h=c(0), col=c("blue"), lty=c(2), lwd=c(1))

stripchart(CWM_Ratio ~ Exposure_Depth,
           data = Shallow_All_Ratio,
           method = "jitter",
           at = c(0.5,1.5,2.5, 4.5,5.5,6.5, 8.5,9.5,10.5, 12.5,13.5,14.5),
           pch = 17,
           col = c("lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(2 , 15 , 4), 
     labels = label , 
     tick=FALSE , cex=1.2)

boxplot(CWM_SLA ~ Exposure_Depth, data = Shallow_All_Leaf,
        boxwex = 0.5, col = c("lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque"),
        at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15),
        main = NA,
        xlab = NA, ylab = "CWM SLA",
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

abline(h=c(0), col=c("blue"), lty=c(2), lwd=c(1))

stripchart(CWM_SLA ~ Exposure_Depth,
           data = Shallow_All_Leaf,
           method = "jitter",
           at = c(0.5,1.5,2.5, 4.5,5.5,6.5, 8.5,9.5,10.5, 12.5,13.5,14.5),
           pch = 17,
           col = c("lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(2 , 15 , 4), 
     labels = label , 
     tick=FALSE , cex=1.2)

par(mar = c(3, 5, 2, 3))
boxplot(CWM_PC1 ~ Exposure_Depth, data = Shallow_All_PCA,
        boxwex = 0.5, col = c("lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque"),
        at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15),
        main = NA,
        xlab = NA, ylab = "CWM PC1",
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

abline(h=c(0), col=c("blue"), lty=c(2), lwd=c(1))

stripchart(CWM_PC1 ~ Exposure_Depth,
           data = Shallow_All_PCA,
           method = "jitter",
           at = c(0.5,1.5,2.5, 4.5,5.5,6.5, 8.5,9.5,10.5, 12.5,13.5,14.5),
           pch = 17,
           col = c("lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(2 , 15 , 4), 
     labels = label , 
     tick=FALSE , cex=1.2)


boxplot(CWM_PC2 ~ Exposure_Depth, data = Shallow_All_PCA,
        boxwex = 0.5, col = c("lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque"),
        at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15),
        main = NA,
        xlab = NA, ylab = "CWM PC2",
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

abline(h=c(0), col=c("blue"), lty=c(2), lwd=c(1))

stripchart(CWM_PC2 ~ Exposure_Depth,
           data = Shallow_All_PCA,
           method = "jitter",
           at = c(0.5,1.5,2.5, 4.5,5.5,6.5, 8.5,9.5,10.5, 12.5,13.5,14.5),
           pch = 17,
           col = c("lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque","lightsalmon","azure2","bisque"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(2 , 15 , 4), 
     labels = label , 
     tick=FALSE , cex=1.2)

dev.off ()

