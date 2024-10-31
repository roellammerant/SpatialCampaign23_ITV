###########################
#          Height         #
###########################

Indices_height_SpecificMinusFixedA_Shallow <- subset(Indices_height_SpecificMinusFixedA, Depth=="Shallow")
Indices_height_SpecificMinusFixedA_Shallow$CWM_Height <- as.numeric(Indices_height_SpecificMinusFixedA_Shallow$CWM_Height)
library(plyr)
Indices_height_SpecificMinusFixedA_Shallow$Exposure_Depth <- 
  revalue(Indices_height_SpecificMinusFixedA_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Intra", 
                                                                       "Semi_Shallow" = "Semi_Shallow_Intra",
                                                                       "Sheltered_Shallow" = "Sheltered_Shallow_Intra", 
                                                                       "Pojo_Shallow" = "Pojo_Shallow_Intra"))

Indices_height_SpecificMinusFixedA_Shallow$CWM_Height <- abs(Indices_height_SpecificMinusFixedA_Shallow$CWM_Height)

Height_Intra <- aggregate(Indices_height_SpecificMinusFixedA_Shallow$CWM_Height,
                            by = list(Indices_height_SpecificMinusFixedA_Shallow$Site_Exposure),
                            FUN = function(x) c(mean = mean(x), sd = sd(x),
                                                n = length(x)))
Height_Intra <- do.call(data.frame, Height_Intra) 
Height_Intra$se <- Height_Intra$x.sd / sqrt(Height_Intra$x.n)
colnames(Height_Intra) <- c("Site", "mean", "sd", "n", "se")


#### Environmental variables

Site_environmental <- read_excel("DATA/Site environmental.xlsx")

ord <- prcomp(~ Site_environmental$Salinity +
                Site_environmental$LOI +
                Site_environmental$`Percent Sand` +
                Site_environmental$`Percent Clay` 
              , center = TRUE, scale = TRUE, data=Site_environmental)

summary(ord) # percent of variance explained by each PC AXIS

data.scores = as.data.frame(ord$x[,1:2])
data.scores$Site <- Site_environmental$Site

### Add environmental data
Height_Intra$Environment <- c("-1.62333207","-1.69838978","-1.72817599","-1.70327689","-1.74470137",
                              "-1.47503293","-1.41429651","-1.45208646","-1.54668228","-1.63683175",
                              "2.18665693","1.62674717","2.32420861","3.27077221","2.60230981",
                              "0.06966474","-0.51600985","0.16431899","2.17639904","2.11773837")
Height_Intra$Environment <- as.numeric(Height_Intra$Environment)


#### Plot Height Intra versus exposure gradient

plot(Height_Intra$mean ~ Height_Intra$Environment, las = 1,
     cex.axis = 1.25, cex.lab =1.25, pch = 16,
     ylim = c(0, 150),
     ylab = "Amount of ITV",
     xlab = "Exposure gradient",
     border = "black", axes = TRUE, 
                      col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                              "palegreen","palegreen","palegreen","palegreen","palegreen",
                              "orange2","orange2","orange2","orange2","orange2",
                              "plum1","plum1","plum1","plum1","plum1"))

segments(Height_Intra$Environment, Height_Intra$mean - Height_Intra$se,
         Height_Intra$Environment, Height_Intra$mean + Height_Intra$se)

abline(lm(Height_Intra$mean ~ Height_Intra$Environment),col='black', lty=1, lwd=3) 

legend("topright", legend = c("Exposed", "Semi-exposed","Sheltered","Pojo bay"), 
       col=c("burlywood3","palegreen","plum1","orange2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

cor.test(Height_Intra$mean,Height_Intra$Environment)
x<-lm(Height_Intra$mean ~ Height_Intra$Environment)
summary(x)

###########################
#       RootDepth         #
###########################

Indices_RootDepth_SpecificMinusFixedA_Shallow <- subset(Indices_RootDepth_SpecificMinusFixedB, Depth=="Shallow")
Indices_RootDepth_SpecificMinusFixedA_Shallow$CWM_RootDepth <- as.numeric(Indices_RootDepth_SpecificMinusFixedA_Shallow$CWM_RootDepth)
Indices_RootDepth_SpecificMinusFixedA_Shallow$Exposure_Depth <- 
  revalue(Indices_RootDepth_SpecificMinusFixedA_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Intra", 
                                                                          "Semi_Shallow" = "Semi_Shallow_Intra",
                                                                          "Sheltered_Shallow" = "Sheltered_Shallow_Intra", 
                                                                         "Pojo_Shallow" = "Pojo_Shallow_Intra"))

Indices_RootDepth_SpecificMinusFixedA_Shallow$CWM_RootDepth <- abs(Indices_RootDepth_SpecificMinusFixedA_Shallow$CWM_RootDepth)


RootDepth_Intra <- aggregate(Indices_RootDepth_SpecificMinusFixedA_Shallow$CWM_RootDepth,
                          by = list(Indices_RootDepth_SpecificMinusFixedA_Shallow$Site_Exposure),
                          FUN = function(x) c(mean = mean(x), sd = sd(x),
                                              n = length(x)))
RootDepth_Intra <- do.call(data.frame, RootDepth_Intra) 
RootDepth_Intra$se <- RootDepth_Intra$x.sd / sqrt(RootDepth_Intra$x.n)
colnames(RootDepth_Intra) <- c("Site", "mean", "sd", "n", "se")

### Add environmental data
RootDepth_Intra$Environment <- c("-1.62333207","-1.69838978","-1.72817599","-1.70327689","-1.74470137",
                              "-1.47503293","-1.41429651","-1.45208646","-1.54668228","-1.63683175",
                              "2.18665693","1.62674717","2.32420861","3.27077221","2.60230981",
                              "0.06966474","-0.51600985","0.16431899","2.17639904","2.11773837")
RootDepth_Intra$Environment <- as.numeric(RootDepth_Intra$Environment)


#### Plot RootDepth Intra versus exposure gradient

plot(RootDepth_Intra$mean ~ RootDepth_Intra$Environment, las = 1,
     cex.axis = 1.25, cex.lab =1.25, pch = 16,
     ylim = c(0, 20),
     ylab = "Amount of ITV",
     xlab = "Exposure gradient",
     border = "black", axes = TRUE, 
     col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
             "palegreen","palegreen","palegreen","palegreen","palegreen",
             "orange2","orange2","orange2","orange2","orange2",
             "plum1","plum1","plum1","plum1","plum1"))

segments(RootDepth_Intra$Environment, RootDepth_Intra$mean - RootDepth_Intra$se,
         RootDepth_Intra$Environment, RootDepth_Intra$mean + RootDepth_Intra$se)

cor.test(RootDepth_Intra$mean,RootDepth_Intra$Environment)

mshapiro_test(RootDepth_Intra$mean) # significant

x2<-lm(RootDepth_Intra$mean ~ RootDepth_Intra$Environment)
summary(x2)

###########################
#       R-S ratio         #
###########################

Indices_Ratio_SpecificMinusFixedA_Shallow <- subset(Indices_Ratio_SpecificMinusFixedB, Depth=="Shallow")
Indices_Ratio_SpecificMinusFixedA_Shallow$CWM_Ratio <- as.numeric(Indices_Ratio_SpecificMinusFixedA_Shallow$CWM_Ratio)
Indices_Ratio_SpecificMinusFixedA_Shallow$Exposure_Depth <- 
  revalue(Indices_Ratio_SpecificMinusFixedA_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Intra", 
                                                                      "Semi_Shallow" = "Semi_Shallow_Intra",
                                                                      "Sheltered_Shallow" = "Sheltered_Shallow_Intra", 
                                                                      "Pojo_Shallow" = "Pojo_Shallow_Intra"))
Indices_Ratio_SpecificMinusFixedA_Shallow$CWM_Ratio <- abs(Indices_Ratio_SpecificMinusFixedA_Shallow$CWM_Ratio)
Ratio_Intra <- aggregate(Indices_Ratio_SpecificMinusFixedA_Shallow$CWM_Ratio,
                             by = list(Indices_Ratio_SpecificMinusFixedA_Shallow$Site_Exposure),
                             FUN = function(x) c(mean = mean(x), sd = sd(x),
                                                 n = length(x)))
Ratio_Intra <- do.call(data.frame, Ratio_Intra) 
Ratio_Intra$se <- Ratio_Intra$x.sd / sqrt(Ratio_Intra$x.n)
colnames(Ratio_Intra) <- c("Site", "mean", "sd", "n", "se")

### Add environmental data
Ratio_Intra$Environment <- c("-1.62333207","-1.69838978","-1.72817599","-1.70327689","-1.74470137",
                                 "-1.47503293","-1.41429651","-1.45208646","-1.54668228","-1.63683175",
                                 "2.18665693","1.62674717","2.32420861","3.27077221","2.60230981",
                                 "0.06966474","-0.51600985","0.16431899","2.17639904","2.11773837")
Ratio_Intra$Environment <- as.numeric(Ratio_Intra$Environment)


#### Plot R:S ratio Intra versus exposure gradient

plot(Ratio_Intra$mean ~ Ratio_Intra$Environment, las = 1,
     cex.axis = 1.25, cex.lab =1.25, pch = 16,
     ylim = c(0, 0.60),
     ylab = "Amount of ITV",
     xlab = "Exposure gradient",
     border = "black", axes = TRUE, 
     col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
             "palegreen","palegreen","palegreen","palegreen","palegreen",
             "orange2","orange2","orange2","orange2","orange2",
             "plum1","plum1","plum1","plum1","plum1"))

segments(Ratio_Intra$Environment, Ratio_Intra$mean - Ratio_Intra$se,
         Ratio_Intra$Environment, Ratio_Intra$mean + Ratio_Intra$se)

cor.test(Ratio_Intra$mean,Ratio_Intra$Environment)

mshapiro_test(Ratio_Intra$mean) # significant

x3<-lm(Ratio_Intra$mean ~ Ratio_Intra$Environment)
summary(x3)

###########################
#          SLA            #
###########################

Indices_Leaf_SpecificMinusFixedA_Shallow <- subset(Indices_Leaf_SpecificMinusFixedB, Depth=="Shallow")
Indices_Leaf_SpecificMinusFixedA_Shallow$CWM_SLA <- as.numeric(Indices_Leaf_SpecificMinusFixedA_Shallow$CWM_SLA)
Indices_Leaf_SpecificMinusFixedA_Shallow$Exposure_Depth <- 
  revalue(Indices_Leaf_SpecificMinusFixedA_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Intra", 
                                                                     "Semi_Shallow" = "Semi_Shallow_Intra",
                                                                     "Sheltered_Shallow" = "Sheltered_Shallow_Intra", 
                                                                     "Pojo_Shallow" = "Pojo_Shallow_Intra"))
Indices_Leaf_SpecificMinusFixedA_Shallow$CWM_SLA <- abs(Indices_Leaf_SpecificMinusFixedA_Shallow$CWM_SLA)

SLA_Intra <- aggregate(Indices_Leaf_SpecificMinusFixedA_Shallow$CWM_SLA,
                         by = list(Indices_Leaf_SpecificMinusFixedA_Shallow$Site_Exposure),
                         FUN = function(x) c(mean = mean(x), sd = sd(x),
                                             n = length(x)))
SLA_Intra <- do.call(data.frame, SLA_Intra) 
SLA_Intra$se <- SLA_Intra$x.sd / sqrt(SLA_Intra$x.n)
colnames(SLA_Intra) <- c("Site", "mean", "sd", "n", "se")

### Add environmental data
SLA_Intra$Environment <- c("-1.62333207","-1.69838978","-1.72817599","-1.70327689","-1.74470137",
                             "-1.47503293","-1.41429651","-1.45208646","-1.54668228","-1.63683175",
                             "2.18665693","1.62674717","2.32420861","3.27077221","2.60230981",
                             "0.06966474","-0.51600985","0.16431899","2.17639904","2.11773837")
SLA_Intra$Environment <- as.numeric(SLA_Intra$Environment)


#### Plot SLA Intra versus exposure gradient

plot(SLA_Intra$mean ~ SLA_Intra$Environment, las = 1,
     cex.axis = 1.25, cex.lab =1.25, pch = 16,
     ylim = c(0, 10),
     ylab = "Amount of ITV",
     xlab = "Exposure gradient",
     border = "black", axes = TRUE, 
     col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
             "palegreen","palegreen","palegreen","palegreen","palegreen",
             "orange2","orange2","orange2","orange2","orange2",
             "plum1","plum1","plum1","plum1","plum1"))

segments(SLA_Intra$Environment, SLA_Intra$mean - SLA_Intra$se,
         SLA_Intra$Environment, SLA_Intra$mean + SLA_Intra$se)

cor.test(SLA_Intra$mean,SLA_Intra$Environment)

mshapiro_test(SLA_Intra$mean) # significant

x4<-lm(SLA_Intra$mean ~ SLA_Intra$Environment)
summary(x4)

###########################
#          PC1            #
###########################

Indices_PCA_SpecificMinusFixedB$CWM_PC1 <- as.numeric(Indices_PCA_SpecificMinusFixedB$CWM_PC1)
Indices_PCA_SpecificMinusFixedB$Exposure_Depth <- 
  revalue(Indices_PCA_SpecificMinusFixedB$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Intra", 
                                                            "Semi_Shallow" = "Semi_Shallow_Intra",
                                                            "Sheltered_Shallow" = "Sheltered_Shallow_Intra", 
                                                            "Pojo_Shallow" = "Pojo_Shallow_Intra"))

Indices_PCA_SpecificMinusFixedB$CWM_PC1 <- abs(Indices_PCA_SpecificMinusFixedB$CWM_PC1)

PC1_Intra <- aggregate(Indices_PCA_SpecificMinusFixedB$CWM_PC1 ,
                       by = list(Indices_PCA_SpecificMinusFixedB$Site_Exposure),
                       FUN = function(x) c(mean = mean(x), sd = sd(x),
                                           n = length(x)))
PC1_Intra <- do.call(data.frame, PC1_Intra) 
PC1_Intra$se <- PC1_Intra$x.sd / sqrt(PC1_Intra$x.n)
colnames(PC1_Intra) <- c("Site", "mean", "sd", "n", "se")

### Add environmental data
PC1_Intra$Environment <- c("-1.62333207","-1.69838978","-1.72817599","-1.70327689","-1.74470137",
                           "-1.47503293","-1.41429651","-1.45208646","-1.54668228","-1.63683175",
                           "2.18665693","1.62674717","2.32420861","3.27077221","2.60230981",
                           "0.06966474","-0.51600985","0.16431899","2.17639904","2.11773837")
PC1_Intra$Environment <- as.numeric(PC1_Intra$Environment)


#### Plot PC1 ratio Intra versus exposure gradient

plot(PC1_Intra$mean ~ PC1_Intra$Environment, las = 1,
     cex.axis = 1.25, cex.lab =1.25, pch = 16,
     ylim = c(0, 2),
     ylab = "Amount of ITV",
     xlab = "Exposure gradient",
     border = "black", axes = TRUE, 
     col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
             "palegreen","palegreen","palegreen","palegreen","palegreen",
             "orange2","orange2","orange2","orange2","orange2",
             "plum1","plum1","plum1","plum1","plum1"))

segments(PC1_Intra$Environment, PC1_Intra$mean - PC1_Intra$se,
         PC1_Intra$Environment, PC1_Intra$mean + PC1_Intra$se)

cor.test(PC1_Intra$mean,PC1_Intra$Environment)

###########################
#          PC2            #
###########################

Indices_PCA_SpecificMinusFixedB$CWM_PC2 <- as.numeric(Indices_PCA_SpecificMinusFixedB$CWM_PC2)
Indices_PCA_SpecificMinusFixedB$CWM_PC2 <- abs(Indices_PCA_SpecificMinusFixedB$CWM_PC2)

PC2_Intra <- aggregate(Indices_PCA_SpecificMinusFixedB$CWM_PC2,
                       by = list(Indices_PCA_SpecificMinusFixedB$Site_Exposure),
                       FUN = function(x) c(mean = mean(x), sd = sd(x),
                                           n = length(x)))
PC2_Intra <- do.call(data.frame, PC2_Intra) 
PC2_Intra$se <- PC2_Intra$x.sd / sqrt(PC2_Intra$x.n)
colnames(PC2_Intra) <- c("Site", "mean", "sd", "n", "se")

### Add environmental data
PC2_Intra$Environment <- c("-1.62333207","-1.69838978","-1.72817599","-1.70327689","-1.74470137",
                           "-1.47503293","-1.41429651","-1.45208646","-1.54668228","-1.63683175",
                           "2.18665693","1.62674717","2.32420861","3.27077221","2.60230981",
                           "0.06966474","-0.51600985","0.16431899","2.17639904","2.11773837")
PC2_Intra$Environment <- as.numeric(PC2_Intra$Environment)


#### Plot PC2 ratio Intra versus exposure gradient

plot(PC2_Intra$mean ~ PC2_Intra$Environment, las = 1,
     cex.axis = 1.25, cex.lab =1.25, pch = 16,
     ylim = c(0, 2),
     ylab = "Amount of ITV",
     xlab = "Exposure gradient",
     border = "black", axes = TRUE, 
     col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
             "palegreen","palegreen","palegreen","palegreen","palegreen",
             "orange2","orange2","orange2","orange2","orange2",
             "plum1","plum1","plum1","plum1","plum1"))

segments(PC2_Intra$Environment, PC2_Intra$mean - PC2_Intra$se,
         PC2_Intra$Environment, PC2_Intra$mean + PC2_Intra$se)

cor.test(PC2_Intra$mean,PC2_Intra$Environment)
##%######################################################%##
#                                                          #
####              Overall amount of ITV                 ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Amount ITV versus exposure.jpg",
  width = 20,
  height = 10,
  units = 'in',
  res = 300
)


par(mfcol=c(2,3))

#### Height

par(mar = c(6, 6, 3, 1))

plot(Height_Intra$mean ~ Height_Intra$Environment, las = 1,
     cex.axis = 1.5, cex.lab =1.75, pch = 16, cex = 2,
     ylim = c(0, 150),
     ylab = "Total ITV of Height",
     xlab = "Exposure gradient",
     border = "black", axes = TRUE, 
     col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
             "palegreen","palegreen","palegreen","palegreen","palegreen",
             "orange2","orange2","orange2","orange2","orange2",
             "plum1","plum1","plum1","plum1","plum1"))

segments(Height_Intra$Environment, Height_Intra$mean - Height_Intra$se,
         Height_Intra$Environment, Height_Intra$mean + Height_Intra$se,
         col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                 "palegreen","palegreen","palegreen","palegreen","palegreen",
                 "orange2","orange2","orange2","orange2","orange2",
                 "plum1","plum1","plum1","plum1","plum1"))

abline(lm(Height_Intra$mean ~ Height_Intra$Environment),col='black', lty=1, lwd=2) 

legend("topright", legend = c("Exposed", "Semi-Sheltered","Sheltered","Pojo bay"), 
       col=c("burlywood3","palegreen","plum1","orange2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.75,  horiz = F)

#### Root Depth

plot(RootDepth_Intra$mean ~ RootDepth_Intra$Environment, las = 1,
     cex.axis = 1.5, cex.lab =1.75, pch = 16, cex = 2,
     ylim = c(0, 20),
     ylab = "Total ITV of Root depth",
     xlab = "Exposure gradient",
     border = "black", axes = TRUE, 
     col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
             "palegreen","palegreen","palegreen","palegreen","palegreen",
             "orange2","orange2","orange2","orange2","orange2",
             "plum1","plum1","plum1","plum1","plum1"))

segments(RootDepth_Intra$Environment, RootDepth_Intra$mean - RootDepth_Intra$se,
         RootDepth_Intra$Environment, RootDepth_Intra$mean + RootDepth_Intra$se,
         col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                 "palegreen","palegreen","palegreen","palegreen","palegreen",
                 "orange2","orange2","orange2","orange2","orange2",
                 "plum1","plum1","plum1","plum1","plum1"))

abline(lm(RootDepth_Intra$mean ~ RootDepth_Intra$Environment),col='black', lty=1, lwd=2) 

#### R:S ratio

plot(Ratio_Intra$mean ~ Ratio_Intra$Environment, las = 1,
     cex.axis = 1.5, cex.lab =1.75, pch = 16, cex = 2,
     ylim = c(0, 0.60),
     ylab = "Total ITV of R:S ratio",
     xlab = "Exposure gradient",
     border = "black", axes = TRUE, 
     col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
             "palegreen","palegreen","palegreen","palegreen","palegreen",
             "orange2","orange2","orange2","orange2","orange2",
             "plum1","plum1","plum1","plum1","plum1"))

segments(Ratio_Intra$Environment, Ratio_Intra$mean - Ratio_Intra$se,
         Ratio_Intra$Environment, Ratio_Intra$mean + Ratio_Intra$se,
         col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                 "palegreen","palegreen","palegreen","palegreen","palegreen",
                 "orange2","orange2","orange2","orange2","orange2",
                 "plum1","plum1","plum1","plum1","plum1"))

abline(lm(Ratio_Intra$mean ~ Ratio_Intra$Environment),col='black', lty=1, lwd=2) 

#### SLA

plot(SLA_Intra$mean ~ SLA_Intra$Environment, las = 1,
     cex.axis = 1.5, cex.lab =1.75, pch = 16, cex = 2,
     ylim = c(0, 10),
     ylab = "Total ITV of SLA",
     xlab = "Exposure gradient",
     border = "black", axes = TRUE, 
     col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
             "palegreen","palegreen","palegreen","palegreen","palegreen",
             "orange2","orange2","orange2","orange2","orange2",
             "plum1","plum1","plum1","plum1","plum1"))

segments(SLA_Intra$Environment, SLA_Intra$mean - SLA_Intra$se,
         SLA_Intra$Environment, SLA_Intra$mean + SLA_Intra$se,
         col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                 "palegreen","palegreen","palegreen","palegreen","palegreen",
                 "orange2","orange2","orange2","orange2","orange2",
                 "plum1","plum1","plum1","plum1","plum1"))

abline(lm(SLA_Intra$mean ~ SLA_Intra$Environment),col='lightgrey', lty=2, lwd=2) 

#### PC1
plot(PC1_Intra$mean ~ PC1_Intra$Environment, las = 1,
     cex.axis = 1.5, cex.lab =1.75, pch = 16, cex = 2,
     ylim = c(0, 2),
     ylab = "Total ITV of PC1",
     xlab = "Exposure gradient",
     border = "black", axes = TRUE, 
     col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
             "palegreen","palegreen","palegreen","palegreen","palegreen",
             "orange2","orange2","orange2","orange2","orange2",
             "plum1","plum1","plum1","plum1","plum1"))

segments(PC1_Intra$Environment, PC1_Intra$mean - PC1_Intra$se,
         PC1_Intra$Environment, PC1_Intra$mean + PC1_Intra$se,
         col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                 "palegreen","palegreen","palegreen","palegreen","palegreen",
                 "orange2","orange2","orange2","orange2","orange2",
                 "plum1","plum1","plum1","plum1","plum1"))

abline(lm(PC1_Intra$mean ~ PC1_Intra$Environment),col='lightgrey', lty=2, lwd=2) 

#### PC2

plot(PC2_Intra$mean ~ PC2_Intra$Environment, las = 1,
     cex.axis = 1.5, cex.lab =1.75, pch = 16, cex = 2,
     ylim = c(0, 2),
     ylab = "Total ITV of PC2",
     xlab = "Exposure gradient",
     border = "black", axes = TRUE, 
     col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
             "palegreen","palegreen","palegreen","palegreen","palegreen",
             "orange2","orange2","orange2","orange2","orange2",
             "plum1","plum1","plum1","plum1","plum1"))

segments(PC2_Intra$Environment, PC2_Intra$mean - PC2_Intra$se,
         PC2_Intra$Environment, PC2_Intra$mean + PC2_Intra$se,
         col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                 "palegreen","palegreen","palegreen","palegreen","palegreen",
                 "orange2","orange2","orange2","orange2","orange2",
                 "plum1","plum1","plum1","plum1","plum1"))

abline(lm(PC2_Intra$mean ~ PC2_Intra$Environment),col='lightgrey', lty=2, lwd=2) 


dev.off ()


