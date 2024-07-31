Indices_RootDepth_Allsites_Shallow <- subset(Indices_RootDepth_AllsitesB, Depth=="Shallow")
Indices_RootDepth_Allsites_Shallow$CWM_RootDepth <- as.numeric(Indices_RootDepth_Allsites_Shallow$CWM_RootDepth)
Indices_RootDepth_Allsites_Shallow$FDis_RootDepth <- as.numeric(Indices_RootDepth_Allsites_Shallow$FDis_RootDepth)
library(plyr)
Indices_RootDepth_Allsites_Shallow$Exposure_Depth <- 
  revalue(Indices_RootDepth_Allsites_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_All", 
                                                            "Semi_Shallow" = "Semi_Shallow_All",
                                                            "Sheltered_Shallow" = "Sheltered_Shallow_All", 
                                                            "Pojo_Shallow" = "Pojo_Shallow_All"))


Indices_RootDepth_Fixed_Shallow <- subset(Indices_RootDepth_Fixed, Depth=="Shallow")
Indices_RootDepth_Fixed_Shallow$CWM_RootDepth <- as.numeric(Indices_RootDepth_Fixed_Shallow$CWM_RootDepth)
Indices_RootDepth_Fixed_Shallow$FDis_RootDepth <- as.numeric(Indices_RootDepth_Fixed_Shallow$FDis_RootDepth)
Indices_RootDepth_Fixed_Shallow$Exposure_Depth <- 
  revalue(Indices_RootDepth_Fixed_Shallow$Exposure_Depth, c("Exposed_Shallow" = "Exposed_Shallow_Turnover", 
                                                         "semi_Shallow" = "Semi_Shallow_Turnover",
                                                         "Sheltered_Shallow" = "Sheltered_Shallow_Turnover", 
                                                         "Pojo_Shallow" = "Pojo_Shallow_Turnover"))


Indices_RootDepth_SpecificMinusFixedA_Shallow <- subset(Indices_RootDepth_SpecificMinusFixedB, Depth=="Shallow")
Indices_RootDepth_SpecificMinusFixedA_Shallow$CWM_RootDepth <- as.numeric(Indices_RootDepth_SpecificMinusFixedA_Shallow$CWM_RootDepth)
Indices_RootDepth_SpecificMinusFixedA_Shallow$FDis_RootDepth <- as.numeric(Indices_RootDepth_SpecificMinusFixedA_Shallow$FDis_RootDepth)
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
png(
  "output_plot/RootDepth_TakePatternsApart.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)

par(mfcol=c(2,1))

par(mar = c(3, 5, 2, 3))
boxplot(CWM_RootDepth ~ Exposure_Depth, data = Shallow_All_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","grey","bisque","azure2","grey","bisque","azure2","grey","bisque","azure2","grey"),
        at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15),
        main = NA,
        xlab = NA, ylab = "CWM RootDepth",
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(CWM_RootDepth ~ Exposure_Depth,
           data = Shallow_All_RootDepth,
           method = "jitter",
           at = c(0.5,1.5,2.5, 4.5,5.5,6.5, 8.5,9.5,10.5, 12.5,13.5,14.5),
           pch = 17,
           col = c("bisque","azure2","grey","bisque","azure2","grey","bisque","azure2","grey","bisque","azure2","grey"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(2 , 15 , 4), 
     labels = label , 
     tick=FALSE , cex=1.2)


boxplot(FDis_RootDepth ~ Exposure_Depth, data = Shallow_All_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","grey","bisque","azure2","grey","bisque","azure2","grey","bisque","azure2","grey"),
        at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15),
        main = NA,
        xlab = NA, ylab = "FDis RootDepth",
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(FDis_RootDepth ~ Exposure_Depth,
           data = Shallow_All_RootDepth,
           method = "jitter",
           at = c(0.5,1.5,2.5, 4.5,5.5,6.5, 8.5,9.5,10.5, 12.5,13.5,14.5),
           pch = 17,
           col = c("bisque","azure2","grey","bisque","azure2","grey","bisque","azure2","grey","bisque","azure2","grey"),
           vertical = TRUE,
           add = TRUE)


label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(2 , 15 , 4), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topleft", legend = c("Total", "Species turnover", "Intra"), 
       col=c("bisque","azure2","grey"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)
dev.off()

