#%######################################################%##
#                                                          #
####        Plotting Community indices height           ####
#                                                          #
##%######################################################%##

Indices_PCA_AllsitesB$CWM_PC1 <- as.numeric(Indices_PCA_AllsitesB$CWM_PC1)
Indices_PCA_AllsitesB$FDis_PC1 <- as.numeric(Indices_PCA_AllsitesB$FDis_PC1)
Indices_PCA_AllsitesB$CWM_PC2 <- as.numeric(Indices_PCA_AllsitesB$CWM_PC2)
Indices_PCA_AllsitesB$FDis_PC2 <- as.numeric(Indices_PCA_AllsitesB$FDis_PC2)
Indices_PCA_AllsitesB$Exposure_Depth <- factor(Indices_PCA_AllsitesB$Exposure_Depth , # Reorder factor levels
                                               c("Exposed_Shallow","Exposed_Deep",
                                                 "Semi_Shallow", "Semi_Deep", 
                                                 "Sheltered_Shallow",
                                                 "Pojo_Shallow", "Pojo_Deep"))

png(
  "output_plot/PCA_Indices.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(2,2))
par(mar = c(3, 5, 2, 3))

boxplot(CWM_PC1 ~ Exposure_Depth, data = Indices_PCA_AllsitesB,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM PC1",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(CWM_PC1 ~ Exposure_Depth,
           data = Indices_PCA_AllsitesB,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-exposed", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topleft", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)


boxplot(FDis_PC1 ~ Exposure_Depth, data = Indices_PCA_AllsitesB,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis PC1",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(FDis_PC1 ~ Exposure_Depth,
           data = Indices_PCA_AllsitesB,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-exposed", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

boxplot(CWM_PC2 ~ Exposure_Depth, data = Indices_PCA_AllsitesB,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM PC2",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(CWM_PC2 ~ Exposure_Depth,
           data = Indices_PCA_AllsitesB,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-exposed", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)


boxplot(FDis_PC2 ~ Exposure_Depth, data = Indices_PCA_AllsitesB,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis LAP",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(FDis_PC2 ~ Exposure_Depth,
           data = Indices_PCA_AllsitesB,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-exposed", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

dev.off()



