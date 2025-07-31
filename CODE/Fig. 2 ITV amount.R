##%######################################################%##
#                                                          #
####            Plotting intra versus inter             ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Intra_MainFigure 2.jpg",
  width = 16,
  height = 8,
  units = 'in',
  res = 600
)
par(mfcol=c(2,3))

par(mar = c(3, 5, 2, 3))

##%######################################################%##
#                                                          #
####                      Height                        ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("23.62", "10.08", "129.01", "-115.47")),
        col = c("lightsalmon","azure2","bisque","aquamarine"),
        ylab = "Explained variance of Height", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 2, ylim=c(-150,150))


legend("bottomleft", legend = c("Total variation", "Species turnover", "Intraspecific variation", "Covariation"), 
       col=c("lightsalmon","azure2","bisque","aquamarine"),
       pch = 15, bty = "n", pt.cex = 3, cex = 2,  horiz = F)

##%######################################################%##  
#                                                          #
####                    Root Depth                      ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("15.95","16.55","50.91","-51.50")),
        col = c("lightsalmon","azure2","bisque","aquamarine"),
        ylab = "Explained variance of Root depth", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 2, ylim=c(-60,60))


##%######################################################%##
#                                                          #
####                      R-S ratio                     ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("35.64","25.15","1.96","8.54")),
        col = c("lightsalmon","azure2","bisque","aquamarine"),
        ylab = "Explained variance of R:S ratio", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 2, ylim=c(-60,60))


##%######################################################%##
#                                                          #
####                        SLA                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("16.58","12.36","17.94","-13.72")),
        col = c("lightsalmon","azure2","bisque","aquamarine"),
        ylab = "Explained variance of SLA", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 2, ylim=c(-60,60))

##%######################################################%##
#                                                          #
####                        PC1                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("17.42","19.48","12.87","-14.94")),
        col = c("lightsalmon","azure2","bisque","aquamarine"),
        ylab = "Explained variance of PC1", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 2, ylim=c(-60,60))

##%######################################################%##
#                                                          #
####                        PC2                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("41.45","39.76","22.72","-21.04")),
        col = c("lightsalmon","azure2","bisque","aquamarine"),
        ylab = "Explained variance of PC2", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 2, ylim=c(-60,60))


dev.off()

