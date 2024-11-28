##%######################################################%##
#                                                          #
####            Plotting intra versus inter             ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Intra Figure 2.jpg",
  width = 16,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(2,3))

par(mar = c(3, 5, 2, 3))

##%######################################################%##
#                                                          #
####                      Height                        ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("10.05", "128.98", "-115.40", "23.64")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of Height", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-150,150))


legend("topright", legend = c("Species turnover", "Intraspecific variation", "Covariation", "Total variation"), 
       col=c("azure2","bisque","aquamarine","lightsalmon"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.2,  horiz = F)

##%######################################################%##  
#                                                          #
####                    Root Depth                      ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("16.55","50.88","-51.50","15.95")),
        col = c("azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of Root depth", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))


##%######################################################%##
#                                                          #
####                      R-S ratio                     ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("25.14","1.96","8.52","35.63")),
        col = c("azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of R-S ratio", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))


##%######################################################%##
#                                                          #
####                        SLA                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("12.34","17.88","-13.60","16.62")),
        col = c("azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of SLA", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

##%######################################################%##
#                                                          #
####                        PC1                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("19.51","12.82","-14.87","17.46")),
        col = c("azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of PC1", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

##%######################################################%##
#                                                          #
####                        PC2                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("39.72","22.66","-20.91","41.47")),
        col = c("azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of PC2", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))


dev.off()

