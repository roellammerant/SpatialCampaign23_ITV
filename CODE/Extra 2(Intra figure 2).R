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

barplot(as.numeric(c("10.08","129.01","-115.47","23.62")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of Height", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-175,175))


label=c("CWM")

axis(1, 
     at = seq(3 ,6 , 4), 
     labels = label , 
     tick=FALSE)

legend("topright", legend = c("Species turnover", "Intraspecific variation", "Covariation", "Total variation"), 
       col=c("azure2","bisque","aquamarine","lightsalmon"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.2,  horiz = F)

##%######################################################%##  
#                                                          #
####                    Root Depth                      ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("16.54", "20.18", "-5.15", "15.95")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of Root depth", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("CWM")

axis(1, 
     at = seq(3 ,6 , 4), 
     labels = label , 
     tick=FALSE)

##%######################################################%##
#                                                          #
####                      R-S ratio                     ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("25.14", "1.96", "8.54", "35.64")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of R-S ratio", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("CWM", "FDis")

axis(1, 
     at = seq(3 ,6 , 4), 
     labels = label , 
     tick=FALSE)


##%######################################################%##
#                                                          #
####                        SLA                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("12.37", "17.94", "-13.72", "16.58")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of SLA", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("CWM")

axis(1, 
     at = seq(3 ,6 , 4), 
     labels = label , 
     tick=FALSE)

##%######################################################%##
#                                                          #
####                        PC1                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("19.48", "12.88", "-14.94", "17.42")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of PC1", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("CWM")

axis(1, 
     at = seq(3 ,6 , 4), 
     labels = label , 
     tick=FALSE)

##%######################################################%##
#                                                          #
####                        PC2                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("18.57", "22.72", "0.00", "41.45")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of PC2", space = as.numeric(c("0.25","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("CWM")

axis(1, 
     at = seq(3 ,6 , 4), 
     labels = label , 
     tick=FALSE)

dev.off ()

