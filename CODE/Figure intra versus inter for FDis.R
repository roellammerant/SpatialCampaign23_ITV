##%######################################################%##
#                                                          #
####            Plotting intra versus inter             ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Intra versus Inter FDis.jpg",
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

barplot(as.numeric(c("6.34", "21.44", "-17.46", "10.32","6.70","21.43","-17.65","10.47")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of Height", space = as.numeric(c("0.25","0.25","0.25","0.25", "2.0","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("Category", "Site")

axis(1, 
     at = seq(2.5 , 10 , 6.5), 
     labels = label , 
     tick=FALSE)

legend("top", legend = c("Species turnover", "Intraspecific variation", "Covariation", "Total variation"), 
       col=c("azure2","bisque","aquamarine","lightsalmon"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.2,  horiz = F)

##%######################################################%##  
#                                                          #
####                    Root Depth                      ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("8.32","20.18","-21.62","6.87","8.19", "13.21", "-14.88", "6.51")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of Root depth", space = as.numeric(c("0.25","0.25","0.25","0.25", "2.0","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("Category", "Site")

axis(1, 
     at = seq(2.5 , 10 , 6.5), 
     labels = label , 
     tick=FALSE)

##%######################################################%##
#                                                          #
####                      R-S ratio                     ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("14.12","14.19","-13.90","14.41","15.97", "8.65", "-16.80", "7.81")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of R-S ratio", space = as.numeric(c("0.25","0.25","0.25","0.25", "2.0","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("Category", "Site")

axis(1, 
     at = seq(2.5 , 10 , 6.5), 
     labels = label , 
     tick=FALSE)

##%######################################################%##
#                                                          #
####                        SLA                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("10.30","13.52","-17.88","5.95","10.96", "9.36", "-12.81", "7.52")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of SLA", space = as.numeric(c("0.25","0.25","0.25","0.25", "2.0","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("Category", "Site")

axis(1, 
     at = seq(2.5 , 10 , 6.5), 
     labels = label , 
     tick=FALSE)

##%######################################################%##
#                                                          #
####                        LAP                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("18.19","7.76","-20.00","5.95","13.60", "19.72", "-18.03", "15.29")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of LAP", space = as.numeric(c("0.25","0.25","0.25","0.25", "2.0","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("Category", "Site")

axis(1, 
     at = seq(2.5 , 10 , 6.5), 
     labels = label , 
     tick=FALSE)

dev.off()

