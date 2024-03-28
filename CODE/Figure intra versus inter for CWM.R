##%######################################################%##
#                                                          #
####            Plotting intra versus inter             ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Intra versus Inter CWM.jpg",
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

barplot(as.numeric(c("14.29", "162.19", "-168.47", "8.00","10.08","129.01","-115.47","23.62")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of Height", space = as.numeric(c("0.25","0.25","0.25","0.25", "2.0","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-175,175))

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

barplot(as.numeric(c("16.45","41.05","-39.70","17.80","16.54", "20.18", "-5.15", "15.95")),
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

barplot(as.numeric(c("25.14","18.65","-5.65","38.15","25.14", "1.96", "8.54", "35.64")),
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

barplot(as.numeric(c("2.64","9.12","11.50","23.27","3.83", "55.38", "-42.63", "16.58")),
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

barplot(as.numeric(c("7.60","44.48","-45.56","6.52","3.78", "2.52", "5.43", "11.74")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of LAP", space = as.numeric(c("0.25","0.25","0.25","0.25", "2.0","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("Category", "Site")

axis(1, 
     at = seq(2.5 , 10 , 6.5), 
     labels = label , 
     tick=FALSE)

dev.off()

