library(vegan)
library(car)
library(tidyr)
library(ellipse)

Site_environmental <- read_excel("DATA/Site environmental.xlsx")

ord <- prcomp(~ Site_environmental$Salinity +
                Site_environmental$LOI +
                Site_environmental$`Percent Sand` +
                Site_environmental$`Percent Clay` 
              , center = TRUE, scale = TRUE, data=Site_environmental)

summary(ord) # percent of variance explained by each PC AXIS

data.scores = as.data.frame(ord$x[,1:2])
data.scores$Exposure <- Site_environmental$Exposure

##################################################################
### FIGURE OF ORDINATION
##################################################################
colors <- c("burlywood3","orange2","palegreen","plum1")

par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(ord$x[,1:2], pch=16, col=c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                                "palegreen","palegreen","palegreen","palegreen","palegreen",
                                "plum1","plum1","plum1","plum1","plum1",
                                "orange2","orange2","orange2","orange2","orange2"), 
     cex=0.75, asp=1, ylim = c(-2,2))

dataEllipse(data.scores$PC1, data.scores$PC2, groups = as.factor(data.scores$Exposure), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)

mtext("A", 3, -1.25, adj=0.05, font=2)
abline(v=0, h=0, lty=3)

l.x <- ord$rotation[,1]*1.5
l.y <- ord$rotation[,2]*1.5

arrows(rep(0,11), rep(0,11), l.x, l.y, len=0.1, lwd=1.25)

#### adjusted nmds


#make community matrix - extract columns with site variables
com = Site_environmental[,3:6]
#turn data frame into a matrix
m_com = as.matrix(com)
set.seed(123)
nmds = metaMDS(m_com, distance = "bray")
nmds
par(mfcol=c(1,1))
plot(nmds)

#extract NMDS scores (x and y coordinates)

data.scores = as.data.frame(scores(nmds)$sites)
data.scores$Exposure <- Site_environmental$Exposure



plot(data.scores$NMDS1, data.scores$NMDS2, xlim = c(-0.7,0.7), ylim = c(-0.2,0.2),
     col=colors[as.factor(data.scores$Exposure)], pch = 19, ylab="NMDS2", xlab="NMDS1",)
dataEllipse(data.scores$NMDS1, data.scores$NMDS2, groups = as.factor(data.scores$Exposure), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)

l.x <- nmds$species[,1]*0.9
l.y <- nmds$species[,2]*0.9

arrows(rep(0,11), rep(0,11), l.x, l.y, len=0.1, lwd=1.25)



text(-0.28, -0.08, expression("Salinity"))
text(0.43, 0.017, expression("LOI"))
text(-0.32, 0.08, expression("Sand %"))
text(0.69, -0.006, expression("Clay %"))

















