
##########################################
######### Exposed ########################
##########################################


##Species max height##
TraitData_exp <- Exposed_data[,c(1:5, 19, 31:35)]
TraitData_exp[, c(5:11)] <- sapply(TraitData_exp[, c(5:11)], as.numeric)
names(TraitData_exp)[5:11]<-paste(c("Height", "MaxRootingDepth", "SLA1", "SLA2",
                                "LAP_Ratio1", "LAP_Ratio2", "SpecificRhizomeLength"))
names(TraitData_exp)[2]<-paste(c("Site_number"))

Heighttrait_exp <- TraitData_exp[,c(1:5)]
Heighttrait_exp <- na.omit(Heighttrait_exp)

#### Species Max values at exposed, exposed deep and exposed shallow
Deep_heighttrait_exp <- Heighttrait_exp[c(1:48),]
Shallow_heighttrait_exp <- Heighttrait_exp[-c(1:48),]
HeightMax_exp <- aggregate(x= Heighttrait_exp$Height, by = list(Heighttrait_exp$Species), FUN = max)
names(HeightMax_exp)[1:2]<-paste(c("Species", "Height"))
HeightMaxDeep_exp <- aggregate(x= Deep_heighttrait_exp$Height, by = list(Deep_heighttrait_exp$Species), FUN = max)
names(HeightMaxDeep_exp)[1:2]<-paste(c("Species", "Height"))
HeightMaxShallow_exp <- aggregate(x= Shallow_heighttrait_exp$Height, by = list(Shallow_heighttrait_exp$Species), FUN = max)
names(HeightMaxShallow_exp)[1:2]<-paste(c("Species", "Height"))
View(HeightMax_exp)
View(HeightMaxDeep_exp)
View(HeightMaxShallow_exp)

### Testing if depths differ significantly
Depth <- c("D",5)
Depth <- c(rep("D",5))
HeightMaxDeep_exp <- cbind(HeightMaxDeep_exp,Depth)
Depth <- c(rep("S",6))
HeightMaxShallow_exp <- cbind(HeightMaxShallow_exp, Depth)
HeightMaxBoth_exp <- rbind(HeightMaxDeep_exp,HeightMaxShallow_exp)
t.test(Height ~Depth, data=HeightMaxBoth_exp)

#### Species Max values at each exposed site and deep and shallow each site

Sites_exp <-aggregate(x=Heighttrait_exp$Height, by=list(Heighttrait_exp$Site_number, Heighttrait_exp$Species), FUN=max)
names(Sites_exp)[1:3]<-paste(c("Site","Species", "Height"))
View(Sites_exp)

##Plotting the site data
library("ggplot2")
ggplot(Sites_exp,aes(x=Species,y=Height,group=Site))+
  geom_point(aes(color=as.factor(Site)))+
  scale_color_manual(values=c('Black','Green','Blue','Red','Orange'))+
  labs(color = "Site")+ggtitle("Max height of species at exposed sites")
ggplot(Sites_exp,aes(x=Site,y=Height,group=Site))+
  geom_point(aes(color=as.factor(Site)))+
  scale_color_manual(values=c('Black','Green','Blue','Red','Orange'))+
  labs(color = "Site")+ggtitle("Max height at exposed sites")
ggplot(Sites_exp,aes(x=Site,y=Height,group=Species))+
  geom_point(aes(color=as.factor(Species)))+
  labs(color = "Species")+ggtitle("Max height of species at exposed sites")

##Testing if sites differ 
model1 <- aov(Height~ Site, data = Sites_exp)
summary(model1)

Sites_depths_exp <-aggregate(x=Heighttrait_exp$Height, by=list(Heighttrait_exp$Site_number, 
                                                               Heighttrait_exp$Depth,Heighttrait_exp$Species), FUN=max)
names(Sites_depths_exp)[1:4]<-paste(c("Site","Depth","Species", "Height"))
View(Sites_depths_exp)

###Plotting the depth data 
library("ggplot2")
ggplot(Sites_depths_exp,aes(x=Site,y=Height,group=Depth))+
  geom_point(aes(color=Depth))+ggtitle("Max height at exposed deep and shallow sites")
ggplot(Sites_depths_exp,aes(x=Species,y=Height,group=Depth))+
  geom_point(aes(color=as.factor(Depth)))+
  labs(color = "Depth")+ggtitle("Species max height at exposed deep and shallow sites")
ggplot(Sites_depths_exp,aes(x=Depth,y=Height,group=Species))+
  geom_point(aes(color=as.factor(Species)))+
  labs(color = "Species")+ggtitle("Species max height at exposed deep and shallow sites")

Sites_deep_exp <- Sites_depths_exp[c(1,5,6,14:16,22,23,29:31),]
Sites_shallow_exp <- Sites_depths_exp[-c(1,5,6,14:16,22,23,29:31),]

View(Sites_deep_exp)
View(Sites_shallow_exp)
##Testing if depths differ from the data that has values for each site and depth
t.test(Height~Depth, data=Sites_depths_exp)
##Testing if sites differ from the data that has values for each site and depth
model3 <-aov(Height~Site, data=Sites_depths_exp)
summary(model3)

##Testing if sites differ inside depths 
model4 <- aov(Height ~ Site, data=Sites_deep_exp)
summary(model4)
model5 <-aov(Height~Site, data=Sites_shallow_exp)
summary(model5)


###########################################
############# SEMI ########################
###########################################

##Species max height##
Heighttrait_semi <- Semi_data[,c(1:5)]
Heighttrait_semi[, c(5)] <- sapply(Heighttrait_semi[, c(5)], as.numeric)
Heighttrait_semi <- na.omit(Heighttrait_semi)
names(Heighttrait_semi)[2]<-paste(c("Site_number"))
names(Heighttrait_semi)[5]<-paste(c("Height"))

#### Species Max values at semi, semi deep and semi shallow
Deep_heighttrait_semi <- Heighttrait_semi[c(1:48),]
Shallow_heighttrait_semi <- Heighttrait_semi[-c(1:48),]
HeightMax_semi <- aggregate(x= Heighttrait_semi$Height, by = list(Heighttrait_semi$Species), FUN = max)
names(HeightMax_semi)[1:2]<-paste(c("Species", "Height"))
HeightMaxDeep_semi <- aggregate(x= Deep_heighttrait_semi$Height, by = list(Deep_heighttrait_semi$Species), FUN = max)
names(HeightMaxDeep_semi)[1:2]<-paste(c("Species", "Height"))
HeightMaxShallow_semi <- aggregate(x= Shallow_heighttrait_semi$Height, by = list(Shallow_heighttrait_semi$Species), FUN = max)
names(HeightMaxShallow_semi)[1:2]<-paste(c("Species", "Height"))
View(HeightMax_semi)
View(HeightMaxDeep_semi)
View(HeightMaxShallow_semi)


### Testing if depths differ significantly

Depth <- c(rep("D",7))
HeightMaxDeep_semi <- cbind(HeightMaxDeep_semi,Depth)
Depth <- c(rep("S",8))
HeightMaxShallow_semi <- cbind(HeightMaxShallow_semi, Depth)
HeightMaxBoth_semi <- rbind(HeightMaxDeep_semi,HeightMaxShallow_semi)
t.test(Height ~Depth, data=HeightMaxBoth_semi)

#### Species Max values at each semi site

Sites_semi <-aggregate(x=Heighttrait_semi$Height, by=list(Heighttrait_semi$Site_number, Heighttrait_semi$Species), FUN=max)
names(Sites_semi)[1:3]<-paste(c("Site","Species", "Height"))
View(Sites_semi)
##Testing if sites differ 
model1 <- aov(Height~ Site, data = Sites_semi)
summary(model1)

##Plotting the site data
library("ggplot2")
ggplot(Sites_semi,aes(x=Species,y=Height,group=Site))+
  geom_point(aes(color=as.factor(Site)))+
  scale_color_manual(values=c('Black','Green','Blue','Red','Orange'))+
  labs(color = "Site")+ggtitle("Max height of species at semi sites")
ggplot(Sites_semi,aes(x=Site,y=Height,group=Site))+
  geom_point(aes(color=as.factor(Site)))+
  scale_color_manual(values=c('Black','Green','Blue','Red','Orange'))+
  labs(color = "Site")+ggtitle("Max height at semi sites")
ggplot(Sites_semi,aes(x=Site,y=Height,group=Species))+
  geom_point(aes(color=as.factor(Species)))+
  labs(color = "Species")+ggtitle("Max height of species at semi sites")

## Species max values at depth and shallow at each site

Sites_depths_semi <-aggregate(x=Heighttrait_semi$Height, by=list(Heighttrait_semi$Site_number, 
                                                               Heighttrait_semi$Depth,Heighttrait_semi$Species), FUN=max)
names(Sites_depths_semi)[1:4]<-paste(c("Site","Depth","Species", "Height"))
View(Sites_depths_semi)
Sites_deep_semi <- Sites_depths_semi[c(1:3,6:10,15:16,24:25,28:30,36:38,42),]
Sites_shallow_semi <- Sites_depths_semi[-c(1:3,6:10,15:16,24:25,28:30,36:38,42),]
View(Sites_deep_semi)
View(Sites_shallow_semi)

###Plotting the depth data 
library("ggplot2")
ggplot(Sites_depths_semi,aes(x=Site,y=Height,group=Depth))+
  geom_point(aes(color=Depth))+ggtitle("Max height at semi deep and shallow sites")
ggplot(Sites_depths_semi,aes(x=Species,y=Height,group=Depth))+
  geom_point(aes(color=as.factor(Depth)))+
  labs(color = "Depth")+ggtitle("Species max height at semi deep and shallow sites")
ggplot(Sites_depths_semi,aes(x=Depth,y=Height,group=Species))+
  geom_point(aes(color=as.factor(Species)))+
  labs(color = "Species")+ggtitle("Species max height at semi deep and shallow sites")


##Testing if depths differ from the data that has values for each site and depth
t.test(Height~Depth,data=Sites_depths_semi)
##Testing if sites differ from the data that has values for each site and depth
model3 <-aov(Height~Site, data=Sites_depths_semi)
summary(model3)

##Testing if sites differ inside depths 
model4 <- aov(Height ~ Site, data=Sites_deep_semi)
summary(model4)
model5 <-aov(Height~Site, data=Sites_shallow_semi)
summary(model5)



#############################################
############# SHELTERED #####################
#############################################

##Species max height##
Heighttrait_sheltered <- Shel_data[,c(1:5)]
Heighttrait_sheltered[, c(5)] <- sapply(Heighttrait_sheltered[, c(5)], as.numeric)
Heighttrait_sheltered <- na.omit(Heighttrait_sheltered)
names(Heighttrait_sheltered)[2]<-paste(c("Site_number"))
names(Heighttrait_sheltered)[5]<-paste(c("Height"))

#### Species Max values at sheltered
HeightMax_sheltered <- aggregate(x= Heighttrait_sheltered$Height, by = list(Heighttrait_sheltered$Species), FUN = max)
names(HeightMax_sheltered)[1:2]<-paste(c("Species", "Height"))
View(HeightMax_sheltered)


#### Species Max values at each semi site

Sites_sheltered <-aggregate(x=Heighttrait_sheltered$Height, by=list(Heighttrait_sheltered$Site_number, Heighttrait_sheltered$Species), FUN=max)
names(Sites_sheltered)[1:3]<-paste(c("Site","Species", "Height"))
View(Sites_sheltered)
##Testing if sites differ 
model1 <- aov(Height~ Site, data = Sites_sheltered)
summary(model1)

##Plotting the site data
library("ggplot2")
ggplot(Sites_sheltered,aes(x=Species,y=Height,group=Site))+
  geom_point(aes(color=as.factor(Site)))+
  scale_color_manual(values=c('Black','Green','Blue','Red','Orange'))+
  labs(color = "Site")+ggtitle("Max height of species at sheltered sites")
ggplot(Sites_sheltered,aes(x=Site,y=Height,group=Site))+
  geom_point(aes(color=as.factor(Site)))+
  scale_color_manual(values=c('Black','Green','Blue','Red','Orange'))+
  labs(color = "Site")+ggtitle("Max height at sheltered sites")
ggplot(Sites_sheltered,aes(x=Site,y=Height,group=Species))+
  geom_point(aes(color=as.factor(Species)))+
  labs(color = "Species")+ggtitle("Max height of species at sheltered sites")


###########################################
############# POJO ########################
###########################################

##Species max height##
Heighttrait_pojo <- Pojo_data[,c(1:5)]
Heighttrait_pojo[, c(5)] <- sapply(Heighttrait_pojo[, c(5)], as.numeric)
Heighttrait_pojo <- na.omit(Heighttrait_pojo)
names(Heighttrait_pojo)[2]<-paste(c("Site_number"))
names(Heighttrait_pojo)[5]<-paste(c("Height"))

#### Species Max values at pojo, pojo deep and pojo shallow
Deep_heighttrait_pojo <- Heighttrait_pojo[c(1:17),]
Shallow_heighttrait_pojo <- Heighttrait_pojo[-c(1:17),]
HeightMax_pojo <- aggregate(x= Heighttrait_pojo$Height, by = list(Heighttrait_pojo$Species), FUN = max)
names(HeightMax_pojo)[1:2]<-paste(c("Species", "Height"))
HeightMaxDeep_pojo <- aggregate(x= Deep_heighttrait_pojo$Height, by = list(Deep_heighttrait_pojo$Species), FUN = max)
names(HeightMaxDeep_pojo)[1:2]<-paste(c("Species", "Height"))
HeightMaxShallow_pojo <- aggregate(x= Shallow_heighttrait_pojo$Height, by = list(Shallow_heighttrait_pojo$Species), FUN = max)
names(HeightMaxShallow_pojo)[1:2]<-paste(c("Species", "Height"))
View(HeightMax_pojo)
View(HeightMaxDeep_pojo)
View(HeightMaxShallow_pojo)


### Testing if depths differ significantly

Depth <- c(rep("D",6))
HeightMaxDeep_pojo <- cbind(HeightMaxDeep_pojo,Depth)
Depth <- c(rep("S",11))
HeightMaxShallow_pojo <- cbind(HeightMaxShallow_pojo, Depth)
HeightMaxBoth_pojo <- rbind(HeightMaxDeep_pojo,HeightMaxShallow_pojo)
t.test(Height ~Depth, data=HeightMaxBoth_pojo)

#### Species Max values at each pojo site

Sites_pojo <-aggregate(x=Heighttrait_pojo$Height, by=list(Heighttrait_pojo$Site_number, Heighttrait_pojo$Species), FUN=max)
names(Sites_pojo)[1:3]<-paste(c("Site","Species", "Height"))
View(Sites_pojo)
##Testing if sites differ 
model1 <- aov(Height~ Site, data = Sites_pojo)
summary(model1)

##Plotting the site data
library("ggplot2")
ggplot(Sites_pojo,aes(x=Species,y=Height,group=Site))+
  geom_point(aes(color=as.factor(Site)))+
  scale_color_manual(values=c('Black','Green','Blue','Red','Orange'))+
  labs(color = "Site")+ggtitle("Max height of species at pojo sites")
ggplot(Sites_pojo,aes(x=Site,y=Height,group=Site))+
  geom_point(aes(color=as.factor(Site)))+
  scale_color_manual(values=c('Black','Green','Blue','Red','Orange'))+
  labs(color = "Site")+ggtitle("Max height at pojo sites")
ggplot(Sites_pojo,aes(x=Site,y=Height,group=Species))+
  geom_point(aes(color=as.factor(Species)))+
  labs(color = "Species")+ggtitle("Max height of species at pojo sites")

## Species max values at depth and shallow at each site

Sites_depths_pojo <-aggregate(x=Heighttrait_pojo$Height, by=list(Heighttrait_pojo$Site_number, 
                                                                 Heighttrait_pojo$Depth,Heighttrait_pojo$Species), FUN=max)
names(Sites_depths_pojo)[1:4]<-paste(c("Site","Depth","Species", "Height"))
View(Sites_depths_pojo)
Sites_deep_pojo <- Sites_depths_pojo[c(1:3,14,22,27,28,31),]
Sites_shallow_pojo <- Sites_depths_pojo[-c(1:3,14,22,27,28,31),]
View(Sites_deep_pojo)
View(Sites_shallow_pojo)

###Plotting the depth data 
library("ggplot2")
ggplot(Sites_depths_pojo,aes(x=Site,y=Height,group=Depth))+
  geom_point(aes(color=Depth))+ggtitle("Max height at pojo deep and shallow sites")
ggplot(Sites_depths_pojo,aes(x=Species,y=Height,group=Depth))+
  geom_point(aes(color=as.factor(Depth)))+
  labs(color = "Depth")+ggtitle("Species max height at pojo deep and shallow sites")
ggplot(Sites_depths_pojo,aes(x=Depth,y=Height,group=Species))+
  geom_point(aes(color=as.factor(Species)))+
  labs(color = "Species")+ggtitle("Species max height at pojo deep and shallow sites")


##Testing if depths differ from the data that has values for each site and depth
t.test(Height~Depth,data=Sites_depths_pojo)
##Testing if sites differ from the data that has values for each site and depth
model3 <-aov(Height~Site, data=Sites_depths_pojo)
summary(model3)

##Testing if sites differ inside depths 
model4 <- aov(Height ~ Site, data=Sites_deep_pojo)
summary(model4)
model5 <-aov(Height~Site, data=Sites_shallow_pojo)
summary(model5)

################################################
########## Max height boxplots ################
###############################################

Heighttrait_all$Site_group <- factor(Heighttrait_all$Site_group, 
                                    levels=c("exposed", "semi","sheltered", "Pojo" ))
boxplot(Height ~ Site_group, data = Heighttrait_all, main="Max height along exposure gradient", 
        xlab=NA, ylab="Max height (cm)", col=c("green","magenta","cyan","blue"))

boxplot(Height ~ Species, data=Heighttrait_all, main= "Maximum height of different species", col = c("lightblue"), 
        ylab="Max height (cm)", xlab="Species", cex.axis=0.7)

##Both depths
Heighttrait_all_deep <- subset(Heighttrait_all, Depth =="D")
boxplot(Height ~ Species, data=Heighttrait_all_deep, main= "Maximum height of different species at deep sites", col = c("lightblue"), 
        ylab="Max height (cm)", xlab="Species", cex.axis=0.7)

Heighttrait_all_shallow <- subset(Heighttrait_all, Depth =="S")
boxplot(Height ~ Species, data=Heighttrait_all_shallow, main= "Maximum height of different species at shallow sites", col = c("lightblue"), 
        ylab="Max height (cm)", xlab="Species", cex.axis=0.7)


## All site groups
boxplot(Height ~ Species, data=Heighttrait_exp, main= "Maximum height of different species at exposed sites", col = c("lightblue"), 
        ylab="Max height (cm)", xlab="Species", cex.axis=1)
boxplot(Height ~ Species, data=Heighttrait_semi, main= "Maximum height of different species at semi-sheltered sites", col = c("lightblue"), 
        ylab="Max height (cm)", xlab="Species", cex.axis=0.8)
boxplot(Height ~ Species, data=Heighttrait_shel, main= "Maximum height of different species at sheltered sites", col = c("lightblue"), 
        ylab="Max height (cm)", xlab="Species", cex.axis=0.8)
boxplot(Height ~ Species, data=Heighttrait_pojo, main= "Maximum height of different species at Pojo Bay sites", col = c("lightblue"), 
        ylab="Max height (cm)", xlab="Species", cex.axis=0.8)

############################################
### Myriophyllum height across gradient ###
############################################

## From all measured values

Heighttrait_exp$Site_group <- c(rep("exposed",136))
Heighttrait_semi$Site_group <- c(rep("semi",118))
Heighttrait_sheltered$Site_group <- c(rep("sheltered",69))
Heighttrait_pojo$Site_group <- c(rep("Pojo",126))
Heighttrait_all <- rbind(Heighttrait_exp, Heighttrait_semi, Heighttrait_sheltered, Heighttrait_pojo)

Heighttrait_all_MYRSPI <- subset(Heighttrait_all,Species=="MYRSPI")
Heighttrait_all_MYRSPI$Site_group <- factor(Heighttrait_all_MYRSPI$Site_group, 
                                           levels=c("exposed", "semi","sheltered", "Pojo" ))
boxplot(Height~ Site_group, data =Heighttrait_all_MYRSPI, main="Max height of Myriophyllum spicatum along exposure gradient",
        xlab="Exposure", ylab="Max height (cm)")

## From site specific max values
Sites_exp$Site_group <- c(rep("exposed",23))
Sites_semi$Site_group <- c(rep("semi",27))
Sites_sheltered$Site_group <- c(rep("sheltered",23))
Sites_pojo$Site_group <- c(rep("Pojo",33))
Sites_all <- rbind(Sites_exp, Sites_semi, Sites_sheltered, Sites_pojo)

Sites_all_MYRSPI <- subset(Sites_all,Species=="MYRSPI")
Sites_all_MYRSPI$Site_group <- factor(Sites_all_MYRSPI$Site_group, 
                                            levels=c("exposed", "semi","sheltered", "Pojo" ))
boxplot(Height~ Site_group, data =Sites_all_MYRSPI, main="Max height of Myriophyllum spicatum along exposure gradient",
        xlab="Exposure", ylab="Max height (cm)")

###########################################################################
##########################                ##################################
##########################  Exploring SLA  #################################
###############################################################################

##Leaftraits matrixes were made in MSc_cwm_traits file

Leaftraits_exp$Site_group <- c(rep("exposed",271))
Leaftraits_semi$Site_group <- c(rep("semi",236))
Leaftraits_shel$Site_group <- c(rep("sheltered",138))
Leaftraits_pojo$Site_group <- c(rep("Pojo",248))
Leaftraits_all <- rbind(Leaftraits_exp, Leaftraits_semi, Leaftraits_shel, Leaftraits_pojo)
Leaftraits_all_deep <- subset(Leaftraits_all, Depth=="D")
Leaftraits_all_shallow <- subset(Leaftraits_all, Depth=="S")
Leaftraits_all$Site_group <- factor(Leaftraits_all$Site_group, 
                                           levels=c("exposed", "semi","sheltered", "Pojo" ))
boxplot(SLA ~ Site_group, data=Leaftraits_all, main="SLA along exposure gradient")
boxplot(SLA ~ Site_group, data = Leaftraits_all_deep, main="SLA along exposure gradient on deep sites")
boxplot(SLA ~ Site_group, data = Leaftraits_all_shallow, main="SLA along exposure gradient on shallow sites")

boxplot(SLA ~ Species, data=Leaftraits_all, main="Specific Leaf Area", xlab="Species", ylab="SLA (cm2/mg)", cex.axis=0.7, col = c("lightblue"), cex.main=2)
boxplot(SLA ~ Species, data=Leaftraits_all_deep, main="Specific Leaf Area in deep sites", xlab="Species", ylab="SLA (cm2/mg)", cex.axis=0.7, col = c("lightblue"), cex.main=2)
boxplot(SLA ~ Species, data=Leaftraits_all_shallow, main="Specific Leaf Area in shallow sites", xlab="Species", ylab="SLA (cm2/mg)", cex.axis=0.7, col = c("lightblue"), cex.main=2)

### Site groups separately

boxplot(SLA ~ Site_number, data=Leaftraits_exp, main= "SLA in exposed sites")
boxplot(SLA ~ Species, data=Leaftraits_exp, main= "SLA in exposed sites", col = c("lightblue"))
                        
boxplot(SLA ~ Site_number, data=Leaftraits_semi, main ="SLA in semi sites")
boxplot(SLA ~ Species, data=Leaftraits_semi, main ="SLA in semi sites", cex.axis=1, col = c("lightblue"), cex.main=2)

boxplot(SLA ~ Site_number, data=Leaftraits_shel, main= "SLA in sheltered sites")
boxplot(SLA ~ Species, data=Leaftraits_shel, main= "SLA in sheltered sites", col = c("lightblue"), cex.main=2)
               
boxplot(SLA ~ Site_number, data=Leaftraits_pojo, main= "SLA in Pojo bay sites")
boxplot(SLA ~ Species, data=Leaftraits_pojo, main= "SLA in Pojo bay sites" , col = c("lightblue"), cex.main=2)

Leaftraits_all_MYRSPI <- subset(Leaftraits_all,Species=="MYRSPI")
Leaftraits_all_MYRSPI$Site_group <- factor(Leaftraits_all_MYRSPI$Site_group, 
                                       levels=c("exposed", "semi","sheltered", "Pojo" ))
boxplot(SLA~ Site_group, data =Leaftraits_all_MYRSPI, main="SLA of Myriophyllum spicatum along exposure gradient")
                        