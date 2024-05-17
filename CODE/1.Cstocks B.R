##%######################################################%##
#                                                          #
####             Exposed plant Cstocks                  ####
#                                                          #
##%######################################################%##

Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")
Exposed_data <- Spatial_Campaign_data_exp[,c(1:4,29:33)]

#######################################                          
##Cstocks plants based on C/N Samples##
#######################################

C_N_Spatial <- read_excel("DATA/C-N.xlsx")
C_N_Exposed <- C_N_Spatial[ which(C_N_Spatial$Exposure=='Exposed'), ]
LEAF_Exp <- C_N_Exposed[ which(C_N_Exposed$Tissue=='Leaf'), ]
ROOT_Exp <- C_N_Exposed[ which(C_N_Exposed$Tissue=='Root'), ]
RHI_Exp <- C_N_Exposed[ which(C_N_Exposed$Tissue=='Rhi'), ]


### Mean carbon values of tissues
LEAF_C <- aggregate(x= LEAF_Exp$`C (µg/mg)`, by = list(LEAF_Exp$Species), FUN = median)
names(LEAF_C)[1:2]<-paste(c("Species", "LEAF_C"))
ROOT_C <- aggregate(x= ROOT_Exp$`C (µg/mg)`, by = list(ROOT_Exp$Species), FUN = median)
names(ROOT_C)[1:2]<-paste(c("Species", "ROOT_C"))
RHI_C <- aggregate(x= RHI_Exp$`C (µg/mg)`, by = list(RHI_Exp$Species), FUN = median)
names(RHI_C)[1:2]<-paste(c("Species", "RHI_C"))

### combine biomass and species carbon values
Exposed_C<- merge(Exposed_data,LEAF_C,by="Species",  all = TRUE)
Exposed_C<- merge(Exposed_C,ROOT_C,by="Species",  all = TRUE)
Exposed_C<- merge(Exposed_C,RHI_C,by="Species",  all = TRUE)

### get total carbon stock for each species in grams in a quadrat
Exposed_C$CarbonStockRoot <- (Exposed_C$`Total root dry mass (mg)`*(Exposed_C$ROOT_C/1000))/1000
Exposed_C$CarbonStockRhi <- (Exposed_C$`Total rhizome dry mass (mg)`*(Exposed_C$RHI_C/1000))/1000
Exposed_C$CarbonStockLeaf <- (Exposed_C$`Total aboveground dry mass (mg)`*(Exposed_C$LEAF_C/1000))/1000
Exposed_C[is.na(Exposed_C)] <- 0
Exposed_C$TotalC <- Exposed_C$CarbonStockLeaf + Exposed_C$CarbonStockRoot + Exposed_C$CarbonStockRhi

### Total  Cstock per quadrat
Plot_Abun_C <- with(Exposed_C, tapply(TotalC, list(Site, Species), sum))
Plot_Abun_C[is.na(Plot_Abun_C)] = 0
ExposedPlantCstock <- as.data.frame (rowSums(Plot_Abun_C))
setDT(ExposedPlantCstock, keep.rownames = "Plot")
names(ExposedPlantCstock)[2]<-paste("CarbonStock")
ExposedPlantCstock$Category <- c("Exposed")
ExposedPlantCstock$site <- c("D_1","D_1","D_1","D_1","D_1","D_1","D_1","D_1",
                                 "D_2","D_2","D_2","D_2","D_2","D_2",
                                 "D_3","D_3","D_3","D_3","D_3","D_3",
                                 "D_4","D_4","D_4","D_4","D_4","D_4",
                                 "S_1","S_1","S_1","S_1","S_1","S_1","S_1","S_1",
                                 "S_2","S_2","S_2","S_2","S_2",
                                 "S_3","S_3","S_3","S_3","S_3","S_3",
                                 "S_4","S_4","S_4","S_4","S_4","S_4",
                                 "S_5","S_5","S_5","S_5","S_5","S_5")

### total Cstock per square meter
ExposedPlantCstock$CarbonStock <- ExposedPlantCstock$CarbonStock/(0.27*0.27)

ExposedPlantCstockB <- aggregate(x=ExposedPlantCstock$CarbonStock, by=list(ExposedPlantCstock$site), FUN=mean)
colnames(ExposedPlantCstockB)<-c("Site_number","Cstock_gm2")
Site_group = c("exposed", "exposed", "exposed","exposed","exposed", "exposed", "exposed","exposed", "exposed")
ExposedPlantCstockB <- cbind(ExposedPlantCstockB,Site_group)

Cstock_exp_5 <- data.frame(Site_number=c("D_5"),Cstock_gm2=c(0),Site_group=c("exposed"))
ExposedPlantCstockB <-rbind(ExposedPlantCstockB,Cstock_exp_5)
ExposedPlantCstockB$Depth <- c("Deep","Deep","Deep","Deep","Shallow",
                               "Shallow","Shallow","Shallow","Shallow","Deep")


##%######################################################%##
#                                                          #
####             Exposed algae Cstocks                  ####
#                                                          #
##%######################################################%##
# get total carbon stock for each species in grams in a quadrat
Exposed_Algae <- Spatial_Campaign_data_exp[,c(1:3,25:26)]
Exposed_Algae[is.na(Exposed_Algae)] = 0
Exposed_Algae$`Tolypella (mg)` <- (Exposed_Algae$`Tolypella (mg)` * 0.38)/1000
Exposed_Algae$`Chorda (mg)` <- (Exposed_Algae$`Chorda (mg)`  * 0.38)/1000
Exposed_Algae$TotalC <- Exposed_Algae$`Tolypella (mg)`+ Exposed_Algae$`Chorda (mg)`

### Total  Cstock per quadrat
Plot_Abun_C <- with(Exposed_Algae, tapply(TotalC, list(Site), sum))
Plot_Abun_C[is.na(Plot_Abun_C)] = 0
ExposedAlgaeCstock <- as.data.frame (Plot_Abun_C)
setDT(ExposedAlgaeCstock, keep.rownames = "Plot")
names(ExposedAlgaeCstock)[2]<-paste("CarbonStock")
ExposedAlgaeCstock$Category <- c("Exposed")
ExposedAlgaeCstock$site <- c("D_1","D_1","D_1","D_1","D_1","D_1","D_1","D_1",
                             "D_2","D_2","D_2","D_2","D_2","D_2",
                             "D_3","D_3","D_3","D_3","D_3","D_3",
                             "D_4","D_4","D_4","D_4","D_4","D_4",
                             "S_1","S_1","S_1","S_1","S_1","S_1","S_1","S_1",
                             "S_2","S_2","S_2","S_2","S_2",
                             "S_3","S_3","S_3","S_3","S_3","S_3",
                             "S_4","S_4","S_4","S_4","S_4","S_4",
                             "S_5","S_5","S_5","S_5","S_5","S_5")

### total Cstock per square meter
ExposedAlgaeCstock$CarbonStock <- ExposedAlgaeCstock$CarbonStock/(0.27*0.27)

ExposedAlgaeCstockB <- aggregate(x=ExposedAlgaeCstock$CarbonStock, by=list(ExposedAlgaeCstock$site), FUN=mean)
colnames(ExposedAlgaeCstockB)<-c("Site_number","Cstock_gm2")
Site_group = c("exposed", "exposed", "exposed","exposed","exposed", "exposed", "exposed","exposed", "exposed")
ExposedAlgaeCstockB <- cbind(ExposedAlgaeCstockB,Site_group)

Cstock_exp_5 <- data.frame(Site_number=c("D_5"),Cstock_gm2=c(0),Site_group=c("exposed"))
ExposedAlgaeCstockB <-rbind(ExposedAlgaeCstockB,Cstock_exp_5)
ExposedAlgaeCstockB$Depth <- c("Deep","Deep","Deep","Deep","Shallow",
                               "Shallow","Shallow","Shallow","Shallow","Deep")


##%######################################################%##
#                                                          #
####             Semi plant Cstocks                     ####
#                                                          #
##%######################################################%##

Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Semi", na = "NA")
Semi_data <- Spatial_Campaign_data_semi[,c(1:4,30,32,33)]

#######################################                          
##Cstocks plants based on C/N Samples##
#######################################

C_N_Semi <- C_N_Spatial[ which(C_N_Spatial$Exposure=='Semi'), ]
LEAF_Semi <- C_N_Semi[ which(C_N_Semi$Tissue=='Leaf'), ]
ROOT_Semi <- C_N_Semi[ which(C_N_Semi$Tissue=='Root'), ]
RHI_Semi <- C_N_Semi[ which(C_N_Semi$Tissue=='Rhi'), ]


### Median carbon values of tissues
LEAF_C <- aggregate(x= LEAF_Semi$`C (µg/mg)`, by = list(LEAF_Semi$Species), FUN = median)
names(LEAF_C)[1:2]<-paste(c("Species", "LEAF_C"))
ROOT_C <- aggregate(x= ROOT_Semi$`C (µg/mg)`, by = list(ROOT_Semi$Species), FUN = median)
names(ROOT_C)[1:2]<-paste(c("Species", "ROOT_C"))
RHI_C <- aggregate(x= RHI_Semi$`C (µg/mg)`, by = list(RHI_Semi$Species), FUN = median)
names(RHI_C)[1:2]<-paste(c("Species", "RHI_C"))

### combine biomass and species carbon values
Semi_C<- merge(Semi_data,LEAF_C,by="Species",  all = TRUE)
Semi_C<- merge(Semi_C,ROOT_C,by="Species",  all = TRUE)
Semi_C<- merge(Semi_C,RHI_C,by="Species",  all = TRUE)

### get total carbon stock for each species in grams in a quadrat
Semi_C$CarbonStockRoot <- (Semi_C$`Total root dry mass (mg)`*(Semi_C$ROOT_C/1000))/1000
Semi_C$CarbonStockRhi <- (Semi_C$`Total rhizome dry mass (mg)`*(Semi_C$RHI_C/1000))/1000
Semi_C$CarbonStockLeaf <- (Semi_C$`Total aboveground dry mass (mg)`*(Semi_C$LEAF_C/1000))/1000
Semi_C[is.na(Semi_C)] <- 0
Semi_C$TotalC <- Semi_C$CarbonStockLeaf + Semi_C$CarbonStockRoot + Semi_C$CarbonStockRhi

### Total  Cstock per quadrat
Plot_Abun_C <- with(Semi_C, tapply(TotalC, list(Site, Species), sum))
Plot_Abun_C[is.na(Plot_Abun_C)] = 0
SemiPlantCstock <- as.data.frame (rowSums(Plot_Abun_C))
setDT(SemiPlantCstock, keep.rownames = "Plot")
names(SemiPlantCstock)[2]<-paste("CarbonStock")
SemiPlantCstock$Category <- c("Semi")
SemiPlantCstock <- SemiPlantCstock[-c(1),]
SemiPlantCstock$site <- c("D_1","D_1","D_1","D_1","D_1","D_1",
                             "D_2","D_2","D_2","D_2","D_2","D_2",
                             "D_3","D_3","D_3","D_3","D_3","D_3",
                             "D_4","D_4","D_4","D_4","D_4","D_4",
                             "D_5","D_5","D_5","D_5","D_5","D_5",
                             "S_1","S_1","S_1","S_1","S_1","S_1",
                             "S_2","S_2","S_2","S_2","S_2","S_2",
                             "S_3","S_3","S_3","S_3","S_3","S_3",
                             "S_4","S_4","S_4","S_4","S_4","S_4",
                             "S_5","S_5","S_5","S_5","S_5","S_5")

### total Cstock per square meter
SemiPlantCstock$CarbonStock <- SemiPlantCstock$CarbonStock/(0.27*0.27)

SemiPlantCstockB <- aggregate(x=SemiPlantCstock$CarbonStock, by=list(SemiPlantCstock$site), FUN=mean)
colnames(SemiPlantCstockB)<-c("Site_number","Cstock_gm2")
Site_group = c("Semi", "Semi", "Semi","Semi","Semi", "Semi", "Semi", "Semi","Semi","Semi")
SemiPlantCstockB <- cbind(SemiPlantCstockB,Site_group)
SemiPlantCstockB$Depth <- c("Deep","Deep","Deep","Deep","Deep",
                               "Shallow","Shallow","Shallow","Shallow","Shallow")


##%######################################################%##
#                                                          #
####             Semi algae Cstocks                  ####
#                                                          #
##%######################################################%##


##%######################################################%##
#                                                          #
####           Sheltered plant Cstocks                  ####
#                                                          #
##%######################################################%##

Spatial_Campaign_data_Sheltered <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")
Shel_data <- Spatial_Campaign_data_semi[,c(1:4,34,36,37)]

#######################################                          
##Cstocks plants based on C/N Samples##
#######################################

C_N_Shel <- C_N_Spatial[ which(C_N_Spatial$Exposure=='Sheltered'), ]
LEAF_Shel <- C_N_Shel[ which(C_N_Shel$Tissue=='Leaf'), ]
ROOT_Shel <- C_N_Shel[ which(C_N_Shel$Tissue=='Root'), ]
RHI_Shel <- C_N_Shel[ which(C_N_Shel$Tissue=='Rhi'), ]


### Median carbon values of tissues
LEAF_C <- aggregate(x= LEAF_Shel$`C (µg/mg)`, by = list(LEAF_Shel$Species), FUN = median)
names(LEAF_C)[1:2]<-paste(c("Species", "LEAF_C"))
ROOT_C <- aggregate(x= ROOT_Shel$`C (µg/mg)`, by = list(ROOT_Shel$Species), FUN = median)
names(ROOT_C)[1:2]<-paste(c("Species", "ROOT_C"))
RHI_C <- aggregate(x= RHI_Shel$`C (µg/mg)`, by = list(RHI_Shel$Species), FUN = median)
names(RHI_C)[1:2]<-paste(c("Species", "RHI_C"))

### combine biomass and species carbon values
Shel_C<- merge(Shel_data,LEAF_C,by="Species",  all = TRUE)
Shel_C<- merge(Shel_C,ROOT_C,by="Species",  all = TRUE)
Shel_C<- merge(Shel_C,RHI_C,by="Species",  all = TRUE)

### get total carbon stock for each species in grams in a quadrat
Shel_C$CarbonStockRoot <- (Shel_C$`Total root dry mass (mg)`*(Shel_C$ROOT_C/1000))/1000
Shel_C$CarbonStockRhi <- (Shel_C$`Total rhizome dry mass (mg)`*(Shel_C$RHI_C/1000))/1000
Shel_C$CarbonStockLeaf <- (Shel_C$`Total aboveground dry mass (mg)`*(Shel_C$LEAF_C/1000))/1000
Shel_C[is.na(Shel_C)] <- 0
Shel_C$TotalC <- Shel_C$CarbonStockLeaf + Shel_C$CarbonStockRoot + Shel_C$CarbonStockRhi

### Total  Cstock per quadrat
Plot_Abun_C <- with(Shel_C, tapply(TotalC, list(Site, Species), sum))
Plot_Abun_C[is.na(Plot_Abun_C)] = 0
ShelPlantCstock <- as.data.frame (rowSums(Plot_Abun_C))
setDT(ShelPlantCstock, keep.rownames = "Plot")
names(ShelPlantCstock)[2]<-paste("CarbonStock")
ShelPlantCstock$Category <- c("Semi")
ShelPlantCstock <- ShelPlantCstock[-c(1),]
ShelPlantCstock$site <- c("D_1","D_1","D_1","D_1","D_1","D_1",
                          "D_2","D_2","D_2","D_2","D_2","D_2",
                          "D_3","D_3","D_3","D_3","D_3","D_3",
                          "D_4","D_4","D_4","D_4","D_4","D_4",
                          "D_5","D_5","D_5","D_5","D_5","D_5",
                          "S_1","S_1","S_1","S_1","S_1","S_1",
                          "S_2","S_2","S_2","S_2","S_2","S_2",
                          "S_3","S_3","S_3","S_3","S_3","S_3",
                          "S_4","S_4","S_4","S_4","S_4","S_4",
                          "S_5","S_5","S_5","S_5","S_5","S_5")

### total Cstock per square meter
ShelPlantCstock$CarbonStock <- ShelPlantCstock$CarbonStock/(0.27*0.27)

ShelPlantCstockB <- aggregate(x=ShelPlantCstock$CarbonStock, by=list(ShelPlantCstock$site), FUN=mean)
colnames(ShelPlantCstockB)<-c("Site_number","Cstock_gm2")
Site_group = c("Shel","Shel","Shel","Shel","Shel","Shel")
ShelPlantCstockB <- cbind(ShelPlantCstockB,Site_group)
ShelPlantCstockB$Depth <- c("Shallow","Shallow","Shallow","Shallow","Shallow")


##%######################################################%##
#                                                          #
####              Shel algae Cstocks                    ####
#                                                          #
##%######################################################%##
