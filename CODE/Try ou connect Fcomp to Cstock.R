##%######################################################%##
#                                                          #
####                 C stock combine                    ####
#                                                          #
##%######################################################%##

### Datasets from C calculations

names(Cstock_exp_quad)[1:3]<-paste(c("Quadrat", "Site", "Cstock"))
Cstock_exp_quad$Exposure <- "Exposed"
Cstock_exp_quad$Match <- paste(Cstock_exp_quad$Quadrat, Cstock_exp_quad$Exposure, sep="_")

names(Cstock_semi_quad)[1:3]<-paste(c("Quadrat", "Site", "Cstock"))
Cstock_semi_quad$Exposure <- "Semi"
Cstock_semi_quad$Match <- paste(Cstock_semi_quad$Quadrat, Cstock_semi_quad$Exposure, sep="_")

names(Cstock_shel_quad_Shallow)[1:3]<-paste(c("Quadrat", "Site", "Cstock"))
Cstock_shel_quad_Shallow$Exposure <- "Sheltered"
Cstock_shel_quad_Shallow$Match <- paste(Cstock_shel_quad_Shallow$Quadrat, Cstock_shel_quad_Shallow$Exposure, sep="_")

names(Cstock_pojo_quad )[1:3]<-paste(c("Quadrat", "Site", "Cstock"))
Cstock_pojo_quad$Exposure <- "Pojo"
Cstock_pojo_quad$Match <- paste(Cstock_pojo_quad$Quadrat, Cstock_pojo_quad$Exposure, sep="_")

Cstocks_AllsitesB<- rbind.data.frame(Cstock_exp_quad,Cstock_semi_quad,
                                     Cstock_shel_quad_Shallow,Cstock_pojo_quad)
Cstocks_AllsitesB <- Cstocks_AllsitesB[-c(154, 156, 158, 159),] 

### Datasest from community indices B

Indices_PCA_AllsitesB["Site"][Indices_PCA_AllsitesB["Site"] == "1"]  <- "D2.1"
Indices_PCA_AllsitesB["Site"][Indices_PCA_AllsitesB["Site"] == "2"]  <- "D2.2"
Indices_PCA_AllsitesB["Site"][Indices_PCA_AllsitesB["Site"] == "3"]  <- "D2.3"
Indices_PCA_AllsitesB["Site"][Indices_PCA_AllsitesB["Site"] == "4"]  <- "D2.4"
Indices_PCA_AllsitesB["Site"][Indices_PCA_AllsitesB["Site"] == "5"]  <- "D2.5"
Indices_PCA_AllsitesB["Site"][Indices_PCA_AllsitesB["Site"] == "6"]  <- "D2.6"

Indices_PCA_AllsitesB["Site"][Indices_PCA_AllsitesB["Site"] == "D4.11"]  <- "D5.1"
Indices_PCA_AllsitesB["Site"][Indices_PCA_AllsitesB["Site"] == "D4.21"]  <- "D5.2"
Indices_PCA_AllsitesB["Site"][Indices_PCA_AllsitesB["Site"] == "D4.31"]  <- "D5.3"
Indices_PCA_AllsitesB["Site"][Indices_PCA_AllsitesB["Site"] == "D4.41"]  <- "D5.4"
Indices_PCA_AllsitesB["Site"][Indices_PCA_AllsitesB["Site"] == "D4.51"]  <- "D5.5"
Indices_PCA_AllsitesB["Site"][Indices_PCA_AllsitesB["Site"] == "D4.61"]  <- "D5.6"

Indices_PCA_AllsitesB$Match <- paste(Indices_PCA_AllsitesB$Site, Indices_PCA_AllsitesB$Exposure, sep="_")
Lmdata <- merge(Cstocks_AllsitesB, Indices_PCA_AllsitesB, by.x="Match", by.y="Match")
LmdataB <-Lmdata[,c(2,4,7,8, 9:15)]
Lmdata_Shallow <- subset(LmdataB, Depth=="Shallow")
summary(Lmdata_Shallow)
Lmdata_Shallow$CWM_PC1 <- as.numeric(Lmdata_Shallow$CWM_PC1 )
Lmdata_Shallow$CWM_PC2 <- as.numeric(Lmdata_Shallow$CWM_PC2)
Lmdata_Shallow$FDis_PC1 <- as.numeric(Lmdata_Shallow$FDis_PC1)
Lmdata_Shallow$FDis_PC2 <- as.numeric(Lmdata_Shallow$FDis_PC2)

summary(lm(Lmdata_Shallow$Cstock ~ Lmdata_Shallow$CWM_PC1))
summary(lm(Lmdata_Shallow$Cstock ~ Lmdata_Shallow$CWM_PC2))
summary(lm(Lmdata_Shallow$Cstock ~ Lmdata_Shallow$FDis_PC1))
summary(lm(Lmdata_Shallow$Cstock ~ Lmdata_Shallow$FDis_PC2))



plot(Lmdata_Shallow$Cstock ~ Lmdata_Shallow$FDis_PC1)
abline(lm(Lmdata_Shallow$Cstock ~ Lmdata_Shallow$FDis_PC1))
plot(Lmdata_Shallow$Cstock ~ Lmdata_Shallow$FDis_PC2)
abline(lm(Lmdata_Shallow$Cstock ~ Lmdata_Shallow$FDis_PC2))


Lmdata_exp <- subset(Lmdata_Shallow, Exposure.y=="Exposed")
plot(Lmdata_exp$Cstock ~ Lmdata_exp$FDis_PC1)
abline(lm(Lmdata_exp$Cstock ~ Lmdata_exp$FDis_PC1))
plot(Lmdata_exp$Cstock ~ Lmdata_exp$FDis_PC2)
abline(lm(Lmdata_exp$Cstock ~ Lmdata_exp$FDis_PC2))

Lmdata_semi <- subset(Lmdata_Shallow, Exposure.y=="Semi")

plot(Lmdata_semi$Cstock ~ Lmdata_semi$FDis_PC1)
abline(lm(Lmdata_semi$Cstock ~ Lmdata_semi$FDis_PC1))
plot(Lmdata_semi$Cstock ~ Lmdata_semi$FDis_PC2)
abline(lm(Lmdata_semi$Cstock ~ Lmdata_semi$FDis_PC2))

Lmdata_shel <- subset(Lmdata_Shallow, Exposure.y=="Sheltered")

plot(Lmdata_shel$Cstock ~ Lmdata_shel$FDis_PC1)
abline(lm(Lmdata_shel$Cstock ~ Lmdata_shel$FDis_PC1))
plot(Lmdata_shel$Cstock ~ Lmdata_shel$FDis_PC2)
abline(lm(Lmdata_shel$Cstock ~ Lmdata_shel$FDis_PC2))

Lmdata_pojo <- subset(Lmdata_Shallow, Exposure.y=="Pojo")

plot(Lmdata_pojo$Cstock ~ Lmdata_pojo$FDis_PC1)
abline(lm(Lmdata_pojo$Cstock ~ Lmdata_pojo$FDis_PC1))
plot(Lmdata_pojo$Cstock ~ Lmdata_pojo$FDis_PC2)
abline(lm(Lmdata_pojo$Cstock ~ Lmdata_pojo$FDis_PC2))

