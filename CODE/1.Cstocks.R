##%######################################################%##
#                                                          #
####                  Load R packages                   ####
#                                                          #
##%######################################################%##
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar())

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c('readxl', 'data.table', 'vegan', 'reshape2', 'moments')
ipak(packages)


############################                          
##Tidying up the datasheet##
############################

Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                    sheet = "Exposed", na = "NA")
Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Semi", na = "NA")
Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Sheltered", na = "NA")
Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Pojo", na = "NA")

Exposed_data <- Spatial_Campaign_data_exp[,c(1:4,29:34)]
Semi_data <-Spatial_Campaign_data_semi[,c(1:4,30:35)]
Shel_data <-Spatial_Campaign_data_shel[,c(1:4,34:39)]
Pojo_data <-Spatial_Campaign_data_pojo[,c(1:4,32:37)]


################################                          
##Cstocks based on C/N Samples##
################################

#######################################                          
##Cstocks on previous aggregate value##
#######################################

### Plants
Exposed_data$`Total  dry mass (mg)`[is.na(Exposed_data$`Total  dry mass (mg)`)] <- 0
Semi_data$`Total  dry mass (mg)`[is.na(Semi_data$`Total  dry mass (mg)`)] <- 0
Shel_data$`Total  dry mass (mg)`[is.na(Shel_data$`Total  dry mass (mg)`)] <- 0
Pojo_data$`Total  dry mass (mg)`[is.na(Pojo_data$`Total  dry mass (mg)`)] <- 0

Exposed_data$Total_dry_mass_gm2 <- (Exposed_data$`Total  dry mass (mg)`/(0.27*0.27))/1000
Semi_data$Total_dry_mass_gm2 <- (Semi_data$`Total  dry mass (mg)`/(0.27*0.27))/1000
Shel_data$Total_dry_mass_gm2 <- (Shel_data$`Total  dry mass (mg)`/(0.27*0.27))/1000
Pojo_data$Total_dry_mass_gm2 <- (Pojo_data$`Total  dry mass (mg)`/(0.27*0.27))/1000

Exposed_data$Cstock_gm2 <- Exposed_data$Total_dry_mass_gm2*0.38
Semi_data$Cstock_gm2 <- Semi_data$Total_dry_mass_gm2*0.38
Shel_data$Cstock_gm2 <- Shel_data$Total_dry_mass_gm2*0.38
Pojo_data$Cstock_gm2 <- Pojo_data$Total_dry_mass_gm2*0.38

### Algae
Exposed_data$Algae_dry_mass_gm2 <- (Exposed_data$`Algae dry mass (mg)`/(0.27*0.27))/1000
Semi_data$Algae_dry_mass_gm2 <- (Semi_data$`Algae dry mass (mg)`/(0.27*0.27))/1000
Shel_data$Algae_dry_mass_gm2 <- (Shel_data$`Algae dry mass (mg)`/(0.27*0.27))/1000
Pojo_data$Algae_dry_mass_gm2 <- (Pojo_data$`Algae dry mass (mg)`/(0.27*0.27))/1000

Exposed_data$Algae_Cstock_gm2 <- Exposed_data$Algae_dry_mass_gm2*0.38
Semi_data$Algae_Cstock_gm2 <- Semi_data$Algae_dry_mass_gm2*0.38
Shel_data$Algae_Cstock_gm2 <- Shel_data$Algae_dry_mass_gm2*0.38
Pojo_data$Algae_Cstock_gm2 <- Pojo_data$Algae_dry_mass_gm2*0.38

##%######################################################%##
#                                                          #
####            Total c stocks per quadrat              ####
#                                                          #
##%######################################################%##

### Plant
Cstock_exp_quad <- aggregate(x=Exposed_data$Cstock_gm2,
                             by=list(Exposed_data$Site,Exposed_data$`Site number`), FUN=sum)
Cstock_exp_quad_Deep <- Cstock_exp_quad[-c(9:16,23:27,34:39,46:57),]
Cstock_exp_quad_Shallow <-Cstock_exp_quad[c(9:16,23:27,34:39,46:57),]
colnames(Cstock_exp_quad_Deep) <- c("Site","Site_number","Cstock_gm2")
colnames(Cstock_exp_quad_Shallow) <- c("Site","Site_number","Cstock_gm2")

Cstock_semi_quad <- aggregate(x=Semi_data$Cstock_gm2,
                              by=list(Semi_data$Site,Semi_data$`Site number`), FUN=sum)
Cstock_semi_quad_Deep <- Cstock_semi_quad[-c(7:12,19:24,31:36,43:48,55:60),]
Cstock_semi_quad_Shallow <-Cstock_semi_quad[c(7:12,19:24,31:36,43:48,55:60),]
colnames(Cstock_semi_quad_Deep) <- c("Site","Site_number","Cstock_gm2")
colnames(Cstock_semi_quad_Shallow) <- c("Site","Site_number","Cstock_gm2")

Cstock_shel_quad_Shallow <- aggregate(x=Shel_data$Cstock_gm2,
                                      by=list(Shel_data$Site,Shel_data$`Site number`), FUN=sum)
colnames(Cstock_shel_quad_Shallow) <- c("Site","Site_number","Cstock_gm2")

Cstock_pojo_quad <- aggregate(x=Pojo_data$Cstock_gm2,
                              by=list(Pojo_data$Site,Pojo_data$`Site number`), FUN=sum)
Cstock_pojo_quad_Deep <- Cstock_pojo_quad[c(7:12,19:24),]
Cstock_pojo_quad_Shallow <-Cstock_pojo_quad[c(1:6,13:18,25:42),]
colnames(Cstock_pojo_quad_Deep) <- c("Site","Site_number","Cstock_gm2")
colnames(Cstock_pojo_quad_Shallow) <- c("Site","Site_number","Cstock_gm2")

### Algae
Cstock_exp_Alg_quad <- aggregate(x=Exposed_data$Algae_Cstock_gm2,
                             by=list(Exposed_data$Site,Exposed_data$`Site number`), FUN=sum)
Cstock_exp_Alg_quad_Deep <- Cstock_exp_Alg_quad[-c(9:16,23:27,34:39,46:57),]
Cstock_exp_Alg_quad_Shallow <-Cstock_exp_Alg_quad[c(9:16,23:27,34:39,46:57),]
colnames(Cstock_exp_Alg_quad_Deep) <- c("Site","Site_number","Algae_Cstock_gm2")
colnames(Cstock_exp_Alg_quad_Shallow) <- c("Site","Site_number","Algae_Cstock_gm2")

Cstock_semi_Alg_quad <- aggregate(x=Semi_data$Algae_Cstock_gm2,
                              by=list(Semi_data$Site,Semi_data$`Site number`), FUN=sum)
Cstock_semi_Alg_quad_Deep <- Cstock_semi_Alg_quad[-c(7:12,19:24,31:36,43:48,55:60),]
Cstock_semi_Alg_quad_Shallow <-Cstock_semi_Alg_quad[c(7:12,19:24,31:36,43:48,55:60),]
colnames(Cstock_semi_Alg_quad_Deep) <- c("Site","Site_number","Algae_Cstock_gm2")
colnames(Cstock_semi_Alg_quad_Shallow) <- c("Site","Site_number","Algae_Cstock_gm2")

Cstock_shel_Alg_quad_Shallow <- aggregate(x=Shel_data$Algae_Cstock_gm2,
                                      by=list(Shel_data$Site,Shel_data$`Site number`), FUN=sum)
colnames(Cstock_shel_Alg_quad_Shallow) <- c("Site","Site_number","Algae_Cstock_gm2")

Cstock_pojo_Alg_quad <- aggregate(x=Pojo_data$Algae_Cstock_gm2,
                              by=list(Pojo_data$Site,Pojo_data$`Site number`), FUN=sum)
Cstock_pojo_Alg_quad_Deep <- Cstock_pojo_Alg_quad[c(7:12,19:24),]
Cstock_pojo_Alg_quad_Shallow <-Cstock_pojo_Alg_quad[c(1:6,13:18,25:42),]
colnames(Cstock_pojo_Alg_quad_Deep) <- c("Site","Site_number","Algae_Cstock_gm2")
colnames(Cstock_pojo_Alg_quad_Shallow) <- c("Site","Site_number","Algae_Cstock_gm2")


##%######################################################%##
#                                                          #
####            Average c stocks per site               ####
#                                                          #
##%######################################################%##

################################
##Mean Cstock per site (DEEP)##
################################

### Plant
Cstock_exp_deep <- aggregate(x=Cstock_exp_quad_Deep$Cstock_gm2, by=list(Cstock_exp_quad_Deep$Site_number), FUN=mean)
colnames(Cstock_exp_deep)<-c("Site_number","Cstock_gm2")
Site_group = c("exposed", "exposed", "exposed","exposed")
Cstock_exp_deep <- cbind(Cstock_exp_deep,Site_group)
Cstock_exp_5 <- data.frame(Site_number=c(5),Cstock_gm2=c(0),Site_group=c("exposed"))
Cstock_exp_deep <-rbind(Cstock_exp_deep,Cstock_exp_5)

Cstock_semi_deep <- aggregate(x=Cstock_semi_quad_Deep$Cstock_gm2, by=list(Cstock_semi_quad_Deep$Site_number), FUN=mean)
colnames(Cstock_semi_deep)<-c("Site_number","Cstock_gm2")
Site_group = c("semi", "semi", "semi","semi","semi")
Cstock_semi_deep <- cbind(Cstock_semi_deep,Site_group)


Site_number <-c(1,2,3,4,5)
Cstock_gm2<-c(0,0,0,0,0)
Site_group <- c("sheltered","sheltered","sheltered","sheltered","sheltered")
Cstock_shel_deep <-data.frame(Site_number=Site_number,
                              Cstock_gm2=Cstock_gm2,
                              Site_group=Site_group)

Cstock_pojo_deep <- aggregate(x=Cstock_pojo_quad_Deep$Cstock_gm2, 
                              by=list(Cstock_pojo_quad_Deep$Site_number), FUN=mean)
colnames(Cstock_pojo_deep)<-c("Site_number","Cstock_gm2")
Site_group = c("Pojo", "Pojo")
Cstock_pojo_deep <- cbind(Cstock_pojo_deep,Site_group)
Cstock_pojo_145 <- data.frame(Site_number=c(1,4,5),Cstock_gm2=c(0,0,0),Site_group=c("Pojo","Pojo","Pojo"))
Cstock_pojo_deep <-rbind(Cstock_pojo_deep,Cstock_pojo_145)

### Algae
Cstock_exp_deep_Alg <- aggregate(x=Cstock_exp_Alg_quad_Deep$Algae_Cstock_gm2, by=list(Cstock_exp_Alg_quad_Deep$Site_number), FUN=mean)
colnames(Cstock_exp_deep_Alg)<-c("Site_number","Cstock_gm2")
Site_group = c("exposed", "exposed", "exposed","exposed")
Cstock_exp_deep_Alg <- cbind(Cstock_exp_deep_Alg,Site_group)
Cstock_exp_5 <- data.frame(Site_number=c(5),Cstock_gm2=c(0),Site_group=c("exposed"))
Cstock_exp_deep_Alg <-rbind(Cstock_exp_deep_Alg,Cstock_exp_5)

Cstock_semi_deep_Alg <- aggregate(x=Cstock_semi_Alg_quad_Deep$Algae_Cstock_gm2, by=list(Cstock_semi_quad_Deep$Site_number), FUN=mean)
colnames(Cstock_semi_deep_Alg)<-c("Site_number","Cstock_gm2")
Site_group = c("semi", "semi", "semi","semi","semi")
Cstock_semi_deep_Alg <- cbind(Cstock_semi_deep_Alg,Site_group)


Site_number <-c(1,2,3,4,5)
Cstock_gm2<-c(0,0,0,0,0)
Site_group <- c("sheltered","sheltered","sheltered","sheltered","sheltered")
Cstock_shel_deep_Alg <-data.frame(Site_number=Site_number,
                              Cstock_gm2=Cstock_gm2,
                              Site_group=Site_group)

Cstock_pojo_deep_Alg <- aggregate(x=Cstock_pojo_Alg_quad_Deep$Algae_Cstock_gm2, 
                              by=list(Cstock_pojo_Alg_quad_Deep$Site_number), FUN=mean)
colnames(Cstock_pojo_deep_Alg)<-c("Site_number","Cstock_gm2")
Site_group = c("Pojo", "Pojo")
Cstock_pojo_deep_Alg <- cbind(Cstock_pojo_deep_Alg,Site_group)
Cstock_pojo_deep_Alg <-rbind(Cstock_pojo_deep_Alg,Cstock_pojo_145)



Cstock_all_mean_deep <-rbind(Cstock_exp_deep,Cstock_semi_deep,Cstock_shel_deep,Cstock_pojo_deep,
                             Cstock_exp_deep_Alg,Cstock_semi_deep_Alg,Cstock_shel_deep_Alg,Cstock_pojo_deep_Alg)
Cstock_all_mean_deep$Macrophytes <- c("Plants","Plants","Plants","Plants","Plants",
                                      "Plants","Plants","Plants","Plants","Plants",
                                      "Plants","Plants","Plants","Plants","Plants",
                                      "Plants","Plants","Plants","Plants","Plants",
                                      "Algae","Algae","Algae","Algae","Algae",
                                      "Algae","Algae","Algae","Algae","Algae",
                                      "Algae","Algae","Algae","Algae","Algae",
                                      "Algae","Algae","Algae","Algae","Algae")

##################################
##Mean Cstock per site (Shallow)##
##################################

### Plant
Cstock_exp_shallow <- aggregate(x=Cstock_exp_quad_Shallow$Cstock_gm2, by=list(Cstock_exp_quad_Shallow$Site_number), FUN=mean)
colnames(Cstock_exp_shallow)<-c("Site_number","Cstock_gm2")
Site_group = c("exposed", "exposed", "exposed","exposed","exposed")
Cstock_exp_shallow <- cbind(Cstock_exp_shallow,Site_group)

Cstock_semi_shallow <- aggregate(x=Cstock_semi_quad_Shallow$Cstock_gm2, by=list(Cstock_semi_quad_Shallow$Site_number), FUN=mean)
colnames(Cstock_semi_shallow)<-c("Site_number","Cstock_gm2")
Site_group = c("semi", "semi", "semi","semi","semi")
Cstock_semi_shallow <- cbind(Cstock_semi_shallow,Site_group)

Cstock_shel_shallow <- aggregate(x=Cstock_shel_quad_Shallow$Cstock_gm2, by=list(Cstock_shel_quad_Shallow$Site_number), FUN=mean)
colnames(Cstock_shel_shallow)<-c("Site_number","Cstock_gm2")
Site_group = c("sheltered", "sheltered", "sheltered","sheltered","sheltered")
Cstock_shel_shallow <- cbind(Cstock_shel_shallow,Site_group)

Cstock_pojo_shallow <- aggregate(x=Cstock_pojo_quad_Shallow$Cstock_gm2,
                                 by=list(Cstock_pojo_quad_Shallow$Site_number), FUN=mean)
colnames(Cstock_pojo_shallow)<-c("Site_number","Cstock_gm2")
Site_group = c("Pojo", "Pojo", "Pojo","Pojo","Pojo")
Cstock_pojo_shallow <- cbind(Cstock_pojo_shallow,Site_group)

### Algae
Cstock_exp_shallow_Alg <- aggregate(x=Cstock_exp_Alg_quad_Shallow$Algae_Cstock_gm2, by=list(Cstock_exp_Alg_quad_Shallow$Site_number), FUN=mean)
colnames(Cstock_exp_shallow_Alg)<-c("Site_number","Cstock_gm2")
Site_group = c("exposed", "exposed", "exposed","exposed","exposed")
Cstock_exp_shallow_Alg <- cbind(Cstock_exp_shallow_Alg,Site_group)

Cstock_semi_shallow_Alg <- aggregate(x=Cstock_semi_Alg_quad_Shallow$Algae_Cstock_gm2, by=list(Cstock_semi_Alg_quad_Shallow$Site_number), FUN=mean)
colnames(Cstock_semi_shallow_Alg)<-c("Site_number","Cstock_gm2")
Site_group = c("semi", "semi", "semi","semi","semi")
Cstock_semi_shallow_Alg <- cbind(Cstock_semi_shallow_Alg,Site_group)

Cstock_shel_shallow_Alg <- aggregate(x=Cstock_shel_Alg_quad_Shallow$Algae_Cstock_gm2, by=list(Cstock_shel_Alg_quad_Shallow$Site_number), FUN=mean)
colnames(Cstock_shel_shallow_Alg)<-c("Site_number","Cstock_gm2")
Site_group = c("sheltered", "sheltered", "sheltered","sheltered","sheltered")
Cstock_shel_shallow_Alg <- cbind(Cstock_shel_shallow_Alg,Site_group)

Cstock_pojo_shallow_Alg <- aggregate(x=Cstock_pojo_Alg_quad_Shallow$Algae_Cstock_gm2,
                                 by=list(Cstock_pojo_Alg_quad_Shallow$Site_number), FUN=mean)
colnames(Cstock_pojo_shallow_Alg)<-c("Site_number","Cstock_gm2")
Site_group = c("Pojo", "Pojo", "Pojo","Pojo","Pojo")
Cstock_pojo_shallow_Alg <- cbind(Cstock_pojo_shallow_Alg,Site_group)



Cstock_all_mean_shallow <-rbind(Cstock_exp_shallow,Cstock_semi_shallow,Cstock_shel_shallow,Cstock_pojo_shallow,
                             Cstock_exp_shallow_Alg,Cstock_semi_shallow_Alg,Cstock_shel_shallow_Alg,
                             Cstock_pojo_shallow_Alg)
Cstock_all_mean_shallow$Macrophytes <- c("Plants","Plants","Plants","Plants","Plants",
                                      "Plants","Plants","Plants","Plants","Plants",
                                      "Plants","Plants","Plants","Plants","Plants",
                                      "Plants","Plants","Plants","Plants","Plants",
                                      "Algae","Algae","Algae","Algae","Algae",
                                      "Algae","Algae","Algae","Algae","Algae",
                                      "Algae","Algae","Algae","Algae","Algae",
                                      "Algae","Algae","Algae","Algae","Algae")

##%######################################################%##
#                                                          #
####        Boxplot Average c stocks per site           ####
#                                                          #
##%######################################################%##

Cstock_all_mean_shallow$MacrophyteCategory <- factor(paste(Cstock_all_mean_shallow$Site_group, 
                                                           Cstock_all_mean_shallow$Macrophytes, sep = "_"),
  levels = c(
    "exposed_Plants", "exposed_Algae",
    "semi_Plants", "semi_Algae",
    "sheltered_Plants", "sheltered_Algae",
    "Pojo_Plants", "Pojo_Algae"
  ))
Cstock_all_mean_deep$MacrophyteCategory <- factor(paste(Cstock_all_mean_deep$Site_group, 
                                                           Cstock_all_mean_deep$Macrophytes, sep = "_"),
                                                     levels = c(
                                                       "exposed_Plants", "exposed_Algae",
                                                       "semi_Plants", "semi_Algae",
                                                       "sheltered_Plants", "sheltered_Algae",
                                                       "Pojo_Plants", "Pojo_Algae"
                                                     ))



png(
  "output_plot/CarbonStocksBoxplotB.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)

par(mfcol=c(2,1))

par(mar = c(3, 5, 2, 3))

Cstock_all_mean_shallow$MacrophyteCategory <- factor(Cstock_all_mean_shallow$MacrophyteCategory , # Reorder factor levels
                        c("exposed_Plants", "exposed_Algae", "semi_Plants", "semi_Algae", 
                          "sheltered_Plants", "sheltered_Algae", "Pojo_Plants", "Pojo_Algae"))

boxplot(Cstock_gm2 ~ MacrophyteCategory, data = Cstock_all_mean_shallow,
        boxwex = 0.5, col = c("bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "g C m-2 (1-2m)", ylim = c(0, 200),at = c(1, 2, 4, 5, 7, 8, 10, 11),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(Cstock_gm2 ~ MacrophyteCategory,
           data = Cstock_all_mean_shallow,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,7.5,9.5,10.5),
           pch = 17,
           col = c("bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-exposed", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.5 , 11 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topleft", legend = c("Plant", "Algae"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)


Cstock_all_mean_deep$MacrophyteCategory <- factor(Cstock_all_mean_deep$MacrophyteCategory , # Reorder factor levels
                                                     c("exposed_Plants", "exposed_Algae", "semi_Plants", "semi_Algae", 
                                                       "sheltered_Plants", "sheltered_Algae", "Pojo_Plants", "Pojo_Algae"))
boxplot(Cstock_gm2 ~ MacrophyteCategory, data = Cstock_all_mean_deep,
        boxwex = 0.5, col = c("bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "g C m-2 (3-4m)", ylim = c(0, 100),at = c(1, 2, 4, 5, 7, 8, 10, 11),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(Cstock_gm2 ~ MacrophyteCategory,
           data = Cstock_all_mean_deep,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,7.5,9.5,10.5),
           pch = 17,
           col = c("bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-exposed", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.5 , 11 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)
dev.off()









