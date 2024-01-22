##%######################################################%##
#                                                          #
####              Update data file for stats            ####
#                                                          #
##%######################################################%##

Cstock_all_mean_deep$Depth <- c(rep("Deep",40))
Cstock_all_mean_shallow$Depth <- c(rep("Shallow",40))
Cstocks_all_mean <- rbind.data.frame(Cstock_all_mean_deep,Cstock_all_mean_shallow)
Cstocks_all_mean$Exposure_Depth <- factor(paste(Cstocks_all_mean$Site_group, 
                                                Cstocks_all_mean$Depth, sep = "_"),
                                                 levels = c(
                                                   "exposed_Shallow", "exposed_Deep",
                                                   "semi_Shallow", "semi_Deep",
                                                   "Pojo_Shallow", "Pojo_Deep",
                                                   "sheltered_Shallow","sheltered_Deep"
                                                 ))

##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##

### Exposed_Deep
Cstocks_all_mean[c(1:5, 21:25),c(6)]
mshapiro_test(as.numeric(Cstocks_all_mean[c(1:5),c(2)])) # significant for plants
mshapiro_test(as.numeric(Cstocks_all_mean[c(1:4),c(2)])) # non-significant for plants
mshapiro_test(as.numeric(Cstocks_all_mean[c(21:25),c(2)])) # significant for algae

### Exposed_Shallow
Cstocks_all_mean[c(41:45, 61:65),c(6)]
mshapiro_test(as.numeric(Cstocks_all_mean[c(41:45),c(2)])) # non-significant for plants
mshapiro_test(as.numeric(Cstocks_all_mean[c(61:65),c(2)])) # significant for algae

### Semi_Deep
Cstocks_all_mean[c(6:10, 26:30),c(6)]
mshapiro_test(as.numeric(Cstocks_all_mean[c(6:10),c(2)])) # significant for plants
mshapiro_test(as.numeric(Cstocks_all_mean[c(26:30),c(2)])) # all zero so no results for algae

### Semi_Shallow
Cstocks_all_mean[c(46:50, 66:70),c(6)]
mshapiro_test(as.numeric(Cstocks_all_mean[c(46:50),c(2)])) # non-significant for plants
mshapiro_test(as.numeric(Cstocks_all_mean[c(66:70),c(2)])) # non-significant for algae

### Sheltered_Deep
Cstocks_all_mean[c(11:15, 31:35),c(6)]
mshapiro_test(as.numeric(Cstocks_all_mean[c(11:15),c(2)])) # all zero so no results for plants
mshapiro_test(as.numeric(Cstocks_all_mean[c(31:35),c(2)])) # all zero so no results for algae

### Sheltered_Shallow
Cstocks_all_mean[c(51:55, 71:75),c(6)]
mshapiro_test(as.numeric(Cstocks_all_mean[c(51:55),c(2)])) # significant for plants
mshapiro_test(as.numeric(Cstocks_all_mean[c(71:75),c(2)])) # significant for algae

### Pojo_Deep
Cstocks_all_mean[c(16:20, 36:40),c(6)]
mshapiro_test(as.numeric(Cstocks_all_mean[c(16:20),c(2)])) # significant for plants
mshapiro_test(as.numeric(Cstocks_all_mean[c(36:40),c(2)])) # significant for algae

### Pojo_Shallow
Cstocks_all_mean[c(56:60, 76:80),c(6)]
mshapiro_test(as.numeric(Cstocks_all_mean[c(56:60),c(2)])) # significant for plants
mshapiro_test(as.numeric(Cstocks_all_mean[c(76:80),c(2)])) # marginally significant for algae

# Conclusion non-parametric tests for both depths and algae/plants

##%######################################################%##
#                                                          #
####                  Homogeneity tests                 ####
#                                                          #
##%######################################################%##

Cstocks_all_mean$Cstock_gm2<- as.numeric(Cstocks_all_mean$Cstock_gm2) 
Algae_Shallow <- subset(Cstocks_all_mean, Macrophytes=="Algae" & Depth=="Shallow")
Algae_Deep <- subset(Cstocks_all_mean, Macrophytes=="Algae" & Depth=="Deep")
Plant_Shallow <- subset(Cstocks_all_mean, Macrophytes=="Plants" & Depth=="Shallow")
Plant_Deep <- subset(Cstocks_all_mean, Macrophytes=="Plants" & Depth=="Deep")

fligner.test(Algae_Shallow$Cstock_gm2 ~ Algae_Shallow$Exposure) # non-signifcant
fligner.test(Algae_Deep$Cstock_gm2 ~ Algae_Deep$Exposure) # non-signifcant

fligner.test(Plant_Shallow$Cstock_gm2 ~ Plant_Shallow$Exposure) # non - signifcant
fligner.test(Plant_Deep$Cstock_gm2 ~ Plant_Deep$Exposure) # signifcant 

# Conclusion to test for gradients within a depth, we can assume mostly homogeneity of variance

##%######################################################%##
#                                                          #
####            Tests within the same depth             ####
#                                                          #
##%######################################################%##

kruskal.test(Cstock_gm2 ~ Exposure_Depth, data = Algae_Shallow) # non-significant
kruskal.test(Cstock_gm2 ~ Exposure_Depth, data = Algae_Deep) # non-significant
kruskal.test(Cstock_gm2 ~ Exposure_Depth, data = Plant_Shallow) # non-significant
kruskal.test(Cstock_gm2 ~ Exposure_Depth, data = Plant_Deep) # significant differences


############## Mann-Whitney U Post hoc test ##############

### Deep plant C stocks

Plant_DeepA <- Plant_Deep[c(1:10),]
Plant_DeepA2 <- Plant_Deep[c(1:4, 6:10),]
Plant_DeepB <- Plant_Deep[c(6:15),] 
Plant_DeepC <- Plant_Deep[c(11:20),] 

A <-wilcox.test(Cstock_gm2 ~ Exposure_Depth, data = Plant_DeepA, paired = FALSE, alternative = "greater")
A # Exposed to semi are marginally significant lower

A2 <-wilcox.test(Cstock_gm2 ~ Exposure_Depth, data = Plant_DeepA2, paired = FALSE, alternative = "greater")
A2 # Exposed to semi are significant lower if we take the 0 outlier out

B <-wilcox.test(Cstock_gm2 ~ Exposure_Depth, data = Plant_DeepB, paired = FALSE, alternative = "greater")
B # semi to sheltered are significant lower

B2 <-wilcox.test(Cstock_gm2 ~ Exposure_Depth, data = Plant_DeepC, paired = FALSE, alternative = "greater")
B2 # Sheltered to Pojo are margnially significant lower


##%######################################################%##
#                                                          #
####          Tests between different depths            ####
#                                                          #
##%######################################################%##

Plant_Exposure <- Cstocks_all_mean[c(1:5, 41:45),]
Plant_DeepExposure2 <- Cstocks_all_mean[c(1:4, 41:45),]
Plant_Semi <- Cstocks_all_mean[c(6:10, 46,50),] 
Plant_Sheltered <- Cstocks_all_mean[c(11:15, 51:55),] 
Plant_Pojo <- Cstocks_all_mean[c(16:20, 56:60),] 

A <-wilcox.test(Cstock_gm2 ~ Depth, data = Plant_Exposure, paired = FALSE, alternative = "less")
A # No significant difference between depths

A2 <-wilcox.test(Cstock_gm2 ~ Depth, data = Plant_DeepExposure2, paired = FALSE, alternative = "greater")
A2 # Goes further away from significance when taking out the 0 value

B <-wilcox.test(Cstock_gm2 ~ Depth, data = Plant_Semi, paired = FALSE, alternative = "less")
B # Deep sites have significantly lower C stocks

B2 <-wilcox.test(Cstock_gm2 ~ Depth, data = Plant_Sheltered, paired = FALSE, alternative = "less")
B2 # Deep sites have significantly lower C stocks

C <-wilcox.test(Cstock_gm2 ~ Depth, data = Plant_Pojo, paired = FALSE, alternative = "less")
C # Deep sites have significantly lower C stocks
