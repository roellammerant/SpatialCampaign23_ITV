#########################
##Boxplot of abundance###
#########################

##Preparing data, Total abundance per quadrat##

Abundance_exp_quad <- aggregate(x=Exposed_data$`Individual count`,
                                by=list(Exposed_data$Site,Exposed_data$`Site number`), FUN=sum)
colnames(Abundance_exp_quad) <- c("Site","Site number","Individual count")
Abundance_exp_quad$Individual_count_m2 <- Abundance_exp_quad$`Individual count`/(0.2*0.2)
Abundance_exp_quad_Deep <- Abundance_exp_quad[-c(9:16,23:27,34:39,46:57),]
Abundance_exp_quad_Shallow <-Abundance_exp_quad[c(9:16,23:27,34:39,46:57),]



Abundance_semi_quad <- aggregate(x=Semi_data$`Individual count`,
                                 by=list(Semi_data$Site,Semi_data$`Site number`), FUN=sum)
colnames(Abundance_semi_quad) <- c("Site","Site number","Individual count")
Abundance_semi_quad$Individual_count_m2 <- Abundance_semi_quad$`Individual count`/(0.2*0.2)
Abundance_semi_quad_Deep <- Abundance_semi_quad[-c(7:12,19:24,31:36,43:48,55:60),]
Abundance_semi_quad_Shallow <-Abundance_semi_quad[c(7:12,19:24,31:36,43:48,55:60),]

Individual_Shel_data <- Shel_data[,c(1:4,6)]
Individual_Shel_data <-na.omit(Individual_Shel_data)
Abundance_shel_quad_Shallow <- aggregate(x=Individual_Shel_data$`Individual count`,
                                         by=list(Individual_Shel_data$Site,Individual_Shel_data$`Site number`), FUN=sum)
colnames(Abundance_shel_quad_Shallow) <- c("Site","Site number","Individual count")
Abundance_shel_quad_Shallow$Individual_count_m2 <- Abundance_shel_quad_Shallow$`Individual count`/(0.2*0.2)


Individual_Pojo_data <- Pojo_data[,c(1:4,6)]
Individual_Pojo_data <-na.omit(Individual_Pojo_data)
Abundance_pojo_quad <- aggregate(x=Individual_Pojo_data$`Individual count`,
                                 by=list(Individual_Pojo_data$Site,Individual_Pojo_data$`Site number`), FUN=sum)
colnames(Abundance_pojo_quad) <- c("Site","Site number","Individual count")
Abundance_pojo_quad$Individual_count_m2 <- Abundance_pojo_quad$`Individual count`/(0.2*0.2)
Abundance_pojo_quad_Deep <- Abundance_pojo_quad[-c(1:6,9,14:29),]
Abundance_pojo_quad_Shallow <-Abundance_pojo_quad[c(1:6,9,14:29),]



##Mean abundance for deep sites##

Abundance_exp_deep <- aggregate(x=Abundance_exp_quad_Deep$Individual_count_m2, by=list(Abundance_exp_quad_Deep$`Site number`), FUN=mean)
colnames(Abundance_exp_deep)<-c("Site_number","Individual_count_per_m2")
Site_group = c("exposed", "exposed", "exposed","exposed")
Abundance_exp_deep <- cbind(Abundance_exp_deep,Site_group)


Abundance_semi_deep <- aggregate(x=Abundance_semi_quad_Deep$Individual_count_m2, by=list(Abundance_semi_quad_Deep$`Site number`), FUN=mean)
colnames(Abundance_semi_deep)<-c("Site_number","Individual_count_per_m2")
Site_group = c("semi", "semi", "semi","semi","semi")
Abundance_semi_deep <- cbind(Abundance_semi_deep,Site_group)



Abundance_pojo_deep <- aggregate(x=Abundance_pojo_quad_Deep$Individual_count_m2, 
                                 by=list(Abundance_pojo_quad_Deep$`Site number`), FUN=mean)
colnames(Abundance_pojo_deep)<-c("Site_number","Individual_count_per_m2")
Site_group = c("Pojo", "Pojo")
Abundance_pojo_deep <- cbind(Abundance_pojo_deep,Site_group)


Abundance_all_deep <-rbind(Abundance_exp_deep,Abundance_semi_deep,Abundance_pojo_deep)


##Boxplot of mean abundances (DEEP)#
Abundance_all_deep$Site_group <- factor(Abundance_all_deep$Site_group, 
                                        levels=c("exposed", "semi", "Pojo" ))
boxplot(Abundance_all_deep$Individual_count_per_m2~Abundance_all_deep$Site_group, 
        main= "Mean abundance in deep sites",col = c("green","magenta","cyan","blue"),
        xlab="Exposure",ylab="Individual count/m2")

Abundance_exp_semi_deep <- Abundance_all_deep[-c(10,11),]                     ##without pojo
abundance_exp5 <- data.frame(Site_number=c(5),Individual_count_per_m2=c(0),Site_group=c("exposed"))
Abundance_exp_semi_deep <- rbind(Abundance_exp_semi_deep,abundance_exp5)
Abundance_exp_semi_deep$Site_group <- factor(Abundance_exp_semi_deep$Site_group,
                                             levels=c("exposed","semi"))
Abundance_exp_semi_deep <- Abundance_exp_semi_deep[order(Abundance_exp_semi_deep$Site_group), ]

boxplot(Abundance_exp_semi_deep$Individual_count_per_m2~Abundance_exp_semi_deep$Site_group, 
        main= "Mean abundance in deep sites",col = c("green","magenta"),
        xlab="Exposure",ylab="Individual count/m2")





##Mean abundance for shallow sites##

Abundance_exp_shallow <- aggregate(x=Abundance_exp_quad_Shallow$Individual_count_m2, 
                                   by=list(Abundance_exp_quad_Shallow$`Site number`), FUN=mean)
colnames(Abundance_exp_shallow)<-c("Site_number","Individual_count_per_m2")
Site_group = c("exposed", "exposed", "exposed","exposed","exposed")
Abundance_exp_shallow <- cbind(Abundance_exp_shallow,Site_group)


Abundance_semi_shallow <- aggregate(x=Abundance_semi_quad_Shallow$Individual_count_m2, 
                                    by=list(Abundance_semi_quad_Shallow$`Site number`), FUN=mean)
colnames(Abundance_semi_shallow)<-c("Site_number","Individual_count_per_m2")
Site_group = c("semi", "semi", "semi","semi","semi")
Abundance_semi_shallow<- cbind(Abundance_semi_shallow,Site_group)

Abundance_shel_shallow <- aggregate(x=Abundance_shel_quad_Shallow$Individual_count_m2,
                                    by=list(Abundance_shel_quad_Shallow$`Site number`),FUN=mean)
colnames(Abundance_shel_shallow) <-c("Site_number","Individual_count_per_m2")
Site_group= c("sheltered","sheltered","sheltered","sheltered","sheltered")
Abundance_shel_shallow <- cbind(Abundance_shel_shallow, Site_group)

Abundance_pojo_shallow <- aggregate(x=Abundance_pojo_quad_Shallow$Individual_count_m2, 
                                    by=list(Abundance_pojo_quad_Shallow$`Site number`), FUN=mean)
colnames(Abundance_pojo_shallow)<-c("Site_number","Individual_count_per_m2")
Site_group = c("Pojo", "Pojo","Pojo","Pojo","Pojo")
Abundance_pojo_shallow <- cbind(Abundance_pojo_shallow,Site_group)


Abundance_all_shallow <-rbind(Abundance_exp_shallow,Abundance_semi_shallow, Abundance_shel_shallow,Abundance_pojo_shallow)


##Boxplot of mean abundances (Shallow)#
Abundance_all_shallow$Site_group <- factor(Abundance_all_shallow$Site_group, 
                                           levels=c("exposed", "semi", "sheltered","Pojo" ))
boxplot(Abundance_all_shallow$Individual_count_per_m2~Abundance_all_shallow$Site_group, 
        main= "Mean abundance in shallow sites",col = c("green","magenta","cyan","blue"),
        xlab="Exposure",ylab="Individual count/m2")

Abundance_exp_semi_shallow <- Abundance_all_shallow[-c(11:20),]                     ##without pojo
Abundance_exp_semi_shallow$Site_group <- factor(Abundance_exp_semi_shallow$Site_group,
                                                levels=c("exposed","semi"))
boxplot(Abundance_exp_semi_shallow$Individual_count_per_m2~Abundance_exp_semi_shallow$Site_group, 
        main= "Mean abundance in shallow sites",col = c("green","magenta"),
        xlab="Exposure",ylab="Individual count/m2")



##########################################################################
######### Boxplot of mean data from both depths in same figure ##########
##########################################################################

##depth column and combining data frames
Abundance_exp_semi_deep$Depth <- c(rep("Deep",10))
Abundance_exp_semi_shallow$Depth <- c(rep("Shallow",10))
Abundance_exp_semi <- rbind(Abundance_exp_semi_deep, Abundance_exp_semi_shallow)
Abundance_exp_semi$Site_group <- factor(Abundance_exp_semi$Site_group,
                                             levels=c("exposed","semi"))
Abundance_exp_semi$Depth <- factor(Abundance_exp_semi$Depth, levels = c("Deep", "Shallow"))
Abundance_exp_semi$Site_Depth <- factor(
  paste(Abundance_exp_semi$Site_group, Abundance_exp_semi$Depth, sep = "_"),
  levels = c(
    "exposed_Deep", "exposed_Shallow",
    "semi_Deep", "semi_Shallow"
  )
)

# Boxplot colors and legend colors
boxplot_colors <- c("lightblue","bisque","lightblue","bisque")
legend_colors <- c("lightblue","bisque")

# Create the initial boxplot without stripchart
boxplot(Individual_count_per_m2 ~ Site_group * Depth, data = Abundance_exp_semi,
        main = "Mean abundance at deep and shallow sites", boxwex = 0.5, col = boxplot_colors,
        xlab = NA, ylab = "Individual count per m2", lex.order = TRUE, xaxt = "n", cex.lab = 1.1, cex.main =1.5)

# Add the stripchart to the existing plot
stripchart(Individual_count_per_m2 ~ Site_Depth,
           data = Abundance_exp_semi,
           method = "jitter",
           at = c(1.4, 2.4, 3.4, 4.4),
           pch = 17,
           col = boxplot_colors,
           vertical = TRUE,
           add = TRUE)

# Labels for the x-axis
label = c("Exposed", "Semi-sheltered")

# Add custom x-axis
axis(1, at = seq(1.5, 3.5, 2), labels = label, tick = FALSE, cex.axis = 1.2)

# Add legend
legend("topleft", legend = c("Deep","Shallow"), fill = legend_colors)
