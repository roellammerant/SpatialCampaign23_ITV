########### Approach nested rank test ############

### Rank site groups within a categroy by there median
Rank_Height_Shallow <-aggregate(x=Shallow_Height$CWM_Height, 
                                by=list(Shallow_Height$Site_Exposure), FUN=median)
names(Rank_Height_Shallow)[1:2]<-paste(c("Site","CWM_Height"))
Rank_Height_Shallow$Rank <- c("5","4","1","2","3",
                              "1","3","4","5","2",
                              "2","1","4","3","5",
                              "1","4","5","3","2")
Shallow_Height$Site <- c(rep("5",8),rep("4",5),rep("1",6),rep("2",6),rep("3",6),
                         rep("1",6),rep("3",6),rep("4",6),rep("5",6),rep("2",6),
                         rep("2",6),rep("1",6),rep("4",6),rep("3",6),rep("5",6),
                         rep("1",6),rep("4",6),rep("5",6),rep("3",6),rep("2",6))

## Sites between exposure levels are compared based on there rank
## two sites with the same ranking will form one group of CWM values
## all the CWM for each group seperately get ranked without regard to exposure (Normal rank sum test)
## group specific between exposure Z-scores are caculated 
## then the Z-scores are aggregated by weighting each group's contribution by sample size.

shallow_HeightA <- Shallow_Height[c(1:31, 32:61),] #Exposed and Semi
Shallow_HeightB <- Shallow_Height[c(32:61, 92:121),] #Semi and sheltered
Shallow_HeightC <- Shallow_Height[c(92:121,62:91),] #Sheltered and Pojo

  
  

nestedRanksTest(CWM_Height ~ Exposure | Site_number, n.iter = 1000,
                data = Shallow_HeightA) #Non significant

nestedRanksTest(CWM_Height ~ Exposure | Site_number, n.iter = 1000,
                data = Shallow_HeightB) #Significant

nestedRanksTest(CWM_Height ~ Exposure | Site_number, n.iter = 1000,
                data = Shallow_HeightC) # marginally significant



nestedRanksTest(FDis_Height ~ Exposure | Site_number, n.iter = 1000,
                data = Shallow_HeightA) #Non significant

nestedRanksTest(FDis_Height ~ Exposure | Site_number, n.iter = 1000,
                data = Shallow_HeightB) #Non-Significant

nestedRanksTest(FDis_Height ~ Exposure | Site_number, n.iter = 1000,
                data = Shallow_HeightC) #Non-Significant




