##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##
library(rstatix)

### Exposed_Shallow
Indices_height_Allsites[c(1:31),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(1:31),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(1:31),c(6)])) # marginally significant

### Exposed_Deep
Indices_height_Allsites[c(31:57),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(31:57),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(31:57),c(6)])) # significant

### Semi_Shallow
Indices_height_Allsites[c(58:87),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(58:87),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(58:87),c(6)])) # significant

### Semi_Deep
Indices_height_Allsites[c(88:117),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(88:117),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(88:117),c(6)])) # significant

### Pojo_Deep
Indices_height_Allsites[c(118:125),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(118:125),c(5)])) # non-significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(118:125),c(6)])) # significant

### Pojo_Shallow
Indices_height_Allsites[c(126:155),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(126:155),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(126:155),c(6)])) # significant

### Sheltered_Shallow
Indices_height_Allsites[c(156:185),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(156:185),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(156:185),c(6)])) # significant



