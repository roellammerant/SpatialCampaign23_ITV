##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##
library(rstatix)

### Exposed_Deep
Indices_Ratio_Allsites[c(1:26),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(1:26),c(2)])) # significant
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(1:26),c(5)])) # significant

### Exposed_Shallow
Indices_Ratio_Allsites[c(31:57),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(27:57),c(2)])) # non-significant
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(27:57),c(5)])) # significant

### Semi_Deep
Indices_Ratio_Allsites[c(58:87),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(58:87),c(2)])) # significant
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(58:87),c(5)])) # significant

### Semi_shallow
Indices_Ratio_Allsites[c(88:117),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(88:117),c(2)])) # significant
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(88:117),c(5)])) # significant

### Pojo_Deep
Indices_Ratio_Allsites[c(118:125),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(118:125),c(2)])) # non-significant
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(118:125),c(5)])) # non-significant

### Pojo_Shallow
Indices_Ratio_Allsites[c(126:155),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(126:155),c(2)])) # significant
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(126:155),c(5)])) # significant

### Sheltered_Shallow
Indices_Ratio_Allsites[c(156:185),c(6)]
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(156:185),c(2)])) # significant
mshapiro_test(as.numeric(Indices_Ratio_Allsites[c(156:185),c(5)])) # significant



