##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##
library(rstatix)

### Exposed_Shallow
Indices_leaf_Allsites[c(1:31),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(1:31),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(1:31),c(3)])) # significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(1:31),c(6)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(1:31),c(7)])) # significant

### Exposed_Deep
Indices_leaf_Allsites[c(31:57),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(31:57),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(31:57),c(3)])) # significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(31:57),c(6)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(31:57),c(7)])) # significant

### Semi_Shallow
Indices_leaf_Allsites[c(58:87),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(58:87),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(58:87),c(3)])) # significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(58:87),c(6)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(58:87),c(7)])) # significant

### Semi_Deep
Indices_leaf_Allsites[c(88:117),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(88:117),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(88:117),c(3)])) # significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(88:117),c(6)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(88:117),c(7)])) # significant

### Pojo_Deep
Indices_leaf_Allsites[c(118:125),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(118:125),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(118:125),c(3)])) # non-significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(118:125),c(6)])) # non-significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(118:125),c(7)])) # non-significant

### Pojo_Shallow
Indices_leaf_Allsites[c(126:155),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(126:155),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(126:155),c(3)])) # significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(126:155),c(6)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(126:155),c(7)])) # significant

### Sheltered_Shallow
Indices_leaf_Allsites[c(156:185),c(10)]

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(156:185),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(156:185),c(3)])) # significant

mshapiro_test(as.numeric(Indices_leaf_Allsites[c(156:185),c(6)])) # significant
mshapiro_test(as.numeric(Indices_leaf_Allsites[c(156:185),c(7)])) # significant
