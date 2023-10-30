library(tictoc)

# Time original method
tic("Original method")
source("scripts/original.R")
toc()

# Time parallel loop method
tic("parallel loop method runtime")
source("scripts/parallel_loop_method.R")
toc()

# Time parallel MTweedieTests method
tic("Parallel MTweedieTests method runtime")
source("scripts/parallel_mtweedietests_method.R")
toc()
 