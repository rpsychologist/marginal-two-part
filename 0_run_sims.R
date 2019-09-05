# Required directories
# `save/` used by powerlmm to save intermediate results
stopifnot(dir.exists("save")) 
# Completed simulations are saved to `data\`
stopifnot(dir.exists("data"))

# Run all sims
source("code/sim_1_0.R")
source("code/sim_1_2.R")
source("code/sim_2_2.R")
source("code/sim_3.R")
source("code/sim_4_variance.R")
source("code/sim_MAR.R")

# Prepare and save results
source("code/sim_summary.R")
