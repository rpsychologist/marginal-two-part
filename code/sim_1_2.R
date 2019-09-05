## Scenario 1
source("code/compute_setup.R")
source("code/sim_1_mod_setup.R")

# Sims --------------------------------------------------------------------
p_m <- update(p2,
              n1 = c(3, 6, 11), 
              n2 = c(25, 50, 75, 150))

res <- simulate(p_m,
                 formula = f,
                 nsim = nsim,  # code/compute_setup.R
                 satterthwaite = FALSE,
                 CI = FALSE,
                 save = TRUE,
                 cores = cores)

saveRDS(res, file = "data/sim_1_2.Rds")