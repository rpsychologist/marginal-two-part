## Scenario 1 with more variance between subjects
require(powerlmm)
source("code/compute_setup.R")
source("code/sim_1_mod_setup.R")

p <- study_parameters(
    design = study_design(family = "hurdle"),
    n1 = 11,
    n2 = 50,
    T_end = 10,
    fixed_intercept = log(10), # median(Y)
    fixed_intercept_tx = 0,
    fixed_hu_intercept = qlogis(c(0.2)), # prop == 0
    fixed_hu_intercept_tx = 0, # prop == 0
    fixed_slope = 0,
    fixed_hu_slope = 0,
    sd_hu_intercept = c(2.5),
    sd_hu_slope = 0.1,
    sd_intercept = c(2),
    sd_slope = 0.05,
    cor_intercept_slope = -0.1,
    cor_intercept_hu_intercept = -0.8,
    cor_intercept_hu_slope = -0.2,
    cor_slope_hu_intercept = -0.15,
    cor_slope_hu_slope = -0.1,
    cor_hu_intercept_hu_slope = 0.15,
    shape = 1.6,
    RR_cont = c(0.2),
    OR_hu = 10,
    marginal = TRUE,
    family = "gamma")

# Sims --------------------------------------------------------------------
p_m <- update(p,
              n1 = c(3, 6, 11), 
              n2 = c(25, 50, 75, 150))

res <- simulate(p_m, 
                 formula = f,
                 nsim = nsim, # code/compute_setup.R
                 satterthwaite = FALSE,
                 CI = FALSE,
                 save = TRUE,
                 cores = cores)

saveRDS(res, file = "data/sim_4_variance.Rds")