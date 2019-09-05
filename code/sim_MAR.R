# MAR missing data simulation

source("code/compute_setup.R")
source("code/sim_2_mod_setup.R")
source("code/functions_MAR.R")

require(parallel) 
require(tidyverse) # dplyr used with `data_transform`

f0 <- sim_formula(bfit_gamma_mtp, 
                  iter = 2000, 
                  marginalize = TRUE, 
                  data_transform = add_MAR_missing,
                  post_test = post_brms)

f <- sim_formula_compare("raw"= sim_formula("y ~ time * treatment + (1 + time | subject)",
                                            post_test = post_test,
                                            data_transform = add_MAR_missing),
                         "logtrans" = sim_formula("log(y + 1) ~ time * treatment + (1 + time | subject)",
                                                  post_test = post_test,
                                                  data_transform = add_MAR_missing),
                         "logtrans2" = sim_formula("log(y + 0.0001) ~ time * treatment + (1 + time | subject)",
                                                   post_test = post_test,
                                                   data_transform = add_MAR_missing),
                         "hurdle_gamma_MTP" = f0,
                         "t_test" = sim_formula("y ~ treatment", 
                                                data_transform = add_MAR_missing_post,
                                                test = "treatment"),
                         "t_test_logtrans" = sim_formula("log(y + 1) ~ treatment", 
                                                         data_transform = add_MAR_missing_post,
                                                         test = "treatment"),
                         "t_test_logtrans2" = sim_formula("log(y + 0.0001) ~ treatment", 
                                                          data_transform = add_MAR_missing_post,
                                                          test = "treatment"),
                         "ANCOVA" = sim_formula("y ~ treatment", 
                                                data_transform = add_MAR_missing_post,
                                                test = "treatment"),
                         "ANCOVA_logtrans" = sim_formula("log(y + 1) ~ pre + treatment", 
                                                         data_transform = add_MAR_missing_post,
                                                         test = "treatment"),
                         "ANCOVA_logtrans2" = sim_formula("log(y + 0.0001) ~ pre + treatment", 
                                                          data_transform = add_MAR_missing_post,
                                                          test = "treatment")
                         )


p_m <- update(p0,
              n2 = c(25, 50, 75, 150))

res <- simulate(p_m,
                formula = f,
                nsim = nsim,  # code/compute_setup.R
                satterthwaite = FALSE,
                CI = FALSE,
                save = TRUE,
                cores = cores)

saveRDS(res, file = "data/sim_MAR.Rds")

