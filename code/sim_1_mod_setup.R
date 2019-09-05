
require(brms)
source("code/sim_1_setup.R")
source("code/custom_brms.R")

logtrans <- function(shift = 1) {
    function(data) {
        data$y <- log(data$y + shift)
        data   
    }
}


d <- simulate_data(p0)

prior <- c(prior(student_t(3, 0, 10), class = "Intercept"),
           prior(student_t(3, 0, 10), class = sd, dpar = hu),
           prior(student_t(3, 0, 10), class = sd),
           prior(gamma(0.01, 0.01), class = "shape"),
           prior("lkj(1)", class = "cor")
           )

# compile brms model
bfit_gamma_mtp <- brm(bf(y ~ time * treatment + (1 + time | c | subject),
                         hu ~ time * treatment + (1 + time | c | subject)),
                      data = d,
                      family = hurdle_gamma_mtp,
                      stanvar = hurdle_gamma_mtp_stanvars,
                      prior = prior,
                      chains = 1,
                      cores = 1,
                      iter = 1)

f0 <- sim_formula(bfit_gamma_mtp, 
                  iter = 2000, 
                  marginalize = TRUE)

# Sims to compare
f <- sim_formula_compare("raw"= sim_formula("y ~ time * treatment + (1 + time | subject)"),
                         "logtrans" = sim_formula("log(y + 1) ~ time * treatment + (1 + time | subject)"),
                         "logtrans2" = sim_formula("log(y + 0.0001) ~ time * treatment + (1 + time | subject)"),
                         "hurdle_gamma_MTP" = f0)
