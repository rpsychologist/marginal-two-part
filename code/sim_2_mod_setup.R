
require(brms)
source("code/sim_2_setup.R")
source("code/custom_brms.R")

# lmer: get effects at posttest
post_test <- function(fit, d = NULL) {
    err_func <- function(e) {
        data.frame(parameter = "TE",
                   Estimate = NA,
                   `Std. Error` = NA,
                   `Pr(>|t|)` = NA,
                   df = NA,
                   df_bw = NA, 
                check.names = FALSE)
    }
    
    res <- tryCatch(lmerTest::contest1D(fit, c(0, 0, 1, 10)), 
                    error = err_func)
    out <- data.frame(parameter = "TE",
                      estimate = res$Estimate,
                      se = res$`Std. Error`,
                      pval = res$`Pr(>|t|)`,
                      df = res$df,
                      df_bw = NA)
    
    out
}

# brms: get effects at posttest
post_brms <- function(fit, d = NULL) {
    err_func <- function(e) {
        data.frame(parameter = "TE_log",
                   estimate = NA,
                   se = NA,
                   CI_lwr = NA,
                   CI_upr = NA)
    }
    out <- tryCatch({
        ss <- brms::posterior_samples(fit, 
                                pars = c("b_treatment", "b_time:treatment"))
        ss <- ss[,1] + ss[,2] * 10
        out <- data.frame(parameter = "TE_log",
                          estimate = median(ss),
                          se = sd(ss),
                          CI_lwr = quantile(ss, 0.025),
                          CI_upr = quantile(ss, 0.975))
        },
        error = err_func)

    out
}

d <- simulate_data(p0)

# priors
prior <- c(prior(student_t(3, 4, 10), class = "Intercept"),
           prior(student_t(3, 0, 10), class = "sd", dpar = "hu"),
           prior(student_t(3, 0, 10), class = "sd"),
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

# sim_formula for brms model
f0 <- sim_formula(bfit_gamma_mtp, iter = 2000, marginalize = TRUE, post_test = post_brms)

# combine sim formulas
f <- sim_formula_compare("raw"= sim_formula("y ~ time * treatment + (1 + time | subject)",
                                            post_test = post_test),
                         "logtrans" = sim_formula("log(y + 1) ~ time * treatment + (1 + time | subject)",
                                                  post_test = post_test),
                         "logtrans2" = sim_formula("log(y + 0.0001) ~ time * treatment + (1 + time | subject)",
                                                   post_test = post_test),
                         "hurdle_gamma_MTP" = f0)
