require(powerlmm)

p0 <- study_parameters(
    design = study_design(family = "hurdle"),
    n1 = 11,
    n2 = 50,
    T_end = 10,
    fixed_intercept = log(15), # median(Y)
    fixed_intercept_tx = 0,
    fixed_hu_intercept = qlogis(0.5), # prop == 0
    fixed_hu_intercept_tx = 0, # prop == 0
    fixed_slope = 0,
    fixed_hu_slope = log(2)/10,
    sd_hu_intercept = 1.5,
    sd_hu_slope = 0.1,
    sd_intercept = 1,
    sd_slope = 0.05,
    cor_intercept_slope = -0.1,
    cor_intercept_hu_intercept = -0.8,
    cor_intercept_hu_slope = -0.2,
    cor_slope_hu_intercept = -0.15,
    cor_slope_hu_slope = -0.1,
    cor_hu_intercept_hu_slope = 0.15,
    shape = 1.6,
    RR_cont = c(0.5),
    OR_hu = c(1/4),
    marginal = TRUE,
    family = "gamma")

# p_grid <- update(p, n1 = c(3, 5, 12), 
#                  n2 = c(25, 50, 75))
