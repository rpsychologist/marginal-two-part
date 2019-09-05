# #################################################################
# Custom brms family for marginal two-part gamma/lognormal models #
###################################################################

# Custom marginal Gamma two-part family ------------------------------------------
hurdle_gamma_mtp <- custom_family(
    "hurdle_gamma_mtp",
    dpars = c("shape","mu", "hu"),
    lb = c(0, NA, 0),
    ub = c(NA, NA, 1),
    links = c("log", "identity", "logit"),
    type = "real"
)

scode <- "
/* hurdle gamma log-PDF of a single response
* logit parameterization of the hurdle part
* Args:
*   y: the response value
*   alpha: shape parameter of the gamma distribution
*   beta: rate parameter of the gamma distribution
*   hu: linear predictor for the hurdle part
* Returns:
*   a scalar to be added to the log posterior
*/
real hurdle_gamma_mtp_lpdf(real y, real alpha, real mu, real hu) {
    if (y == 0) {
        return bernoulli_logit_lpmf(1 | hu);
    } else {
        real beta;
        beta = alpha * exp(-(mu - log1m_inv_logit(hu)));
        return bernoulli_logit_lpmf(0 | hu) +
        gamma_lpdf(y | alpha, beta);
}
}
"
hurdle_gamma_mtp_stanvars <- stanvar(scode = scode,
                                     block = "functions")

# Custom lognormal marginal two-part family ------------------------------------------
hurdle_lognormal_mtp <- custom_family(
    "hurdle_lognormal_mtp",
    dpars = c("mu", "sigma", "hu"),
    lb = c(NA, 0, 0),
    ub = c(NA, NA, 1),
    links = c("identity", "log", "logit"), type = "real"
)

hurdle_lognormal_mtp$specials <- "logscale"

scode_lognormal_mtp <- "
  /* hurdle lognormal log-PDF of a single response
   * Args:
   *   y: the response value
   *   mu: mean parameter of the lognormal distribution
   *   sigma: sd parameter of the lognormal distribution
   *   hu: hurdle probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real hurdle_lognormal_mtp_lpdf(real y, real mu, real sigma, real hu) {
        if (y == 0) {
            return bernoulli_logit_lpmf(1 | hu);
        } else {
            real mu2;
            mu2 = mu - log1m_inv_logit(hu) - (sigma*sigma)/2;
            return bernoulli_logit_lpmf(0 | hu) +
                lognormal_lpdf(y | mu2, sigma);
    }
  }
"
hurdle_lognormal_mtp_stanvars<- stanvar(scode = scode_lognormal_mtp,
                                  block = "functions")




# Postprocessing --------------------------------------------------------
## The following function adds support for the custom hurdle family to the
## fitted, predict, and log_lik (and loo and waic) generics.
## Thus, do not call these methods directly

## Gamma MTP models
predict_hurdle_gamma_mtp <- function (i, draws, ...) {
    
    # hu = Pr(Y = 0)
    hu <- draws$dpars$hu[, i]
    shape <- draws$dpars$shape
    
    # mu on log scale
    # solve for g(y | y > 0)
    mu <- draws$dpars$mu[, i] - log(1 - hu)
    ndraws <- draws$nsamples

    # Sample observations
    prob <- runif(ndraws, 0, 1)
    ifelse(prob < hu, 
           0, 
           rgamma(ndraws, 
                  shape = shape, 
                  rate = shape * exp(-mu))
           )
}

fitted_hurdle_gamma_mtp <- function (draws, ...) {
    with(draws$dpars, exp(mu))
}

log_lik_hurdle_gamma_mtp <- function (i, draws) 
{
    hu <- draws$dpars$hu[, i]
    shape <- draws$dpars$shape
    mu <- draws$dpars$mu[, i] - log(1 - hu)
    scale <- exp(mu)/shape
    y <- draws$data$Y[i]
     
    # log_lik hurdle gamma
     if(y == 0) log(hu) else log(1 - hu) + dgamma(y, 
                                                  shape = shape, 
                                                  scale = scale, 
                                                  log = TRUE)
}

## Lognormal MTP model
predict_hurdle_lognormal_mtp<- function (i, draws, ...) {
    
    # probs Y = 0
    hu <- draws$dpars$hu[, i]
    
    # mu on log scale
    # solve for g(Y | y > 0)
    sigma <- draws$dpars$sigma
    mu <- draws$dpars$mu[, i] - log(1-hu) - sigma^2/2
    
    ndraws <- draws$nsamples
    prob <- runif(ndraws, 0, 1)
    ifelse(prob < hu, 
           0, 
           rlnorm(ndraws, 
                  meanlog = mu, 
                  sdlog = sigma))
}
fitted_hurdle_lognormal_mtp <- function (draws, ...) {
    with(draws$dpars, exp(mu))
}
log_lik_hurdle_lognormal_mtp <- function (i, draws) 
{
    hu <- draws$dpars$hu[, i]
    sigma <- draws$dpars$sigma
    nu <- draws$dpars$mu[, i] 
    mu <- nu - log(1 - hu) - sigma^2/2

    y <- draws$data$Y[i]
    
    # log_lik hurdle LN
    if(y == 0) log(hu) else log(1 - hu) + dlnorm(y, 
                                                meanlog = mu, 
                                                sdlog = sigma, 
                                                log = TRUE)
}

