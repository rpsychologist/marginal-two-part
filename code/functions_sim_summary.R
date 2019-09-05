###############################################
# Functions to summary the simulation results #
###############################################

#' Summarize simulation results for a specific "model"
#'
#' This function is within the `get_model_func` passed to `summary_one_scen`.
#' The purpose is to specify the required parameters need to summarise each model, 
#' as they are not identical to all model.
#' 
#' @param res a powerlmm simulation result object
#' @param model character; model name used in powerlmm's sim_formula_compare-function.
#' @param label character; label to use for the model
#' @param para character; which parameter to extrect
#' @param theta numeric; the parameters true value
#' @param scale numeric; used to rescale theta and estimates to represent difference at posttest (time = 10)
#' @param link charecter; either "log" or "response"
#'
#' @return a data.frame
summary_sim <- function(res, 
                        model, 
                        label, 
                        para, 
                        theta, 
                        scale = 10, 
                        link = "log") {
    
    x <-  res$res[[model]]$FE
    
    if(model != "hurdle_gamma_MTP") {
        x <- x %>% 
            mutate(CI_lwr = estimate - 1.96 * se,
                   CI_upr = estimate + 1.96 * se) 
    }
    
    x %>% 
        filter(parameter == !!para) %>% 
        summarise(CI_cover = mean(CI_lwr < theta/scale & CI_upr > theta/scale),
                  Power = mean(sign(CI_lwr) == sign(CI_upr)),
                  est = median(estimate * scale),
                  est_RB = (est - theta)/theta,
                  est_mean = mean(estimate*scale),
                  est_mean_RB = (est_mean - theta)/theta,
                  RMSE = sqrt(mean((estimate*scale - theta)^2)),
                  se = mean(se),
                  emp_SE = sd(estimate * scale)) %>% 
        add_column(model = label, 
                   theta = theta, 
                   link = link, 
                   .before = 1)
}


#' Summarize one simulation scenario
#'
#' @param res a plcp_multi_sim-object
#' @param lab character; label used to name the scenario
#' @param get_model_func function used to speficy how each model should be summarised.
#' see `get_all_models`
#' @return a data.frame
summary_one_scen <- function(res, 
                             lab, 
                             get_model_func) {
    
    i <- 1
    # thetas
    p <- res[[i]]$paras
    # marginalize thetas
    m <- marginalize(p, R = 1e5)
    # marginal theta on response scale
    theta <- m$post %>% 
        filter(var == "marg_post_diff") %>% 
        pull(est)
    theta_log <- p$fixed_intercept_tx + log(p$RR_cont)
    theta_logodds <- p$fixed_hu_intercept_tx + log(p$OR_hu)
    
    res_sum <- map_df(res, 
                      get_model_func, 
                      theta = theta, 
                      theta_log = theta_log, 
                      theta_logodds = theta_logodds)
    
    
    res_sum %>% 
        mutate(n1 = factor(n1, ordered = TRUE),
               lab = lab,
               theta_exp_pre = exp(p$fixed_intercept_tx),
               theta_hu_exp_pre = exp(p$fixed_hu_intercept_tx),
               theta_exp = exp(theta_log), 
               theta_hu_exp = exp(theta_logodds))
}

#' Summarize multiple simulations
#' 
#' This is the function called when creating the simulation results
#'
#' @param objects a list with one or more powerlmm plcp_multi_sim-objects
#' @param labels character; used to label each item in `objects`
#' @param get_model_func see `summary_one_scen`
#'
#' @return a data.frame
summarize_multiple_objects <- function(objects, 
                                       labels, 
                                       get_model_func) {
    stopifnot(is.list(objects))
    stopifnot(length(objects) == length(labels))
    
    map2_df(objects, 
            labels, 
            summary_one_scen,
            get_model_func)
}


# argument: get_model_func functions --------------------------------------------
#' Create a summary data.frame based on one simulation 
#' 
#' This function is passed to `summary_one_scen`
#'
#' @param res a powerlmm plcp_sim_formula_compare-object. Contains a single simulation with multiple models evaluated.
#' @param theta the marginal/population-average treatment effect on the response scale
#' @param theta_log treatment effect on log scale
#' @param theta_logodds treatment effect for binary part (logodds)
#'
#' @return a data.frame where each model is summarized on 1 row
#' @export
#'
#' @examples
get_all_models <- function(res,
                           theta, 
                           theta_log,
                           theta_logodds) {
    x <- rbind(summary_sim(res, "raw", "gaussian", para = "time:treatment", theta, scale = 10, link = "response"),
               summary_sim(res, "hurdle_gamma_MTP", "2P-diff",para = "marg_post_diff", theta, scale = 1, link = "response"),
               summary_sim(res, "logtrans", "log+1", para = "time:treatment", theta_log, scale = 10),
               summary_sim(res, "logtrans2", "log+0.0001", para = "time:treatment", theta_log, scale = 10),
               summary_sim(res, "hurdle_gamma_MTP", "2P-log", para = "time:treatment", theta_log, scale = 10),
               summary_sim(res, "hurdle_gamma_MTP", "2P-logodds", para = "hu_time:treatment", theta_logodds, scale = 10))
    
    p <- res$paras
    
    x <- x %>% add_column(n1 = p$n1, 
                          n2 = p$n2)
    x
}

# See `get_all_models`
get_all_models_2 <- function(res,
                             theta, 
                             theta_log,
                             theta_logodds) {
    x <- rbind(summary_sim(res, "raw", "gaussian", para = "TE", theta, scale = 1, link = "response"),
               summary_sim(res, "hurdle_gamma_MTP", "2P-diff", para = "marg_post_diff", theta, scale = 1, link = "response"),
               summary_sim(res, "logtrans", "log+1", para = "TE", theta_log, scale = 1),
               summary_sim(res, "logtrans2", "log+0.0001", para = "TE", theta_log, scale = 1),
               summary_sim(res, "hurdle_gamma_MTP", "2P-log", para = "TE_log", theta_log, scale = 1))
    
    p <- res$paras
    
    x <- x %>% add_column(n1 = p$n1, 
                          n2 = p$n2)
    x
}


