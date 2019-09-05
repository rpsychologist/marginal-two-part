require(powerlmm)
require(tidyverse)
res <- readRDS("data/sim_MAR.Rds")

# Get thetas
i <- 1
p <- res[[i]]$paras
m <- marginalize(p, 
                 R = 1e5)
theta <- m$post %>% 
  filter(var == "marg_post_diff") %>% 
  pull(est)
theta_log <- p$fixed_intercept_tx + log(p$RR_cont)
theta_logodds <- p$fixed_hu_intercept_tx + log(p$OR_hu)

# Summarize
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
                  est_mean = mean(estimate * scale),
                  est_mean_RB = (est_mean - theta)/theta,
                  RMSE = sqrt(mean((estimate * scale - theta)^2)),
                  se = mean(se),
                  se_RB = (se - sd(estimate))/sd(estimate),
                  emp_SE = sd(estimate * scale)) %>% 
        add_column(model = label,
                   theta = theta, 
                   link = link, 
                   .before = 1)
}

get_all_models <- function(res) {
    x <- rbind(summary_sim(res, "raw", "gaussian", para = "TE", theta, scale = 1, link = "response"),
               summary_sim(res, "hurdle_gamma_MTP", "2P-diff",para = "marg_post_diff", theta, scale = 1, link = "response"),
               summary_sim(res, "logtrans", "log+1", para = "TE", theta_log, scale = 1),
               summary_sim(res, "logtrans2", "log+0.0001", para = "TE", theta_log, scale = 1),
               summary_sim(res, "hurdle_gamma_MTP", "2P-log", para = "TE_log", theta_log, scale = 1),
               summary_sim(res, "t_test", "t-test_raw", para = "treatment", theta, scale = 1, link = "response"),
               summary_sim(res, "t_test_logtrans", "t-test_logtrans", para = "treatment", theta_log, scale = 1),
               summary_sim(res, "ANCOVA", "ANCOVA_raw", para = "treatment", theta, scale = 1, link = "response"),
               summary_sim(res, "ANCOVA_logtrans", "ANCOVA_logtrans", para = "treatment", theta_log, scale = 1)
               
               )
    
    p <- res$paras
    
    x <- x %>% add_column(n1 = p$n1, n2 = p$n2)
    x
}

# Combine
res_sum <- map_df(res, 
                  get_all_models)

res_sum <- res_sum %>% 
    mutate(n1 = factor(n1, 
                       ordered = TRUE))

# Table with results
res_sum %>% 
    filter(model != "log+0.0001") %>% 
    rename(type_1 = Power, scale = link) %>% 
    mutate(est = round(est, 1),
           RMSE = round(RMSE, 1),
           se_RB = round(se_RB, 2),
           type_1 = round(type_1, 2), 
           n1 = factor(n1, levels = c(11, 6, 3)),
           model = case_when(model == "2P-log" ~ "Two-part", 
                             model == "2P-diff" ~ "Two-part",
                             model == "ANCOVA_logtrans" ~ "ANCOVA (log+1)",
                             model == "log+1" ~ "LMM (log+1)",
                             model == "gaussian" ~ "LMM",
                             model == "ANCOVA_raw" ~"ANCOVA",
                             model == "t-test_logtrans" ~ "t-test (log+1)",
                             model == "t-test_raw" ~ "t-test",
                             TRUE ~ model),
           model = factor(model, levels = c("t-test (log+1)", 
                                           "ANCOVA (log+1)", 
                                           "LMM (log+1)",
                                           "Two-part",
                                           "t-test",
                                           "ANCOVA",
                                           "LMM"))
  ) %>% 
    arrange(scale, model) %>% 
    group_by(model, scale) %>% 
    mutate(model2 = c(as.character(model[1]), "", "", "")) %>% 
    ungroup() %>% 
    select(model2, n2, CI_cover, type_1, est, RMSE, se_RB, -theta, -est_RB, -n1) %>% 
    as.data.frame %>% 
format(digits = 2, round = 1, scientific = FALSE, nsmall = 0) %>% 
    knitr::kable(format = "latex", booktabs = T)

