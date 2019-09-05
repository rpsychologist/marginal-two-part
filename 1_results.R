##############################
# Create the result figures  #
##############################
#

# Folder `figures/` must exist
stopifnot(dir.exists("figures"))

require(tidyverse)
require(ggstance)
require(cowplot)
theme_set(theme_minimal())


# Scenario 0) no effect & 1) control relapse
res1 <- readRDS("data/sim1_sum.Rds")

# Scenario 2) control heavy gamble
res2 <- readRDS("data/sim2_sum.Rds")
# res2 <- filter(res2, lab == 2) %>% 
#     mutate(lab = 2)

# Scenario 3) difference in zeros
res3 <- readRDS("data/sim3_sum.Rds") 

# Scenario 4) More variance
res4 <- readRDS("data/sim4_sum.Rds") 

res_sum <- rbind(res1, 
                 res2,
                 res3,
                 res4)

alpha_linerange <- 0.25
theme_cust <- theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 7, 
                                     angle = 45,
                                     hjust = 1),
          axis.text.y = element_text(size = 8),
          panel.background = element_rect(fill = "white", 
                                          color = "white")
    )

theme_set(theme_cust)


res_sum <- res_sum %>% 
    filter(!model %in% c("log+0.0001", "2P-logodds")) %>% 
    mutate(n1 = factor(n1, levels = c(11, 6, 3)),
           model = case_when(model == "2P-log" ~ "Two-part", 
                             model == "2P-diff" ~ "Two-part",
                             model == "log+1" ~ "LMM (log y+1)",
                             model == "gaussian" ~ "LMM",
                             TRUE ~ model),
           model = factor(model, levels = c("LMM (log y+1)",
                                            "LMM", 
                                            "Two-part"))
           )


# Cover
p_cov_log <- res_sum %>% 
    #filter(model != "log+1") %>% 
    filter(link == "log") %>% 
    arrange(desc(n1)) %>% 
    
    ggplot(aes(CI_cover,model, color = n1)) + 
    geom_line(aes(group = rev(n1))) +
    geom_point() + #position = position_dodgev(height = .3)
    #geom_linerangeh(aes(xmin = CI_cover, xmax = 0.95), position = position_dodgev(height = .3), alpha = alpha_linerange) +
    annotate("rect", xmin = 0.925, xmax = 0.975, ymin = -Inf, ymax = Inf, alpha = 0.1) +
    geom_vline(xintercept = 0.95, linetype = "dotted") +
    facet_grid(n2 ~ lab, scales = "free_x") +
    labs(x = "95% CI Coverage", y = NULL) +
    scale_color_viridis_d(direction = 1) 

p_cov_response <- res_sum %>% 
    #filter(model != "log+1") %>% 
    filter(link == "response") %>% 
    arrange(desc(n1)) %>% 
    
    ggplot(aes(CI_cover,model, color = n1)) + 
    geom_line(aes(group = rev(n1))) +
    geom_point() + #position = position_dodgev(height = .3)
    #geom_linerangeh(aes(xmin = CI_cover, xmax = 0.95), position = position_dodgev(height = .3), alpha = alpha_linerange) +
    annotate("rect", xmin = 0.925, xmax = 0.975, ymin = -Inf, ymax = Inf, alpha = 0.1) +
    geom_vline(xintercept = 0.95, linetype = "dotted") +
    labs(x = "95% CI Coverage", y = NULL) +
    facet_grid(n2 ~ lab, scales = "free_x") +
    scale_color_viridis_d(direction = 1) 

legend <- get_legend(p_cov_log + theme(legend.position = "bottom"))
#ggsave(filename = "figures/sim_legend.pdf", legend, useDingbats = FALSE)

p_cov <- plot_grid(p_cov_log, 
                   p_cov_response, 
                   ncol = 1, 
                   align = "v")


# Power
p_power_log <- res_sum %>% 
    filter(link == "log", lab != 0) %>% 
    arrange(desc(n1)) %>% 
    ggplot(aes(Power,model, color = n1)) + 
    geom_line(aes(group = rev(n1))) +
    geom_point() + 
    #geom_linerangeh(aes(xmin = 0, xmax = Power), position = position_dodgev(height = .3), alpha = alpha_linerange) +
    #annotate("rect", xmin = 0.925, xmax = 0.975, ymin = -Inf, ymax = Inf, alpha = 0.1) +
    geom_vline(xintercept = 0.8, linetype = "dotted") +
    labs(x = "Power", y = NULL) +
    facet_grid(n2~lab) +
    scale_color_viridis_d(direction = 1) 

p_power_response <- res_sum %>% 
    filter(link == "response", lab != 0) %>% 
    arrange(desc(n1)) %>% 
    ggplot(aes(Power,model, color = n1)) + 
    geom_line(aes(group = rev(n1))) +
    geom_point() + 
    #geom_linerangeh(aes(xmin = 0, xmax = Power), position = position_dodgev(height = .3), alpha = alpha_linerange) +
    #annotate("rect", xmin = 0.925, xmax = 0.975, ymin = -Inf, ymax = Inf, alpha = 0.1) +
    geom_vline(xintercept = 0.8, linetype = "dotted") +
    labs(x = "Power", y = NULL) +
    facet_grid(n2~lab) +
    scale_color_viridis_d(direction = 1) 

p_power <- plot_grid(p_power_log, p_power_response, ncol = 1, align = "v")

# RMSE
p_RMSE_log <- res_sum %>%
    filter(link == "log") %>%
    arrange(desc(n1)) %>% 
    
    ggplot(aes(RMSE, model, color = factor(n1))) +
    geom_line(aes(group = rev(n1))) +
    geom_point() +
    #geom_linerangeh(aes(xmin = 0, xmax = RMSE), position = position_dodgev(height = .3), alpha = alpha_linerange) +
    #annotate("rect", ymin = 0.925, ymax = 0.975, xmin = -Inf, xmax = Inf, alpha = 0.1) +
    #geom_hline(yintercept = 0.95, linetype = "dotted") +
    facet_grid(n2~lab, scales = "free") +
    labs(x = "RMSE", y = NULL) +
    scale_color_viridis_d(direction = 1)


p_RMSE_response <- res_sum %>% 
    filter(link == "response") %>% 
    arrange(desc(n1)) %>% 
    
    ggplot(aes(RMSE, model, color = factor(n1))) + 
    geom_line(aes(group = rev(n1))) +
    geom_point() + 
    # geom_linerangeh(aes(xmin = 0, xmax = RMSE),
    #                 position = position_dodgev(height = .3), alpha = alpha_linerange) +
    #annotate("rect", ymin = 0.925, ymax = 0.975, xmin = -Inf, xmax = Inf, alpha = 0.1) +
    #geom_hline(yintercept = 0.95, linetype = "dotted") +
    facet_grid(n2~lab, scales = "free") +
    labs(x = "RMSE", y = NULL) +
    scale_color_viridis_d(direction = 1) 

p_RMSE <- plot_grid(p_RMSE_log, 
                    p_RMSE_response, 
                    ncol = 1, 
                    align = "v")


# RB
p_RB_log <- res_sum %>% 
    mutate(est_mean_RB = case_when(theta == 0 ~ est_mean,
                                   TRUE ~ est_mean_RB)) %>% 
    filter(link == "log", lab != 0) %>% 
    arrange(desc(n1)) %>% 
    ggplot(aes(est_mean_RB, model, color = n1)) + 
    geom_line(aes(group = rev(n1))) +
    geom_point() + 

    # geom_linerangeh(aes(xmin = 0,
    #                     xmax = est_mean_RB), 
    #                 position = position_dodgev(height = .3), 
    #                 alpha = alpha_linerange) +
    annotate("rect", xmin = -0.05, xmax = 0.05, ymin = -Inf, ymax = Inf, alpha = 0.1) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    labs(x = "Relative bias", y = NULL) +
    facet_grid(n2~lab) +
    scale_color_viridis_d(direction = 1) 

p_RB_response <- res_sum %>% 
    filter(link == "response", lab != 0) %>% 
    mutate(est_mean_RB = case_when(theta == 0 ~ est_mean,
                                   TRUE ~ est_mean_RB)) %>% 
    arrange(desc(n1)) %>% 
    ggplot(aes(est_mean_RB, model, color = n1)) + 

    geom_line(aes(group = rev(n1))) +
    geom_point() + 
    # geom_linerangeh(aes(xmin = 0, xmax = est_mean_RB), position = position_dodgev(height = .3),
    #                 alpha = alpha_linerange) +
    annotate("rect", 
             xmin = -0.05, 
             xmax = 0.05, 
             ymin = -Inf, 
             ymax = Inf, 
             alpha = 0.1) +
    geom_vline(xintercept = 0, 
               linetype = "dotted") +
    facet_grid(n2 ~ lab, scales = "free") +
    labs(x = "Relative bias", y = NULL) +
    scale_color_viridis_d(direction = 1) 

p_RB <- plot_grid(p_RB_log, 
                  p_RB_response, 
                  ncol = 1, 
                  align = "v")


# Save
width <- 15
height <- 13
ggsave("figures/fig_coverage.pdf", p_cov, width = width, height = height, units = "cm", useDingbats = FALSE)
ggsave("figures/fig_power.pdf", p_power, width = width, height = height, units = "cm", useDingbats = FALSE)
ggsave("figures/fig_RMSE.pdf", p_RMSE, width = width, height = height, units = "cm", useDingbats = FALSE)
ggsave("figures/fig_RB.pdf", p_RB, width = width, height = height, units = "cm", useDingbats = FALSE)
