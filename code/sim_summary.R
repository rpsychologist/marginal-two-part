library(powerlmm)
library(tidyverse)
source("code/functions_sim_summary.R")


# Scenario 0 and 1 --------------------------------------------------------
res0 <- readRDS("data/sim_1_0.Rds")
res1 <- readRDS("data/sim_1_2.Rds")


res_sum <- summarize_multiple_objects(
    list(res0,
         res1),
    labels = c(0, 1), 
    get_model_func = get_all_models
)


saveRDS(res_sum, file = "data/sim1_sum.Rds")

# Scenari 2 --------------------------------------------------------------
res2 <- readRDS("data/sim_2_2.Rds")

res_sum2 <- summarize_multiple_objects(
    list(res2),
    labels = 2, 
    get_model_func = get_all_models_2
    )

saveRDS(res_sum2, file = "data/sim2_sum.Rds")

# Scenario 3 --------------------------------------------------------------
res3 <- readRDS("data/sim_3.Rds")

res_sum3 <- summarize_multiple_objects(
    list(res3),
    labels = 3, 
    get_model_func = get_all_models
    )

saveRDS(res_sum3, file = "data/sim3_sum.Rds")


# Scenario 4 --------------------------------------------------------------
res4 <- readRDS("data/sim_4_variance.Rds")

res_sum4 <- summarize_multiple_objects(
    list(res4),
    labels = 4, 
    get_model_func = get_all_models
    )

saveRDS(res_sum4, file = "data/sim4_sum.Rds")