######################################################
# Functions to create the MAR missing data mechanism #
######################################################


#' Calculate the probability of dropping out
#'
#' @param y numeric; the outcome variable
#' @param treatment numeric; 0 (control) or 1 (treatment)
#'
#' @return
p_MAR <- function(y, treatment) {
    ylag <- dplyr::lag(y)
        
    ylag <- ylag + 1
    ylag_log <- log(ylag, base = 10)
    ylag_log_c <-  ylag_log - log(500 + 1, base = 10)
    
    # MAR function
    treatment <- unique(treatment)
    if(treatment == 1) {
        p_miss <- plogis(qlogis(0.15) + ylag_log_c * 2)
    } else {
        p_miss <- plogis(qlogis(0.15) + ylag_log * -2)
    }

    # T = 1 is complete
    p_miss[1] <- 0
    
    p_miss
}

#' Create missingness indicator
#' 
#' Converts a vector with intermittent missingness into monotonic dropout. 
#' For instance,
#' 0 0 0 1 0 1 0 0 0 0 0 is converted to
#' 0 0 0 1 1 1 1 1 1 1 1
#'
#' @param miss a numeric indicator, 0 = not missing, 1 = missing
#'
#' @return a numeric vector where 1 indicates values in Y that should be NA
add_dropout_ind <- function(miss) {
    dropout <- which(miss == 1)[1]
    if(!is.na(dropout)) miss[dropout:length(miss)] <- 1
    
    miss
}
#' Add missing data
#'
#' @param data a data.frame with the complete data
#'
#' @return a data.frame where values in Y is replaced with NA
#' based on MAR dropout
add_MAR_missing <- function(data) {
    d <- data
    
    d <- d %>% 
        group_by(subject) %>% 
        mutate(p_miss = p_MAR(y, treatment))
    
    d$miss <- rbinom(nrow(d), 1, d$p_miss)
    
    d <- d %>% 
        group_by(subject) %>% 
        mutate(miss = add_dropout_ind(miss)) %>% 
        ungroup() %>% 
        mutate(y = ifelse(miss == 1, NA, y))

    d
}
#' Add missing data and convert into pre-post data for ANCOVA 
#'
#' @param data The complete data in long format
#'
#' @return a data.frame where `y` is the posttest variable with missing observations
#' and `pre` is the pretest scores.
add_MAR_missing_post <- function(data) {
    d <- add_MAR_missing(data)
    
    pre <- dplyr::filter(d, time == 0)
    post <- dplyr::filter(d, time == 10)
    post$pre <- pre$y
    
    post
}

