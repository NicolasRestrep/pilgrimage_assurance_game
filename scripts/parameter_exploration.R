###############################
#### Parameter Exploration ####
###############################

# Packages ----
library(tidyverse)
library(extraDistr)
library(fdrtool)
library(doParallel)

# Source main function ----
source("scripts/bayesian_stag_hunt.R")

# Parameters to explore ----

# Create different parameter combinations
parameter_df <- expand_grid(
  N = c(10,50,100), 
  ss2_0 = c(0.33^2, 
            0.52^2, 
            0.6^2), 
  pay_var = seq(from = 0.2, to = 1.6, length.out = 8), 
  social_biased_learning = c(TRUE, FALSE), 
  general_biased_learning = c(TRUE, FALSE),
  tick_reset = c(1,3)
)

# Filter redundant conditions
parameter_df <- parameter_df %>% 
  filter( (social_biased_learning == F & 
            general_biased_learning == F) | 
          (social_biased_learning == T & 
            general_biased_learning == F )| 
          (social_biased_learning == F & 
            general_biased_learning == T))

parameter_df <- parameter_df %>% 
  mutate(hm_0 = 1.5, 
         hn_0 = 25, 
         hs2_0 = ss2_0, 
         sm_0 = 0, 
         sn_0 = 10, 
         M = case_when(
           N == 10 ~ 3, 
           N == 50 ~ 15, 
           N == 100 ~ 30
         )) 

# Run with implicit cluster ----

registerDoParallel(cores = detectCores() - 1)

set.seed(1, "L'Ecuyer")
start <- Sys.time()
op <- lift(foreach)(parameter_df) %dopar%
  bayesian_stag_hunt(
    N = N, 
    pay_var = pay_var, 
    social_biased_learning = social_biased_learning, 
    general_biased_learning = general_biased_learning,
    tick_reset = tick_reset,
    hm_0 = hm_0, 
    hn_0 = hn_0,
    hs2_0 = hs2_0,
    sm_0 = sm_0,
    sn_0 = sn_0, 
    ss2_0 = ss2_0, 
    M = M,
    t_max = 300,
    r_max = 50,
    ob = 1,
    h_pay = 1.5,
    s_pay = 3
  )
end <- Sys.time()
stopImplicitCluster()
end-start

# Prepare the results ----

long_form_results <- tibble()

for (i in 1:nrow(parameter_df)) {
  
  tmp_df <- op[[i]][[1]]
  
  grp_df <- tmp_df %>% 
    group_by(turn) %>% 
    summarise(m = mean(p)) %>% 
    ungroup()
  
  long_form_results <- long_form_results %>% 
    rbind(grp_df)
  
}

long_form_parameters <- parameter_df %>% 
  slice(rep(1:n(), each = 300)) 

long_form_df <- cbind(long_form_parameters, 
                      long_form_results)


# Save the results ----
write.csv(long_form_df, 
          "data/bayesian_parameter_exploration.csv")
saveRDS(op, 
        "data/full_bayesian_parameter_exploration.rds")
