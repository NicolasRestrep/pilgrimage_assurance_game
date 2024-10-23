# Main Model Function ----

bayesian_stag_hunt <- function(
  
  # turns
  t_max = 300,
  # rounds
  r_max = 3,
  # Agents
  N = 10,
  # Threshold
  M = 3,
  # Number of models observed per turn
  ob = 1,
  # Payoff variance (should be the same for both)
  pay_var = 0.6,
  # Hare base payoff
  h_pay = 1.5,
  # Stag base payoff
  s_pay = 3,
  
  # Prior mean for hare hunting
  hm_0 = 1.5,
  # Prior certainty (can be interpreted as prior sample) for hare hunting
  hn_0 =  25,
  # Prior Variance
  hs2_0 =  0.2704,
  # Prior mean for stag hunting
  sm_0 = 0,
  # Prior certainty (can be interpreted as prior sample) for stag hunting
  sn_0 = 10,
  # has to be min 2
  # Prior Variance
  ss2_0 =  0.2704,
  
  # Biased transmission for just social players
  social_biased_learning =  FALSE,
  
  # Biased transmission for everyone 
  general_biased_learning = FALSE,
  
  # How many turns do social players stay in the strategy?
  tick_reset = 1) {
  
  # Shape prior for hare
  hv_0 =  hn_0 - 1
  # Shape prior for stag
  sv_0 <-  sn_0 - 1
  
  # Data frame for the output
  output_df <- tibble(
    turn = rep(1:t_max, r_max),
    p = as.numeric(rep(NA, t_max * r_max)),
    run = as.factor(rep(1:r_max, each = t_max))
  )
  
  output_agents <- matrix(NA_real_,
                          nrow = N * r_max,
                          ncol = 13)
  # Main Loop ----
  
  for (r in 1:r_max) {
    
    print(paste0("Working on round: ", r, " var: ", pay_var, " stag: ", ss2_0))
    # Dataframe to keep track of agents
    
    # Create the matrix
    agents_df <- matrix(NA_real_,
                        nrow = N,
                        ncol = 13)
    
    # Colnames
    colnames(agents_df) <- c(
      "id",
      "strategy",
      "hm_0",
      "hn_0",
      "hs2_0",
      "hv_0",
      "sm_0",
      "sn_0",
      "ss2_0",
      "sv_0",
      "fitness",
      "run", 
      "ticker"
    )
    
    # Fill in matrix appropriately
    agents_df[, 1] <- 1:N
    agents_df[, 2] <- NA_real_
    agents_df[, 3] <- hm_0
    agents_df[, 4] <- hn_0
    agents_df[, 5] <- hs2_0
    agents_df[, 6] <- hv_0
    agents_df[, 7] <- sm_0
    agents_df[, 8] <- sn_0
    agents_df[, 9] <- ss2_0
    agents_df[, 10] <- sv_0
    agents_df[, 11] <- NA_real_
    agents_df[, 12] <- r
    agents_df[, 13] <- 0
    
    # For t turns ----
    
    for (t in 1:t_max) {
      
      # Take a tick down of the clock for the social players
      agents_df[which(agents_df[, "ticker"]>=1), "ticker"] <- agents_df[which(agents_df[, "ticker"]>=1), "ticker"]-1
      
      # Update those at 0 
      updaters <- which(agents_df[, "ticker"]==0)
      
      # Each agent selects what strategy to play
      # Draw from the dist of hare beliefs
      phi_h <-
        rgamma(length(updaters), agents_df[updaters, "hv_0"] / 2, agents_df[updaters, "hs2_0"] * agents_df[updaters, "hv_0"] /
                 2)
      sigma_h <-  1 / sqrt(phi_h)
      mu_h <-
        rnorm(length(updaters), mean = agents_df[updaters, "hm_0"], sd = sigma_h / (sqrt(agents_df[updaters, "hn_0"])))
      y_h <-  rnorm(length(updaters), mu_h, sigma_h)
      
      # Draw from the dist of stag beliefs
      phi_s <-
        rgamma(length(updaters), agents_df[updaters, "sv_0"] / 2, agents_df[updaters, "ss2_0"] * agents_df[updaters, "sv_0"] /
                 2)
      sigma_s <-  1 / sqrt(phi_s)
      mu_s <-
        rnorm(length(updaters), mean = agents_df[updaters, "sm_0"], sd = sigma_s / (sqrt(agents_df[updaters, "sn_0"])))
      y_s <-  rnorm(length(updaters), mu_s, sigma_s)
      
      # They play the strategy of whoever did better
      agents_df[updaters,"strategy"] <-
        ifelse(y_s > y_h, 1, 0) #stag hunters=1
      
      # Update the tickers of new social players
      agents_df[agents_df[which(agents_df[,"strategy"]==1 & agents_df[,"ticker"]==0)],"ticker"] <- agents_df[agents_df[which(agents_df[,"strategy"]==1 & 
                                                                                                                               agents_df[,"ticker"]==0)],"ticker"] + tick_reset 
      
      # Count how many stag hunters
      k <- sum(agents_df[, "strategy"])
      
      # Fitness for hare hunters
      # Draw from a normal
      agents_df[agents_df[, "strategy"] == 0, "fitness"] <-
        rnorm(N - k,
              h_pay,
              pay_var)
      
      
      # If stag hunters, draw
      
      if (k > 0 & k < M) {
        agents_df[agents_df[, "strategy"] == 1, "fitness"] <- rnorm(k,
                                                                    0,
                                                                    pay_var)
        
        
      } else if (k > 0 & k >= M) {
        agents_df[agents_df[, "strategy"] == 1, "fitness"] <- rnorm(k,
                                                                    s_pay,
                                                                    pay_var)
        
        
      }
      
      # Individual Learning
      # update on personal payoffs (so only update dist for own strategy)
      for (i in 1:N) {
        if (agents_df[i, "strategy"] == 1) {
          # New piece of info
          new_draw <- agents_df[i, "fitness"]
          # Update sample observed
          upt_sample  <- agents_df[i, "sn_0"] + 1
          # Update the mean
          upt_mean <-
            (new_draw + agents_df[i, "sn_0"] * agents_df[i, "sm_0"]) / upt_sample
          # Update shape parameter
          upt_shape <- agents_df[i, "sv_0"] + 1
          # Update variance
          upt_sigma <-
            (agents_df[i, "sv_0"] * agents_df[i, "ss2_0"] + agents_df[i, "sn_0"] *
               (agents_df[i, "sm_0"] - new_draw) ^ 2 / upt_sample) / upt_shape
          
          # Now record values
          agents_df[i, c("sn_0", "sm_0", "sv_0", "ss2_0")] <-
            c(upt_sample,
              upt_mean,
              upt_shape,
              upt_sigma)
        } else if (agents_df[i, "strategy"] == 0) {
          # New piece of info
          new_draw <- agents_df[i, "fitness"]
          # Update sample observed
          upt_sample  <- agents_df[i, "hn_0"] + 1
          # Update the mean
          upt_mean <-
            (new_draw + agents_df[i, "hn_0"] * agents_df[i, "hm_0"]) / upt_sample
          # Update shape parameter
          upt_shape <- agents_df[i, "hv_0"] + 1
          # Update variance
          upt_sigma <-
            (agents_df[i, "hv_0"] * agents_df[i, "hs2_0"] + agents_df[i, "hn_0"] *
               (agents_df[i, "hm_0"] - new_draw) ^ 2 / upt_sample) / upt_shape
          
          # Now record values
          agents_df[i, c("hn_0", "hm_0", "hv_0", "hs2_0")] <-
            c(upt_sample,
              upt_mean,
              upt_shape,
              upt_sigma)
        }
      }
      
      # Social Learning
      
      # Assign demonstrators 
      # Empty vector for demos
      demonstrators <- matrix(NA_real_,
                              nrow = N,
                              ncol = ob)
      
      # Consider all agents first
      possible_demonstrators <- 1:N
      
      # Prune possible demonstrators based on tranmission rules
      if (social_biased_learning == T) {
        
        non_demonstrators <- which(agents_df[, "strategy"] == 1 &
                                     agents_df[, "fitness"] < h_pay)
        
        if (is_empty(non_demonstrators)) {
          possible_demonstrators <- possible_demonstrators
        } else {
          possible_demonstrators <- possible_demonstrators[-non_demonstrators]
        }
        
      } else if (general_biased_learning == T) {
        
        non_demonstrators <- which(agents_df[, "fitness"] < h_pay)
        
        if (is_empty(non_demonstrators)) {
          possible_demonstrators <- possible_demonstrators
        } else {
          possible_demonstrators <- possible_demonstrators[-non_demonstrators]
        }
        
      }
      
      # Now assign demonstrators 
      # If there are no possible demonstrators, then no social learning takes place 
      
      if (!is_empty(possible_demonstrators)) {
        for (i in 1:N) {
          if (social_biased_learning == T | general_biased_learning == T) {
            demonstrators[i, ] <-
              sample(possible_demonstrators,
                     size = ob,
                     replace = T)
          } else {
            demonstrators[i, ] <- sample(1:N, size = ob, replace = T)
          }
        }
        
        for (j in 1:N) {
          if (agents_df[demonstrators[j], "strategy"] == 1) {
            # New piece of info
            new_draw <- agents_df[demonstrators[j], "fitness"]
            # Update sample observed
            upt_sample  <- agents_df[j, "sn_0"] + ob
            # Update the mean
            upt_mean <-
              (new_draw + agents_df[j, "sn_0"] * agents_df[j, "sm_0"]) / upt_sample
            # Update shape parameter
            upt_shape <- agents_df[j, "sv_0"] + ob
            # Update variance
            ifelse(
              ob == 1,
              upt_sigma <-
                (
                  agents_df[j, "sv_0"] * agents_df[j, "ss2_0"] + agents_df[j, "sn_0"] * (agents_df[j, "sm_0"] -
                                                                                           new_draw) ^ 2 / upt_sample
                ) / upt_shape,
              upt_sigma <-
                ((ob - 1) * var(new_draw) + agents_df[j, "sv_0"] * agents_df[j, "ss2_0"] + agents_df[j, "sn_0"] *
                   (agents_df[j, "sm_0"] - mean(new_draw)) ^ 2 / upt_sample
                ) / upt_shape
            )
            
            
            # Now record values
            agents_df[j, c("sn_0", "sm_0", "sv_0", "ss2_0")] <-
              c(upt_sample,
                upt_mean,
                upt_shape,
                upt_sigma)
          } else if (agents_df[demonstrators[j], "strategy"] == 0) {
            # New piece of info
            new_draw <- agents_df[demonstrators[j], "fitness"]
            # Update sample observed
            upt_sample  <- agents_df[j, "hn_0"] + ob
            # Update the mean
            upt_mean <-
              (new_draw + agents_df[j, "hn_0"] * agents_df[j, "hm_0"]) / upt_sample
            # Update shape parameter
            upt_shape <- agents_df[j, "hv_0"] + ob
            # Update variance
            upt_sigma <-
              (agents_df[j, "hv_0"] * agents_df[j, "hs2_0"] + agents_df[j, "hn_0"] *
                 (agents_df[j, "hm_0"] - new_draw) ^ 2 / upt_sample) / upt_shape
            
            ifelse(
              ob == 1,
              upt_sigma <-
                (
                  agents_df[j, "hv_0"] * agents_df[j, "hs2_0"] + agents_df[j, "hn_0"] * (agents_df[j, "hm_0"] -
                                                                                           new_draw) ^ 2 / upt_sample
                ) / upt_shape,
              upt_sigma <-
                ((ob - 1) * var(new_draw) + agents_df[j, "hv_0"] * agents_df[j, "hs2_0"] + agents_df[j, "hn_0"] *
                   (agents_df[j, "hm_0"] - mean(new_draw)) ^ 2 / upt_sample
                ) / upt_shape
            )
            
            
            # Now record values
            agents_df[j, c("hn_0", "hm_0", "hv_0", "hs2_0")] <-
              c(upt_sample,
                upt_mean,
                upt_shape,
                upt_sigma)
          }
        }
      }
      
      # update % each strategy
      
      output_df[output_df$turn == t &
                  output_df$run == r, "p"] <- k / N
      
    } # end of turn
    
    output_agents[((r * N) - (N-1)):(r * N), 1:13] <- agents_df
  } # end of run
  
  # Record Data ----
  
  output_agents <- as.data.frame(output_agents)
  colnames(output_agents) <-  c(
    "id",
    "strategy",
    "hm_0",
    "hn_0",
    "hs2_0",
    "hv_0",
    "sm_0",
    "sn_0",
    "ss2_0",
    "sv_0",
    "fitness",
    "run", 
    "ticker"
  )
  
  return(list(output_df,
              output_agents))
  
}
