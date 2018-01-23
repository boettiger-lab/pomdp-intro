## parallel version
pomdp_overestimates <-
  function (transition, 
            model_observation, 
            reward, 
            discount, 
            true_observation,
            x0, 
            a0 = 1, 
            Tmax = 20,
            alpha,
            reps = 100) 
  {
  sims <- parallel::mclapply(1:reps, function(i) {
    sim <- pomdp_overestimate(transition, 
                              model_observation, 
                              reward, 
                              discount,           
                              true_observation, 
                              x0, 
                              a0, 
                              Tmax, 
                              alpha)
    sim$df$rep <- i
    sim
  }, ...)
  list(df = do.call(rbind, lapply(sims, `[[`, "df")), 
       state_posterior = do.call(rbind, lapply(sims, `[[`, "state_posterior")))

}

pomdp_overestimate <-
function (transition, 
          model_observation, 
          reward, 
          discount, 
          true_observation,
          x0, 
          a0 = 1, 
          Tmax = 20,
          alpha) 
{
  observation <- model_observation
  
  stopifnot(identical(dim(true_observation), dim(model_observation)))

  n_states <- dim(observation)[1]
  n_obs <- dim(observation)[2]
  value <- obs <- action <- state <- numeric(Tmax + 1)
  state_posterior <- array(NA, dim = c(Tmax + 1, n_states))
  state_prior <- rep(1, n_states)/n_states # assume unif prior
  
  state[2] <- x0
  action[1] <- a0
  state_posterior[2, ] <- state_prior
  
  for (t in 2:Tmax) {
      ## Policy based on model
      out <- compute_policy(alpha, 
                            transition, 
                            model_observation, 
                            reward, 
                            state_posterior[t, ], 
                            action[t - 1])
      
    ## Simulation based on True
    obs[t] <- sample(1:n_obs, 
                     1, 
                     prob = 
              true_observation[state[t],, action[t - 1]])
    
    action[t] <- out$policy[obs[t]]
    value[t] <- reward[state[t], action[t]] * discount^(t - 1)
    state[t + 1] <- sample(1:n_states, 
                           1, 
                prob = transition[state[t], , action[t]])
    
    ## Update belief based on model
    state_posterior[t + 1, ] <- 
      update_belief(state_posterior[t,], 
                    transition, 
                    model_observation, 
                    obs[t], 
                    action[t - 1])
  }
  
  ## Collect results
  df <- data.frame(time = 0:Tmax, 
                   state, 
                   obs, 
                   action, 
                   value)[2:Tmax,]
  list(df = df, 
       state_posterior = state_posterior[2:(Tmax + 1), ])
}