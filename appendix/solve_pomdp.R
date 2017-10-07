# devtools::install_github("boettiger-lab/sarsop")  ## install package first if necessary.
library(sarsop)       # the main POMDP package
library(tidyverse)    # for munging and plotting
library(parallel)
options(mc.cores = 4) # Reserve ~ 10 GB per core
log_dir <- "pomdp_intro"

r <- 0.75
K <- 1
## Classic Graham-Schaefer. Note that recruitment occurs *before* harvest
f <- function(x, h){ 
  x + x * r * (1 - x / K) - pmin(x,h)
}
reward_fn <- function(x,h) pmin(x,h)
discount <- 0.99

## Discretize space
states <- seq(0,2, length=100)
actions <- states
observations <- states

## All parameter values combinations for which we want solutions
meta <- expand.grid(sigma_g = c(0.01, 0.05, 0.1), 
                    sigma_m = c(0, 0.1, 0.2),
                    stringsAsFactors = FALSE)

## Create the models
models <- 
  parallel::mclapply(1:dim(meta)[1], 
                     function(i){
                       fisheries_matrices(
                         states = states,
                         actions = actions,
                         observed_states = observations,
                         reward_fn = reward_fn,
                         f = f,
                         sigma_g = meta[i,"sigma_g"][[1]],
                         sigma_m = meta[i,"sigma_m"][[1]],
                         noise = "normal")
                     })

## log metadata associated with each run

dir.create(log_dir)

## POMDP solution (slow, >10,000 seconds per loop memory intensive)
system.time(
  alphas <- 
    parallel::mclapply(1:length(models), 
    function(i){
      log_data <- data.frame(model = "gs", 
                             r = r, 
                             K = K, 
                             sigma_g = meta[i,"sigma_g"][[1]], 
                             sigma_m = meta[i,"sigma_m"][[1]], 
                             noise = "normal")
      
      sarsop(models[[i]]$transition,
             models[[i]]$observation,
             models[[i]]$reward,
             discount = discount,
             precision = 2e-6,
             timeout = 10000,
             log_dir = log_dir,
             log_data = log_data)
    })
)


