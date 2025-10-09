setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())  
library(tidyverse)
library(latex2exp)
library(patchwork)
library(grid)
library(glue)

ggplot() + theme_void() + theme(plot.background=element_rect(fill="lightblue1"))
select <- dplyr::select
filter <- dplyr::filter

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(rstan)
rstan_options(auto_write = TRUE)              # Cache compiled Stan programs
options(mc.cores = parallel::detectCores())   # Parallelize chains

util <- new.env()                             # Creates clean new environment
source('stan_utility.R', local=util)          # Source the file located in WD
ls(util)                                      # Check the utils functionality

library(tidybayes)
library(loo)
library(posterior)
options(posterior.num_args=list(digits=2))
library(bayesplot)
theme_set(theme_bw(base_family = "serif", base_size = 14))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

calc_metrics <- function(nj, ni) {
  
  mypath <- getwd()
  fit_path <- file.path(mypath, "stanfits")
  fit_files <- list.files(fit_path,
                          pattern = paste0("nj=", nj, "_ni=", ni, ".*\\.rds$"),
                          full.names = TRUE)
  stanfits <- lapply(fit_files, readRDS)
  K <- length(stanfits)
  
  # empty result-list of length K
  results <- vector("list", K)
  
  for (i in seq_len(K)) {
    
    draws <- stanfits[[i]]$fit %>% as_draws_df()
    truth <- stanfits[[i]]$config
    params <- names(truth)
    
    # RMSE
    sq_err <- sweep(draws[, params], 2, as.numeric(truth), "-")^2
    rmse_point <- sqrt(colMeans(sq_err))
    # Take sqrt() to get back squared errors to "error scale".
    rmse_ci <- apply(sq_err, 2, function(se) {
      quantile(sqrt(se), probs = c(0.025, 0.975))
    })
    
    # OUTPUT LIST OF RESULTS
    results[[i]] <- list(
      rmse_point = rmse_point,
      rmse_ci = rmse_ci,
      config = truth
    )
    # End of for loop
  }
  
  return(results)
  # End of function
}


metrics <- calc_metrics(nj = 2, ni = 3)





