setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())  
library(tidyverse)
theme_set(theme_bw())
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

calc_metrics <- function(model) {
  
  mypath <- getwd()
  data_path <- file.path(mypath, "stanfits")
  data_files <- list.files(data_path, pattern = model, full.names = TRUE)
  stanfits <- lapply(data_files, readRDS)
  K <- length(stanfits)
  
  results <- vector("list", K)
  
  for (i in seq_len(K)) {
    
    draws <- stanfits[[i]]$fit %>% as_draws_df()
    truth <- stanfits[[i]]$config
    if (model == "funnel") {
      params <- "sigma_v"
    } else if (model == "hrb") {
      params <- names(truth)  
    }
    
    sq_err <- sweep(draws[, params], 2, as.numeric(truth), "-")^2
    
    rmse_point <- colMeans(sq_err) %>% sqrt()
    
    # Take sqrt() to get back squared errors to parameter scale.
    rmse_ci <- apply(sq_err, 2, function(se) {
      quantile(sqrt(se), probs = c(0.025, 0.975))
    })
    
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

# --------------------------------------------------- #
#                   RUN FUNCTION                      #
# --------------------------------------------------- #
metrics_hrb <- calc_metrics(model = "hrb")
metrics_funnel <- calc_metrics(model = "funnel")






