library(posterior)
library(dplyr)
library(purrr)
library(stringr)

quantile_mcse_hrb <- function(nj, ni,
                              path = file.path(getwd(), "stanfits"),
                              pattern = NULL) {
  # If you want a custom pattern, you can pass it; otherwise build it:
  if (is.null(pattern)) {
    pattern <- paste0("nj=", nj, "_ni=", ni, ".*\\.rds$")
  }
  
  fit_files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (length(fit_files) == 0) {
    stop("No .rds files matched pattern '", pattern, "' in: ", path)
  }
  
  results <- map_dfr(seq_along(fit_files), function(i) {
    f  <- fit_files[i]
    sf <- readRDS(f)
    draws <- sf$fit %>% as_draws_df()
    
    # Extract numeric vectors for the three parameters
    mu <- as.numeric(draws$mu)
    a  <- as.numeric(draws$a)
    b  <- as.numeric(draws$b)
    
    # Safe helpers
    q05  <- function(x) as.numeric(quantile(x, 0.05))
    q95  <- function(x) as.numeric(quantile(x, 0.95))
    mcsq <- function(x, p) as.numeric(mcse_quantile(x, probs = p))
    
    # True values (if stored)
    cfg <- tryCatch(sf$config, error = function(e) NULL)
    true_mu <- if (!is.null(cfg) && !is.null(cfg[["mu"]])) as.numeric(cfg[["mu"]]) else NA_real_
    true_a  <- if (!is.null(cfg) && !is.null(cfg[["a"]]))  as.numeric(cfg[["a"]])  else NA_real_
    true_b  <- if (!is.null(cfg) && !is.null(cfg[["b"]]))  as.numeric(cfg[["b"]])  else NA_real_
    
    tibble(
      set_id         = i,  # simple index; change if you have explicit set labels
      
      # mu_mcse_mean   = as.numeric(mcse_mean(mu)),
      # mu_q05         = q05(mu),
      # mu_mcse_q05    = mcsq(mu, 0.05),
      # mu_q95         = q95(mu),
      # mu_mcse_q95    = mcsq(mu, 0.95),
      
      # a_mcse_mean    = as.numeric(mcse_mean(a)),
      # a_q05          = q05(a),
      # a_mcse_q05     = mcsq(a, 0.05),
      # a_q95          = q95(a),
      # a_mcse_q95     = mcsq(a, 0.95),
      
      b_mcse_mean    = as.numeric(mcse_mean(b)),
      b_q05          = q05(b),
      b_mcse_q05     = mcsq(b, 0.05),
      b_q95          = q95(b),
      b_mcse_q95     = mcsq(b, 0.95)
    )
  }) %>% arrange(set_id)
  
  return(results)
}

metrics_hrb <- quantile_mcse_hrb(nj = 2, ni = 3)
print(metrics_hrb)


metrics_chr <- metrics_hrb %>%
  mutate(across(where(is.numeric), ~ formatC(.x, digits = 8, format = "f", drop0trailing = FALSE)))

xt <- xtable(metrics_chr)

print(xt, include.rownames = FALSE, sanitize.text.function = identity)
