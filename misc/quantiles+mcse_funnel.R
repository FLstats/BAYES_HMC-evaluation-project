library(posterior)
library(dplyr)
library(purrr)
library(stringr)
library(xtable)

quantile_mcse_df <- function(path = file.path(getwd(), "stanfits"),
                             pattern = "funnel_fit.*\\.rds$") {
  
  fit_files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (length(fit_files) == 0)
    stop("No .rds files found in: ", path)
  
  results <- map_dfr(fit_files, function(f) {
    sf <- readRDS(f)
    draws <- sf$fit %>% as_draws_df()
    v <- as.numeric(draws$v)
    
    tibble(
      P         = if (!is.null(sf$P)) sf$P else as.numeric(str_match(basename(f), "P=([0-9]+)")[,2]),
      mcse_mean = as.numeric(mcse_mean(v)),
      q05       = as.numeric(quantile(v, 0.05)),
      mcse_q05  = as.numeric(mcse_quantile(v, probs = 0.05)),
      q95       = as.numeric(quantile(v, 0.95)),
      mcse_q95  = as.numeric(mcse_quantile(v, probs = 0.95))
    )
  }) %>%
    arrange(P)
  
  return(results)
}

metrics_df <- quantile_mcse_df()
print(metrics_df)



# Convert numeric columns to character with full precision
metrics_chr <- metrics_df %>%
  mutate(across(where(is.numeric), ~ formatC(.x, digits = 8, format = "f", drop0trailing = FALSE)))

xt <- xtable(metrics_chr)

print(xt, include.rownames = FALSE, sanitize.text.function = identity)


