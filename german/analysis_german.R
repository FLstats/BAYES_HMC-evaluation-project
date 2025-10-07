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

fit <- readRDS(file.path(getwd(), "stanfits", "hlr_fit_mlvl_alpha=ncp_beta=ncp_P=146.rds"))
draws <- posterior::as_draws_df(fit)


# --------------------------------------------------- #
#       Caterpillar plot of OR median for betas       #
# --------------------------------------------------- #
# (top 20 distances from 1)

### Summarize betas (assumes Stan name beta[1], beta[2], ...)
# Long-format posterior draws.
# 4 chains x 1000 iterations x 146 beta[i] = 584 000 rows.
# Compute with odds ratios per coef and draw ("given this sampled beta[i], 
# the odds multiply by this factor per +1 increase in x").
# Summarise the draws per coef --> end up with P rows.
# e.g. ORmed is the median of the OR for the draws
betas <- draws %>%
  gather_draws(beta[i]) %>%
  mutate(OR = exp(.value)) %>%
  group_by(i) %>%
  summarise(
    med   = median(.value),
    lo95  = quantile(.value, .025),
    hi95  = quantile(.value, .975),
    ORmed = median(OR),
    ORlo  = quantile(OR, .025),
    ORhi  = quantile(OR, .975),
    pr_pos = mean(.value > 0)
  ) %>%
  mutate(delta = abs(ORmed - 1)) %>% # absolute distance from 1
  arrange(desc(delta)) # sort descending

# Keep top 20 for the main figure
betas_top <- betas %>% 
  slice_head(n = 20) %>%
  mutate(i = reorder(as.factor(i), delta))

# Caterpillar plot
ggplot(betas_top, aes(x = i, y = ORmed, ymin = ORlo, ymax = ORhi)) +
  geom_hline(yintercept = 1, linetype = 2, lwd = 0.5) +
  # geom_pointrange(size = 0.5, color = cbbPalette[6]) +
  geom_errorbar(width = 0.3) +
  geom_point(size = 2, color = cbbPalette[6]) +
  coord_flip() +
  labs(x = "Beta coefficient index",
       y = "Median odds ratio") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("plots/cpplot_betas_med-or.pdf", width = 6, height = 5)

# --------------------------------------------------- #
#         Caterpillar plot of alpha_g effects         #
# --------------------------------------------------- #
### Group effects (random intercepts), e.g. alpha[g]
# 1000 iter * 4 chains * 4 groups = 16 000
# Interpretation: baseline odds of Y=1 for each group when all predictors
# are at their mean levels
alpha_groups <- draws %>%
  gather_draws(alpha_g[g]) %>%
  mutate(OR = exp(.value)) %>%
  group_by(g) %>%
  median_qi(OR, .width = 0.95)

alpha_groups %>%
  mutate(g = reorder(g, OR)) %>%
  ggplot(aes(x=OR, y=g)) +
  geom_point(size = 2, color = cbbPalette[6]) +
  geom_errorbar(aes(xmin=.lower, xmax=.upper), width = 0.1) +
  labs(x = "Group-intercept effect (OR)", y = "Group")
 
# --------------------------------------------------- #
#       Marginal effect for one covariate xj          #
# --------------------------------------------------- #
# Construct a small design grid Xnew varying xj, others at typical values
# (adapt to your design naming)
x_seq <- seq(from = -2, to = 2, length.out = 100)
# example: use posterior means for other covariates = 0; add a group if needed
# eta = alpha + beta_j * x_seq  (extend to full X as appropriate)
eta_draws <- draws %>%
  spread_draws(alpha_g[g], beta[i]) %>%
  filter(g == 1, i == 1) %>%
  slice_sample(n = 1000) %>%
  tidyr::crossing(x = x_seq) %>%
  mutate(eta = alpha_g + beta * x,  # pick your j
         p = plogis(eta))

marg <- eta_draws %>%
  group_by(x) %>%
  summarise(med = median(p), lo = quantile(p, .025), hi = quantile(p, .975))

ggplot(marg, aes(x, med, ymin = lo, ymax = hi)) +
  geom_ribbon(alpha = .15) + geom_line() +
  labs(y = "Predicted probability", x = "x_j (standardized)")
