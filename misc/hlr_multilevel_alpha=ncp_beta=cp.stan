/* Shared slopes (no grouping variable) (CP)
*  Grouped intercepts (grouping variable = job) (NCP)
*/

data {
  int N;
  int P;
  int<lower=1> N_g;  // Number of levels in covariate g
  array[N] int<lower=1,upper=N_g> g_id;  // Mapping from obs to level in g
  int<lower=0,upper=1> y[N];
  matrix[N,P] X;  // (no intercept)
}

parameters {
  // Shared slopes
  vector[P] beta;  // Shared slope vector for all obs
  real<lower=0> sigma2;  // Slope variance

  // Grouped intercepts
  vector[N_g] alpha_raw_g;  // One intercept per level in g
  real mu_alpha_g;
  real<lower=0> sigma_alpha_g;
}

transformed parameters {
  // NCP for grouped intercepts
  vector[N_g] alpha_g	= mu_alpha_g
						+ alpha_raw_g
						* sigma_alpha_g;
}

model {
  // Log-likelihood
  target += bernoulli_logit_lpmf(y | alpha_g[g_id] + X * beta);

  // Log-priors
  target += student_t_lpdf(beta | 3, 0, sqrt(sigma2));
	
  target += normal_lpdf(alpha_raw_g | 0, 1);
  target += normal_lpdf(mu_alpha_g | 0, 10);
  target += student_t_lpdf(sigma_alpha_g | 3, 0, 2.5);

  // Hyperprior
  target += exponential_lpdf(sigma2 | 0.01);
}
