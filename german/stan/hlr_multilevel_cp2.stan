data {
  int N;
  int P;
  int<lower=1> N_g;  // Number of levels in covariate g
  array[N] int<lower=1,upper=N_g> g_id;  // Mapping from obs to level in g
  int<lower=0,upper=1> y[N];
  matrix[N,P] X;
}

parameters {
  vector[P] beta;  // Shared slope vector for all obs

  vector[N_g] alpha_g;
  real mu_alpha_g;
  real<lower=0> sigma_alpha_g;
  real<lower=0> tau;
}

model {
  // Log-likelihood
  target += bernoulli_logit_lpmf(y | alpha_g[g_id] + X * beta);

  // Log-priors
  target += normal_lpdf(beta | 0, tau);
  target += student_t_lpdf(tau | 3, 0, 2.5);
	
  target += normal_lpdf(alpha_g | mu_alpha_g, sigma_alpha_g);
  target += normal_lpdf(mu_alpha_g | 0, 10);
  target += student_t_lpdf(sigma_alpha_g | 3, 0, 2.5);
}
