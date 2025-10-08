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

  real<lower=0> sigma2;
}

transformed parameters {
  vector[N] alpha_obs;  // Every obs mapped to its intercept level
  for(n in 1:N) {
    alpha_obs[n] = alpha_g[g_id[n]];
  }
}

model {
  // Log-likelihood
  target += bernoulli_logit_lpmf(y | alpha_obs + X * beta);

  // Log-priors
  target += student_t_lpdf(beta | 3, 0, sqrt(sigma2));
	
  target += normal_lpdf(alpha_g | mu_alpha_g, sigma_alpha_g);
  target += normal_lpdf(mu_alpha_g | 0, 10);
  target += student_t_lpdf(sigma_alpha_g | 3, 0, 2.5);

  // Hyperprior
  target += exponential_lpdf(sigma2 | 0.01);
}
