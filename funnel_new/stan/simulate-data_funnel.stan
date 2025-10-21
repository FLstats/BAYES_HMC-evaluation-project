data {
  int<lower=1> N;  // Number of obs to generate
  int<lower=1> P;  // Number of x-variables to generate
}

generated quantities {
  matrix[N, P] X;

  for(n in 1:N) {
    real v = normal_rng(0, 3);
	
    for(p in 1:P) {
      X[n, p] = normal_rng(0, sqrt(exp(v)));
    }
  }
}
