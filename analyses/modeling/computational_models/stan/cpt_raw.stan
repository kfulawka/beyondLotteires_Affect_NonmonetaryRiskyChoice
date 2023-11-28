data {
  int n;
  int n_cp;
  matrix[n_cp, n] xA;
  matrix[n_cp, n] xB;
  matrix[n_cp, 2] p;
  int<lower = 0, upper = 1> co[n_cp, n];
  int s;
}

parameters {
  
  real mu_beta_Phi;
  real<lower = 0> sigma_beta;
  vector[n] beta_Phi;
  
  real mu_del_Phi;
  real<lower = 0> sigma_del;
  vector[n] del_Phi;
  
  real mu_gam_Phi;
  real<lower = 0> sigma_gam;
  vector[n] gam_Phi;
  
  real mu_theta_Phi;
  real<lower = 0> sigma_theta;
  vector[n] theta_Phi;
}

transformed parameters {
  
  // subjective values and weights
  matrix[n_cp, n] vXa;
  matrix[n_cp, n] wXa;
  matrix[n_cp, n] vsA;
  
  matrix[n_cp, n] vXb;
  matrix[n_cp, n] wXb;
  matrix[n_cp, n] vsB;
  
  matrix[n_cp, n] del_a;
  matrix[n_cp, n] gam_a;
  
  matrix[n_cp, n] del_b;
  matrix[n_cp, n] gam_b;  

  // parameters on wanted scales
  vector[n] beta;
  vector[n] del;
  vector[n] gam;
  vector[n] theta;
  
  // scaling
  beta = Phi_approx(mu_beta_Phi + sigma_beta * beta_Phi);
  gam = Phi_approx(mu_gam_Phi + sigma_gam * gam_Phi);
  del = exp(mu_del_Phi + sigma_del * del_Phi);
  theta = exp(mu_theta_Phi + sigma_theta * theta_Phi);
  
  // CPT
  for (j in 1:n) {
    for (i in 1:n_cp) {
      
      // option A
      vXa[i,j] = s * fabs(xA[i,j]) * pow( max( [fabs(xA[i,j]), fabs(xB[i,j]) ] ) / fabs(xA[i,j]), beta[j] );
      
      // adw pars
      gam_a[i,j] = pow(gam[j], fabs(vXa[i,j])/10);
      del_a[i,j] = del[j] * fabs(vXa[i,j]/10);
      
      wXa[i,j] = ( del_a[i,j] * pow(p[i,1], gam_a[i,j]) ) / ( del_a[i,j] * pow(p[i,1], gam_a[i,j]) + pow( (1-p[i,1]), gam_a[i,j]) );
      
      vsA[i,j] = vXa[i,j] * wXa[i,j];
      
      // option B
      vXb[i,j] = s * fabs(xB[i,j]) * pow( max( [fabs(xA[i,j]), fabs(xB[i,j]) ] ) / fabs(xB[i,j]), beta[j] );
      
      // adw pars
      gam_b[i,j] = pow(gam[j], fabs(vXb[i,j])/10);
      del_b[i,j] = del[j] * fabs(vXb[i,j]/10);
      
      wXb[i,j] = ( del_b[i,j] * pow(p[i,2], gam_b[i,j]) ) / ( del_b[i,j] * pow(p[i,2], gam_b[i,j]) + pow( (1-p[i,2]), gam_b[i,j]) );
      
      vsB[i,j] = vXb[i,j] * wXb[i,j];
      
      // A choice prob
      // pA[i,j] = 1 / (1 + exp(-theta[j] * (vsA[i,j] - vsB[i,j])));
      // pA_c[i,j] = fmax(1e-10, fmin(pA[i,j], .9999999999));
    }
  }
}

model {
  
  mu_beta_Phi ~ std_normal();
  sigma_beta ~ normal(.5, .13);

  mu_gam_Phi ~ std_normal();
  sigma_gam ~ normal(.5, .13);
  
  // mu_del_Phi ~ std_normal();
  mu_del_Phi ~ std_normal();
  sigma_del ~ normal(.5, .13);
  
  // mu_theta_Phi ~ std_normal();
  mu_theta_Phi ~ std_normal();
  sigma_theta ~ normal(.5, .13);
  
  // individual parameters (vectorized)
  beta_Phi ~ std_normal();
  gam_Phi ~ std_normal();
  del_Phi ~ std_normal();
  theta_Phi ~ std_normal();
  
  // CPT
  for (j in 1:n) {
    for (i in 1:n_cp) {
      
      co[i,j] ~ bernoulli_logit( theta[j] * (vsA[i,j] - vsB[i,j]) );
      
    }
  }
}

// logliks for loo
generated quantities {

  matrix[n_cp, n] log_lik;

  real mu_beta = Phi_approx(mu_beta_Phi);
  real mu_gam = Phi_approx(mu_gam_Phi);
  real mu_del = exp(mu_del_Phi);
  real mu_theta = exp(mu_theta_Phi);
  
  for (j in 1:n) {
    for( i in 1:n_cp) {
      
      log_lik[i, j] = bernoulli_logit_lpmf( co[i,j] | theta[j] * (vsA[i,j] - vsB[i,j]) );
      
    }
  }
}