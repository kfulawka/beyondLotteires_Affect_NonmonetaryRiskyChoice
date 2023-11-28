functions{
  
  real Vsc(real vA, real vB, real varA, real varB) {
    
    real d = vA - vB;
    real varT = varA + varB;
    real r;
    
    if( varT == 0 ) { r = 0; } else { r = d / sqrt(varT); }
    
    return(r);

  }
  
}

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

  // option A
  matrix[n_cp, n] vXa;
  
  matrix[n_cp, n] wXa_n;
  matrix[n_cp, n] wXa_d;
  matrix[n_cp, n] wXa;
  
  matrix[n_cp, n] vsA;
  matrix[n_cp, n] varA;
  
  // option B
  matrix[n_cp, n] vXb;
  
  matrix[n_cp, n] wXb_n;
  matrix[n_cp, n] wXb_d;
  matrix[n_cp, n] wXb;
  
  matrix[n_cp, n] vsB;
  matrix[n_cp, n] varB;
  
  // difference score
  matrix[n_cp, n] d;
  
  // APW parameters
  matrix[n_cp, n] delA;
  matrix[n_cp, n] gamA;
  
  matrix[n_cp, n] delB;
  matrix[n_cp, n] gamB;
  
  // parameters on wanted scales
  vector[n] del;
  vector[n] gam;
  vector[n] theta;
  
  // scaling
  gam = Phi_approx(mu_gam_Phi + sigma_gam * gam_Phi);
  del = Phi_approx(mu_del_Phi + sigma_del * del_Phi);
  theta = exp(mu_theta_Phi + sigma_theta * theta_Phi);
  
  // CPT
  for (j in 1:n) {
    for (i in 1:n_cp) {
      
      // option A
      vXa[i,j] = s * fabs(xA[i,j]);
      
      // adw pars
      gamA[i,j] = pow(gam[j], 1.1 - fabs(vXa[i,j])/10 );
      delA[i,j] = del[j] * fabs(vXa[i,j]/10);
      
      wXa_n[i,j] = (1 - gamA[i,j]) * (1 - delA[i,j]) * p[i,1]^2 + (1 - p[i,1] * gamA[i,j]) * delA[i,j] * p[i,1];
      wXa_d[i,j] = (1 - gamA[i,j]) * (1 - p[i,1]) + (1 - gamA[i,j]) * p[i,1]^2 + gamA[i,j] * p[i,1] * (1 - p[i,1]);
      wXa[i,j] = wXa_n[i,j] / wXa_d[i,j];

      vsA[i,j] = vXa[i,j] * wXa[i,j];
      
      varA[i,j] = wXa[i,j] * vXa[i,j]^2 - vsA[i,j]^2;
      
      // option B
      vXb[i,j] = s * fabs(xB[i,j]);
      
      // adw pars
      gamB[i,j] = pow(gam[j], 1.1 - fabs(vXb[i,j])/10 );
      delB[i,j] = del[j] * fabs(vXb[i,j]/10);
      
      wXb_n[i,j] = (1 - gamB[i,j]) * (1 - delB[i,j]) * p[i,2]^2 + (1 - p[i,2] * gamB[i,j]) * delB[i,j] * p[i,2];
      wXb_d[i,j] = (1 - gamB[i,j]) * (1 - p[i,2]) + (1 - gamB[i,j]) * p[i,2]^2 + gamB[i,j] * p[i,2] * (1 - p[i,2]);
      wXb[i,j] = wXb_n[i,j] / wXb_d[i,j];
      
      vsB[i,j] = vXb[i,j] * wXb[i,j];
      
      varB[i,j] = wXb[i,j] * vXb[i,j]^2 - vsB[i,j]^2;
      
      
      // scale difference
      d[i,j] = Vsc(vsA[i,j], vsB[i,j], varA[i,j], varB[i,j]);
      
    }
  }
}

model {

  mu_gam_Phi ~ std_normal();
  sigma_gam ~ normal(.5, .13);
  
  mu_del_Phi ~ std_normal();
  sigma_del ~ normal(.5, .13);
  
  mu_theta_Phi ~ std_normal();
  sigma_theta ~ normal(.5, .13);
  
  // individual parameters (vectorized)
  gam_Phi ~ std_normal();
  del_Phi ~ std_normal();
  theta_Phi ~ std_normal();
  
  // CPT
  for (j in 1:n) {
    for (i in 1:n_cp) {
      
      co[i,j] ~ bernoulli_logit( 2 * theta[j] * d[i,j] );
      
    }
  }
}

// logliks for loo
generated quantities {

  matrix[n_cp, n] log_lik;

  real mu_gam = Phi_approx(mu_gam_Phi);
  real mu_del = Phi_approx(mu_del_Phi);
  real mu_theta = exp(mu_theta_Phi);
  
  for (j in 1:n) {
    for( i in 1:n_cp) {
      
      log_lik[i, j] = bernoulli_logit_lpmf( co[i,j] | 2 * theta[j] * d[i,j] );
      
    }
  }
}