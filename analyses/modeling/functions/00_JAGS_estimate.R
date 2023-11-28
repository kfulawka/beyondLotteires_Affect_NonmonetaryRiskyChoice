# function for estimating model parameters using R2Jags
jags_estimate <- function(data, # list with data for JAGS
                          jags_mod, # path to JAGS model written as a R function
                          n_par, # no. of paramteres
                          g_pars = NULL,
                          i_pars = NULL,
                          l_pars = NULL,
                          pars_monit = c(g_pars, i_pars, l_pars),
                          n_chains = 8,
                          n_burn = 1e3,
                          n_thin = 10,
                          n_sample = 4e2,
                          n_adapt = 1e3,
                          # starting_vals = NULL,
                          diag = T,
                          write_path = getwd()
                          
)  {
  
  # library to run jags
  library(runjags)
  library(loo)
  library(coda)
  
  # function to generate starting values
  # source('analyses/modeling/00_initial_values_generator.R')
  
  # function to separate groups of posterior samples
  source('analyses/modeling/functions/00_extract_samples.R')
  
  # function to approximate loo
  source('analyses/modeling/functions/00_loo_approximation.R')
  
  # pseudo r-squared
  source('analyses/modeling/functions/00_rsq_nag.R')
  
  # function to run diagnostics
  source('analyses/modeling/functions/00_mcmc_diagnostics.R')
  
  # function to compute predicted p(a)
  source('analyses/modeling/functions/00_predicted_pa.R')
  
  # create the function
  # iv <- initial_values_generator(starting_vals = starting_vals,
  #                                scaling_vals = scaling_vals,
  #                                n_par = n_par)
  
  # fit the model
  M <- try(run.jags(model = jags_mod,
                    monitor = pars_monit,
                    data = data,
                    n.chains = n_chains,
                    inits = NULL,
                    adapt = n_adapt,
                    burnin = n_burn,
                    sample = n_sample,
                    thin = n_thin,
                    method = 'parallel'))
  
  if(class(M) == 'try-error') {
    
    return('sampling failed')
    
  }
  
  # seprate loglik, group-lvl and ind-lvl parameters
  post_samples <- separate_pars_from_logl(M,
                                          group_p = g_pars,
                                          id_p = i_pars)
  
  # remove logPp from the samples (saves working memory?)
  M$mcmc <- M$mcmc[ , c(post_samples$g_pars_n, post_samples$i_pars_n), ]
  
  # add LOO and G^2 estimates if logPp was monitored
  if('logPp' %in% pars_monit) {
    
    ns = dim(post_samples$ll)[[2]]
    
    # LOO
    loo_r <- loo_comp(ll = post_samples$ll,
                      chains = post_samples$chains)
    
    # loglikelihood
    logLik <- sum( colMeans(post_samples$ll) )
    
    # pseudo r-Squared
    r_sq_n <- r_sq_nag(logLik, sum(log(rep(.5, ns))), ns)
      
    
    # predicted choice probabilities
    pa <- try(predicted_pa(colMeans(post_samples$ll),
                           choice = data$co))
    
    pa_loo <- try(predicted_pa(loo_r$pointwise[,1],
                               choice = data$co))
    
  }
  
  # remove ll matrix from post-samples (saves working memory?)
  post_samples$ll <- NULL
  
  # summary statistics
  M <- add.summary(M,
                   summary.iters = n_sample,
                   trace.iters = n_sample,
                   plots = F)
  
  # diagnostic plots (if requested)
  if(diag) {
    
    # chains and convergence
    diagnostic_mcmc(M = M,
                    pars = post_samples$g_pars_n,
                    write_path = write_path)
    
    # pairs plot for the group-lvl pars
    jpeg(filename = paste0(write_path, '_pairs.jpg'),
         units = 'cm',
         width = 24,
         height = 24,
         res = 700,
         quality = 100)

    pairs(post_samples$g_pars,
          upper.panel = NULL,
          col = rgb(.1,.1,.7, .5),
          pch = '.')

    dev.off()
    
  }
  
  # sampling info
  sampling_info <- c(M[c('model', 'burnin', 'sample', 'thin', 'monitor')],
                     n_chains = length(unique(post_samples$chains)) )
  
  # OUTPUT
  return(list( g_pars = post_samples$g_pars, 
               i_pars = post_samples$i_pars, 
               loo_r = loo_r, 
               logLik = logLik,
               r_sq_n = r_sq_n,
               pa = pa,
               pa_loo = pa_loo,
               sampling_info = sampling_info,
               summary_stats = M$summaries) )
  
}

