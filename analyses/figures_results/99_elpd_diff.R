# function to gather elpd differences
elpd_diff <- function(L, p = NULL, n_m = n_mods) {
  
  # get loos
  loos <- lapply(L[n_m], function(l) l$looE)
  
  # get comparisons
  d <- sapply(p, function(pp) {
    
    elpd_d = loos[[pp[2]]]$estimates[1,1] - loos[[pp[1]]]$estimates[1,1]
    se = loo_compare(loos[[pp[2]]], loos[[pp[1]]])[2,2]
    
    return(c( elpd_d = elpd_d,
              li = elpd_d - se * 1.96,
              ui = elpd_d + se * 1.96) )
    
  }); d <- data.frame(t(d))
  
  d$comp <- names(p)
  
  return(d)
}