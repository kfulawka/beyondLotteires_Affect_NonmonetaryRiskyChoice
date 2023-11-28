# function to gather estimated elpds
elpd_extract = function(L,
                        elpd = T,
                        n_m = NULL) {
  
  # all models if n not supplied
  if(is.null(n_m)) {
    
    n_m = names(L)
    
  }
  
  if(elpd) {
    
    # extract elpds and 95% CIs
    elpds = sapply(n_m, function(l) {
      
      e = L[[l]]$looE$estimates[1,]
      e[2:3] = e[1] + c(-1.96, 1.96) * e[2] 
      
      names(e) = c('elpd', 'li', 'ui')
      
      return(e)
      
    })
    
  } else {
    
    # extract balanced-accuracy_loo
    elpds = sapply(n_m, function(l) {
      
      e = L[[l]]$ind_ba$ba
      
      # arithmetic mean and sd
      ee = c(elpd = mean(e),
             li = mean(e) - sd(e),
             ui = mean(e) + sd(e))
      
      return(ee)
      
    })
    
  }
  
  # transpose
  elpds = data.frame( t(elpds) )
  
  return(elpds)
  
}