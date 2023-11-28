source('analyses/modeling/functions/01_cpt_functions.R')

cpt_mods_pr <- function(xy, p, a, b, d, g, t,
                        ar = T,
                        rv = T,
                        apwf = T,
                        out = 'pa') {
  
  # subjective outcome values
  if(ar) {
    
    if(rv) {      
      
      sv = t( rvo(xy, b) )
      
    } else {
      
      sv = xy
      
    }
  } else {
    
    sv = v(xy, a)
    
  }
  
  # subjective probs
  if(apwf) {
    
    wp1 <- pwf_ge(p[,1], g = g^(abs(sv[,1])/10), d = d * (abs(sv[,1])/10) )
    wp2 <- pwf_ge(p[,2], g = g^(abs(sv[,2])/10), d = d * (abs(sv[,2])/10) )
    wp <- cbind(wp1, wp2)

  } else {
    
    wp <- pwf_ge(p, g = g, d = d)
    
  }
  
  # subjective values
  va <- sv[,1] * wp[,1]
  vb <- sv[,2] * wp[,2]
  
  if(out == 'ev') return( cbind(va = va, vb = vb))
  
  
  # choice A probability
  pa <- softmax(va, vb, theta = t)
  
  if(out == 'pa') return(pa)
  
  
  # predicted choice
  co <- rbinom(length(pa), 1, pa)
  
  if(out == 'co') return(co)
  
  
}
