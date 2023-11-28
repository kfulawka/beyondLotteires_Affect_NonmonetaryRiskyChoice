# nagelkerke's R-squared
r_sq_nag <- function(m1, m0, n) {
  
  r1 = 1 - exp(-2/n * (m1 - m0))
  r_max = 1 - exp(2/n * m0)
  r2 <- r1 / r_max
  return(r2)
  
}