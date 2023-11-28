# value fun
v <- function(x, a) sign(x) * abs(x)^a

# rvo
rvo <- function(xy, b) apply( xy, 1, function(x) x * ( max(abs(x))/abs(x) )^b )

# pwf
pwf_ge <- function(p, d, g) { (d*p^g) / ( d*p^g + (1-p)^g ) }

# choice rule
softmax <- function(va, vb, theta) { 1 / ( 1 + exp(-theta * (va - vb))) }
