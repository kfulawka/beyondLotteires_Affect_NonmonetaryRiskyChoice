# weighting function plots ----------------------------------------

pdf('analyses/figures_results/figs/Fig03v.pdf',
     height = 7 * 0.393701,
     width = 15 * 0.393701,
     pointsize = 9)

# figures' margins
par( mar = c(3, 3, 3, 2))

# layout
layout(matrix(c(1, 2),
              byrow = T,
              nrow = 1))
# RVO ---------------------------------------------------------------------

# example parameter
b = c(.33, .66)

# calculates relative value of option A's outcome
rv <- function(x, y, b) {
  ifelse(abs(x) < abs(y), x * (y/x)^b, x)
}

# panel
plot(1:10, 1:10, 
     type = 'n', 
     xlim = c(.8, 10.2), 
     ylim = c(.8, 10.2),
     main = '',
     yaxt = 'n', 
     xaxt = 'n', 
     ylab = '',
     xlab = '')

# axes
axis(1, at = 1:10, padj = -1, cex.axis = 1)
axis(2, at = 1:10, padj = 1, cex.axis = 1)
title(xlab = expression('a'[x]~'[affect rating of x]'), 
      ylab = expression('v(a'[x]~')'), 
      line = 1.7,
      cex.lab = 1)

title(main = 'Relative affective valuation',
      line = .5)

# grid
abline(v = 1:10,
       h = 1:10,
       col = rgb(.7, .7, .7, .5))

# colors
cols = c()
for (i in 1:10) {
  cols[i] = rgb(.1, .1, i/10, .5)
}

# for each exemplary beta
for(p in 1:2) {
  
  # observed relative valuation
  dd <- data.frame(x = rep(1:10, each = 10)
                   , y = rep(1:10, times = 10)
                   , sv = rv(x = rep(1:10, each = 10)
                             , y = rep(1:10, times = 10)
                             , b = b[p]))
  dd <- dd[dd$x != dd$sv, ]
  
  # example RVOs
  for (i in 1:9) {
    
    rvos = dd$sv[dd$x == i]
    ll = length(rvos)
    xx = rep(i, ll) + ifelse(p == 1, -.2, .2)
    
    for (j in 1:ll) {
      points(xx[j], 
             rvos[j], 
             col = cols[j+(10-ll)], 
             pch = ifelse(p == 1, 0, 1),
             cex = 1.2)
    }
  }
  
  points(10, 10, 
         col = cols[10], 
         pch = ifelse(p == 1, 0, 1), 
         cex = 1.2)
  
}

# AR legend
legend(6, 5, 
       legend = c(2, 5, 10),
       pch = 16, 
       col = cols[c(2, 5, 10)],
       title = expression('a'[ref]),
       cex = 1,
       bty = 'n')

# Beta legend
legend(8, 5,
       legend = b,
       title = expression(beta),
       bty = 'n',
       pch = c(0, 1),
       cex = 1)

mtext('a',
      side = 3,
      line = 1.75,
      adj = 0,
      col = 'black')

# ADW ---------------------------------------------------------------------

# PWF
GE <-function(x, gam, del) del*x^gam / ( del*x^gam + (1-x)^gam )

# affect ratings
aa <- 1:10

# example parameters
g <- .32
d <- .9

# PANEL
plot(0,
     type = 'n',
     xlim = c(0, 1), ylim = c(0, 1),
     main = '',
     yaxt = 'n',
     xaxt = 'n',
     ylab = '',
     xlab = '')

title(main = 'Affective probability weighting',
      line = .5)

# AXES
axis(1, padj = -1, cex.axis = 1)
axis(2, padj = 1, cex.axis = 1)
title(xlab = 'p [probability of x]', 
      ylab = expression('w'[a]~'(p)'), 
      line = 1.7, 
      cex.lab = 1)

# REFERENCE LINE
lines(x = 0:1, 
      y = 0:1, 
      col = 'black', 
      lwd = 1, 
      lty = 2)

# grid
abline(h = seq(0, 1, .1),
       v = seq(0, 1, .1),
       col = rgb(.7, .7, .7, .5))


# ADW EXAMPLE
for(i in 1:length(g)) {
  
  for ( j in 1:10) {
    curve(GE(x, gam = g[i]^(aa[j]/10), del = d[i] * aa[j]),
          from = 0, to = 1,
          n = 1000,
          add = T,
          col = cols[j],
          lty = ifelse(i == 1, 1, 3),
          lwd = 2)
  }
  
}

# # gamma legend
# legend(.7, .55,
#        legend = g,
#        title = expression(gamma),
#        bty = 'n',
#        lty = c(1, 3),
#        cex = 1)
# 
# # delta legend
# legend(.7, .3,
#        legend = paste(d),
#        title = expression(delta),
#        bty = 'n',
#        lty = c(1, 3),
#        cex = 1)

legend(.7, .55,
       legend = bquote(paste(gamma, " = ", .(g))),
       title = '',
       bty = 'n',
       cex = 1)

legend(.7, .45,
       legend = bquote(paste(delta, " = ", .(d))),
       title = '',
       bty = 'n',
       cex = 1)

# LEGEND
legend(.5, .45, 
       legend = c('1', '5', '10'),
       title = expression('a'[x]),
       bty = 'n',
       col = c(cols[1], cols[5], cols[10]),
       pch = 16, 
       cex = 1)

mtext('b',
      side = 3,
      line = 1.75,
      adj = 0,
      col = 'black')

# save figure -------------------------------------------------------------

dev.off()
