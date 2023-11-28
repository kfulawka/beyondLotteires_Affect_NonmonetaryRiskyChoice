choice_prop_fig <- function(mm,  
                            pa,
                            choices, 
                            p,
                            fig_name = 'Fig05.pdf') {
  
  # number of choice problems
  n_cp <- nrow(choices)
  
  # number of participants
  n <- ncol(choices)
  
  # index of cps for which B is riskier (pB < pA)
  ii <- p[,'pB'] < p[,'pA']
  
  # # choice prop. by choice problem ------------------------------------------
  # 
  # # choice proportions data
  # cp <- data.frame(observed = apply(choices, 1, mean),
  #                  cpt = mm$cpt_nmon_wtp$pa$cp_pred,
  #                  ar = mm$cpt_nmon_ar$pa$cp_pred,
  #                  rvo = mm$cpt_nmon_rvo$pa$cp_pred,
  #                  apwf = mm$cpt_nmon_radw$pa$cp_pred)
  # 
  # # recode into probability of A choice
  # cp_r <- apply(cp, 2, function(x) {
  #   
  #   x[ii] <- 1 - x[ii]
  #   return(x)
  #   
  # })
  # 
  # cp_r <- cp_r[order(cp_r[,'observed']), ]
  # 
  # dimnames(cp_r)[[2]] <- c('Observed', 'CPT',
  #                          'Affect ratings', 'RVO',
  #                          'Affective weighting')
  # 
  # individual risky choices ------------------------------------------------
  
  # pointwise a choices
  ipa <- list(observed = choices,
              cpt = pa$cpt_nmon_wtp,
              ar = pa$cpt_nmon_ar,
              apwf = pa$cpt_nmon_radw)
  
  # into prop of risky choice
  cpi <- sapply(ipa, function(x) {
    
    # recode
    x[ii,] <- 1 - x[ii,]
    
    # into individual means
    cpi <- colMeans(x)
    
    return(cpi)
    
  })
  
  # order by data
  cpi <- cpi[order(cpi[,1]), ]
  
  dimnames(cpi)[[2]] <- c('Observed', 'CPT',
                          'AV', 'rAV-rAPW')
  
  # correct choice predictions by Participant -----------------------------------
  
  lls <- list(cpt = mm$cpt_nmon_wtp$looE$pointwise[,1],
              ar = mm$cpt_nmon_ar$looE$pointwise[,1],
              apwf = mm$cpt_nmon_radw$looE$pointwise[,1])
  
  # into prop of risky choice
  lla <- sapply(lls, function(x) {
    
    # into matrix
    x <- matrix(x,
                nrow = n_cp,
                ncol = n)
    
    # into individual means
    # cci <- 1 - colSums(x) / sum(log(rep(.5, n_cp)))
    cci = colSums(x)
    
    return(cci)
    
  })
  
  # figure ------------------------------------------------------------------
  
  # colors
  cc <- c(rgb(.7, .1, .1, .6), # data
          rgb(.1, .1, .7, .6) # model
  )
  
  pdf(paste0('analyses/figures_results/figs/', fig_name),
      height = 11 * 0.393701,
      width = 16 * 0.393701,
      pointsize = 10)
  
  # figure
  par( mar = c(5, 4, 3, 1) )
  layout( matrix(1:6, 2, 3, byrow = T))
  
  for(i in 2:4) {
    
    # individual plots
    plot(1:n, cpi[,1], 
         pch = 19, 
         col = cc[1],
         ylab = '',
         xlab = '',
         xlim = c(1, n),
         ylim = c(0, 1),
         xaxt = 'n',
         main = dimnames(cpi)[[2]][i])
    axis(1, at = c(1, seq(20, 80, by = 20)))
    
    mtext(ifelse(i == 3, 'Participant index', ''),
          side = 1, 
          line = 2.5,
          cex = .65)
    
    mtext(ifelse(i == 2, 'Proportion of risky drug choices', '' ),
          side = 2, 
          line = 2.5,
          cex = .65)

    
    # grid
    abline(h = seq(0, 1, .1),
           v = c(1, seq(10, 80, 10)),
           col = rgb(.7, .7, .7, .5))
    
    # model predictions
    lines(1:n, cpi[,i],
          col = cc[2],
          lwd = 1)
    points(1:n, cpi[,i],
           col = cc[2],
           pch = 19)
    
    # # correlation
    # text(round(n/2), .2,
    #      # label = paste( 'r =', round( cor(cpi[,1], cpi[,i]), 2 ) )
    #      label = paste( 'SSE =', round( sum( (cpi[,1] - cpi[,i])^2), 3 ) )
    #      # label = paste( 'MAD =', round( median( abs(cpi[,1] - cpi[,i]) ), 3 )) 
    #      
    #      
    # )
    
    # 
    if(i == 2) {
      
      legend(1, 1,
             col = cc[1],
             pch = 19,
             legend = 'observed proportion\n(data)',
             bty = 'n')
    }
    
    #
    if(i == 3) {
      
      legend(1, 1,
             col = cc[2],
             lty = 1, 
             lwd = 1,
             pch = 19,
             legend = 'posterior mean prediction\n(model)',
             bty = 'n')
      
    }
    
    # # individual predictions correct
    # plot(1:n, sort(lla[,i-1]),
    #      pch = 19,
    #      col = cc[2],
    #      ylab = '',
    #      xlab = '',
    #      xlim = c(1, 80),
    #      ylim = c(min(lla), max(lla)),
    #      xaxt = 'n',
    #      main = dimnames(cpi)[[2]][i])
    # axis(1, at = c(1, seq(20, 80, by = 20)))
    # 
    # mtext(ifelse(i == 3, 'Participant index', ''),
    #       side = 1, 
    #       line = 2.5,
    #       cex = .65)
    # 
    # mtext(ifelse(i == 2, expression('Individual elpd'['loo']), '' ),
    #       side = 2, 
    #       line = 2.5,
    #       cex = .65)
    # 
    # 
    # abline(h = seq(-30, 0, 2.5),
    #        v = c(1, seq(10, 80, 10)),
    #        col = rgb(.7, .7, .7, .5))
    # 
    # abline(h = sum(log(rep(.5, n_cp))),
    #        col = rgb(.1, .1, .1, .7),
    #        lty = 2)
  }
  
  # PAIRWISE BOXPLOTS
  mc = list(p1 = c('cpt', 'ar'), p2 = c('cpt', 'apwf'),
            p3 = c('ar', 'apwf'))
  mn = list(p1 = c('CPT', 'AV'), p2 = c('CPT', 'rAV-rAPW'),
            p3 = c('AV', 'rAV-rAPW'))
  
  for(i in names(mc)) {
    
    # individual predictions correct
    boxplot(lla[,mc[[i]]],
            type = 'n',
            ylab = '',
            xlab = '',
            ylim = c(min(lla), max(lla)),
            xaxt = 'n',
            lwd = 1,
            staplewex = 0,
            col  = rgb(1, 1, 1, 0),
            main = ifelse(i == 'p2', expression('Individual-level performance'), '' ))
    
    for(j in 1:2) points(rep(j, nrow(lla)), lla[,mc[[i]][j]], 
                         col = cc[2], 
                         pch = 19)
    
    segments(x0 = 1, x1 = 2, y0 = lla[,mc[[i]][1]], lla[,mc[[i]][2]],
             col = rgb(.1, .1, .7, .2))
    
    axis(1, at = 1:2, labels = mn[[i]])

    mtext(ifelse(i == 'p2', 'Model', ''),
          side = 1,
          line = 2.5,
          cex = .65)

    mtext(ifelse(i == 'p1', expression('Individual elpd'['loo']), '' ),
          side = 2,
          line = 2.5,
          cex = .65)


    abline(h = seq(-30, 0, 2.5),
           col = rgb(.7, .7, .7, .5))

    abline(h = sum(log(rep(.5, n_cp))),
           col = rgb(.1, .1, .1, .7),
           lty = 2)
    
    
  }
  
  dev.off()
  
}