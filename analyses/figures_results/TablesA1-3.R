rm(list = ls())

# function for plotting ---------------------------------------------------

source('analyses/figures_results/99_table_gl_est.R')

# pachur 2017 data --------------------------------------------------------

mods_pach17 = readRDS("analyses/modeling/posteriors/stan/p17.rds")

# list with results
res = lapply(mods_pach17[c(1, 7:12)], function(x) {
  
  gp = x$pars$p_pars
  
  gp = gp[1:(length(gp)/2)]
  
  gp = do.call(data.frame, gp)
  
  return(gp)
  
  })

table_gl_est(res,
             a = 6,
             'tables/tab1_pa17.txt')

# pachur 2014 data --------------------------------------------------------

mods_sut16 = readRDS("analyses/modeling/posteriors/stan/s16.rds")

# list with results
res = lapply(mods_sut16[c(1, 7:12)], function(x) {
  
  gp = x$pars$p_pars
  
  gp = gp[1:(length(gp)/2)]
  
  gp = do.call(data.frame, gp)
  
  return(gp)
  
})

table_gl_est(res,
             a = 6,
             'tables/tab1_s16.txt')

# pachur 2014 data --------------------------------------------------------

mods_pach14 = readRDS("analyses/modeling/posteriors/stan/p14.rds")

# list with results
res = lapply(mods_pach14[c(1, 7:12)], function(x) {
  
  gp = x$pars$p_pars
  
  gp = gp[1:(length(gp)/2)]
  
  gp = do.call(data.frame, gp)
  
  return(gp)
  
})

table_gl_est(res,
             a = 6,
             'tables/tab1_pa14.txt')