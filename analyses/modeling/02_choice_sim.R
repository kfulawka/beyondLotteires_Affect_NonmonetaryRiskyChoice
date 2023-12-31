rm(list = ls(all.names = T))

library(runjags)
# library(loo)
library(coda)
library(future.apply)

# # data
# load("analyses/modeling/00_p17_mod_dat_stan.RData")
# load("analyses/modeling/00_s16_mod_dat_stan.RData")

# modeling results and data
mods_pach17 = readRDS("analyses/modeling/posteriors/stan/p17.rds")
mods_sut16 = readRDS("analyses/modeling/posteriors/stan/s16.rds")

# function to simulate the choices
source('analyses/modeling/functions/01_cpt_mods.R')

# data for choice simulations

# choice simulation -------------------------------------------------------

# models for simulations
mod_sim = list(pa17_sim = mods_pach17[c('cpt_nmon_wtp', 'cpt_nmon_ar', 'cpt_nmon_radw')],
               su16_sim = mods_sut16[c('cpt_nmon_wtp', 'cpt_nmon_ar', 'cpt_nmon_radw')])

# add simulation settings
mod_sim = lapply(mod_sim, function(x) {
  
  x$cpt_nmon_wtp$set = c(ar = F, rv = F, apwf = F)
  x$cpt_nmon_ar$set = c(ar = T, rv = F, apwf = F)
  x$cpt_nmon_radw$set = c(ar = T, rv = T, apwf = T)
  
  return(x)
  
})

# simulate choices for each data set of interes
co_pa = lapply(mod_sim, function(dat) {
  
  # for each model of interes
  lapply(dat, function(mm) {
    
    # data
    dd = mm$sampling_info$data
    
    # list with ind-lvl post pars
    ipp = mm$pars$i_pars
    
    # no of par
    np = length(ipp)
    
    # par names
    par_n = names(ipp)
    
    # posterior means
    ip = sapply(ipp, colMeans)
    
    # plan(multisession)
    co_pa = sapply(1:80, FUN = function(i) {
      
      # outcome values
      xy = cbind(dd$xA[,i], dd$xB[,i])
      
      # simulate choices of pa
      pa = cpt_mods_pr(xy = xy,
                       p = dd$p,
                       a = ip[i,'alp'],
                       b = ip[i,'beta'],
                       g = ip[i,'gam'],
                       d = ip[i,'del'],
                       t = ip[i,'theta'],
                       ar = mm$set['ar'],
                       rv = mm$set['rv'],
                       apwf = mm$set['apwf'],
                       out = 'pa')
      
    })
    
  })
  
})

# save the results
saveRDS(co_pa, file = 'analyses/modeling/02_pa_co_sim.rds')