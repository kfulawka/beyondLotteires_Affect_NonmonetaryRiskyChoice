rm(list = ls())

# set up ------------------------------------------------------------------

# libraries
library(ggplot2)
library(patchwork)
library(data.table)
library(loo)

# modeling results
mods_pach14 = readRDS("analyses/modeling/posteriors/stan/p14.rds")
mods_pach17 = readRDS("analyses/modeling/posteriors/stan/p17.rds")
mods_sut16 = readRDS("analyses/modeling/posteriors/stan/s16.rds")

M = list(p17 = mods_pach17,
          s16 = mods_sut16,
          p14 = mods_pach14)

rm(mods_pach14, mods_sut16, mods_pach17)

source('analyses/figures_results/99_elpd_extract.R')
source('analyses/figures_results/99_elpd_diff.R')

# model performance -------------------------------------------------------

# model names for the visualizations
n_mods = names(M[[1]])

# gather elpds for all datasets
mods_elpds = lapply(names(M), function(m) {
  
  d = elpd_extract(M[[m]], 
                   elpd = F,
                   n_m = n_mods[1:12])
  d$data = m
  d$mod = rownames(d)
  
  return(d)
  
}); mods_elpds = data.frame(rbindlist(mods_elpds))


# add info on monetary-medical
mods_elpds$domain = rep( c('Monetary', 'Nonmonetary'), each = 6 ) 

mods_elpds$mod = c('CPT', 'AV', 'rAV', 'APW', 'rAV-APW', 'rAV-rAPW')
mods_elpds$mod = factor(mods_elpds$mod,
                        levels = c('CPT', 'AV', 'rAV', 'APW', 
                                   'rAV-APW', 'rAV-rAPW'),
                        ordered = T)

#
mods_elpds$data = factor(mods_elpds$data,
                         levels = c('s16', 'p17', 'p14'),
                         ordered = T)

mods_elpds$out = ifelse(mods_elpds$data %in% c('s16', 'p17'),
                        'neg', 'pos')

mods_elpds$data = factor(mods_elpds$data,
                         levels = c('s16', 'p17', 'p14'),
                         labels = c('Suter et al. (2016)', 
                                    'Pachur et al. (2017)', 
                                    'Pachur et al. (2014)'),
                         ordered = T)

mods_elpds$out = factor(mods_elpds$out,
                        levels = c('neg', 'pos'),
                        labels = c('Negative outcomes', 'Positive outcomes'),
                        ordered = T)

# model comparison --------------------------------------------------------

# list with all pairwise comparisons
p = lapply(list(mon = 1:6,
                nmon = 7:12), function(pc) {
                  
                  combn(pc, 2, simplify = F, FUN = function(x) n_mods[x])
                  
                })

# working comp names
com_n = c('av_wtp', 'rav_wtp', 'aw_wtp', 'ravAw_wtp', 'raw_wtp',
          'rav_av', 'aw_av', 'ravAw_av', 'raw_av',
          'aw_rav', 'ravAw_rav', 'raw_rav',
          'ravAw_aw', 'raw_aw', 
          'raw_ravAw')

# names for the figure
mn1 = c( rep('CPT', 5), rep('AV', 4),  rep('rAV', 3), rep('APW', 2), 'rAV-APW')
mn2 = c('AV', 'rAV', 'APW', 'rAV-APW', 'rAV-rAPW', 
        'rAV', 'APW', 'rAV-APW', 'rAV-rAPW',
        'APW', 'rAV-APW', 'rAV-rAPW',
        'rAV-APW', 'rAV-rAPW', 
        'rAV-rAPW')

p = lapply(p, function(pp) { names(pp) = com_n; return(pp)})

# gather elpds for all datasets
mods_elpd_d = lapply(p, function(pp) {
  
  dd = lapply(names(M), function(m) {
    
    d = elpd_diff(M[[m]], p = pp, n_m = n_mods)
    d$data = m
    d$mn1 = factor(mn1,
                   levels = unique(mn1),
                   ordered = T)
    d$mn2 = factor(mn2,
                   levels = c(unique(mn2)),
                   ordered = T)
    
    return(d)
    
  })
  
  dd = data.frame(rbindlist(dd))
  
  dd$data = factor(dd$data,
                   levels = c('s16', 'p17', 'p14'),
                   ordered = T)
  
  dd$out = ifelse(dd$data %in% c('s16', 'p17'),
                  'neg', 'pos')
  
  dd$out = factor(dd$out,
                  levels = c('neg', 'pos'),
                  labels = c('Negative outcomes', 'Positive outcomes'),
                  ordered = T)
  
  # remove APW v RVO -- this one is not of any interest
  dd = dd[dd$comp != 'aw_rav', ]
  
  return(dd)
  
})

# Fig 4A ---------------------------------------------------------------

# model performance
mod_perf_pl = lapply(list(nonmonetary = 'Nonmonetary', monetary = 'Monetary'), 
                      function(cc) {
                        
                        mod_perf_pl1 = ggplot(data = mods_elpds[mods_elpds$domain == cc, ],
                                               mapping = aes(x = data, 
                                                             y = elpd, 
                                                             ymin = li,
                                                             ymax = ui,
                                                             col = mod,
                                                             shape = data)) +
                          geom_point(position = position_dodge(.75)) +
                          # geom_text(aes(label = rep(c('CPT', 'AV', 'RAV', 'AW', 'RavAw', 'RAW'), 3)),
                          #           position = position_dodge(.75)) +
                          geom_linerange(show.legend = F,
                                        position = position_dodge(.75)) +
                          scale_color_manual(name = 'Model',
                                             values = viridis::viridis(6, 1)) +
                          scale_shape_manual(name = 'Model',
                                             values = 1:3,
                                             guide = 'none') +
                          xlab('Data set') +
                          geom_hline(yintercept = .5, 
                                     lty = 2,
                                     col = rgb(.5, .5, .5, .9)) +
                          scale_y_continuous(name = expression('accuracy'['loo']),
                                             breaks = seq(.5, 1, .05),
                                             minor_breaks = seq(.5, 1, .025)) +
                          scale_x_discrete(name = 'Data set') +
                          facet_grid(. ~ out, scales = "free_x", space = 'free') +
                          ggtitle( paste0(cc, ' choices: model performance'),
                                   'Mean (+/- 1SD) individual proportion of correct predictions') +
                          theme_bw() +
                          theme(legend.position = 'top',
                                plot.title = element_text(face = 'bold'),
                                # text = element_text(family = 'serif'),
                                panel.grid.major.y = element_line(linewidth = 1),
                                panel.grid.major.x = element_blank()) +
                          guides(color = guide_legend(nrow = 1))
                        
                      } )

# Fig 4B ------------------------------------------------------------------

comp_plts = lapply(c('nmon', 'mon'), function(d) {
  
  plt = ggplot(data = mods_elpd_d[[d]], 
               mapping = aes(x = mn1, 
                             y = elpd_d, 
                             ymin = li,
                             ymax = ui,
                             shape = data,
                             group = data)) +
    geom_point(position = position_dodge(.5),
               col = 'black',
               show.legend = T) +
    geom_errorbar(width = .03,
                  show.legend = T,
                  position = position_dodge(.5),
                  col = 'black') +
    geom_hline(yintercept = 0, 
               lty = 2,
               col = rgb(.5, .5, .5, .9)) +
    scale_shape_manual(name = 'Data',
                       values = 1:3,
                       labels = c('Suter et al. (2016)',
                                  'Pachur et al. (2017)',
                                  'Pachur et al. (2014)')) +
    # ylab(expression('elpd'['r']~'difference')) +
    ylab(expression('elpd'['loo']~'difference')) +
    theme_bw() +
    facet_grid(rows = vars(data),
               cols = vars(mn2),
               space = 'free_x',
               scales = 'free',
               labeller = labeller(data = c(p14 = 'Pa. (2014)',
                                            p17 = 'Pa. (2017)',
                                            s16 = 'Su. (2016)'))) +
    ggtitle('Model performance comparisons',
            'Evidence in favor of the model in the panel title against the model on the x-axis') +
    theme( plot.title = element_text(face = 'bold'),
           legend.position = 'none',
           axis.text.x = element_text(angle = 10),
           axis.title.x = element_blank())
  
  return(plt)
  
})

# Fig 4 into file ---------------------------------------------------------

# main results
fig04 = mod_perf_pl$nonmonetary / comp_plts[[1]] +
  plot_layout(heights = c(1, 2))

ggsave('analyses/figures_results/figs/Fig04.pdf',
       plot = fig04,
       device = 'pdf',
       units = 'cm',
       width = 16,
       height = 14,
       scale = 1.2)


# fig 6 into file ---------------------------------------------------------

# main results
fig06 = mod_perf_pl$monetary / comp_plts[[2]] +
  plot_layout(heights = c(1, 2))

ggsave('analyses/figures_results/figs/Fig06.pdf',
       plot = fig06,
       device = 'pdf',
       units = 'cm',
       width = 16,
       height = 14,
       scale = 1.2)