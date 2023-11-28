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

# dft mods
n_mods = names(mods_pach17)[13:15]

M = list(p17 = mods_pach17[n_mods],
         s16 = mods_sut16[n_mods],
         p14 = mods_pach14[n_mods])

rm(mods_pach14, mods_sut16, mods_pach17)

source('analyses/figures_results/99_elpd_extract.R')
source('analyses/figures_results/99_elpd_diff.R')

# model performance -------------------------------------------------------

# gather elpds for all datasets
mods_elpds = lapply(names(M), function(m) {
  
  d = elpd_extract(M[[m]], 
                   elpd = F,
                   n_m = n_mods)
  d$data = m
  d$mod = rownames(d)
  
  return(d)
  
}); mods_elpds = data.frame(rbindlist(mods_elpds))


# add info on monetary-medical
mods_elpds$mod = c('DFT', 'AV', 'APW')
mods_elpds$mod = factor(mods_elpds$mod,
                        levels = c('DFT', 'AV', 'APW'),
                        ordered = T)

#
mods_elpds$data = factor(mods_elpds$data,
                         levels = c('s16', 'p17', 'p14'),
                         ordered = T)

mods_elpds$out = ifelse(mods_elpds$data %in% c('s16', 'p17'),
                        'neg', 'pos')

mods_elpds$data = factor(mods_elpds$data,
                         levels = c('s16', 'p17', 'p14'),
                         labels = c('SUter et al. (2016)', 
                                    'Pachur et al. (2017)', 
                                    'Pachur et al. (2014)'),
                         ordered = T)

mods_elpds$out = factor(mods_elpds$out,
                        levels = c('neg', 'pos'),
                        labels = c('Negative outcomes', 'Positive outcomes'),
                        ordered = T)

# model comparison --------------------------------------------------------

# list with all pairwise comparisons
p = combn(1:3, 2, simplify = F, FUN = function(x) n_mods[x])

# working comp names
com_n = c('av_wtp', 'aw_wtp',  'aw_av')

names(p) = com_n

# names for the figure
mn1 = c( 'DFT', 'DFT', 'AV')
mn2 = c('AV', 'APW', 'APW')

# gather elpds for all datasets
mods_elpd_d = lapply(names(M), function(m) {
  
  d = elpd_diff(M[[m]], p = p, n_m = n_mods)
  d$data = m
  d$mn1 = factor(mn1,
                 levels = unique(mn1),
                 ordered = T)
  d$mn2 = factor(mn2,
                 levels = c(unique(mn2)),
                 ordered = T)
  
  return(d)
  
})

mods_elpd_d = data.frame(rbindlist(mods_elpd_d))

mods_elpd_d$data = factor(mods_elpd_d$data,
                          levels = c('s16', 'p17', 'p14'),
                          ordered = T)

mods_elpd_d$out = ifelse(mods_elpd_d$data %in% c('s16', 'p17'),
                         'neg', 'pos')

mods_elpd_d$out = factor(mods_elpd_d$out,
                         levels = c('neg', 'pos'),
                         labels = c('Negative outcomes', 'Positive outcomes'),
                         ordered = T) 

# Fig C1A ---------------------------------------------------------------

# model performance
mod_perf_pl = ggplot(data = mods_elpds,
                     mapping = aes(x = data, 
                                   y = elpd, 
                                   ymin = li,
                                   ymax = ui,
                                   col = mod,
                                   shape = data)) +
  geom_point(position = position_dodge(.75)) +
  geom_linerange(show.legend = F,
                 position = position_dodge(.75)) +
  scale_color_manual(name = 'Model',
                     values = viridis::viridis(3, .7)) +
  scale_shape_manual(name = 'Model',
                     values = 1:3,
                     guide = 'none') +
  xlab('Data set') +
  geom_hline(yintercept = .5, 
             lty = 2,
             col = rgb(.5, .5, .5, .9)) +
  scale_y_continuous(name = expression('ccp'['loo']),
                     breaks = seq(.5, 1, .05),
                     minor_breaks = seq(.5, 1, .025)) +
  scale_x_discrete(name = 'Data set') +
  facet_grid(. ~ out, scales = "free_x", space = 'free') +
  ggtitle( 'Nonmonetary choices: model performance',
           'Mean (+/- 1SD) individual proportion of correct predictions') +
  theme_bw() +
  theme(legend.position = 'top',
        plot.title = element_text(face = 'bold'),
        panel.grid.major.y = element_line(linewidth = 1),
        panel.grid.major.x = element_blank()) +
  guides(color = guide_legend(nrow = 1))

# Fig C1B ------------------------------------------------------------------

comp_plts = ggplot(data = mods_elpd_d, 
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
                       values = 1:3) +
    ylab(expression('elpd'['loo']~'difference')) +
    theme_bw() +
    facet_grid(rows = vars(data),
               cols = vars(mn2),
               space = 'free_x',
               scales = 'free',
               labeller = labeller(data = c(p14 = 'Pa. (2014)',
                                            p17 = 'Pa. (2017)',
                                            s16 = 'Su. (2016)'))
               ) +
    ggtitle('Model performance comparisons',
            'Evidence in favor of the model in the panel title against the model on the x-axis') +
    theme( plot.title = element_text(face = 'bold'),
           legend.position = 'none',
           axis.title.x = element_blank())

# Fig C01 into file ---------------------------------------------------------

# main results
figC01 = mod_perf_pl / comp_plts +
  plot_layout(heights = c(1, 2))

ggsave('analyses/figures_results/figs/FigC01.pdf',
       plot = figC01,
       device = 'pdf',
       units = 'cm',
       width = 16,
       height = 14,
       scale = 1.2)