rm(list = ls())

source('analyses/figures_results/choice_prop_fig.R')

# simulated p(A)
pa_sim = readRDS('analyses/modeling/02_pa_co_sim.rds')

# pachur 2017 -------------------------------------------------------------

# modeling results and data
mods_pach17 = readRDS("analyses/modeling/posteriors/stan/p17.rds")

# figure
choice_prop_fig(mm = mods_pach17[c('cpt_nmon_wtp', 'cpt_nmon_ar', 'cpt_nmon_radw')],
                pa = pa_sim$pa17_sim,
                choices = mods_pach17$cpt_nmon_wtp$sampling_info$data$co,
                p = mods_pach17$cpt_nmon_wtp$sampling_info$data$p,
                fig_name = 'Fig05.pdf')

# suter 2016 --------------------------------------------------------------

# modeling results
mods_sut16 = readRDS("analyses/modeling/posteriors/stan/s16.rds")

# figure
choice_prop_fig(mm = mods_sut16[c('cpt_nmon_wtp', 'cpt_nmon_ar', 'cpt_nmon_radw')],
                pa = pa_sim$su16_sim,
                choices = mods_sut16$cpt_nmon_wtp$sampling_info$data$co,
                p = mods_sut16$cpt_nmon_wtp$sampling_info$data$p,
                fig_name = 'Fig05_s16.pdf')

# # pachur 2014 -------------------------------------------------------------
# 
# # data
# source('analyses/data_prep/00c_pachur2014.R')
# p <- probs
# colnames(p) <- c('pA', 'pB')
# 
# rm(list = setdiff(ls(), c('choices', 'p', 'choice_prop_fig')) )
# 
# # modeling results
# mods_pach14 = readRDS("analyses/modeling/posteriors/stan/p14.rds")
# 
# # figure
# choice_prop_fig(mm = mods_pach14[3:6],
#                 choices = choices[,,2],
#                 p = p,
#                 fig_name = 'Fig05_pa14.pdf')
