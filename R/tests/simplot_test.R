
library(ggplot2) # and ggsci
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('R/tests/make_dummy_sim.R')
source('R/PLOTS_simplot.R')
if (exists('surveillance.manager'))
    surveillance.manager = load.data.manager('../jheem_analyses/cached/surveillance.manager_080923.rdata')


my.simplot = simplot(sim=make.dummy.sim(version='ehe',
                                        location='MD',
                                        from.year=2006,
                                        to.year=2023),
                     outcomes=c('new'), # remember to add in the hard-coded corresponding observed outcomes in PLOTS_simplot.R
                     split.by = 'risk',
                     facet.by = c('sex'),
                     dimension.values = list(), #c(sex='female'), Why did Todd have this as a vector?
                     data.manager = surveillance.manager,
                     style.manager = NULL)
