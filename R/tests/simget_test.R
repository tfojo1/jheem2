sim=make.dummy.sim(version='ehe',
                   location='C.12580',
                   from.year=2010,
                   to.year=2021)
# sim$get(outcomes = 'diagnosed.prevalence',keep.dimensions = 'year', dimension.values = list(year=c('2010', '2013'), sex=c('heterosexual_male', 'msm')), check.consistency = F)
# sim$get(outcomes = 'proportion.tested', keep.dimensions = 'year', dimension.values = list(year=c('2010', '2013'), sex=c('heterosexual_male', 'msm')), check.consistency = F)
simdata = sim$get(outcomes = c('diagnosed.prevalence', 'proportion.tested'), keep.dimensions = c('year', 'sex'), dimension.values = list(year=c('2010', '2013'), sex=c('heterosexual_male', 'msm')), check.consistency = T)
