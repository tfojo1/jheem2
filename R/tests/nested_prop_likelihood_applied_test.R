
p.bias.inside = 0
p.bias.outside = 0
p.bias.sd.inside = 0.03
p.bias.sd.outside = 0.03


suppression.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "suppression",
                                                   outcome.for.sim = "suppression",
                                                   denominator.outcome.for.data = 'diagnosed.prevalence',
                                                   denominator.outcome.for.sim = 'diagnosed.prevalence',
                                                   
                                                   location.types = c('COUNTY','STATE','CBSA'),
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   
                                                   dimensions = c("age","sex","race","risk"),
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = as.integer(2008), 
                                                   
                                                   p.bias.inside.location = p.bias.inside,
                                                   p.bias.outside.location = p.bias.outside,
                                                   p.bias.sd.inside.location = p.bias.sd.inside,
                                                   p.bias.sd.outside.location = p.bias.sd.outside,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   measurement.error.sd = 0.03,
                                                   
                                                   partitioning.function = xx,
                                                   
                                                   weights = list(1), # upweight?
                                                   equalize.weight.by.year = T 
  )
