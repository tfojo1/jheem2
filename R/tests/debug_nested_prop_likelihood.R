
source('../jheem_analyses/applications/ehe/ehe_specification.R')
msa = 'C.16740'

engine = create.jheem.engine('ehe', msa, end.year=2035, max.run.time.seconds = 10)
params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.13
sim = engine$run(params)

suppression.likelihood.instructions = 
    create.nested.proportion.likelihood.instructions(outcome.for.data = "suppression",
                                                     outcome.for.sim = "suppression",
                                                     denominator.outcome.for.data = 'diagnosed.prevalence',
                                                     
                                                     # want to be able to specify max # for each location type;
                                                     # have to decide how to order (probably by denominator)
                                                     location.types = c('COUNTY','STATE','CBSA'), 
                                                     minimum.geographic.resolution.type = 'COUNTY',
                                                     # limit.to.n.location
                                                     
                                                     dimensions = c("age","sex","race","risk"),
                                                     #dimensions = c("sex"),
                                                     levels.of.stratification = c(0,1), 
                                                     from.year = 2008, 
                                                     
                                                     p.bias.inside.location = suppression.bias.estimates$in.mean, 
                                                     p.bias.outside.location = suppression.bias.estimates$out.mean,
                                                     p.bias.sd.inside.location = suppression.bias.estimates$in.sd,
                                                     p.bias.sd.outside.location = suppression.bias.estimates$out.sd,
                                                     
                                                     within.location.p.error.correlation = 0.5,
                                                     within.location.n.error.correlation = 0.5,
                                                     
                                                     observation.correlation.form = 'compound.symmetry', 
                                                     p.error.variance.term = 0.04560282, # from calculating_error_terms_for_ehe_likelihoods.R
                                                     p.error.variance.type = 'sd',
                                                     
                                                     partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                     
                                                     weights = (1*FULL.WEIGHT),
                                                     equalize.weight.by.year = T
    )


total.suppression.likelihood.instructions = 
    create.nested.proportion.likelihood.instructions(outcome.for.data = "suppression",
                                                     outcome.for.sim = "suppression",
                                                     denominator.outcome.for.data = 'diagnosed.prevalence',
                                                     
                                                     # want to be able to specify max # for each location type;
                                                     # have to decide how to order (probably by denominator)
                                                     location.types = c('COUNTY','STATE','CBSA'), 
                                                     minimum.geographic.resolution.type = 'COUNTY',
                                                     # limit.to.n.location
                                                     
                                                     dimensions = c("age","sex","race","risk"),
                                                     #dimensions = c("sex"),
                                                     levels.of.stratification = c(0), 
                                                     from.year = 2008, 
                                                     
                                                     p.bias.inside.location = suppression.bias.estimates$in.mean, 
                                                     p.bias.outside.location = suppression.bias.estimates$out.mean,
                                                     p.bias.sd.inside.location = suppression.bias.estimates$in.sd,
                                                     p.bias.sd.outside.location = suppression.bias.estimates$out.sd,
                                                     
                                                     within.location.p.error.correlation = 0.5,
                                                     within.location.n.error.correlation = 0.5,
                                                     
                                                     observation.correlation.form = 'compound.symmetry', 
                                                     p.error.variance.term = 0.04560282, # from calculating_error_terms_for_ehe_likelihoods.R
                                                     p.error.variance.type = 'sd',
                                                     
                                                     partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                     
                                                     weights = (1*FULL.WEIGHT),
                                                     equalize.weight.by.year = T
    )

full.lik = suppression.likelihood.instructions$instantiate.likelihood('ehe',msa)
total.lik = total.suppression.likelihood.instructions$instantiate.likelihood('ehe',msa)

total.lik$compute(sim, debug=T)
full.lik$compute(sim, debug=T)
