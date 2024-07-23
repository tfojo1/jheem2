test.population.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age"),
                                       levels.of.stratification = c(0,1), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                        
                                        dimension.values = list(age='25-34 years'),
                                       
                                       # assumes correlation between all combos of years is the same
                                       observation.correlation.form = 'compound.symmetry', 
                                       
                                       # should always be specified; describes how precise the estimates are; 
                                       # e.g., estimates can be off by 3% each year
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       
                                       # downweight because large population size; 
                                       # can get more specific with create.likelihood.weights 
                                       #(e.g., different weight for age X)
                                       weights = 1, 
                                       
                                       # if there are more datapoints for certain years, this will normalize
                                       # e.g., if there are a few years with only the totals 
                                       # before the stratifications are available
                                       equalize.weight.by.year = T 
  )

test.lik = test.population.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
# exp(test.lik$compute(sim2)-test.lik$compute(sim.last))
# 
# exp(test.lik$compute(sim2, debug=T))
