# Baltimore

# write instructions, join them
# - new diagnese
# - prevalnce
# - mortality
#also will be:
# - idu #interpose a multiplier
# - total population #will take a tweak to the likelihood
# - variance is sqrt(numerator "population")

# instantiate for Baltimore

# compute for a simulation

total.weight = 1/16
new.weight = 1
prev.weight = 1/4
mort.weight = 1/4

new.instr = create.basic.likelihood.instructions(outcome.for.data = 'diagnoses',
                                                 outcome.for.sim = 'new',
                                                 denominator.outcome.for.sim = 'population',
                                                 dimensions = c('age', 'sex', 'race', 'risk'),
                                                 levels.of.stratification = c(0,1,2),
                                                 denominator.dimensions = character(), # if NULL, is what 'dimensions' are
                                                 measurement.error.coefficient.of.variance = 0.5, # require user choice with no default value to prompt healthy discussions
                                                 weight = total.weight * new.weight #No, I'd like this to be as a list, even if it has just one element
)

prev.instr = create.basic.likelihood.instructions(outcome.for.data = 'prevalence',
                                                  outcome.for.sim = 'diagnosed.prevalence',
                                                  denominator.outcome.for.sim = 'population',
                                                  dimensions = c('age', 'sex', 'race', 'risk'),
                                                  levels.of.stratification = c(0,1,2),
                                                  denominator.dimensions = character(), # if NULL, is what 'dimensions' are
                                                  measurement.error.coefficient.of.variance = 0.67, # require user choice with no default value to prompt healthy discussions
                                                  weight = total.weight * prev.weight
)

mort.instr = create.basic.likelihood.instructions(outcome.for.data = 'hiv deaths',
                                                  outcome.for.sim = 'hiv.mortality',
                                                  denominator.outcome.for.sim = 'population', # would be NULL if we were calibrating to population itself
                                                  dimensions = c('age', 'sex', 'race', 'risk'), # in reality is only by 'sex' but state level could have more
                                                  levels.of.stratification = c(0,1,2),
                                                  denominator.dimensions = character(), # if NULL, is what 'dimensions' are
                                                  measurement.error.coefficient.of.variance = 0.67, # require user choice with no default value to prompt healthy discussions
                                                  weight = total.weight * mort.weight
)

joint.instr = join.likelihood.instructions(new.instr,
                                           prev.instr,
                                           mort.instr) #generate names for each component by its outcome.for.sim

register.likelihood.instructions(joint.instr,
                                 code = 'v1',
                                 description = 'basic likelihood that calibrates to all the basic targets'
                                 )

# Prefer to instantiate just by providing the code (and version, location, etc.)
like = joint.instr$instantiate.likelihood(version = 'ehe',
                                          location = 'c.12580', #MD if need be
                                          data.manager,
                                          throw.error.if.no.data)
like$compute(sim=make.dummy.sim(version='ehe',
                                location='c.12580',
                                from.year=1970,
                                to.year=2025),
             check.consistency = F,
             log = T)

### GOAL: get up to here and instantiate and compute!


# Proportions
# no denominator.dimensions because proportions MUST have the same denominator dimensions as the numerator.
suppression.weight = 1/16

suppression.instr = create.nested.proportion.likelihood.instructions(outcome.for.data = 'suppression',
                                                                     outcome.for.sim = 'suppresssion',
                                                                     dimensions = c('age', 'sex', 'race', 'risk'),
                                                                     leves.of.stratification = c(0,1),
                                                                     measurement.error.coefficient.of.variance = 0.05,
                                                                     sub.location.types = 'county',
                                                                     super.location.types = 'state',
                                                                     weight = total.weight * suppression.weight
                                                                     )
# will pull all data for the MSA as well as all data for the enclosing state(s) and the enclosed county(ies)
# do fancy math to understand correlations between the states, counties and MSA
# take Todd's cpp code and write wrapper to hand off to the cpp

pop.weight = 1/128
pop.instr = create.basic.likelihood.instructions(outcome.for.data = 'population',
                                                 outcome.for.sim = 'population',
                                                 denominator.outcome.for.sim = NULL,
                                                 dimensions = c('age', 'sex', 'race', 'risk'),
                                                 levels.of.stratification = c(0,1,2),
                                                 denominator.dimensions = character(), # if NULL, is what 'dimensions' are
                                                 measurement.error.coefficient.of.variance = 0.5, # require user choice with no default value to prompt healthy discussions
                                                 weight = total.weight * pop.weight
)




pull(keep.dimensions = ...,
     dimension.values = list(year=)) # have a data manager function that shares the highest/lowest years


new.instr = create.basic.likelihood.instructions(outcome.for.data = 'diagnoses',
                                                 outcome.for.sim = 'new',
                                                 dimensions = c('age', 'sex', 'race', 'risk'),
                                                 levels.of.stratification = c(0,1,2),
                                                 from.year = -Inf, #-Inf
                                                 to.year = Inf, #Inf
                                                 omit.years = NULL, #NULL
                                                 correlation.different.years = #0.5,
                                                     correlation.different.strata = #0.1,
                                                     correlation.different.sources = #0.3,
                                                     correlation.same.source.different.details = #0.3,
                                                     measurement.error.coefficient.of.variance = #0.5, # require user choice with no default value to prompt healthy discussions
                                                     weights = list(1/4)
)



create.likelihood.weights(total.weight=4,
                          create.likelihood.subweights(weight=2,
                                                       dimension.values = list(risk='msm')),
                          create.likelihood.subweights(weight=0.5,
                                                       dimension.values = list(age=c(4,5))))
                          
list(create.likelihood.weights(weight=4, dimension.values=list()),
   5,
   create.likelihood.weights(weight=2, dimension.values = list(risk='msm', race=c('black', 'hispanic'))))

# acceptable
weights = 6 #numeric vector
weights = list(6) #numeric vectors and weights objects BEFORE processing; afterwards, is only likelihood weights objects in the list
weights = list(6, c(5,4,3), create...)
weights = create.likelihood.weights(6) #jheem.weights.object
weights = create.likelihood.weights(6, dimension.values = list())