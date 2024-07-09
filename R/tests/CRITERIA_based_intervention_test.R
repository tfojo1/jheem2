#Interventions are scaled up linearly from January 1st of START.YEAR to January 1st of IMPLEMENTED.BY.YEAR
START.YEAR = 2025
IMPLEMENTED.BY.YEAR = 2030

#No intervention
noint=get.null.intervention()

#Individual interventions on testing, viral suppression, and PrEP
testing.increase = create.intervention.effect('general.population.testing', #testing rate
                                              start.time = START.YEAR,
                                              effect.values = 'testing.multiplier',
                                              scale = 'rate',
                                              apply.effects.as = 'multiplier', #times what testing rate would have been on 1/1/2030
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = F, #do not allow testing rate to decrease vs. NC
                                              allow.values.greater.than.otherwise = T) #allow testing rate to increase vs. NC

suppression.increase = create.intervention.effect('suppression.of.diagnosed', #percent suppressed
                                                  start.time = START.YEAR,
                                                  effect.values = 'unsuppressed.multiplier',
                                                  scale = 'proportion.staying', #1-p
                                                  apply.effects.as = 'multiplier', #times what % unsuppressed would have been on 1/1/2030
                                                  times = IMPLEMENTED.BY.YEAR,
                                                  allow.values.less.than.otherwise = F, #do not allow % suppressed to decrease vs. NC
                                                  allow.values.greater.than.otherwise = T) #allow % suppressed to increase vs. NC

prep.increase = create.intervention.effect('oral.prep.uptake', #% initiated PrEP
                                           start.time = START.YEAR,
                                           effect.values = 'uninitiated.multiplier',
                                           scale = 'proportion.staying', #1-p
                                           apply.effects.as = 'multiplier', #times what % unsuppressed would have been on 1/1/2030
                                           times = IMPLEMENTED.BY.YEAR,
                                           allow.values.less.than.otherwise = F, #allow % suppressed to decrease vs. NC?
                                           allow.values.greater.than.otherwise = T) #allow % suppressed to increase vs. NC?

#Create joint intervention and specify target population
base.intervention = create.intervention(WHOLE.POPULATION,
                                        testing.increase,
                                        suppression.increase,
                                        prep.increase)


#-- Create Criteria --#
testing.criterion = create.monotonic.criterion(parameter.name = 'testing.multiplier',
                                               outcome = 'testing',
                                               inversely.related = F,
                                               parameter.scale = 'ratio',
                                               parameter.initial.value = 4,
                                               target.value = .955,
                                               min.acceptable.value = .95,
                                               max.acceptable.value = .96)
suppression.criterion = create.monotonic.criterion(parameter.name = 'unsuppressed.multiplier',
                                               outcome = 'suppression.of.diagnosed',
                                               inversely.related = T,
                                               parameter.scale = 'complementary.proportion',
                                               parameter.initial.value = .5,
                                               target.value = .955,
                                               min.acceptable.value = .95,
                                               max.acceptable.value = .96)
prep.criterion = create.monotonic.criterion(parameter.name = 'uninitiated.multiplier',
                                               outcome = 'oral.prep.uptake',
                                               inversely.related = T,
                                               parameter.scale = 'complementary.proportion',
                                               parameter.initial.value = .5,
                                               target.value = .52,
                                               min.acceptable.value = .5,
                                               max.acceptable.value = .55)

crit.int = create.monotonic.criteria.based.intervention <- function(base.intervention = base.intervention,
                                                                    completion.criteria = list(testing.criterion,
                                                                                               suppression.criterion,
                                                                                               prep.criterion),
                                                                    max.iterations = 20,
                                                                    n.iterations.after.satisfying.criteria = 5,
                                                                    max.iterations.first.sim = 100,
                                                                    n.iterations.after.satisfying.criteria.first.sim = 20,
                                                                    max.failure.rate = 0,
                                                                    code=NULL, 
                                                                    name=NULL)