

#Interventions are scaled up linearly from January 1st of START.YEAR to January 1st of IMPLEMENTED.BY.YEAR
START.YEAR = 2025
IMPLEMENTED.BY.YEAR = 2030

WHOLE.POPULATION = create.target.population(name = 'Whole Population')

testing.increase = create.intervention.effect('general.population.testing', #testing rate
                                              start.time = START.YEAR,
                                              effect.values = 'testing.multiplier',
                                              scale = 'rate',
                                              apply.effects.as = 'multiplier', #times what testing rate would have been on 1/1/2030
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = F, #do not allow testing rate to decrease vs. NC
                                              allow.values.greater.than.otherwise = T) #allow testing rate to increase vs. NC

#Suggest initial parameter distributions
test.intervention = create.intervention(WHOLE.POPULATION,
                                        testing.increase,
                                        parameter.distribution = join.distributions(
                                            testing.multiplier=Uniform.Distribution(4,6)
                                        ))


# model no intervention
noint=get.null.intervention()

# simset = get(load("../jheem_analyses/cached/temp/full.with.aids_simset_2024-07-08_C.12580.Rdata"))
simset = get(load("../jheem_analyses/cached/temp/test.simset_2024.07.24.Rdata"))
simset = copy.simulation.set(simset) # to make sure is up to date
sim.noint = noint$run(simset, start.year=2025, end.year=2035, verbose=T)

sim.int = test.intervention$run(simset, start.year=2025, end.year=2035, verbose=T)


# beta: couldn't get outcome above 0.3.
testing.criterion = create.monotonic.criterion(parameter.name = 'testing.multiplier',
                                               outcome = 'testing',
                                               inversely.related = F,
                                               parameter.scale = 'ratio',
                                               parameter.initial.value = 4,
                                               target.value = .955,
                                               min.acceptable.value = .95,
                                               max.acceptable.value = .96)
criterion.intervention = create.monotonic.criteria.based.intervention(base.intervention = test.intervention,
                                                                      completion.criteria = testing.criterion,
                                                                      max.iterations = 20,
                                                                      n.iterations.after.satisfying.criteria = 5,
                                                                      max.iterations.first.sim = 100,
                                                                      n.iterations.after.satisfying.criteria.first.sim = 20,
                                                                      max.failure.rate = 0,
                                                                      code=NULL, 
                                                                      name=NULL)
sim.crit = criterion.intervention$run(simset, start.year=2025, end.year=2035, verbose=T)

# trying another example to see if it will work
suppression.criterion = create.monotonic.criterion(parameter.name = 'unsuppressed.multiplier',
                                                   outcome = 'suppression',
                                                   inversely.related = T,
                                                   parameter.scale = 'complementary.proportion',
                                                   parameter.initial.value = .5,
                                                   target.value = .955,
                                                   min.acceptable.value = .95,
                                                   max.acceptable.value = .96)
suppression.criterion.intervention = create.monotonic.criteria.based.intervention(base.intervention = test.intervention,
                                                                                  completion.criteria = suppression.criterion,
                                                                                  max.iterations = 20,
                                                                                  n.iterations.after.satisfying.criteria = 5,
                                                                                  max.iterations.first.sim = 100,
                                                                                  n.iterations.after.satisfying.criteria.first.sim = 20,
                                                                                  max.failure.rate = 0,
                                                                                  code=NULL, 
                                                                                  name=NULL)
sim.supp.crit = suppression.criterion.intervention$run(simset, start.year=2025, end.year=2035, verbose=T)


# An intervention where I just pick a parameter and change it.