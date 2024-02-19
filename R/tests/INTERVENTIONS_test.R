INT.TEST.REDO = T

if (INT.TEST.REDO)
    source('R/tests/ENGINE_test.R')
source('R/INTERVENTIONS_main.R')


if (1==2)
{
    
    
    
    print("INTERVENTION TEST - running the intervention")
    simset.noint = noint$run(simset,
                             start.year = 2025,
                             end.year = 2030,
                             keep.from.year = 2000,
                             keep.to.year = 2030)
    #increase.mortality = 
    
    simplot(simset, simset.noint, 'population')
    simplot(simset, simset.noint, 'new')
    

    
    tpop = union.target.populations(
        create.target.population(race='other', sex='msm', name='other race'),
        create.target.population(race = 'black', name='black')
    )
    
    
    simset.int = int$run(simset,
                      start.year = 2025,
                      end.year = 2030,
                      keep.from.year = 2000,
                      keep.to.year = 2030)
    
    simplot(simset.noint, simset.int, 'population', facet.by = 'sex', split.by='race')
    simplot(simset.noint, simset.int, 'population', facet.by = 'sex', split.by='race')
}

if (1==2)
{
    inc.mort = create.intervention.effect('non.idu.general.mortality',
                                          start.time = 2025,
                                          effect.values = .5,
                                          times = 2026,
                                          allow.values.less.than.otherwise = T,
                                          allow.values.greater.than.otherwise = T,
                                          scale = 'rate',
                                          apply.effects.as = 'value'
    )
    
    var.mort = create.intervention.effect('non.idu.general.mortality',
                                          start.time = 2025,
                                          effect.values = 'mort.mult',
                                          times = 2026,
                                          allow.values.less.than.otherwise = T,
                                          allow.values.greater.than.otherwise = T,
                                          scale = 'rate',
                                          apply.effects.as = 'value'
    )
    
    int.dist = Lognormal.Distribution(0,1, var.name='mort.mult')
    
    int = create.intervention(var.mort, WHOLE.POPULATION, parameter.distribution = int.dist)
    int = create.intervention(WHOLE.POPULATION, inc.mort)
    
    sim.int = int$run(sim, 2025, 2030)
    
    simplot(sim, sim.int, 'population')
    
    #int.eng = sim.int$get.engine()
    #int.eng$crunch(prior.sim.index = 1)
    #int.eng$test()
}
 
 
 
 
 test.testing.increase = create.intervention.effect('general.population.testing', #testing rate
                                                    start.time = 2025,
                                                    effect.values = 10,
                                                    scale = 'rate',
                                                    apply.effects.as = 'multiplier', #times what testing rate would have been on 1/1/2030
                                                    times = 2030,
                                                    allow.values.less.than.otherwise = F, #do not allow testing rate to decrease vs. NC
                                                    allow.values.greater.than.otherwise = T) #allow testing rate to increase vs. NC
 
 test.testing.intervention = create.intervention(WHOLE.POPULATION, test.testing.increase)
 
 sim.testing.int = test.testing.intervention$run(sim, 2025, 2030)
 
 
# test.supp = create.intervention.effect('suppression.of.diagnosed',
#                                        start.time = 2025,
#                                        effect.values = .9,
#                                        times = 2030,
#                                        apply.effects.as = 'value',
#                                        scale = 'proportion',
#                                        allow.values.less.than.otherwise = F,
#                                        allow.values.greater.than.otherwise = T)
# test.int = create.intervention(test.supp, BLACK)
# 
# sim.int = test.int$run(sim, 2025, 2030)

 
 source('../jheem_analyses/applications/ehe_disparities/ehe_disparities_interventions.R')
sim.int = full.intervention$run(sim, 2025, 2035, verbose = T) 
