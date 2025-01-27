
source('../jheem_analyses/applications/EHE/ehe_specification.R')

params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.13



engine.bdf = create.jheem.engine('ehe',
                                 'C.12580',
                                 end.year=2030,
                                 solver.metadata = create.solver.metadata(method='BDF'))
engine.dp5 = create.jheem.engine('ehe',
                                 'C.12580',
                                 end.year=2030,
                                 solver.metadata = create.solver.metadata(method='DP5'))
engine.lsoda = create.jheem.engine('ehe',
                                   'C.12580',
                                   end.year=2030,
                                   solver.metadata = create.solver.metadata(method='lsoda',
                                                                            atol = 1e-1,
                                                                            rtol = 1e-2))
engine.bdf = create.jheem.engine('ehe',
                                 'C.12580',
                                 end.year=2030,
                                 solver.metadata = create.solver.metadata(method='bdf',
                                                                          package='deSolve'))
engine.adams = create.jheem.engine('ehe',
                                 'C.12580',
                                 end.year=2030,
                                 solver.metadata = create.solver.metadata(method='adams',
                                                                          package='deSolve'))

engine.adams = create.jheem.engine('ehe',
                                 'C.12580',
                                 end.year=2030,
                                 solver.metadata = create.solver.metadata(method='Adams',
                                                                          atol = 1e-07,
                                                                          rtol = 1e-07))
engine.ode45 = create.jheem.engine('ehe',
                                   'C.12580',
                                   end.year=2030,
                                   solver.metadata = create.solver.metadata(method='ode45',
                                                                            package='deSolve'))
engine.rk4 = create.jheem.engine('ehe',
                                   'C.12580',
                                   end.year=2030,
                                   solver.metadata = create.solver.metadata(method='rk4',
                                                                            package='deSolve',
                                                                            rtol=1e-04,
                                                                            atol=1e-03))

sim.bdf = engine.bdf$run(parameters = params)
sim.adams = engine.adams$run(parameters = params)
sim.dp5 = engine.dp5$run(parameters = params)
sim.lsoda = engine.lsoda$run(parameters = params)
sim.ode45 = engine.ode45$run(parameters = params)
sim.rk4 = engine.rk4$run(parameters = params)



simplot(sim.dp5, sim.ode45, sim.rk4, 'new')
simplot(sim.dp5, sim.ode45, sim.rk4, 'new', split.by='race', facet.by='risk')
simplot(sim.dp5, sim.ode45, sim.rk4, 'diagnosed.prevalence')
simplot(sim.dp5, sim.ode45, sim.rk4, 'diagnosed.prevalence', split.by='race', facet.by='risk')
simplot(sim.dp5, sim.ode45, sim.rk4, 'aids.diagnoses', facet.by='risk')
simplot(sim.dp5, sim.ode45, sim.rk4, 'population')
simplot(sim.dp5, sim.ode45, sim.rk4, 'population', split.by='race',facet.by='age')



simplot(sim.dp5, sim.lsoda, 'new')
simplot(sim.dp5, sim.ode45, 'new')
simplot(sim.dp5, sim.rk4, 'new')

simplot(sim.dp5, sim.bdf, sim.adams, 'new')
simplot(sim.dp5, sim.bdf, sim.adams, 'diagnosed.prevalence')

simplot(sim.dp5, sim.bdf, 'new')

simplot(sim.dp5, sim.bdf, 'population')
simplot(sim.dp5, sim.adams, 'population')
