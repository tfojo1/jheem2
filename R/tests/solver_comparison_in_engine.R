

print("SOLVER TEST: Creating Specification")
source('../jheem_analyses/applications/EHE/ehe_specification.R')

params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.04


print("SOLVER TEST: Setting up Engines")
engine.odeintr = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10)
engine.diffeqr = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10,
                                     solver.metadata = create.solver.metadata(method='BS3'))
engine.diffeqr.1e32 = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10,
                                     solver.metadata = create.solver.metadata(method='BS3',
                                                                              rtol = 1e-03))
engine.odeintr.1e32 = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10,
                                          solver.metadata = create.solver.metadata(method='DP5',
                                                                                   rtol = 1e-03))

print("SOLVER TEST: Running")
sim.odeintr = engine.odeintr$run(parameters = params)
sim.odeintr.1e32 = engine.odeintr.1e32$run(parameters = params)
sim.diffeqr = engine.diffeqr$run(params)
sim.diffeqr.1e32 = engine.diffeqr.1e32$run(params)

#params2 = suppressWarnings(get.quantiles(EHE.PARAMETERS.PRIOR, 0.4))
#params2['global.trate'] = 0.1
#sim2 = engine$run(parameters = params2)

print("SOLVER TEST: ALL DONE!")

comp.from.year = 2010

compare.sim.outcomes <- function(sim1, sim2, comp.from.year=2010,
                                 digits = 3,
                                 pct.digits = 1)
{
    abs.diff = sapply(sim1$outcomes, function(outcome){
        
        years = dimnames(sim1[[outcome]])$year
        years = years[years >= comp.from.year]
        if (length(years)==0)
            return (NA)
    
        diff = array.access(sim1[[outcome]], year=years) - array.access(sim2[[outcome]], year=years)    
        diff[order(abs(diff), decreasing = T)][1]
    })
    
    
    rel.diff = sapply(sim1$outcomes, function(outcome){
        
        years = dimnames(sim1[[outcome]])$year
        years = years[years >= comp.from.year]
        if (length(years)==0)
            return (NA)
        
        sub1 = array.access(sim1[[outcome]], year=years)
        diff = (sub1 - array.access(sim2[[outcome]], year=years)) / sub1
        
        diff[order(abs(diff), decreasing = T)][1]
    })
    
    o = order(abs(rel.diff), decreasing = T)
    
    rv = paste0(round(abs.diff, digits), " (",
                round(100*rel.diff, pct.digits), "%)")
    names(rv) = sim1$outcomes
    rv[o]
}

compare.sim.outcomes(sim.odeintr, sim.diffeqr)
compare.sim.outcomes(sim.odeintr, sim.odeintr.1e32)
compare.sim.outcomes(sim.odeintr, sim.diffeqr.1e32)


