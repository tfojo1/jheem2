
if (1==2)
{
    source('../jheem_analyses/applications/EHE/ehe_specification.R')
    simset = assemble.simulations.from.calibration('ehe', 'C.12580', 'init.transmission.ehe')
    sim = simset[simset$n.sim]
}

source('R/INTERVENTIONS_main.R')
source('../jheem_analyses/applications/ehe_disparities/ehe_disparities_interventions.R')


if (1==2)
{
    simset = assemble.simulations.from.calibration('ehe', 'C.12580', 'full.with.aids')
    
    slim.set = simset[1:3]
    y = sub.intervention$run(slim.set, start.year = 2025, end.year = 2035, verbose = T)
}
#x=sub.intervention$run(sim, start.year = 2025, end.year = 2035, verbose = T)
x=full.intervention$run(sim, start.year = 2025, end.year = 2035, verbose = T)
