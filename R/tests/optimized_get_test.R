
if (1==2)
{
    source('R/tests/ENGINE_test.R')
    
    sim = engine$run()
    sim.metadata = get.simulation.metadata('ehe','C.12580')
    
    
    sim = join.simulation.sets(sim, sim)
    sim.metadata = get.simulation.metadata('ehe','C.12580', n.sim=2)
}



args = list(
    outcomes = c('suppression','proportion.general.population.tested'),
    keep.dimensions = c('year','race','sex','risk'),
    output = 'value',
    replace.inf.values.with.zero = T,
    dimension.values = list(year=as.character(2008:2020),
                            race = c('black','hispanic'),
                            sex = c('msm','female'))
)

opt.instr = do.call(sim.metadata$prepare.optimized.get.instructions, args)

opt.result = sim$optimized.get(opt.instr)

get.result = do.call(sim$get, args)

range(get.result-opt.result)

success = all(abs(get.result - opt.result) < .00001)

if (success)
    print("optimized.get worked!")
if (!success)
{
    print("The optimized.get results DO NOT LINE UP!!!")
    stop("Failure :(")
}

# time test

N.TEST = 200

opt.start = Sys.time()
for (i in 1:N.TEST)
    opt.result = sim$optimized.get(opt.instr)
opt.end = Sys.time()

get.start = Sys.time()
for (i in 1:N.TEST)
    get.result = do.call(sim$get, args)
get.end = Sys.time()

opt.time = as.numeric(opt.end) - as.numeric(opt.start)
get.time = as.numeric(get.end) - as.numeric(get.start)

print(paste0("get took ",
             round(get.time/opt.time, 1), " times as long as optimized.get"))