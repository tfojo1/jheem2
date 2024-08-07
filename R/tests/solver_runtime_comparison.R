
N.SIM = 50

print("SOLVER TEST: Creating Specification")
source('../jheem_analyses/applications/EHE/ehe_specification.R')

params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.04

engines = list(
    odeintr.42 = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10),
    odeintr.32 = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10,
                                     solver.metadata = create.solver.metadata(rtol=1e-03)),
    bs3.42 = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10,
                                  solver.metadata = create.solver.metadata(method='BS3')),
    diffeqr.dp5.42 = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10,
                                 solver.metadata = create.solver.metadata(method='DP5', package='diffeqr')),
    bs3.32 = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10,
                                 solver.metadata = create.solver.metadata(method='BS3', rtol=1e-03)),
    rk4 = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10,
                                 solver.metadata = create.solver.metadata(method='RK4'))
    
)

sims = list()
for (engine in engines)
    sim = engine$run(params)

print(paste0("SOLVER TEST: Running first sim on ", length(engines), " engines"))

print(paste0("SOLVER TEST: Running ", N.SIM, " test sims on each of ", length(engines), " engines"))
times = rep(0, length(engines))
names(times) = names(engines)

for (i in 1:N.SIM)
{
    for (j in 1:length(engines))
    {
        start.time = Sys.time()
        sims[[i]] = engines[[j]]$run()
        end.time = Sys.time()
        
        times[j] = times[j] + as.numeric(end.time) - as.numeric(start.time)
    }
    
    cat("Done iteration ", i, " of ", N.SIM, '\n')
}


print("SOLVER TEST: Done Running")

o = order(times)
cat(paste0("Times (seconds) for each solver per sim: \n",
             paste0(" - ", names(times[o]), ": ", round(times[o]/N.SIM, 2), collapse='\n'),
           "\n"))

cat(paste0("The fastest was ", names(times)[o][1], ". Relative to this, solvers took: \n",
             paste0(" - ", names(times[o][-1]), ": ",
                    round(times[o][-1] / times[o][1], 2), collapse='\n'),
           "\n"))

cat(paste0("Relative to ", names(times)[1], ". Relative to this, solvers took: \n",
           paste0(" - ", names(times[o]), ": ",
                  round(times[o] / times[1], 2), collapse='\n'),
           "\n"))
