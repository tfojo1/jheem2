
DO.RPROF = T


source('../jheem_analyses/applications/EHE/ehe_specification.R')

# change data manager pull to have "time.keep=T" instead of FALSE
SURVEILLANCE.MANAGER = load.data.manager.from.cache('surveillance.manager.rdata', set.as.default=T)
TIME.KEEPER = new.env()

# source your likelihoods

if (DO.RPROF)
    Rprof()

start.time = Sys.time()
suppression.likelihood = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe', 'C.12580')
end.time = Sys.time()

if (DO.RPROF)
{
    Rprof(NULL)
    summ = summaryRprof()
    

run.time = as.numeric(end.time) - as.numeric(start.time)
print(paste0("Done - took ", round(run.time/60,1), " minutes"))

# total time in pull
TIME.KEEPER$total

# time that is getting mapping to apply
TIME.KEEPER$getting.mta

# time that is doing the as.ontology thing
TIME.KEEPER$initial.strat

# time in the main lapply
TIME.KEEPER$lapply

}