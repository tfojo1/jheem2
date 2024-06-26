# change data manager pull to have "time.keep=T" instead of FALSE
SURVEILLANCE.MANAGER = load.data.manager.from.cache('surveillance.manager.rdata', set.as.default=T)
TIME.KEEPER = new.env()
# source your likelihoods
suppression.likelihood = suppression.likelihood.instructions$instantiate.likelihood('ehe', 'C.12580')

# total time in pull
TIME.KEEPER$total

# time that is getting mapping to apply
TIME.KEEPER$getting.mta

# time that is doing the as.ontology thing
TIME.KEEPER$initial.strat

# time in the main lapply
TIME.KEEPER$lapply
