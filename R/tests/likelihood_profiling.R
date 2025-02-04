
N.ITER = 200

sim  = mcmc@simulations[[length(mcmc@simulations)]]
lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood(sim$version, sim$location)



Rprof()

for (i in 1:N.ITER)
    x = lik$compute(sim, use.optimized.get = T)


summ.lik = summaryRprof()

Rprof(NULL)


lik$compute(sim, debug=T)






