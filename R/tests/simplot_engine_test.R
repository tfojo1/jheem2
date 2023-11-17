

source("R/tests/ENGINE_test.R")
data.manager = load.data.manager('../jheem_analyses/cached/surveillance.manager.rdata')
simplot(sim, outcomes='new', data.manager = data.manager)