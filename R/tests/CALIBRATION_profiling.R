N.SIM = 50
RUN.RPROF = T
SOLVER.METHOD = 'DP5'

source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

print("STARTING PROFILING")

set.jheem.root.directory('Q:test')
LOCATION = 'C.12580'
CALIBRATION.CODE.TO.RUN = paste0('calprof.', SOLVER.METHOD) # CALIBRATION.CODE.POPULATION

par.names.pop = c("black.birth.rate.multiplier",
                  "hispanic.birth.rate.multiplier",
                  "other.birth.rate.multiplier",
                  "age1.non.idu.general.mortality.rate.multiplier",
                  "age2.non.idu.general.mortality.rate.multiplier",
                  "age3.non.idu.general.mortality.rate.multiplier",
                  "age4.non.idu.general.mortality.rate.multiplier",
                  "age5.non.idu.general.mortality.rate.multiplier",
                  # "black.age1.aging.multiplier",
                  # "hispanic.age1.aging.multiplier",
                  # "other.age1.aging.multiplier",
                  # "black.age2.aging.multiplier",
                  # "hispanic.age2.aging.multiplier",
                  # "other.age2.aging.multiplier",
                  "black.age1.aging.multiplier.1",
                  "hispanic.age1.aging.multiplier.1",
                  "other.age1.aging.multiplier.1",
                  "black.age2.aging.multiplier.1",
                  "hispanic.age2.aging.multiplier.1",
                  "other.age2.aging.multiplier.1",
                  "black.age1.aging.multiplier.2",
                  "hispanic.age1.aging.multiplier.2",
                  "other.age1.aging.multiplier.2",
                  "black.age2.aging.multiplier.2",
                  "hispanic.age2.aging.multiplier.2",
                  "other.age2.aging.multiplier.2",
                  
                  "black.age3.aging.multiplier",
                  "hispanic.age3.aging.multiplier",
                  "other.age3.aging.multiplier",
                  "age4.aging.multiplier",
                  # "black.domino.aging.multiplier",
                  # "hispanic.domino.aging.multiplier",
                  # "other.domino.aging.multiplier",
                  "immigration.multiplier.time.1",
                  "immigration.multiplier.time.2",
                  "emigration.multiplier.time.1",
                  "emigration.multiplier.time.2",
                  "black.migration.multiplier.time.1",
                  "black.migration.multiplier.time.2",
                  "hispanic.migration.multiplier.time.1",
                  "hispanic.migration.multiplier.time.2",
                  "other.migration.multiplier.time.1",
                  "other.migration.multiplier.time.2",
                  "age1.migration.multiplier.time.1",
                  "age1.migration.multiplier.time.2",
                  "age2.migration.multiplier.time.1",
                  "age2.migration.multiplier.time.2",
                  "age3.migration.multiplier.time.1",
                  "age3.migration.multiplier.time.2",
                  "age4.migration.multiplier.time.1",
                  "age4.migration.multiplier.time.2",
                  "age5.migration.multiplier.time.1",
                  "age5.migration.multiplier.time.2",
                  "global.trate"
)



register.calibration.info(CALIBRATION.CODE.TO.RUN,
                          likelihood.instructions = joint.pop.migration.total.trans.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = par.names.pop,
                          n.iter = N.SIM,
                          thin = 5, 
                          fixed.initial.parameter.values = c(global.trate=0.02), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                     #     solver.metadata = create.solver.metadata(method=SOLVER.METHOD, rtol = 1e-06, atol = 1e-06),
                          description = "A quick run to get population parameters in the general vicinity"
)

clear.calibration.cache(version='ehe',
                        location=LOCATION,
                        calibration.code = CALIBRATION.CODE.TO.RUN)

# all.time = 0
# loop.time = 0
# pre.bindings.time = 0
# bindings.time = 0
# eval.time = 0
# fn.comp.time = 0
# incorporate.time = 0
# outer.loop.time = 0
# outside.loop.time = 0

set.up.calibration(version='ehe',
                   location=LOCATION,
                   calibration.code = CALIBRATION.CODE.TO.RUN,
                   cache.frequency = N.SIM)  

set.seed(12345)
start.time = Sys.time()
print(paste0("STARTING MCMC RUN AT ", Sys.time()))

if (RUN.RPROF)
    Rprof()

# all.time = 0
# loop.time = 0
# pre.bindings.time = 0
# bindings.time = 0
# eval.time = 0
# fn.comp.time = 0
# incorporate.time = 0
# outer.loop.time = 0
# outside.loop.time = 0

mcmc = run.calibration(version = 'ehe',
                       location = LOCATION,
                       calibration.code = CALIBRATION.CODE.TO.RUN,
                       chains = 1,
                       update.frequency = 10,
                       update.detail = 'med')
end.time = Sys.time()
run.time = as.numeric(end.time) - as.numeric(start.time)

if (RUN.RPROF)
{
    summ = summaryRprof()
    Rprof(NULL)
}

print(paste0("DONE RUNNING MCMC: Took ",
             round(run.time/60, 0), " minutes to run ",
             format(N.SIM, big.mark = ","),
             " simulations (",
             round(run.time / N.SIM, 1), " seconds per simulation on average)"))


# simset = assemble.simulations.from.calibration(version = 'ehe', calibration.code = CALIBRATION.CODE.TO.RUN, location = LOCATION)


# times = c(
#     all = all.time,
#     outside.loop = outside.loop.time,
#     outer.loop = outer.loop.time,
#     loop = loop.time,
#     pre.bindings = pre.bindings.time,
#     bindings = bindings.time,
#     eval = eval.time,
#     fn.comp = fn.comp.time,
#     incorporate = incorporate.time
# )

# cat(paste0("% of all time: \n",
#            paste0(names(times), ": ",
#                   round(100*times/all.time, 1), "%",
#                   collapse='\n'), "\n"))
