N.SIM = 200
RUN.RPROF = T

source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

print("STARTING PROFILING")

LOCATION = 'NY'
CALIBRATION.CODE.TO.RUN = 'fullprof'


copy.calibration.info(from.code = 'final.ehe.state',
                      to.code = CALIBRATION.CODE.TO.RUN,
                      n.iter = N.SIM,
                      thin = 5)


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
