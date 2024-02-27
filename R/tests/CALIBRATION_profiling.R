source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

set.jheem.root.directory('Q:test')

N.SIM = 50
RUN.RPROF = F
LOCATION = BALTIMORE.MSA
CALIBRATION.CODE.TO.RUN = 'test' # or CALIBRATION.CODE.POPULATION


# a hack for testing
register.calibration.info(CALIBRATION.CODE.TO.RUN,
                          # removed pop likelihoods, added aids diagnoses 
                          likelihood.instructions = one.way.transmission.and.aids.likelihood.instructions, 
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = c(par.names.transmission),
                          n.iter = N.SIM,
                          thin = 5, 
                          fixed.initial.parameter.values = params.manual[par.names.transmission], 
                          pull.parameters.and.values.from.preceding = F,
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "A quick TESTING run to get transmission parameters in the general vicinity",
                          preceding.calibration.codes = CALIBRATION.CODE.POPULATION
)

clear.calibration.cache(version='ehe',
                        location=LOCATION,
                        calibration.code = CALIBRATION.CODE.TO.RUN)

set.up.calibration(version='ehe',
                   location=LOCATION,
                   calibration.code = CALIBRATION.CODE.TO.RUN,
                   cache.frequency = N.SIM)  

set.seed(12345)
start.time = Sys.time()
print(paste0("STARTING MCMC RUN AT ",Sys.time()))

if (RUN.RPROF)
    Rprof()

mcmc = run.calibration(version = 'ehe',
                       location = LOCATION,
                       calibration.code = CALIBRATION.CODE.TO.RUN,
                       chains = 1,
                       update.frequency = 50,
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


simset = assemble.simulations.from.calibration(version = 'ehe', calibration.code = CALIBRATION.CODE.TO.RUN, location = LOCATION)
