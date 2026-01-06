# new algo test

print("SOURCING CODE")
# source('../jheem_analyses/applications/EHE/ehe_specification.R')
# source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
#source('../jheem_analyses/commoncode/locations_of_interest.R')

CALIBRATION.CODE.POPULATION = 'init.pop.ehe'

N.ITER.TEST = 10000
N.ITER = 20000
N.ITER.FULL = 50000
N.ITER.FINAL = 250000

K = 1000
PTK=2
THIN=1
CACHE.EVERY=200 # sims (if 600, around ten to fifteen minutes)

# load params manual
load("../jheem_analyses/applications/EHE/calibration_runs/params.manual_2024_02_21.Rdata") 

print("REGISTERING CALIBRATIONS")
#-- REGISTER POPULATION CALIBRATION  --#
par.names.pop = c(
    POPULATION.PARAMETERS.PRIOR@var.names,
    "global.trate"#,
)

register.calibration.info(CALIBRATION.CODE.POPULATION,
                          likelihood.instructions = joint.pop.migration.total.trans.likelihood.instructions, # added race/risk transmission targets 10/21
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = par.names.pop,
                          n.iter = N.ITER,
                          thin = 50, 
                          fixed.initial.parameter.values = c(global.trate=0.13), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          n.chains = K, # will be 1000
                          description = "A quick run to get population parameters in the general vicinity"
)

# ----

set.seed(12345)

run.new.algorithm(version='ehe',
                  location='C.12580',
                  calibration.code=CALIBRATION.CODE.POPULATION,
                  k=K,
                  ptk=PTK,
                  thin=THIN,
                  cache.every=CACHE.EVERY,
                  root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                  sub.version = NULL,
                  # cache.frequency = 500,
                  allow.overwrite.cache = F,
                  verbose = T)

