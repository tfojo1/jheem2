MCMC.SUB.DIRECTORY = 'mcmc_runs'
SIMULATION.SUB.DIRECTORY = 'simulations'

get.jheem.file.path <- function(version, location, calibration.code)
{
    file.path(version, location, calibration.code) # handle NULL calibration code case?
}

