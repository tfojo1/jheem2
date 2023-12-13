
#'@export
set.up.calibration <- function(version,
                               location,
                               calibration.code,
                               dir,
                               cache.frequency = 500)
{
    # Pull the calibration info object
    
    # Pull the parameter prior and sampling blocks from version manager
    
    # Get start values:
    # - Pull medians from the parameters prior for the relevant parameters (suppress warnings here, because improper priors will generate a warning)
    # - Overwrite with fixed.initial.parameter.values
    # - Load up MCMC runs from draw.initial.parameter.values.from and overwrite with a sampling of their values
    # - Check - if any NAs left, throw an error
    
    # Set up the parameter blocks
    # - Keep any blocks that have at least one parameter named in calibration info
    # - From each block, remove all paramters that are not named in calibration info
    
    # Set up the adaptive metropolis control
    # bayesian.simulations::create.adaptive.blockwise.metropolis.control()
    
    # arguments are:
    # var.names=prior@var.names,
    # simulation.function=run.simulation,
    # log.prior.distribution = get.density.function(prior),
    # log.likelihood = likelihood,
    # burn=burn, thin=thin,
    # var.blocks = parameter.var.blocks,
    # reset.adaptive.scaling.update.after = 0,
    # transformations = transformations,
    # 
    # initial.covariance.mat = initial.cov.mat, -- set initial.cov.mat = diag((get.sds(prior)/20)^2)
    # initial.scaling.parameters = initial.scaling.parameters, == set = starting.values / 20
    # 
    # target.acceptance.probability=target.acceptance.rate,
    
    
    # Set up the cache
    # bayesian.simulations::create.mcmc.cache()
}

#'@export
run.calibration <- function(version,
                            location,
                            calibration.code,
                            dir)
{
    # Pull the calibration info object
}

#'@export
register.calibration.info <- function(code,
                                      likelihood.instructions,
                                      parameter.names,
                                      n.chains,
                                      n.iter,
                                      n.burn,
                                      draw.initial.parameter.values.from,
                                      fixed.initial.parameter.values,
                                      description,
                                      error.prefix = "Error registering likelihood instructions: ")
{
    # Validate arguments
    
    # code is
    # - a single, non-NA character value
    # - We should not let calibration code overlap with any intervention codes
    
    # likelihood.instructions is a likelihood.instructions object
    
    # parameter names is a non-empty character vector with no NA values
    
    # n.chains is an integer >= 1
    
    # draw.initial.parameter.values.from is a character vector, may be empty
    # - no NA values
    # - are all previously registered calibration codes
    
    # fixed.initial.parameter.values is a named numeric vector
    # - may be empty
    # - no NA values
    
    # description is a single, non-NA character value
}

get.simset.filename <- function(version,
                                location,
                                intervention.code,
                                calibration.code)
{
    
}