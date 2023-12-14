


#'@export
set.up.calibration <- function(version,
                               location,
                               calibration.code,
                               root.dir,
                               cache.frequency = 500,
                               allow.overwrite.cache)
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
    
    
    
    #-- Get the Prior --#
    prior = get.parameters.distribution.for.version(version, type='calibrated')
    relevant.prior = distributions::subset.distribution(prior, calibration.info$parameter.names) # from calibration info
    
    # Assume we have at this point
    # prior
    #   - an object of class Distribution from the distributions package
    # fixed.initial.parameter.values
    #   - A named numeric vector, set in the calibration info
    # parameter.blocks
    #   - a list of character vectors
    # preliminary
    #   - a single logical value indicating whether we should treat this run as a prelim to tune parameters or as the real-deal
    
    
    #-- Pull Basic Info We Are Going to Use from the Prior --#
    
    parameter.scales = get.distribution.variable.transformation(prior, unknown.value='')
    
    prior.sds = suppressWarnings(get.sds(prior))
    prior.medians = suppressWarnings(get.medians(prior))
    prior.means = suppressWarnings(get.means(prior))
    
    
    #-- Default parameter values --#
    
    default.parameter.values = prior.medians
    default.parameter.values[names(fixed.initial.parameter.values)] = fixed.initial.parameter.values
    
    
    #-- Set up initial.cov.mat --#
    
    SD.REDUCTION.FACTOR = 20 # The 40 here is arbitrary, but seems to work well
    
    raw.sds = prior.sds
    raw.sds[prior@is.improper] = default.parameter.values
    
    raw.means = prior.means
    raw.means[prior@is.improper] = default.parameter.values
    
    default.parameter.sds = raw.sds
    default.parameter.sds[parameter.scales=='log'] = sqrt(log(raw.sds^2 / raw.means^2 + 1)) # This comes from the relationship between mean and SD in a lognormal
    default.parameter.sds = default.parameter.sds / SD.REDUCTION.FACTOR
    
    default.initial.cov.mat = diag(default.parameter.sds^2)
    
    
    #-- Set up initial.scaling.parameters --#
    
    # Get the default scaling parameters
    default.initial.scaling.parameters = lapply(parameter.blocks, function(block){
        sapply(block, function(var.in.block){
            2.38^2/length(block)
        })
    })
    
    # Overwrite with info from prior MCMC runs
    #   (to be done later)
    
    # for now
    initial.cov.mat = default.initial.cov.mat
    initial.scaling.parameters = default.initial.scaling.parameters
    
    
    #-- Set MCMC parameters depending on whether this is prelim or not --#
    if (preliminary) # get is.preliminary of calibration info object
    {
        target.acceptance.rate=0.1
        
        SCALING.BASE.UPDATE = 1
        SCALING.UPDATE.PRIOR=10
        SCALING.UPDATE.DECAY=.5
        
        COV.BASE.UPDATE=0.2
        COV.UPDATE.PRIOR=500
        COV.UPDATE.DECAY=0.5
    }
    else
    {
        target.acceptance.rate=0.238
        
        SCALING.BASE.UPDATE = 1
        SCALING.UPDATE.PRIOR=100
        SCALING.UPDATE.DECAY=.5
        
        N.ITER.BEFORE.COV = 0
        COV.BASE.UPDATE=1
        COV.UPDATE.PRIOR=500
        COV.UPDATE.DECAY=1
    }
    
    # NEED TO FILL IN STILL
    'make the engine'
    engine = create.jheem.engine(version=version, location=location, start.year=calibration.info$start.year, end.year=calibration.info$start.year, max.run.time.seconds = calibration.info$max.run.time.seconds) # get some of these from the calibration info object
    # 'parameters' is the subset of parameters that we will be modifying in the MCMC
    run.simulation <- function(parameters) {
        all.parameters = default.parameter.values
        all.parameters[names(parameters)] = parameters
        engine$run(all.parameters)
    }
    likelihood = calibration.info$likelihood.instructions$instantiate(version=version, location=location, data.manager=calibration.info$data.manager)
    compute.likelihood <- function(sim) {
        likelihood$compute(sim=sim, log=T, check.consistency=F)
    }
    
    burn = "from the calibration info"
    thin = "from the calibration infoo"
    
    
    #-- Set up the MCMC Control --#
    ctrl = bayesian.simulations::create.adaptive.blockwise.metropolis.control(var.names=calibration.info$parameter.names,
                                                                              simulation.function=run.simulation,
                                                                              log.prior.distribution = distributions::get.density.function(relevant.prior),
                                                                              log.likelihood = compute.likelihood, # saves the data manager in here!
                                                                              burn=burn, thin=thin,
                                                                              var.blocks = parameter.blocks,
                                                                              reset.adaptive.scaling.update.after = 0,
                                                                              transformations = parameter.scales,
                                                                              
                                                                              initial.covariance.mat = initial.cov.mat,
                                                                              initial.scaling.parameters = initial.scaling.parameters,
                                                                              
                                                                              target.acceptance.probability=target.acceptance.rate,
                                                                              
                                                                              n.iter.before.use.adaptive.covariance = N.ITER.BEFORE.COV,
                                                                              adaptive.covariance.base.update = COV.BASE.UPDATE,
                                                                              adaptive.covariance.update.prior.iter = COV.UPDATE.PRIOR,
                                                                              adaptive.covariance.update.decay = COV.UPDATE.DECAY,
                                                                              adaptive.scaling = 'componentwise',
                                                                              adaptive.scaling.base.update = SCALING.BASE.UPDATE,
                                                                              adaptive.scaling.update.prior.iter= SCALING.UPDATE.PRIOR,
                                                                              adaptive.scaling.update.decay= SCALING.UPDATE.DECAY
    )
    
    #-- Set up the cache --#
    bayesian.simulations::create.mcmc.cache(dir = file.path(root.dir, MCMC.SUB.DIRECTORY, get.jheem.file.path(version, location, calibration.code)),
                                            control = ctrl,
                                            n.iter = 'calibration_objects_n.iter', # just get from calibration object info
                                            starting.values = 'starting.values', # harder
                                            prior.mcmc = NULL,
                                            cache.frequency = cache.frequency,
                                            allow.overwrite.cache = allow.overwrite.cache)
}

#'@export
run.calibration <- function(version,
                            location,
                            calibration.code,
                            root.dir,
                            chains,
                            update.frequency = 500,
                            update.detail = 'low')
{
    # Pull the calibration info object
    bayesian.simulations::run.mcmc.from.cache(dir = file.path(root.dir, MCMC.SUB.DIRECTORY, get.jheem.file.path(version, location, calibration.code)),
                                              chains = chains,
                                              update.frequency = update.frequency,
                                              update.detail = update.detail,
                                              remove.cache.when.done = F)
                                              
}

#'@export
assemble.from.calibration <- function(version,
                                      location,
                                      calibration.code,
                                      root.dir,
                                      allow.incomplete=F,
                                      chains = NULL)
{
    mcmc = bayesian.simulations::assemble.mcmc.from.cache(dir = file.path(root.dir, MCMC.SUB.DIRECTORY, get.jheem.file.path(version, location, calibration.code)),
                                                          allow.incomplete = allow.incomplete,
                                                          chains = chains)
    # could choose to delete cache now but will not, undecided
    join.simulation.sets(mcmc@simulations[as.integer(mcmc@simulation.indices)])
}

#'@export
register.calibration.info <- function(code,
                                      likelihood.instructions,
                                      data.manager,
                                      start.year,
                                      end.year,
                                      parameter.names,
                                      n.chains,
                                      n.iter,
                                      n.burn,
                                      draw.initial.parameter.values.from,
                                      fixed.initial.parameter.values,
                                      is.preliminary,
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