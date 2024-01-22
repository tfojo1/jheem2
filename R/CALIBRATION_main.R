# Hello, Andrew


#'@export
set.up.calibration <- function(version,
                               location,
                               calibration.code,
                               root.dir,
                               sub.version = NULL,
                               cache.frequency = 500,
                               allow.overwrite.cache = F)
{
    #------------------------#
    #-- Validate Arguments --#
    #------------------------#
    
    # @Andrew to do
    
    #--------------------------------------#
    #-- Pull the calibration info object --#
    #--------------------------------------#
    
    calibration.info = get.calibration.info(calibration.code,
                                            error.prefix = "Cannot Set Up Calibration: ")
    
    #---------------------------------------#
    #-- Pull Down the Prior and Subset it --#
    #---------------------------------------#
    
    # Pull the prior for the version
    prior = get.parameters.distribution.for.version(version, type='calibrated')
    
    # Subset the relevant parameters
    relevant.prior = distributions::subset.distribution(prior, 
                                                        calibration.info$parameter.names) # from calibration info
    
    # Pull Basic Info We Are Going to Use from the Prior
    
    parameter.scales = get.distribution.variable.transformation(prior, unknown.value='')
    names(parameter.scales) = prior@var.names
    
    prior.sds = suppressWarnings(get.sds(prior))
    prior.medians = suppressWarnings(get.medians(prior))
    prior.means = suppressWarnings(get.means(prior))
    
    #---------------------------------#
    #-- Prepare the Sampling Blocks --#
    #---------------------------------#
    
    # Pull down the sampling blocks
    sampling.blocks = get.parameter.sampling.blocks.for.version(version)
    
    # Include only the relevant parameters
    sampling.blocks = lapply(sampling.blocks, function(block){
        intersect(block, calibration.info$parameter.names)
    })
    
    # for now, we'll do this here
    # I think when we incorporate prior MCMC runs, we may have to keep
    #   the empty blocks and prune them later
    sampling.blocks = sampling.blocks[sapply(sampling.blocks, length)>0]
    
    
    #-------------------------------------------------------------#
    #-- Prepare Default Initial Values, Step Sizes, and Cov Mat --#
    #-------------------------------------------------------------#

    #-- Default parameter values --#
    
    default.parameter.values = prior.medians
    default.parameter.values[names(calibration.info$fixed.initial.parameter.values)] = 
        calibration.info$fixed.initial.parameter.values

    #-- Set up initial.cov.mat --#
    
    DEFAULT.SD.REDUCTION.FACTOR = 20 # The 40 here is arbitrary, but seems to work well
    
    raw.sds = prior.sds
    raw.sds[prior@is.improper] = default.parameter.values[prior@is.improper]
    
    raw.means = prior.means
    raw.means[prior@is.improper] = default.parameter.values[prior@is.improper]
    
    default.parameter.sds = raw.sds
    default.parameter.sds[parameter.scales=='log'] = sqrt(log(raw.sds[parameter.scales=='log']^2 / raw.means[parameter.scales=='log']^2 + 1)) # This comes from the relationship between mean and SD in a lognormal
    default.parameter.sds = default.parameter.sds / DEFAULT.SD.REDUCTION.FACTOR
    
    default.initial.cov.mat = diag(default.parameter.sds^2)
    dimnames(default.initial.cov.mat) = list(names(default.parameter.values), names(default.parameter.values))
    
    # Get the default scaling parameters
    default.initial.scaling.parameters = lapply(sampling.blocks, function(block){
        sapply(block, function(var.in.block){
            2.38^2/length(block)
        })
    })
    
    #--------------------------------------------------------------------------#
    #-- Incorporate prior MCMC runs into Initial Values, Step Sizes, Cov Mat --#
    #--------------------------------------------------------------------------#
    
    # For now, just pull the default whole-hog
    initial.cov.mat = default.initial.cov.mat[calibration.info$parameter.names, calibration.info$parameter.names]
    initial.scaling.parameters = default.initial.scaling.parameters
    
    # For starting values, we need one row for each chain
    relevant.default.values = default.parameter.values[calibration.info$parameter.names]
    starting.parameter.values = t(sapply(1:calibration.info$n.chains, function(chain){
        relevant.default.values
    }))
    dim(starting.parameter.values) = c(calibration.info$n.chains, length(relevant.default.values))
    dimnames(starting.parameter.values) = list(NULL, parameter=calibration.info$parameter.names)
     
    
    #-----------------------------#
    #-- Set up the MCMC Control --#
    #-----------------------------#
    
    #-- Set up the Run Simulation function --#
    engine = create.jheem.engine(version=version, location=location, 
                                 sub.version = sub.version,
                                 start.year = calibration.info$start.year,
                                 end.year = calibration.info$end.year, 
                                 max.run.time.seconds = calibration.info$max.run.time.seconds,
                                 calibration.code = calibration.code)
    engine$crunch(parameters = default.parameter.values)

    # 'params' is the subset of parameters that we will be modifying in the MCMC
    run.simulation <- function(params) {
        all.parameters = default.parameter.values
        all.parameters[names(params)] = params
        engine$run(all.parameters)
    }

    #-- Set up the compute likelihood function --#
    likelihood = calibration.info$likelihood.instructions$instantiate.likelihood(version=version,
                                                                                 location=location, 
                                                                                 sub.version=sub.version,
                                                                                 data.manager=calibration.info$data.manager)
    compute.likelihood <- function(sim) {
        likelihood$compute(sim=sim, log=T, check.consistency=F)
    }
    
    #-- Set MCMC parameters depending on whether this is prelim or not --#
    if (calibration.info$is.preliminary) # get is.preliminary of calibration info object
    {
        target.acceptance.rate=0.1
        
        scaling.base.update = 1
        scaling.update.prior=10
        scaling.update.decay=.5
        
        n.iter.before.cov = 0
        cov.base.update=0.2
        cov.update.prior=500
        cov.update.decay=0.5
    }
    else
    {
        target.acceptance.rate=0.238
        
        scaling.base.update = 1
        scaling.update.prior=100
        scaling.update.decay=.5
        
        n.iter.before.cov = 0
        cov.base.update=1
        cov.update.prior=500
        cov.update.decay=1
    }
    
    #-- Make the call to create the control --#
    
    ctrl = bayesian.simulations::create.adaptive.blockwise.metropolis.control(
        var.names = calibration.info$parameter.names,
        simulation.function = run.simulation,
        log.prior.distribution = distributions::get.density.function(relevant.prior),
        log.likelihood = compute.likelihood, # saves the data manager in here!
        burn = calibration.info$n.burn,
        thin = calibration.info$thin,
        var.blocks = sampling.blocks,
        reset.adaptive.scaling.update.after = 0,
        transformations = parameter.scales[calibration.info$parameter.names],
        
        initial.covariance.mat = initial.cov.mat,
        initial.scaling.parameters = initial.scaling.parameters,
        
        target.acceptance.probability=target.acceptance.rate,
        
        n.iter.before.use.adaptive.covariance = n.iter.before.cov,
        adaptive.covariance.base.update = cov.base.update,
        adaptive.covariance.update.prior.iter = cov.update.prior,
        adaptive.covariance.update.decay = cov.update.decay,
        adaptive.scaling = 'componentwise',
        adaptive.scaling.base.update = scaling.base.update,
        adaptive.scaling.update.prior.iter= scaling.update.prior,
        adaptive.scaling.update.decay = scaling.update.decay
    )
    
    #----------------------#
    #-- Set up the Cache --#
    #----------------------#
    
    calibration.dir = get.calibration.dir(version=version,
                                          location=location,
                                          calibration.code=calibration.code,
                                          root.dir=root.dir)
    
    if (!dir.exists(calibration.dir))
        dir.create(calibration.dir, recursive = T)
    
    bayesian.simulations::create.mcmc.cache(
        dir = calibration.dir,
        control = ctrl,
        n.iter = calibration.info$n.iter, # just get from calibration object info
        starting.values = starting.parameter.values, # harder
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
    # Make sure a cache directory has been set up in place
    bayesian.simulations::run.mcmc.from.cache(dir = get.calibration.dir(version=version,
                                                                        location=location,
                                                                        calibration.code=calibration.code,
                                                                        root.dir=root.dir),
                                              chains = chains,
                                              update.frequency = update.frequency,
                                              update.detail = update.detail,
                                              remove.cache.when.done = F)
                                              
}

#'@export
clear.calibration.cache <- function(version,
                                    location,
                                    calibration.code,
                                    root.dir,
                                    allow.remove.incomplete = F)
{
    bayesian.simulations::remove.mcmc.cache(get.calibration.dir(version=version,
                                                                location=location,
                                                                calibration.code=calibration.code,
                                                                root.dir=root.dir),
                                            allow.remove.incomplete = allow.remove.incomplete)    
}

#'@export
assemble.mcmc.from.calibration <- function(version,
                                           location,
                                           calibration.code,
                                           root.dir,
                                           allow.incomplete=F,
                                           chains = NULL)
{
    mcmc = bayesian.simulations::assemble.mcmc.from.cache(dir = get.calibration.dir(version=version,
                                                                                    location=location,
                                                                                    calibration.code=calibration.code,
                                                                                    root.dir=root.dir),
                                                          allow.incomplete = allow.incomplete,
                                                          chains = chains)
    
    mcmc
}

#'@export
assemble.simulations.from.calibration <- function(version,
                                                  location,
                                                  calibration.code,
                                                  root.dir,
                                                  allow.incomplete=F,
                                                  chains = NULL)
{
    mcmc = assemble.mcmc.from.calibration(version = version,
                                                      location = location,
                                                      calibration.code = calibration.code,
                                                      root.dir = root.dir,
                                                      allow.incomplete = allow.incomplete,
                                                      chains = chains)
    
    # could choose to delete cache now but will not, undecided
    join.simulation.sets(mcmc@simulations[as.integer(mcmc@simulation.indices)])
}

#'@export
register.calibration.info <- function(code,
                                      likelihood.instructions,
                                      is.preliminary,
                                      start.year,
                                      end.year,
                                      parameter.names,
                                      n.iter,
                                      thin,
                                      fixed.initial.parameter.values,
                                      description,
                                      max.run.time.seconds = 10,
                                      n.chains = if (is.preliminary) 1 else 4,
                                      n.burn = if (is.preliminary) 0 else floor(n.iter / 2),
                                      data.manager = get.default.data.manager(),
                                      draw.initial.parameter.values.from = character(),
                                      error.prefix = "Error registering calibration info: ")
{
    # Validate arguments
    
    # code is
    # - a single, non-NA character value
    # - if already present, we need to check for equality
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
    
    # For now, just going to package up and store it so things work
    calibration.info = list(
        code = code,
        likelihood.instructions = likelihood.instructions,
        data.manager = data.manager,
        start.year = start.year,
        end.year = end.year,
        parameter.names = parameter.names,
        n.chains = n.chains,
        n.iter = n.iter,
        n.burn = n.burn,
        thin = thin,
        draw.initial.parameter.values.from = draw.initial.parameter.values.from,
        fixed.initial.parameter.values = fixed.initial.parameter.values,
        is.preliminary = is.preliminary,
        max.run.time.seconds = max.run.time.seconds,
        description = description
    )
    
    CALIBRATION.MANAGER$info[[code]] = calibration.info
    
    invisible(NULL)
}

CALIBRATION.MANAGER = new.env()
CALIBRATION.MANAGER$info = list()

get.calibration.info <- function(code, throw.error.if.missing=T, error.prefix='')
{
    rv = CALIBRATION.MANAGER$info[[code]]
    if (is.null(rv) && throw.error.if.missing)
        stop(paste0(error.prefix, "No calibration info has been registered under code '", code, "'"))
    
    rv
}

##-------------##
##-- HELPERS --##
##-------------##

get.calibration.dir <- function(version, location, calibration.code, root.dir)
{
    file.path(root.dir, 
              MCMC.SUB.DIRECTORY, 
              get.jheem.file.path(version=version, 
                                  location=location,
                                  calibration.code=calibration.code))
}

get.distribution.variable.transformation = function(dist, unknown.value)
{
    if (is(dist, "Joint_Independent_Distributions"))
        unlist(lapply(dist@subdistributions, 
                      get.distribution.variable.transformation, 
                      unknown.value = unknown.value))
    else if (is.null(dist@transformation))
        rep(unknown.value, dist@n.var)
    else
        rep(dist@transformation@name, dist@n.var)
}
