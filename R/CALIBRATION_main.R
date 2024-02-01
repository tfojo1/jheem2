# Hello, Andrew


#'@export
set.up.calibration <- function(version,
                               location,
                               calibration.code,
                               root.dir,
                               sub.version = NULL,
                               cache.frequency = 500,
                               allow.overwrite.cache = F,
                               verbose = T)
{
    #------------------------#
    #-- Validate Arguments --#
    #------------------------#
    
    # @Andrew to do
    
    error.prefix = paste0("Cannot Set Up Calibration for code '", calibration.code, "' for version '", version, "' and location '", location, "': ")
    verbose.prefix = paste0("CALIBRATION '", calibration.code, "' (", version, " / ", location, "): ")
    
    #--------------------------------------#
    #-- Pull the calibration info object --#
    #--------------------------------------#
    
    calibration.info = get.calibration.info(calibration.code,
                                            error.prefix = error.prefix)
    
    #---------------------------------------#
    #-- Pull Down the Prior and Subset it --#
    #---------------------------------------#
    
    if (verbose)
        print(paste0(verbose.prefix, "Pulling prior, setting up defaults..."))
    
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
    
    if (verbose)
        print(paste0(verbose.prefix, "Incorporating prior MCMC runs..."))
    
    initial.cov.mat = default.initial.cov.mat[calibration.info$parameter.names, calibration.info$parameter.names]
    initial.scaling.parameters = default.initial.scaling.parameters
    
    parameters.already.pulled.from.preceding = character()
    dynamic.preceding.codes = calibration.info$preceding.calibration.codes #we'll potentially add to this list in the while block below
    preceding.index = 1
    while (preceding.index <= length(dynamic.preceding.codes))
    {
        preceding.code = dynamic.preceding.codes[preceding.index]
        preceding.index = preceding.index + 1
        
        preceding.info = get.calibration.info(code = preceding.code, throw.error.if.missing = F)
        if (is.null(preceding.info))
            stop(paste0(error.prefix, "No calibration has been registered for the preceding.calibration.code of '", preceding.code, "'"))
        
        dynamic.preceding.codes = union(dynamic.preceding.codes, preceding.info$preceding.calibration.codes)
        
        parameters.to.pull.from.preceding = intersect(calibration.info$parameter.names,
                                                      preceding.info$parameter.names)
        parameters.to.pull.from.preceding = setdiff(parameters.to.pull.from.preceding,
                                                    parameters.already.pulled.from.preceding)
        
        
        if (length(parameters.to.pull.from.preceding)>0)
        {
            other.parameters = setdiff(calibration.info$parameter.names, parameters.to.pull.from.preceding)
            
            parameters.already.pulled.from.preceding = union(parameters.already.pulled.from.preceding,
                                                             parameters.to.pull.from.preceding)
            
            
            mcmc.summary = prepare.mcmc.summary(version = version,
                                                location = location,
                                                calibration.code = preceding.code,
                                                root.dir = root.dir,
                                                get.one.set.of.parameters = calibration.info$is.preliminary,
                                                error.prefix = error.prefix)
            
            # default parameter values
            transformed.parameter.values.to.pull = mcmc.summary$transformed.parameter.values[parameters.already.pulled.from.preceding]
            default.parameter.values[parameters.to.pull.from.preceding] = sapply(parameters.to.pull.from.preceding, function(param){
                if (parameter.scales[param] == 'log')
                    exp(transformed.parameter.values.to.pull[param])
                else if (parameter.scales[param] == 'logit')
                    1 / (1 + exp(-transformed.parameter.values.to.pull[param]))
                else
                    transformed.parameter.values.to.pull[param] 
            })
            
            # cov mat
            initial.cov.mat[parameters.to.pull.from.preceding,parameters.to.pull.from.preceding] = initial.cov.mat[parameters.to.pull.from.preceding,parameters.to.pull.from.preceding] * (1-calibration.info$weight.to.preceding.variance.estimates) +
                mcmc.summary$cov.mat[parameters.to.pull.from.preceding,parameters.to.pull.from.preceding] * calibration.info$weight.to.preceding.variance.estimates
            initial.cov.mat[parameters.to.pull.from.preceding,other.parameters] = initial.cov.mat[parameters.to.pull.from.preceding,other.parameters] = 0
            initial.cov.mat[other.parameters, parameters.to.pull.from.preceding] = initial.cov.mat[other.parameters, parameters.to.pull.from.preceding] = 0
            
            # scaling parameters
            initial.scaling.parameters = lapply(names(initial.scaling.parameters), function(scaling.name){
                scaling = initial.scaling.parameters[[scaling.name]]
                to.pull.scaling = mcmc.summary$initial.scaling.parameters[[scaling.name]]
                if (!is.null(to.pull.scaling))
                {
                    to.pull.params = intersect(parameters.to.pull.from.preceding, names(to.pull.scaling))
                    scaling[to.pull.params] = to.pull.scaling[to.pull.params]
                }
                
                scaling
            })
        }
    }
    
    # For starting values, we need one row for each chain
    relevant.default.values = default.parameter.values[calibration.info$parameter.names]
    starting.parameter.values = t(sapply(1:calibration.info$n.chains, function(chain){
        relevant.default.values
    }))
    dim(starting.parameter.values) = c(calibration.info$n.chains, length(relevant.default.values))
    dimnames(starting.parameter.values) = list(NULL, parameter=calibration.info$parameter.names)
     
    
    #-----------------#
    #-- Some Checks --#
    #-----------------#
    
    if (any(is.na(starting.parameter.values)))
    {
        na.parameters = calibration.info$parameter.names[apply(is.na(starting.parameter.values),2,any)]
        stop(paste0(error.prefix,
                    "NA starting values are present for ",
                    ifelse(length(na.parameters)==1, "parameter ", "parameters "),
                    collapse.with.and("'", na.parameters, "'")))
    }
    
    #-----------------------------#
    #-- Set up the MCMC Control --#
    #-----------------------------#
    
    
    if (verbose)
        print(paste0(verbose.prefix, "Seting up the engine..."))
    
    #-- Set up the Run Simulation function --#
    engine = do.create.jheem.engine(version=version, location=location, 
                                    sub.version = sub.version,
                                    start.year = NULL, #will set start.year to the version's start.year
                                    end.year = calibration.info$end.year, 
                                    max.run.time.seconds = calibration.info$max.run.time.seconds,
                                    calibration.code = calibration.code,
                                    finalize = F)
    engine$crunch(parameters = default.parameter.values)

    # 'params' is the subset of parameters that we will be modifying in the MCMC
    run.simulation <- function(params) {
        all.parameters = default.parameter.values
        all.parameters[names(params)] = params
        engine$run(all.parameters)
    }
    
    #-- Set up the compute likelihood function --#
    if (verbose)
        print(paste0(verbose.prefix, "Instantiate the likelihood..."))

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
    
    if (verbose)
        print("Set up the MCMC control...")
    
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
    
    
    if (verbose)
        print(paste0(verbose.prefix, "All Done!"))
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
    join.simulation.sets(mcmc@simulations[as.integer(mcmc@simulation.indices)], finalize = T)
}

#'@export
register.calibration.info <- function(code,
                                      likelihood.instructions,
                                      is.preliminary,
                                      end.year,
                                      parameter.names,
                                      n.iter,
                                      thin,
                                      description,
                                      fixed.initial.parameter.values = numeric(),
                                      max.run.time.seconds = 10,
                                      n.chains = if (is.preliminary) 1 else 4,
                                      n.burn = if (is.preliminary) 0 else floor(n.iter / 2),
                                      data.manager = get.default.data.manager(),
                                      preceding.calibration.codes = character(),
                                      weight.to.preceding.variance.estimates = 0.5,
                                      pull.parameters.and.values.from.preceding = T,
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
    
    if (length(preceding.calibration.codes)>0)
    {
        for (preceding.code in preceding.calibration.codes)
        {
            preceding.info = get.calibration.info(code = preceding.code, throw.error.if.missing = F)
            if (is.null(preceding.info))
                stop(paste0(error.prefix, "No calibration has been registered for the preceding.calibration.code of '", preceding.code, "'"))
            
            if (pull.parameters.and.values.from.preceding)
            {
                parameter.names = union(parameter.names, preceding.info$parameter.names)
                fixed.initial.parameter.values = c(
                    fixed.initial.parameter.values,
                    preceding.info$fixed.initial.parameter.values[setdiff(names(preceding.info$fixed.initial.parameter.values),
                                                                          names(fixed.initial.parameter.values))]
                )
            }
        }
    }
    
    # For now, just going to package up and store it so things work
    calibration.info = list(
        code = code,
        likelihood.instructions = likelihood.instructions,
        data.manager = data.manager,
        end.year = end.year,
        parameter.names = parameter.names,
        n.chains = n.chains,
        n.iter = n.iter,
        n.burn = n.burn,
        thin = thin,
        preceding.calibration.codes = preceding.calibration.codes,
        fixed.initial.parameter.values = fixed.initial.parameter.values,
        pull.parameters.and.values.from.preceding = pull.parameters.and.values.from.preceding,
        weight.to.preceding.variance.estimates = weight.to.preceding.variance.estimates,
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


prepare.mcmc.summary <- function(version,
                                  location,
                                  calibration.code,
                                  root.dir,
                                 get.one.set.of.parameters = T,
                                  error.prefix = '')
{
    dir = file.path(get.calibration.dir(version=version,
                                        location=location,
                                        calibration.code=calibration.code,
                                        root.dir=root.dir),
                    'cache')
 
    error.prefix = paste0(error.prefix, 
                          "Cannot prepare a summary of the mcmc for calibration '", calibration.code, " for ' and version '", version, "' and location '", location, "' - ")   
    if (!file.exists(dir))
        stop(paste0(error.prefix,
                    "No prior MCMC cache has been set up in the root directory (", root.dir, "')"))
    
    files = list.files(dir)
    chain.control.files = files[grepl('chain[0-9]+_control', files)]
    
    if (length(chain.control.files)==0)
        stop(paste0(error.prefix,
                    "The prior MCMC cache is malformed"))
    
    chain.controls = lapply(file.path(dir, chain.control.files), function(file){
         x = load(file)
         get(x)
    })
    
    chain.done = sapply(chain.controls, function(ctrl){
        all(ctrl@chunk.done)
    })
    
    if (any(!chain.done))
        stop(paste0(error.prefix,
                    "The prior MCMC has not finished running"))
    
    state1 = chain.controls[[1]]@chain.state
    
    #-- Prepare the covariance matrix --#
    all.cov.mats = sapply(chain.controls, function(ctrl){
        ctrl@chain.state@cov.mat
    })
    
    cov.mat = rowMeans(all.cov.mats)
    dim(cov.mat) = dim(state1@cov.mat)
    dimnames(cov.mat) = dimnames(state1@cov.mat)
    
    
    #-- Prepare the parameter values --#
    
    if (!get.one.set.of.parameters)
        stop("We have not yet implemented getting multiple parameters")
    
    all.transformed.parameter.values = sapply(chain.controls, function(ctrl){
        ctrl@chain.state@mean.transformed.parameters
    })
    
    transformed.parameter.values = rowMeans(all.transformed.parameter.values)
    
    #-- Initial Scaling Steps --#
    
    initial.scaling.parameters = sapply(1:length(state1@log.scaling.parameters), function(i){
        all.chain.log.values = sapply(chain.controls, function(ctrl){
            ctrl@chain.state@log.scaling.parameters[[i]]
        })
        
        exp(rowMeans(all.chain.log.values))
    })
    names(initial.scaling.parameters) = names(state1@log.scaling.parameters)
    
    
    #-- Return --#
    list(
        transformed.parameter.values = transformed.parameter.values,
        cov.mat = cov.mat,
        initial.scaling.parameters = initial.scaling.parameters
    )
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
