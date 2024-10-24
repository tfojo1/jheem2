
#'@param version The version of the model specification (must have been previously registered)
#'@param location A single character value representing the location for the calibration
#'@param calibration.code
#'@param root.dir
#'@param sub.version
#'@param cache.frequency
#'@param allow.overwrite.cache
#'@param verbose
#'
#'@export
set.up.calibration <- function(version,
                               location,
                               calibration.code,
                               root.dir = get.jheem.root.directory("Cannot set up calibration: "),
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
        
        parameters.to.pull.from.preceding = intersect(prior@var.names,
                                                      preceding.info$parameter.names)
        parameters.to.pull.from.preceding = setdiff(parameters.to.pull.from.preceding,
                                                    parameters.already.pulled.from.preceding)
        
        parameters.to.pull.cov.from.preceding = intersect(parameters.to.pull.from.preceding,
                                                          calibration.info$parameter.names)
        
        
        if (length(parameters.to.pull.from.preceding)>0)
        {
            mcmc.summary = prepare.mcmc.summary(version = version,
                                                location = location,
                                                calibration.code = preceding.code,
                                                root.dir = root.dir,
                                                get.one.set.of.parameters = calibration.info$is.preliminary,
                                                error.prefix = error.prefix)
            
            parameters.to.pull.from.preceding = intersect(parameters.to.pull.from.preceding, mcmc.summary$parameter.names)
            parameters.to.pull.cov.from.preceding = intersect(parameters.to.pull.cov.from.preceding, mcmc.summary$parameter.names)
            
            other.parameters.for.cov = setdiff(calibration.info$parameter.names, parameters.to.pull.from.preceding)
            
            parameters.already.pulled.from.preceding = union(parameters.already.pulled.from.preceding,
                                                             parameters.to.pull.from.preceding)
            
            # default parameter values
            transformed.parameter.values.to.pull = mcmc.summary$transformed.parameter.values[parameters.to.pull.from.preceding]
            default.parameter.values[parameters.to.pull.from.preceding] = sapply(parameters.to.pull.from.preceding, function(param){
                if (parameter.scales[param] == 'log')
                    exp(transformed.parameter.values.to.pull[param])
                else if (parameter.scales[param] == 'logit')
                    1 / (1 + exp(-transformed.parameter.values.to.pull[param]))
                else
                    transformed.parameter.values.to.pull[param] 
            })
            
            # cov mat
            parameters.with.na.initial.cov.mat.mask = is.na(diag(initial.cov.mat)[parameters.to.pull.cov.from.preceding])
            parameters.to.pull.all.cov.from.preceding = parameters.to.pull.cov.from.preceding[parameters.with.na.initial.cov.mat.mask]
            parameters.to.pull.weighted.cov.from.preceding = parameters.to.pull.cov.from.preceding[!parameters.with.na.initial.cov.mat.mask]
            
            initial.cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding] = initial.cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding] * (1-calibration.info$weight.to.preceding.variance.estimates) +
                mcmc.summary$cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding] * calibration.info$weight.to.preceding.variance.estimates
            initial.cov.mat[parameters.to.pull.all.cov.from.preceding,parameters.to.pull.all.cov.from.preceding] = 
                mcmc.summary$cov.mat[parameters.to.pull.all.cov.from.preceding,parameters.to.pull.all.cov.from.preceding]
            initial.cov.mat[parameters.to.pull.all.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding] = 
                mcmc.summary$cov.mat[parameters.to.pull.all.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding]
            initial.cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.all.cov.from.preceding] = 
                mcmc.summary$cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.all.cov.from.preceding]
            
            initial.cov.mat[parameters.to.pull.cov.from.preceding,other.parameters.for.cov] = initial.cov.mat[parameters.to.pull.cov.from.preceding,other.parameters.for.cov] = 0
            initial.cov.mat[other.parameters.for.cov, parameters.to.pull.cov.from.preceding] = initial.cov.mat[other.parameters.for.cov, parameters.to.pull.cov.from.preceding] = 0
            
            initial.scaling.parameters.original=initial.scaling.parameters
          
            # scaling parameters
            initial.scaling.parameters.names = names(initial.scaling.parameters)
            initial.scaling.parameters = lapply(names(initial.scaling.parameters), function(scaling.name){
                scaling = initial.scaling.parameters[[scaling.name]]
                to.pull.scaling = mcmc.summary$initial.scaling.parameters[[scaling.name]]
                if (!is.null(to.pull.scaling))
                {
                    to.pull.params = intersect(parameters.to.pull.cov.from.preceding, names(to.pull.scaling))
                    scaling[to.pull.params] = to.pull.scaling[to.pull.params]
                }
                
                scaling
            })
            names(initial.scaling.parameters) = initial.scaling.parameters.names
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

    
    #-- Set up the compute likelihood function --#
    if (verbose)
        print(paste0(verbose.prefix, "Instantiate the likelihood..."))

   likelihood = calibration.info$likelihood.instructions$instantiate.likelihood(version=version,
                                                                               location=location,
                                                                              sub.version=sub.version,
                                                                             data.manager=calibration.info$data.manager)

    compute.likelihood <- function(sim) {
        likelihood$compute(sim=sim, log=T, use.optimized.get=T, check.consistency=F)
    }
    

    #-- Set up the Run Simulation function --#
    if (verbose)
        print(paste0(verbose.prefix, "Setting up the engine..."))
    
    jheem.kernel = create.jheem.kernel(version=version, location=location)

    engine = do.create.jheem.engine(jheem.kernel = jheem.kernel,
                                    sub.version = sub.version,
                                    start.year = NULL, #will set start.year to the version's start.year
                                    end.year = calibration.info$end.year, 
                                    max.run.time.seconds = calibration.info$max.run.time.seconds,
                                    calibration.code = calibration.code,
                                    solver.metadata = calibration.info$solver.metadata,
                                    outcome.location.mapping = likelihood$get.outcome.location.mapping(),
                                    finalize = F)
    engine$crunch(parameters = default.parameter.values)

    # 'params' is the subset of parameters that we will be modifying in the MCMC
    run.simulation <- function(params) {
        all.parameters = default.parameter.values
        all.parameters[names(params)] = params
        engine$run(all.parameters)
    }
    
    #-- Check the initial sims to make sure the likelihood computes (not to -Inf) --#
    if (verbose)
        print(paste0(verbose.prefix, "Making sure the likelihood computes on initial simulation(s)..."))
    for (i in 1:dim(starting.parameter.values)[1])
    {
        sim = run.simulation(starting.parameter.values[1,])
        if (likelihood$compute(sim, use.optimized.get=T)==-Inf)
        {
            lik.pieces = likelihood$compute.piecewise(sim, use.optimized.get=T)
            errored.likelihood <<- likelihood
            errored.sim <<- sim
            
            stop(paste0("The likelihood evaluates to -Inf on the ", 
                        get.ordinal(i), " set of initial parameter values. The likelihood components are:\n",
                        paste0(paste0(" - ", names(lik.pieces), " = ", lik.pieces), collapse='\n'),
                        "\nThe simulation and likelihood have been saved in the global environment as 'errored.sim' and 'errored.likelihood'"))
        }
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
        print(paste0(verbose.prefix, "Set up the MCMC control..."))

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

#'@inheritParams set.up.calibration
#'@param chains
#'@param update.frequency
#'@param update.detail
#'
#'@export
run.calibration <- function(version,
                            location,
                            calibration.code,
                            root.dir = get.jheem.root.directory("Cannot set up calibration: "),
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

#'@inheritParams set.up.calibration
#'@param allow.remove.incomplete
#'
#'@export
clear.calibration.cache <- function(version,
                                    location,
                                    calibration.code,
                                    root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                                    allow.remove.incomplete = F)
{
    bayesian.simulations::remove.mcmc.cache(get.calibration.dir(version=version,
                                                                location=location,
                                                                calibration.code=calibration.code,
                                                                root.dir=root.dir),
                                            allow.remove.incomplete = allow.remove.incomplete)    
}

#'@inheritParams set.up.calibration
#'@param allow.remove.incomplete
#'@param chains
#'
#'@export
assemble.mcmc.from.calibration <- function(version,
                                           location,
                                           calibration.code,
                                           root.dir = get.jheem.root.directory("Cannot set up calibration: "),
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

#'@inheritParams assemble.mcmc.from.calibration
#'@param include.first.sim
#'
#'@export
extract.last.simulation.from.calibration <- function(version,
                                                     location,
                                                     calibration.code,
                                                     root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                                                     allow.incomplete=F,
                                                     include.first.sim=F,
                                                     chains = NULL)
{
    dir = get.calibration.dir(version=version,
                              location=location,
                              calibration.code=calibration.code,
                              root.dir=root.dir)
    
    cache.dir = file.path(dir, 'cache')
    chain.dirs = list.dirs(cache.dir, recursive = F, full.names = F)
    n.chains = length(chain.dirs)
    
    max.chunk = -1
    chain.with.max.chunk = -1
    for (chain in 1:n.chains)
    {
        chain.dir = file.path(cache.dir, paste0('chain_', chain))
        chunk.files = list.files(chain.dir)
        chunk = as.numeric(substr(chunk.files, start=nchar(paste0("chain", chain, "_chunk"))+1, nchar(chunk.files)-6))
        max.chunk.for.chain = max(chunk)
        if (max.chunk.for.chain > max.chunk)
        {
            max.chunk = max.chunk.for.chain
            chain.with.max.chunk = chain
        }
    }
    
    if (max.chunk<0)
        stop("Cannot extract.last.simulation.from.calibration - no simulations have been saved")
    
    chain.control = get(load(file.path(cache.dir, paste0('chain', chain.with.max.chunk, "_control.Rdata"))))

    
    if (!all(chain.control@chunk.done))
    {
        if (!allow.incomplete)
            stop("Cannot extract.last.simulation.from.calibration - no chains have finished running. Use allow.incomplete = T to get the last simulation that has been finished")
        
        print(paste0("Sampling incomplete: Using the ",
                     get.ordinal(max.chunk),
                     " chunk (out of ",
                     length(chain.control@chunk.done), ")",
                     ifelse(n.chains==1, '',
                            paste0(" from the ", get.ordinal(chain.with.max.chunk), " chain"))))
    }
    
    mcmc.last = get(load(file.path(cache.dir, 
                                   paste0('chain_', chain.with.max.chunk), 
                                   paste0('chain', chain.with.max.chunk, "_chunk", max.chunk, ".Rdata"))))
    
    sim.last = mcmc.last@simulations[[length(mcmc.last@simulations)]]
    
    if (include.first.sim)
    {
        mcmc.first = mcmc.last = get(load(file.path(cache.dir, 
                                                    paste0('chain_', chain.with.max.chunk), 
                                                    paste0('chain', chain.with.max.chunk, "_chunk1.Rdata"))))
        sim.first = mcmc.first@simulations[[1]]
        
        sim = join.simulation.sets(sim.first, sim.last)
    }
    else
        sim = join.simulation.sets(sim.last) #this finalizes things
    
    sim
}

#'@inheritParams assemble.mcmc.from.calibration
#'
#'@export
assemble.simulations.from.calibration <- function(version,
                                                  location,
                                                  calibration.code,
                                                  root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                                                  allow.incomplete=F,
                                                  chains = NULL)
{
    mcmc = assemble.mcmc.from.calibration(version = version,
                                                      location = location,
                                                      calibration.code = calibration.code,
                                                      root.dir = root.dir,
                                                      allow.incomplete = allow.incomplete,
                                                      chains = chains)
    
    
    sim.indices = as.integer(mcmc@simulation.indices)
    if (any(is.na(sim.indices))) # This is a patch to the error in the MCMC package code with propagating NA values
    {
        print("Error in assembling simset - NAs introduced by MCMC package - we will try to recover")
        search.from = 1
        sim.indices = sapply(1:length(mcmc@simulation.indices), function(i){
            
            params = mcmc@samples[1,i,]
            if (any(is.na(params)))
                return(search.from)
            
            for (j in search.from:length(mcmc@simulations))
            {
                if (all(mcmc@simulations[[j]]$params == params))
                {
                    search.from <<- j
                    return (j)
                }
            }
            
            NA
        })
        
        if (any(is.na(sim.indices)))
            stop("Cannot assemble simulations from calibration - NAs were introduced in the MCMC package code and we cannot recover from this (despite trying)")
    }
    
    run.times = as.numeric(mcmc@run.times)#[sim.indices]
    last.valid = run.times[1]
    run.times = sapply(run.times, function(time){
        if (!is.na(time))
            last.valid = time
        last.valid
    })

    run.metadatas = lapply(1:length(sim.indices), function(i){
        r.meta = mcmc@simulations[[ sim.indices[i] ]]$run.metadata
        copy.run.metadata(r.meta, run.time = run.times[i] / mcmc@thin)
    })
    simulations = mcmc@simulations[sim.indices]
    
    # could choose to delete cache now but will not, undecided
    join.simulation.sets(simulations, finalize = T, run.metadata = join.run.metadata(run.metadatas))
}

#'@param code
#'@param likelihood.instructions
#'@param is.preliminary
#'@param end.year
#'@param parameter.names
#'@param parameter.aliases A named list of character vectors. Each character vector must be one or more parameter names from the parameter distribution, and the names represent the aliases to be used in sampling
#'@param n.iter
#'@param thin
#'@param description
#'@param fixed.initial.parameter.values
#'@param max.run.time.seconds
#'@param solver.metadata
#'@param n.chains
#'@param n.burn
#'@param data.manager
#'@param preceding.calibration.codes
#'@param weight.to.preceding.variance.estimates
#'@param pull.parameters.and.values.from.preceding
#'@param error.prefix
#'
#'@export
register.calibration.info <- function(code,
                                      likelihood.instructions,
                                      is.preliminary,
                                      end.year,
                                      parameter.names,
                                      parameter.aliases = NULL,
                                      n.iter,
                                      thin,
                                      description,
                                      fixed.initial.parameter.values = numeric(),
                                      max.run.time.seconds = 10,
                                      solver.metadata = create.solver.metadata(),
                                      n.chains = if (is.preliminary) 1 else 4,
                                      n.burn = if (is.preliminary) 0 else floor(n.iter / 2),
                                      data.manager = get.default.data.manager(),
                                      preceding.calibration.codes = character(),
                                      weight.to.preceding.variance.estimates = 0.5,
                                      pull.parameters.and.values.from.preceding = F,
                                      error.prefix = "Error registering calibration info: ")
{
    #-- Validate arguments --#
    
    # error.prefix
    if (!is.character(error.prefix) || length(error.prefix)==1 || is.na(error.prefix))
        stop("Cannot register.calibration.info(): 'error.prefix' must be a single, non-NA character value")
    
    # code
    validate.calibration.code(code = code,
                              error.prefix = error.prefix,
                              code.name.for.error = 'code')
    # - We should not let calibration code overlap with any intervention codes?
    
    # likelihood.instructions
    if (!is(likelihood.instructions, 'jheem.likelihood.instructions'))
        stop(paste0(error.prefix, "'likelihood.instructions' must be an object of class 'jheem.likelihood.instructions'"))
    
    # end.year
    if (!is.numeric(end.year) || length(end.year)!=1 || is.na(end.year))
        stop(paste0(error.prefix, "'end.year' must be a single, non-NA numeric value"))
    
    current.year = as.numeric(format(Sys.Date(), "%Y"))
    if (end.year < current.year)
        stop(paste0(error.prefix, "'end.year' must be no earlier than the current year (", current.year, ")"))
    
    # parameter.aliases
    if (is.null(parameter.aliases))
    {}
    else
    {
        if (!is.list(parameter.aliases))
            stop(paste0(error.prefix, "'parameter.aliases' must be a named LIST of character vectors"))
        
        if (any(!sapply(parameter.aliases, is.character)))
            stop(paste0(error.prefix, "'parameter.aliases' must be a named list of CHARACTER VECTORS"))
        
        if (is.null(names(parameter.aliases)))
            stop(paste0(error.prefix, "'parameter.aliases' must be a NAMED list of character vectors"))
        
        if (any(sapply(parameter.aliases, function(values){
            length(values)==0 || any(is.na(values))
        })))
            stop(paste0(error.prefix, "The elements of 'parameter.aliases' must be non-empty and contain no NA values"))
    }
    
    # n.chains is an integer >= 1
    if (!is.numeric(n.chains) || length(n.chains)!=1 || is.na(n.chains) || round(n.chains)!=n.chains || n.chains<1)
        stop(paste0(error.prefix, "'n.chains' must be a single, non-NA integer value greater than or equal to 1"))
    
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
        solver.metadata = solver.metadata,
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

#'@title Copy Calibration Info to a New Code
#'
#'@param from.code The code for the the calibration info to copy
#'@param to.code The calibration code to be used for the copied calibration info
#'
#'@export
copy.calibration.info <- function(from.code,
                                  to.code)
{
    validate.calibration.code(code = from.code,
                              error.prefix = "Cannot copy calibration info: ",
                              code.name.for.error = 'from.code')
    validate.calibration.code(code = to.code,
                              error.prefix = "Cannot copy calibration info: ",
                              code.name.for.error = 'to.code')
    
    calibration.info = get.calibration.info(code = from.code,
                                            throw.error.if.missing = T,
                                            error.prefix = paste0("Cannot copy calibration info from '", from.code, "'"))
    
    calibration.info$code = to.code
    CALIBRATION.MANAGER$info[[to.code]] = calibration.info
    
    invisible(NULL)
}


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
                                 parameter.scales,
                                 get.one.set.of.parameters = T,
                                 burn.fraction = 0.5,
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
    
    x = load(file.path(dir, 'global_control.Rdata'))
    global.control = get(x)
    
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
    
    sample.mean = NULL
    sample.cov = NULL
    n.samples = NULL

samples = NULL
    
    if (!get.one.set.of.parameters)
    {
        # Figure out which chunks we are going to use
        chunks.to.use = (1:global.control@n.chunks)[(1:global.control@n.chunks)/global.control@n.chunks>burn.fraction]
        hypothetical.chunk.files = unlist(lapply(1:global.control@n.chains, function(chain){
            file.path(paste0("chain_", chain),
                      paste0("chain", chain, "_chunk", chunks.to.use, ".Rdata"))
        }))
        chunk.files = chunk.files[sapply(file.path(dir, hypothetical.chunk.files), file.exists)]
        
        if (length(chunk.files)/length(hypothetical.chunk.files) < 1)
            stop("Cannot prepare MCMC summary - we have not finished the sampling for the prior calibration")
        
        # Iterate through each chunk and use to calculate sample mean and covariance    
        for (chunk.file in chunk.files)
        {
            # Calculate the mean and covariance of the new samples
            new.samples = mcmc@samples
            n.new.samples = prod(dim(new.samples)) / dim(new.samples)['variable']
            
            new.samples = sapply(dimnames(new.samples)$variable, function(var.name){
                values = new.samples[,,var.name]
                scale = parameter.scales[var.name]
                if (scale=='identity')
                    values
                else if (scale=='log')
                    log(values)
                else if (scale=='logit')
                    log(values) - log(1-values)
                else
                    stop(paste0("Don't know what to do with scale = '", scale, "'"))
            })
            
            mean.new.samples = colMeans(new.samples)
            cov.new.samples = cov(new.samples)
samples = rbind(samples, new.samples)
            
            # Incorporate into the overall mean and cov mat
            if (is.null(sample.mean))
            {
                sample.mean = mean.new.samples
                sample.cov = cov.new.samples
                n.samples = n.new.samples
            }
            else
            {
                old.n = n.samples
                old.mean = sample.mean
                old.cov = sample.cov
                n.samples = old.n + n.new.samples
                
                sample.mean = old.n/n.samples * old.mean + n.new.samples/n.samples * mean.new.samples
                sample.cov = 1/(n.samples-1) *
                    ( (old.n-1) * old.cov + old.n * old.mean %*% t(old.mean) +
                      (n.new.samples-1) * cov.new.samples + n.new.samples * mean.new.samples %*% t(mean.new.samples) - 
                      n.samples * sample.mean %*% t(sample.mean) )
            }
        }
    }
    
    all.transformed.parameter.values = sapply(chain.controls, function(ctrl){
        ctrl@chain.state@mean.transformed.parameters
    })
    
    transformed.parameter.values = rowMeans(all.transformed.parameter.values)
    
    #-- Initial Scaling Steps --#
    # Andrew says lapply would be safer here since we're expecting a list anyway
    initial.scaling.parameters = sapply(1:length(state1@log.scaling.parameters), function(i){
        all.chain.log.values = sapply(chain.controls, function(ctrl){
            ctrl@chain.state@log.scaling.parameters[[i]]
        })
        
        dim(all.chain.log.values) = c(length(all.chain.log.values) / length(chain.controls),
                                      length(chain.controls))
        
        rv = exp(rowMeans(all.chain.log.values))
        names(rv) = names(chain.controls[[1]]@chain.state@log.scaling.parameters[[i]])
        rv
    })
    names(initial.scaling.parameters) = names(global.control@control@var.blocks)
    
    
    #-- Return --#
    list(
        transformed.parameter.values = transformed.parameter.values,
        cov.mat = cov.mat,
        initial.scaling.parameters = initial.scaling.parameters,
        parameter.names = names(transformed.parameter.values),
        sample.mean = sample.mean,
        sample.cov = sample.cov,
        
samples = samples
    )
}

##-------------##
##-- HELPERS --##
##-------------##





get.distribution.variable.transformation = function(dist, unknown.value)
{
    if (is(dist, "Joint_Independent_Distributions"))
        rv = unlist(lapply(dist@subdistributions, 
                      get.distribution.variable.transformation, 
                      unknown.value = unknown.value))
    else if (is.null(dist@transformation))
        rv = rep(unknown.value, dist@n.var)
    else
        rv = rep(dist@transformation@name, dist@n.var)
    
    names(rv) = dist@var.names
    rv
}
