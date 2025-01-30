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
run.new.algorithm <- function(version,
                              location,
                              calibration.code,
                              root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                              sub.version = NULL,
                              cache.frequency = 500,
                              allow.overwrite.cache = F,
                              verbose = T)
{
    #----
    #------------------------#
    #-- Validate Arguments --#
    #------------------------#
    
    if (!is.character(version) || length(version)!=1 || is.na(version))
        stop("Cannot set.up.calibration: version must be a single, non-NA character value")
    if (!is.specification.registered.for.version(version))
        stop(paste0("Cannot set.up.calibration: no specification has been registered for version '", version, "'"))
    
    if (!is.character(calibration.code) || length(calibration.code)!=1 || is.na(calibration.code))
        stop("Cannot set.up.calibration: calibration.code must be a single, non-NA character value")
    
    if (!is.character(location) || length(location)!=1 || is.na(location))
        stop("Cannot set.up.calibration: location must be a single, non-NA character value")
    
    error.prefix = paste0("Cannot Set Up Calibration for code '", calibration.code, "' for version '", version, "' and location '", location, "': ")
    verbose.prefix = paste0("CALIBRATION '", calibration.code, "' (", version, " / ", location, "): ")
    
    if (!locations::is.location.valid(location))
        stop(paste0(error.prefix, "'", location, "' is not a valid location code"))
    
    if (!is.character(root.dir) || length(root.dir)!=1 || is.na(root.dir))
        stop(paste0(error.prefix, "'root.dir' must be a single, non-NA character value"))
    if (!dir.exists(root.dir) && length(list.files(root.dir))==0)
        stop(paste0(error.prefix, "The specified root.dir ('", root.dir, "') does not exist"))
    
    # sub-version
    if (!is.null(sub.version) &&
        (!is.character(sub.version) || length(sub.version)!=1 || is.na(sub.version)))
        stop(paste0(error.prefix, "'sub.version' must be either NULL or a single, non-NA character value"))
    
    # cache frequency
    if (!is.numeric(cache.frequency) || length(cache.frequency)!=1 || is.na(cache.frequency) || round(cache.frequency)!=cache.frequency)
        stop(paste0(error.prefix, "'cache.frequency' must be a single, non-NA integer value"))
    if (cache.frequency <= 0)
        stop(paste0(error.prefix, "'cache.frequency' must be >= 1"))
    
    # allow.overwrite.cache
    if (!is.logical(allow.overwrite.cache) || length(allow.overwrite.cache)!=1 || is.na(allow.overwrite.cache))
        stop(paste0(error.prefix, "'allow.overwrite.cache must be a single, non-NA logical value (either TRUE or FALSE)"))
    
    # verbose
    if (!is.logical(verbose) || length(verbose)!=1 || is.na(verbose))
        stop(paste0(error.prefix, "'verbose must be a single, non-NA logical value (either TRUE or FALSE)"))
    
    #----
    #--------------------------------------#
    #-- Pull the calibration info object --#
    #--------------------------------------#
    # browser()
    calibration.info = get.calibration.info(calibration.code,
                                            throw.error.if.missing = T,
                                            error.prefix = error.prefix)
    
    spec = get.compiled.specification.for.version(version)
    if (calibration.info$draw.from.parent.version)
    {
        if (is.null(spec$parent.version))
            stop(paste0(error.prefix, "We were instructed to draw.from.parent.version in setting up the calibration, but there is no parent version for the '", version, "' specification"))   
        if (!is.specification.registered.for.version(spec$parent.version))
            stop(paste0(error.prefix, "We were instructed to draw.from.parent.version in setting up the calibration, but no specification has been registered for the parent version '", spec$parent.version, "'"))   
    }
    
    #-------------------------#
    #-- Pull Down the Prior --#
    #-------------------------#
    
    if (verbose)
        print(paste0(verbose.prefix, "Pulling prior, setting up defaults..."))
    
    # Pull the prior for the version
    prior = get.parameters.distribution.for.version(version, type='calibrated')
    
    missing.parameter.names = setdiff(calibration.info$parameter.names, prior@var.names)
    if (length(missing.parameter.names)>0)
        stop(paste0(error.prefix,
                    length(missing.parameter.names), " ",
                    ifelse(length(missing.parameter.names)==1, "parameter is", "parameters are"),
                    " included in the calibration's 'parameter.names' but ", 
                    ifelse(length(missing.parameter.names)==1, "is", "are"),
                    " not present in the '", version,
                    "' prior: ",
                    collapse.with.and("'", missing.parameter.names, "'")))
    
    #------------------------------------#
    #-- Sift Through Parameter Aliases --#
    #------------------------------------#
    
    parsed.aliases = parse.calibration.parameter.aliases(calibration.info = calibration.info, prior = prior)
    
    model.parameters.in.aliases = parsed.aliases$model.parameters.in.aliases
    parameter.aliases = parsed.aliases$parameter.aliases
    non.aliased.model.parameter.names = parsed.aliases$non.aliased.model.parameter.names
    sorted.parameter.aliases = parsed.aliases$sorted.parameter.aliases
    
    mcmc.parameter.names = parsed.aliases$mcmc.parameter.names
    model.parameter.names = parsed.aliases$model.parameter.names
    
    missing.parameter.names = setdiff(model.parameters.in.aliases, prior@var.names)
    if (length(missing.parameter.names)>0)
        stop(paste0(error.prefix,
                    length(missing.parameter.names), " ",
                    ifelse(length(missing.parameter.names)==1, "parameter appears", "parameters appear"),
                    " in the calibration's 'parameter.aliases' but ",
                    ifelse(length(missing.parameter.names)==1, "is", "are"),
                    " not present in the '", version,
                    "' prior: ",
                    collapse.with.and("'", missing.parameter.names, "'")))
    
    missing.fixed.parameter.names = setdiff(names(calibration.info$fixed.initial.parameter.values), 
                                            model.parameter.names)
    if (length(missing.fixed.parameter.names)>0)
        stop(paste0(error.prefix,
                    length(missing.fixed.parameter.names), " ",
                    ifelse(length(missing.fixed.parameter.names)==1, "parameter appears", "parameters appear"),
                    " in the calibration's 'fixed.initial.parameter.values' but ",
                    ifelse(length(missing.fixed.parameter.names)==1, "is", "are"),
                    " not present in among any of the listed 'parameter.names' nor among the values of 'parameter.aliases': ",
                    collapse.with.and("'", missing.fixed.parameter.names, "'")))
    
    #----------------------#
    #-- Subset the Prior --#
    #----------------------#
    
    # Subset the relevant parameters
    relevant.prior.for.sampled.model.parameters = distributions::subset.distribution(prior, model.parameter.names) 
    
    # Pull Basic Info We Are Going to Use from the Prior
    all.parameter.scales = get.distribution.variable.transformation(prior, unknown.value='')
    names(all.parameter.scales) = prior@var.names
    
    all.prior.sds = suppressWarnings(get.sds(prior))
    all.prior.medians = suppressWarnings(get.medians(prior))
    all.prior.means = suppressWarnings(get.means(prior))
    
    
    #---------------------------------#
    #-- Prepare the Sampling Blocks --#
    #---------------------------------#
    
    # Pull down the sampling blocks
    sampling.blocks = get.parameter.sampling.blocks.for.version(version)
    
    # Include only the relevant parameters
    sampling.blocks = lapply(sampling.blocks, function(block){
        intersect(block, calibration.info$parameter.names)
    })
    
    sampling.blocks[names(calibration.info$parameter.aliases)] = 
        as.list(names(calibration.info$parameter.aliases))
    
    sampling.blocks = sampling.blocks[sapply(sampling.blocks, length)>0]
    
    unnamed.block.mask = is.na(names(sampling.blocks)) | names(sampling.blocks)==''
    names(sampling.blocks)[unnamed.block.mask] = paste0('block', 1:sum(unnamed.block.mask))
    
    
    #-------------------------------------------------------------#
    #-- Prepare Default Initial Values, Step Sizes, and Cov Mat --#
    #-------------------------------------------------------------#
    
    
    #-- Default parameter values --#
    
    all.default.model.parameter.values = all.prior.medians
    all.default.model.parameter.values[names(calibration.info$fixed.initial.parameter.values)] = 
        calibration.info$fixed.initial.parameter.values
    
    default.model.parameter.values = all.default.model.parameter.values[model.parameter.names]
    default.alias.parameter.values = rep(1, length(parameter.aliases))
    names(default.alias.parameter.values) = names(parameter.aliases)
    default.mcmc.parameter.values = c(all.default.model.parameter.values[non.aliased.model.parameter.names],
                                      default.alias.parameter.values)[mcmc.parameter.names]
    
    #-- MCMC Parameter Scales --#
    mcmc.parameter.scales = all.parameter.scales[non.aliased.model.parameter.names]
    mcmc.parameter.scales[names(parameter.aliases)] = 'log'
    
    #-- Set up initial.cov.mat --#
    
    DEFAULT.SD.REDUCTION.FACTOR = 20 # The 20 here is arbitrary, but seems to work well
    
    all.raw.sds = all.prior.sds
    improper.param.names = prior@var.names[prior@is.improper]
    all.raw.sds[improper.param.names] = default.model.parameter.values[improper.param.names]
    
    all.raw.means = all.prior.means
    all.raw.means[prior@is.improper] = default.model.parameter.values[prior@is.improper]
    
    all.default.parameter.sds = all.raw.sds
    all.default.parameter.sds[all.parameter.scales=='log'] = sqrt(log(all.raw.sds[all.parameter.scales=='log']^2 / all.raw.means[all.parameter.scales=='log']^2 + 1)) # This comes from the relationship between mean and SD in a lognormal
    all.default.parameter.sds = all.default.parameter.sds / DEFAULT.SD.REDUCTION.FACTOR
    
    mcmc.parameter.sds = all.default.parameter.sds[non.aliased.model.parameter.names]
    
    if (length(parameter.aliases)>0)
        mcmc.parameter.sds = c(mcmc.parameter.sds,
                               sapply(parameter.aliases, function(aliased.param.names){
                                   mean(all.default.parameter.sds[aliased.param.names])
                               }))
    mcmc.parameter.sds = mcmc.parameter.sds[mcmc.parameter.names]
    
    default.initial.cov.mat = diag(mcmc.parameter.sds[mcmc.parameter.names]^2)
    dimnames(default.initial.cov.mat) = list(mcmc.parameter.names, mcmc.parameter.names)
    
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
    
    initial.model.parameter.values = default.model.parameter.values
    initial.cov.mat = default.initial.cov.mat[mcmc.parameter.names, mcmc.parameter.names]
    initial.scaling.parameters = default.initial.scaling.parameters
    
    dynamic.preceding.codes = calibration.info$preceding.calibration.codes #we'll potentially add to this list in the while block below
    mcmc.parameters.already.pulled.from.preceding = character()
    preceding.index = 1
    
    if (calibration.info$draw.from.parent.version)
        version.for.preceding = spec$parent.version
    else
        version.for.preceding = version
    
    while (preceding.index <= length(dynamic.preceding.codes))
    {
        preceding.code = dynamic.preceding.codes[preceding.index]
        preceding.info = get.calibration.info(code = preceding.code, throw.error.if.missing = F)
        if (is.null(preceding.info))
            stop(paste0(error.prefix, "No calibration has been registered for the preceding.calibration.code of '", preceding.code, "'"))
        
        # Separate out mcmc parameters from the preceding info
        parsed.preceding.aliases = parse.calibration.parameter.aliases(calibration.info = preceding.info, prior = prior)
        
        # Figure out which mcmc parameters we want to pull covariance/scaling for
        overlapping.non.aliased.model.parameter.names = intersect(non.aliased.model.parameter.names,
                                                                  parsed.preceding.aliases$non.aliased.model.parameter.names)
        overlapping.parameter.aliases = intersect(names(parameter.aliases),
                                                  names(parsed.preceding.aliases$parameter.aliases))
        overlapping.parameter.aliases = overlapping.parameter.aliases[sapply(overlapping.parameter.aliases, function(alias.name){
            setequal(parameter.aliases[[alias.name]], parsed.preceding.aliases[[alias.name]])
        })]
        mcmc.parameters.to.pull.from.preceding = setdiff(c(overlapping.non.aliased.model.parameter.names, overlapping.parameter.aliases),
                                                         mcmc.parameters.already.pulled.from.preceding)
        
        # Pull from preceding
        if (preceding.index==1 || length(mcmc.parameters.to.pull.from.preceding)>0)
        {
            mcmc.summary = prepare.mcmc.summary(version = version.for.preceding,
                                                location = location,
                                                calibration.code = preceding.code,
                                                root.dir = root.dir,
                                                get.one.set.of.parameters = calibration.info$is.preliminary,
                                                error.prefix = error.prefix)
            
            # Pull the model parameter values
            if (preceding.index==1)
            {
                shared.parameter.names = intersect(names(mcmc.summary$last.sim.parameters),
                                                   names(all.default.model.parameter.values))
                
                initial.model.parameter.values[shared.parameter.names] = mcmc.summary$last.sim.parameters[shared.parameter.names]
            }
            
            other.parameters.for.cov = setdiff(mcmc.parameter.names, mcmc.parameters.to.pull.from.preceding)
            mcmc.parameters.already.pulled.from.preceding = union(mcmc.parameters.already.pulled.from.preceding,
                                                                  mcmc.parameters.to.pull.from.preceding)
            
            # Pull for the cov mat
            parameters.with.na.initial.cov.mat.mask = is.na(diag(initial.cov.mat)[mcmc.parameters.to.pull.from.preceding])
            parameters.to.pull.all.cov.from.preceding = mcmc.parameters.to.pull.from.preceding[parameters.with.na.initial.cov.mat.mask]
            parameters.to.pull.weighted.cov.from.preceding = mcmc.parameters.to.pull.from.preceding[!parameters.with.na.initial.cov.mat.mask]
            parameters.to.pull.weighted.cov.from.preceding = intersect(parameters.to.pull.weighted.cov.from.preceding,
                                                                       dimnames(mcmc.summary$cov.mat)[[1]])
            
            initial.cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding] = initial.cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding] * (1-calibration.info$weight.to.preceding.variance.estimates) +
                mcmc.summary$cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding] * calibration.info$weight.to.preceding.variance.estimates
            initial.cov.mat[parameters.to.pull.all.cov.from.preceding,parameters.to.pull.all.cov.from.preceding] = 
                mcmc.summary$cov.mat[parameters.to.pull.all.cov.from.preceding,parameters.to.pull.all.cov.from.preceding]
            initial.cov.mat[parameters.to.pull.all.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding] = 
                mcmc.summary$cov.mat[parameters.to.pull.all.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding]
            initial.cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.all.cov.from.preceding] = 
                mcmc.summary$cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.all.cov.from.preceding]
            
            initial.cov.mat[mcmc.parameters.to.pull.from.preceding,other.parameters.for.cov] = initial.cov.mat[mcmc.parameters.to.pull.from.preceding,other.parameters.for.cov] = 0
            initial.cov.mat[other.parameters.for.cov, mcmc.parameters.to.pull.from.preceding] = initial.cov.mat[other.parameters.for.cov, mcmc.parameters.to.pull.from.preceding] = 0
            
            initial.scaling.parameters.original=initial.scaling.parameters
            
            # Pull for the scaling parameters
            initial.scaling.parameters.names = names(initial.scaling.parameters)
            initial.scaling.parameters = lapply(names(initial.scaling.parameters), function(scaling.name){
                scaling = initial.scaling.parameters[[scaling.name]]
                to.pull.scaling = mcmc.summary$initial.scaling.parameters[[scaling.name]]
                if (!is.null(to.pull.scaling))
                {
                    to.pull.params = intersect(mcmc.parameters.to.pull.from.preceding, names(to.pull.scaling))
                    scaling[to.pull.params] = to.pull.scaling[to.pull.params]
                }
                
                scaling
            })
            names(initial.scaling.parameters) = initial.scaling.parameters.names
        }
        
        # add it's preceding codes to the pile
        dynamic.preceding.codes = union(dynamic.preceding.codes, preceding.info$preceding.calibration.codes)
        
        # increment for the next iteration through the while loop
        preceding.index = preceding.index + 1
    }
    
    all.initial.model.parameter.values = all.default.model.parameter.values
    all.initial.model.parameter.values[names(initial.model.parameter.values)] = initial.model.parameter.values
    
    #-- Set up starting values --#
    # For starting values, we need one row for each chain
    if (calibration.info$is.preliminary)
    {
        single.starting.mcmc.parameter.values = initial.model.parameter.values[non.aliased.model.parameter.names]
        single.starting.mcmc.parameter.values[names(parameter.aliases)] = 1
        
        starting.mcmc.parameter.values = t(sapply(1:calibration.info$n.chains, function(chain){
            single.starting.mcmc.parameter.values[mcmc.parameter.names]
        }))
        dim(starting.mcmc.parameter.values) = c(calibration.info$n.chains, length(single.starting.mcmc.parameter.values))
        dimnames(starting.mcmc.parameter.values) = list(NULL, parameter=mcmc.parameter.names)
    }
    else
    {
        stop("We're not yet set up to run non-preliminary calibrations")
    }
    
    #-----------------#
    #-- Some Checks --#
    #-----------------#
    
    if (any(is.na(starting.mcmc.parameter.values)))
    {
        na.parameters = calibration.info$parameter.names[apply(is.na(starting.mcmc.parameter.values),2,any)]
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
    
    likelihood = calibration.info$likelihood.instructions$instantiate.likelihood(version = version,
                                                                                 location = location,
                                                                                 sub.version = sub.version,
                                                                                 data.manager = calibration.info$data.manager)
    
    compute.likelihood <- function(sim) {
        lik = likelihood$compute(sim=sim, log=T, use.optimized.get=T, check.consistency=F)
        
        if (is.na(lik))
        {
            error.dir = file.path(get.jheem.root.directory(), 'errors')
            if (!dir.exists(error.dir))
                dir.create(error.dir)
            error.filename = filepath(error.dir, 
                                      paste0("NA_lik_", location, "_", Sys.Date(), ".Rdata"))
            
            print(paste0("THE LIKELIHOOD FOR THE SIMULATION EVALUATED TO NA"))
            print(paste0("  TO ALLOW THE MCMC TO KEEP RUNNING, WILL RETURN log likelihood = -Inf"))
            print(paste0("  SAVING THE SIM THAT GENERATED THIS ERROR TO FILE: '", error.filename, "'"))
            
            -Inf
        }
        else
            lik
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
    
    engine$crunch(parameters = all.initial.model.parameter.values)
    
    # 'params' is the subset of parameters that we will be modifying in the MCMC
    run.simulation <- function(params) {
        all.parameters = all.initial.model.parameter.values
        params.after.aliases = apply.calibration.parameter.aliases(sorted.parameter.aliases = sorted.parameter.aliases,
                                                                   non.aliased.parameter.names = non.aliased.model.parameter.names,
                                                                   parameter.initial.values = all.initial.model.parameter.values,
                                                                   sampled.values = params)
        all.parameters[names(params.after.aliases)] = params.after.aliases
        engine$run(all.parameters)
    }
    
    #-- Check the initial sims to make sure the likelihood computes (not to -Inf) --#
    if (verbose)
        print(paste0(verbose.prefix, "Making sure the likelihood computes on initial simulation(s)..."))
    for (i in 1:dim(starting.mcmc.parameter.values)[1])
    {
        sim = run.simulation(starting.mcmc.parameter.values[i,])
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
    
    prior.density.function = distributions::get.density.function(relevant.prior.for.sampled.model.parameters, default.log = T)
    mcmc.prior = function(parameters){
        parameters.after.aliases = apply.calibration.parameter.aliases(sorted.parameter.aliases = sorted.parameter.aliases,
                                                                       non.aliased.parameter.names = non.aliased.model.parameter.names,
                                                                       parameter.initial.values = initial.model.parameter.values,
                                                                       sampled.values = parameters)
        
        prior.density.function(parameters.after.aliases)
    }

    # run MCMC ----
    run.mcmc <- function(start.params,
                         n.iterations) {
        modified.ctrl = 
        bayesian.simulations::run.mcmc.with.cache(ctrl, n.iterations, starting.values = matrix(start.params, nrow=1), update.frequency = 1, cache.frequency = 500, cache.dir = tempdir())
    }
    # browser()
    # Plop in my whole algorithm here, which uses the run.mcmc above
    
    K=6
    ptK=2
    thin.by=2
    
    ctrl = bayesian.simulations::create.adaptive.blockwise.metropolis.control(
        var.names = mcmc.parameter.names,
        simulation.function = run.simulation,
        log.prior.distribution = mcmc.prior,
        log.likelihood = compute.likelihood, # saves the data manager in here!
        burn = calibration.info$n.burn,
        thin = thin.by, # this will need to change
        var.blocks = sampling.blocks,
        reset.adaptive.scaling.update.after = 0,
        transformations = mcmc.parameter.scales,
        
        initial.covariance.mat = initial.cov.mat,
        initial.scaling.parameters = initial.scaling.parameters,
        
        target.acceptance.probability = target.acceptance.rate,
        
        n.iter.before.use.adaptive.covariance = n.iter.before.cov,
        adaptive.covariance.base.update = cov.base.update,
        adaptive.covariance.update.prior.iter = cov.update.prior,
        adaptive.covariance.update.decay = cov.update.decay,
        adaptive.scaling = 'componentwise',
        adaptive.scaling.base.update = scaling.base.update,
        adaptive.scaling.update.prior.iter= scaling.update.prior,
        adaptive.scaling.update.decay = scaling.update.decay
    )
    
    start.params = starting.mcmc.parameter.values # 10 by 80
    
    current.sample = lapply(1:K, function(k) {run.simulation(params = starting.mcmc.parameter.values[k,])})
    
    i = 1
    h = Inf
    pi.value = sapply(1:K, function(k) {runif(1)})
    sample.likelihoods = sapply(current.sample, function(s) {likelihood$compute(s, log=T)})
    l = max(sample.likelihoods)
    
    while (h > 0 && i < 3) {
        i = i + 1
        
        # select threshold level h
        g.values = sapply(1:K, function(k) {
            log(pi.value[k]) + l - sample.likelihoods[k]
        })
        names(g.values) = 1:K
        g.values = sort(g.values)
        h = 0.5 * (g.values[ptK] + g.values[ptK + 1]) # pt-percentile of ordered set
        n = sum(g.values <= max(h, 0))
        if (h < 0) h = 0

        # run n Markov chains with K * thin / n iterations each,
        # so that we end up with K samples for the next round
        new.sample = lapply(as.numeric(names(g.values)), function(k) {
            tryCatch({run.mcmc(start.params[k,],
                               n.iterations = K * thin.by/n)}, error=function(e) {browser()})
            
        })
        browser()
        
        new.sample.simulations = unlist(lapply(new.sample, function(x) {x@simulations}))
        
        sample.likelihoods = sapply(new.sample.simulations, function(s) {likelihood$compute(s, log=T)})
        
        # update the value of the scaling constant "l"
        l.new = max(l, sample.likelihoods)
        h = h - l + l.new
        l = l.new
        
        # decrease dependence of the K samples
        pi.value = sapply(1:K, function(k) {runif(0, min(1, exp(sample.likelihoods) - l + h))})
        
    }
    
    if (verbose)
        print(paste0(verbose.prefix, "All Done!"))
}