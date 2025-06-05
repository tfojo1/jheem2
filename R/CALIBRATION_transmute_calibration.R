
#'@export
set.up.transmute.calibration <- function(transmute.code,
                                         from.calibration.code,
                                         location,
                                         n.sim,
                                         n.chunks,
                                         allow.overwrite.cache = F,
                                         root.dir = get.jheem.root.directory(),
                                         return.simulations = F,
                                         verbose = T)
{
    #------------------------#
    #-- Validate Arguments --#
    #------------------------#
    
    if (!is.character(transmute.code) || length(transmute.code)!=1 || is.na(transmute.code))
        stop("Cannot set.up.transmute.calibration: transmute.code must be a single, non-NA character value")
    
    if (!is.character(from.calibration.code) || length(from.calibration.code)!=1 || is.na(from.calibration.code))
        stop("Cannot set.up.transmute.calibration: from.calibration.code must be a single, non-NA character value")
    
    if (!is.character(location) || length(location)!=1 || is.na(location))
        stop("Cannot set.up.transmute.calibration: location must be a single, non-NA character value")
    
    error.prefix = paste0("Cannot Set Up Transmute Calibration for code '", transmute.code, "' for location '", location, "': ")
    verbose.prefix = paste0("TRANSMUTE CALIBRATION '", transmute.code, "' (", location, "): ")
    
    if (!locations::is.location.valid(location))
        stop(paste0(error.prefix, "'", location, "' is not a valid location code"))
    
    if (!is.numeric(n.sim) && length(n.sim)!=1 || is.na(n.sim) || round(n.sim)!=n.sim)
        stop(paste0(error.prefix, "'n.sim' must be a single, non-NA, integer value"))
    
    if (!is.character(root.dir) || length(root.dir)!=1 || is.na(root.dir))
        stop(paste0(error.prefix, "'root.dir' must be a single, non-NA character value"))
    if (!dir.exists(root.dir) && length(list.files(root.dir))==0)
        stop(paste0(error.prefix, "The specified root.dir ('", root.dir, "') does not exist"))
    
    # n.chunks
    if (!is.numeric(n.chunks) || length(n.chunks)!=1 || is.na(n.chunks) || round(n.chunks)!=n.chunks)
        stop(paste0(error.prefix, "'n.chunks' must be a single, non-NA integer value"))
    if (n.chunks <= 0)
        stop(paste0(error.prefix, "'n.chunks' must be >= 1"))
    
    # allow.overwrite.cache
    if (!is.logical(allow.overwrite.cache) || length(allow.overwrite.cache)!=1 || is.na(allow.overwrite.cache))
        stop(paste0(error.prefix, "'allow.overwrite.cache must be a single, non-NA logical value (either TRUE or FALSE)"))
    
    # verbose
    if (!is.logical(verbose) || length(verbose)!=1 || is.na(verbose))
        stop(paste0(error.prefix, "'verbose must be a single, non-NA logical value (either TRUE or FALSE)"))
    
    
    #--------------------------------------#
    #-- Pull the calibration info object --#
    #--------------------------------------#
    
    calibration.info = get.transmute.calibration.info(code = transmute.code,
                                                      throw.error.if.missing = T,
                                                      error.prefix = error.prefix)
    
    if (!is.specification.registered.for.version(calibration.info$from.version))
        stop(paste0(error.prefix, "No specification has been registered for from.version '", calibration.info$from.version, "'"))
    if (!is.specification.registered.for.version(calibration.info$to.version))
        stop(paste0(error.prefix, "No specification has been registered for to.version '", calibration.info$to.version, "'"))
    
    from.specification = get.compiled.specification.for.version(calibration.info$from.version)
    to.specification = get.compiled.specification.for.version(calibration.info$to.version)
    
    # sub-version
    if (!is.null(calibration.info$from.sub.version) && !any(from.specification==calibration.info$from.sub.version))
        stop(paste0(error.prefix, "The from.version '", calibration.info$from.version, 
                    "' does not contain a sub-version named '",
                    calibration.info$from.sub.version))
    
    if (!is.null(calibration.info$to.sub.version) && !any(to.specification==calibration.info$to.sub.version))
        stop(paste0(error.prefix, "The to.version '", calibration.info$to.version, 
                    "' does not contain a sub-version named '",
                    calibration.info$to.sub.version))
    
    
    #-- PULL THE PREVIOUS SIMSET --#
    
    if (verbose)
        print(paste0(verbose.prefix, "Loading '", calibration.info$from.version, "' simset to transmute..."))
    
    pre.simset = retrieve.simulation.set(version = calibration.info$from.version,
                                         location = location,
                                         calibration.code = from.calibration.code,
                                         sub.version = calibration.info$from.sub.version,
                                         n.sim = n.sim)
    
    #-- SET UP TRANSMUTER --#
    
    if (verbose)
        print(paste0(verbose.prefix, "Setting up transmuter..."))
    
    transmuter = create.jheem.transmuter(pre.simset, 
                                         to.version = calibration.info$to.version,
                                         to.sub.version = calibration.info$to.sub.version)
    
    #-- SET UP THE 'RUN' SIMULATION FUNCTION --#
    
    
    transmute.simulation = function(parameters)
    {
        # return(
        #     transmuter$transmute(sim.index = mcmc.settings$sim.index,
        #                          parameters = parameters))
    #    tryCatch({
            transmuter$transmute(sim.index = mcmc.settings$sim.index,
                                 parameters = parameters)
        # },
        # error = function(e){
        #     browser()
        #     NULL
        # })
    }
    
    trans.env = new.env(parent = baseenv())
    trans.env$transmuter = transmuter
    trans.env$mcmc.settings = list()
    environment(transmute.simulation) = trans.env
    
    #-- INSTANTIATE THE LIKELIHOOD --#
    
    if (verbose)
        print(paste0(verbose.prefix, "Instantiating the likelihood..."))
    
    likelihood = calibration.info$likelihood.instructions$instantiate.likelihood(
        version = calibration.info$to.version,
        location = location
    )
    
    compute.likelihood = function(sim)
    {
        if (is.null(sim))
            -Inf
        else
            likelihood$compute(sim, use.optimized.get=T, log=T, check.consistency=F)
    }
    
    lik.env = new.env(parent = baseenv())
    lik.env$likelihood = likelihood
    environment(compute.likelihood) = lik.env
    
    #-- SET UP PARAMETERS, SCALES, and COV MAT --#
    
    if (verbose)
        print(paste0(verbose.prefix, "Setting up initial parameters and prior..."))
    
    # Parameter Scales
    parameter.scales = get.distribution.variable.transformation(calibration.info$prior.distribution)
    
    # Initial cov.mat
    
    parameter.sds = suppressWarnings(get.sds(calibration.info$prior.distribution))
    if (any(is.na(parameter.sds)))
        stop("Cannot have improper variables in the prior distribution")
    parameter.means = suppressWarnings(get.means(calibration.info$prior.distribution))
    
    parameter.sds[parameter.scales=='log'] = sqrt(log(parameter.sds[parameter.scales=='log']^2 / parameter.means[parameter.scales=='log']^2 + 1)) # This comes from the relationship between mean and SD in a lognormal
    DEFAULT.SD.REDUCTION.FACTOR = 20 # The 20 here is arbitrary, but seems to work well
    parameter.sds = parameter.sds / DEFAULT.SD.REDUCTION.FACTOR
    
    initial.cov.mat = diag(parameter.sds^2)
    
    # Initial scaling parameters
    initial.scaling.parameters = lapply(calibration.info$sampling.blocks, function(block){
        sapply(block, function(var.in.block){
            2.38^2/length(block)
        })
    })
    
    # Initial Parameters
    
    default.start.values = suppressWarnings(get.medians(calibration.info$prior.distribution))
    default.start.values[names(calibration.info$fixed.initial.parameter.values)] = calibration.info$fixed.initial.parameter.values
    if (any(is.na(default.start.values)))
    {
        na.vars = names(default.start.values)[is.na(default.start.values)]
        stop(paste0(error.prefix,
                    "The prior distribution produces ",
                    ifelse(length(na.vars)==1, 
                           "an NA median value for parameter ",
                           "NA median values for parameters "),
                    collapse.with.and("'", na.vars, "'"),
                    ". Use fixed.initial.parameter.values in the register.transmute.calibration() function to set values for these parameters"))
    }
    
    
    
    # Do we want to add an "update default start values" function here?
    #sim0 = transmuter$transmute(sim.index = 1, parameters = default.start.values)

    log.prior.fn = function(x, log=T)
    {
        distributions::calculate.density(prior, x, log=log)
    }
    
    prior.env = new.env(baseenv())
    prior.env$prior = calibration.info$prior.distribution
    environment(log.prior.fn) = prior.env
    
    #-- PULL IT TOGETHER in MCMC SETTINGS --#
    
    trans.env$mcmc.settings$sim.index = 1
    trans.env$mcmc.settings$start.values = default.start.values
    trans.env$mcmc.setting$n.iter = 0
    trans.env$mcmc.settings$ctrl = bayesian.simulations::create.adaptive.blockwise.metropolis.control(
        var.names = prior.env$prior@var.names,
        simulation.function = transmute.simulation,
        log.prior.distribution = log.prior.fn,
        log.likelihood = compute.likelihood, # saves the data manager in here!
        burn = calibration.info$n.iter.first.sim,
        thin = 1,
        var.blocks = calibration.info$sampling.blocks,
        reset.adaptive.scaling.update.after = 0,
        transformations = parameter.scales,
        
        initial.covariance.mat = initial.cov.mat,
        initial.scaling.parameters = initial.scaling.parameters,
        
        target.acceptance.probability = 0.238,
        
        n.iter.before.use.adaptive.covariance = 0,
        adaptive.covariance.base.update = 1,
        adaptive.covariance.update.prior.iter = 50,
        adaptive.covariance.update.decay = 1,
        adaptive.scaling = 'componentwise',
        adaptive.scaling.base.update = 1,
        adaptive.scaling.update.prior.iter= 10,
        adaptive.scaling.update.decay = 0.5
    )
    
    #-- RUN THE FIRST SIMULATION --#
    
    if (verbose)
        print(paste0(verbose.prefix, "Running the first sim..."))
    
    first.sim = trans.env$transmuter$transmute(sim.index = 1, parameters=trans.env$mcmc.settings$start.values)
    lik.value = compute.likelihood(first.sim)
    
    if (is.infinite(lik.value))
    {
        .GlobalEnv$errored.sim = first.sim
        .GlobalEnv$errored.likelihood = lik.env$likelihood
        stop(paste0("The likelihood on the first transmutation of the first sim evaluates to -Inf. The first simulation and likelihood have been saved in the global environment as 'errored.sim' and 'errored.likelihood"))
    }
    
    if (verbose)
        print(paste0(verbose.prefix, "Running MCMC on the first sim..."))
    
    mcmc = bayesian.simulations::run.mcmc(control = trans.env$mcmc.settings$ctrl,
                                          n.iter = calibration.info$n.iter.first.sim,
                                          starting.values = trans.env$mcmc.settings$start.values,
                                          cache.frequency = NA,
                                          update.detail = 'none',
                                          update.frequency = NA)
    
    trans.env$mcmc.settings = update.transmute.mcmc.settings(mcmc = mcmc,
                                   mcmc.settings = trans.env$mcmc.settings,
                                   n.iter = calibration.info$n.iter.subsequent.sims)
    
    #-- SET UP THE CONTROL and SAVE IT --#
    
    if (verbose)
        print(paste0(verbose.prefix, "Saving calibration control..."))
    
    ctrl = create.transmute.calibration.control(transmute.calibration.info = calibration.info,
                                                from.calibration.code = from.calibration.code,
                                                location = location,
                                                n.sim = n.sim,
                                                n.chunks = n.chunks,
                                                likelihood = likelihood,
                                                default.mcmc.settings = trans.env$mcmc.settings)
    
    dir = get.transmute.calibration.dir(to.version = calibration.info$to.version,
                                        location = location,
                                        transmute.code = transmute.code,
                                        n.sim = n.sim,
                                        to.sub.version = calibration.info$to.sub.version,
                                        root.dir = root.dir)
    
    if (!dir.exists(dir))
        dir.create(dir, recursive = T)
    
    file = get.transmute.calibration.control.file(to.version = calibration.info$to.version,
                                                  location = location,
                                                  transmute.code = transmute.code,
                                                  n.sim = n.sim,
                                                  to.sub.version = calibration.info$to.sub.version,
                                                  root.dir = root.dir)
    
    save(ctrl, file=file)
    
    #-- DONE --#
    
    
    # return the MCMC in case we want to examine it
    if (return.simulations)
    {
        if (verbose)
            print(paste0(verbose.prefix, "Packaging up simulations from initial MCMC..."))
        
        rv = join.simulation.sets(mcmc@simulations[mcmc@simulation.indices])
        
        if (verbose)
            print(paste0(verbose.prefix, "All Done!"))
        
        rv
    }
    else
    {
        if (verbose)
            print(paste0(verbose.prefix, "All Done!"))
        
        invisible(NULL)
    }
}

create.transmute.calibration.control <- function(transmute.calibration.info,
                                                 from.calibration.code,
                                                 location,
                                                 n.sim,
                                                 n.chunks,
                                                 likelihood,
                                                 default.mcmc.settings)
{
    rv = transmute.calibration.info
    
    rv$from.calibration.code = from.calibration.code
    rv$n.chunks = n.chunks
    rv$location = location
    rv$n.sim = n.sim
    rv$likelihood = likelihood
    
    sim.index.breaks = floor(seq(0,n.sim,length=n.chunks+1))
    rv$sim.indices.for.chunk = lapply(1:n.chunks, function(chunk){
        (sim.index.breaks[chunk]+1):sim.index.breaks[chunk+1]
    })
    
    rv$default.mcmc.settings = default.mcmc.settings
    
    rv
}

#'@export
run.transmute.calibration <- function(transmute.code,
                                      location,
                                      n.sim,
                                      chunks = NULL,
                                      verbose = T,
                                      update.frequency = 500,
                                      update.detail = 'low',
                                      ignore.errors = T,
                                      root.dir = get.jheem.root.directory())
{
    #-- VALIDATE ARGUMENTS --#
    
    if (!is.character(transmute.code) || length(transmute.code)!=1 || is.na(transmute.code))
        stop("Cannot run.transmute.calibration: transmute.code must be a single, non-NA character value")
    
    if (!is.character(location) || length(location)!=1 || is.na(location))
        stop("Cannot run.transmute.calibration: location must be a single, non-NA character value")
    
    error.prefix = paste0("Cannot Set Up Transmute Calibration for code '", transmute.code, "' for location '", location, "': ")
    
    calibration.info = get.transmute.calibration.info(code = transmute.code,
                                                      throw.error.if.missing = T,
                                                      error.prefix = error.prefix)
    
    if (!is.numeric(n.sim) && length(n.sim)!=1 || is.na(n.sim) || round(n.sim)!=n.sim)
        stop(paste0(error.prefix, "'n.sim' must be a single, non-NA, integer value"))
    
    if (!is.numeric(chunks) && length(chunks)==0 || any(is.na(chunks)) || any(round(chunks)!=chunks))
        stop(paste0(error.prefix, "'chunks' must be a non-empty, non-NA, integer vector"))
    
    #-- LOAD THE CONTROL --#
    file = get.transmute.calibration.control.file(to.version = calibration.info$to.version,
                                                  location = location,
                                                  transmute.code = transmute.code,
                                                  n.sim = n.sim,
                                                  to.sub.version = calibration.info$to.sub.version,
                                                  root.dir = root.dir)
    
    if (!file.exists(file))
        stop(paste0(error.prefix, "No tranmute control has been set up at '", file, "'"))
    
    ctrl = get(load(file)[1])
    
    if (any(chunks <= 0) || any(chunks > ctrl$n.chunks))
        stop(paste0(error.prefix, "'chunks' must be between 1 and ", ctrl$n.chunks))
    
    
    #-- LOAD THE BASE SIMSET --#
    
    pre.simset = retrieve.simulation.set(version = calibration.info$from.version,
                                         n.sim = n.sim,
                                         location = location,
                                         calibration.code = ctrl$from.calibration.code)
    
    
    #-- RUN THE CHUNKS --#
    
    mcmc = NULL
    mcmc.settings = get.most.advanced.transmute.chunk.mcmc.settings(ctrl = ctrl, root.dir = root.dir)
    start.time = Sys.time()
    for (chunk in chunks)
    {
        sim.indices = ctrl$sim.indices.for.chunk[[chunk]]
        
       # tryCatch({
            
            # Actually Run It
            chunk.sims = list()
            for (i.index in 1:length(sim.indices))
            {
                i = sim.indices[i.index]
                
                mcmc.settings$sim.index = i
                n.iter.for.i = ctrl$n.iter.subsequent.sims
                
                if (verbose)
                    print(paste0("STARTING MCMC FOR SIM ", i, " of ", pre.simset$n.sim))
                
                look.back.i.sims.for.parameters = 0
                successful.first.sim = F
                sim = NULL
                while (!successful.first.sim && look.back.i.sims.for.parameters<calibration.info$max.lookback.attempts)
                {
                    look.back.i.sims.for.parameters = look.back.i.sims.for.parameters + 1
                    look.back.to.sim.i = i - look.back.i.sims.for.parameters
                   
                    if (look.back.to.sim.i==0)
                    {}
                    else if (look.back.to.sim.i >= sim.indices[1])
                        mcmc.settings$start.values = chunk.sims[[look.back.to.sim.i]]$params[names(mcmc.settings$start.values)]
                    else
                        break
                    
                    # Run the first sim and make sure the likelihood evaluates
                    sim = mcmc.settings$ctrl@simulation.function(mcmc.settings$start.values)
                    
                    if (!is.null(sim))
                    {
                        successful.first.sim = ctrl$likelihood$compute(sim, use.optimized.get=T)!=-Inf
                    }
                }
                
                if (!successful.first.sim)
                {
                    if (is.null(sim))
                    {
                        errored.params <<- mcmc.settings$start.values
                        
                        stop(paste0("The ", 
                                    get.ordinal(i), " simulation is NULL after ",
                                    look.back.i.sims.for.parameters,
                                    " attempt(s). The parameters have been saved in the global environment as 'errored.params'"))
                    }
                    else
                    {
                        lik.pieces = ctrl$likelihood$compute.piecewise(sim, use.optimized.get=T)
                        .GlobalEnv$errored.likelihood <- ctrl$likelihood
                        .GlobalEnv$errored.sim <- sim
                        .GlobalEnv$errored.params <- mcmc.settings$start.values
                        
                        stop(paste0("The likelihood evaluates to -Inf on the initial parameter values for the ", 
                                    get.ordinal(i), " simulation after ",
                                    look.back.i.sims.for.parameters,
                                    " attempt(s). The likelihood components are:\n",
                                    paste0(paste0(" - ", names(lik.pieces), " = ", lik.pieces), collapse='\n'),
                                    "\nThe parameters, simulation, and likelihood have been saved in the global environment as 'errored.params', 'errored.sim', and 'errored.likelihood'"))
                    }
                }
                
                # Run the MCMC
                mcmc = bayesian.simulations::run.mcmc(control = mcmc.settings$ctrl,
                                                      n.iter = n.iter.for.i,
                                                      starting.values = mcmc.settings$start.values,
                                                      cache.frequency = NA,
                                                      update.detail = 'none',
                                                      update.frequency = NA)
                #update.frequency = ifelse(verbose, 50, NA))
                
                if (verbose)
                {
                    total.seconds = (as.numeric(Sys.time())-as.numeric(start.time))
                    if (total.seconds > 60)
                    {
                        total.minutes = total.seconds / 60
                        if (total.minutes > 60)
                        {
                            total.hours = floor(total.minutes/60)
                            total.minutes.remaining = floor(total.minutes - 60*total.hours)
                            total.text = paste0(total.hours,
                                                ifelse(total.hours==1, " hour", " hours"),
                                                ", ",
                                                total.minutes.remaining,
                                                ifelse(total.minutes.remaining==1, " minute", " minutes"),
                                                " elapsed")
                        }
                        else
                        {
                            total.minutes = floor(total.minutes)
                            total.seconds.remaining = floor(total.seconds - 60*total.minutes)
                            total.text = paste0(total.minutes,
                                                ifelse(total.minutes==1, " minute", " minutes"),
                                                ", ",
                                                total.seconds.remaining,
                                                ifelse(total.seconds.remaining==1, " second", " seconds"),
                                                " elapsed")
                        }
                    }
                    else
                    {
                        total.text = paste0(round(total.seconds, 1), " seconds elapsed")
                    }
                    
                    
                    seconds.per.sim = total.seconds / i.index
                    if (seconds.per.sim > 60)
                    {
                        minutes.per.sim = floor(seconds.per.sim/60)
                        remaining.seconds.per.sim = floor(seconds.per.sim - 60*minutes.per.sim)
                        per.sim.text = paste0(minutes.per.sim, 
                                              ifelse(minutes.per.sim==1, " minute", " minutes"),
                                              ", ",
                                              remaining.seconds.per.sim,
                                              ifelse(remaining.seconds.per.sim==1, " second", " seconds"),
                                              " per simulation")
                    }
                    else
                    {
                        per.sim.text = paste0(round(seconds.per.sim, 1), " seconds per simulation")
                    }
                    
                    print(paste0("   DONE. ",
                                 total.text, ". (", per.sim.text, ")"))
                    
                }
                
                mcmc.settings = update.transmute.mcmc.settings(mcmc = mcmc,
                                               mcmc.settings = mcmc.settings,
                                               n.iter = ctrl$n.iter.subsequent.sims)
                
                chunk.sims = c(chunk.sims, list(mcmc@simulations[[length(mcmc@simulations)]]))
            }
            
            chunk.simset = join.simulation.sets(chunk.sims)
            
            # Save the results
            
            chunk.file = get.transmute.calibration.chunk.files(
                to.version = ctrl$to.version,
                location = ctrl$location,
                transmute.code = ctrl$code,
                n.sim = ctrl$n.sim,
                chunks = chunk,
                to.sub.version = ctrl$to.sub.version,
                root.dir = root.dir)
            
            save(chunk.simset, file = chunk.file)
            
            
            mcmc.settings.file = get.transmute.calibration.chunk.mcmc.settings.files(
                to.version = ctrl$to.version,
                location = ctrl$location,
                transmute.code = ctrl$code,
                n.sim = ctrl$n.sim,
                chunks = chunk,
                to.sub.version = ctrl$to.sub.version,
                root.dir = root.dir)
            
            save(mcmc.settings, file = chunk.file)
        #     
        # },
        # error = function(e){
        #     
        #     if (ignore.errors)
        #     {
        #         print(paste0("There was an error fitting a transmuted chunk ", chunk, " for ", location, ":"))
        #         print(e$message)
        #         print(paste0("Skipping chunk ", chunk, " and moving on to the next one"))
        #     }
        #     else
        #         stop(e)
        # })
    }
    
    invisible(mcmc)
}

# Internal helper
update.transmute.mcmc.settings <- function(mcmc,
                                           mcmc.settings,
                                           n.iter,
                                           track.mcmc = F)
{
    new.ctrl = bayesian.simulations::create.adaptive.blockwise.metropolis.control(
        var.names = mcmc.settings$ctrl@var.names,
        simulation.function = mcmc.settings$ctrl@simulation.function,
        log.prior.distribution = mcmc.settings$ctrl@log.prior.distribution,
        log.likelihood = mcmc.settings$ctrl@log.likelihood,
        burn = ifelse(track.mcmc, 0, n.iter - 1),
        thin = mcmc.settings$ctrl@thin,
        var.blocks = mcmc.settings$ctrl@var.blocks,
        reset.adaptive.scaling.update.after = mcmc.settings$ctrl@reset.adaptive.scaling.update.after,
        transformations = mcmc.settings$ctrl@transformations,
        
        initial.covariance.mat = mcmc@chain.states[[1]]@cov.mat,
        initial.scaling.parameters = lapply(mcmc@chain.states[[1]]@log.scaling.parameters, exp),
        
        target.acceptance.probability = mcmc.settings$ctrl@target.acceptance.probability,
        
        n.iter.before.use.adaptive.covariance = 0,
        adaptive.covariance.base.update = mcmc.settings$ctrl@adaptive.covariance.base.update,
        adaptive.covariance.update.prior.iter = mcmc.settings$ctrl@adaptive.covariance.update.prior.iter + ceiling(mcmc@n.iter / 50),
        adaptive.covariance.update.decay = mcmc.settings$ctrl@adaptive.covariance.update.decay,
        adaptive.scaling = mcmc.settings$ctrl@adaptive.scaling,
        adaptive.scaling.base.update = mcmc.settings$ctrl@adaptive.scaling.base.update,
        adaptive.scaling.update.prior.iter= mcmc.settings$ctrl@adaptive.scaling.update.prior.iter + ceiling(mcmc@n.iter / 50),
        adaptive.scaling.update.decay = mcmc.settings$ctrl@adaptive.scaling.update.decay
    )
    
    mcmc.settings$ctrl = new.ctrl
    mcmc.settings$start.values = mcmc@chain.states[[1]]@current.parameters
    mcmc.settings$n.iter = mcmc.settings$n.iter + mcmc@n.iter
    
    mcmc.settings
}

# Internal helper
get.most.advanced.transmute.chunk.mcmc.settings <- function(ctrl,
                                                            root.dir)
{
    mcmc.settings.files = get.transmute.calibration.chunk.mcmc.settings.files(
        to.version = ctrl$to.version,
        location = ctrl$location,
        transmute.code = ctrl$code,
        n.sim = ctrl$n.sim,
        chunks = 1:ctrl$n.chunks,
        to.sub.version = ctrl$to.sub.version,
        root.dir = root.dir)

    mcmc.settings.files = mcmc.settings.files[sapply(mcmc.settings.files, file.exists)]
    
    if (length(mcmc.settings.files)==0)
        ctrl$default.mcmc.settings
    else
    {
        most.advanced.settings = NULL
        most.advanced.n.iter = -Inf
        
        for (file in mcmc.settings.files)
        {
            load(file)
            if (mcmc.settings$n.iter > .most.advanced.n.iter)
            {
                most.advanced.settings = mcmc.settings
                most.advanced.n.iter = mcmc.settings$n.iter
            }
        }
        
        most.advanced.settings
    }
}

#'@export
assemble.transmuted.simulations <- function(transmute.code,
                                            location,
                                            chunks = NULL)
{
    #-- LOAD UP EACH CHUNK --#
    
    #-- JOIN THE SIMULATIONS --#
    
    #-- RERUN THE SIMULATIONS --#
}

#'@export
register.transmute.calibration.info <- function(transmute.code,
                                                from.version,
                                                to.version,
                                                n.iter.first.sim,
                                                n.iter.subsequent.sims,
                                                likelihood.instructions,
                                                prior.distribution,
                                                sampling.blocks,
                                                fixed.initial.parameter.values = numeric(),
                                                from.sub.version = NULL,
                                                to.sub.version = NULL,
                                                max.lookback.attempts = 20)
{   
    # code
    validate.calibration.code(code = transmute.code,
                              error.prefix = "Cannot register transmute calibration: ",
                              code.name.for.error = 'transmute.code')
    
    error.prefix = paste0("Cannot register transmute calibration '", transmute.code, "': ")
    
    
    if (!is.character(from.version) || length(from.version)!=1 || is.na(from.version))
        stop("Cannot set.up.calibration: from.version must be a single, non-NA character value")
    
    if (!is.character(to.version) || length(to.version)!=1 || is.na(to.version))
        stop("Cannot set.up.calibration: to.version must be a single, non-NA character value")
    
    
    if (!is.null(to.sub.version) &&
        (!is.character(to.sub.version) || length(to.sub.version)!=1 || is.na(to.sub.version)))
        stop(paste0(error.prefix, "'to.sub.version' must be either NULL or a single, non-NA character value"))
    
    if (!is.null(from.sub.version) &&
        (!is.character(from.sub.version) || length(from.sub.version)!=1 || is.na(from.sub.version)))
        stop(paste0(error.prefix, "'from.sub.version' must be either NULL or a single, non-NA character value"))
    
    if (!is.numeric(fixed.initial.parameter.values) || any(is.na(fixed.initial.parameter.values)))
        stop(paste0(error.prefix, "'fixed.initial.parameter.values' must be a numeric vector with no NA values"))
    
    if (length(fixed.initial.parameter.values) > 0 &&
        (is.null(names(fixed.initial.parameter.values)) || any(is.na(names(fixed.initial.parameter.values)))))
        stop(paste0(error.prefix, "'fixed.initial.parameter.values' must be a NAMED numeric vector (with no NA names)"))
    
    missing.from.sampling.blocks = setdiff(prior.distribution@var.names, unlist(sampling.blocks))
    if (length(missing.from.sampling.blocks)>0)
    {
        stop(paste0("The following variable",
                    ifelse(length(missing.from.sampling.blocks)==1, ' is', 's are'),
                    " present in the prior.distribution but ",
                    ifelse(length(missing.from.sampling.blocks)==1, 'is', 'are'),
                    " ABSENT from all sampling.blocks: ",
                    collapse.with.and("'", missing.from.sampling.blocks, "'")))
    }
    
    calibration.info = list(
        code = transmute.code,
        from.version = from.version,
        to.version = to.version,
        from.sub.version = from.sub.version,
        to.sub.version = to.sub.version,
        n.iter.first.sim = n.iter.first.sim,
        n.iter.subsequent.sims = n.iter.subsequent.sims,
        likelihood.instructions = likelihood.instructions,
        prior.distribution = prior.distribution,
        sampling.blocks = sampling.blocks,
        max.lookback.attempts = max.lookback.attempts,
        fixed.initial.parameter.values = fixed.initial.parameter.values
    )
    
    CALIBRATION.MANAGER$transmute.info[[transmute.code]] = calibration.info
    
    invisible(NULL)
}

get.transmute.calibration.info <- function(code, throw.error.if.missing=T, error.prefix='')
{
    rv = CALIBRATION.MANAGER$transmute.info[[code]]
    if (is.null(rv) && throw.error.if.missing)
        stop(paste0(error.prefix, "No transmute.calibration info has been registered under code '", code, "'"))
    
    rv
}
