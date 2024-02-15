


JHEEM.SIMSET.COLLECTION = R6::R6Class(
    'jheem.simset.collection',
    
    
    public = list(
        
        initialize = function(version,
                              sub.version,
                              calibration.code,
                              n.sim,
                              locations,
                              intervention.codes)
        {
            
        },
        
        run = function(start.year,
                       end.year,
                       keep.from.year = NULL,
                       keep.to.year = end.year,
                       overwrite.prior = F,
                       verbose = F,
                       stop.for.errors = F)
        {
            stop("The run() method for jheem.simset.collections should be implemented at the subclass level")
        },
        
        # This function should have the same arguments as sim$get, plus stop.for.errors
        get = function(outcomes,
                       output = c('value', 'numerator', 'denominator')[[1]],
                       keep.dimensions=NULL,
                       dimension.values = list(),
                       ...,
                       check.consistency = T,
                       drop.single.outcome.dimension = T,
                       drop.single.sim.dimension = F, # BE CAREFUL WITH THE NEW SUMMARY CODE!
                       replace.inf.values.with.zero = T,
                       summary.type = c('individual.simulation', 'mean.and.interval', 'median.and.interval')[1],
                       interval.coverage = 0.95,
                       error.prefix = "Error getting simulation results: ",
                       verbose = F,
                       stop.for.errors = F)
        {
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop("Cannot get() from simset.collection - 'error.prefix' must be a single, non-NA character value")
            
            inner.dim.names = NULL
            list.rv = lapply(private$i.intervention.codes, function(int.code){
                lapply(private$i.locations, function(loc){
                    
                    simset = private$do.get.simset(location = loc,
                                                   intervention.code = int.code,
                                                   verbose = verbose,
                                                   verbose.prefix = " ",
                                                   stop.for.errors = stop.for.errors)
                    
                    if (is.null(simset))
                        NULL
                    else if (simset$n.sim != self$n.sim)
                        stop(paste0(error.prefix, "The simset for location"))
                    else
                    {
                        sub.rv = simset$get(outcomes = outcomes,
                                            output = output,
                                            keep.dimensions = keep.dimensions,
                                            dimension.values = dimension.values,
                                            ...,
                                            check.consistency = check.consistency,
                                            drop.single.outcome.dimension = drop.single.outcome.dimension,
                                            drop.single.sim.dimension = drop.single.sim.dimension, # BE CAREFUL WITH THE NEW SUMMARY CODE!
                                            replace.inf.values.with.zero = replace.inf.values.with.zero,
                                            summary.type = summary.type,
                                            interval.coverage = interval.coverage,
                                            error.prefix = error.prefix)
                        
                        if (is.null(inner.dim.names))
                            inner.dim.names = dimnames(sub.rv)
                        else if (!dim.names.equal(inner.dim.names, dimnames(sub.rv)))
                            stop(paste0(error.prefix, "The dimnames for the results of get() on the simset for location ",
                                        loc,
                                        ifelse(is.null(int.code), "", 
                                               paste0(" and intervention '", int.code, "'")),
                                        " do not match the dimnames for results of get() on preceding simsets"))
                        
                        sub.rv
                    }
                })
            })
            
            if (is.null(inner.dim.names)) # we didn't get a single successful pull
                stop(paste0(error.prefix, "No simset in the collection had any data available"))
            
            # Fold it into a numeric object, filling in NA's for NULL
            n.inner = prod(sapply(inner.dim.names, length))
            
            rv = sapply(list.rv, function(list.rv.for.int){
                list.rv.for.int[sapply(list.rv.for.int, is.null)] = rep(as.numeric(NA), n.inner)
                list.rv.for.int
            })
            
            # Set dimnames and return
            dim.names = list(inner.dim.names,
                             location = private$i.locations,
                             intervention = private$i.intervention.code.labels)
            
            dim(rv) = sapply(dim.names, length)
            dimnames(rv) = dim.names
            rv
        }
    ),
    
    active = list(
        
        n.sim = function(value)
        {
            
        },
        
        version = function(value)
        {
            
        }
    ),
    
    private = list(
        
        i.version = NULL,
        i.sub.version = NULL,
        i.calibration.code = NULL,
        i.locations = NULL,
        i.intervention.codes = NULL,
        i.intervention.code.labels = NULL,
        i.n.sim = NULL,
        
        do.get.simset = function(location, intervention.code, verbose, verbose.prefix, stop.for.errors)
        {
            stop("The do.get.simset() method for jheem.simset.collections should be implemented at the subclass level")
        }
    )
)

JHEEM.FILE.BASED.SIMSET.COLLECTION = R6::R6Class(
    'jheem.file.based.simset.collection',
    inherit = JHEEM.SIMSET.COLLECTION,
    
    public = list(
        
        initialize = function(version,
                              sub.version = NULL,
                              calibration.code,
                              locations,
                              intervention.codes,
                              n.sim = NULL,
                              root.dir = get.jheem.root.directory("Cannot create file-based simset collection: "))
        {
            # Check root.dir
            if (!is.character(root.dir) || length(root.dir)!=1 || is.na(root.dir))
                stop("Cannot create file-based simset collection: 'root.dir' must be a single, non-NA character value")
            if (!dir.exists(root.dir))
                stop(paste0("Cannot create file-based simset collection: '", root.dir, "' does not exist"))
            
            # Figure out n.sim if NULL
            if (is.null(n.sim))
            {
                n.sim = infer.stored.simset.n.sim(version = version,
                                                  sub.version = sub.version,
                                                  calibration.code = calibration.code,
                                                  root.dir = root.dir,
                                                  error.prefix = "Cannot create file-based simset collection")
                
                n.sim = n.sim[!is.na(n.sim)]
                
                error.infix = paste0("prior simulations for version '",
                                     version, "'",
                                     ifelse(is.null(sub.version), "",
                                            paste0(", sub.version '", sub.version, "'")),
                                     ifelse(is.null(calibration.code), "",
                                            paste0(", calibration.code '", calibration.code, )))
                if (length(n.sim)==0)
                    stop(paste0("Cannot create file-based simset collection: no ", error.infix, 
                                " have been saved, so we cannot infer n.sim. You must explicitly specify 'n.sim' if you want to create this simset collection"))
                else if (length(n.sim)>1)
                    stop(paste0("Cannot create file-based simset collection: ", error.infix, 
                                " have been run with multiple numbers of simulations (",
                                collapse.with.and(n.sim),
                                " simulations), so we cannot infer n.sim. You must explicitly specify 'n.sim' if you want to create this simset collection"))
            }
            
            super$initialize(version = version,
                             sub.version = sub.version,
                             calibration.code = calibration.code,
                             locations = locations,
                             intervention.codes = intervention.codes,
                             n.sim = n.sim)
            
            # store root dir
            private$i.root.dir = root.dir
        },
        
        run = function(start.year,
                       end.year,
                       keep.from.year = start.year,
                       keep.to.year = end.year,
                       overwrite.prior = F,
                       verbose = F,
                       stop.for.errors = F)
        {
            for (i in 1:length(private$i.locations))
            {
                loc = private$i.locations[i]
                
                if (overwrite.prior)
                    need.to.do = rep(T, length(private$i.intervention.codes))
                else
                {
                    need.to.do = !sapply(private$i.intervention.codes, function(int.code){
                        file = private$do.get.simset.file(location = loc,
                                                          intervention.code = int.code,
                                                          error.prefix = 'Cannot find file for simset in collection: ')
                        
                        file.exists(file)
                    })
                }
                
                if (any(need.to.do))
                {
                    if (verbose)
                        cat("Running ", length(private$i.intervention.codes), " interventions for location '",
                            loc, "' (", i, "/", length(private$i.locations),
                            " locations):\n", sep='')
                    
                    # Load the baseline simset
                    simset = private$do.get.simset(location = loc,
                                                   intervention.code = NULL,
                                                   verbose = verbose,
                                                   verbose.prefix = ' - ',
                                                   stop.for.errors = stop.for.errors)
                      
                    if (is.null(simset))
                    {
                        verbose = T
                    }
                    else
                    {
                        for (j in 1:sum(need.to.do))
                        {
                            int.code = private$i.intervention.codes[need.to.do][j]
                            
                            if (verbose)
                                cat(" - Running intervention '", int.code, 
                                    "' (", j, "/", sum(need.to.do),
                                    " interventions)...", sep='')
                            
                            int = get.intervention.from.code(code = int.code,
                                                             throw.error.if.missing = T)
                            
                            if (!is.null(int))
                            {
                                new.simset = int$run(simset, 
                                                     start.year = start.year,
                                                     end.year = end.year,
                                                     keep.from.year = keep.from.year,
                                                     keep.to.year = keep.to.year)
                                
                                save.simulation.set(new.simset, root.dir = private$i.root.dir)
                            }
                            
                            if (verbose)
                                cat("Done\n")
                        }
                    }
                }
                else
                {
                    if (verbose)
                        cat("Skipping ", 
                            loc, "' (", i, "/", length(private$i.locations),
                            " - we have previously run all interventions and don't need to redo\n",)
                }
            }
            
            if (verbose)
                cat("All Done")
        },
    ),
    
    active = list(
        
    ),
    
    private = list(
        
        i.root.dir = NULL,
        
        do.get.simset = function(location, intervention.code, verbose, verbose.prefix, stop.for.errors)
        {
            file = private$do.get.simset.file(location = location,
                                              intervention.code = intervention.code,
                                              error.prefix = 'Cannot load simset in collection: ')
            
            if (!file.exists(file))
            {
                if (stop.for.errors)
                {
                    if (verbose)
                        cat("\n")
                    
                    stop(paste0("Cannot load simset in collection: no simset has been saved at ", file))
                }
                else
                    NULL
            }
            else
            {
                if (verbose)
                    cat(verbose.prefix, "Loading ", 
                        tail(strsplit(file, "/")[[1]],1),
                        '...', sep='')
                
                x = load(file)
                get(x)
                
                if (verbose)
                    cat("Done\n")
            }
        },
        
        do.get.simset.file = function(location, intervention.code, error.prefix)
        {
            get.simset.filename(version = private$i.version,
                                sub.version = private$i.sub.version,
                                calibration.code = private$i.calibration.code,
                                n.sim = private$i.n.sim,
                                location = location,
                                intervention.code = intervention.code,
                                include.path = T,
                                root.dir = private$i.root.dir,
                                error.prefix = error.prefix)
        }
        
    )
)