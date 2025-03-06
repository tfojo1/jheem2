
#'@title Create a Collection of JHEEM Simulation-Sets
#'
#'@param version,sub.version The version and (optional) sub-version for the simsets
#'@param locations A character vector of locations for which to collect simsets
#'@param interventions The interventions for which to collect interventions. Can be either (a) a character vector of intervention codes or (b) a list of 'jheem.intervention' objects or single character intervention codes
#'@param n.sim Optional - the number of simulations which collected simsets should contain. If NULL, the number will attempt to be figured out
#'@param root.dir The root directory relative to which all simset files (and all JHEEM files) are saved
#'
#'@return An object of class 'jheem.simset.collection'
#'
#'@export
create.simset.collection <- function(version,
                                     calibration.code,
                                     locations,
                                     interventions,
                                     n.sim = NULL,
                                     sub.version = NULL,
                                     root.dir = get.jheem.root.directory("Cannot create simset collection: "))
{
    JHEEM.FILE.BASED.SIMSET.COLLECTION$new(version = version,
                                           sub.version = sub.version,
                                           calibration.code = calibration.code,
                                           locations = locations,
                                           interventions = interventions,
                                           n.sim = n.sim,
                                           root.dir = root.dir)
}

JHEEM.SIMSET.COLLECTION = R6::R6Class(
    'jheem.simset.collection',
    
    
    public = list(
        
        initialize = function(version,
                              sub.version,
                              calibration.code,
                              n.sim,
                              locations,
                              interventions)
        {
            #-- Validate arguments --#
            
            # version
            if (!is.character(version) || length(version)!=1 || is.na(version))
                stop(paste0(error.prefix, "'version' must be a single, non-NA character value"))
            
            # sub-version
            if (is.null(sub.version))
            {}
            else if (!is.character(sub.version) || length(sub.version)!=1 || is.na(sub.version))
                stop(paste0(error.prefix, "'sub.version' must be either NULL or a single, non-NA character value"))
            
            if (!is.character(version) || length(version)!=1 || is.na(version))
                stop(paste0(error.prefix, "'version' must be a single, non-NA character value"))
            
            # calibration.code
            if (is.null(calibration.code))
            {}
            else if (!is.character(calibration.code) || length(calibration.code)!=1 || is.na(calibration.code))
                stop(paste0(error.prefix, "'calibration.code' must be either NULL or a single, non-NA character value"))
            
            # n.sim
            if (!is.numeric(n.sim) || length(n.sim)!=1 || is.na(n.sim) || n.sim<=0 || round(n.sim)!=n.sim)
                stop(paste0(error.prefix, "'n.sim' must be a single, non-NA, positive integer value"))
            
            # locations
            if (!is.character(locations) || length(locations)==0 || any(is.na(locations)))
                stop(paste0(error.prefix, "'locations' must be a non-empty character vector with no NA values"))
            
            # interventions
            if (is.character(interventions))
                interventions = as.list(interventions)
            else
                interventions
            
            if (is.list(interventions))
            {
                if (length(interventions)==0)
                    stop(paste0(error.prefix, "'interventions' cannot be empty"))
                
                intervention.codes = lapply(1:length(interventions), function(i){
                    
                    int = interventions[[i]]
                    
                    if (is(int, 'jheem.intervention'))
                    {
                        code = int$code
                        if (is.intervention.code.temporary(code))
                            stop(paste0(error.prefix, "The given interventions cannot have a temporary code, but the ",
                                        get.ordinal(i), " element of 'interventions' does. Specify a code when registering this intervention to be able to use it in a simset collection"))
                    }
                    
                    if (is.null(int))
                        code = NULL # OK
                    else if (is.character(int))
                    {
                        code = int
                        if (length(code)!=1 || is.na(code))
                            stop(paste0(error.prefix, "When character values are given in 'interventions' they must be length==1 and non-NA"))
                        
                        if (is.intervention.code.temporary(code))
                            stop(paste0(error.prefix, "The given intervention codes cannot be temporary codes, but the ",
                                        get.ordinal(i), " element of 'interventions' ('", code,
                                        "') is. Specify a code when registering this intervention to be able to use it in a simset collection"))
                    }
                    else 
                        stop(paste0(error.prefix, "The elements of 'intervention.codes' must be either NULL, character intervention codes, or jheem.intervention objects"))
                    
                    code
                })
            }
            else
                stop(paste0(error.prefix, "'interventions' must be either a character vector of intervention codes, or a list containing intervention codes and/or jheem.intervention objects"))
            
            #-- Store variables --#
            
            private$i.version = version
            private$i.sub.version = sub.version
            private$i.calibration.code = calibration.code
            private$i.locations = locations
            private$i.intervention.codes = intervention.codes
            private$i.n.sim = n.sim
            
            private$i.intervention.code.labels = intervention.codes
            private$i.intervention.code.labels[sapply(private$i.intervention.code.labels, is.null)] = 'baseline'
            private$i.intervention.code.labels = as.character(unlist(private$i.intervention.code.labels))
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
                       mapping = NULL,
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
                        stop(paste0(error.prefix, "The simset for location ", loc, " and intervention ", int.code, " does not have the expected number of simulations"))
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
                                            mapping = mapping,
                                            error.prefix = error.prefix)
                        
                        this.inner.dim.names = dimnames(sub.rv)
                        if (any(names(this.inner.dim.names)=='location'))
                        {
                            if (length(this.inner.dim.names$location)==1 || this.inner.dim.names$location==loc)
                                this.inner.dim.names = this.inner.dim.names[setdiff(names(this.inner.dim.names), 'location')]
                            else if (length(private$i.locations)>1 && !is.null(inner.dim.names))
                            {
                                if (!length(this.inner.dim.names$location) != length(inner.dim.names$location) ||
                                    !all(this.inner.dim.names$location == inner.dim.names$location))
                                    stop(paste0(error.prefix, "The dimensions for the results of get() produce different 'location' dimensions for different locations in the simset collection, and cannot be collected into one array"))
                            }
                        }
                        
                        if (is.null(inner.dim.names))
                            inner.dim.names <<- this.inner.dim.names
                        else if (!dim.names.equal(inner.dim.names, this.inner.dim.names))
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
                #list.rv.for.int[sapply(list.rv.for.int, is.null)] = rep(as.numeric(NA), n.inner)
                sapply(list.rv.for.int, function(sub){
                    if (is.null(sub))
                        rep(as.numeric(NA), n.inner)
                    else
                        sub
                })
            })
            
            # Set dimnames and return
            dim.names = c(inner.dim.names,
                          list(location = private$i.locations,
                               intervention = private$i.intervention.code.labels))

            dim(rv) = sapply(dim.names, length)
            dimnames(rv) = dim.names
            rv
        },

        get.parameters = function(parameter.names = NULL,
                                  summary.type = c('individual.simulation', 'mean.and.interval', 'median.and.interval')[1],
                                  interval.coverage = 0.95,
                                  error.prefix = "Error getting simulation parameters: ",
                                  verbose = F,
                                  stop.for.errors = F)
        {
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop("Cannot get.parameters() from simset.collection - 'error.prefix' must be a single, non-NA character value")
            
            if (is.null(parameter.names))
                param.names.to.use = character()
            else
                param.names.to.use = parameter.names
            
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
                        stop(paste0(error.prefix, "The simset for location ", loc, " and intervention ", int.code, " does not have the expected number of simulations"))
                    else
                    {
                        if (is.null(parameter.names))
                            param.names.to.use <<- union(param.names.to.use, simset$parameter.names)
                        
                        if (is.null(parameter.names))
                            sub.rv = simset$parameters
                        else
                        {
                            # invalid.param.names = setdiff(parameter.names, simset$parameter.names)
                            # if (length(invalid.param.names)>0)
                            #     stop(paste0(error.prefix,
                            #                 ifelse(length(invalid.param.names)==1, "Parameter ", "Parameters "),
                            #                 collapse.with.and("'", invalid.param.names, "'"),
                            #                 ifelse(length(invalid.param.names)==1, " is", " are"),
                            #                 " not present in the simulation for location '",
                            #                 loc, "' and intervention '", int.code, "'"))
                            sub.rv = simset$parameters[intersect(parameter.names, simset$parameter.names),,drop=F]
                        }
                        
                        sub.rv
                    }
                })
            })

            if (all(sapply(list.rv, function(sub1){
                            all(sapply(sub1, is.null))})))
                stop(paste0(error.prefix, "No simset in the collection had any data available"))
            
            # Fold it into a numeric object, filling in NA's for NULL
            n.inner = length(param.names.to.use)
            
            dummy.parameters = array(as.numeric(NA),
                                     dim=c(parameter=n.inner,
                                           sim=self$n.sim),
                                     dimnames = list(params=param.names.to.use,
                                                     sim=1:self$n.sim))
            rv = sapply(list.rv, function(list.rv.for.int){
#                list.rv.for.int[sapply(list.rv.for.int, is.null)] = rep(as.numeric(NA), n.inner)
                sapply(list.rv.for.int, function(sub){
                    sub.rv = dummy.parameters
                    if (!is.null(sub))
                        sub.rv[dimnames(sub)[[1]],] = sub
                    
                    sub.rv
                })
            })
            
            # Set dimnames and return
            dim.names = list(parameter = param.names.to.use,
                             simulation = 1:self$n.sim,
                             location = private$i.locations,
                             intervention = private$i.intervention.code.labels)
            
            dim(rv) = sapply(dim.names, length)
            dimnames(rv) = dim.names
            
            if (summary.type == 'mean.and.interval' ||
                summary.type == 'median.and.interval')
            {
                alpha = (1-interval.coverage)/2
                rv = apply(rv, c('parameter','location','intervention'), function(val){
                    if (summary.type == 'mean.and.interval')
                        c(mean(val), quantile(val, probs=c(alpha, 1-alpha)))
                    else
                        quantile(val, probs=c(0.5, alpha, 1-alpha))
                })
                
                dim.names = c(list(value=NULL),
                              dim.names[c('parameter','location','intervention')])
                if (summary.type == 'mean.and.interval')
                    dim.names$value = c("mean", "lower", "upper")
                else
                    dim.names$value = c("median", "lower", "upper")
                
                dim(rv) = sapply(dim.names, length)
                dimnames(rv) = dim.names
                
                rv = apply(rv, c('parameter','value','location','intervention'), function(x){x})
            }
            
            rv
        },
        
        print = function(...)
        {
            n = length(private$i.locations) * length(private$i.intervention.codes)
            print(paste0("A simset collection with ", n,
                         ifelse(n==1, " simset (one location and one intervention)",
                                paste0(" simsets (", 
                                       length(private$i.locations),
                                       ifelse(length(private$i.locations)==1, " location", " locations"),
                                       " and ", length(private$i.intervention.codes),
                                       ifelse(length(private$i.intervention.codes)==1, " intervention", " interventions"),
                                       ")"))))
        }
    ),
    
    active = list(
        
        version = function(value)
        {
            if (missing(value))
                private$iversion
            else
                stop("Cannot modify 'version' for a jheem.simset.collection - it is read-only")
        },
        
        sub.version = function(value)
        {
            if (missing(value))
                private$i.sub.version
            else
                stop("Cannot modify 'sub.version' for a jheem.simset.collection - it is read-only")
        },
        
        calibration.code = function(value)
        {
            if (missing(value))
                private$i.calibration.code
            else
                stop("Cannot modify 'calibration.code' for a jheem.simset.collection - it is read-only")
        },
        
        n.sim = function(value)
        {
            if (missing(value))
                private$i.n.sim
            else
                stop("Cannot modify 'n.sim' for a jheem.simset.collection - it is read-only")
        },
        
        locations = function(value)
        {
            if (missing(value))
                private$i.locations
            else
                stop("Cannot modify 'locations' for a jheem.simset.collection - they are read-only")
        },
        
        intervention.codes = function(value)
        {
            if (missing(value))
                private$i.intervention.codes
            else
                stop("Cannot modify 'intervention.codes' for a jheem.simset.collection - it is read-only")
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
                              interventions,
                              n.sim = NULL,
                              root.dir = get.jheem.root.directory("Cannot create file-based simset collection: "))
        {
            # Check root.dir
            if (!is.character(root.dir) || length(root.dir)!=1 || is.na(root.dir))
                stop("Cannot create file-based simset collection: 'root.dir' must be a single, non-NA character value")
            if (!dir.exists(root.dir) && !dir.exists(file.path(root.dir, SIMULATION.SUB.DIRECTORY))) # the second condition here is because dir.exists('Q:') will return
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
                                     ifelse(!is.null(sub.version) && is.null(calibration.code), ",", ""),
                                     ifelse(!is.null(sub.version) && is.null(calibration.code), " and", ""),
                                     ifelse(is.null(sub.version), "",
                                            paste0(" sub.version '", sub.version, "'")),
                                     ifelse(!is.null(sub.version) && !is.null(calibration.code), ",", ""),
                                     ifelse(!is.null(calibration.code), " and", ""),
                                     ifelse(is.null(calibration.code), "",
                                            paste0(" calibration.code '", calibration.code, "'")))
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
                             interventions = interventions,
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
                        cat("Running ", length(private$i.intervention.codes), 
                            ifelse(length(private$i.intervention.codes)==1, " intervention", " interventions"),
                            " for location '",
                            loc, "' (", i, "/", length(private$i.locations),
                            ifelse(length(private$i.locations)==1, " location", " locations"),
                            "):\n", sep='')
                    
                    # Load the baseline simset
                    simset = private$do.get.simset(location = loc,
                                                   intervention.code = NULL,
                                                   verbose = verbose,
                                                   verbose.prefix = ' - ',
                                                   stop.for.errors = stop.for.errors)
                      
                    if (is.null(simset))
                    {
                        if (verbose)
                            cat(" - No baseline simset has been run for location '", loc, "' - skipping all ",
                                sum(need.to.do), " interventions for this location\n", sep='')
                    }
                    else
                    {
                        for (j in 1:sum(need.to.do))
                        {
                            int.code = private$i.intervention.codes[need.to.do][[j]]
                            
                            if (verbose)
                                cat(" - Running intervention '", int.code, 
                                    "' (", j, "/", sum(need.to.do),
                                    " interventions)...", sep='')
                            
                            if (is.null(int.code))
                                int = get.null.intervention()
                            else
                                int = get.intervention.from.code(code = int.code,
                                                                 throw.error.if.missing = T)
                            
                            if (!is.null(int))
                            {
                                if (stop.for.errors)
                                {
                                    new.simset = int$run(simset, 
                                                         start.year = start.year,
                                                         end.year = end.year,
                                                         keep.from.year = keep.from.year,
                                                         keep.to.year = keep.to.year)
                                }
                                else
                                {
                                    new.simset = NULL
                                    tryCatch({
                                        new.simset = int$run(simset, 
                                                             start.year = start.year,
                                                             end.year = end.year,
                                                             keep.from.year = keep.from.year,
                                                             keep.to.year = keep.to.year)
                                    },
                                    error = function(e){
                                        if (verbose)
                                            cat("\n   - There was an error running the intervention: ",
                                                e$message, '\n   - Skipping this intervention\n', sep='')
                                        new.simset = NULL
                                    })
                                }
                                
                                if (!is.null(new.simset))
                                {
                                    save.simulation.set(new.simset, root.dir = private$i.root.dir)
                                    if (verbose)
                                        cat("Done\n")
                                }
                            }
                            else
                            {
                                if (verbose)
                                    cat("\n   - No intervention was registered for code '", int.code, "' - skipping\n")
                            }
                        }
                        
                        if (verbose)
                            cat(" - Done with all ", sum(need.to.do), " interventions for location '", loc, "'\n", sep='')
                            
                    }
                }
                else
                {
                    if (verbose)
                        cat("Skipping ", 
                            loc, "' (", i, "/", length(private$i.locations),
                            ifelse(length(private$i.locations)==1, " location", " locations"),
                            ") - we have previously run all interventions and don't need to redo\n", sep='')
                }
            }
            
            if (verbose)
                cat("All Done\n")
        }
    ),
    
    active = list(
        
        root.dir = function(value)
        {
            if (missing(value))
                private$i.root.dir
            else
                stop("Cannot modify 'root.dir' for a jheem.file.based.simset.collection - it is read-only")
        }
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
                
                if (verbose)
                    cat("Done\n")
                
                get(x)
            }
        },
        
        files.exist = function()
        {
            rv = sapply(private$i.intervention.codes, function(code){
                sapply(private$i.locations, function(loc){
                    
                    file.exists(private$do.get.simset.file(location=loc,
                                                           intervention.code=intervention.code,
                                                           error.prefix=''))
                    
                })
            })
            
            dimnames(rv) = list(location = private$i.locations,
                                intervention = private$i.intervention.codes)
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
