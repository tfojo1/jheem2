JHEEM.SIMULATION.CODE.ITERATION = '2.0'

##----------------------##
##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##
##----------------------##

#'@name Get Simulation Data
#'
#'@param outcomes A character vector with one or more outcomes for which to pull data. Must be a subset of sim$outcomes
#'@param keep.dimensions Which dimensions should be retained in the resulting array
#'@param dimension.values A set of dimension values according to which to subset the data (will be combined with ...)
#'@param ... The parameters in ... are interpreted as additional dimension values according to which to subset the data. Each must be named, and either a character, integer, or logical vector
#'@param check.consistency Whether to check the consistency of arguments. Setting to FALSE will yield faster performance but unintelligible error messages
#'@param drop.single.outcome.dimensions When set to FALSE, the resulting array will have 'outcome' as its last dimension regardless of how many outcomes we are getting data for. If TRUE, the returned array will omit that dimension when only one outcome is requested
#'@param error.prefix A character value to prepend to any errors generating in getting these data
#'
#'@details Returns an array whose dimensions are keep.dimensions - plus (if there is more than one outcome or drop.single.outcome.dimensions==FALSE) an 'outcome' dimension at the end
#'
#'@export
get.simset.data <- function(simset,
                            outcomes,
                            output = c('value', 'numerator', 'denominator')[[1]],
                            keep.dimensions=NULL,
                            dimension.values = list(),
                            ...,
                            check.consistency = T,
                            drop.single.outcome.dimension = T,
                            error.prefix = "Error getting simulation results: ")
{
    if (!is(simset, "R6") || !is(simset, "jheem.simulation.set"))
        stop("simset must be an R6 object of class 'jheem.simulation.set'") 
    
    simset$get(outcomes = outcomes,
               keep.dimensions = keep.dimensions,
               dimension.values = dimension.values,
               ...,
               check.consistency = check.consistency,
               drop.single.outcome.dimension = drop.single.outcome.dimension,
               error.prefix = error.prefix)
}

#'@name Get a Simulation Metadata Object
#'
#'@param version The version of the model specification (must have been previously registered) for which to get metadata
#'@param location A single character value representing the location for the metadata
#'@param error.prefix A string to prepend to any errors generated in getting the metadata object
#'@param from.year,to.year The years which a corresponding simulation will have data for
#'
#'@details A simulation.metadata object contains metadata, such as the dim.names to which its contents will conform
#'
#'@export
get.simulation.metadata <- function(version, 
                                    location,
                                    from.year = NULL, to.year = NULL,
                                    n.sim = 1,
                                    sub.version = NULL,
                                    error.prefix = paste0("Error deriving the simulation-metadata for '", version, "' and location '", location, "': "))
{
    SIMULATION.METADATA$new(version=version,
                            sub.version=sub.version,
                            location=location,
                            from.year=from.year,
                            to.year=to.year,
                            n.sim = n.sim,
                            error.prefix=error.prefix)
}


##-----------------------------------------------------------##
##-----------------------------------------------------------##
##-- INTERNAL (to the package) WRAPPERS OF THE CONSTRUCTOR --##
##-----------------------------------------------------------##
##-----------------------------------------------------------##

create.single.simulation <- function(version,
                                     sub.version,
                                     location,
                                     outcome.numerators, # no sim dimension
                                     outcome.denominators, # no sim dimension
                                     parameters,
                                     from.year,
                                     to.year,
                                     intervention.code,
                                     calibration.code,
                                     outcome.location.mapping,
                                     solver.metadata,
                                     run.metadata,
                                     finalize,
                                     is.degenerate)
{
    outcome.numerators.with.sim.dimension = lapply(outcome.numerators, function(arr) {
        new.dimnames = c(dimnames(arr), sim=1)
        array(arr, dim=sapply(new.dimnames, length), new.dimnames)
    })
    outcome.denominators.with.sim.dimension = lapply(outcome.denominators, function(arr) {
        if (is.null(arr)) return(arr)
        new.dimnames = c(dimnames(arr), sim=1)
        array(arr, dim=sapply(new.dimnames, length), new.dimnames)
    })
    parameters = matrix(parameters, ncol=1, dimnames=list(parameter=names(parameters), sim=NULL)) # used to say "parameters.indexed.by.sim = list('1'=parameters)" but I don't think I need the character number there. When simsets are joined, the index is meaningless anyways
    
    JHEEM.SIMULATION.SET$new(version = version,
                             sub.version = sub.version,
                             location = location,
                             outcome.numerators = outcome.numerators.with.sim.dimension,
                             outcome.denominators = outcome.denominators.with.sim.dimension,
                             parameters = parameters,
                             from.year = from.year,
                             to.year = to.year,
                             n.sim = 1,
                             intervention.code = intervention.code,
                             calibration.code = calibration.code,
                             outcome.location.mapping = outcome.location.mapping,
                             solver.metadata = solver.metadata,
                             run.metadata = run.metadata,
                             finalize = finalize,
                             is.degenerate = is.degenerate)
}

derive.degenerate.simulation <- function(sim)
{
    outcome.numerators = lapply(sim$data$outcome.numerators, function(num){
        num[] = as.numeric(NA)
        num
    })
    
    outcome.denominators = lapply(sim$data$outcome.numerators, function(denom){
        if (!is.null(denom))
            denom[] = as.numeric(NA)
        denom
    })
    
    create.single.simulation(version = sim$version,
                             sub.version = sim$sub.version,
                             location = sim$location,
                             outcome.numerators = outcome.numerators,
                             outcome.denominators = outcome.denominators,
                             parameters = sim$parameters,
                             from.year = sim$from.year,
                             to.year = sim$to.year,
                             intervention.code = sim$intervention.code,
                             calibration.code = sim$calibration.code,
                             outcome.location.mapping = sim$outcome.location.mapping,
                             solver.metadata = sim$solver.metadata,
                             run.metadata = sim$run.metadata,
                             finalize = sim$is.finalized,
                             is.degenerate = T)
}

join.simulation.sets <- function(..., finalize=T, run.metadata=NULL)
{
    # Validate
    # each argument must be either a simset or a list of simsets
    # browser()
    error.prefix = "Error joining simulation sets: "
    simset.list = unlist(list(...), recursive = F)
    
    if (any(sapply(simset.list, function(element) {!R6::is.R6(element) || !is(element, 'jheem.simulation.set')})))
        stop(paste0(error.prefix, "arguments must all be either 'jheem.simulation.set' objects or lists containing only 'jheem.simulation.set' objects"))
    
    # all simsets should have the same metadata. This will require implementing an equals() method in simulation metadata class
    
    new.n.sim = sum(sapply(simset.list, function(simset) {simset$n.sim}))

    sample.simset = simset.list[[1]]
    outcomes = sample.simset$outcomes
    
    outcome.dimnames = lapply(sample.simset$outcome.ontologies, function(outcome.ontology) {
        dim.names = outcome.ontology
        dim.names[['sim']] = 1:new.n.sim
        dim.names
    })
    
    combined.outcome.numerators = lapply(outcomes, function(outcome) {
        data.vec = sapply(simset.list, function(simset) {simset$data$outcome.numerators[[outcome]]})
        if (any(sapply(data.vec, is.null))) return(NULL)
        array(data.vec, sapply(outcome.dimnames[[outcome]], length), outcome.dimnames[[outcome]])
    })
    names(combined.outcome.numerators) = outcomes
    
    combined.outcome.denominators = lapply(outcomes, function(outcome) {
        data.vec = sapply(simset.list, function(simset) {simset$data$outcome.denominators[[outcome]]})
        if (any(sapply(data.vec, is.null))) return(NULL)
        array(data.vec, sapply(outcome.dimnames[[outcome]], length), outcome.dimnames[[outcome]])
    })
    names(combined.outcome.denominators) = outcomes
    
    combined.is.degenerate = unlist(sapply(simset.list, function(sim){
        sim$is.degenerate
    }))

    #combined.parameters = unlist(lapply(simset.list, function(simset) {simset$data$parameters}), recursive=F)
    
    combined.parameters = sapply(simset.list, function(simset){simset$parameters})
    dimnames(combined.parameters) = list(parameter = dimnames(simset.list[[1]]$parameters)[[1]],
                                         sim = NULL)
    
    if (is.null(run.metadata))
        run.metadata = join.run.metadata(lapply(simset.list, function(sim){sim$run.metadata}))
    
    intervention.code = sample.simset$intervention.code
    
    JHEEM.SIMULATION.SET$new(version = sample.simset$version,
                             sub.version = sample.simset$sub.version,
                             location = sample.simset$location,
                             outcome.numerators = combined.outcome.numerators,
                             outcome.denominators = combined.outcome.denominators,
                             parameters = combined.parameters,
                             from.year = sample.simset$from.year,
                             to.year = sample.simset$to.year,
                             n.sim = new.n.sim,
                             intervention.code = intervention.code,
                             calibration.code = sample.simset$calibration.code,
                             outcome.location.mapping = sample.simset$outcome.location.mapping,
                             solver.metadata = sample.simset$solver.metadata,
                             run.metadata = run.metadata,
                             finalize = finalize,
                             is.degenerate = combined.is.degenerate)
}

'[.jheem.simulation.set' <- function(obj, x) {
    rv = obj$subset(x)
}

##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##


SIMULATION.METADATA = R6::R6Class(
    'simulation.metadata',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              sub.version,
                              location,
                              from.year = NULL,
                              to.year = NULL,
                              n.sim = 1,
                              type = "Simulation Metadata",
                              years.can.be.missing = T,
                              error.prefix)
        {
            #-- Call the superclass constructor --#
            super$initialize(version = version,
                             sub.version = sub.version,
                             location = location,
                             type = type,
                             error.prefix = error.prefix)
            
            # Validate from.year, to.year
            if (!is.logical(years.can.be.missing) || length(years.can.be.missing)!=1 || is.na(years.can.be.missing))
                stop(paste0(error.prefix, "'years.can.be.missing' must be a single, non-NA logical value"))
            
            if (is.null(from.year))
            {
                if (!years.can.be.missing)
                    stop(paste0(error.prefix, "'from.year' and 'to.year' must both be specified (ie, non-NULL)"))
                
                if (!is.null(to.year))
                    stop(paste0(error.prefix, "'to.year' cannot be set if 'from.year' is NULL"))
            }
            else
            {
                if (is.null(to.year))
                {
                    if (years.can.be.missing)
                        stop(paste0(error.prefix, "'from.year' cannot be set if 'to.year' is NULL"))
                    else
                        stop(paste0(error.prefix, "'from.year' and 'to.year' must both be specified (ie, non-NULL)"))
                }
                
                if(!is.numeric(from.year) || length(from.year)!=1 || is.na(from.year))
                    stop(paste0(error.prefix, "'from.year' must be a single, non-NA numeric value"))
                
                if(!is.numeric(to.year) || length(to.year)!=1 || is.na(to.year))
                    stop(paste0(error.prefix, "'to.year' must be a single, non-NA numeric value"))
                
                if (from.year > to.year)
                    stop(paste0(error.prefix, "'from.year' (", from.year, ") must be BEFORE 'to.year' (", to.year, ")"))
            }
            
            private$i.metadata = list(from.year = from.year,
                                      to.year = to.year)
            
            # n.sim must be an integer greater than 0.
            if (!is.numeric(n.sim) || length(n.sim)!=1 || is.na(n.sim) || n.sim%%1!=0)
                stop(paste0(error.prefix, "'n.sim' must be a whole number greater than zero"))
            private$i.n.sim = n.sim
            
            # Pull outcome ontologies and metadata from the specification
            
            specification = get.compiled.specification.for.version(version)
            specification.metadata = self$specification.metadata
            
            private$i.metadata$outcome.ontologies = list()
            private$i.metadata$outcome.metadata = list()
            
            for (outcome.name in specification$get.outcome.names.for.sub.version(private$i.sub.version))
            {
                outcome = specification$get.outcome(outcome.name)
                if (outcome$save)
                {
                    if (outcome$is.cumulative && !is.null(private$i.metadata$from.year) && !is.null(private$i.metadata$to.year))
                    {
                        from.year = max(private$i.metadata$from.year, outcome$from.year)
                        to.year = min(private$i.metadata$to.year, outcome$to.year)
                        
                        if (to.year>=from.year)
                            years.for.ont = as.character(from.year:to.year)
                        else
                            years.for.ont = character()
                    }
                    else if (outcome$is.intrinsic && !is.null(private$i.metadata$from.year) && !is.null(private$i.metadata$to.year))
                    {
                        from.year = max(private$i.metadata$from.year, outcome$from.year)
                        to.year = min(private$i.metadata$to.year+1, outcome$to.year)
                        
                        if (to.year>=from.year)
                            years.for.ont = as.character(from.year:to.year)
                        else
                            years.for.ont = character()
                    }
                    else
                        years.for.ont = NULL
                    
                    ont = c(ontology(year=years.for.ont, incomplete.dimensions = 'year'),
                            specification.metadata$apply.aliases(outcome$ontology, error.prefix = error.prefix))
                    private$i.metadata$outcome.ontologies[[outcome$name]] = ont
                    
                    private$i.metadata$outcome.metadata[[outcome$name]] = MODEL.OUTCOME.METADATA$new(outcome.metadata = outcome$metadata,
                                                                                                     is.cumulative = outcome$is.cumulative,
                                                                                                     is.intrinsic = outcome$is.intrinsic,
                                                                                                     corresponding.observed.outcome = outcome$corresponding.data.outcome)
                }
            }
            
            # Pull in the outcome location mapping
            if (is.null(outcome.location.mapping))
                private$i.metadata$outcome.location.mapping = outcome.location.mapping = NULL
            else if (!is(outcome.location.mapping, 'outcome.location.mapping'))
                stop(paste0(error.prefix, "'outcome.location.mapping' must be an object of class 'outcome.location.mapping'"))
            else
                private$i.metadata$outcome.location.mapping = outcome.location.mapping
                
        },
        # Returns the dimnames that the results of a call to simulation$get will have
        # It's arguments mirror simulation$get
        get.dim.names = function(outcomes,
                                 keep.dimensions=NULL, # will always include sim
                                 dimension.values = list(),
                                 ...,
                                 check.consistency = T,
                                 drop.single.outcome.dimension = T,
                                 drop.single.sim.dimension = F,
                                 error.prefix = "Error getting dimnames of simulation results: ")
        {
            dimension.values = private$process.dimension.values(dimension.values, ..., check.consistency=check.consistency, error.prefix=error.prefix)
            
            # Validate outcomes
            if (!is.character(outcomes) || length(outcomes)==0 || any(is.na(outcomes)))
                stop(paste0(error.prefix, "'outcomes' must be a non-empty character vector with no NA values"))
            invalid.outcomes = setdiff(outcomes, self$outcomes)
            if (length(invalid.outcomes)>0)
                stop(paste0(error.prefix, "Invalid ",
                            ifelse(length(invalid.outcomes)==1, 'outcome: ', 'outcomes: '),
                            collapse.with.and("'", invalid.outcomes, "'"),
                            ifelse(length(invalid.outcomes)==1, ' is', ' are'),
                            " not defined in the '", self$version, "' model specification"))
            
            
            # Pull the ontologies
            ontologies = lapply(outcomes, function(outcome){
                
                ont = self$outcome.ontologies[[outcome]]
                if (is.null(keep.dimensions))
                    keep.dimensions <<- intersect(names(ont),
                                                  union(names(ont)[!is.complete(ont)],
                                                        names(dimension.values)[sapply(dimension.values, length)>1]))
                if (check.consistency)
                {
                    # Make sure keep dimensions work
                    invalid.keep.dimensions = setdiff(keep.dimensions, names(ont))
                    if (length(invalid.keep.dimensions)>0)
                        stop(paste0(error.prefix, "For the '", outcome, 
                                    "' outcome, ",
                                    ifelse(length(invalid.keep.dimensions)==1, "dimension ", "dimensions "),
                                    collapse.with.and("'", invalid.keep.dimensions, "'"),
                                    ifelse(length(invalid.keep.dimensions)==1, " is requested as a keep.dimension, but is", " are requested as keep.dimensions, but are"),
                                    " not present in the ontology"))
                    
                    # Make sure dimension.values dimensions work
                    invalid.dimension.value.dimensions = setdiff(names(dimension.values), names(ont))
                    if (length(invalid.dimension.value.dimensions)>0)
                        stop(paste0(error.prefix, "For the '", outcome, 
                                    "' outcome, ",
                                    ifelse(length(invalid.dimension.value.dimensions)==1, "dimension ", "dimensions "),
                                    collapse.with.and("'", invalid.dimension.value.dimensions, "'"),
                                    ifelse(length(invalid.dimension.value.dimensions)==1, " is specified in dimension.values, but is", " are specified in dimension.values, but are"),
                                    " not present in the ontology"))
                    
                    # If any of the ontologies dimensions are NULL, they must have dimension.values set
                    null.dimensions.in.ontology = names(ont)[sapply(ont, is.null)]
                    missing.dimension.values = setdiff(null.dimensions.in.ontology, names(dimension.values))
                    if (length(missing.dimension.values)>0)
                        stop(paste0(error.prefix,
                                    "For the '", outcome, "' outcome, '",
                                    ifelse(length(missing.dimension.values)==1, "dimension ", "dimensions "),
                                    collapse.with.and("'", missing.dimension.values, "'"),
                                    " must have dimension.values specified (",
                                    ifelse(length(missing.dimension.values)==1, "it is", "they are"),
                                    " NULL in the ontology and must be specified in the get() call)"
                        ))
                    
                    
                    dimension.values = resolve.ontology.dimension.values(ont = ont,
                                                                         dimension.values = dimension.values,
                                                                         error.prefix = error.prefix)
                    ont[names(dimension.values)] = dimension.values
                    
                    
                    # The below should be rendered unnecessary by the resolve.ontology.dimension.values above
                    # HOWEVER - do we need to check that years is within to/from for complete outcomes? if years is NULL?
                    # Make sure dimension.values work
                    #                    for (d in names(dimension.values))
                    #                    {
                    #                        if (is.null(ont[[d]]))
                    #                        {
                    #                            # do we need to check that years is within to/from for complete outcomes?
                    #                        }
                    #                        else
                    #                        {
                    #                            invalid.dimension.values = setdiff(dimension.values[[d]], ont[[d]])
                    #                            if (length(invalid.dimension.values)>0)
                    #                                stop(paste0(error.prefix, "For the '", outcome, 
                    #                                            "' outcome, ",
                    #                                            collapse.with.and("'", invalid.dimension.values, "'"),
                    #                                            ifelse(length(invalid.dimension.value.dimensions)==1, " is an invalid value", " are invalid values"),
                    #                                            " for the '", d, "' dimension of the ontology",
                    #                                            ifelse(length(ont[[d]])<=6, 
                    #                                                   paste0(" (", paste0("'", ont[[d]], "'", collapse=", "), ")"),
                    #                                                   "")
                    #                                ))
                    #                        }
                    #                    }
                    
                }
                
                for (d in names(dimension.values))
                {
                    if (is.null(ont[[d]]))
                        ont[[d]] = dimension.values[[d]]
                }
                ont = ont[keep.dimensions]
                keep.dimension.values = dimension.values[intersect(keep.dimensions, names(dimension.values))]
                if (length(keep.dimension.values)>0 && !check.consistency)
                    ont[names(keep.dimension.values)] = resolve.ontology.dimension.values(ont, dimension.values=keep.dimension.values, error.prefix = error.prefix)
                
                ont
            })
            names(ontologies) = outcomes
            
            # Make sure the ontologies are congruous
            ont1 = as.list(ontologies[[1]])
            if (check.consistency)
            {
                outcome1 = outcomes[1]
                for (outcome2 in outcomes[-1])
                {
                    ont2 = as.list(ontologies[[2]])
                    
                    unequal.dimensions.mask = sapply(names(ont1), function(d){
                        !identical(ont1[[d]], ont2[[d]])
                    })
                    
                    if (any(unequal.dimensions.mask))
                    {
                        unequal.dimensions = names(ont1)[unequal.dimensions.mask]
                        stop(paste0(error.prefix, "Cannot get data for outcomes '", outcome1, "' and '", outcome2, 
                                    "' in one function call. Their ontologies have differing values for the ",
                                    collapse.with.and("'", unequal.dimensions, "'"),
                                    ifelse(length(unequal.dimensions)==1, " dimension.", " dimensions.")))
                    }
                }
            }
            # if (debug) browser()
            # Fold together, with outcome and sim as the last dimensions
            # ont1 = ontologies[[1]]
            if (!drop.single.sim.dimension || self$n.sim > 1)
                ont1 = c(ont1, list(sim=1:self$n.sim))
                # return(c(ont1, ontology(sim=1:self$n.sim, incomplete.dimensions = 'sim')))
            if (!drop.single.outcome.dimension || length(outcomes)>1)
                ont1 = c(ont1, list(outcome=outcomes))
            
            return (ont1)
        },
        
        prepare.optimized.get.instructions = function(outcomes,
                                                      keep.dimensions=NULL, # will always include sim
                                                      dimension.values = list(),
                                                      output = c('value', 'numerator', 'denominator')[[1]],
                                                      check.consistency = T,
                                                      drop.single.outcome.dimension = T,
                                                      drop.single.sim.dimension = F,
                                                      replace.inf.values.with.zero = T,
                                                      error.prefix = "Error preparing optimized get info: ")
        {
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop("Cannot prepare.optimized.get.instructions() - error.prefix must a single, non-NA character value")
            
            OPTIMIZED.GET.INSTRUCTIONS$new(
                sim.metadata = self,
                outcomes = outcomes,
                keep.dimensions = keep.dimensions,
                dimension.values = dimension.values,
                output = output,
                check.consistency = check.consistency,
                drop.single.outcome.dimension = drop.single.outcome.dimension,
                drop.single.sim.dimension = drop.single.sim.dimension,
                replace.inf.values.with.zero = replace.inf.values.with.zero,
                error.prefix = error.prefix
            )
        }
    ),
    
    active = list(
        metadata = function(value)
        {
            if (missing(value))
                private$i.metadata
            else
                stop("Cannot modify a simulation.metadata's 'metadata' - it is read-only")
        },
        
        outcomes = function(value)
        {
            if (missing(value))
                names(private$i.metadata$outcome.ontologies)
            else
                stop("Cannot modify a simulation.metadata's 'outcomes' - they are read-only")
        },
        
        outcome.ontologies = function(value)
        {
            if (missing(value))
                private$i.metadata$outcome.ontologies
            else
                stop("Cannot modify a simulation.metadata's 'outcome.ontologies' - they are read-only")
        },
        
        outcome.metadata = function(value)
        {
            if (missing(value))
                private$i.metadata$outcome.metadata
            else
                stop("Cannot modify a simulation.metadata's 'outcome.metadata' - it is read-only")
        },
        
        from.year = function(value)
        {
            if (missing(value))
                private$i.metadata$from.year
            else
                stop("Cannot modify a simulation.metadata's 'from.year' - it is read-only")
        },
        
        to.year = function(value)
        {
            if (missing(value))
                private$i.metadata$to.year
            else
                stop("Cannot modify a simulation.metadata's 'to.year' - it is read-only")
        },
        
        outcome.location.mapping = function(value)
        {
            if (missing(value))
            {
                if (is.null(private$i.metadata$outcome.location.mapping))
                    create.default.outcome.location.mapping(version = private$i.version,
                                                            location = private$i.location,
                                                            sub.version = private$i.sub.version)
                else
                    private$i.metadata$outcome.location.mapping
            }
            else
                stop("Cannot modify a simulation.metadata's 'outcome.location.mapping' - it is read-only")
        },
        
        n.sim = function(value)
        {
            if (missing(value))
                private$i.n.sim
            else
                stop("Cannot modify a simulation.set's 'n.sim' - it is read-only")
        }
    ),
    
    private = list(
        
        i.metadata = NULL,
        i.n.sim = NULL, # Now all simsets with this metadata must have the same n.sim!
        
        get.current.code.iteration = function()
        {
            JHEEM.SIMULATION.CODE.ITERATION
        },
        
        process.dimension.values = function(dimension.values, ..., check.consistency, error.prefix)
        {
            # Validate dimension values (and fold in ...)
            dot.dot.dot = list(...)
            
            if (check.consistency) {
                check.dimension.values.valid(dot.dot.dot,
                                             variable.name.for.error = "The elements of ...",
                                             error.prefix = error.prefix,
                                             allow.empty = T)
                
                check.dimension.values.valid(dimension.values,
                                             variable.name.for.error = "dimension.values",
                                             error.prefix = error.prefix,
                                             allow.empty = T)
            }
            
            dimension.values[names(dot.dot.dot)] = dot.dot.dot
            
            if (any(names(dimension.values)=='year'))
                dimension.values$year = as.character(dimension.values$year)
            
            dimension.values
        }
    )
    
)

OPTIMIZED.GET.INSTRUCTIONS = R6::R6Class(
    'jheem.simulation.optimized.get.instructions',
    
    public = list(
        
        initialize = function(sim.metadata,
                              outcomes,
                              keep.dimensions=NULL, # will always include sim
                              dimension.values = list(),
                              output = c('value', 'numerator', 'denominator')[[1]],
                              check.consistency = T,
                              drop.single.outcome.dimension = T,
                              drop.single.sim.dimension = F,
                              replace.inf.values.with.zero = T,
                              error.prefix = "Error preparing optimized get info: ")
        {
            # Set up the value dim.names
            private$i.value.dim.names = value.dim.names = sim.metadata$get.dim.names(
                outcomes = outcomes,
                keep.dimensions = keep.dimensions,
                dimension.values = dimension.values,
                check.consistency = check.consistency,
                drop.single.outcome.dimension = F,
                drop.single.sim.dimension = F,
                error.prefix = error.prefix
            )
            
            if (drop.single.outcome.dimension && length(value.dim.names$outcome)==1)
                private$i.value.dim.names = private$i.value.dim.names[setdiff(names(private$i.value.dim.names), 'outcome')]
            
            if (drop.single.sim.dimension && length(value.dim.names$sim)==1)
                private$i.value.dim.names = private$i.value.dim.names[setdiff(names(private$i.value.dim.names), 'sim')]
            
            # Set up the years
            raw.years = dimension.values$year
            if (is.null(raw.years))
                stop(paste0(error.prefix, "'dimension.values' MUST contain values for 'year'"))
            private$i.target.years = as.numeric(raw.years)
            if (any(is.na(private$i.target.years)))
                stop(paste0(error.prefix, "'dimension.values$year' MUST only contain character representations of numeric values"))
            private$i.min.target.year = min(private$i.target.years)
            private$i.max.target.year = max(private$i.target.years)
            
            if (is.null(private$i.value.dim.names$year))
                stop(paste0(error.prefix, "'keep.dimensions' MUST include 'year'"))
            
            private$i.n.sim = sim.metadata$n.sim
            
            private$i.outcomes = outcomes
            private$i.n.per.outcome = prod(sapply(private$i.value.dim.names, length)) / length(outcomes)
            
            
            if (check.consistency && (!is.character(output) || length(output) != 1 || !(output %in% c('value', 'numerator', 'denominator'))))
                stop(paste0(error.prefix, "'output' must be one of 'value', 'numerator', or 'denominator'"))
            private$i.output = output
            
            if (!is.logical(replace.inf.values.with.zero) || length(replace.inf.values.with.zero)!=1 || is.na(replace.inf.values.with.zero))
                stop(paste0(error.prefix, "'replace.inf.values.with.zero', must be a single, non-NA, logical value"))
            private$i.replace.inf.values.with.zero = replace.inf.values.with.zero
            
            private$i.info.by.outcome = lapply(private$i.outcomes, function(outcome){
                
                # Make sure output is appropriate to the outcome
                outcome.metadata = sim.metadata$outcome.metadata[[outcome]]
                if (output=='denominator' && 
                    (outcome.metadata$scale=='non.negative.number' || outcome.metadata$scale=='number'))
                {
                    stop(paste0(error.prefix,
                                "output is set to 'denominator', but the outcome '",
                                outcome, "' does not carry a denominator"))
                }
                
                # Set up the outcome ontology
                outcome.ontology = sim.metadata$outcome.ontologies[[outcome]]
                outcome.ontology$year = as.character(private$i.target.years)
                outcome.ontology$sim = as.character(1:private$i.n.sim)
                
                # Figure out which dimensions come before/after the year dimension
                year.dimension.index = (1:length(outcome.ontology))[names(outcome.ontology)=='year']
                before.year.mask = (1:length(outcome.ontology)) < year.dimension.index
                after.year.mask = (1:length(outcome.ontology)) > year.dimension.index
                
                # Set up the subset of the ontology from which we will draw values
                draw.from.dim.names = intersect.joined.dim.names(outcome.ontology, dimension.values)
                
                # Get indices from the ontology to the draw-from subset
                outcome.to.draw.from.indices = get.array.access.indices(arr.dim.names = outcome.ontology,
                                                                        dimension.values = draw.from.dim.names,
                                                                        index.from = 1)
                
                # Get indices into the dim.names (just for this outcome)
                outcome.subset.of.dim.names = value.dim.names
                outcome.subset.of.dim.names$outcome = outcome
                
                outcome.subset.to.dim.names = get.array.access.indices(arr.dim.names = private$i.value.dim.names,
                                                                       dimension.values = outcome.subset.of.dim.names,
                                                                       index.from = 0)
                
                draw.from.dim.names$outcome = outcome
                draw.from.to.outcome.subset = get.expand.array.indices(to.expand.dim.names = outcome.subset.of.dim.names,
                                                                       target.dim.names = draw.from.dim.names,
                                                                       index.from = 1)
                
                draw.from.to.dim.names.indices = outcome.subset.to.dim.names[draw.from.to.outcome.subset]
                
                
                list(
                    outcome = outcome,
                    n.before.year.dimension = prod(as.numeric(sapply(outcome.ontology[before.year.mask], length))),
                    n.after.year.dimension = prod(as.numeric(sapply(outcome.ontology[after.year.mask], length))),
                    to.indices = draw.from.to.dim.names.indices,
                    raw.from.indices = outcome.to.draw.from.indices,
                    result.indices = outcome.subset.to.dim.names,
                    pull.numerator.only = output=='numerator' || (output=='value' && (outcome.metadata$scale=='non.negative.number' || outcome.metadata$scale=='number')),
                    pull.denominator.only = output=='denominator',
                    pull.numerator.denominator.ratio = output=='value' && outcome.metadata$scale!='non.negative.number' && outcome.metadata$scale!='number'
                )
            })
            names(private$i.info.by.outcome) = private$i.outcomes
        },
        
        do.get = function(outcome.numerators, 
                          outcome.denominators,
                          n.sim,
                          sim.from.year,
                          sim.to.year,
                          error.prefix)
        {
            # Make sure n.sim is the same in sim and optimized.info
            if (n.sim != private$i.n.sim)
                stop(paste0(error.prefix, "The optimized.get.instructions was prepared for ", 
                            private$i.n.sim, ifelse(private$i.n.sim==1, ' simulation', " simulations"),
                            ", but the simulation.set contains ",
                            ifelse(n.sim < private$i.n.sim, "only ", ""),
                            n.sim, ifelse(n.sim==1, " simulation", " simulations")))
            
            # Make sure years can accomodate
            if (sim.from.year > private$i.min.target.year || sim.to.year < private$i.max.target.year)
                stop(paste0(error.prefix, "In order to execute this optimized get, the simulation must span at least from ",
                            private$i.min.target.year, " to ", private$i.max.target.year,
                            ", but it only spans from ", sim.from.year, " to ", sim.to.year, "."))
            
            # If we need to, re-index by year
            if (is.null(private$i.cached.from.year) || private$i.cached.from.year!=sim.from.year || private$i.cached.to.year!=sim.to.year)
            {
                private$i.info.by.outcome = lapply(private$i.info.by.outcome, function(info){
                    
                    year.indices = get_year_indices_for_optimized_info(outcome_years = as.numeric(dimnames(outcome.numerators[[info$outcome]])$year),
                                                                       target_years = private$i.target.years,
                                                                       n_before_year_dimension = info$n.before.year.dimension,
                                                                       n_after_year_dimension = info$n.after.year.dimension)
                    
                    if (is.null(year.indices))
                        stop(paste0(error.prefix, "There was an error in the cpp function to get year indices for outcome '", info$outcome, "'"))
                    
                    info$from.indices = year.indices[info$raw.from.indices]
                    
                    info
                })
            }
            
            # Do the get
            rv = do_optimized_get(numerators = outcome.numerators[private$i.outcomes],
                                  denominators = outcome.denominators[private$i.outcomes],
                                  info_by_outcome = private$i.info.by.outcome,
                                  n_to_per_outcome = private$i.n.per.outcome,
                                  avoid_infinite = private$i.replace.inf.values.with.zero)
            
            # Set dimnames and return
            dim(rv) = sapply(private$i.value.dim.names, length)
            dimnames(rv) = private$i.value.dim.names
            rv
        }
    ),
    
    active = list(
        
    ),
    
    private = list(
        
        i.outcomes = NULL,
        i.output = NULL,
        i.replace.inf.values.with.zero = NULL,
        
        i.value.dim.names = NULL,
        i.target.years = NULL,
        i.info.by.outcome = NULL,
        
        i.n.per.outcome = NULL,
        
        i.min.target.year = NULL,
        i.max.target.year = NULL,
        i.cached.from.year = NULL,
        i.cached.to.year = NULL,
        
        i.n.sim = NULL
    )
)

JHEEM.SIMULATION.SET = R6::R6Class(
    'jheem.simulation.set',
    inherit = SIMULATION.METADATA,
    lock_object = F,
    portable = F,
    
    public = list(
        initialize = function(version, # needs a subversion
                              sub.version,
                              location,
                              outcome.numerators, # now must have sim dimension
                              outcome.denominators, # now must have sim dimension
                              parameters,
                              from.year,
                              to.year,
                              n.sim,
                              solver.metadata,
                              run.metadata,
                              intervention.code,
                              calibration.code,
                              outcome.location.mapping,
                              is.degenerate = NULL,
                              finalize, #a logical - should we add sampled parameters?
                              error.prefix = "Error constructing simulation")
        {
            #-- Call the superclass constructor --#
            super$initialize(version = version,
                             sub.version = sub.version,
                             location = location,
                             type = "Simulation",
                             years.can.be.missing = F,
                             from.year = from.year,
                             to.year = to.year,
                             n.sim = n.sim,
                             error.prefix = error.prefix)
            
            #-- Check engine if it is not NULL --#
            
            # We have decided never to store an engine - going to be dangerous if saving
          #  private$i.engine = engine
            
            # I have not yet written the validation code
            
            #-- Validate outcome data --#
            
            # Make sure all expected outcomes are present
            # - Numerators for all
            # - Denominators unless the type is number or non.negative.number
            # Make sure they are numeric arrays with dimensions that match the ontology
            # browser()
            # if (!setequal(names(outcome.numerators), self$outcomes))
            #     stop(paste0(error.prefix, "'outcome.numerators' must have an array for each outcome expected for this version and location"))
            # if (!setequal(names(outcome.denominators) != self$outcomes))
            #     stop(paste0(error.prefix, "'outcome.denominators' must have an array for each outcome expected for this version and location"))
            # if (any(sapply(outcome.numerators, function(arr) {!is.numeric(arr) || !is.array(array())})))
            #     stop(paste0(error.prefix, "'outcome.numerators' must contain only numeric arrays"))
            # if (any(sapply(outcome.denominators, function(arr) {!is.numeric(arr) || !is.array(arr)})))
            #     stop(paste0(error.prefix, "'outcome.denominators' must contain only numeric arrays"))
            # 
            # if (any(sapply(names(outcome.numerators), function(outcome) {
            #     any(sapply(names(dim(outcome.numerators[[outcome]])), function(d) {
            #         !setequal(dimnames(outcome.numerators[[outcome]])[[d]], self$outcome.ontologies[[outcome]][[d]]) # will years mess this up?
            #     }))
            # })))
            #     stop(paste0(error.prefix, "each array in 'outcome.numerators' must have dimensions matching its outcome's ontology"))
            # 
            # if (any(sapply(names(outcome.denominators), function(outcome) {
            #     any(sapply(names(dim(outcome.denominators[[outcome]])), function(d) {
            #         !setequal(dimnames(outcome.denominators[[outcome]])[[d]], self$outcome.ontologies[[outcome]][[d]])
            #     }))
            # })))
            #     stop(paste0(error.prefix, "each array in 'outcome.denominators' must have dimensions matching its outcome's ontology"))
            
            #-- Validate parameters --#
            
            # should be a matrix with one row for each sim
            # dimnames(parameters)[[1]] must be set
            if (!is.matrix(parameters) || !is.numeric(parameters))
                stop(paste0(error.prefix, "'parameters' must be a numeric matrix"))
            if (any(is.na(parameters)))
                stop(paste0(error.prefix, "'parameters' cannot contain any NA values"))
            if (is.null(dimnames(parameters)) || is.null(dimnames(parameters)[[1]]) || any(is.na(dimnames(parameters)[[1]])) || any(nchar(dimnames(parameters)[[1]])==0))
                stop(paste0(error.prefix, "'parameters' must have dimnames set for its rows (dimension 1), and those names cannot be NA or empty strings"))
            tabled.parameter.names = table(dimnames(parameters)[[1]])
            if (any(tabled.parameter.names>1))
                stop(paste0(error.prefix, "the parameter names, as given by dimnames(parameters)[[1]], must be UNIQUE. ",
                            ifelse(sum(tabled.parameter.names>1)==1, "The parameter ", "Parameters "),
                            collapse.with.and("'", names(tabled.parameter.names[tabled.parameter.names>1]), "'"),
                            ifelse(sum(tabled.parameter.names>1)==1, " appears", " appear"),
                            " more than once."))
            
            #-- If we are going to finalize: --#
            #   - Add a random seed to base future numbers on
            #   - Generate sampled parameters
            
            if (finalize)
            {
                private$i.seed = round(runif(1, 0, .Machine$integer.max))
                
                sampled.parameters.distribution = get.parameters.distribution.for.version(version, type='sampled')
                
                
                if (!is.null(sampled.parameters.distribution))
                {
                    parameter.names = dimnames(parameters)[[1]]
                    sampled.parameters.to.generate = setdiff(sampled.parameters.distribution@var.names,
                                                             parameter.names)
                    
                    if (length(sampled.parameters.to.generate)>0)
                    {
                        reset.seed = round(runif(1, 0, .Machine$integer.max))
                        set.seed(self$seed)
                        
                        new.parameters = generate.random.samples(sampled.parameters.to.generate, n=n.sim)
                        set.seed(reset.seed) # this keeps our code from always setting to the same seed  
                        
                        parameters = rbind(t(new.parameters[,sampled.parameters.to.generate,drop=F]))
                    }
                }
            }
            
        
            # let's standardize the dimnames here
            dimnames(parameters) = list(parameter = dimnames(parameters)[[1]],
                                        sim = as.character(1:n.sim))
            
            #-- Update the outcome metadata's years for each of the non-cumulative outcomes --#
            
            # Validate run.metadata
            if (!is(run.metadata, 'jheem.run.metadata'))
                stop(paste0(error.prefix, "'run.metadata' must be an object of class 'jheem.run.metadata'"))
            
            # Validate solver.metadata
            if (!is(solver.metadata, 'solver.metadata'))
                stop(paste0(error.prefix, "'solver.metadata' must be an object of class 'solver.metadata'"))
            
            # Validate intervention.code
            if (!is.null(intervention.code))
            {
                if (!is.character(intervention.code) || length(intervention.code)!=1 || is.na(intervention.code))
                    stop(paste0(error.prefix, "'intervention.code' must be a single, non-NA character value"))
            }

            # Validate calibration.code
            if (!is.null(calibration.code))
            {
                if (!is.character(calibration.code) || length(calibration.code)!=1 || is.na(calibration.code))
                    stop(paste0(error.prefix, "'calibration.code' must be a single, non-NA character value"))
            }
            
            #-- Store data --#
            private$i.data = list(outcome.numerators = outcome.numerators,
                                  outcome.denominators = outcome.denominators,
                                  parameters = parameters)
    
            private$i.solver.metadata = solver.metadata        
            private$i.run.metadata = run.metadata
            private$i.intervention.code = intervention.code
            private$i.calibration.code = calibration.code
            
            private$i.is.degenerate = is.degenerate
            private$i.finalized = finalize
            
            #-- Update the ontologies for non-cumulative, non-intrinsic outcomes --#
            for (outcome.name in self$outcomes)
            {
                metadata = private$i.metadata$outcome.metadata[[outcome.name]]
                if (!metadata$is.cumulative && !metadata$is.intrinsic)
                {
                    private$i.metadata$outcome.ontologies[[outcome.name]]$year = dimnames(outcome.numerators[[outcome.name]])$year
                }
            }
            
            #-- Make Active Bindings with the Names of Outcomes --#
            outcomes.to.bind = setdiff(self$outcomes, names(self))
            lapply(outcomes.to.bind, function(outcome.name){
                
                fn = eval(parse(text=paste0("function(value){private$eval.outcome.active.binding(value, outcome.name='",outcome.name,"')}")))
                environment(fn) = self
                
                makeActiveBinding(sym = outcome.name,
                                  fun = fn,
                                  env = self)
            })
            
            lockEnvironment(self)
        },
        
        print = function(...)
        {
            base::print(paste0(ifelse(private$i.n.sim==1, 
                                      'A single JHEEM simulation', 
                                      paste0("A set of ", private$i.n.sim, " JHEEM simulations")),
                               ", from ", self$from.year, " to ", self$to.year, 
                               ", for model version '", private$i.version, "' and location '", private$i.location, "'",
                               ifelse(is.null(self$intervention.code), '',
                                      paste0(" with intervention '", self$intervention.code, "'"))))
        },
        
        optimized.get = function(optimized.get.instructions,
                                 error.prefix = "Error in executing optimized.get(): ")
        {
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop(paste0("Cannot execute optimized.get() - 'error.prefix' must be a single, non-NA character vector"))
            
            if (!is(optimized.get.instructions, "jheem.simulation.optimized.get.instructions"))
                stop(paste0(error.prefix, "'optimized.get.instructions' must be an object of class 'jheem.simulation.optimized.get.instructions', created by prepare.optimized.get.instructions()"))
            
            optimized.get.instructions$do.get(outcome.numerators = private$i.data$outcome.numerators,
                                              outcome.denominators = private$i.data$outcome.denominators,
                                              n.sim = private$i.n.sim,
                                              sim.from.year = self$from.year,
                                              sim.to.year = self$to.year,
                                              error.prefix = error.prefix)
        },
        
        # NB: these arguments all need to be duplication in JHEEM_simset_collection.R's get() method
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
                       debug=F)
        {
            if (debug) browser()
            if (check.consistency && (!is.character(output) || length(output) != 1 || !(output %in% c('value', 'numerator', 'denominator'))))
                stop(paste0(error.prefix, "'output' must be one of 'value', 'numerator', or 'denominator'"))
            
            # keep.dimensions will be the union of the incomplete dimensions in the outcome ontology and any dimension value dimensions
            if (is.null(keep.dimensions)) {
                incomplete.dimensions = unique(unlist(lapply(outcomes, function(outcome) {incomplete.dimensions(self$outcome.ontologies[[outcome]])}))) # unshared incompletes will catch error below
                keep.dimensions = union(incomplete.dimensions, names(dimension.values))
            }
            
            dim.names = self$get.dim.names(outcomes = outcomes,
                                           keep.dimensions = keep.dimensions,
                                           dimension.values = dimension.values,
                                           ...,
                                           check.consistency = check.consistency,
                                           drop.single.outcome.dimension = drop.single.outcome.dimension,
                                           drop.single.sim.dimension = drop.single.sim.dimension,
                                           error.prefix = error.prefix)
            dimension.values = private$slowerFoo(dimension.values, ..., check.consistency = check.consistency, error.prefix=error.prefix)
            # dimension.values = private$process.dimension.values(dimension.values, ..., error.prefix=error.prefix)
            # if (drop.single.sim.dimension && self$n.sim==1)
            #     keep.dimensions = names(dim.names)
            # else
            #     keep.dimensions = names(dim.names)[-length(dim.names)]
            # 
            # if (!drop.single.sim.dimension || self$n.sim > 1)
            #     keep.dimensions = union(keep.dimensions, 'sim')

            rv = sapply(outcomes, function(outcome){
                slowFoo(outcome, dimension.values, keep.dimensions, check.consistency, output, replace.inf.values.with.zero)
                # scale = self$outcome.metadata[[outcome]]$scale
                # numerator.needed = output %in% c('value', 'numerator')
                # denominator.needed = scale.needs.denominator(scale) && output %in% c('value', 'denominator')
                # 
                # numerator.data = NULL
                # denominator.data = NULL
                # 
                # # noting that some outcomes, like aids.diagnoses, do not have all years
                # years.this.outcome = NULL
                # unused.years.this.outcome = NULL
                # dimension.values.this.outcome = dimension.values
                # 
                # if (numerator.needed)
                #     numerator.data = private$i.data$outcome.numerators[[outcome]]
                # if (denominator.needed) {
                #     denominator.data = private$i.data$outcome.denominators[[outcome]]
                #     if (check.consistency && is.null(denominator.data))
                #         stop(paste0(error.prefix, "outcome '", outcome, "' missing denominator data"))
                # }
                # if (check.consistency && output == 'denominator' && !scale.needs.denominator(scale))
                #      stop(paste0(error.prefix, "outcome '", outcome, "' does not use a denominator"))
                # 
                # # check that this outcome has the years we want before subset
                # if (numerator.needed) years.this.outcome = dimnames(numerator.data)$year
                # else if (denominator.needed) years.this.outcome = dimnames(denominator.data)$year
                # else years.this.outcome = intersect(dimnames(numerator.data)$year, dimnames(denominator.data)$year)
                # if ('year' %in% names(dimension.values.this.outcome)) {
                #     unused.years.this.outcome = setdiff(dimension.values[['year']], years.this.outcome)
                #     dimension.values.this.outcome[['year']] = intersect(dimension.values.this.outcome[['year']], years.this.outcome)
                # }  
                # 
                # if (numerator.needed) numerator.data = array.access(numerator.data, dimension.values.this.outcome)
                # if (denominator.needed) denominator.data = array.access(denominator.data, dimension.values.this.outcome)
                # 
                # # Aggregation
                # if (numerator.needed) pre.agg.dimnames = dimnames(numerator.data)
                # else pre.agg.dimnames = dimnames(denominator.data)
                # 
                # dimensions.to.drop = intersect(which(length(pre.agg.dimnames) == 1), which(!(names(pre.agg.dimnames) %in% keep.dimensions)))
                # 
                # if (length(dimensions.to.drop) > 0) {
                #     pre.agg.dimnames = pre.agg.dimnames[-dimensions.to.drop]
                #     if (numerator.needed) numerator.data = array(numerator.data, dim = sapply(pre.agg.dimnames, length), dimnames = pre.agg.dimnames)
                #     if (denominator.needed) denominator.data = array(denominator.data, dim = sapply(pre.agg.dimnames, length), dimnames = pre.agg.dimnames)
                # }
                # 
                # if (length(pre.agg.dimnames) > length(keep.dimensions)) {
                #     if (numerator.needed) numerator.data = apply(numerator.data, c(keep.dimensions, 'sim'), sum)
                #     if (denominator.needed) denominator.data = apply(denominator.data, c(keep.dimensions, 'sim'), sum)
                # }
                # 
                # if (output == 'numerator' || output == 'value' && !denominator.needed) output.array = numerator.data
                # else if (output == 'denominator') output.array = denominator.data
                # else {
                #     output.array = numerator.data / denominator.data
                #     if (replace.inf.values.with.zero && denominator.needed && output == 'value')
                #         output.array[denominator.data == 0] = 0
                # }
                # 
                # # add NAs for unused years so that this outcome's array can be mixed with the other outcomes' arrays
                # if (length(unused.years.this.outcome)>0) {
                #     dimnames.with.all.years = dimnames(output.array)
                #     dimnames.with.all.years$year = dimension.values$year
                #     output.array.with.all.years = array(NA, sapply(dimnames.with.all.years, length), dimnames.with.all.years)
                #     output.array.with.all.years[get.array.access.indices(dimnames.with.all.years, dimnames(output.array))] = output.array
                #     output.array=output.array.with.all.years
                # }
                # 
                # output.array
            })

            dim(rv) = sapply(dim.names, length)
            dimnames(rv) = dim.names
            if (summary.type == 'mean.and.interval') {
                alpha = (1-interval.coverage)/2
                rv = apply(rv, setdiff(names(dim.names), 'sim'), function(x) {
                    c(mean(x), quantile(x, probs=c(alpha, 1-alpha)))
                })
                new.dim.names = c(list(metric=c('mean', 'lower', 'upper')), dim.names[setdiff(names(dim.names), 'sim')])
                dim(rv) = sapply(new.dim.names, length)
                dimnames(rv) = new.dim.names
            }
            if (summary.type == 'median.and.interval') {
                alpha = (1-interval.coverage)/2
                rv = apply(rv, setdiff(names(dim.names), 'sim'), function(x) {
                    c(median(x), quantile(x, probs=c(alpha, 1-alpha)))
                })
                new.dim.names = c(list(metric=c('mean', 'lower', 'upper')), dim.names[setdiff(names(dim.names), 'sim')])
                dim(rv) = sapply(new.dim.names, length)
                dimnames(rv) = new.dim.names
            }
            rv
        },
        
        subset = function(x)
        {
            error.prefix = "Error subsetting jheem.simulation.set: "
            # 'x' must be either an integer or logical vector with valid length and values
            if ((!is.numeric(x) && !is.logical(x)) || any(is.na(x)) || any(duplicated(x)))
                stop(paste0(error.prefix, "'x' must be a numeric or logical vector with no NAs or repeats"))
            if (is.numeric(x) && (any(x < 1) || any(x > self$n.sim)))
                stop(paste0(error.prefix, "if 'x' is a numeric vector, all values must be integers between 1 and this simulation.set's 'n.sim'"))
            if (is.logical(x) && length(x) != self$n.sim)
                stop(paste0(error.prefix, "if 'x' is a logical vector, it must have length equal to this simulation.set's 'n.sim'"))

            if (is.logical(x)) x = (1:self$n.sim)[x]
            
            new.n.sim = length(x)
            new.outcome.numerators = lapply(private$i.data$outcome.numerators, function(outcome.arr) {
                if (is.null(outcome.arr) || length(outcome.arr) == 0) return(NULL)
                new.arr = array.access(outcome.arr, sim=x, drop=F)
                dimnames(new.arr)[['sim']] = 1:new.n.sim
                new.arr
            })
            new.outcome.denominators = lapply(private$i.data$outcome.denominators, function(outcome.arr) {
                if (is.null(outcome.arr) || length(outcome.arr) == 0) return(NULL)
                new.arr = array.access(outcome.arr, sim=x, drop=F)
                dimnames(new.arr)[['sim']] = 1:new.n.sim
                new.arr
            })
            new.parameters = private$i.data$parameters[,x,drop=F]
            
            JHEEM.SIMULATION.SET$new(version = self$version,
                                     location = self$location,
                                     sub.version = self$sub.version,
                                     outcome.numerators = new.outcome.numerators,
                                     outcome.denominators = new.outcome.denominators,
                                     parameters = new.parameters,
                                     from.year = self$from.year,
                                     to.year = self$to.year,
                                     n.sim = new.n.sim,
                                     calibration.code = private$i.calibration.code,
                                     intervention.code = private$i.intervention.code,
                                     run.metadata = private$i.run.metadata$subset(x),
                                     is.degenerate = private$i.is.degenerate[x],
                                     finalize = private$i.finalized)
        },
        
        # n: keeps every nth (rounding DOWN) sim counting backwards from the last sim
        # keep: may be fraction or a number to keep. Decimal values above 1 are rounded down to nearest whole number.
        thin = function(n=NULL, keep=NULL)
        {
            error.prefix = "Error thinning simulation.set: "
            if (is.null(n) && is.null(keep))
                stop(paste0(error.prefix, "either 'n' or 'keep' must be specified"))
            if (!is.null(n) && !is.null(keep))
                stop(paste0(error.prefix, "exactly one of 'n' or 'keep' must be specified"))
            if (!is.null(n) && (!is.numeric(n) || length(n) != 1 || n > self$n.sim || n < 1))
                stop(paste0(error.prefix, "'n' must be a single integer value between 1 and 'n.sim'"))
            if (!is.null(keep) && (!is.numeric(keep) || length(keep) != 1 || keep > self$n.sim || keep <= 0))
                stop(paste0(error.prefix, "'keep' must be either a single integer value between 1 and 'n.sim' or a fraction between 0 and 1"))
            
            if (is.null(keep)) keep = ceiling(self$n.sim / n)
            if (keep < 1) keep = ceiling(self$n.sim * keep)
            if (is.null(n)) n = self$n.sim / keep
            
            self$subset(ceiling(self$n.sim - n * (0 : (keep - 1))))
            
        },
        
        # keep: may be fraction or a number to keep (to stay consistent with simset$thin arguments)
        burn = function(keep=NULL)
        {
            error.prefix = "Error burning sims from simulation.set: "
            if (!is.numeric(keep) || length(keep) != 1 || keep > self$n.sim || keep <= 0)
                stop(paste0(error.prefix, "'keep' must be either a single integer value between 1 and 'n.sim' or a fraction between 0 and 1"))
            
            if (keep < 1) keep = ceiling(self$n.sim * keep)
            
            self$subset(1 : keep)
            
        },
        
        get.engine = function(start.year = NULL,
                              end.year = NULL,
                              max.run.time.seconds = NULL,
                              keep.from.year = NULL,
                              keep.to.year = NULL,
                              intervention.code = self$intervention.code,
                              error.prefix = "Cannot get JHEEM Engine from simulation set")
        {
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop(paste0("Error in simulation.set$get.engine() - 'error.prefix' must be a single, non-NA character vector"))
            
            # substitute defaults for years if given NULL
            if (is.null(start.year))
                start.year = self$from.year
            
            if (is.null(end.year))
                end.year = self$to.year
            
            if (is.null(keep.from.year))
                keep.from.year = start.year
            
            if (is.null(keep.to.year))
                keep.to.year = end.year
            
            if (!identical(intervention.code, private$i.intervention.code))
            {
                # Check the new intervention code
                new.intervention = get.intervention.from.code(intervention.code, throw.error.if.missing=F)
                if (is.null(new.intervention))
                {
                    stop(paste0("Cannot set intervention.code '", intervention.code, 
                                "' in getting a new engine from the simulation: no intervention with that code has been registered.",
                                ifelse(is.intervention.code.temporary(intervention.code),
                                       paste0("'", intervention.code, "' is a temporary code - it was probably created as a one-off intervention that was not formally saved."),
                                       '')))
                }
                
                # Check against the prior intervention
                if (!is.null(self$intervention.code))
                {
                    prior.intervention = get.intervention.from.code.from.code(self$intervention.code, throw.error.if.missing = F)
                    if (is.null(prior.intervention))
                    {
                        stop(paste0("Cannot get the engine for the simulation set. The simulation ran with intervention.code '", self$intervention.code, 
                                    "' , but no intervention with that code has been registered.",
                                    ifelse(is.intervention.code.temporary(self$intervention.code),
                                           paste0("'", self$intervention.code, "' is a temporary code - it was probably created as a one-off intervention that was not formally saved."),
                                           '')))
                    }
                    
                    if (is.null(intervention.code))
                        intervention.code = self$intervention.code
                    else if (!is.no.intervention(prior.intervention) && !prior.intervention$equals(new.intervention))
                        stop(paste0(error.prefix, "Cannot change the intervention.code to '", intervention.code, 
                                    "' when getting an engine for the simulation - a different intervention ('",
                                    self$intervention.code, "') was already used to run the simulation"))
                }
            }
            
            engine = do.create.jheem.engine(version = private$i.version,
                                            sub.version = private$i.sub.version,
                                            location = private$i.location,
                                            start.year = start.year,
                                            end.year = end.year,
                                            max.run.time.seconds = max.run.time.seconds,
                                            prior.simulation.set = self,
                                            keep.from.year = keep.from.year,
                                            keep.to.year = keep.to.year,
                                            intervention.code = intervention.code,
                                            calibration.code = private$i.calibration.code,
                                            solver.metadata = private$i.solver.metadata,
                                            finalize = T,
                                            error.prefix = error.prefix)
        },
        
        #'@value Returns a NEW simulation set object, run out to end.year
        extend = function(end.year,
                          keep.from.year = self$from.year,
                          keep.to.year = end.year,
                          max.run.time.seconds = NULL)
        {
            if (!is.numeric(end.year) || length(end.year) != 1 || is.na(end.year))
                stop("Cannot extend simulation set: 'end.year' must be a single, non-NA numeric value")
            
            engine = self$get.engine(start.year = self$to.year + 1,
                                    end.year = end.year,
                                    max.run.time.seconds = max.run.time.seconds,
                                    keep.from.year = keep.from.year,
                                    keep.to.year = keep.to.year,
                                    intervention.code = private$i.code,
                                    error.prefix = "Cannot get JHEEM Engine from simulation set")
            
            sim.list = lapply(1:private$i.n.sim, function(i){
                engine$run(parameters = private$i.parameters[,i],
                           prior.sim.index = i)
            })
            
            join.simulation.sets(sim.list, finalize=T)
        },
        
        get.intervention = function()
        {
            if (!is.null(private$i.intervention.code))
            {
                rv = get.intervention.from.code(private$i.intervention.code, throw.error.if.missing=F)
                if (is.null(rv))
                    stop(paste0("The simulation set has a registered intervention code of '",
                                private$i.intervention.code, "' but no intervention has been registered for that code in this R session",
                                ifelse(is.intervention.code.temporary(private$i.intervention.code),
                                       paste0("('", private$i.intervention.code, "' was a temporary code created for a one-off intervention that was not formally registered with a code)"),
                                       "")))
                rv
            }
            else
                NULL
        },
        
        save = function(root.dir = get.jheem.root.directory("Cannot save simulation set: "))
        {
            save.simulation.set(simset = self, root.dir = root.dir)
        },
        # just for diagnostics, will be removed soon
        slowFoo = function(outcome, dimension.values, keep.dimensions, check.consistency, output, replace.inf.values.with.zero) {
            scale = self$outcome.metadata[[outcome]]$scale
            numerator.needed = output %in% c('value', 'numerator')
            denominator.needed = scale.needs.denominator(scale) && output %in% c('value', 'denominator')
            
            numerator.data = NULL
            denominator.data = NULL
            
            # noting that some outcomes, like aids.diagnoses, do not have all years
            years.this.outcome = NULL
            unused.years.this.outcome = NULL
            dimension.values.this.outcome = dimension.values
            
            if (numerator.needed)
                numerator.data = private$i.data$outcome.numerators[[outcome]]
            if (denominator.needed) {
                denominator.data = private$i.data$outcome.denominators[[outcome]]
                if (check.consistency && is.null(denominator.data))
                    stop(paste0(error.prefix, "outcome '", outcome, "' missing denominator data"))
            }
            if (check.consistency && output == 'denominator' && !scale.needs.denominator(scale))
                stop(paste0(error.prefix, "outcome '", outcome, "' does not use a denominator"))
            
            # check that this outcome has the years we want before subset
            if (numerator.needed) years.this.outcome = dimnames(numerator.data)$year
            else if (denominator.needed) years.this.outcome = dimnames(denominator.data)$year
            else years.this.outcome = intersect(dimnames(numerator.data)$year, dimnames(denominator.data)$year)
            if ('year' %in% names(dimension.values.this.outcome)) {
                unused.years.this.outcome = setdiff(dimension.values[['year']], years.this.outcome)
                dimension.values.this.outcome[['year']] = intersect(dimension.values.this.outcome[['year']], years.this.outcome)
            }  
            
            if (numerator.needed) numerator.data = array.access(numerator.data, dimension.values.this.outcome)
            if (denominator.needed) denominator.data = array.access(denominator.data, dimension.values.this.outcome)
            
            # Aggregation
            if (numerator.needed) pre.agg.dimnames = dimnames(numerator.data)
            else pre.agg.dimnames = dimnames(denominator.data)
            
            dimensions.to.drop = intersect(which(length(pre.agg.dimnames) == 1), which(!(names(pre.agg.dimnames) %in% keep.dimensions)))
            
            if (length(dimensions.to.drop) > 0) {
                pre.agg.dimnames = pre.agg.dimnames[-dimensions.to.drop]
                if (numerator.needed) numerator.data = array(numerator.data, dim = sapply(pre.agg.dimnames, length), dimnames = pre.agg.dimnames)
                if (denominator.needed) denominator.data = array(denominator.data, dim = sapply(pre.agg.dimnames, length), dimnames = pre.agg.dimnames)
            }
            
            if (length(pre.agg.dimnames) > length(keep.dimensions)) {
                if (numerator.needed) numerator.data = apply(numerator.data, c(keep.dimensions, 'sim'), sum)
                if (denominator.needed) denominator.data = apply(denominator.data, c(keep.dimensions, 'sim'), sum)
            }
            
            if (output == 'numerator' || output == 'value' && !denominator.needed) output.array = numerator.data
            else if (output == 'denominator') output.array = denominator.data
            else {
                output.array = numerator.data / denominator.data
                if (replace.inf.values.with.zero && denominator.needed && output == 'value')
                    output.array[denominator.data == 0] = 0
            }
            
            # add NAs for unused years so that this outcome's array can be mixed with the other outcomes' arrays
            if (length(unused.years.this.outcome)>0) {
                dimnames.with.all.years = dimnames(output.array)
                dimnames.with.all.years$year = dimension.values$year
                output.array.with.all.years = array(NA, sapply(dimnames.with.all.years, length), dimnames.with.all.years)
                output.array.with.all.years[get.array.access.indices(dimnames.with.all.years, dimnames(output.array))] = output.array
                output.array=output.array.with.all.years
            }
            
            output.array
        }
        
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                'simulation'
            else
                stop("Cannot modify a simulation's 'descriptor' - it is read-only")
        },
        
        parameters = function(value)
        {
            if (missing(value))
                private$i.data$parameters
            else
                stop("Cannot modify a simulation's 'parameters' - they are read-only")
        },
        
        params = function(value)
        {
            if (missing(value))
            {
                if (private$i.n.sim==1)
                    private$i.data$parameters[,1]
                else
                    stop("The 'params' field is only available for single-simulation sets (ie, when nsim==1)")
            }
            else
                stop("Cannot modify a simulation's 'params' - they are read-only")
        },
        
        data = function(value)
        {
            if (missing(value))
                private$i.data
            else
                stop("Cannot modify a simulation.set's 'data' - it is read-only")
        },
        
        solver.metadata = function(value)
        {
            if (missing(value))
                private$i.solver.metadata
            else
                stop("Cannot modify a simulation.set's 'solver.metadata' - it is read-only")
        },
        
        run.metadata = function(value)
        {
            if (missing(value))
                private$i.run.metadata
            else
                stop("Cannot modify a simulation.set's 'run.metadata' - it is read-only")
        },
        
        calibration.code = function(value)
        {
            if (missing(value))
                private$i.calibration.code
            else
                stop("Cannot modify a simulation.set's 'calibration.code' - it is read-only")
        },
        
        intervention.code = function(value)
        {
            if (missing(value))
                private$i.intervention.code
            else
                stop("Cannot modify a simulation.set's 'intervention.code' - it is read-only")
        },
        
        is.degenerate = function(value)
        {
            if (missing(value))
                private$i.is.degenerate
            else
                stop("Cannot modify a simulation.set's 'is.degenerate' - it is read-only")
        },
        
        seed = function(value) #get's a random seed for this simulation set
        {
            if (missing(value))
                private$i.seed
            else
                stop("Cannot modify a simulation.set's 'seed' - it is read-only")
        },
        
        is.finalized = function(value)
        {
            if (missing(value))
                private$i.finalized
            else
                stop("Cannot modify a simulation.set's 'is.finalized' - it is read-only")
        }
    ),
    
    private = list(
        i.data = NULL,
        i.run.metadata = NULL,
        
        # i.numerators = NULL,
        # i.denominators = NULL,
        # i.parameters = NULL,
        
        i.calibration.code = NULL,
        i.intervention.code = NULL,
        
        i.is.degenerate = NULL,
        i.finalized = NULL,
        
        i.solver.metadata = NULL,
        
        i.seed = NULL,
        # just for diagnostics, will be removed soon
        slowerFoo = function(dimension.values, ..., check.consistency, error.prefix) {
            private$process.dimension.values(dimension.values, ..., check.consistency = check.consistency, error.prefix=error.prefix)
        },
        
        eval.outcome.active.binding = function(value, outcome.name)
        {
            if (missing(value))
            {
                if (is.null(private$i.data$outcome.denominators[[outcome.name]]) || is.null(private$i.data$outcome.numerators[[outcome.name]]))
                    private$i.data$outcome.numerators[[outcome.name]]
                else
                {
                    rv = private$i.data$outcome.numerators[[outcome.name]] /
                        private$i.data$outcome.denominators[[outcome.name]]
                    rv[private$i.data$outcome.denominators[[outcome.name]]==0] = 0
                    rv
                }
            }
            else
                stop(paste0("Cannot modify a simulation's '", outcome.name, "' - it is read-only"))
        }
    )
)