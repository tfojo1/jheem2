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
    if (!is(jheem.engine, "R6") || !is(simset, "jheem.simulation.set"))
        stop("sim must be an R6 object of class 'jheem.simulation.set'") 
    
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
get.simulation.metadata <- function(version, location,
                                    from.year = NULL, to.year = NULL,
                                    error.prefix = paste0("Error deriving the simulation-metadata for '", version, "' and location '", location, "': "))
{
    SIMULATION.METADATA$new(version=version,
                            location=location,
                            from.year=from.year,
                            to.year=to.year,
                            error.prefix=error.prefix)
}


##-----------------------------------------------------------##
##-----------------------------------------------------------##
##-- INTERNAL (to the package) WRAPPERS OF THE CONSTRUCTOR --##
##-----------------------------------------------------------##
##-----------------------------------------------------------##

create.single.simulation <- function(version,
                                     location,
                                     outcome.numerators, # no sim dimension
                                     outcome.denominators, # no sim dimension
                                     parameters,
                                     from.year,
                                     to.year)
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
    parameters.indexed.by.sim = list('1'=parameters)
    
    JHEEM.SIMULATION.SET$new(version = version,
                             location = location,
                             outcome.numerators = outcome.numerators.with.sim.dimension,
                             outcome.denominators = outcome.denominators.with.sim.dimension,
                             parameters = parameters.indexed.by.sim,
                             from.year = from.year,
                             to.year = to.year,
                             n.sim = 1)
}

join.simulation.sets <- function(...)
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

    combined.parameters = unlist(lapply(simset.list, function(simset) {simset$data$parameters}), recursive=F)
    
    JHEEM.SIMULATION.SET$new(version = sample.simset$version,
                             location = sample.simset$location,
                             outcome.numerators = combined.outcome.numerators,
                             outcome.denominators = combined.outcome.denominators,
                             parameters = combined.parameters,
                             from.year = sample.simset$from.year,
                             to.year = sample.simset$to.year,
                             n.sim = new.n.sim)
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
                              location,
                              from.year = NULL,
                              to.year = NULL,
                              type = "Simulation Metadata",
                              years.can.be.missing = T,
                              error.prefix)
        {
            #-- Call the superclass constructor --#
            super$initialize(version = version,
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
            
            # Pull outcome ontologies and metadata from the specification
            
            specification = get.compiled.specification.for.version(version)
            specification.metadata = self$specification.metadata
            
            private$i.metadata$outcome.ontologies = list()
            private$i.metadata$outcome.metadata = list()
            
            for (outcome.name in specification$outcome.names)
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
                    else
                        years.for.ont = NULL
                    
                    ont = c(ontology(year=years.for.ont, incomplete.dimensions = 'year'),
                            specification.metadata$apply.aliases(outcome$ontology, error.prefix = error.prefix))
                    private$i.metadata$outcome.ontologies[[outcome$name]] = ont
                    
                    private$i.metadata$outcome.metadata[[outcome$name]] = MODEL.OUTCOME.METADATA$new(outcome.metadata = outcome$metadata,
                                                                                                     is.cumulative = outcome$is.cumulative,
                                                                                                     corresponding.observed.outcome = outcome$corresponding.data.outcome)
                }
            }
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
        }
    ),
    
    private = list(
        
        i.metadata = NULL,
        
        # i.from.year = NULL,
        # i.to.year = NULL,
        # 
        # i.outcome.ontologies = NULL,
        # i.outcome.metadata = NULL,
        
        get.current.code.iteration = function()
        {
            JHEEM.SIMULATION.CODE.ITERATION
        },
        
        process.dimension.values = function(dimension.values, ..., error.prefix)
        {
            # Validate dimension values (and fold in ...)
            dot.dot.dot = list(...)
            
            check.dimension.values.valid(dot.dot.dot,
                                         variable.name.for.error = "The elements of ...",
                                         error.prefix = error.prefix,
                                         allow.empty = T)
            
            check.dimension.values.valid(dimension.values,
                                         variable.name.for.error = "dimension.values",
                                         error.prefix = error.prefix,
                                         allow.empty = T)
            
            dimension.values[names(dot.dot.dot)] = dot.dot.dot
            
            if (any(names(dimension.values)=='year'))
                dimension.values$year = as.character(dimension.values$year)
            
            dimension.values
        }
    )
    
)

JHEEM.SIMULATION.SET = R6::R6Class(
    'jheem.simulation.set',
    inherit = SIMULATION.METADATA,
    lock_object = F,
    portable = F,
    
    public = list(
        initialize = function(version, # needs a subversion
                              location,
                              outcome.numerators, # now must have sim dimension
                              outcome.denominators, # now must have sim dimension
                              parameters,
                              from.year,
                              to.year,
                              n.sim,
                              error.prefix = "Error constructing simulation")
        {
            #-- Call the superclass constructor --#
            super$initialize(version = version,
                             location = location,
                             type = "Simulation",
                             years.can.be.missing = F,
                             from.year = from.year,
                             to.year = to.year,
                             error.prefix = error.prefix)
            
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
            
            #-- Update the outcome metadata's years for each of the non-cumulative outcomes --#
            
            
            #-- Store data --#
            private$i.data = list(outcome.numerators = outcome.numerators,
                                  outcome.denominators = outcome.denominators,
                                  parameters = parameters)
            private$i.n.sim = n.sim
            
            #-- Make Active Bindings with the Names of Outcomes --#
            outcomes.to.bind = setdiff(self$outcomes, names(self))
            lapply(outcomes.to.bind, function(outcome.name){
                
                makeActiveBinding(sym=outcome.name,
                                  fun = function(value){
                                      
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
                                          stop("Cannot modify a simulation's 'parameters' - they are read-only")
                                      
                                  },
                                  env = self)
            })
            
            lockEnvironment(self)
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
            dimension.values = private$process.dimension.values(dimension.values, ..., error.prefix=error.prefix)
            
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
            ont1 = ontologies[[1]]
            if (check.consistency)
            {
                out1 = outcomes[1]
                for (out2 in outcomes[-1])
                {
                    ont2 = ontologies[[2]]
                    
                    unequal.dimensions.mask = sapply(names(ont1), function(d){
                        !identical(ont1[[d]], ont2[[d]])
                    })
                    
                    if (any(unequal.dimensions.mask))
                    {
                        unequal.dimensions = names(ont1)[unequal.dimensions.mask]
                        stop(paste0(error.prefix, "Cannot get data for outcomes '", out1, "' and '", out2, 
                                    "' in one function call. Their ontologies have differing values for the ",
                                    collapse.with.and("'", unequal.dimensions, "'"),
                                    ifelse(length(unequal.dimensions)==1, " dimension.", " dimensions.")))
                    }
                }
            }
            # if (debug) browser()
            # Fold together, with outcome and sim as the last dimensions
            if (!drop.single.outcome.dimension || length(outcomes)>1) {
                if (!drop.single.sim.dimension || self$n.sim > 1)
                    c(ont1,
                      ontology(outcome=outcomes, sim=1:self$n.sim, incomplete.dimensions = c('outcome', 'sim')))
                else
                    c(ont1,
                      ontology(outcome=outcomes, incomplete.dimensions = 'outcome'))
            }
            else {
                if (!drop.single.sim.dimension || self$n.sim > 1)
                    c(ont1,
                      ontology(sim=1:self$n.sim, incomplete.dimensions = 'sim'))
                else
                    ont1
            }
        },
        
        get = function(outcomes,
                       output = c('value', 'numerator', 'denominator')[[1]],
                       keep.dimensions=NULL,
                       dimension.values = list(),
                       ...,
                       check.consistency = T,
                       drop.single.outcome.dimension = T,
                       drop.single.sim.dimension = F,
                       replace.inf.values.with.zero = T,
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
            
            dimension.values = private$process.dimension.values(dimension.values, ..., error.prefix=error.prefix)
            # if (drop.single.sim.dimension && self$n.sim==1)
            #     keep.dimensions = names(dim.names)
            # else
            #     keep.dimensions = names(dim.names)[-length(dim.names)]
            # 
            # if (!drop.single.sim.dimension || self$n.sim > 1)
            #     keep.dimensions = union(keep.dimensions, 'sim')

            rv = sapply(outcomes, function(outcome){
                scale = self$outcome.metadata[[outcome]]$scale
                numerator.needed = output %in% c('value', 'numerator')
                denominator.needed = scale.needs.denominator(scale) && output %in% c('value', 'denominator')
                
                numerator.data = NULL
                denominator.data = NULL
                
                if (numerator.needed) {
                    numerator.data = private$i.data$outcome.numerators[[outcome]]
                    numerator.data = array.access(numerator.data, dimension.values)
                }
                if (denominator.needed) {
                    denominator.data = private$i.data$outcome.denominators[[outcome]]
                    if (check.consistency && is.null(denominator.data))
                        stop(paste0(error.prefix, "outcome '", outcome, "' missing denominator data"))
                    denominator.data = array.access(denominator.data, dimension.values)
                }
                if (check.consistency && output == 'denominator' && !scale.needs.denominator(scale))
                     stop(paste0(error.prefix, "outcome '", outcome, "' does not use a denominator"))

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
                output.array
            })
            
            dim(rv) = sapply(dim.names, length)
            dimnames(rv) = dim.names
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
            new.parameters = private$i.data$parameters[x]
            
            JHEEM.SIMULATION.SET$new(version = self$version,
                                     location = self$location,
                                     outcome.numerators = new.outcome.numerators,
                                     outcome.denominators = new.outcome.denominators,
                                     parameters = new.parameters,
                                     from.year = self$from.year,
                                     to.year = self$to.year,
                                     n.sim = new.n.sim)
        },
        
        # n: keeps every nth (rounding DOWN) sim counting backwards from the last sim
        # keep: may be fraction or a number to keep
        thin = function(n=NULL, keep=NULL)
        {
            error.prefix = "Error thinning simulation.set: "
            if (is.null(n) && is.null(keep))
                stop(paste0(error.prefix, "either 'n' or 'keep' must be specified"))
            if (!is.null(n) && !is.null(keep))
                stop(paste0(error.prefix, "exactly one of 'n' or 'keep' must be specified"))
            if (!is.null(n) && !is.numeric(n) || length(n) != 1 || n > self$n.sim || n < 1)
                stop(paste0(error.prefix, "'n' must be a single integer value between 1 and 'n.sim'"))
            if (!is.null(keep) && !is.numeric(keep) || length(keep) != 1 || keep > self$n.sim || keep < 0)
                stop(paste0(error.prefix, "'keep' must be either a single integer value between 1 and 'n.sim' or a fraction between 0 and 1"))
            
            if (is.null(keep)) keep = ceiling(self$n.sim / n)
            if (keep < 1) keep = ceiling(self$n.sim * keep)
            if (is.null(n)) n = self$n.sim / keep
            
            self$subset(ceiling(self$n.sim - n * (0 : (keep - 1))))
            
        },
        
        burn = function(n=NULL, keep=NULL)
        {
            error.prefix = "Error burning sims from simulation.set: "
            
            
        },
        
        get.engine = function()
        {
            
        },
        
        get.intervention = function()
        {
            
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
        
        data = function(value)
        {
            if (missing(value))
                private$i.data
            else
                stop("Cannot modify a simulation.set's 'data' - it is read-only")
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
        i.data = NULL,
        
        # i.numerators = NULL,
        # i.denominators = NULL,
        # i.parameters = NULL,
        
        i.n.sim = NULL,
        
        i.intervention = NULL,
        i.intervention.code = NULL
        
    )
)