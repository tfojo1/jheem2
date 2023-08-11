JHEEM.SIMULATION.CODE.ITERATION = '2.0'

##----------------------##
##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##
##----------------------##


get.sim.quantity <- function(sim, outcome)
{
    
}



#'@name Get a Simulation Metadata Object
#'
#'@param version The version of the model specification (must have been previously registered) for which to get metadata
#'@param location A single character value representing the location for the metadata
#'@param error.prefix A string to prepend to any errors generated in getting the metadata object
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
            
            private$i.from.year = from.year
            private$i.to.year = to.year
            
            # Pull outcome ontologies and metadata from the specification
            
            specification = get.compiled.specification.for.version(version)
            specification.metadata = self$specification.metadata
            
            private$i.outcome.ontologies = list()
            private$i.outcome.metadata = list()
            
            for (outcome.name in specification$outcome.names)
            {
                outcome = specification$get.outcome(outcome.name)
                if (outcome$save)
                {
                    if (outcome$is.cumulative && !is.null(private$i.from.year) && !is.null(private$i.to.year))
                    {
                        from.year = max(private$i.from.year, outcome$from.year)
                        to.year = min(private$i.to.year, outcome$to.year)
                        
                        if (to.year>=from.year)
                            years.for.ont = as.character(from.year:to.year)
                        else
                            years.for.ont = character()
                    }
                    else
                        years.for.ont = NULL
                    
                    ont = c(ontology(year=years.for.ont, incomplete.dimensions = 'year'),
                            specification.metadata$apply.aliases(outcome$ontology, error.prefix = error.prefix))
                    private$i.outcome.ontologies[[outcome$name]] = ont
                    
                    private$i.outcome.metadata[[outcome$name]] = MODEL.OUTCOME.METADATA$new(outcome.metadata = outcome$metadata,
                                                                                            is.cumulative = outcome$is.cumulative,
                                                                                            corresponding.observed.outcome = outcome$corresponding.data.outcome)
                }
            }
        },
        
        
        # Returns the dimnames that the results of a call to simulation$get will have
        # It's arguments mirror simulation$get
        get.dim.names = function(outcomes,
                                 keep.dimensions=NULL,
                                 dimension.values = list(),
                                 ...,
                                 error.prefix = "Error getting dimnames of simulation results: ")
        {
            #@Andrew - need to calculate default keep.dimensions as we did for data manager
            if (is.null(keep.dimensions))
                keep.dimensions = 'year'
            
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
                
                # Make sure dimension.values work
                for (d in names(dimension.values))
                {
                    if (is.null(ont[[d]]))
                        ont[[d]] = dimension.values[[d]]
                    else
                    {
                        invalid.dimension.values = setdiff(dimension.values[[d]], ont[[d]])
                        if (length(invalid.dimension.values)>0)
                            stop(paste0(error.prefix, "For the '", outcome, 
                                        "' outcome, ",
                                        collapse.with.and("'", invalid.dimension.values, "'"),
                                        ifelse(length(invalid.dimension.value.dimensions)==1, " is an invalid value", " are invalid values"),
                                        " for the '", d, "' dimension of the ontology",
                                        ifelse(length(ont[[d]])<=6, 
                                               paste0(" (", paste0("'", ont[[d]], "'", collapse=", "), ")"),
                                               "")
                            ))
                    }
                }
                
                ont[names(dimension.values)] = dimension.values
                ont[keep.dimensions]
            })
            names(ontologies) = outcomes
            
            # Make sure the ontologies are congruous
            out1 = outcomes[1]
            ont1 = ontologies[[1]]
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
            
            # Fold together, with outcome as the last dimension
            c(ont1,
              ontology(outcome=outcomes, incomplete.dimensions = 'outcome'))
        }
        
    ),
    
    active = list(
        
        outcomes = function(value)
        {
            if (missing(value))
                names(private$i.outcome.ontologies)
            else
                stop("Cannot modify a simulation.metadata's 'outcomes' - they are read-only")
        },
        
        outcome.ontologies = function(value)
        {
            if (missing(value))
                private$i.outcome.ontologies
            else
                stop("Cannot modify a simulation.metadata's 'outcome.ontologies' - they are read-only")
        },
        
        outcome.metadata = function(value)
        {
            if (missing(value))
                private$i.outcome.metadata
            else
                stop("Cannot modify a simulation.metadata's 'outcome.metadata' - it is read-only")
        },
        
        from.year = function(value)
        {
            if (missing(value))
                private$i.from.year
            else
                stop("Cannot modify a simulation.metadata's 'from.year' - it is read-only")
        },
        
        to.year = function(value)
        {
            if (missing(value))
                private$i.to.year
            else
                stop("Cannot modify a simulation.metadata's 'to.year' - it is read-only")
        }
    ),
    
    private = list(
        
        i.from.year = NULL,
        i.to.year = NULL,
        
        i.outcome.ontologies = NULL,
        i.outcome.metadata = NULL,
        
        get.current.code.iteration = function()
        {
            JHEEM.SIMULATION.CODE.ITERATION
        },
        
        process.dimension.values = function(dimension.values, ..., error.prefix)
        {
            # Validate dimension values (and fold in ...)
            #@Andrew - need to fold ... into dimension.values, as we did for data manager
            # is how I did it right?
            dot.dot.dot = list(...)
            check.dimension.values.valid(dot.dot.dot,
                                         variable.name.for.error = "The elements of ...",
                                         error.prefix = error.prefix)
            
            check.dimension.values.valid(dimension.values,
                                         variable.name.for.error = "dimension.values",
                                         error.prefix = error.prefix)
            
            dimension.values[names(dot.dot.dot)] = dot.dot.dot
            
            if (any(names(dimension.values)=='year'))
                dimension.values$year = as.character(dimension.values$year)
            
            dimension.values
        }
    )
    
)

JHEEM.SIMULATION = R6::R6Class(
    'jheem.simulation',
    inherit = SIMULATION.METADATA,
    portable = F,
    
    public = list(
        initialize = function(version,
                              location,
                              outcome.data,
                              from.year,
                              to.year,
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
            # Make sure they are numeric arrays with dimensions that match the ontology

            #-- Update the outcome metadata's years for each of the non-cumulative outcomes --#
            
            
            #-- Store data --#
            private$i.data = outcome.data
            
        },
        
        get = function(outcomes,
                       keep.dimensions=NULL,
                       dimension.values = list(),
                       ...,
                       error.prefix = "Error getting simulation results: ")
        {
            dim.names = self$get.dim.names(outcomes = outcomes,
                                           keep.dimensions = keep.dimensions,
                                           dimension.values = dimension.values,
                                           ...,
                                           error.prefix = error.prefix)
            
            dimension.values = private$process.dimension.values(dimension.values, ..., error.prefix=error.prefix)
            keep.dimensions = names(dim.names)[-length(dim.names)]
            
            # The below implementation is not correct
            # - need to access to make sure dimension values are in the right order
            # - will probably make sense to write a custom cpp call for this
            # - we need to aggregate with denominators
            rv = sapply(outcomes, function(outcome){
                
                outcome.data = private$i.data[[outcome]]
                apply(array.access(outcome.data, dimension.values), keep.dimensions, sum)
            })
            
            dim(rv) = sapply(dim.names, length)
            dimnames(rv) = dim.names
            rv
        }
        
    ),
    
    active = list(
        
    ),
    
    private = list(
        
        i.data = NULL
        
    )
)