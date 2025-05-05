

#'@title Assess Whether Simulations from one Version/Specification can be Transmuted to Another Version
#'
#'@description 'Transmutation' of a simulation is calculating a set of outcomes for another version without re-running the (expensive) differential equation solver
#'
#'@param from.version,to.version The versions from and to which we want to transmute
#'@param from.sub.version,to.sub.versions The sub-versions from and to which we want to transmute
#'@param print.details Whether to print the reasons why transmutation cannot happen
#'
#'@value A single logical value
#'
#'@export
can.transmute.versions <- function(from.version,
                                   to.version,
                                   from.sub.version = NULL,
                                   to.sub.version = NULL,
                                   print.details = T)
{
    
    if (!is.character(from.version) || length(from.version)!=1 || is.na(from.version))
        stop("Error in can.transmute.versions() - 'from.version' must be a single, non-NA character value")
    
    from.specification = get.compiled.specification.for.version(from.version)
    if (is.null(from.specification))
        stop("Error in can.transmute.versions() - There is no registered specification for version '", from.version, "'")
    
    
    rv = do.evaluate.can.transmute(from.kernel.or.specification = from.specification,
                                   from.sub.version = from.sub.version,
                                   to.version = to.version,
                                   to.sub.version = to.sub.version)
    
    if (is.null(rv))
        T
    else
    {
        print(rv)
        F
    }
}

# Returns NULL if can be evaluated
# Otherwise returns a message why
do.evaluate.can.transmute <- function(from.kernel.or.specification,
                                      from.sub.version,
                                      to.version,
                                      to.sub.version)
{
    #-- Versions --#
    if (!is.character(to.version) || length(to.version)!=1 || is.na(to.version))
        stop("Error in can.transmute.versions() - 'to.version' must be a single, non-NA character value")

    to.specification = get.compiled.specification.for.version(to.version)
    if (is.null(to.specification))
        stop("Error in can.transmute.versions() - There is no registered specification for version '", to.version, "'")
    
    #-- Sub-versions --#
    
    #-- Parameters --#
    
    from.version = from.kernel.or.specification$version
    
    # 1) The calibrated.parameters are equal
    calibrated.from.parameters = get.parameter.names.for.version(from.version, type='calibrated')
    calibrated.to.parameters = get.parameter.names.for.version(to.version, type='calibrated')
    
    if (!setequal(calibrated.from.parameters, calibrated.to.parameters))
        return (paste0("Calibrated parameters are not the same in the '", to.version, "' (from) specification and the '",
                       from.kernel.or.specification$version, "' (to) kernel"))
    
    # 2) The sampled plus set parameters in to.version are superset of sampled plus set parameters in from.version
    sampled.from.parameters = get.parameter.names.for.version(from.version, type='sampled')
    sampled.to.parameters = get.parameter.names.for.version(to.version, type='sampled')
    set.from.parameters = get.parameter.names.for.version(from.version, type='set')
    set.to.parameters = get.parameter.names.for.version(to.version, type='set')
    
    sampled.plus.set.from.parameters = c(sampled.from.parameters, set.from.parameters)
    sampled.plus.set.to.parameters = c(sampled.to.parameters, set.to.parameters)
    
    missing.params = setdiff(sampled.plus.set.from.parameters, sampled.plus.set.to.parameters)
    if (length(missing.params)>0)
        return (paste0(length(missing.params),
                       " set/sampled",
                       ifelse(length(missing.params)==1, " parameter is", " parameters are"),
                       " present in the '", from.kernel.or.specification$version, "' (to) kernel but not in the '",
                       to.version, "' (to) specification: ",
                       collapse.with.and("'", missing.params, "'")
                       ))

    #-- Outcomes --#
    
    # Every outcome in 'to' is either:
    #   a) Not present in from OR
    #   b) Both
    #       i) Has the same dimensions
    #       ii) The dimnames of the outcome in 'to' is a superset of the dimnames in 'from'
    
    if (is(from.kernel.or.specification, 'jheem.kernel'))
        specification.metadata = from.kernel.or.specification$specification.metadata
    else
        specification.metadata = get.specification.metadata(from.kernel.or.specification$version,
                                                            location = from.kernel.or.specification$location)
    
    to.outcome.names = to.specification$get.outcome.names.for.sub.version(to.sub.version)
    from.outcome.names = from.kernel.or.specification$get.outcome.names.for.sub.version(from.sub.version)
    
    for (outcome.name in to.specification$get.outcome.names.for.sub.version(to.sub.version))
    {
        if (is(from.kernel.or.specification, 'jheem.kernel'))
            from.outcome = from.kernel.or.specification$get.outcome.kernel(outcome.name)
        else
            from.outcome = from.kernel.or.specification$get.outcome(outcome.name)
        
        to.outcome = to.specification$get.outcome(outcome.name)
        if (all(from.outcome.names != outcome.name))
        {   
            if (to.outcome$is.dynamic)
            {
                if (is.null(from.outcome))
                    return(paste0("Outcome '", outcome.name, "' is not present in the '",
                                  from.kernel.or.specification$version, "' (from) kernel and is a DYNAMIC outcome in the '",
                                  to.version, "' (to) specification. To be able to transmuate a simulation, new dynamic outcomes cannot be present in the to specification"))
                else
                    return(paste0("Outcome '", outcome.name, "' is not SAVED in the '",
                                  from.kernel.or.specification$version, "' (from) simulation.set and is a DYNAMIC outcome in the '",
                                  to.version, "' (to) specification. To be able to transmuate a simulation, dynamic outcomes that were not saved in the simulation.set to transmute cannot be present in the to specification"))
            }

            depends.on.outcomes = union(to.outcome$depends.on.outcomes,
                                        intersect(to.outcome$depends.on.quantities.or.outcomes, to.specification$outcome.names))
            
            missing.outcomes = setdiff(depends.on.outcomes, to.outcome.names)
            if (length(missing.outcomes)>0)
            {
                return(paste0("Calculating the outcome '", outcome$name, "' in the '",
                              to.version, "' (to) specification depends on ",
                              length(missing.outcomes),
                              ifelse(length(missing.outcomes)==1, " outcome ", " outcomes "),
                              "(", collapse.with.and("'", missing.outcomes, "'"), ") which ",
                              ifelse(length(missing.outcomes)==1, "is", "are"),
                              " not present in the '", to.version, "' specification"))
            }
        }
        else
        {
            if (!setequal(names(from.outcome$dim.names), names(to.outcome$dim.names)))
            {
                return (paste0("The dimensions for outcome '", outcome.name, "' in the '",
                               from.kernel.or.specification$version, "' (from) specification (",
                               paste0(names(from.outcome$dim.names), collapse='/'),
                               ") are not the same as in the '",
                               to.version, "' (to) specification (",
                               paste0(names(to.outcome$dim.names), collapse='/'), ")"))
            }
            
            to.outcome.dim.names = specification.metadata$apply.aliases(to.outcome$dim.names)
            if (!dim.names.are.subset(sub.dim.names = from.outcome$dim.names, 
                                      super.dim.names = to.outcome.dim.names))
            {
                return (paste0("The dimnames for outcome '", outcome.name, "' in the '",
                               from.kernel.or.specification$version, "' (from) kernel are NOT a subset of the dim.names in the '",
                               to.version, "' (to) specification. ",
                               "Even though both have the same dimensions, there are dimension values present in '",
                               from.kernel.or.specification$version, "' that are missing in '",
                               to.version, "'"))
            }
            
        }
    }
    
    return (NULL)
}

get.outcome.and.quantity.names.for.transmuting <- function(from.kernel.or.specification,
                                                           from.sub.version,
                                                           to.version,
                                                           to.sub.version)
{
    to.specification = get.compiled.specification.for.version(to.version)
    rv = list()
    
    to.outcome.names = to.specification$get.outcome.names.for.sub.version(to.sub.version)
    from.outcome.names = from.kernel.or.specification$get.outcome.names.for.sub.version(from.sub.version)
    
    rv$outcome.names.to.calculate = setdiff(to.outcome.names,
                                            from.outcome.names)
    
    rv$outcome.names.to.transmute = intersect(to.outcome.names,
                                              from.outcome.names)
    
    rv$quantity.names.for.transmutation = intersect(
        to.specification$ordered.quantity.names,
        to.specification$get.outcome.dependee.quantity.names(rv$outcome.names.to.calculate)
    )
    
    # rv$quantity.names.for.transmutation = unique(unlist(lapply(rv$outcome.names.to.calculate, function(outcome.name){
    #     
    #     outcome = to.specification$get.outcome(outcome.name)
    #     
    #     # Theoretically we have checked that these are all valid quantities in the do.evaluate.can.transmute() function
    #     union(outcome$depends.on.quantities,
    #           setdiff(outcome$depends.on.quantities.or.outcomes, to.outcome.names))
    #     
    # })))
    # 
    rv$dependee.outcome.names.to.transmute = unique(unlist(lapply(rv$outcome.names.to.calculate, function(outcome.name){
        
        outcome = to.specification$get.outcome(outcome.name)
        
        depends.on.outcomes = union(outcome$depends.on.outcomes,
                                    intersect(outcome$depends.on.quantities.or.outcomes, to.specification$outcome.names))
        
        setdiff(depends.on.outcomes, rv$outcome.names.to.calculate)
    })))
    
    rv
}

#'@name Create a Transmuter
#'@details A jheem.transmuter can convert simulations from one version/specification to another using its transmute() method
#'
#'@param simulation.set The simulation.set to transmute
#'@param to.version,to.sub.version The version and sub.version to transmute to
#'@from.year,to.year The from and to years to keep in the transmuted simulations
#'
#'@export
create.jheem.transmuter <- function(simulation.set,
                                    to.version,
                                    from.year = simulation.set$from.year,
                                    to.year = simulation.set$to.year,
                                    to.sub.version = NULL)
{
    JHEEM.TRANSMUTER$new(simulation.set = simulation.set,
                         to.version = to.version,
                         keep.from.year = from.year,
                         keep.to.year = to.year,
                         to.sub.version = to.sub.version)
}


JHEEM.TRANSMUTER = R6::R6Class(
    'jheem.transmuter',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function(simulation.set,
                              to.version,
                              keep.from.year,
                              keep.to.year,
                              to.sub.version)
        {
            #-- Check Arguments --#
            
            # Check Simulation Set
            
            # Check to.version and to.sub.version
            
            # Check Keep Years
            
            # Make sure we can transmute
            result = do.evaluate.can.transmute(from.kernel.or.specification = simulation.set$jheem.kernel, 
                                               from.sub.version = simulation.set$sub.version,
                                               to.version = to.version,
                                               to.sub.version = to.sub.version)
            if (!is.null(result))
                stop(paste0("We cannot transmute simulations from '", simulation.set$version, "' simulation.set to '",
                            to.version, "' version:\n",
                            result))

            super$initialize(version = to.version,
                             to.sub.version = to.sub.version,
                             location = simulation.set$location,
                             type = 'transmuter')
            
            #-- Make the JHEEM Engine --#
            kernel = create.jheem.kernel(to.version, simulation.set$location)
            private$i.jheem = JHEEM$new(jheem.kernel = kernel,
                                        to.sub.version = to.sub.version,
                                        transmute.from.kernel = simulation.set$jheem.kernel,
                                        transmute.from.sub.version = simulation.set$sub.version,
                                        error.prefix = error.prefix)
            
            #-- Store stuff --#
            private$i.simulation.set = simulation.set
            
            private$i.keep.from.year = keep.from.year
            private$i.keep.to.year = keep.to.year
            
            private$i.check.consistency = T
            
        },
        
        crunch = function(sim.index, parameters = numeric())
        {
            private$prepare.to.transmute.or.crunch(parameters = parameters,
                                                   sim.index = sim.index,
                                                   error.prefix = 'Cannot crunch JHEEM Transmuter: ')
            
            private$i.jheem$crunch.for.transmutationcrunch.for.transmutation(keep.from.year = private$i.keep.from.year,
                                                                             keep.to.year = private$i.keep.to.year,
                                                                             prior.simulation.set = private$i.simulation.set,
                                                                             prior.sim.index = sim.index,
                                                                             check.consistency = private$i.check.consistency)
            
            private$i.check.consistency = F  
        },
        
        transmute = function(sim.index, parameters = numeric())
        {
            private$prepare.to.transmute.or.crunch(parameters = parameters,
                                                   sim.index = sim.index,
                                                   error.prefix = 'Cannot run JHEEM Transmuter: ')
            
            rv = private$i.jheem$transmute(prior.simulation.set = private$i.simulation.set,
                                           prior.sim.index = sim.index,
                                           keep.from.year = private$i.keep.from.year,
                                           keep.to.year = private$i.keep.to.year,
                                           finalize = T,
                                           check.consistency = private$i.check.consistency)

            private$i.check.consistency = F
            rv
        },
        
        test = function()
        {
            private$i.jheem$test()
        },
        
        extract.quantity.values = function()
        {
            private$i.jheem$extract.quantity.values()
        }
    ),
    
    active = list(
        keep.from.year = function(value)
        {
            if (missing(value))
                private$i.keep.from.year
            else
                stop("Cannot modify a JHEEM tranmuter's 'keep.from.year' - they are read-only")
        },
        
        keep.to.year = function(value)
        {
            if (missing(value))
                private$i.keep.to.year
            else
                stop("Cannot modify a JHEEM tranmuter's 'keep.to.year' - they are read-only")
        }
    ),
    
    private = list(
        
        i.jheem = NULL,
        i.simulation.set = NULL,
        i.check.consistency = NULL,
        i.keep.from.year = NULL,
        i.keep.to.year = NULL,
        
        prepare.to.transmute.or.crunch = function(parameters, sim.index, error.prefix)
        {
            if (length(parameters)>0)
            {
                if (!is.numeric(parameters) || any(is.na(parameters)))
                    stop(paste0(error.prefix, "'parameters' must be a numeric vector with no NA values"))
                
                if (is.null(names(parameters)))   
                    stop(paste0(error.prefix, "'parameters' must be a NAMED numeric vector"))
            }
            
            
            if (is.null(sim.index))
                stop(paste0(error.prefix, "You MUST specify a 'sim.index'"))
            
            if (!is.numeric(sim.index) || length(sim.index)!=1 || is.na(sim.index) || floor(sim.index)!=sim.index)
                stop(paste0(error.prefix, "'sim.index' must be a single, non-NA, integer value"))
            
            if (sim.index < 1 || sim.index > private$i.simulation.set$n.sim)
                stop(paste0(error.prefix, "'sim.index' (", sim.index, ") must be a valid index into the simulation.set - i.e. between 1 and ", private$i.simulation.set$n.sim))

            new.parameters = parameters
            parameters = private$i.simulation.set$parameters[,sim.index]
            parameters[names(new.parameters)] = new.parameters
            
            private$i.jheem$set.parameters(parameters, check.consistency = private$i.check.consistency)
        },
        
        get.current.code.iteration = function()
        {
            JHEEM.CODE.ITERATION
        }
    )
)
