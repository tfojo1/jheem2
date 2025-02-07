

DEBUG.VERSIONS = T # an internal flag for debugging and making error stack traces more useful

##-- CONSTRUCT the VERSION MANAGER --##

VERSION.MANAGER.ELEMENTS = c(
    'specification', 'compiled.specification',
    'prior.versions',
    'calibrated.parameters', 'sampled.parameters', 'set.parameters',
    'apply.calibrated.parameters.function', 'apply.sampled.parameters.function',
    'calibrated.parameters.distribution', 'sampled.parameters.distribution',
    'calibrated.parameters.sampling.blocks', 
    'calibrate.to.year',
    'nested.proportions.partition.function'
)

VERSION.MANAGER = new.env() #making this an environment allows us to modify by reference within functions

# This wrapper is also how we'll create the version manager

#'@title Clear JHEEM Versions
#'@export
clear.versions <- function()
{
    VERSION.MANAGER$versions = character()
    VERSION.MANAGER$prior.versions = list()
    
    for (element.name in VERSION.MANAGER.ELEMENTS)
        VERSION.MANAGER[[element.name]] = list()
    
}

clear.versions()

# This is also where I'm putting a wrapper that will clear all our managers (Version, Calibration, Intervention, Ontology Mappings) at once.

#'@title Clear JHEEM Version, Calibration, Intervention, and Ontology Mappings
#'@export
clear.all.managers <- function()
{
    clear.versions()
    clear.calibrations()
    clear.interventions()
    clear.ontology.mappings()
}

##--------------------------------------------------##
##-- SETTER and GETTERS for VERSION/SPECIFICATION --##
##--------------------------------------------------##

#'@title Register a JHEEM Model Specification
#'
#'@param specification A jheem.specification object, as created by \code{\link{create.jheem.specification}}
#'
#'@details Once a specification object is registered, it can no longer be modified
#'
#'@family JHEEM Version Management Functions
#'
#'@export
register.model.specification <- function(specification)
{
    if (!is(specification, 'jheem.specification'))
        stop("'specification' must be an object of class 'jheem.specification'")
    
    if (any(VERSION.MANAGER$versions == specification$version) &&
        any(sapply(VERSION.MANAGER$prior.versions, function(priors){any(priors==specification$version)})))
    {
        stop(paste0("A specification for version '", specification$version, "' has already been registered"))
    }
    else
    {
        version = specification$version
   
        if (DEBUG.VERSIONS)
        {
            VERSION.MANAGER$versions = union(VERSION.MANAGER$versions, version)
            
            do.register.for.version(version=version,
                                    element.name='specification',
                                    element.value=specification,
                                    element.class='jheem.specification')
            
            do.remove.for.version(version=version,
                                  element.name='compiled.specification')
            
            do.register.for.version(version=version,
                                    element.name='prior.versions',
                                    element.value=specification$parent.version,
                                    element.class='character')
            
            compiled.specification = specification$compile()
            do.register.for.version(version=version,
                                    element.name='compiled.specification',
                                    element.value=compiled.specification,
                                    element.class=NULL) #compiled specifications are not assigned a class for optimization purposes (to avoid S3 method lookup)
            
        }
        else
        {
            tryCatch({
                VERSION.MANAGER$versions = union(VERSION.MANAGER$versions, version)
                
                do.register.for.version(version=version,
                                        element.name='specification',
                                        element.value=specification,
                                        element.class='jheem.specification')
        
                do.remove.for.version(version=version,
                                      element.name='compiled.specification')
                
                do.register.for.version(version=version,
                                        element.name='prior.versions',
                                        element.value=specification$parent.version,
                                        element.class='character')
                
                compiled.specification = specification$compile()
                do.register.for.version(version=version,
                                        element.name='compiled.specification',
                                        element.value=compiled.specification,
                                        element.class=NULL) #compiled specifications are not assigned a class for optimization purposes (to avoid S3 method lookup)
            },
            error = function(e)
            {
                VERSION.MANAGER$versions = setdiff(VERSION.MANAGER$versions, version)
                VERSION.MANAGER$specification[[version]] = NULL
                VERSION.MANAGER$prior.versions[[version]] = NULL
                
                stop(e)
            })
        }
    }
    
    invisible(NULL)
}


#'@family JHEEM Version Management Functions
#'
#'@export
get.prior.versions <- function(version,
                               recursive=T)
{
    prior.versions = do.get.for.version(version=version,
                                        element.name='prior.versions')
    
    if (recursive && length(prior.versions)>0)
    {
        prior.index = 1
        while (prior.index <= length(prior.versions))
        {
            to.add = get.prior.versions(version=prior.versions[prior.index],
                                        recursive=T)
            
            prior.versions = c(prior.versions, setdiff(to.add, prior.versions))
            prior.index = prior.index + 1
        }
    }
    
    prior.versions
}

#'@title Get a JHEEM Specification Object
#'
#'@param version The name of the version under which the specification was registered
#'
#'@return An object of class 'jheem.specification'
#'
#'@family JHEEM Version Management Functions
#'
#'@export
get.specification.for.version <- function(version)
{
    do.get.for.version(version=version,
                       element.name='specification',
                       allow.null = !throw.error.if.missing)
}

# This function is internal to the package
is.specification.registered.for.version <- function(version)
{
    !is.null(do.get.for.version(version=version,
                               element.name='specification',
                               allow.null = T))
}

# This function is internal to the package
get.compiled.specification.for.version <- function(version)
{
    do.get.for.version(version=version,
                       element.name='compiled.specification')
}

# This function is internal to the package
is.compiled.specification.registered.for.version <- function(version)
{
    !is.null(do.get.for.version(version=version,
                               element.name='compiled.specification',
                               allow.null = T))
}

##--------------------------------------##
##-- SETTER for CALIBRATED PARAMETERS --##
##--------------------------------------##


#'@title Register Parameters to Be Used in Calibrating a JHEEM Version
#'
#'@details *Calibrated* parameters have their values fitted though a Bayesian calibration process
#'
#'@param version The name of an EHE version for which a specification has been registered with \code{\link{register.model.specification}}
#'@param distribution An object of class 'distribution' that specifies the prior distribution for the parameters
#'@param sampling.blocks A list (which may optionally be named) of character vectors. Each element (a character vector) denotes a subset of the parameters to be sampled as a single block during the calibration process. These elements must contain ONLY values present in distribution@var.names, and every value in distribution@var.names must be present in at least one of the blocks
#'@param apply.function A function that knows how to apply parameter values to a jheem.model.settings object. The function should take two arguments: (1) 'model.settings', an object of class jheem.model.settings, and (2) 'parameters', a named numeric vector
#'@param join.with.previous.version Whether the distribution, apply.function, and sampling blocks should be merged with those of the previous version
#'@param calibrate.to.year The year to which the calibration should be run before sampling parameters
#'
#'@export
register.calibrated.parameters.for.version <- function(version,
                                                       distribution,
                                                       sampling.blocks,
                                                       apply.function,
                                                       join.with.previous.version,
                                                       calibrate.to.year)
{
    
    # Try to figure out the function's name, so that we can print an intelligible error
    fn.name = deparse(substitute(apply.function))
    if (!is.character(fn.name) || length(fn.name) != 1)
        fn.name = NULL
    
    error.prefix = "Cannot register calibrated parameters: "
    
    #-- Validate Sampling Blocks --#
    if (!is.list(sampling.blocks))
        stop(paste0(error.prefix, "'sampling.blocks' must be a LIST, containing only character vectors"))
    
    if (any(!sapply(sampling.blocks, is.character)))
        stop(paste0(error.prefix, "'sampling.blocks' must be a list containing ONLY character vectors"))
    
    if (any(sapply(sampling.blocks, length)==0))
        stop(paste0(error.prefix, "'sampling.blocks' cannot contain any empty character vectors"))
    
    flattened = unlist(sampling.blocks)
    if (any(is.na(flattened)))
        stop(paste0(error.prefix, "'sampling.blocks' cannot contain any NA values"))
    
    missing.from.blocks = setdiff(distribution@var.names, flattened)
    if (length(missing.from.blocks)>0)
        stop(paste0(error.prefix, "The elements of 'sampling.blocks' must contain every variable name from 'distribution' at least once between them. ",
                    length(missing.from.blocks),
                    ifelse(length(missing.from.blocks)==1, " variable is", " variables are"),
                    " missing: ",
                    collapse.with.and("'", missing.from.blocks, "'")
        ))
    
    extra.in.blocks = setdiff(flattened, distribution@var.names)
    if (length(extra.in.blocks)>0)
        stop(paste0(error.prefix, "The elements of 'sampling.blocks' must contain ONLY variable names from 'distribution'. ",
                    "However, ",
                    length(extra.in.blocks),
                    " extraneous ",
                    ifelse(length(extra.in.blocks)==1, "value is", "values are"),
                    " present: ",
                    collapse.with.and("'", extra.in.blocks, "'")
        ))
    
    #-- Validate calibrate to year --#
    
    if (!is.numeric(calibrate.to.year) || length(calibrate.to.year)!=1 || is.na(calibrate.to.year))
        stop(paste0(error.prefix, "'calibrate.to.year' must be a single, non-NA numeric value"))
    
    calibrate.to.window = 5 #years from present year
    min.calibrate.to.year = as.numeric(substr(Sys.Date(), 1,4)) - calibrate.to.window
    max.calibrate.to.year = as.numeric(substr(Sys.Date(), 1,4)) + calibrate.to.window
    if (calibrate.to.year < min.calibrate.to.year || calibrate.to.year > max.calibrate.to.year)
        stop(paste0(error.prefix,
                    "'calibrate.to.year' (", calibrate.to.year, 
                    ") must be within +/- ", calibrate.to.window,
                    " years of the current year (ie, between ",
                    min.calibrate.to.year, " and ", max.calibrate.to.year, ")"))
    
    if (!is.character(version) || length(version)!=1 || is.na(version))
        stop("'version' must be a single, non-NA character value")
    if (all(VERSION.MANAGER$versions!=version))
        stop(paste0("'", version, "' has not been registered as a version"))
    
    specification = get.compiled.specification.for.version(version)
    
    if (specification$start.year >= calibrate.to.year)
        stop(paste0(error.prefix,
                    "'calibrate.to.year (", calibrate.from.year,
                    ") must be AFTER the '", version, "' version's start.year' (",
                    specification$start.year, ")"))
    
    #-- Register --#
    
    # Register the distribution and function
    do.register.parameters.distribution.and.apply.function(version = version,
                                                           distribution = distribution,
                                                           apply.function = apply.function,
                                                           join.with.previous.version = join.with.previous.version,
                                                           apply.function.name = fn.name,
                                                           parameter.names = NULL,
                                                           type='calibrated',
                                                           error.prefix = error.prefix)
    
    # Register the blocks
    do.register.for.version(version = version,
                            element.name = 'calibrated.parameters.sampling.blocks',
                            element.value = sampling.blocks,
                            element.class = 'list',
                            join.with.previous.version.value = join.with.previous.version,
                            join.function = c)
    
    # Register the calibrate.to.year
    do.register.for.version(version = version,
                            element.name = 'calibrate.to.year',
                            element.value = calibrate.to.year,
                            element.class = 'numeric',
                            join.with.previous.version.value = F)
}

#'@title Register Parameters to Be Sampled Prior to Making Projections from a JHEEM Version
#'
#'@details *Sampled* parameters have their values randomly chosen from a distribution prior to running projections PAST the calibration period
#'
#'@inheritParams register.calibrated.parameters.for.version
#'@param join.with.previous.version Whether the distribution and apply.function should be merged with those of the previous version
#'
#'@export
register.sampled.parameters.for.version <- function(version,
                                                    distribution,
                                                    apply.function,
                                                    join.with.previous.version)
{
    # Try to figure out the function's name, so that we can print an intelligible error
    fn.name = deparse(substitute(apply.function))
    if (!is.character(fn.name) || length(fn.name) != 1)
        fn.name = NULL
    
    do.register.parameters.distribution.and.apply.function(version = version,
                                                           distribution = distribution,
                                                           apply.function = apply.function,
                                                           join.with.previous.version = join.with.previous.version,
                                                           apply.function.name = fn.name,
                                                           parameter.names = NULL,
                                                           type='sampled',
                                                           error.prefix = "Cannot register sampled parameters: ")
}

#'@title Register Parameters to Be Set for Running a Simulation
#'
#'@details *Set* parameters have their values set explicitly (neither sampled nor calibrated)
#'
#'@inheritParams register.calibrated.parameters.for.version
#'@param parameter.names A character vector giving the names of parameters which will be set
#'@param join.with.previous.version Whether the parameter.names and apply.function should be merged with those of the previous version
#'
register.set.parameters.for.version <- function(version,
                                                parameter.names,
                                                apply.function,
                                                join.with.previous.version)
{
    # Try to figure out the function's name, so that we can print an intelligible error
    fn.name = deparse(substitute(apply.function))
    if (!is.character(fn.name) || length(fn.name) != 1)
        fn.name = NULL
    
    do.register.parameters.distribution.and.apply.function(version = version,
                                                           distribution = NULL,
                                                           apply.function = apply.function,
                                                           join.with.previous.version = join.with.previous.version,
                                                           apply.function.name = fn.name,
                                                           parameter.names = parameter.names,
                                                           type='calibrated',
                                                           error.prefix = "Cannot register set parameters: ")
}

##-----------------------------------------------##
##-- PARTITION FUNCTION for NESTED PROPORTIONS --##
##-----------------------------------------------##

register.partition.function.for.nested.proportions <- function(version,
                                                               partition.function)
{
    # Try to figure out the function's name, so that we can print an intelligible error
    fn.name = deparse(substitute(partition.function))
    if (!is.character(fn.name) || length(fn.name) != 1)
        fn.name = NULL
    
    error.prefix = "Cannot register partition function for nested proportions: "
    
    # Check the function
    if (!is(partition.function, 'function'))
        stop(paste0(error.prefix, "'partition.function' must be a function"))
    
    # Pull the arguments
    fn.args = formals(args(partition.function))
    arg.names = names(fn.args)
    arg.names.without.default.value = arg.names[sapply(fn.args, function(val){val==''})]
    
    # Check that it takes 'arr', 'version', and 'location'
    error.prefix = paste0("Cannot register partition.function for nested.proportions: The function passed to 'partition.function' ",
                          ifelse(is.null(fn.name), "", paste0("(", fn.name, ") ")))
    if (all(arg.names != 'arr'))
        stop(paste0(error.prefix, "must take 'arr' as an argument"))
    if (all(arg.names != 'version'))
        stop(paste0(error.prefix, "must take 'version' as an argument"))
    if (all(arg.names != 'location'))
        stop(paste0(error.prefix, "must take 'location' as an argument"))
    
    # Check that there are no other required arguments
    extraneous.arg.names = setdiff(arg.names.without.default.value, c('arr','version','location'))
    if (length(extraneous.arg.names)>0)
        stop(paste0(error.prefix, " requires ",
                    ifelse(length(extraneous.arg.names)==1, 'argument ', 'arguments '),
                    collapse.with.and("'", extraneous.arg.names, "'"),
                    ", but the only arguments to the function should be 'arr', 'version', and 'location'"))
    
    #-- Register It --#
    do.register.for.version(version = version,
                            element.name = 'nested.proportions.partition.function',
                            element.value = partition.function,
                            element.class = 'function',
                            join.with.previous.version.value = F,
                            join.function = NULL)
}

# This function is internal to the package
get.partition.function.for.nested.proportions <- function(version)
{
    do.get.for.version(version=version,
                       element.name='nested.proportions.partition.function')
}

##---------------------##
##-- THE MAIN HELPER --##
##---------------------##

# A helper to streamline code for the two register.<x>.parameters.for.version functions
do.register.parameters.distribution.and.apply.function <- function(version,
                                                                   distribution,
                                                                   apply.function,
                                                                   join.with.previous.version,
                                                                   apply.function.name,
                                                                   parameter.names,
                                                                   require.distribution = T,
                                                                   type=c('calibrated','sampled','set'),
                                                                   error.prefix)
{
    #-- Validate Version --#
    if (!is.character(version) || length(version)!=1 || is.na(version) || nchar(version)==0)
        stop(paste0(error.prefix, "'version' must be a single, non-NA, non-empty character value"))
    
    if (!is.specification.registered.for.version(version))
        stop(paste0(error.prefix, "version '", version, "' has not had a specification registered. Use register.model.specification() to do so first"))
    
    #-- Make Sure We Have Not Already Registered --#
    
    parameter.names.name = paste0(type, '.parameters')
    if (!is.null(do.get.for.version(version=version,
                                    element.name = parameter.names.name,
                                    pull.previous.version.value.if.missing = F,
                                    allow.null = T)))
        stop(paste0(error.prefix, "Calibrated parameters have already been registered for version '", version, "'"))
    
    #-- Validate Parameter Names/Distribution --#
    if (type=='set')
    {
        if (!is.null(distribution))
            stop(paste0(error.prefix, "Cannot set a distribution for 'set' parameters"))
        
        if (!is.character(parameter.names) || length(parameter.names)==0 || any(is.na(parameter.names)))
            stop(paste0(error.prefix, "'parameter.names' must be a non-empty character vector with no NA values"))
    }   
    else
    {
        if (!is(distribution, 'Distribution'))
            stop(paste0(error.prefix, "'distribution' must be an object of class 'distribution'"))
        
        if (distribution@n.var==0)
            stop(paste0(error.prefix, "'distribution' must have at least one variable in it"))
        
        if (is.null(distribution@var.names))
            stop(paste0(error.prefix, "'distribution' must contain NAMED variables"))
        
        parameter.names = distribution@var.names
    }
    
    #-- Validate Apply Function --#
    # Make sure the function is a function and only requires arguments 'jheem.engine' and 'parameters' --#
    
    if (!is.function(apply.function))
        stop(paste0("Cannot register apply.parameters.to.engine.function: The value passed to 'apply.function' ",
                    ifelse(is.null(apply.function.name), "", paste0("(", apply.function.name, ") ")),
                    " is not a function"))
    
    fn.args = formals(args(apply.function))
    arg.names = names(fn.args)
    arg.names.without.default.value = arg.names[sapply(fn.args, function(val){val==''})]
    
    # Check that it takes 'model.settings' and 'parameters'
    error.prefix = paste0("Cannot register apply.parameters.to.engine.function: The function passed to 'apply.function' ",
                          ifelse(is.null(apply.function.name), "", paste0("(", apply.function.name, ") ")))
    if (all(arg.names != 'model.settings'))
        stop(paste0(error.prefix, "must take 'model.settings' as an argument"))
    if (all(arg.names != 'parameters'))
        stop(paste0(error.prefix, "must take 'parameters' as an argument"))
    
    # Check that there are no other required arguments
    extraneous.arg.names = setdiff(arg.names.without.default.value, c('model.settings','parameters'))
    if (length(extraneous.arg.names)>0)
        stop(paste0(error.prefix, " requires ",
                    ifelse(length(extraneous.arg.names)==1, 'argument ', 'arguments '),
                    collapse.with.and("'", extraneous.arg.names, "'"),
                    ", but the only arguments to the function should be 'jheem.engine' and 'parameters'"))
    
    #-- Register Them --#
    
    do.register.for.version(version = version,
                            element.name = parameter.names.name,
                            element.value = parameter.names,
                            element.class = 'character',
                            join.with.previous.version.value = join.with.previous.version,
                            join.function = union)
    
    if (type!='set')
    {
        distribution.name = paste0(type, '.parameters.distribution')
        do.register.for.version(version = version,
                                element.name = distribution.name,
                                element.value = distribution,
                                element.class = 'Distribution',
                                join.with.previous.version.value = join.with.previous.version,
                                join.function = join.distributions)
    }
    
    do.register.for.version(version = version,
                            element.name = paste0('apply.', type, '.parameters.function'),
                            element.value = apply.function,
                            element.class = 'function',
                            join.with.previous.version.value = join.with.previous.version,
                            join.function = join.apply.parameters.functions)
}


# Getters (all internal to the package)

# This function is internal to the package

get.parameter.names.for.version <- function(version,
                                            type)
{
    distribution.name = paste0(type, '.parameters')
    do.get.for.version(version=version,
                       element.name = distribution.name,
                       pull.previous.version.value.if.missing = T,
                       allow.null = T)
}

get.parameters.distribution.for.version <- function(version,
                                                    type)
{
    distribution.name = paste0(type, '.parameters.distribution')
    do.get.for.version(version=version,
                       element.name = distribution.name,
                       pull.previous.version.value.if.missing = T,
                       allow.null = T)
}

# This function is internal to the package
get.parameters.apply.function.for.version <- function(version,
                                                      type)
{   
    do.get.for.version(version=version,
                       element.name = paste0('apply.', type, '.parameters.function'),
                       pull.previous.version.value.if.missing = T,
                       allow.null = T)
}

# This function is internal to the package
get.parameter.sampling.blocks.for.version <- function(version)
{
    do.get.for.version(version=version,
                       element.name = 'calibrated.parameters.sampling.blocks',
                       pull.previous.version.value.if.missing = T,
                       allow.null = T)
}

# This function is internal to the package
get.calibrate.from.year.for.version <- function(version)
{
    do.get.for.version(version=version,
                       element.name = 'calibrate.from.year',
                       pull.previous.version.value.if.missing = T,
                       allow.null = T)
}

# This function is internal to the package
get.calibrate.to.year.for.version <- function(version)
{
    do.get.for.version(version=version,
                       element.name = 'calibrate.to.year',
                       pull.previous.version.value.if.missing = T,
                       allow.null = T)
}


##-- GENERIC HELPER GETTER AND SETTER --##

do.register.for.version <- function(version,
                                    element.name,
                                    element.value,
                                    element.class,
                                    element.length=NULL,
                                    require.unique=F,
                                    join.with.previous.version.value=F,
                                    join.function = NULL,
                                    check.join = function(previous.value, element.value){} #prints an error message if appropriate
)
{
    if (all(VERSION.MANAGER$versions!=version))
        stop(paste0("'", version, "' has not been registered as a version"))
    if (all(element.name != VERSION.MANAGER.ELEMENTS))
        stop(paste0("'", element.name, "' is not a valid element.name for version tracking"))
    
    if (!is.null(element.class) && !is(element.value, element.class))
        stop(paste0("The value for '", element.name, "' must be an object of class '", element.class, 
                    "', but its class is: ", 
                    paste0("'", class(element.value), "'", collapse = ' + ')))
    
    if (!is.null(element.length) && length(element.value)!=element.length)
        stop(paste0("The value for '", element.name, "' must be a ", element.class, " vector of length ", 
                    element.length, ", but it has length ", length(element.value)))
    
    if (require.unique)
    {
        other.versions = setdiff(VERSION.MANAGER$versions, version)
        if (length(other.versions)>0)
        {
            matching.mask = sapply(VERSION.MANAGER[[element.name]][other.versions], function(elem){
                elem==element.value
            })
            
            if (any(matching.mask))
                stop(paste0("Cannot register '", element.value, "' as '", element.name, "' for version '",
                            version, "'. It has already been registered for version '",
                            paste0(other.versions[matching.mask], collapse=', '), "."))
        }
    }
    
    if (join.with.previous.version.value)
    {
        previous.versions = get.prior.versions(version, recursive=F)
        if (length(previous.versions)==0)
            stop(paste0("Cannot join ", element.name, " with that of previous version, because there is no previous version registered for '", version, "'"))
        if (length(previous.versions)>1)
            stop(stop(paste0("Cannot join ", element.name, " with that of previous version, because there is more than one previous version for '", 
                             version, "': ",
                             paste0("'", previous.versions, "'", collapse=', '))))
        
        previous.value = do.get.for.version(version=previous.versions,
                                            element.name=element.name)
        
        check.join(previous.value, element.value)
        
        if (is.null(join.function) || !is.function(join.function))
            stop("You must specify a valid function that takes two arguments as the join.function")
        
        orig.element.value = element.value
        element.value = join.function(previous.value, orig.element.value)
        
        if (!is(element.value, element.class))
            stop(paste0("The value for the joined-with-previous '", element.name, "' must be an object of class '", element.class, 
                        "', but its class is: ", 
                        paste0("'", class(element.value), "'", collapse = ' + ')))
        if (!is.null(element.length) && length(element.value)!=element.length)
            stop(paste0("The value for the joined-with-previous '", element.name, "' must be a ", element.class, " vector of length ", 
                        element.length, ", but it has length ", length(element.value)))
    }
    
    VERSION.MANAGER[[element.name]][[version]] = element.value
}

do.get.for.version <- function(version,
                               element.name,
                               pull.previous.version.value.if.missing=F,
                               allow.null=F)
{
    if (!is.character(version) || length(version)!=1 || is.na(version))
        stop("'version' must be a single, non-NA character value")
    
    if (all(VERSION.MANAGER$versions!=version))
    {
        if (allow.null)
            return (NULL)
        else
            stop(paste0("'", version, "' has not been registered as a version"))
    }
    if (all(element.name != VERSION.MANAGER.ELEMENTS))
        stop(paste0("'", element.name, "' is not a valid element.name for version tracking"))
    
    rv = VERSION.MANAGER[[element.name]][[version]]
    if (is.null(rv))
    {
        if (pull.previous.version.value.if.missing)
        {
            previous.versions = get.prior.versions(version, recursive=F)
            if (length(previous.versions)==0 )
            {
                if (allow.null)
                    return (NULL)
                else
                    stop(paste0("Cannot pull ", element.name, " from previous version, because there is no previous version registered for '", version, "'"))
            }
            
            if (length(previous.versions)>1)
                stop(stop(paste0("Cannot pull ", element.name, " from previous version, because there is more than one previous version for '", 
                                 version, "': ",
                                 paste0("'", previous.versions, "'", collapse=', '))))
            
            rv = do.get.for.version(version=previous.versions,
                                    element.name=element.name,
                                    pull.previous.version.value.if.missing=T,
                                    allow.null=T)
            
            if (!allow.null && is.null(rv))
                stop(paste0("No '", element.name, "' for version '", version, 
                            "' or for its parent version ('", previous.versions,
                            "') has been registered with this version manager"))
        }
        else if (!allow.null)
            stop(paste0("No '", element.name, "' for version '", version, "' has been registered with this version manager"))
    }
    
    rv
}


do.remove.for.version <- function(version,
                                  element.name)
{
    if (all(VERSION.MANAGER$versions!=version))
        stop(paste0("'", version, "' has not been registered as a version"))
    if (all(element.name != VERSION.MANAGER.ELEMENTS))
        stop(paste0("'", element.name, "' is not a valid element.name for version tracking"))
    
    VERSION.MANAGER[[element.name]][[version]] = NULL
}

##-- TO DAISY-CHAIN COMPONENTS FUNCTIONS --##

join.apply.parameters.functions <- function(f1, f2)
{
    function(...)
    {
        c(f1(...), f2(...))
    }
}


