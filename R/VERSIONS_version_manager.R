

##-- CONSTRUCT the VERSION MANAGER --##

VERSION.MANAGER.ELEMENTS = c(
    'specification', 'compiled.specification',
    'apply.parameters.to.engine.function', 'parameters.prior','parameter.sampling.blocks',
    'apply.projection.parameters.to.engine.function', 'projection.parameters.distribution',
    'prior.versions'
)

VERSION.MANAGER = new.env() #making this an environment allows us to modify by reference within functions

VERSION.MANAGER$versions = character()
VERSION.MANAGER$prior.versions = list()

for (element.name in VERSION.MANAGER.ELEMENTS)
    VERSION.MANAGER[[element.name]] = list()

##--------------------------------------------------##
##-- SETTER and GETTERS for VERSION/SPECIFICATION --##
##--------------------------------------------------##

#'@description Register a JHEEM Model Specification
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

#'@description Get a JHEEM Specification Object
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

##------------------------------------------------------##
##-- SETTER and GETTER for APPLY PARAMETERS to ENGINE --##
##------------------------------------------------------##

#'@description Register a Function to Apply Parameters to a JHEEM Engine
#'
#'@details 
#'
#'@param version The name of a JHEEM version for which a specification has already been registered (using \code{\link{}})
#'@param fn A function which takes two arguments: 'jheem.engine', an object of class jheem.engine, and 'parameters', a named numeric vector. Any return value from the function will be ignored
#'@param join.with.previous.version.function Whether the apply.parameters.to.engine.function should be called first when this function is invoked (in other words, whether this function should 'inherit' the behavior of the previous function)
#'
#'@family JHEEM Version Management Functions
#'
#'@export
register.apply.parameters.to.engine.function <- function(version,
                                                         fn,
                                                         join.with.previous.version.function = T)
{
    #-- Make sure the function is a function and only requires arguments 'jheem.engine' and 'parameters' --#
    
    # First, try to figure out the function's name, so that we can print an intelligible error
    fn.name = deparse(substitute(fn))
    if (!is.character(fn.name) || length(fn.name) != 1)
        fn.name = NULL
    
    if (!is.function(fn))
        stop(paste0("Cannot register apply.parameters.to.engine.function: The value passed to 'fn' ",
                    ifelse(is.null(fn.name), "", paste0("(", fn.name, ") ")),
                    " is not a function"))
    
    fn.args = formals(args(fn))
    arg.names = names(fn.args)
    arg.names.without.default.value = arg.names[sapply(fn.args, function(val){val==''})]
    
    # Check that it takes 'jheem.engine' and 'parameters'
    error.prefix = paste0("Cannot register apply.parameters.to.engine.function: The function passed to 'fn' ",
                           ifelse(is.null(fn.name), "", paste0("(", fn.name, ") ")))
    if (all(arg.names != 'jheem.engine'))
        stop(paste0(error.prefix, "must take 'jheem.engine' as an argument"))
    if (all(arg.names != 'parameters'))
        stop(paste0(error.prefix, "must take 'parameters' as an argument"))
    
    # Check that there are no other required arguments
    extraneous.arg.names = setdiff(arg.names.without.default.value, c('jheem.engine','parameters'))
    if (length(extraneous.arg.names)>0)
        stop(paste0(error.prefix, " requires ",
                    ifelse(length(extraneous.arg.names)==1, 'argument ', 'arguments '),
                    collapse.with.and("'", extraneous.arg.names, "'"),
                    ", but the only arguments to the function should be 'jheem.engine' and 'parameters',"))
    
    #-- Register It --#
    do.register.for.version(version = version,
                            element.name = 'apply.parameters.to.engine.function',
                            element.value = fn,
                            element.class = 'function',
                            join.with.previous.version.value = join.with.previous.version.function,
                            join.function = create.joint.function)
}

#'@export
get.apply.parameters.to.engine.function <- function(version,
                                                    include.parameters.check=T,
                                                    pull.previous.version.value.if.missing = T)
{
    fn = do.get.for.version(version=version,
                            element.name='apply.parameters.to.engine.function',
                            pull.previous.version.value.if.missing = pull.previous.version.value.if.missing)
    
    if (include.parameters.check)
    {
        param.names = get.parameters.prior.for.version(version)@var.names
        function(parameters, components, data.managers = ALL.DATA.MANAGERS) {
            missing.parameters = setdiff(param.names, names(parameters))
            if (length(missing.parameters)>0)
                stop(paste0("Missing values in parameters vector: ",
                            paste0("'", missing.parameters, "'", collapse=', '),
                            ". Cannot execute get.components function for version '", version, "'"))
            
            fn(parameters, components, data.managers)
        }
    }
    else
        fn
}

#'@export
get.parameters.prior.for.version <- function(version,
                                             pull.previous.version.value.if.missing = T)
{
    do.get.for.version(version=version,
                       element.name='parameters.prior',
                       pull.previous.version.value.if.missing = pull.previous.version.value.if.missing)
}

#'@export
get.parameter.sampling.blocks.for.version <- function(version,
                                                      pull.previous.version.value.if.missing = T)
{
    do.get.for.version(version=version,
                       element.name='parameter.sampling.blocks',
                       pull.previous.version.value.if.missing = pull.previous.version.value.if.missing)
}

#'@export
get.projection.prarameters.distribution.for.version <- function(version, 
                                                                pull.from.previous.version.if.missing=T)
{
    do.get.for.version(version=version,
                       element.name='projection.parameters.distribution',
                       pull.previous.version.value.if.missing = pull.from.previous.version.if.missing)
}

#'@export
get.apply.projection.parameters.to.engine.function <- function(version, 
                                                                  include.parameters.check=T,
                                                                  pull.from.previous.version.if.missing=T)
{
    fn = do.get.for.version(version=version,
                            element.name='apply.projection.parameters.to.engine.function',
                            pull.previous.version.value.if.missing = pull.from.previous.version.if.missing)
    
    
    if (include.parameters.check)
    {
        param.names = get.projection.prarameters.distribution.for.version(version)@var.names
        function(parameters, components, data.managers = ALL.DATA.MANAGERS) {
            missing.parameters = setdiff(param.names, names(parameters))
            if (length(missing.parameters)>0)
                stop(paste0("Missing values in parameters vector: ",
                            paste0("'", missing.parameters, "'", collapse=', '),
                            ". Cannot execute get.projection.update.components function for version '", version, "'"))
            
            fn(parameters, components, data.managers)
        }
    }
    else
        fn
}


##-- SPECIFIC SETTERS --##

#'@export
register.parameters.prior <- function(version,
                                      prior,
                                      join.with.previous.version.prior = F)
{
    check.join.function = function(dist1, dist2)
    {
        if (length(intersect(dist1@var.names, dist2@var.names)))
            stop(paste0("Cannot join prior distribution with that of previous version, because they have overlapping variable name(s): ",
                        paste0("'", intersect(dist1@var.names, dist2@var.names), "'", collapse=', ')))
    }
    
    do.register.for.version(version=version,
                            element.name='parameters.prior',
                            element.value=prior,
                            element.class='Distribution',
                            join.with.previous.version.value = join.with.previous.version.prior,
                            join.function = join.distributions,
                            check.join = check.join.function
    )
}

#'@export
register.parameter.sampling.blocks <- function(version,
                                               blocks,
                                               join.with.previous.version.sampling.blocks=F)
{
    do.register.for.version(version=version,
                            element.name='parameter.sampling.blocks',
                            element.value=blocks,
                            element.class='list',
                            join.with.previous.version.value = join.with.previous.version.sampling.blocks,
                            join.function=c)
}


#'@export
register.projection.update.components.function <- function(version,
                                                           fn,
                                                           join.with.previous.version.function=F)
{
    do.register.for.version(version=version,
                            element.name='projection.update.components.function',
                            element.value=fn,
                            element.class='function',
                            join.with.previous.version.value = join.with.previous.version.function,
                            join.function = join.get.components.functions)
}

#'@export
register.projection.parameters.distribution <- function(version,
                                                        distribution,
                                                        join.with.previous.version.distribution = F)
{
    check.join.function = function(dist1, dist2)
    {
        if (length(intersect(dist1@var.names, dist2@var.names)))
            stop(paste0("Cannot join projection parameters distribution with that of previous version, because they have overlapping variable name(s): ",
                        paste0("'", intersect(dist1@var.names, dist2@var.names), "'", collapse=', ')))
    }
    
    do.register.for.version(version=version,
                            element.name='projection.parameters.distribution',
                            element.value=distribution,
                            element.class='Distribution',
                            join.with.previous.version.value = join.with.previous.version.distribution,
                            join.function = join.distributions,
                            check.join = check.join.function
    )
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
    if (all(VERSION.MANAGER$versions!=version))
        stop(paste0("'", version, "' has not been registered as a version"))
    if (all(element.name != VERSION.MANAGER.ELEMENTS))
        stop(paste0("'", element.name, "' is not a valid element.name for version tracking"))
    
    rv = VERSION.MANAGER[[element.name]][[version]]
    if (is.null(rv))
    {
        if (pull.previous.version.value.if.missing)
        {
            previous.versions = get.prior.versions(version, recursive=F)
            if (length(previous.versions)==0)
                stop(paste0("Cannot pull ", element.name, " from previous version, because there is no previous version registered for '", version, "'"))
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

create.joint.functions <- function(f1, f2)
{
    function(...)
    {
        f1(...)
        f2(...)
    }
}


