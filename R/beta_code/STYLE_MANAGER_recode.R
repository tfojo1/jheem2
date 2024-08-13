if (!exists('default.style.manager.holder'))
{
    default.style.manager.holder = new.env()
    default.style.manager.holder$default.style.manager = NULL
}

##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##

#'@title Get the default style manager
#'
#'@export
get.default.style.manager <- function()
{
    default.data.manager.holder$default.style.manager
}

#'@title Set the default style manager
#'
#'@export
set.default.style.manager <- function(style.manager)
{
    if (!R6::is.R6(style.manager) || !is(style.manager, 'jheem.style.manager'))
        stop("'style.manager' must be an R6 object with class 'jheem.style.manager'")
    default.style.manager.holder$default.style.manager = style.manager
}

#'@title Create a JHEEM Style Manager
#'
#'@param name The name of the style manager
#'@param description A short description
#'
#'@export
create.style.manager <- function(name,
                                 description,
                                 color.sim.by,
                                 linetype.sim.by,
                                 color.data.by,
                                 shape.data.by,
                                 shade.data.by,
                                 size.data.by,
                                 set.as.default = F)
{
    new.style.manager = JHEEM.STYLE.MANAGER$new(name, 
                                                description=description,
                                                color.sim.by = color.sim.by,
                                                linetype.sim.by = linetype.sim.by,
                                                color.data.by = color.data.by,
                                                shape.data.by = shape.data.by,
                                                shade.data.by = shade.data.by,
                                                size.data.by = size.data.by)
    if (set.as.default) default.style.manager.holder$default.style.manager = new.style.manager
    invisible(new.style.manager)
}



##----------------------##
##-- CLASS DEFINITION --##
##----------------------##



JHEEM.STYLE.MANAGER = R6::R6Class(
    'jheem.style.manager',
    
    public = list(
        check = function()
        {
            browser()
        },
        
        initialize = function(name,
                              description,
                              color.sim.by,
                              linetype.sim.by,
                              color.data.by,
                              shape.data.by,
                              shade.data.by,
                              size.data.by)
        {
            error.prefix = "Error creating style manager: "
            # Validate arguments
            # *name* is a single, non-empty, non-NA character value
            if (!is.character(name) || length(name)!=1 || is.na(name) || nchar(name)==0)
                stop(paste0(error.prefix, "'name' must be a single, non-empty, non-NA character value"))
            
            # *description* must be a single, non-empty, non-NA character value
            if (!is.character(description) || length(description)!=1 || is.na(description) || nchar(description)==0)
                stop(paste0(error.prefix, "'description' must be a single, non-empty, non-NA character value"))
            
            # TO DO: VALIDATE AES ARGUMENTS
            private$assign.aes(aes.for.simset, 'aes.for.simset', error.prefix)
            private$assign.aes(aes.for.split.by, 'aes.for.split.by', error.prefix)
            private$assign.aes(aes.for.simset.single.point, 'aes.for.simset.single.point', error.prefix)
            private$assign.aes(aes.for.location, 'aes.for.location', error.prefix)
            
            # Store them
            private$i.name = name
            private$i.color.sim.by = color.sim.by
            private$i.linetype.sim.by = linetype.sim.by
            private$i.color.data.by = color.data.by
            private$i.shape.data.by = shape.data.by
            private$i.shade.data.by = shade.data.by
            private$i.size.data.by = size.data.by
        }
    ),
    
    active = list(
        
        name = function(value)
        {
            if (missing(value))
                private$i.name
            else
                stop("Cannot modify 'name' in jheem.data.manager - it is read-only")
        },
        
        description = function(value)
        {
            if (missing(value))
                private$i.description
            else
                stop("Cannot modify 'description' in jheem.data.manager - it is read-only")
        },
        
        aes.for.simset = function(value)
        {
            if (missing(value))
                private$i.aes.for.simset
            else
                stop("Cannot modify 'aes.for.simset' in jheem.data.manager - it is read-only")
        },
        
        aes.for.split.by = function(value)
        {
            if (missing(value))
                private$i.aes.for.split.by
            else
                stop("Cannot modify 'aes.for.split.by' in jheem.data.manager - it is read-only")
        },
        
        aes.for.simset.single.point = function(value)
        {
            if (missing(value))
                private$i.aes.for.simset.single.point
            else
                stop("Cannot modify 'aes.for.simset.single.point' in jheem.data.manager - it is read-only")
        },
        
        aes.for.location = function(value)
        {
            if (missing(value))
                private$i.aes.for.location
            else
                stop("Cannot modify 'aes.for.location' in jheem.data.manager - it is read-only")
        }
    ),
    
    private = list(
        i.name = NULL,
        i.description = NULL,
        
        i.aes.for.simset = NULL, #linetype
        i.aes.for.split.by = NULL, #color
        i.aes.for.simset.single.point = NULL, #shape
        i.aes.for.location = NULL, #shape
        
        i.linetype.assignment = NULL,
        i.color.assignment = NULL,
        i.shape.point.assignment = NULL,
        i.shape.line.assignment = NULL,
        
        assign.aes = function(aes, purpose, error.prefix) {
            error.prefix = paste0(error.prefix, ": ", "error assigning aesthetic '", aes, "' as ", purpose, ": ")
            
            if (!is.character(aes) || length(aes) != 1 || is.na(aes))
                stop(paste0(error.prefix, "'aes' must be a length one, non-empty character vector"))
            
            # NOTE: aes will be "shape", not "shape.point" or "shape.line"
            
            aes.mapping = c(linetype='i.linetype.assignment',
                            color='i.color.assignment',
                            shape.point = 'i.shape.point.assignment',
                            shape.line = 'i.shape.line.assignment')
            valid.aes.for.purpose = list(
                aes.for.simset = c('linetype', 'color'),
                aes.for.split.by = c('color'),
                aes.for.simset.single.point = c('shape', 'color'),
                aes.for.location = c('shape', 'color')
            )
            if (purpose == 'aes.for.simset.single.point') browser()
            print(paste0(aes, purpose))
            if (!(aes %in% valid.aes.for.purpose[[purpose]]))
                stop(paste0(error.prefix, "not a valid aes for this purpose"))
            if (!is.null(private[[aes.mapping[aes]]]))
                stop(paste0(error.prefix, "already assigned"))
            private[[aes.mapping[aes]]] = purpose
        }
        
    )
)