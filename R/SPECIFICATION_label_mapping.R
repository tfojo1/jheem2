
#'@export 
get.jheem.label.mapping <- function(version, location)
{
    
}

JHEEM.LABEL.MAPPING <- R6::R6Class(
    'jheem.label.mapping',
    portable = F,
    
    public = list(
        
        # A 'dumb' constructor
        initialize = function(labels)
        {
            private$i.labels = labels
        },
        
        get.labels = function(to.label)
        {
            rv = private$i.labels[to.label]
            if (any(is.na(rv)))
                rv[is.na(rv)] = str.to.title(to.label[is.na(rv)]) #defined in HELPERS_misc_helpers.R
            
            rv
        }
    ),
    
    active = list(
        
        labels = function(value)
        {
            if (missing(value))
                private$i.labels
            else
                stop("Cannot modify a model.setting's labels - they are read-only")
        }
        
    ),
    
    private = list(
        
        i.labels = NULL
        
    )
)

