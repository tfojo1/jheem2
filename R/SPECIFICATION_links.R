

# A 'public' function within in the package

get.link <- function(link,
                     min=NA,
                     max=NA,
                     variable.name.for.error='link',
                     error.prefix='Cannot get link: ')
{
    if (!is.character(link) || length(link)!=1 || is.na(link))
        stop(paste0(error.prefix, "'", variable.name.for.error,
                    "' must be a single, non-NA character value"))

    if (all(link != names(LINK.INITIALIZERS)))
        stop(paste0(error.prefix, "Invalid ", variable.name.for.error, " '", link,
                    "'. Must be one of ", collapse.with.or("'", names(LINK.INITIALIZERS), "'")))
    
    LINK.INITIALIZERS[[link]]$new(min=min, max=max, error.prefix=error.prefix)
}


##----------------------------##
##----------------------------##
##-- LINK CLASS DEFINITIONS --##
##----------------------------##
##----------------------------##

LINK = R6::R6Class(
    'link',
    
    public = list(
        initialize = function(type,
                              min, max,
                              default.min, default.max,
                              min.min = default.min, 
                              min.min.inclusive=T,
                              max.max = default.max,
                              max.max.inclusive=T,
                              error.prefix='')
        {
            # Check type
            if (!is.character(type) || length(type)!=1 || is.na(type))
                stop(paste0(error.prefix, "Link 'type' must be a single, non-NA, non-empty character value"))

            # Check min.min and max.max
            if (!is.numeric(min.min) || length(min.min)!=1 || is.na(min.min))
                stop(paste0(error.prefix, "Link 'min.min' must be a single, non-NA, non-empty numeric value"))
            
            if (!is.numeric(max.max) || length(max.max)!=1 || is.na(max.max))
                stop(paste0(error.prefix, "Link 'max.max' must be a single, non-NA, non-empty numeric value"))
            
            if (max.max <= min.min)
                stop(paste0(error.prefix, "Link 'max.max' must greater than 'min.min'"))

            # Check min and max
            if (length(min)==1 && is.na(min))
                min = default.min
            if (!is.numeric(min))
                stop(paste0(error.prefix,
                            "'min' must be a single *numeric* value"))
            if (length(min) != 1)
                stop(paste0(error.prefix,
                            "'min' must be a SINGLE numeric value"))
            
            if (length(max)==1 && is.na(max))
                max = default.max
            if (!is.numeric(max))
                stop(paste0(error.prefix,
                            "'max' must be a single *numeric* value"))
            if (length(max) != 1)
                stop(paste0(error.prefix,
                            "'max' must be a SINGLE numeric value"))
            
            if (min.min.inclusive)
            {
                if (min < min.min)
                    stop(paste0(error.prefix,
                                "For ", type, " links, min must be greater than or equal to ",
                                min.min))
            }
            else
            {
                if (min <= min.min)
                {
                    if (is.infinite(min.min))
                        stop(paste0(error.prefix,
                                    "For ", type, " links, min must be  finite"))
                    else
                        stop(paste0(error.prefix,
                                    "For ", type, " links, min must be greater than ",
                                    min.min))
                }
            }
            
            if (max.max.inclusive)
            {
                if (max > max.max)
                    stop(paste0(error.prefix,
                                "For ", type, " links, max must be less than or equal to ",
                                max.max))
            }
            else
            {
                if (max >= max.max)
                {
                    if (is.infinite(max.max))
                        stop(paste0(error.prefix,
                                    "For ", type, " links, max must be  finite"))
                    else
                        stop(paste0(error.prefix,
                                    "For ", type, " links, max must be less than ",
                                    max.max))
                }
            }
            
            if (max <= min)
                stop(paste0(error.prefix,
                            "'max' (", round(max, 4),
                            ") must be greater than 'min' (",
                            round(min, 4),
                            ")"))
            
            # Store the values
            private$i.type = type
            
            private$i.min = min
            private$i.max = max
            
            private$i.default.min = default.min
            private$i.default.max = default.max
        },
        
        print = function(...)
        {
            first.letter = tolower(substr(private$i.type, 1, 1))
            begins.with.vowel = first.letter=='a' || first.letter=='e' || 
                first.letter=='i' || first.letter=='o' || first.letter=='u'
            
            msg = paste0(ifelse(begins.with.vowel, 'An ', 'A '),
                         private$i.type, " link")
            
            if (private$i.min != private$i.default.min || private$i.max != private$i.default.max)
            {
                if (private$i.min == -Inf)
                    msg = paste0(msg, " bounded to be less than or equal to ", round(private$i.max, 4))
                else if (private$i.max == Inf)
                    msg = paste0(msg, " bounded to be greater than or equal to ", round(private$i.min, 4))
                else
                    msg = paste0(msg, " bounded between ", round(private$i.min, 4), " and ", round(private$i.max, 4))
            }
            
            print(msg)
        },
        
        apply = function(values)
        {
            stop("Incompletely specified link subclass - the 'apply' method must be specified at the subclass level")
        },
        
        reverse.apply = function(values)
        {
            stop("Incompletely specified link subclass - the 'reverse.apply' method must be specified at the subclass level")
        },
        
        check.untransformed.values = function(values,
                                              variable.name.for.error=NULL,
                                              error.prefix='')
        {
            if (!is.numeric(values))
            {
                if (is.null(variable.name.for.error))
                    stop(paste0(error.prefix, "'values' must be a numeric object"))
                else
                    stop(paste0(error.prefix, "'", variable.name.for.error, "' must be a numeric object"))
            }
            
            if (any(!is.na(values) & (values<private$i.min | values>private$i.max)))
            {
                if (private$i.min==-Inf)
                    msg = paste0("less than or equal to ", round(private$i.max, 4))
                else if (private$i.max==Inf)
                    msg = paste0("greater than or equal to ", round(private$i.min, 4))
                else
                    msg = paste0("between ", round(private$i.min, 4), " and ", round(private$i.max, 4))
                
                if (!is.null(variable.name.for.error))
                    stop(paste0(error.prefix, "'", variable.name.for.error, "' must be ", msg))
                else if (private$i.min==private$i.default.min && private$i.max==private$i.default.max)
                {
                    stop(paste0(error.prefix,
                                "Values for a ", private$i.name, " link must be ",))
                }
                else
                {
                    stop(paste0(error.prefix,
                                "For this bounded ", private$i.name, " link, values must be ", msg))
                }
            }
        },
        
        # Gets the link that gives a transformation for
        # eg, for a model with the logistic link, the coefficients (ORs) would use a log link (since on the additive scale they are log ORs)
        get.coefficient.link = function()
        {
            get.link(private$i.type)
        },
        
        equals = function(other.link)
        {
            is(other.link, 'link') && other.link$type == private$i.type &&
                other.link$min == private$i.min && other.link$max == private$i.max
        }
    ),
    
    active = list(
        
        type = function(value)
        {
            if (missing(value))
                private$i.type
            else
                stop("Cannot modify 'type' for a link object - it is read-only")
        },
        
        min = function(value)
        {
            if (missing(value))
                private$i.min
            else
                stop("Cannot modify 'min' for a link object - it is read-only")
        },
        
        max = function(value)
        {
            if (missing(value))
                private$i.max
            else
                stop("Cannot modify 'max' for a link object - it is read-only")
        }
    ),
    
    private = list(
        i.type = NULL,
        
        i.min = NULL,
        i.max = NULL,
        
        i.default.min = NULL,
        i.default.max = NULL
    )
)

LOGISTIC.LINK = R6::R6Class(
    'logistic.link',
    inherit = LINK,
    
    public = list(
        
        initialize = function(min, max, error.prefix='')
        {
            super$initialize(type = 'logistic',
                             min = min,
                             max = max,
                             default.min = 0,
                             default.max = 1,
                             min.min = -Inf,
                             min.min.inclusive = F,
                             max.max = Inf,
                             max.max.inclusive = F,
                             error.prefix = error.prefix)
        },
        
        apply = function(values)
        {
            log( (values-private$i.min) / (private$i.max-private$i.min) ) -
                log( 1 - (values-private$i.min) / (private$i.max-private$i.min) )
        },
        
        reverse.apply = function(values)
        {
            private$i.min + (private$i.max-private$i.min) /
                ( 1 + exp(-values) )
        },
        
        get.coefficient.link = function()
        {
            get.link('log')
        }
    )
)

LOG.LINK = R6::R6Class(
    'log.link',
    inherit = LINK,
    
    public = list(
        
        initialize = function(min, max, error.prefix = '')
        {
            super$initialize(type = 'log',
                             min = min,
                             max = max,
                             default.min = 0,
                             default.max = Inf,
                             min.min = -Inf,
                             min.min.inclusive = F,
                             error.prefix = error.prefix)
        },
        
        apply = function(values)
        {
            log( (values-private$i.min) )
        },
        
        reverse.apply = function(values)
        {
            pmin(private$i.max, private$i.min + exp(values))
        }
    )
)

IDENTITY.LINK = R6::R6Class(
    'identity.link',
    inherit = LINK,
    
    public = list(
        
        initialize = function(min, max, error.prefix = '')
        {
            super$initialize(type = 'identity',
                             min = min,
                             max = max,
                             default.min = -Inf,
                             default.max = Inf,
                             error.prefix = error.prefix)
        },
        
        apply = function(values)
        {
            values
        },
        
        reverse.apply = function(values)
        {
            pmin(private$i.max, pmax(private$i.min, values))
        }
    )
)

LINK.INITIALIZERS = list(
    'identity' = IDENTITY.LINK,
    'log' = LOG.LINK,
    'logistic' = LOGISTIC.LINK,
    'logit' = LOGISTIC.LINK
)
