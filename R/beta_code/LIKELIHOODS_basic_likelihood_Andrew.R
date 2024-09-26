
#'@export
create.likelihood <- function(sim,
                              data.manager, # does the sim know its data manager?
                              outcomes, # does the sim know its outcomes?
                              ontology, # I assume the jheem ontology is accessible?
                              location,
                              years,
                              stratifications,
                              rho.same, # 0.5
                              rho.different, # less than 0.5
                              k,
                              ...
) {
    error.prefix = (paste0("Unable to create likelihood: "))
    
    # *stratifications* is a list ... with no repeats (PLACEHOLDER)
    if (!is.list(stratifications) || FALSE)
        stop(paste0(error.prefix, "'stratifications' must be a list"))
    
    # for the given stratifications, we will want to make the variance-covariance matrix only once
    # so that we don't have to redo it every time we compute the likelihood
    
    # we want to pull the necessary data (observations) according to the stratifications

    # generating the dimension values for the variance-covariance and correlation matrices
    combos = sapply(stratifications, function(strat) {
        # for each stratification, we want every combination of the stratification's dimension values along with a year for each year
        
    })
    
    # make the empty correlation matrix
    cor.matrix = matrix(NA, nrow = length(combos), ncol = length(combos), dimnames = list(first=combos, second=combos))
    
    # fill diagonal with 1; maybe initialize it this way to save a step?
    cor.matrix[diag(cor.matrix)] = 1
    # fill other positions with rho.same or rho.different depending on whether the observations have different collection methods,
    # as determined by the details metadata of the pulled data?
    # which would mean we have to check the source and details for every pair in the combinations list after we pull
    
}


#'@export
register.likelihood <- function(likelihood)
{
    if (!is(likelihood, 'jheem.likelihood') && R6::is.R6(int))
        stop("'likelihood' must be an R6 object with class 'jheem.likelihood'")
    
    # Check likelihood validity
    if (is.null(likelihood$name))
        stop("A likelihood can only be registered if it has a name specified")
    
    if (!is.character(likelihood$name) || length(likelihood$name)!=1 || is.na(likelihood$name) || 
        nchar(likelihood$name)<MINIMUM.INTERVENTION.CODE.NCHAR || nchar(intervention$code)>MAXIMUM.INTERVENTION.CODE.NCHAR)
        stop(paste0("Invalid intervention: 'code' must be a single, non-NA character value with between ",
                    MINIMUM.INTERVENTION.CODE.NCHAR, " and ", MAXIMUM.INTERVENTION.CODE.NCHAR, "letters"))

    # Is something already registered?
    already.registered = get.intervention(intervention$code, throw.error.if.missing = F)
    if (!is.null(already.registered) && !already.registered$equals(intervention))
        stop(paste0("A different intervention has already been registered with the code '", intervention$code, "'"))
    
    # Register it    
    LIKELIHOOD.MANAGER[[likelihood$name]] = likelihood
    
    # For convenience, return back the intervention (invisibly)
    invisible(likelihood)
}


JHEEM.LIKELIHOOD = R6::R6Class(
    'jheem.likelihood',
    
    public = list(
        initialize = function(name,
                              parameters)
        {
            # Validate name
            
            private$i.name = name
            private$i.parameters = parameters
            
            if (!is.null(name))
                register.likelihood(self)
        },
        
        compute = function(sim, log=T)
        {
            stop("The 'compute' function must be overridden in a descendant class")
        },
        
        get.compute.function = function(default.log=T, default.use.optimized.get=F)
        {
            function(sim, log=default.log, use.optimized.get=default.use.optimized.get)
            {
                self$compute(sim, log=log, use.optimized.get=use.optimized.get)
            }
        }
    ),
    
    active = list(
        
        name = function(value)
        {
            if (missing(value))
                private$i.name
            else
                stop("Cannot modify 'name' for a jheem.likelihood - it is read-only")
        },
        
        parameters = function(value)
        {
            if (missing(value))
                private$i.parameters
            else
                stop("Cannot modify 'parameters' for a jheem.likelihood - they are read-only")
        }
    ),
    
    
    private = list(
        
        i.name = NULL,
        i.parameters = NULL
    )
)

JHEEM.COMBINATION.LIKELIHOOD = R6::R6Class(
    'jheem.combination.likelihood',
    inherit = 'jheem.likelihood',
    
    public = list(
        initialize = function(sub.likelihoods,
                              name)
        {
            
            super$initialize(name=name, parameters=NULL)
            private$i.sub.likelihoods = sub.likelihoods
        },
        
        compute = function(sim, log=T, use.optimized.get=F)
        {
            sub.values = sapply(self$sub.likelihoods, function(lik){
                lik$compute(sim, log=log, use.optimized.get = use.optimized.get)
            })
            
            if (log)
                sum(sub.values)
            else
                prod(sub.values)
        }
    ),
    
    active = list(
        
        sub.likelihoods = function(value)
        {
            if (missing(value))
                private$i.sub.likelihoods
            else
                stop("Cannot modify 'sub.likelihoods' for a jheem.combination.likelihood - they are read-only")
        }
    ),
    
    private = list(
        i.sub.likelihoods = NULL
    )
)

JHEEM.SIMPLE.LIKELIHOOD = R6::R6Class(
    'jheem.simple.likelihood',
    inherit = JHEEM.LIKELIHOOD,
    
    public = list(
        
    ),
    
    private = list(
        
    )
)