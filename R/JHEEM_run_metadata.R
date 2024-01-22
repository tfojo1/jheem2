

create.single.run.metadata <- function(run.time,
                                       preprocessing.time,
                                       postprocessing.time,
                                       n.trials,
                                       labels = NULL)
{
    if (!is.null(labels))
    {
        dim.names = list(run=labels, sim=1:(length(run.time)/length(labels)))
        
        dim(run.time) = dim(preprocessing.time) = dim(postprocessing.time) = dim(n.trials) = sapply(dim.names, length)
        dimnames(run.time) = dimnames(preprocessing.time) = dimnames(postprocessing.time) = dimnames(n.trials) = dim.names
    }
    
    JHEEM.RUN.METADATA$new(run.time = run.time,
                           preprocessing.time = preprocessing.time,
                           postprocessing.time = postprocessing.time,
                           n.trials = n.trials)
}

join.run.metadata <- function(metadata.to.join)
{
    n.sim = sum(sapply(metadata.to.join, function(run.metadata){
        run.metadata$n.sim
    }))
    
    run.time = sapply(metadata.to.join, function(run.metadata){
        run.metadata$run.time
    })
    
    preprocessing.time = sapply(metadata.to.join, function(run.metadata){
        run.metadata$preprocessing.time
    })
    
    postprocessing.time = sapply(metadata.to.join, function(run.metadata){
        run.metadata$postprocessing.time
    })
    
    n.trials = sapply(metadata.to.join, function(run.metadata){
        run.metadata$n.trials
    })
    
    dim.names = list(run = metadata.to.join[[1]]$run.labels,
                     sim = 1:n.sim)
    
    dim(run.time) = dim(preprocessing.time) = dim(postprocessing.time) = dim(n.trials) = sapply(dim.names, length)
    dimnames(run.time) = dimnames(preprocessing.time) = dimnames(postprocessing.time) = dimnames(n.trials) = dim.names
    
    JHEEM.RUN.METADATA$new(run.time = run.time,
                           preprocessing.time = preprocessing.time,
                           postprocessing.time = postprocessing.time,
                           n.trials = n.trials)
}

JHEEM.RUN.METADATA = R6::R6Class(
    'jheem.run.metadata',
    
    public = list(
        
        initialize = function(run.time,
                              preprocessing.time,
                              postprocessing.time,
                              n.trials)
        {
            if (!is.numeric(run.time) || any(is.na(run.time)))
                stop("Cannot create run metadata: 'run.time' must be a numeric array with no NA values")
            if (!length(dim(run.time)==2))
                stop("Cannot create run metadata: 'run.time' must be a numeric array with no NA values")
            
            if (!is.numeric(preprocessing.time) || any(is.na(preprocessing.time)))
                stop("Cannot create run metadata: 'preprocessing.time' must be a numeric array with no NA values")
            if (!length(dim(preprocessing.time)==2))
                stop("Cannot create run metadata: 'preprocessing.time' must be a numeric array with no NA values")
            
            if (!is.numeric(postprocessing.time) || any(is.na(postprocessing.time)))
                stop("Cannot create run metadata: 'postprocessing.time' must be a numeric array with no NA values")
            if (!length(dim(postprocessing.time)==2))
                stop("Cannot create run metadata: 'postprocessing.time' must be a numeric array with no NA values")
            
            if (!is.numeric(n.trials) || any(is.na(n.trials)))
                stop("Cannot create run metadata: 'n.trials' must be a numeric array with no NA values")
            if (!length(dim(n.trials)==2))
                stop("Cannot create run metadata: 'n.trials' must be a numeric array with no NA values")
            
            private$i.run.time = run.time
            private$i.preprocessing.time = preprocessing.time
            private$i.postprocessing.time = postprocessing.time
            private$i.n.trials = n.trials
        },
        
        subset = function(x)
        {
            JHEEM.RUN.METADATA$new(run.time = run.time[,x, drop=F],
                                   preprocessing.time = preprocessing.time[,x, drop=F],
                                   postprocessing.time = postprocessing.time[,x, drop=F],
                                   n.trials = n.trials[,x, drop=F])
        }
        
    ),
    
    active = list(
        
        run.time = function(value)
        {
            if (missing(value))
                private$i.run.time
            else
                stop("Cannot modify a run.metadata's 'run.time' - it is read-only")
        },
        
        preprocessing.time = function(value)
        {
            if (missing(value))
                private$i.preprocessing.time
            else
                stop("Cannot modify a run.metadata's 'preprocessing.time' - it is read-only")
        },
        
        postprocessing.time = function(value)
        {
            if (missing(value))
                private$i.postprocessing.time
            else
                stop("Cannot modify a run.metadata's 'postprocessing.time' - it is read-only")
        },
        
        diffeq.time = function(value)
        {
            if (missing(value))
                private$i.run.time - private$i.preprocessing.time - private$i.postprocessing.time
            else
                stop("Cannot modify a run.metadata's 'diffeq.time' - it is read-only")
        },
        
        n.trials = function(value)
        {
            if (missing(value))
                private$i.n.trials
            else
                stop("Cannot modify a run.metadata's 'n.trials' - it is read-only")
        },
        
        n.sim = function(value)
        {
            if (missing(value))
                dim(private$i.run.time)[2]
            else
                stop("Cannot modify a run.metadata's 'n.sim' - it is read-only")
        },
        
        run.labels = function(value)
        {
            if (missing(value))
                dimname(private$i.run.time)[[1]]
            else
                stop("Cannot modify a run.metadata's 'run.labels' - it is read-only")
        }
    ),
    
    private = list(
        
        i.run.time = NULL,
        i.preprocessing.time = NULL,
        i.postprocessing.time = NULL,
        
        i.n.trials = NULL
        
    )
)