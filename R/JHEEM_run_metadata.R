

create.single.run.metadata <- function(run.time,
                                       preprocessing.time,
                                       diffeq.time,
                                       postprocessing.time,
                                       n.trials,
                                       n.diffeq.evaluations,
                                       labels = NULL)
{
    if (!is.null(labels))
    {
        dim.names = list(run=labels, sim=1:(length(run.time)/length(labels)))
        
        dim(run.time) = dim(preprocessing.time) = dim(diffeq.time) = dim(postprocessing.time) = dim(n.trials) = dim(n.diffeq.evaluations) = sapply(dim.names, length)
        dimnames(run.time) = dimnames(preprocessing.time) = dimnames(diffeq.time) = dimnames(postprocessing.time) = dimnames(n.trials) = dimnames(n.diffeq.evaluations) = dim.names
    }
    
    JHEEM.RUN.METADATA$new(run.time = run.time,
                           preprocessing.time = preprocessing.time,
                           diffeq.time = diffeq.time,
                           postprocessing.time = postprocessing.time,
                           n.trials = n.trials,
                           n.diffeq.evaluations = n.diffeq.evaluations)
}

copy.run.metadata <- function(metadata.to.copy,
                              run.time = metadata.to.copy$run.time,
                              preprocessing.time = metadata.to.copy$preprocessing.time,
                              diffeq.time = metadata.to.copy$diffeq.time,
                              postprocessing.time = metadata.to.copy$postprocessing.time,
                              n.trials = metadata.to.copy$n.trials,
                              n.diffeq.evaluations = metadata.to.copy$n.diffeq.evaluations)
{
    if (!is(metadata.to.copy, 'jheem.run.metadata'))
        stop("Cannot copy run metadata: 'metadata.to.copy' must be a 'jheem.run.metadata' object")
    
    dim.names = NULL
    if (is.null(dim.names))
        dim.names = dimnames(run.time)
    if (is.null(dim.names))
        dim.names = dimnames(preprocessing.time)
    if (is.null(dim.names))
        dim.names = dimnames(diffeq.time)
    if (is.null(dim.names))
        dim.names = dimnames(postprocessing.time)
    if (is.null(dim.names))
        dim.names = dimnames(n.trials)
    if (is.null(dim.names))
        dim.names = dimnames(n.diffeq.evaluations)
    if (is.null(dim.names))
        stop("Cannot copy.run.metadata() - the dimnames for at least one of run.time, preprocessing.time, diffeq.time, postprocessing.time, n.trials, or n.diffeq.evaluations must be set")
    
    if (is.null(n.diffeq.evaluations)) # for backward compatibility with sims that did not track this
        n.diffeq.evaluations = -1
    
    dim(run.time) = dim(preprocessing.time) = dim(diffeq.time) =
        dim(postprocessing.time) = dim(n.trials) = dim(n.diffeq.evaluations) = 
        sapply(dim.names, length)
    
    dimnames(run.time) = dimnames(preprocessing.time) = dimnames(diffeq.time) =
        dimnames(postprocessing.time) = dimnames(n.trials) = dimnames(n.diffeq.evaluations) =
        dim.names

    
    JHEEM.RUN.METADATA$new(run.time = run.time,
                           preprocessing.time = preprocessing.time,
                           diffeq.time = diffeq.time,
                           postprocessing.time = postprocessing.time,
                           n.trials = n.trials,
                           n.diffeq.evaluations = n.diffeq.evaluations)
}

join.run.metadata <- function(metadata.to.join)
{
    n.sim = sum(unlist(lapply(metadata.to.join, function(run.metadata){
        run.metadata$n.sim
    })))
    
    run.time = unlist(lapply(metadata.to.join, function(run.metadata){
        run.metadata$run.time
    }))
    
    preprocessing.time = unlist(lapply(metadata.to.join, function(run.metadata){
        run.metadata$preprocessing.time
    }))
    
    diffeq.time = unlist(lapply(metadata.to.join, function(run.metadata){
        run.metadata$diffeq.time
    }))
    
    postprocessing.time = unlist(lapply(metadata.to.join, function(run.metadata){
        run.metadata$postprocessing.time
    }))
    
    n.trials = unlist(lapply(metadata.to.join, function(run.metadata){
        run.metadata$n.trials
    }))
    
    n.diffeq.evaluations = unlist(lapply(metadata.to.join, function(run.metadata){
        run.metadata$n.diffeq.evaluations
    }))
    
    dim.names = list(run = metadata.to.join[[1]]$run.labels,
                     sim = 1:n.sim)
    
    dim(run.time) = dim(preprocessing.time) = dim(diffeq.time) = dim(postprocessing.time) = dim(n.trials) = dim(n.diffeq.evaluations) = sapply(dim.names, length)
    dimnames(run.time) = dimnames(preprocessing.time) = dimnames(diffeq.time) = dimnames(postprocessing.time) = dimnames(n.trials) = dimnames(n.diffeq.evaluations) = dim.names
    
    JHEEM.RUN.METADATA$new(run.time = run.time,
                           preprocessing.time = preprocessing.time,
                           diffeq.time = diffeq.time,
                           postprocessing.time = postprocessing.time,
                           n.trials = n.trials,
                           n.diffeq.evaluations = n.diffeq.evaluations)
}

append.run.metadata <- function(metadata1, metadata2)
{
    if (is.null(metadata1))
        metadata2
    else if (is.null(metadata2))
        metadata1
    else
        JHEEM.RUN.METADATA$new(run.time = rbind(metadata1$run.time, metadata2$run.time),
                               preprocessing.time = rbind(metadata1$preprocessing.time, metadata2$preprocessing.time),
                               diffeq.time = rbind(metadata1$diffeq.time, metadata2$diffeq.time),
                               postprocessing.time = rbind(metadata1$postprocessing.time, metadata2$postprocessing.time),
                               n.trials = rbind(metadata1$n.trials, metadata2$n.trials),
                               n.diffeq.evaluations = rbind(metadata1$n.diffeq.evaluations, metadata2$n.diffeq.evaluations))
}

JHEEM.RUN.METADATA = R6::R6Class(
    'jheem.run.metadata',
    
    public = list(
        
        initialize = function(run.time,
                              preprocessing.time,
                              diffeq.time,
                              postprocessing.time,
                              n.trials,
                              n.diffeq.evaluations = NULL)
        {
            if (!is.numeric(run.time) || any(is.na(run.time)))
                stop("Cannot create run metadata: 'run.time' must be a numeric array with no NA values")
            if (!length(dim(run.time)==2))
                stop("Cannot create run metadata: 'run.time' must be a numeric array with no NA values")
            
            if (!is.numeric(preprocessing.time) || any(is.na(preprocessing.time)))
                stop("Cannot create run metadata: 'preprocessing.time' must be a numeric array with no NA values")
            if (!length(dim(preprocessing.time)==2))
                stop("Cannot create run metadata: 'preprocessing.time' must be a numeric array with no NA values")
            
            if (!is.numeric(diffeq.time) || any(is.na(diffeq.time)))
                stop("Cannot create run metadata: 'diffeq.time' must be a numeric array with no NA values")
            if (!length(dim(diffeq.time)==2))
                stop("Cannot create run metadata: 'diffeq.time' must be a numeric array with no NA values")
            
            if (!is.numeric(postprocessing.time) || any(is.na(postprocessing.time)))
                stop("Cannot create run metadata: 'postprocessing.time' must be a numeric array with no NA values")
            if (!length(dim(postprocessing.time)==2))
                stop("Cannot create run metadata: 'postprocessing.time' must be a numeric array with no NA values")
            
            if (!is.numeric(n.trials) || any(is.na(n.trials)))
                stop("Cannot create run metadata: 'n.trials' must be a numeric array with no NA values")
            if (!length(dim(n.trials)==2))
                stop("Cannot create run metadata: 'n.trials' must be a numeric array with no NA values")
            
            # for backward compatibility with sims that did not track this
            if (is.null(n.diffeq.evaluations))
            {
                n.diffeq.evaluations = n.trials
                n.diffeq.evaluations[] = -1
            }
            else
            {
                if (!is.numeric(n.diffeq.evaluations) || any(is.na(n.diffeq.evaluations)))
                    stop("Cannot create run metadata: 'n.diffeq.evaluations' must be a numeric array with no NA values")
                if (!length(dim(n.diffeq.evaluations)==2))
                    stop("Cannot create run metadata: 'n.diffeq.evaluations' must be a numeric array with no NA values")
            }
            
            private$i.run.time = run.time
            private$i.preprocessing.time = preprocessing.time
            private$i.diffeq.time = diffeq.time
            private$i.postprocessing.time = postprocessing.time
            private$i.n.trials = n.trials
            private$i.n.diffeq.evaluations = n.diffeq.evaluations
        },
        
        subset = function(x)
        {
            JHEEM.RUN.METADATA$new(run.time = private$i.run.time[,x, drop=F],
                                   preprocessing.time = private$i.preprocessing.time[,x, drop=F],
                                   diffeq.time = private$i.diffeq.time[,x, drop=F],
                                   postprocessing.time = private$i.postprocessing.time[,x, drop=F],
                                   n.trials = private$i.n.trials[,x, drop=F],
                                   n.diffeq.evaluations = private$i.n.diffeq.evaluations[,x, drop=F])
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
                private$i.diffeq.time
            else
                stop("Cannot modify a run.metadata's 'diffeq.time' - it is read-only")
        },
        
        other.time = function(value)
        {
            if (missing(value))
                private$i.run.time - private$i.preprocessing.time - private$i.diffeq.time - private$i.postprocessing.time
            else
                stop("Cannot modify a run.metadata's 'other.time' - it is read-only")
        },
        
        n.trials = function(value)
        {
            if (missing(value))
                private$i.n.trials
            else
                stop("Cannot modify a run.metadata's 'n.trials' - it is read-only")
        },
        
        n.diffeq.evaluations = function(value)
        {
            if (missing(value))
                private$i.n.diffeq.evaluations
            else
                stop("Cannot modify a run.metadata's 'n.diffeq.evaluations' - it is read-only")
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
                dimnames(private$i.run.time)[[1]]
            else
                stop("Cannot modify a run.metadata's 'run.labels' - it is read-only")
        }
    ),
    
    private = list(
        
        i.run.time = NULL,
        i.preprocessing.time = NULL,
        i.diffeq.time = NULL,
        i.postprocessing.time = NULL,
        
        i.n.trials = NULL,
        i.n.diffeq.evaluations = NULL
    )
)