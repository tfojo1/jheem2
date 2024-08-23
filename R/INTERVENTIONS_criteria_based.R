
#'@name Create a criterion for monotonic parameter, criteria-based interventions
#'
#'@param parameter.name The name of the parameter to be modified to achieve the outcome
#'@param outcome A character vector indicating the name of the outcome to which the criterion applies
#'
#'@param parameter.scale The scale on which the parameter operates
#'@param parameter.initial.value Either a single scalar numeric or a function that takes argument sim and returns a single scalar numeric
#'
#'@param target.value A single, numeric value that we want the outcome to get to
#'@param min.acceptable.value,max.acceptable.value Indicators of the min and max possible values of the outcome we would be willing to accept. Must be either (1) single, numeric values or (2) numeric vectors. In this case, the first values are the min/max threshold until subsequent.thresholds.apply.after.iteration[1], the second values are the thresholds prior to subsequent.thresholds.apply.after.iteration[2], etc
#'@param subsequent.thresholds.apply.after.iteration The number of iterations after which the 2nd, 3rd, etc thresholds in min.acceptable.value, max.acceptable.value apply
#'@param dimension.values The dimension values for which we should pull the outcome. Must be a named list of either character vectors, integer vectors, or logical vectors
#'@param ... An alternative way of supplying dimensions values - must be named character vectors, integer vectors, or logical vectors
#'@param draw.parameters.from.previous.sims Whether the initial value for the parameter after the first simulation in a set should be the given initial value, or should be based on what worked for previous simulations in the set
#'
#'@export
create.monotonic.criterion <- function(parameter.name,
                                       outcome,
                                       
                                       parameter.scale,
                                       parameter.initial.value,
                                       
                                       target.value,
                                       min.acceptable.value,
                                       max.acceptable.value,
                                       subsequent.thresholds.apply.after.iteration=numeric(),
                                       dimension.values = list(), 
                                       
                                       draw.parameter.from.previous.sims = !is.function(parameter.initial.value),
                                       ...)
{
    MONOTONIC.OUTCOME.INTERVENTION.CRITERION$new(parameter.name = parameter.name,
                                                 outcome = outcome,
                                                 
                                                 parameter.scale = parameter.scale,
                                                 parameter.initial.value = parameter.initial.value,
                                                 
                                                 target.value = target.value,
                                                 min.acceptable.value = min.acceptable.value,
                                                 max.acceptable.value = max.acceptable.value,
                                                 subsequent.thresholds.apply.after.iteration = subsequent.thresholds.apply.after.iteration,
                                                 dimension.values = dimension.values,
                                                 
                                                 draw.parameter.from.previous.sims = !is.function(parameter.initial.value),
                                                 ...)
}

#'@name Create a "Guess-and-Check" Intervention that Must Satisfy Some Criteria by Varying Parameters
#'
#'@param base.intervention A single.iteration.intervention, as created by \code{\link{create.intervention}} or \code{\link{join.interventions}}
#'@param completion.criteria Either a single jheem.intervention.criterion object, as created by \code{\link{create.intervention.criterion}} or a list containing only jheem.intervention.criterion objects
#'@param max.iterations The maximum number of iterations to run in trying to satisfy intervention criteria. If criteria are not satisfied after max.iterations, will 'give up' (and either produce degenerate simulations or throw an error, depending on max.failure.rate)
#'@param n.iterations.after.satisfying.criteria The number of iterations to run after we have satisfied the criteria to try to get as close as possible to the target
#'@param max.failure.rate The maximum proportion of simulations in a simulation set which can fail to find a solution that satisfies intervention criteria within max.iterations. If this threshold is exceeded, will throw an error; otherwise, will return degenerate simulations when it could not find a solution
#'@inheritParams create.intervention
#'
#'@export
create.monotonic.criteria.based.intervention <- function(base.intervention,
                                                         completion.criteria,
                                                         max.iterations,
                                                         n.iterations.after.satisfying.criteria = 5,
                                                         max.iterations.first.sim = max.iterations,
                                                         n.iterations.after.satisfying.criteria.first.sim = ceiling(max.iterations.first.sim/2),
                                                         max.failure.rate = 0,
                                                         discount.prior.n = 0.5,
                                                         code=NULL, 
                                                         name=NULL)
{
    MONOTONIC.CRITERIA.BASED.INTERVENTION$new(base.intervention = base.intervention,
                                              completion.criteria = completion.criteria,
                                              max.iterations = max.iterations,
                                              n.iterations.after.satisfying.criteria = n.iterations.after.satisfying.criteria,
                                              max.iterations.first.sim = max.iterations.first.sim,
                                              n.iterations.after.satisfying.criteria.first.sim = n.iterations.after.satisfying.criteria.first.sim,
                                              max.failure.rate = max.failure.rate,
                                              discount.prior.n = discount.prior.n,
                                              code = code,
                                              name = name)
}

##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##

MONOTONIC.CRITERIA.BASED.INTERVENTION = R6::R6Class(
    'monotonic.criteria.based.intervention',
    inherit = JHEEM.INTERVENTION,
    
    public = list(
        
        initialize = function(base.intervention,
                              completion.criteria,
                              max.iterations,
                              n.iterations.after.satisfying.criteria,
                              max.iterations.first.sim = max.iterations,
                              n.iterations.after.satisfying.criteria.first.sim,
                              max.failure.rate,
                              discount.prior.n,
                              code, 
                              name)
        {
            #@Andrew
            # browser()
            super$initialize(code = code,
                             name = name,
                             parameter.distribution = base.intervention$parameter.distribution)
            
            error.prefix = "Cannot create monotonic, criteria-based intervention: "
            
            # Base Intervention
            if (!is(base.intervention, 'jheem.standard.intervention'))
                stop(paste0(error.prefix, "'base.intervention' must be an object of class jheem.standard.intervention - as created by create.intervention() or join.interventions()"))
            
            # Completion Criteria
            if (is(completion.criteria, "monotonic.outcome.intervention.criterion"))
                completion.criteria = list(completion.criteria)
            
            if (!is.list(completion.criteria))
                stop(paste0(error.prefix, "'completion.critera' must be either a 'monotonic.outcome.intervention.criterion' object or a list of 'monotonic.outcome.intervention.criterion' objects"))
            
            if (length(completion.criteria)==0)
                stop(paste0(error.prefix, "'completion.criteria' cannot be an empty list"))
            
            if (any(!sapply(completion.criteria, is, "monotonic.outcome.intervention.criterion")))
                stop(paste0(error.prefix, "'completion.critera' must be either a 'monotonic.outcome.intervention.criterion' object or a list of 'monotonic.outcome.intervention.criterion' objects"))
            
            # The parameters of the completion criteria must not be shared by each other or the base intervention.
            if (any(duplicated(c(sapply(completion.criteria, function(criterion) {criterion$parameter.name}),
                                 if (!is.null(base.intervention$parameter.distribution)) base.intervention$parameter.distribution@var.names else NULL))))
                stop(paste0(error.prefix, "completion criteria may not share parameters with each other or with any parameter distributions in the base intervention"))
           
            # n.iterations.after.satisfying.criteria
            if (!is.numeric(n.iterations.after.satisfying.criteria) || length(n.iterations.after.satisfying.criteria)!=1 || is.na(n.iterations.after.satisfying.criteria))
                stop(paste0(error.prefix, "'n.iterations.after.satisfying.criteria' must be a single, non-NA numeric value"))
            if (n.iterations.after.satisfying.criteria<0)
                stop(paste0(error.prefix, "'n.iterations.after.satisfying.criteria' must be >= 0"))
            
            if (!is.numeric(n.iterations.after.satisfying.criteria.first.sim) || length(n.iterations.after.satisfying.criteria.first.sim)!=1 || is.na(n.iterations.after.satisfying.criteria.first.sim))
                stop(paste0(error.prefix, "'n.iterations.after.satisfying.criteria.first.sim' must be a single, non-NA numeric value"))
            if (n.iterations.after.satisfying.criteria.first.sim<0)
                stop(paste0(error.prefix, "'n.iterations.after.satisfying.criteria.first.sim' must be >= 0"))
            
            # max.iterations
            if (!is.numeric(max.iterations) || length(max.iterations)!=1 || is.na(max.iterations))
                stop(paste0(error.prefix, "'max.iterations' must be a single, non-NA numeric value"))
            if (max.iterations < n.iterations.after.satisfying.criteria)
                stop(paste0(error.prefix, "'max.iterations' (", max.iterations, ") cannot be less than 'n.iterations.after.satisfying.criteria' (", n.iterations.after.satisfying.criteria, ")"))
            
            if (!is.numeric(max.iterations.first.sim) || length(max.iterations.first.sim)!=1 || is.na(max.iterations.first.sim))
                stop(paste0(error.prefix, "'max.iterations.first.sim' must be a single, non-NA numeric value"))
            if (max.iterations.first.sim < n.iterations.after.satisfying.criteria.first.sim)
                stop(paste0(error.prefix, "'max.iterations.first.sim' (", max.iterations.first.sim, ") cannot be less than 'n.iterations.after.satisfying.criteria.first.sim' (", n.iterations.after.satisfying.criteria.first.sim, ")"))
            
            # max.failure.rate
            if (!is.numeric(max.failure.rate) || length(max.failure.rate)!=1 || is.na(max.failure.rate))
                stop(paste0(error.prefix, "'max.failure.rate' must be a single, non-NA numeric value"))
            
            if (max.failure.rate < 0 || max.failure.rate > 1)
                stop(paste0(error.prefix, "'max.failure.rate' (", max.failure.rate, ") must be between 0 and 1"))
            
            
            # Store the variables
            private$i.base.intervention = base.intervention
            private$i.completion.criteria = completion.criteria
            names(private$i.completion.criteria) = sapply(completion.criteria, function(criterion) {criterion$parameter.name})
            
            private$i.n.iterations.after.satisfying.criteria = n.iterations.after.satisfying.criteria
            private$i.max.iterations = max.iterations
            private$i.n.iterations.after.satisfying.criteria.first.sim = n.iterations.after.satisfying.criteria.first.sim
            private$i.max.iterations.first.sim = max.iterations.first.sim
            private$i.max.failure.rate = max.failure.rate
            private$i.dx.discount.prior.n = discount.prior.n
            
        },
        
        get.intervention.foregrounds = function()
        {
            private$i.base.intervention$get.intervention.foregrounds()
        },
        
        do.check.can.apply = function(version, location, sub.version, error.prefix)
        {
            # The default does nothing additional
        },
        
        check = function() {browser()}
    ),
    
    active = list(
        
        depends.on = function(value)
        {
            if (missing(value))
                private$i.base.intervention$depends.on
            else
                stop("Cannot modify 'depends.on' for a jheem.intervention - it is read-only")
        },
        
        base.intervention = function(value)
        {
            if (missing(value))
                private$i.base.intervention
            else
                stop("Cannot modify 'base.intervention' for a jheem.intervention - it is read-only")
        }
    ),
    
    private = list(
        
        i.base.intervention = NULL,
        i.completion.criteria = NULL,
        
        i.n.iterations.after.satisfying.criteria = NULL,
        i.max.iterations = NULL,
        i.n.iterations.after.satisfying.criteria.first.sim = NULL,
        i.max.iterations.first.sim = NULL,
        i.max.failure.rate = NULL,
        i.method = NULL,
        
        #@Andrew - I think you'll need these, but feel free to remove
        # run-time parameters - updated for each run
        i.previous.parameter.means = NULL, # a vector that gets updated
        i.parameters.to.optimize.names = NULL,
        i.previous.score = NULL,
        i.n.failures = NULL,
        i.n.sim = NULL,
        i.dx.components = NULL,
        i.dx.discount.prior.n = NULL,
        i.param.has.pos.direction = NULL,
        
        i.total.iterations = NULL,
        
        # is.equal.to = function(other)
        # {
        #     stop("need to implement is.equal.to for criteria-based-intervention")
        # },
        
        prepare.to.run = function(engine, sim, keep.from.year, keep.to.year, verbose)
        {
           #@Andrew fill in
            
            # check whether the parameters to track are actually in the simulation (or base intervention? or intervention's "parameter distribution"?) - Ask Todd
            private$i.parameters.to.optimize.names = sapply(private$i.completion.criteria, function(criterion) {criterion$parameter.name})
            private$i.previous.parameter.means = sapply(private$i.parameters.to.optimize.names, function(x) {0})
            names(private$i.previous.parameter.means) = private$i.parameters.to.optimize.names
            private$i.n.failures = 0
            private$i.n.sim = sim$n.sim
            private$i.dx.components = lapply(private$i.parameters.to.optimize.names, function(x) {
                list(dx=NA, dx.n=1, dx.mean=NA, dx.sd=NA)
            })
            
            private$i.param.has.pos.direction = sapply(private$i.parameters.to.optimize.names, function(x) {NA})
            
            # set up the optimized sim get
            sim.metadata = get.simulation.metadata(version=sim$version, location=sim$location, from.year=keep.from.year, to.year=keep.to.year)
            for (criterion in private$i.completion.criteria) {criterion$prepare.optimized.sim.get.instructions(sim.metadata)}
            
            private$i.total.iterations = 0
        },
        
        do.run = function(engine, sim.index, parameters, verbose)
        {
            #@Andrew fill in
            # ptm = Sys.time()
            # if (sim.index==3) browser()
            #-- Step 1: Run with either parameters set to 1 (for a multiplier) or using previous sim parameters --#
            # browser()

            # keep separate the parameters passed into this function (which come from a distribution and are NOT optimized) and the ones I will optimize and make sure the engine$run gets them all
            
            tsfx.parameters.to.optimize = sapply(private$i.parameters.to.optimize.names, function(parameter.name) {
                criterion.this.parameter = private$i.completion.criteria[[parameter.name]]
                if (criterion.this.parameter$draw.parameter.from.previous.sims && sim.index > 1)
                    value = private$i.previous.parameter.means[[parameter.name]]
                else
                    transform.to.unbounded.scale(criterion.this.parameter$parameter.initial.value, criterion.this.parameter$parameter.scale)
            })
            
            # Take starting point (which has initial value given or inherited from previous sim) and take a step assuming a dx of 1, then save dx and use for future
            untsfx.parameters.to.optimize = sapply(private$i.parameters.to.optimize.names, function(parameter.name) {
                criterion.this.parameter = private$i.completion.criteria[[parameter.name]]
                transform.from.unbounded.scale(tsfx.parameters.to.optimize[[parameter.name]], criterion.this.parameter$parameter.scale)
            })
            prev.sim = engine$run(parameters=c(untsfx.parameters.to.optimize, parameters), prior.sim.index=sim.index)
            iteration = 1 # Iteration refers to how many times engine$run is called
            
            # Check if these parameters are good enough on their own! We can skip to fine tuning as long as we have a derivative to use.
            if (sim.index != 1) {
                criteria.satisfied = all(sapply(private$i.completion.criteria, function(criterion){
                    criterion$is.satisfied(prev.sim, iteration = iteration)
                }))
            }
            if ((sim.index != 1 && !criteria.satisfied) || sim.index == 1) {
                private$i.dx.components = lapply(private$i.parameters.to.optimize.names, function(parameter.name) {
                    if (!is.na(private$i.dx.components[[parameter.name]]$dx))
                        dx = private$i.dx.components[[parameter.name]]$dx
                    else if (!is.na(private$i.param.has.pos.direction[[parameter.name]])) {
                        if (private$i.param.has.pos.direction[[parameter.name]])
                            dx = 1
                        else
                            dx = -1
                    }
                    else dx = 1
                    dx.n = 1
                    dx.mean = dx
                    dx.sd = 10 ## the default starting value
                    list(dx=dx, dx.n=dx.n, dx.mean=dx.mean, dx.sd=dx.sd)
                })
                # browser()
                tsfx.new.parameters.to.optimize = sapply(private$i.parameters.to.optimize.names, function(parameter.name) {
                    criterion.this.parameter = private$i.completion.criteria[[parameter.name]]
                    criterion.this.parameter$suggest.new.parameter(prev.sim, private$i.dx.components[[parameter.name]]$dx, is.fine.tuning = F)
                })
                names(tsfx.new.parameters.to.optimize) = private$i.parameters.to.optimize.names # comes back weird from the above sapply because the inner function returns a named vector
                untsfx.new.parameters.to.optimize = sapply(private$i.parameters.to.optimize.names, function(parameter.name) {
                    criterion.this.parameter = private$i.completion.criteria[[parameter.name]]
                    transform.from.unbounded.scale(tsfx.new.parameters.to.optimize[[parameter.name]], criterion.this.parameter$parameter.scale)
                })
                next.sim = engine$run(parameters=c(untsfx.new.parameters.to.optimize, parameters), prior.sim.index=sim.index)
                iteration = 2
                
                # update dx
                private$i.dx.components = lapply(private$i.parameters.to.optimize.names, function(parameter.name) {
                    criterion.this.parameter = private$i.completion.criteria[[parameter.name]]
                    components = criterion.this.parameter$get.derivative.components(next.sim,
                                                                                    prev.sim,
                                                                                    tsfx.new.parameters.to.optimize[[parameter.name]],
                                                                                    tsfx.parameters.to.optimize[[parameter.name]],
                                                                                    private$i.dx.components[[parameter.name]],
                                                                                    private$i.dx.discount.prior.n,
                                                                                    verbose=verbose)
                })
                
                # update directionality if previously unknown
                for (parameter.name in private$i.parameters.to.optimize.names) {
                    if (is.na(private$i.param.has.pos.direction[[parameter.name]]))
                        private$i.param.has.pos.direction[[parameter.name]] = private$i.dx.components[[parameter.name]]$dx > 0
                }
                
                prev.sim = next.sim
                
                criteria.satisfied = all(sapply(private$i.completion.criteria, function(criterion){
                    criterion$is.satisfied(prev.sim, iteration = iteration)
                }))
            }
            
            #-- Step 2: If not yet in range, loop with new parameters until in range or fail --#
            
            if (!criteria.satisfied) {
                
                if (sim.index == 1) max.iterations = private$i.max.iterations.first.sim
                else
                    max.iterations = private$i.max.iterations
                # browser()
                # Rotate through criteria one by one
                unsatisfied.criteria = seq_along(private$i.completion.criteria) # Note: it's possible that one of ours IS already satisfied...
                criterion.index = 1
                iteration = iteration + 1
                while (iteration < max.iterations && length(unsatisfied.criteria)>0) {
                    # if (iteration ==10) browser()
                    # things that were satisfied can become unsatisfied again
                    # rotate through the criteria that are not yet satisfied
                    criterion.index = (criterion.index %% length(private$i.completion.criteria)) + 1
                    while (!(criterion.index %in% unsatisfied.criteria))
                        criterion.index = (criterion.index %% length(private$i.completion.criteria)) + 1
                    criterion.this.iteration = private$i.completion.criteria[[criterion.index]]
                    parameter.this.criterion = criterion.this.iteration$parameter.name
                    # decide on new parameters
                    tsfx.new.parameters.to.optimize.value = criterion.this.iteration$suggest.new.parameter(prev.sim, private$i.dx.components[[parameter.this.criterion]]$dx, is.fine.tuning=F)
                    tsfx.new.parameters.to.optimize = tsfx.parameters.to.optimize
                    
                    # if it somehow suggested the exact same parameter, move on. Count it as an iteration to avoid getting trapped forever
                    if (is.na(tsfx.new.parameters.to.optimize.value)||is.na(tsfx.parameters.to.optimize[[parameter.this.criterion]])) browser()
                    if (tsfx.new.parameters.to.optimize.value == tsfx.parameters.to.optimize[[parameter.this.criterion]]) {
                        iteration = iteration + 1
                        next
                    }
                    
                    tsfx.new.parameters.to.optimize[[parameter.this.criterion]] = tsfx.new.parameters.to.optimize.value
                    untsfx.new.parameters.to.optimize = sapply(private$i.parameters.to.optimize.names, function(parameter.name) {
                        criterion.this.parameter = private$i.completion.criteria[[parameter.name]]
                        transform.from.unbounded.scale(tsfx.new.parameters.to.optimize[[parameter.name]], criterion.this.parameter$parameter.scale)
                    })
                    
                    # run a new sim
                    tryCatch({next.sim = engine$run(parameters=c(untsfx.new.parameters.to.optimize, parameters),
                                                    prior.sim.index = sim.index)}, error=function(e){browser()})
                    
                    iteration = iteration + 1
                    
                    # Use this information to save a new derivative
                    # But we do not want a derivative of 0, so if the outcome hasn't changed, we should skip it
                    if (criterion.this.iteration$check.has.changed(prev.sim, next.sim)) {
                        components = criterion.this.iteration$get.derivative.components(next.sim,
                                                                                        prev.sim,
                                                                                        tsfx.new.parameters.to.optimize[[parameter.this.criterion]],
                                                                                        tsfx.parameters.to.optimize[[parameter.this.criterion]],
                                                                                        private$i.dx.components[[parameter.this.criterion]],
                                                                                        private$i.dx.discount.prior.n,
                                                                                        verbose=verbose)
                        private$i.dx.components[[parameter.this.criterion]] = components
                    }
                    # Compare score to previous sim's score
                    criterion.satisfied = sapply(private$i.completion.criteria, function(criterion){
                        criterion$is.satisfied(next.sim, iteration = iteration)
                    })
                    next.score = criterion.this.iteration$score.sim(next.sim, iteration=iteration, is.fine.tuning=F, verbose=F)
                    prev.score = criterion.this.iteration$score.sim(prev.sim, iteration=iteration-1, is.fine.tuning=F, verbose=F)
                    if (next.score > prev.score || all(criterion.satisfied)) {
                        prev.sim = next.sim
                        tsfx.parameters.to.optimize = tsfx.new.parameters.to.optimize
                        # save prev score? for now will just calculate it twice (once as prev.score, once later as current.score)
                        
                        # update satisfied criteria for cycling through
                        unsatisfied.criteria = which(!criterion.satisfied)
                    }
                    
                }
                # print(paste0("Sim ", sim.index, " took ", Sys.time()-ptm, " for ", iteration, " iterations"))
                
                if (length(unsatisfied.criteria) > 0) {
                    private$i.n.failures = private$i.n.failures + 1
                    print("Failure")
                    return(derive.degenerate.simulation(sim)) # check with Todd
                }
            }
            #-- Step 3: Try to get closer to the target --#
            
            if (sim.index == 1) n.iterations.after.satisfying.criteria = private$i.n.iterations.after.satisfying.criteria.first.sim
            else n.iterations.after.satisfying.criteria = private$i.n.iterations.after.satisfying.criteria
            criterion.index = 0
            for (i in 1:n.iterations.after.satisfying.criteria) {
                # print(iteration)
                # if (iteration==9) browser()
                # rotate criterion
                criterion.index = (criterion.index %% length(private$i.completion.criteria)) + 1
                criterion.this.iteration = private$i.completion.criteria[[criterion.index]]
                parameter.this.criterion = criterion.this.iteration$parameter.name
                
                # decide on new parameters
                tsfx.new.parameters.to.optimize.value = criterion.this.iteration$suggest.new.parameter(prev.sim, private$i.dx.components[[parameter.this.criterion]]$dx, is.fine.tuning = T)
                tsfx.new.parameters.to.optimize = tsfx.parameters.to.optimize
                tsfx.new.parameters.to.optimize[[parameter.this.criterion]] = tsfx.new.parameters.to.optimize.value
                
                if (is.na(tsfx.new.parameters.to.optimize.value)||is.na(tsfx.parameters.to.optimize[[parameter.this.criterion]])) browser()
                if (tsfx.new.parameters.to.optimize.value == tsfx.parameters.to.optimize[[parameter.this.criterion]]) {
                    iteration = iteration + 1
                    next
                }
                untsfx.new.parameters.to.optimize = sapply(private$i.parameters.to.optimize.names, function(parameter.name) {
                    criterion.this.parameter = private$i.completion.criteria[[parameter.name]]
                    transform.from.unbounded.scale(tsfx.new.parameters.to.optimize[[parameter.name]], criterion.this.parameter$parameter.scale)
                })
                
                next.sim = engine$run(parameters=c(untsfx.new.parameters.to.optimize, parameters),
                                      prior.sim.index = sim.index)
                iteration = iteration + 1
                
                # Use this information to save a new derivative
                # But we do not want a derivative of 0, so if the outcome hasn't changed, we should skip it
                if (criterion.this.iteration$check.has.changed(prev.sim, next.sim)) {
                    # if (sim.index==2 && private$i.dx.components[[parameter.this.criterion]]$dx < -1200) browser()
                    components = criterion.this.iteration$get.derivative.components(next.sim,
                                                                                    prev.sim,
                                                                                    tsfx.new.parameters.to.optimize[[parameter.this.criterion]],
                                                                                    tsfx.parameters.to.optimize[[parameter.this.criterion]],
                                                                                    private$i.dx.components[[parameter.this.criterion]],
                                                                                    private$i.dx.discount.prior.n,
                                                                                    verbose=verbose)
                    private$i.dx.components[[parameter.this.criterion]] = components
                }
                
                # compare to previous sim -- any out of range is a deal breaker.
                criteria.satisfied = all(sapply(private$i.completion.criteria, function(criterion){
                    criterion$is.satisfied(next.sim, iteration = iteration)
                }))
                
               # use sum score instead
                next.score = sum(sapply(private$i.completion.criteria, function(criterion) {
                    criterion$score.sim(next.sim, iteration=iteration, is.fine.tuning=T, verbose=F)}))
                prev.score = sum(sapply(private$i.completion.criteria, function(criterion) {
                    criterion$score.sim(prev.sim, iteration=iteration-1, is.fine.tuning=T, verbose=F)}))
                if (next.score > prev.score && all(criteria.satisfied)) {
                    prev.sim = next.sim
                    tsfx.parameters.to.optimize = tsfx.new.parameters.to.optimize
                }
            }
            final.sim = prev.sim # which was either set to next.sim at the end of the last iteration if accepted, or kept the same if not
            final.score = sum(sapply(private$i.completion.criteria, function(criterion) {
                criterion$score.sim(final.sim, iteration=iteration, is.fine.tuning=T, verbose=F)}))
            
            #-- Step 4: Save parameters to use for next sim --#
            
            # use parameter values to update mean for all sims
            private$i.previous.parameter.means = (private$i.previous.parameter.means * (sim.index - private$i.n.failures - 1) + tsfx.parameters.to.optimize) / (sim.index - private$i.n.failures)
            
            # print(paste0("sim ", sim.index, " obtained a score of ", final.score, " with ", iteration, " iterations"))
            
            private$i.total.iterations = private$i.total.iterations + iteration
            if (verbose && sim.index==private$i.n.sim) print(paste0("iterations after sim ", sim.index, ": ", private$i.total.iterations))
            
            #-- Step 5: Return sim --#
            return(final.sim)
           
        },
        
        do.validate = function(jheem.kernel,
                               sub.version = NULL,
                               simulation.metadata = NULL,
                               error.prefix = '')
        {
            for (criterion in private$i.completion.criteria)
                criterion$validate(jheem.kernel = jheem.kernel,
                                   sub.version = sub.version,
                                   simulation.metadata = simulation.metadata,
                                   error.prefix = error.prefix)
        }
    )
)

MONOTONIC.OUTCOME.INTERVENTION.CRITERION = R6::R6Class(
    'monotonic.outcome.intervention.criterion',
    
    public = list(
        
        initialize = function(parameter.name,
                              outcome,
                              
                              parameter.scale,
                              parameter.initial.value,
                              
                              target.value,
                              min.acceptable.value,
                              max.acceptable.value,
                              subsequent.thresholds.apply.after.iteration=numeric(),
                              dimension.values = list(), 
                              
                              draw.parameter.from.previous.sims = !is.function(parameter.initial.value),
                              score.metric = "normal.with.coefficient.of.variance", #@Todd
                              score.coefficient.of.variance = 0.5, #@Todd
                              ...)
        {
            # browser()
            #@Andrew - validate parameter.name, scale, initial.value, draw from previous sims
            error.prefix = "Error: unable to create monotonic outcome intervention criterion: "
            # parameter.name
            
            # parameter.initial.value
            if (is.numeric(parameter.initial.value))
            {
                if (!is.numeric(parameter.initial.value) || length(parameter.initial.value)!=1 || is.na(parameter.initial.value))
                    stop(paste0(error.prefix, "'parameter.initial.value' must be either a single numeric value, or a function that takes a sim and returns a named numeric vector"))
            }
            else if (is.function(parameter.initial.value))
            {
                arg.names = get.function.argument.names(fn = parameter.initial.value,
                                                        exclude.arguments.with.default.values = T)
                
                if (length(arg.names)!=1 || arg.names!='sim')
                    stop(paste0(error.prefix, "If 'parameter.initial.value' is a function, it must take one and only one argument without a default value: 'sim'"))
            }
            else
                stop(paste0(error.prefix,
                            "'parameter.initial.value' must be either a single numeric value, or a function that takes a sim and returns a named numeric vector"))
            
            # parameter scale
            check.model.scale(scale = parameter.scale,
                              varname.for.error = "'parameter.scale'",
                              require.scalar = T,
                              error.prefix = error.prefix)

            
            # outcome, target
            if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome))
                stop(paste0(error.prefix, "'outcome' must be a single, non-NA character value"))
            
            if (!is.numeric(target.value) || length(target.value)!=1 || any(is.na(target.value)))
                stop(paste0(error.prefix, "'target.value' must be a non-empty, numeric vector with no NA values"))
            
            # min / max
            if (!is.numeric(min.acceptable.value) || length(min.acceptable.value)==0 || any(is.na(min.acceptable.value)))
                stop(paste0(error.prefix, "'min.acceptable.value' must be a non-empty, numeric vector with no NA values"))
            
            if (length(min.acceptable.value)>1 && any(min.acceptable.value[-1] > min.acceptable.value[-length(min.acceptable.value)]))
                stop(paste0(error.prefix, "It does not make sense for min.acceptable.values to INCREASE (ie become more restrictive) at later times"))
            
            if (!is.numeric(max.acceptable.value) || length(max.acceptable.value)==0 || any(is.na(max.acceptable.value)))
                stop(paste0(error.prefix, "'max.acceptable.value' must be a non-empty, numeric vector with no NA values"))
            
            if (length(max.acceptable.value)>1 && any(max.acceptable.value[-1] < max.acceptable.value[-length(max.acceptable.value)]))
                stop(paste0(error.prefix, "It does not make sense for max.acceptable.values to DECREASE (ie become more restrictive) at later times"))
            
            min.acceptable.value = pad.with.last.value(min.acceptable.value, length(max.acceptable.value))
            max.acceptable.value = pad.with.last.value(max.acceptable.value, length(min.acceptable.value))
            
            # subsequent.thresholds.apply.after.iteration
            if (!is.numeric(subsequent.thresholds.apply.after.iteration) || any(is.na(subsequent.thresholds.apply.after.iteration)))
                stop(paste0(error.prefix, "'subsequent.thresholds.apply.after.iteration' must be a numeric vector with no NA values"))
            
            if (length(subsequent.thresholds.apply.after.iteration) != (length(min.acceptable.value)-1))
            {
                if (length(min.acceptable.value)==1)
                    stop(paste0(error.prefix, "If there is only one value each for 'min.acceptable.value' and 'max.acceptable.value', 'subsequent.thresholds.apply.after.iteration' must be an empty vector (ie, length 0)"))
                else
                    stop(paste0(error.prefix, "'subsequent.thresholds.apply.after.iteration' (length ", length(subsequent.thresholds.apply.after.iteration),
                                ") must have length one less than the greater of 'min.acceptable.value' and 'max.acceptable.value' (", length(max.acceptable.value), ")"))
            }
            
            if (length(subsequent.thresholds.apply.after.iteration)>1 && 
                any(subsequent.thresholds.apply.after.iteration[-1] <= subsequent.thresholds.apply.after.iteration[-length(subsequent.thresholds.apply.after.iteration)]))
                stop(paste0(error.prefix, "'subsequent.thresholds.apply.after.iteration' must be in strictly ascending order"))
            
            if (any(subsequent.thresholds.apply.after.iteration<1))
                stop(paste0(error.prefix, "'subsequent.thresholds.apply.after.iteration' must all be >0"))
            
            
            if (any(min.acceptable.value >= max.acceptable.value))
                stop(paste0(error.prefix, "The elements of min.acceptable.value must always be strictly LESS than the corresponding elements in max.acceptable.value"))
            
            # Dimension stuff
            dot.dot.dot = list(...)
            if (length(dot.dot.dot)>0 && length(dimension.values)>0)
                stop(paste0(error.prefix, "Cannot specify BOTH dimension.values and elements in ... - you must use one or the other"))
            
            if (length(dimension.values)==0)
            {
                dimension.values = dot.dot.dot
                dv.name = "the elements of ..."
            }
            else
                dv.name = 'dimension.values'
            
            check.dimension.values.valid(dimension.values = dimension.values,
                                         variable.name.for.error = dv.name,
                                         refer.to.dimensions.as = 'dimension',
                                         allow.empty = T,
                                         allow.duplicate.values.within.dimensions = F,
                                         error.prefix=error.prefix)
            
            #@Todd didn't make these
            if (!is.character(score.metric) || length(score.metric)!=1 || is.na(score.metric))
                stop(paste0(error.prefix, "'score.metric' must be a single character value"))
            if (!is.numeric(score.coefficient.of.variance) || length(score.coefficient.of.variance)!=1 || is.na(score.coefficient.of.variance) || score.coefficient.of.variance<=0)
                stop(paste0(error.prefix, "'score.coefficient.of.variance' must be a single, positive numeric value")) # right?
            
            # Store variables
            #@Andrew - store parameter stuff
            
            private$i.parameter.name = parameter.name
            private$i.parameter.scale = parameter.scale
            private$i.parameter.initial.value = parameter.initial.value
            private$i.draw.parameter.from.previous.sims = draw.parameter.from.previous.sims
            
            private$i.outcome = outcome
            private$i.target.value = target.value
            
            private$i.min.acceptable.value = min.acceptable.value
            private$i.max.acceptable.value = max.acceptable.value
            private$i.subsequent.thresholds.apply.after.iteration = subsequent.thresholds.apply.after.iteration
            
            # private$i.stratify.outcome.by.dimensions = stratify.outcome.by.dimensions
            private$i.dimension.values = dimension.values
            
            # Todd didn't make this:
            private$i.score.metric = score.metric
            private$i.coefficient.of.variance = score.coefficient.of.variance
        },
        
        is.satisfied = function(sim, iteration)
        {
            # sim.value = private$optimized.get.sim.value(sim)
            sim.value = private$get.sim.value(sim)
            
            threshold.mask = c(iteration < private$i.subsequent.thresholds.apply.after.iteration, T)
            min.acceptable = private$i.min.acceptable.value[threshold.mask][1]
            max.acceptable = private$i.max.acceptable.value[threshold.mask][1]
            sim.value >= min.acceptable & sim.value <= max.acceptable
        },
        
        check.has.changed = function(next.sim, prev.sim) {
            private$optimized.get.sim.value(next.sim) != private$optimized.get.sim.value(prev.sim)
        },
        
        get.derivative.components = function(current.sim, prev.sim, tsfx.current.param, tsfx.prev.param, derivative.components, dx.discount.prior.n, verbose=F, debug=F) {
            if (debug) browser()
            # Now takes in a list and returns a list
            # Note that parameters will now be input transformed
            
            prev.dx = derivative.components$dx
            dx.n = derivative.components$dx.n
            dx.mean = derivative.components$dx.mean
            dx.sd = derivative.components$dx.sd
            
            current.value = as.vector(private$optimized.get.sim.value(current.sim))
            prev.value = as.vector(private$optimized.get.sim.value(prev.sim))
            # current.value = private$get.sim.value(current.sim)
            # prev.value = private$get.sim.value(prev.sim)
            
            outcome.scale = current.sim$outcome.metadata[[private$i.outcome]]$scale
            
            tryCatch({tsfx.current.outcome = transform.to.unbounded.scale(current.value, outcome.scale)}, error=function(e) {browser()})
            
            tsfx.prev.outcome = transform.to.unbounded.scale(prev.value, outcome.scale)
            # tsfx.current.param = transform.to.unbounded.scale(current.param, private$i.parameter.scale)
            # tsfx.prev.param = transform.to.unbounded.scale(prev.param, private$i.parameter.scale)
            
            one.dx = (tsfx.current.outcome-tsfx.prev.outcome) / (tsfx.current.param - tsfx.prev.param)
            
            naive.dx = (prev.dx * (dx.n ^ dx.discount.prior.n - 1) + one.dx) / (dx.n ^ dx.discount.prior.n)
            new.dx.n = dx.n + 1
            
            # Check if naive.dx is within 5 sd's of the mean. If not, reject it and take the 1st sd in that direction.
            sd.off = (naive.dx - dx.mean)/dx.sd
            if (sd.off > 5)
                naive.dx = dx.mean + dx.sd
            else if (sd.off < -5)
                naive.dx = dx.mean - dx.sd
            
            # Update dx.mean
            new.dx.mean = (dx.mean * dx.n + naive.dx) / (new.dx.n)
            # Update dx.sd
            new.dx.sd = sqrt(((dx.n-1)/(dx.n))*(dx.sd^2) + (1/new.dx.n)*(naive.dx - new.dx.mean)^2)
            
            if (is.infinite(naive.dx)) browser()
            
            if (is.na(naive.dx)) browser()
            if (naive.dx==0) browser()
            # if (verbose && self$parameter.name == 'testing.multiplier')
            #     print(paste0('testing.multiplier naive.dx: ', naive.dx))
            return (list(dx=naive.dx, dx.n=new.dx.n, dx.mean=new.dx.mean, dx.sd=new.dx.sd))
        },
        
        # added until we do something taking multiple criteria into account simultaneously
        suggest.new.parameter = function(sim, transformed.naive.dx, is.fine.tuning=F, debug=F) {
            if (debug) browser()
            
            low = private$i.min.acceptable.value[1]
            high = private$i.max.acceptable.value[1]
            mid = (high + low) / 2
            
            # If fine tuning, use target. Otherwise, use mean of mid and target.
            if (is.fine.tuning)
                tsfx.target.outcome = transform.to.unbounded.scale(private$i.target.value, sim$outcome.metadata[[private$i.outcome]]$scale)
            else
                tsfx.target.outcome = transform.to.unbounded.scale(mean(mid, private$i.target.value), sim$outcome.metadata[[private$i.outcome]]$scale)
            
            tsfx.current.outcome = transform.to.unbounded.scale(as.vector(private$optimized.get.sim.value(sim)), sim$outcome.metadata[[private$i.outcome]]$scale)
            # tsfx.current.outcome = transform.to.unbounded.scale(private$get.sim.value(sim), sim$outcome.metadata[[private$i.outcome]]$scale)
            
            tsfx.param = transform.to.unbounded.scale(sim$params[private$i.parameter.name], private$i.parameter.scale)
            
            tsfx.step = (tsfx.target.outcome- tsfx.current.outcome) / transformed.naive.dx
            
            next.tsfx.param = tsfx.param + tsfx.step
        },
        
        score.sim = function(sim, iteration, is.fine.tuning=F, verbose, as.log.likelihood=T, debug=F)
        {
            #@Andrew - may need to modify
            if (debug) browser()
            P.IN.RANGE = 0.9999
            
            #-- Prep the bounds --#
            pre.transformed.sim.value = sim.value = private$optimized.get.sim.value(sim)
            # pre.transformed.sim.value = sim.value = private$get.sim.value(sim)
             ##
            low = private$i.min.acceptable.value[1]
            high = private$i.max.acceptable.value[1]
            mid = (high + low) / 2
            scale = sim$outcome.metadata[[private$i.outcome]]$scale
            #mid = pre.transformed.mid = (high + low) / 2
            
            # If fine tuning, use target. Otherwise, use mean of mid and target.
            if (is.fine.tuning)
                target = transform.to.unbounded.scale(private$i.target.value, scale)
            else
                target = transform.to.unbounded.scale(mean(mid, private$i.target.value), scale)
            target = transform.to.unbounded.scale(private$i.target.value, scale)
            above.range = sim.value > high
            below.range = sim.value < low
            is.in.range = !above.range & !below.range
            
            #-- Transform to unbounded scale --#        
            sim.value = transform.to.unbounded.scale(sim.value, scale)
            low = transform.to.unbounded.scale(low, scale)
            high = transform.to.unbounded.scale(high, scale)
            mid = (high + low) / 2
            
            
            
            # browser()
            #-- Figure out our SDs --#
            # base.of.sd = c(private$i.target.value, target, pre.transformed.mid, mid) #putting four things in here helps protect us against getting an sd of zero
            base.of.sd = abs(c(target, mid)) #putting two things in here helps protect us against getting an sd of zero
            base.of.sd = base.of.sd[base.of.sd != 0]
            if (length(base.of.sd)==0)
            {
                base.of.sd = abs(c(private$i.target.value, pre.transformed.mid))
                base.of.sd = base.of.sd[base.of.sd != 0]
            }
            
            if (length(base.of.sd)==0)
                sd.for.score = 0.1
            else
            {
                if (private$i.score.metric=='normal.with.coefficient.of.variance')
                    sd.for.score = mean(base.of.sd * private$i.coefficient.of.variance)
                else if (private$i.score.metric=='normal.with.sqrt.sd')
                    sd.for.score = mean(sqrt(base.of.sd))
                else
                    stop(paste0("we haven't implemented scoring for metric ", private$i.score.metric))
            }
            
            log.f.continuous = dnorm(sim.value, target, sd.for.score, log=T)
            log.f.in.or.out.of.range = log(P.IN.RANGE) * is.in.range + log(1-P.IN.RANGE) * !is.in.range
            
            log.f = log.f.continuous + log.f.in.or.out.of.range
            
            if (verbose)
                base::print(paste0("   ", private$i.outcome, " = ", round(pre.transformed.sim.value,3),
                                   " (p = ", round(exp(log.f), 3), ")"))
            # if (self$parameter.name == 'testing.multiplier')
                # print(paste0("iteration ", iteration, " has value ", pre.transformed.sim.value, " and score ", log.f))
            if (is.na(log.f))
                browser()
            
            if (as.log.likelihood)
                log.f
            else
                exp(-log.f)
        },
        
        prepare.optimized.sim.get.instructions = function(sim.metadata) {
            private$i.optimized.sim.get.instructions = sim.metadata$prepare.optimized.get.instructions(outcome = private$i.outcome,
                                                                                                       keep.dimensions = 'year', #by definition, only allowed to get a single scalar, but optimized get requires year
                                                                                                       dimension.values = private$i.dimension.values)
        },

        equals = function(other)
        {
            stop('need to implement equals for outcome-based criterion')
        },
        
        validate = function(jheem.kernel,
                            sub.version = NULL,
                            simulation.metadata = NULL,
                            error.prefix = '')
        {
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop(paste0("Error in jheem.outcome.intervention.criterion$validate(): 'error.prefix' must be a single, non-NA character value"))
            # browser()
            error.prefix = paste0(error.prefix, "Invalid criterion for '", private$i.outcome, "' - ")
            outcome = jheem.kernel$get.outcome.kernel(private$i.outcome)
            
            if (is.null(outcome))
                stop(paste0(error.prefix, "'", private$i.outcome, "' is not a registered outcome for the '", jheem.kernel$version, "' specification instance in this simulation"))
            
            if (!is.null(outcome$sub.versions) && !is.null(sub.version) && all(outcome$sub.versions!=sub.version))
                stop(paste0(error.prefix, "'", private$i.outcome, "' is not a registered outcome for sub-version '", sub.version, 
                            "' (it is only registered for ", collapse.with.and("'", outcome$sub.versions, "'"), ")"))
            
            invalid.stratify.dimensions = setdiff(private$i.stratify.outcome.by.dimensions, c(outcome$keep.dimensions,'year'))
            if (length(invalid.stratify.dimensions)>0)
                stop(paste0(error.prefix, collapse.with.and("'", invalid.stratify.dimensions, "'"),
                            ifelse(length(invalid.stratify.dimensions)==1, " is not a dimension of", " are not dimensions of"),
                            " the '", private$i.outcome, "' outcome in the '", jheem.kernel$version, "' specification instance in this simulation - and so cannot be used to stratify the outcome for the criterion"))
            
            invalid.dimension.value.dimensions = setdiff(names(private$i.dimension.values), c(outcome$keep.dimensions, 'year'))
            if (length(invalid.dimension.value.dimensions)>0)
                stop(paste0(error.prefix, collapse.with.and("'", invalid.dimension.value.dimensions, "'"),
                            ifelse(length(invalid.dimension.value.dimensions)==1, " is not a dimension of", " are not dimensions of"),
                            " the '", private$i.outcome, "' outcome in the '", jheem.kernel$version, "' specification instance in this simulation - and so cannot be used to specify dimension values of the criterion"))
            
            if (!is.null(simulation.metadata))
            {
                if (!is(simulation.metadata, 'simulation.metadata'))
                    stop(paste0(error.prefix, "'simulation.metadata' must be an object of class 'simulation.metadata', as returned by get.simulation.metadata()"))
                
                outcome.ontology = simulation.metadata$outcome.ontologies[[private$i.outcome]]
                
                for (d in names(private$i.dimension.values))
                {
                    if (!is.null(outcome.ontology[[d]]))
                    {
                        invalid.values = setdiff(private$i.dimension.values[[d]], outcome.ontology[[d]])
                        if (length(invalid.values)>0)
                            stop(paste0(error.prefix, collapse.with.and("'", invalid.values, "'"),
                                        ifelse(length(invalid.values)==1, " is not a valid value", " are valid values"),
                                        " in the '", d, "' dimension of outcome '", private$i.outcome, 
                                        "' in the '", jheem.kernel$version, "' specification instance in this simulation - and so cannot be used as dimension values in the criterion"))
                    }
                }
            }
        },
        check = function() {browser()}
    ),
    
    active = list(
        # Added this because somehow the intervention expects to know this but it was given only to criteria?
        draw.parameter.from.previous.sims = function(value)
        {
            if (missing(value))
                private$i.draw.parameter.from.previous.sims
            else
                stop("Cannot modify 'draw.parameter.from.previous.sims' for a jheem.intervention - it is read-only")
        },
        
        parameter.initial.value = function(value)
        {
            if (missing(value))
                private$i.parameter.initial.value
            else
                stop("Cannot modify 'parameter.initial.value' for a jheem.intervention - it is read-only")
        },
        
        parameter.name = function(value)
        {
            if (missing(value))
                private$i.parameter.name
            else
                stop("Cannot modify 'parameter.name' for a jheem.intervention - it is read-only")
        },
        
        parameter.scale = function(value)
        {
            if (missing(value))
                private$i.parameter.scale
            else
                stop("Cannot modify 'parameter.scale' for a jheem.intervention - it is read-only")
        }
    ),
    
    private = list(
        
        i.parameter.name = NULL,
        i.outcome = NULL,
        
        i.parameter.scale = NULL,
        i.parameter.initial.value = NULL,
        i.draw.parameter.from.previous.sims = NULL,
        
        i.target.value = NULL,
        i.min.acceptable.value = NULL,
        i.max.acceptable.value = NULL,
        i.subsequent.thresholds.apply.after.iteration = NULL,
        
        i.dimension.values = NULL,
        
        # Todd didn't put this
        i.score.metric = NULL,
        i.coefficient.of.variance = NULL,
        
        i.optimized.sim.get.instructions = NULL,
        
        get.sim.value = function(sim)
        {
            sim$get(outcomes = private$i.outcome,
                    dimension.values = private$i.dimension.values,
                    keep.dimensions = character()) #by definition, only allowed to get a single scalar
        },
        optimized.get.sim.value = function(sim) {
            as.vector(sim$optimized.get(private$i.optimized.sim.get.instructions))
        }
        
    )
)
