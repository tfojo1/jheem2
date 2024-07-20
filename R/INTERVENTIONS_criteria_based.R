
#'@name Create a criterion for monotonic parameter, criteria-based interventions
#'
#'@param parameter.name The name of the parameter to be modified to achieve the outcome
#'@param outcome A character vector indicating the name of the outcome to which the criterion applies
#'@param inversely.related If true, increasing the parameter decreases the value of the outcome. If false, increasing the parameter increases the outcome
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
                                       inversely.related,
                                       
                                       parameter.scale,
                                       parameter.initial.value,
                                       
                                       target.value,
                                       min.acceptable.value,
                                       max.acceptable.value,
                                       subsequent.thresholds.apply.after.iteration=numeric(),
                                       dimension.values = list(), 
                                       
                                       draw.parameters.from.previous.sims = !is.function(parameter.initial.value),
                                       ...)
{
    MONOTONIC.OUTCOME.INTERVENTION.CRITERION$new(parameter.name = parameter.name,
                                                 outcome = outcome,
                                                 inversely.related = inversely.related,
                                                 
                                                 parameter.scale = parameter.scale,
                                                 parameter.initial.value = parameter.initial.value,
                                                 
                                                 target.value = target.value,
                                                 min.acceptable.value = min.acceptable.value,
                                                 max.acceptable.value = max.acceptable.value,
                                                 subsequent.thresholds.apply.after.iteration = subsequent.thresholds.apply.after.iteration,
                                                 dimension.values = dimension.values,
                                                 
                                                 draw.parameters.from.previous.sims = !is.function(parameter.initial.value),
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
                              code, 
                              name)
        {
            #@Andrew
            
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
            
            private$i.n.iterations.after.satisfying.criteria = n.iterations.after.satisfying.criteria
            private$i.max.iterations = max.iterations
            private$i.n.iterations.after.satisfying.criteria.first.sim = n.iterations.after.satisfying.criteria.first.sim
            private$i.max.iterations.first.sim = max.iterations.first.sim
            private$i.max.failure.rate = max.failure.rate
            
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
        i.previous.parameter.values = NULL,
        i.previous.score = NULL,
        i.n.failures = NULL,
        i.n.sim = NULL,
        
        is.equal.to = function(other)
        {
            stop("need to implement is.equal.to for criteria-based-intervention")
        },
        
        prepare.to.run = function(engine, sim, verbose)
        {
           #@Andrew fill in
            
            # check whether the parameters to track are actually in the simulation (or base intervention? or intervention's "parameter distribution"?) - Ask Todd
            
            private$i.previous.parameter.values = NULL
            private$i.n.failures = 0
            private$i.n.sim = sim$n.sim
        },
        
        do.run = function(engine, sim.index, parameters, verbose)
        {
            #@Andrew fill in
            # ptm = Sys.time()
            
            cc=private$i.completion.criteria[[1]]
            iteration = 1
            acceptable.sim.found = F
            
            #-- Step 1: Run with either parameters set to 1 (for a multiplier) or using previous sim parameters --#
            
            if (sim.index > 1 && cc$draw.parameters.from.previous.sims) {
                parameters = private$i.previous.parameter.values
                previous.sim = engine$run(parameters=parameters, prior.sim.index = 1) #?
                previous.score = sum(sapply(private$i.completion.criteria, function(criterion) {criterion$score.sim(previous.sim, iteration=iteration, verbose=F)}))
                previous.parameters = parameters
            }
            else {
                previous.sim = engine$run(parameters=sapply(parameters, function(x) {1}), prior.sim.index = 1) #?
                previous.score = sum(sapply(private$i.completion.criteria, function(criterion) {criterion$score.sim(previous.sim, iteration=iteration, verbose=F)}))
                previous.parameters = parameters
            }
            
            #-- Step 2: If not yet in range, loop with new parameters until in range or fail --#
            
            # check if we are already in the range
            criteria.satisfied = all(sapply(private$i.completion.criteria, function(criterion){
                criterion$is.satisfied(previous.sim, iteration = iteration)
            }))
            
            if (!criteria.satisfied) {
                if (sim.index == 1) max.iterations = private$i.max.iterations.first.sim
                else
                    max.iterations = private$i.max.iterations
                while (iteration < max.iterations && !acceptable.sim.found) {
                    
                    iteration = iteration + 1
                    
                    # decide on new parameters?
                    # if overshot, try lower. If undershot, try higher.
                    if (iteration == 1 && !private$i.draw.parameters.from.previous.sims)
                        # for the first sim where we use the initial value
                        parameters[[1]] = cc$parameter.initial.value
                    else parameters = cc$suggest.new.parameter(previous.sim, iteration, debug=F)
                    
                    sim = engine$run(parameters=parameters,
                                     prior.sim.index = sim.index)
                    
                    # browser()
                    
                    # Evaluate output
                    criteria.satisfied = all(sapply(private$i.completion.criteria, function(criterion){
                        criterion$is.satisfied(sim, iteration = iteration)
                    }))
                    if (criteria.satisfied) {
                        acceptable.sim.found = T
                    } else {
                        # compare to previous sim
                        current.score = sum(sapply(private$i.completion.criteria, function(criterion) {criterion$score.sim(sim, iteration=iteration, verbose=F)}))
                        if (current.score > previous.score) {
                            previous.sim = sim
                            previous.score = current.score
                            previous.parameters = parameters
                        }
                    }
                    
                }
                # print(paste0("Sim ", sim.index, " took ", Sys.time()-ptm, " for ", iteration, " iterations"))
                
                if (!acceptable.sim.found) {
                    private$i.n.failures = private$i.n.failures + 1
                    print("Failure")
                    return(derive.degenerate.simulation(sim)) # check with Todd
                }
            }
            
            
            #-- Step 3: Try to get closer to the target --#
            
            if (sim.index == 1) n.iterations.after.satisfying.criteria = private$i.n.iterations.after.satisfying.criteria.first.sim
            else n.iterations.after.satisfying.criteria = private$i.n.iterations.after.satisfying.criteria
            
            for (i in seq_along(n.iterations.after.satisfying.criteria)) {
                
                parameters = cc$suggest.new.parameter(previous.sim, iteration + i)
                sim = engine$run(parameters=parameters,
                                 prior.sim.index = sim.index) # not the previous one, of course
                
                # compare to previous sim
                current.score = cc$score.sim(sim, iteration=iteration + i, verbose=F)
                if (current.score > previous.score) {
                    previous.sim = sim
                    previous.score = current.score
                    previous.parameters = parameters
                }
            }
            
            #-- Step 4: Save parameters to use for next sim --#
            
            if (is.null(private$i.previous.score))
                private$i.previous.parameter.values = parameters
            else if (current.score > private$i.previous.score)
                private$i.previous.parameter.values = parameters # also save best sim??
            print(paste0("sim ", sim.index, " obtained a score of ", current.score))
            
            #-- Step 5: Return sim --#
            return(sim)
           
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
                              inversely.related,
                              
                              parameter.scale,
                              parameter.initial.value,
                              
                              target.value,
                              min.acceptable.value,
                              max.acceptable.value,
                              subsequent.thresholds.apply.after.iteration=numeric(),
                              dimension.values = list(), 
                              
                              draw.parameters.from.previous.sims = !is.function(parameter.initial.value),
                              ...)
        {
            # browser()
            #@Andrew - validate parameter.name, scale, initial.value, draw from previous sims
            error.prefix = "Error: unable to create monotonic outcome intervention criterion: "
            # parameter.name
            
            # parameter.initial.value
            if (is.numeric(parameter.initial.value))
            {
                #@Andrew - should be a single, non-na numeric
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
            
            # Store variables
            #@Andrew - store parameter stuff
            
            private$i.parameter.name = parameter.name
            private$i.parameter.scale = parameter.scale
            private$i.parameter.initial.value = parameter.initial.value
            private$i.inversely.related = inversely.related
            private$i.draw.parameter.from.previous.sims = draw.parameters.from.previous.sims
            
            private$i.outcome = outcome
            private$i.target.value = target.value
            
            private$i.min.acceptable.value = min.acceptable.value
            private$i.max.acceptable.value = max.acceptable.value
            private$i.subsequent.thresholds.apply.after.iteration = subsequent.thresholds.apply.after.iteration
            
            # private$i.stratify.outcome.by.dimensions = stratify.outcome.by.dimensions
            private$i.dimension.values = dimension.values
            
            # Todd didn't make this:
            private$i.score.metric = "normal.with.coefficient.of.variance"
            private$i.coefficient.of.variance = 0.5
        },
        
        is.satisfied = function(sim, iteration)
        {
            sim.value = private$get.sim.value(sim)
            
            threshold.mask = c(iteration < private$i.subsequent.thresholds.apply.after.iteration, T)
            min.acceptable = private$i.min.acceptable.value[threshold.mask][1]
            max.acceptable = private$i.max.acceptable.value[threshold.mask][1]
            
            sim.value >= min.acceptable & sim.value <= max.acceptable
        },
        # added until we do something taking multiple criteria into account simultaneously
        suggest.new.parameter = function(sim, iteration, debug=F) {
            if (debug) browser()
            sim.value = as.vector(private$get.sim.value(sim))
            if (!private$i.inversely.related) {
                percentage.incorrect = (private$i.target.value - sim.value) / private$i.target.value
                old.param.value = sim$params[private$i.parameter.name]
                # print(paste0("iteration ", iteration, ": sim.value: ", sim.value, " target.value: ", private$i.target.value, " old.param.value: ", old.param.value, " percentage incorrect: ", percentage.incorrect))
                new.param.value = old.param.value * (1 + percentage.incorrect)
            }
            else {
                multiplier = private$i.target.value / sim.value
                old.param.value = sim$params[private$i.parameter.name]
                print(paste0("iteration ", iteration, ": sim.value: ", sim.value, " target.value: ", private$i.target.value, " old.param.value: ", old.param.value, " multipler: ", multiplier))
                new.param.value = old.param.value / multiplier
            }
        },
        
        score.sim = function(sim, iteration, verbose, as.log.likelihood=T, debug=F)
        {
            #@Andrew - may need to modify
            if (debug) browser()
            P.IN.RANGE = 0.9999
            
            #-- Prep the bounds --#
            pre.transformed.sim.value = sim.value = private$get.sim.value(sim)
            low = private$i.min.acceptable.value[1]
            high = private$i.max.acceptable.value[1]
            scale = sim$outcome.metadata[[private$i.outcome]]$scale
            #mid = pre.transformed.mid = (high + low) / 2
            
            above.range = sim.value > high
            below.range = sim.value < low
            is.in.range = !above.range & !below.range
            
            #-- Transform to unbounded scale --#        
            sim.value = transform.to.unbounded.scale(sim.value, scale)
            low = transform.to.unbounded.scale(low, scale)
            high = transform.to.unbounded.scale(high, scale)
            target = transform.to.unbounded.scale(private$i.target.value, scale)
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
            
            if (is.na(log.f))
                browser()
            
            if (as.log.likelihood)
                log.f
            else
                exp(-log.f)
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
                                        "' in the '", specification.metadata$version, "' specification instance in this simulation - and so cannot be used as dimension values in the criterion"))
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
        }
    ),
    
    private = list(
        
        i.parameter.name = NULL,
        i.outcome = NULL,
        i.inversely.related = NULL,
        
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
        
        get.sim.value = function(sim)
        {
            dimension.values.this.sim = private$i.dimension.values
            if (!('year' %in% names(private$i.dimension.values)))
                dimension.values.this.sim['year'] = sim$to.year
            sim$get(outcomes = private$i.outcome,
                    dimension.values = dimension.values.this.sim,
                    keep.dimensions = character()) #by definition, only allowed to get a single scalar
        }
        
    )
)
