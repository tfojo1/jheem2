
##---------------------------------------------------------------##
##---------------------------------------------------------------##
##-- THE INTERFACE for CREATING "CRITERIA-BASED" INTERVENTIONS --##
##---------------------------------------------------------------##
##---------------------------------------------------------------##

#'@name Create a criterion for criteria-based interventions
#'
#'@param outcome A character vector indicating the name of the outcome to which the criterion applies
#'@param target.value A single, numeric value that we want the outcome to get to
#'@param min.acceptable.value,max.acceptable.value Indicators of the min and max possible values of the outcome we would be willing to accept. Must be either (1) single, numeric values or (2) numeric vectors. In this case, the first values are the min/max threshold until subsequent.thresholds.apply.after.iteration[1], the second values are the thresholds prior to subsequent.thresholds.apply.after.iteration[2], etc
#'@param subsequent.thresholds.apply.after.iteration The number of iterations after which the 2nd, 3rd, etc thresholds in min.acceptable.value, max.acceptable.value apply
#'@param stratify.outcome.by.dimensions A character vector giving the names of dimensions according to which the outcome should be stratified. Each value in the stratified outcome for the sim must approach the target
#'@param dimension.values The dimension values for which we should pull the outcome. Must be a named list of either character vectors, integer vectors, or logical vectors
#'@param ... An alternative way of supplying dimensions values - must be named character vectors, integer vectors, or logical vectors
#'
#'@export
create.intervention.criterion <- function(outcome,
                                          target.value,
                                          min.acceptable.value,
                                          max.acceptable.value,
                                          subsequent.thresholds.apply.after.iteration=numeric(),
                                          stratify.outcome.by.dimensions = character(),
                                          dimension.values = list(),
                                          ...,
                                          score.metric = c('normal.with.coefficient.of.variance','normal.with.sqrt.sd','root.squared.error')[1],
                                          coefficient.of.variance = 1,
                                          aggregate.scores.as = c('mean','sum')[1],
                                          weight = 1)
{
    JHEEM.OUTCOME.INTERVENTION.CRITERION$new(outcome = outcome,
                                             target.value = target.value,
                                             min.acceptable.value = min.acceptable.value,
                                             max.acceptable.value = max.acceptable.value,
                                             subsequent.thresholds.apply.after.iteration = subsequent.thresholds.apply.after.iteration,
                                             stratify.outcome.by.dimensions = stratify.outcome.by.dimensions,
                                             dimension.values = dimension.values,
                                             ...,
                                             score.metric = score.metric,
                                             coefficient.of.variance = coefficient.of.variance,
                                             aggregate.scores.as = aggregate.scores.as,
                                             weight = weight)
}

#'@name Create a "Guess-and-Check" Intervention that Must Satisfy Some Criteria
#'
#'@param base.intervention A single.iteration.intervention, as created by \code{\link{create.intervention}} or \code{\link{join.interventions}}
#'@param completion.criteria Either a single jheem.intervention.criterion object, as created by \code{\link{create.intervention.criterion}} or a list containing only jheem.intervention.criterion objects
#'@param parameters.to.vary A character vector giving the names of parameters which will vary in trying to satisfy the given criteria
#'@param parameter.scales
#'@param initial.parameter.values Either (1) 
#'@param max.iterations The maximum number of iterations to run in trying to satisfy intervention criteria. If criteria are not satisfied after max.iterations, will 'give up' (and either produce degenerate simulations or throw an error, depending on max.failure.rate)
#'@param n.iterations.after.satisfying.criteria The number of iterations to run after we have satisfied the criteria to try to get as close as possible to the target
#'@param max.failure.rate The maximum proportion of simulations in a simulation set which can fail to find a solution that satisfies intervention criteria within max.iterations. If this threshold is exceeded, will throw an error; otherwise, will return degenerate simulations when it could not find a solution
#'@param draw.parameters.from.previous.sims
#'
#'@export
create.criteria.based.intervention <- function(base.intervention,
                                               completion.criteria,
                                               parameter.scales,
                                               initial.parameter.values,
                                               max.iterations,
                                               n.iterations.after.satisfying.criteria = 5,
                                               max.iterations.first.sim = max.iterations,
                                               n.iterations.after.satisfying.criteria.first.sim = ceiling(max.iterations.first.sim/2),
                                               max.failure.rate = 0,
                                               code=NULL, 
                                               name=NULL, 
                                               parameter.distribution=NULL,
                                               parameters.to.vary = names(parameter.scales),
                                               draw.parameters.from.previous.sims = !is.function(initial.parameter.values))
{
    CRITERIA.BASED.INTERVENTION$new(base.intervention = base.intervention,
                                    completion.criteria = completion.criteria,
                                    parameter.scales = parameter.scales,
                                    initial.parameter.values = initial.parameter.values,
                                    max.iterations = max.iterations,
                                    n.iterations.after.satisfying.criteria = n.iterations.after.satisfying.criteria,
                                    max.iterations.first.sim = max.iterations.first.sim,
                                    n.iterations.after.satisfying.criteria.first.sim = n.iterations.after.satisfying.criteria.first.sim,
                                    max.failure.rate = max.failure.rate,
                                    code = code,
                                    name = name,
                                    parameter.distribution = parameter.distribution,
                                    parameters.to.vary = parameters.to.vary,
                                    draw.parameters.from.previous.sims = draw.parameters.from.previous.sims)
}



CRITERIA.BASED.INTERVENTION = R6::R6Class(
    'multiple.iteration.intervention',
    inherit = JHEEM.INTERVENTION,
    
    public = list(
        
        initialize = function(base.intervention,
                              completion.criteria,
                              parameter.scales,
                              initial.parameter.values,
                              max.iterations,
                              n.iterations.after.satisfying.criteria,
                              max.iterations.first.sim,
                              n.iterations.after.satisfying.criteria.first.sim,
                              max.failure.rate,
                              code=NULL, 
                              name=NULL, 
                              parameter.distribution=NULL,
                              parameters.to.vary = names(parameter.scales),
                              draw.parameters.from.previous.sims = !is.function(initial.parameter.values))
        {
            super$initialize(code = code,
                             name = name,
                             parameter.distribution = parameter.distribution)
            
            error.prefix = "Cannot create criteria-based intervention: "
            
            # Base Intervention
            if (!is(base.intervention, 'jheem.standard.intervention'))
                stop(paste0(error.prefix, "'base.intervention' must be an object of class jheem.standard.intervention - as created by create.intervention() or join.interventions()"))
            
            # Completion Criteria
            if (is(completion.criteria, "jheem.intervention.criterion"))
                completion.criteria = list(completion.criteria)
            
            if (!is.list(completion.criteria))
                stop(paste0(error.prefix, "'completion.critera' must be either a 'jheem.intervention.criterion' object or a list of 'jheem.intervention.criterion' objects"))
            
            if (length(completion.criteria)==0)
                stop(paste0(error.prefix, "'completion.criteria' cannot be an empty list"))
            
            if (any(!sapply(completion.criteria, is, "jheem.intervention.criterion")))
                stop(paste0(error.prefix, "'completion.critera' must be either a 'jheem.intervention.criterion' object or a list of 'jheem.intervention.criterion' objects"))
            
            # parameters to vary
            if (!is.character(parameters.to.vary) || length(parameters.to.vary)==0 || any(is.na(parameters.to.vary)))
                stop(paste0(error.prefix, "'parameters.to.vary' must be a non-empty character vector with no NA values"))
            
            if (!is.character(parameter.scales) || any(is.na(parameter.scales)))
                stop(paste0(error.prefix, "'parameter.scales' must be a character vector with no NA values"))
            
            check.model.scale(scale = parameter.scales,
                              varname.for.error = "'parameter.scales'",
                              require.scalar = F,
                              error.prefix = error.prefix)
            
            if (is.null(names(parameter.scales)))
                stop(paste0(error.prefix, "'parameter.scales' must be a NAMED character vector"))
            
            if (length(setdiff(parameters.to.vary, names(parameter.scales))))
                stop(paste0(error.prefix, "'parameter.scales' must be named with the same values as those in 'parameters.to.vary"))
            parameter.scales = parameter.scales[parameters.to.vary]
            
            
            # initial.parameter.values
            if (is.numeric(initial.parameter.values))
            {
                if (any(is.na(initial.parameter.values)))
                    stop(paste0(error.prefix, "'initial.parameter.values' cannot contain NA values"))
                if (is.null(names(initial.parameter.values)))
                    stop(paste0(error.prefix, "If 'initial.parameter.values' is a numeric vector, it must be NAMED"))
                
                missing.parameters = setdiff(names(initial.parameter.values), parameters.to.vary)
                if (length(missing.parameters)>0)
                    stop(paste0(error.prefix, "If 'initial.parameter.values' is a numeric vector, it must contain values for all of parameters.to.vary (missing ",
                                collapse.with.and("'", missing.parameters, "'"), ")"))
            }
            else if (is.function(initial.parameter.values))
            {
                arg.names = get.function.argument.names(fn = initial.parameter.values,
                                                        exclude.arguments.with.default.values = T)
                
                if (length(arg.names)!=1 || arg.names!='sim')
                    stop(paste0(error.prefix, "If 'initial.parameter.values' is a function, it must take one and only one argument without a default value: 'sim'"))
            }
            else
                stop(paste0(error.prefix,
                            "'initial.parameter.values' must be either a named numeric vector, or a function that takes a sim and returns a named numeric vector"))
            
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
            
            private$i.parameters.to.vary = parameters.to.vary
            private$i.parameter.scales = parameter.scales
            
            private$i.initial.parameter.values = initial.parameter.values
            private$i.n.iterations.after.satisfying.criteria = n.iterations.after.satisfying.criteria
            private$i.max.iterations = max.iterations
            private$i.n.iterations.after.satisfying.criteria.first.sim = n.iterations.after.satisfying.criteria.first.sim
            private$i.max.iterations.first.sim = max.iterations.first.sim
            private$i.max.failure.rate = max.failure.rate
            private$i.draw.parameters.from.previous.sims = draw.parameters.from.previous.sims
            
        },
        
        get.intervention.foregrounds = function()
        {
            private$i.base.intervention$get.intervention.foregrounds()
        },
        
        do.check.can.apply = function(version, location, sub.version, error.prefix)
        {
            # The default does nothing additional
        }
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
        
        i.parameters.to.vary = NULL,
        i.parameter.scales = NULL,
        
        i.initial.parameter.values = NULL,
        i.initial.parameters.fn.name = NULL,
        i.draw.parameters.from.previous.sims = NULL,
        
        i.n.iterations.after.satisfying.criteria = NULL,
        i.max.iterations = NULL,
        i.n.iterations.after.satisfying.criteria.first.sim = NULL,
        i.max.iterations.first.sim = NULL,
        i.max.failure.rate = NULL,
        i.method = NULL,
        
        # run-time parameters - updated for each run
        i.previous.parameter.values = NULL,
        i.n.failures = NULL,
        i.n.sim = NULL,
        
        # MCMC parameters
        i.mcmc.cov.mat = NULL,
        i.first.mcmc.scaling.parameters = NULL,
        i.second.mcmc.scaling.parameters = NULL,
        i.first.mcmc.prior.iter = NULL,
        i.second.mcmc.prior.iter = NULL,
        
        is.equal.to = function(other)
        {
            stop("need to implement is.equal.to for criteria-based-intervention")
        },
        
        prepare.to.run = function(engine, sim, verbose)
        {
            all.parameter.names = engine$parameter.names
            if (!is.null(private$i.parameter.distribution))
                all.parameter.names = union(all.parameter.names, private$i.parameter.distribution@var.names)
            invalid.parameters.to.track = setdiff(private$i.parameters.to.vary,
                                                  c(all.parameter.names, self$depends.on))
            
            if (length(invalid.parameters.to.track))
                stop(paste0("Cannot run intervention: ",
                            collapse.with.and("'", invalid.parameters.to.track, "'"),
                            ifelse(length(invalid.parameters.to.track)==1, 
                                   " was set as a parameter.to.track, but is",
                                   " were set as parameters.to.track, but are"),
                            " not present in either the simulation, the base.intervention, or the intervention's parameter distribution"))
            
            private$i.previous.parameter.values = NULL
            private$i.n.failures = 0
            private$i.n.sim = sim$n.sim
            
            private$i.mcmc.cov.mat = NULL
            private$i.first.mcmc.scaling.parameters = private$i.second.mcmc.scaling.parameters =
                as.list(rep(2.38^2, length(private$i.parameters.to.vary)))
            
            private$i.first.mcmc.prior.iter = private$i.second.mcmc.prior.iter = 0
        },
        
        do.run = function(engine, sim.index, parameters, verbose)
        {
            error.prefix = "Cannot run criteria-based intervention: "
            
            #-- STEP 1: Pull Initial Values --#
            if (private$i.draw.parameters.from.previous.sims && 
                !is.null(private$i.previous.parameter.values))
            {
                initial.values = rowMeans(private$i.previous.parameter.values)
            }
            else if (is.numeric(private$i.initial.parameter.values))
                initial.values = private$i.initial.parameter.values
            else
            {
                initial.values = private$i.initial.parameter.values(sim)
                
                if (is.null(private$i.initial.parameters.fn.name))
                    descriptor = "the criteria's initial.parameter.values function"
                else
                    descriptor = paste0(private$i.initial.parameters.fn.name, "(the criteria's initial.parameter.values function)")
                
                if (!is.numeric(initial.values) || length(initial.values)!=length(private$i.parameters.to.vary) || any(is.na(initial.values)))
                {   
                    stop(paste0(error.prefix,
                                "The value returned by ", descriptor ," must be a numeric vector with exactly ",
                                length(private$i.parameters.to.vary), " non-NA ",
                                ifelse(length(private$i.parameters.to.vary)==1, 
                                       "value (for the parameter to vary)",
                                       "values (one for each parameter to vary)")))
                }
                
                if (is.null(names(initial.values)))
                    stop(paste0(error.prefix,
                                "The value returned by ", descriptor, " must be a NAMED numeric vector"))
                
                if (!setequal(names(initial.values), private$i.parameters.to.vary))
                {
                    if (length(private$i.parameters.to.vary)==1)
                        pl = ''
                    else
                        pl = 's'
                    
                    stop(paste0(error.prefix,
                                "The names of the value", pl, " returned by ", descriptor, 
                                " must exactly match the parameter", pl, " to vary (",
                                collapse.with.and("'", private$i.parameters.to.vary, "'"), ")"))
                }
            }
            
            initial.values = initial.values[private$i.parameters.to.vary]
            
            if (is.null(private$i.mcmc.cov.mat))
            {
                private$i.mcmc.cov.mat = matrix(0, nrow=length(initial.values), ncol=length(initial.values))
                diag(private$i.mcmc.cov.mat) = initial.values/40
            }
            
            #-- STEP 2: Set up run function for MCMC Optimization --#
            
            # Control variables to be modified from within the MCMC using <<-
            mcmc.best.log.likelihood = -Inf
            mcmc.best.sim = NULL
            target.accept.every.nth = 4
            target.acceptance.rate = 1/target.accept.every.nth
            
            if (sim.index==1)
            {
                first.mcmc.max.iterations = private$i.max.iterations.first.sim
                second.mcmc.iterations = private$i.n.iterations.after.satisfying.criteria.first.sim
            }
            else
            {
                first.mcmc.max.iterations = private$i.max.iterations
                second.mcmc.iterations = private$i.n.iterations.after.satisfying.criteria
            }
            sim = NULL
            
            # runs the sim, saves it if appropriate
            fn = function(par){
                
                if (mcmc.iteration >= mcmc.max.iterations ||
                    (is.first.mcmc && !is.null(mcmc.best.sim)))
                {
                    if ((mcmc.iteration %% target.accept.every.nth)==0)
                        Inf
                    else
                        -Inf
                }
                else
                {
                    mcmc.iteration <<- mcmc.iteration + 1
                    
                    parameters[private$i.parameters.to.vary] = par
                    
                    if (verbose)
                        base::print(paste0("Iteration ", mcmc.iteration, ": ", paste0(private$i.parameters.to.vary, " = ", round(parameters[private$i.parameters.to.vary], 4), collapse=', ')))
                    
                    sim <<- engine$run(parameters = parameters,
                                       prior.sim.index = sim.index)
                    
                    criteria.satisfied = all(sapply(private$i.completion.criteria, function(criterion){
                        criterion$is.satisfied(sim, iteration = mcmc.iteration)
                    }))
                    
                    sub.liks =  sapply(private$i.completion.criteria, function(criterion){
                        criterion$score.sim(sim = sim, 
                                            iteration = mcmc.iteration, 
                                            verbose = verbose,
                                            as.log.likelihood = T)
                    })
                    lik = sum(sub.liks)
                    
                    if (mcmc.iteration==1 && any(sub.liks==-Inf))
                        stop(paste0(error.prefix, "The ", 
                                    ifelse(length(private$i.parametrs.to.vary)==1, "initial value", "initial set of values"),
                                    " for parameters.to.vary (",
                                    collapse.with.and("'", private$i.parameters.to.vary, "'"),
                                    ") is SO far from a solution that the ",
                                    ifelse(sum(sub.liks==-Inf)==1, "criterion score", "criteria scores"),
                                    " for ",
                                    collapse.with.and("'", sapply(private$i.completion.criteria[sub.liks==-Inf], function(cr){cr$descriptor}), "'"),
                                    ifelse(sum(sub.liks==-Inf)==1, " evaluates", " evaluate"),
                                    " to infinity - pick ",
                                    ifelse(length(private$i.parametrs.to.vary)==1, "a better starting value",
                                           "better starting values")))
                    
                    if (verbose)
                        base::print(paste0("   --> log-likelihood = ", round(lik,3),
                                           ", ?satified = ", criteria.satisfied))
                    
                    if (criteria.satisfied)
                    {
                        if (is.null(mcmc.best.sim))
                        {
                            optim.max.iterations <<- min(optim.max.iterations,
                                                         optim.iteration + private$i.n.iterations.after.satisfying.criteria)
                        }
                        
                        if (lik > mcmc.best.log.likelihood)
                        {
                            mcmc.best.sim <<- sim
                            mcmc.best.log.likelihood <<- lik
                        }
                    }
                    
                    lik
                }
            }
            
            #-- STEP 3 Set up the shared controls for both mcmcs --#
            
            ctrl = list(
                var.names = private$i.parameters.to.vary,
                simulation.function = fn,
                log.prior.distribution = function(...){1},
                log.likelihood = function(x){x},
                burn = 0,
                thin = 1,
                var.blocks = as.list(private$i.parameters.to.vary),
                reset.adaptive.scaling.update.after = 0,
                transformations = sapply(private$i.parameter.scales, function(scale){
                    tsfx = MODEL.SCALE.INFO[[scale]]$unbounded.transformation$type
                    if (tsfx=='logistic')
                        'logit'
                    else
                        tsfx
                }),
                
                target.acceptance.probability = target.acceptance.rate,
                
                n.iter.before.use.adaptive.covariance = 0,
                adaptive.covariance.base.update = 0.2,
                adaptive.covariance.update.decay = 0.5,
                adaptive.scaling = 'componentwise',
                adaptive.scaling.base.update = 1,
                adaptive.scaling.update.decay = 0.5
            )
            
            #-- STEP 4: Run first MCMC to get to a point that satisfies the criteria --#
            
            first.ctrl = do.call(bayesian.simulations::create.adaptive.blockwise.metropolis.control,
                                 c(ctrl,
                                   list(
                                       initial.covariance.mat = private$i.mcmc.cov.mat,
                                       initial.scaling.parameters = private$i.first.mcmc.scaling.parameters,
                                       adaptive.covariance.update.prior.iter = private$i.first.mcmc.prior.iter,
                                       adaptive.scaling.update.prior.iter= private$i.first.mcmc.prior.iter
                                   )
                                 ))
            
            mcmc.iteration = 0
            mcmc.max.iterations = first.mcmc.max.iterations
            is.first.mcmc = T
            
            mcmc = bayesian.simulations::run.mcmc(control = first.ctrl,
                                                  n.iter = mcmc.max.iterations,
                                                  starting.values = initial.values,
                                                  update.detail = 'none',
                                                  update.frequency = 1,
                                                  cache.frequency = NA)
            
            # Update mcmc parameters for next sim
            private$i.first.mcmc.prior.iter = private$i.first.mcmc.prior.iter + mcmc.iteration
            private$i.first.mcmc.scaling.parameters = lapply(mcmc@chain.states[[1]]@log.scaling.parameters, exp)
            
            # Check the result
            if (is.null(optim.best.sim))
            {
                private$i.n.failures = private$i.n.failures + 1
                failure.rate = private$i.n.failures / private$i.n.sim
                
                if (failure.rate > private$i.max.failure.rate)
                    stop(paste0(error.prefix, "We have been unable to satisfy all criteria ",
                                ifelse(private$i.max.failure.rate==0, "on a simulation",
                                       paste0("on ", private$i.n.failures, ifelse(private$i.n.failures==1, " simulation", " simulations"),
                                              " --> a failure rate of ", round(100*failure.rate, 1), "%, which exceeds the maximum tolerable failure rate of ",
                                              round(100*private$i.max.failure.rate, 1), '%'))))
                
                optim.best.sim = derive.degenerate.simulation(sim)
            }
            else
            {
                values.after.first.mcmc = mcmc@chain.states[[1]]@current.parameters
                
                #-- STEP 5: Run a second MCMC to optimize within range that satisfies criteria --#
                
                second.ctrl = do.call(bayesian.simulations::create.adaptive.blockwise.metropolis.control,
                                      c(ctrl,
                                        list(
                                            initial.covariance.mat = private$i.mcmc.cov.mat,
                                            initial.scaling.parameters = private$i.second.mcmc.scaling.parameters,
                                            adaptive.covariance.update.prior.iter = private$i.second.mcmc.prior.iter,
                                            adaptive.scaling.update.prior.iter= private$i.second.mcmc.prior.iter
                                        )
                                      ))
                
                mcmc.iteration = 0
                mcmc.max.iterations = Inf
                is.first.mcmc = F
                
                mcmc = bayesian.simulations::run.mcmc(control = second.ctrl,
                                                      n.iter = mcmc.max.iterations,
                                                      starting.values = values.after.first.mcmc,
                                                      update.detail = 'none',
                                                      update.frequency = 1,
                                                      cache.frequency = NA)
                
                # Update mcmc parameters for next sim
                private$i.second.mcmc.prior.iter = private$i.second.mcmc.prior.iter + mcmc.iteration
                private$i.second.mcmc.scaling.parameters = lapply(mcmc@chain.states[[1]]@log.scaling.parameters, exp)
                private$i.mcmc.cov.mat = mcmc@chain.states[[1]]@cov.mat
                
                
                #-- STEP 6: Save The Results for Future Sim Iterations --#
                
                if (private$i.draw.parameters.from.previous.sims)
                    private$i.previous.parameter.values = cbind(private$i.previous.parameter.values, parameters[private$i.parameters.to.vary])
            }            
            
            
            #-- STEP 7: DONE! Return --#
            
            optim.best.sim
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

JHEEM.INTERVENTION.CRITERION = R6::R6Class(
    'jheem.intervention.criterion',
    
    public = list(
        
        initialize = function()
        {
            
        },
        
        is.satisfied = function(sim, iteration)
        {
            stop("is.satisfied() for a jheem.intervention.criterion must be implemented at the subclass level")
        },
        
        score.sim = function(sim, iteration, verbose, as.log.likelihood)
        {
            stop("score.sim() for a jheem.intervention.criterion must be implemented at the subclass level")
        },
        
        validate = function(jheem.kernel,
                            sub.version = NULL,
                            simulation.metadata = NULL,
                            error.prefix = '')
        {
            stop("The validate() method must be implemented at the sub-class level for jheem.intervention.criterion")
        }
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                stop("The 'descriptor' active binding must be defined at the sub-class level for a 'jheem.intervention.criterion'")
            else
                stop("Cannot modify 'descriptor' for a jheem.intervention - it is read-only")
        }
    )
)

JHEEM.OUTCOME.INTERVENTION.CRITERION = R6::R6Class(
    'jheem.outcome.intervention.criterion',
    inherit = JHEEM.INTERVENTION.CRITERION,
    
    public = list(
        
        initialize = function(outcome,
                              target.value,
                              min.acceptable.value,
                              max.acceptable.value,
                              subsequent.thresholds.apply.after.iteration = numeric(),
                              stratify.outcome.by.dimensions = character(),
                              dimension.values = list(),
                              ...,
                              score.metric = c('normal.with.coefficient.of.variance','normal.with.sqrt.sd','root.squared.error')[1],
                              coefficient.of.variance = 0.1,
                              aggregate.scores.as = c('mean','sum')[1],
                              weight = 1)
        {
            super$initialize()
            
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
            if (!is.character(stratify.outcome.by.dimensions) || any(is.na(stratify.outcome.by.dimensions)))
                stop(paste0(error.prefix, "'stratify.outcome.by.dimensions' must be a character vector with no NA values"))
            
            if (length(unique(stratify.outcome.by.dimensions) != length(stratify.outcome.by.dimensions)))
                stop(paste0(error.prefix, "The elements of 'stratify.outcome.by.dimensions' must be unique"))
            
            
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
            
            # Score stuff
            if (!is.character(score.metric) || length(score.metric)!=1 || is.na(score.metric))
                stop(paste0(error.prefix, "'score.metric' must be a single, non-NA character value"))
            
            allowed.score.metrics = c('normal.with.coefficient.of.variance','normal.with.sqrt.sd','root.squared.error')
            if (all(score.metric != allowed.score.metrics))
                stop(paste0(error.prefix, "'score.metric' must be either ",
                            collapse.with.or("'", allowed.score.metrics, "'")))
            
            if (!is.numeric(coefficient.of.variance) || length(coefficient.of.variance)!=1 || is.na(coefficient.of.variance))
                stop(paste0(error.prefix, "'coefficient.of.variance' must be a single, non-NA, numeric value"))
            
            if (coefficient.of.variance <= 0)
                stop(paste0(error.prefix, "'coefficient.of.variance' must be a positive number"))
            
            if (!is.character(aggregate.scores.as) || length(aggregate.scores.as)!=1 || is.na(aggregate.scores.as))
                stop(paste0(error.prefix, "'aggregate.scores.as' must be a single, non-NA character value"))
            
            allowed.aggregate.scores.as = c('mean','sum')
            if (all(aggregate.scores.as != allowed.aggregate.scores.as))
                stop(paste0(error.prefix, "'aggregate.scores.as' must be one of ",
                            collapse.with.or("'", allowed.aggregate.scores.as, "'")))
            
            if (!is.numeric(weight) || length(weight)!=1 || is.na(weight))
                stop(paste0(error.prefix, "'weight' must be a single, non-NA, numeric value"))
            
            if (weight <= 0)
                stop(paste0(error.prefix, "'weight' must be a positive number"))
            
            
            # Store variables
            private$i.outcome = outcome
            private$i.target.value = target.value
            
            private$i.min.acceptable.value = min.acceptable.value
            private$i.max.acceptable.value = max.acceptable.value
            private$i.subsequent.thresholds.apply.after.iteration = subsequent.thresholds.apply.after.iteration
            
            private$i.stratify.outcome.by.dimensions = stratify.outcome.by.dimensions
            private$i.dimension.values = dimension.values
            
            private$i.score.metric = score.metric
            private$i.coefficient.of.variance = coefficient.of.variance
            private$i.aggregate.scores.as = aggregate.scores.as
            private$i.score.weight = weight
        },
        
        is.satisfied = function(sim, iteration)
        {
            sim.value = private$get.sim.value(sim)
            
            threshold.mask = c(iteration < private$i.subsequent.thresholds.apply.after.iteration, T)
            min.acceptable = private$i.min.acceptable.value[threshold.mask][1]
            max.acceptable = private$i.max.acceptable.value[threshold.mask][1]
            
            rv = all(sim.value >= min.acceptable & sim.value <= max.acceptable)
            
            rv
        },
        
        score.sim = function(sim, iteration, verbose, as.log.likelihood=T)
        {
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
        
        OLD.score.sim = function(sim, iteration, verbose, as.log.likelihood=T)
        {
            OUT.OF.RANGE.WEIGHT = 3
            IN.RANGE.WEIGHT = 1
            
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
            
            in.range.sd = sd.for.score / sqrt(IN.RANGE.WEIGHT)
            out.of.range.sd = sd.for.score / sqrt(OUT.OF.RANGE.WEIGHT)
            
            #-- Fundamentally, the likelihood we are calculating here is a two-way mixture:
            #   p = 1/2 * p_in_range + 1/2 * p_out_of_range
            #   where
            #   p_in_range = dnorm(sim.value, target, in.range.sd)
            # 
            #   p_out_of_range = dnorm(sim.value, mid, out.of.range.sd) IF sim.value < low OR sim.value > high
            #                    dnorm(low, mid, out.of.range.df) IF low < sim.value < high NB, since mid is centered between low and high, dnorm(low, ...) = dnorm(high, ...)
            
            #-- Calculate a log p that applies if we are out of range --#
            max.out.of.range.log.p = dnorm(low, mean=mid, sd=out.of.range.sd, log=T) 
            # because this distribution is centered at the mid between low and high, this = dnorm(high, ...)
            
            log.p.out.of.range = dnorm(sim.value, mean=mid, sd=out.of.range.sd, log=T)
            log.p.out.of.range[is.in.range] = max.out.of.range.log.p
            
            #-- Calculate the log p's that applies if we are in range --#
            # min.below.range.log.p = dnorm(low, mean=target, sd=in.range.sd, log=T)
            # min.above.range.log.p = dnorm(high, mean=target, sd=in.range.sd, log=T)
            
            log.p.in.range = dnorm(sim.value, mean=target, sd=in.range.sd, log=T)
            # log.p.in.range[above.range] = max.above.range.log.p
            # log.p.in.range[below.range] = max.below.range.log.p
            
            # log sum exp to combine p in range and p out of range
            max.log.p.component = pmax(log.p.out.of.range,
                                       log.p.in.range)
            
            combined.log.p = 0.5 * # multiply by 0.5 because it's a mixture of two p's, each with weight 0.5
                max.log.p.component + log(exp(log.p.out.of.range - max.log.p.component) + 
                                              exp(log.p.in.range - max.log.p.component))
            
            log.p = sum(combined.log.p)
            
            
            if (verbose)
                base::print(paste0("   ", private$i.outcome, " = ", round(pre.transformed.sim.value,3),
                                   " (p = ", round(exp(log.p), 3), ")"))
            
            if (as.log.likelihood)
                log.p
            else
                exp(-log.p)
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
        }
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                private$i.outcome
            else
                stop("Cannot modify 'descriptor' for a jheem.intervention - it is read-only")
        }
    ),
    
    private = list(
        
        i.outcome = NULL,
        
        i.target.value = NULL,
        i.min.acceptable.value = NULL,
        i.max.acceptable.value = NULL,
        i.subsequent.thresholds.apply.after.iteration = NULL,
        
        i.stratify.outcome.by.dimensions = NULL,
        i.dimension.values = NULL,
        
        i.score.metric = NULL,
        i.coefficient.of.variance = NULL,
        i.aggregate.scores.as = NULL,
        i.score.weight = NULL,
        
        get.sim.value = function(sim)
        {
            sim$get(outcomes = private$i.outcome,
                    dimension.values = private$i.dimension.values,
                    keep.dimensions = private$i.stratify.outcome.by.dimensions)
        }
        
    )
)
