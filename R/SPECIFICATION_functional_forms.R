
# depends on:
#   SPECIFICATION_transformations.R
#   HELPERS_dim_names_helpers.R
#   HELPERS_misc_helpers.R
#   stats packages

##----------------------##
##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##
##----------------------##

##----------------------------##
##-- STATIC FUNCTIONAL FORM --##
##----------------------------##

#'@description Create an Static (Time-Invariant) Functional Form
#'
#'@param value array representing the value
#'@param link The name of the transformation to the scale on which alphas are added to the value. One of "identity", "log", "logit"
#'@param min,max The minimum and maximum values this functional form can create. The default (NA) sets to the min/max for the link specified (eg, 0-Inf for a log link, 0-1 for a logistic link)
#'@param value.is.on.transformed.scale Logical indicating whether the value is already on the transformed scale at which alphas apply
#'@param overwrite.parameters.with.alphas Logical indicating whether, when alphas are applied, they OVERWRITE values. If F, alphas are added to the transformed value (on the transformed scale)
#'@param error.prefix String to prepend to any error message in case of an error
#'
#'@export
create.static.functional.form <- function(value,
                                          link=c('identity','log','logit')[1],
                                          min = NA,
                                          max = NA,
                                          value.name='value',
                                          value.is.on.transformed.scale = F,
                                          overwrite.parameters.with.alphas = F,
                                          error.prefix = 'Cannot create static functional.form: ')
{
    STATIC.FUNCTIONAL.FORM$new(value = value,
                               link = link,
                               min = min,
                               max = max,
                               value.name = value.name,
                               value.is.on.transformed.scale = value.is.on.transformed.scale,
                               overwrite.parameters.with.alphas = overwrite.parameters.with.alphas,
                               error.prefix = error.prefix)
}


##-------------------------------------------------##
##-- LINEAR (INTERCEPT + SLOPE) FUNCTIONAL FORMS --##
##-------------------------------------------------##


#'@description Create an Functional Form that is Linear (possibly on a transformed scale)
#'
#'@inheritParams create.static.functional.form
#'@param intercept,slope Arrays or scalar values representing the intercept and slope on the transformed scale
#'@param parameters.is.on.transformed.scale Logical indicating whether the intercept and slope are already on the transformed scale at which alphas apply
#'@param anchor.year The year at which the functional form evaluates to the intercept
#'
#'@seealso create.static.functional.form, create.log.linear.functional.form, create.logistic.linear.functional.form
#'
#'@export
create.linear.functional.form <- function(intercept,
                                          slope,
                                          anchor.year,
                                          link = c('identity','log','logit')[1],
                                          min = NA,
                                          max = NA,
                                          parameters.are.on.transformed.scale = T,
                                          overwrite.parameters.with.alphas = F,
                                          error.prefix = 'Cannot create linear functional.form: ')
{
    LINEAR.FUNCTIONAL.FORM$new(intercept = intercept,
                               slope = slope,
                               anchor.year = anchor.year,
                               link = link,
                               min = min,
                               max = max,
                               parameters.are.on.transformed.scale = parameters.are.on.transformed.scale,
                               overwrite.parameters.with.alphas = overwrite.parameters.with.alphas,
                               error.prefix = error.prefix)
}

#'@description Create a Functional Form that is Linear on the Log Scale
#'
#'@inheritParams create.linear.functional.form
#'@param intercept,slope Arrays or scalar values representing the intercept and slope on the log scale
#'@param parameters.are.on.log.scale Logical indicating whether the intercept and slope are already on the log scale
#'
#'@seealso create.static.functional.form, create.linear.functional.form, create.logistic.linear.functional.form
#'
#'@export
create.log.linear.functional.form <- function(intercept,
                                              slope,
                                              anchor.year,
                                              min = 0,
                                              max = Inf,
                                              parameters.are.on.log.scale = T,
                                              overwrite.parameters.with.alphas=F,
                                              error.prefix = 'Cannot create log-linear functional.form: ')
{
    create.linear.functional.form(intercept = intercept,
                                  slope = slope,
                                  anchor.year = anchor.year,
                                  link = 'log',
                                  min = min,
                                  max = max,
                                  parameters.are.on.transformed.scale = parameters.are.on.log.scale,
                                  overwrite.parameters.with.alphas = overwrite.parameters.with.alphas,
                                  error.prefix = error.prefix)
}

#'@description Create a Functional Form that is Linear on the Logistic Scale
#'
#'@inheritParams create.linear.functional.form
#'@param intercept,slope Arrays or scalar values representing the intercept and slope on the logistic scale
#'@param parameters.are.on.logit.scale Logical indicating whether the intercept and slope are already on the logistic scale
#'
#'@seealso create.static.functional.form, create.linear.functional.form, create.log.linear.functional.form
#'
#'@export
create.logistic.linear.functional.form <- function(intercept,
                                                   slope,
                                                   anchor.year,
                                                   min=0,
                                                   max=1,
                                                   parameters.are.on.logit.scale = T,
                                                   overwrite.parameters.with.alphas = F,
                                                   error.prefix = 'Cannot create logistic-linear functional.form: ')
{
    create.linear.functional.form(intercept = intercept,
                                  slope = slope,
                                  anchor.year = anchor.year,
                                  link = 'logit',
                                  min = min,
                                  max = max,
                                  parameters.are.on.transformed.scale = parameters.are.on.logit.scale,
                                  overwrite.parameters.with.alphas = overwrite.parameters.with.alphas,
                                  error.prefix = error.prefix)
}


#'@description Create a "Logistic Tail" Functional Form
#'
#'@inheritParams create.linear.functional.form
#'@param intercept,slope Scalar values or arrays representing the intercept and slope. Note that the final form constrains the slope to be non-negative
#'@param logistic.after.frac.of.span The fraction of the the total span (max - min) after which the model follows a logistic curve
#'@param min,max The upper and lower limits to which the functional form can evaluate
#'@param future.slope.is.multiplier A logical indicating whether the future slope should be treated as a multiplier of the current slope (vs being added to it)
#'
#'@export
create.logistic.tail.functional.form <- function(intercept,
                                                 slope,
                                                 anchor.year,
                                                 logistic.after.frac.of.span=0.5,
                                                 link = 'logit',
                                                 min=0,
                                                 max=1,
                                                 parameters.are.on.transformed.scale = T,
                                                 overwrite.parameters.with.alphas = F,
                                                 future.slope.is.multiplier = T,
                                                 error.prefix = 'Cannot create logistic-tail functional.form: ')
{
    LOGISTIC.TAIL.FUNCTIONAL.FORM$new(intercept = intercept,
                                      slope = slope,
                                      anchor.year = anchor.year,
                                      logistic.after.frac.of.span = logistic.after.frac.of.span,
                                      link = link,
                                      min = min,
                                      max = max,
                                      parameters.are.on.transformed.scale = parameters.are.on.transformed.scale,
                                      overwrite.parameters.with.alphas = overwrite.parameters.with.alphas,
                                      future.slope.is.multiplier = future.slope.is.multiplier,
                                      error.prefix = error.prefix)
}


##-----------------------------##
##-- SPLINE FUNCTIONAL FORMS --##
##-----------------------------##


#'@description Create a Linear Spline Functional Form
#'
#'@inheritParams create.linear.functional.form
#'@param knot.times The (numeric) times at which the knots of the spline apply. Must have names set
#'@param knot.values Initial values for the knots. Must have names set, matching the names of knot times
#'@param link The name of a transformation to the scale at which the spline should apply. The knots are transformed to this scale, the spline is applied, and then the splined values are back-transformed. One of 'identity', 'log', or 'logistic'
#'@param knots.are.on.transformed.scale Logical indicating whether the knots are already on the transformed scale at which alphas apply
#'@param before.modifier,after.modifier If either of these is set, an additional knot is added to the specified knots. For before.modifier, this knot is calculated as before.modifier added or multiplier by the value of the first knot (on the model scale); after.modifier is added or multiplied by the value of the last knot
#'@param before.time,after.time The times at which the knots calculated from before.modifier and after.modifier apply, respectively
#'@param overwrite.before.modifier.with.alphas,overwrite.after.modifier.with.alphas Logical indicating whether, when alphas are applied to the before.modifier or after.modifier, they OVERWRITE the modifiers. If F, alphas are added (if modifiers.are.multiplicative.on.model.scale=F) or multiplied(if modifiers.are.multiplicative.on.model.scale=T) to the modifiers on the model scale
#'@param before.modifier.application,after.modifier.application A character vector that specifies how before.modifier and/or after.modifier should be applied. Choices are 'additive.on.model.scale' (the modifier is added to the transformed value of the first or last knot - on the scale that the smoothing is applied), 'multiplicative.on.model.scale' (modifier is multiplied by the transformed first or last knot - applies only if scale=='identity), or 'multiplicative.of.change.on.value.scale' (the before knot is - on the scale of resulting values - equal to the first knot plus the modifier times the change from the second to first knot. Analogous for the after.modifier with last knot and penultimate knot)
#'
#'@export
create.linear.spline.functional.form <- function(knot.times,
                                                 knot.values,
                                                 link = c('identity','log','logit')[1],
                                                 min = NA,
                                                 max = NA,
                                                 
                                                 knots.are.on.transformed.scale=F,
                                                 overwrite.knot.values.with.alphas=F,
                                                 
                                                 before.time=NULL,
                                                 before.modifier=NULL,
                                                 before.modifier.application=NULL,
                                                 overwrite.before.modifier.with.alphas=F,
                                                 
                                                 after.time=NULL,
                                                 after.modifier=NULL,
                                                 after.modifier.application=NULL,
                                                 overwrite.after.modifier.with.alphas=F,
                                                 error.prefix = 'Cannot create linear spline functional.form: ')
{
    LINEAR.SPLINE.FUNCTIONAL.FORM$new(knot.times = knot.times,
                                       knot.values = knot.values,
                                       link = link,
                                       min = min,
                                       max = max,
                                       
                                       knots.are.on.transformed.scale = knots.are.on.transformed.scale,
                                       overwrite.knot.values.with.alphas = overwrite.knot.values.with.alphas,
                                       
                                       before.time = before.time,
                                       before.modifier = before.modifier,
                                       before.modifier.application = before.modifier.application,
                                       overwrite.before.modifier.with.alphas = overwrite.before.modifier.with.alphas,
                                       
                                       after.time = after.time,
                                       after.modifier = after.modifier,
                                       after.modifier.application = after.modifier.application,
                                       overwrite.after.modifier.with.alphas = overwrite.after.modifier.with.alphas,
                                       
                                       error.prefix = error.prefix)
}


#'@description Create a Natural Spline Functional Form
#'
#'@inheritParams create.linear.spline.functional.form
#'
#'@export
create.natural.spline.functional.form <- function(knot.times,
                                                  knot.values,
                                                  link = c('identity','log','logit')[1],
                                                  min = NA,
                                                  max = NA,
                                                  
                                                  knots.are.on.transformed.scale=F,
                                                  overwrite.knot.values.with.alphas=F,
                                                  
                                                  before.time=NULL,
                                                  before.modifier=NULL,
                                                  before.modifier.application=NULL,
                                                  overwrite.before.modifier.with.alphas=F,
                                                  
                                                  after.time=NULL,
                                                  after.modifier=NULL,
                                                  after.modifier.application=NULL,
                                                  overwrite.after.modifier.with.alphas=F,
                                                  
                                                  error.prefix = 'Cannot create natural spline functional.form: ')
{
    NATURAL.SPLINE.FUNCTIONAL.FORM$new(knot.times = knot.times,
                                       knot.values = knot.values,
                                       link = link,
                                       min = min,
                                       max = max,
                                       
                                       knots.are.on.transformed.scale = knots.are.on.transformed.scale,
                                       overwrite.knot.values.with.alphas = overwrite.knot.values.with.alphas,
                                       
                                       before.time = before.time,
                                       before.modifier = before.modifier,
                                       before.modifier.application = before.modifier.application,
                                       overwrite.before.modifier.with.alphas = overwrite.before.modifier.with.alphas,
                                       
                                       after.time = after.time,
                                       after.modifier = after.modifier,
                                       after.modifier.application = after.modifier.application,
                                       overwrite.after.modifier.with.alphas = overwrite.after.modifier.with.alphas,
                                       
                                       error.prefix = error.prefix)
}



#'@description Create a Logistic Spline Functional Form
#'
#'@inheritParams create.linear.spline.functional.form
#'
#'@param fraction.of.asymptote.after.end,fraction.of.asymptote.before.start The fraction of the total change in the first or last segment that would take place before the first knot or after the last
#'@param fraction.of.asymptote.for.change.dir The fraction of the total change in a segment that should take place before between two segments
#'@param fraction.asymptote.link The name of a transformation to the scale at which alphas are added to the betas for the fraction.asymptote.<x> values
#'@param overwrite.fraction.asymptote.with.alphas Logical indicating whether, when alphas are applied to the fraction.of.asymptote.<x>, they OVERWRITE the values. If F, alphas are interpreted as either additive or multiplicative to the knot.values, depending on the value of fraction.asymptote.alphas.are.multipliers
#'
#'@export
create.logistic.spline.functional.form <- function(knot.times,
                                                   knot.values,
                                                   link = 'log',
                                                   overwrite.knot.values.with.alphas = F,
                                                   
                                                   fraction.of.asymptote.after.end=0.05,
                                                   fraction.of.asymptote.before.start=0.025,
                                                   fraction.of.asymptote.for.change.dir=0.02,
                                                   fraction.asymptote.link = 'log',
                                                   overwrite.fraction.asymptote.with.alphas = F,
                                                   
                                                   error.prefix = 'Cannot create logistic spline functional.form: '
)
{
    LOGISTIC.SPLINE.FUNCTIONAL.FORM$new(knot.times = knot.times,
                                        knot.values = knot.values,
                                        link = link,
                                        overwrite.knot.values.with.alphas = overwrite.knot.values.with.alphas,
                                        
                                        fraction.of.asymptote.after.end=fraction.of.asymptote.after.end,
                                        fraction.of.asymptote.before.start=fraction.of.asymptote.before.start,
                                        fraction.of.asymptote.for.change.dir=fraction.of.asymptote.for.change.dir,
                                        fraction.asymptote.link = fraction.asymptote.link,
                                        overwrite.fraction.asymptote.with.alphas = overwrite.fraction.asymptote.with.alphas,
                                        
                                        error.prefix = error.prefix)
}

##---------------------------------------##
##---------------------------------------##
##-- FUNCTIONAL FORM CLASS DEFINITIONS --##
##---------------------------------------##
##---------------------------------------##


#'@export
FUNCTIONAL.FORM = R6::R6Class(
    'functional.form',
    
    public = list(
        
        #'@param type A single character name of the type of functional form
        #'@param betas A named list of the values for the betas
        #'@param link A link object
        #'@param alphas.are.additive Either a single logical value or a named logical vector with the same names as betas
        #'@param alpha.links Either a single link object or a named list of link objects with the same names as betas
        #'@param is.static A single logical value
        initialize = function(type,
                              betas,
                              link,
                              future.slope.link,
                              alphas.are.additive, #either a single logical value or a named logical vector with the same names as betas
                              alpha.links, #either a single value ('identity','log','logit','custom') or named character vector with the same names as betas
                              is.static,
                              error.prefix='') #whether the model is time-varying
        {
            #-- Check Type --#
            if (!is.character(type) || length(type)!=1 || is.na(type) || nchar(type)==0)
                stop(paste0(error.prefix, "A functional.form's 'type' must be a non-empty, non-NA single character value"))
            
            #-- Check link --#
            if (!is(link, 'link'))
                stop(paste0(error.prefix, "A functional.form's 'link' must be a an object of class 'link' (obtained by calling 'get.link')"))
            
            # update type
            if (link$type != 'identity')
                type = paste0(link$type, " ", type)

                        
            #-- Check future.slope.link --#
            if (!is.null(future.slope.link) && !is(future.slope.link, 'link'))
                stop(paste0(error.prefix, "A functional.form's 'future.slope.link' must be either NULL or a an object of class 'link' (obtained by calling 'get.link')"))

            #-- Check Betas --#
            if (!is.list(betas) || length(betas)==0 || is.null(names(betas)) || any(names(betas)==''))
                stop("'betas' must be a named list")
            sapply(names(betas), function(beta.name){
                beta = betas[[beta.name]]
                if (!is.numeric(beta) || length(beta)==0 || any(is.na(beta)))
                    stop(paste0(error.prefix, "'", beta.name, "' must be a non-NA, non-empty, numeric object"))
            })
            
            alpha.names = names(betas)
            if (max(table(alpha.names)>1))
                stop(paste0(error.prefix, "Functional.form 'betas' cannot have repeated names"))
            
            #-- Make the dim.names --#
            dim.names = NULL
            for (i in 1:length(betas))
            {
                beta.name = names(betas)[i]
                beta = betas[[i]]
                beta.dim.names = dimnames(beta)
                if (is.null(beta.dim.names))
                {
                    if (length(beta) != 1)
                        stop(paste0("'", beta.name, 
                                    "' must either be a scalar (single) numeric value or have dim.names set"))
                }
                else
                {
                    if (is.null(names(beta.dim.names)))
                        stop(paste0("The dimnames of '", beta.name, 
                                    "' must be named"))
                    
                    if (!are.dim.names.compatible(dim.names, beta.dim.names))
                        stop(paste0("The dimension names for '", beta.name, 
                                    "' are not compatible with the dimension.names for ",
                                    paste0("'", names(betas)[1:i], "'", collapse=', ')))
                    
                    dim.names = union.matching.dim.names(dim.names, beta.dim.names)
                }
            }
            
            #-- Check alphas.are.additive --#
            if (!is.logical(alphas.are.additive) || length(alphas.are.additive)==0 || any(is.na(alphas.are.additive)))
                stop(paste0(error.prefix,
                            "'alphas.are.additive' for a functional.form ('",
                            type, "') must be either an unnamed logical scalar or a named logical vector"))
            
            if (length(alphas.are.additive)==1)
            {
                if (!is.null(names(alphas.are.additive)) && length(betas)>1)
                    stop(paste0(error.prefix,
                                "'alphas.are.additive' for a functional.form ('",
                                type, "') must be either an unnamed logical scalar or a named logical vector with the same names as 'betas'"))
                
                alphas.are.additive = rep(alphas.are.additive, length(betas))
                names(alphas.are.additive) = alpha.names
            }
            else
            {
                if (length(alphas.are.additive) != length(betas) || 
                    !setequal(names(alphas.are.additive), alpha.names))
                    stop(paste0(error.prefix,
                                "'alphas.are.additive' for a functional.form ('",
                                type, "') must be either an unnamed logical scalar or a named logical vector with the same names as 'betas'"))
                
                alphas.are.additive = alphas.are.additive[alpha.names]
            }
            
            #-- Check alpha.links --#
            if (is(alpha.links, 'link'))
                alpha.links = list(alpha.links)
            
            if (!is.list(alpha.links) || length(alpha.links)==0)
                stop(paste0(error.prefix, "'alpha.links' for a functional.form ('",
                            type, "') must be either a single link object, or a list containing only link objects"))
            
            if (any(!sapply(alpha.links, is, 'link')))
                stop(paste0(error.prefix, "If 'alpha.links' for a functional.form ('",
                            type, "') is a list, it may contain ONLY link objects"))

            if (length(alpha.links)==1)
            {
                if (!is.null(names(alpha.links)) && length(betas)>1)
                    stop(paste0("'alpha.links' for a functional.form ('",
                                type, "') must be either a link object or a named list of link objects with the same names as 'betas'"))
                
                alpha.links = lapply(1:length(betas), function(i){alpha.links[[1]]})
                names(alpha.links) = alpha.names
            }
            else
            {
                if (length(alpha.links) != length(betas) || 
                    !setequal(names(alpha.links), alpha.names))
                    stop(paste0(error.prefix, "'alpha.links' for a functional.form ('",
                                type, "') must be either a link object or a named list of link objects with the same names as 'betas'"))
                
                alpha.links = alpha.links[alpha.names]
            }
            
            #-- Check is.static --#
            if (!is.logical(is.static) || length(is.static)!=1 || is.na(is.static))
                stop(paste0(error.prefix, "'is.static' for a functional.form ('", 
                            type, "') must be a single, non-NA logical value"))
            
            #-- Store and Return --#
            
            private$i.type = type
            private$i.betas = betas
            private$i.minimum.dim.names = dim.names
            private$i.link = link
            private$i.future.slope.link = future.slope.link
            private$i.alphas.are.additive = alphas.are.additive
            private$i.alpha.links = alpha.links
            private$i.is.static = is.static
        },
        
        project = function(years,
                           alphas,
                           dim.names,
                           future.slope = NULL,
                           future.slope.after.year = NULL,
                           future.slope.is.on.transformed.scale=F,
                           check.consistency=T,
                           error.prefix='')
        {
            #-- Check Arguments --#
            if (check.consistency)
            {
                # Check years
                if (length(years)==0 || !is.numeric(years) || any(is.na(years)))
                    stop(paste0(error.prefix, "years must be a non-empty, non-NA numeric vector"))
                if (!is.null(alphas) && length(alphas)>0 && 
                    (!is.list(alphas) || any(!sapply(alphas, is, class2='functional.form.alphas'))))
                    stop(paste0(error.prefix, "alphas must be a list of 'functional.form.alphas' objects"))
                
                # Check dim.names is a valid list
                if (length(dim.names)>0 && 
                    (!is.list(dim.names) || is.null(names(dim.names)) || any(is.na(names(dim.names))) ||
                     any(!sapply(dim.names, is.character)) || any(sapply(dim.names, length)==0) ||
                     any(sapply(dim.names, function(vals){any(is.na(vals))}))))
                    stop(paste0(error.prefix, "dim.names must be either NULL or a named list of non-NA character vectors"))
                    
                # Check that dim.names is sufficient
                if (!are.dim.names.compatible(dim.names.1=dim.names, dim.names.2=private$i.minimum.dim.names,
                                              match.order = F, dim.names.2.is.subset = T))
                    stop(paste0(error.prefix, "The given 'dim.names' are not compatible with the minimum dim.names for the functional form"))
                
                # Check future.slope and future.slope.after.year
                if (!is.null(future.slope))
                {
                    if (!is.numeric(future.slope) || length(future.slope)!=1 || is.na(future.slope))
                        stop("'future.slope' must be a single numeric value")
                    if (!is.numeric(future.slope.after.year) || length(future.slope.after.year)!=1 || is.na(future.slope.after.year))
                        stop("'future.slope.after.year' must be a single numeric value")
                }
                
                # Check alphas
                invalid.alphas = setdiff(names(alphas), self$alpha.names)
                if (length(invalid.alphas)>0)
                    stop(paste0(error.prefix,
                                "Invalid alpha(s) for functional.form of type '", private$i.type, "': ",
                                paste0("'", invalid.alphas, "'", collapse=', ')))
            }
            
            #-- Transform future.slope --#
            if (is.null(future.slope))
            {
                if (!is.null(future.slope) && future.slope != 0)
                    stop(paste0(error.prefix, private$i.type, " functional.form cannot accomodate 'future.slope' - it must be set to NULL or 0"))
                
                future.slope = 0
                future.slope.after.year = Inf
            }
            else
            {
                if (is.null(future.slope))
                {
                    future.slope = 0
                    future.slope.after.year = Inf
                }
                else
                {
                    if (!future.slope.is.on.transformed.scale)
                    {
                        if (check.consistency)
                            private$i.future.slope.link$check.untransformed.values(future.slope,
                                                                                   variable.name.for.error = 'future.slope',
                                                                                   error.prefix = error.prefix)
                        future.slope = private$i.future.slope.link$apply(future.slope)
                    }
                    
                    if (is.null(future.slope.after.year))
                        stop(paste0(error.prefix, "'future.slope.after.year' must be set in order to project from functional.form"))
                }
            }

            #-- Incorporate Alphas --#
            terms = lapply(self$alpha.names, function(name){
                incorporate.alphas(betas=private$i.betas[[name]],
                                   alphas=alphas[[name]],
                                   target.dim.names=dim.names,
                                   error.prefix=error.prefix)
            })
            names(terms) = self$alpha.names
            
            if (check.consistency)
            {
                na.mask = sapply(terms, function(term){
                    any(is.na(term))
                })
                
                if (any(na.mask))
                    stop(paste0(error.prefix,
                                "Generated NA values in ",
                                ifelse(sum(na.mask)==1, 'term ', 'terms '),
                                collapse.with.and("'", names(terms)[na.mask], "'"),
                                " for the ", private$i.type, " functional.form"))
            }
            
            #-- Call the sub-function --#
            private$do.project(terms = terms,
                               years = years,
                               future.slope = future.slope,
                               future.slope.after.year = future.slope.after.year,
                               dim.names = dim.names,
                               check.consistency = check.consistency,
                               error.prefix = error.prefix)
        },
        
        project.static = function(alphas,
                                  dim.names,
                                  check.consistency=T,
                                  error.prefix='')
        {
            if (!private$i.is.static)
                stop(paste0(error.prefix, "Cannot project.static from a dynamic functional.form ('", private$i.type, "')"))
            
            self$project(years = 0,
                         alphas = alphas,
                         future.slope = 0,
                         future.slope.after.year = 0,
                         dim.names = dim.names,
                         check.consistency = check.consistency,
                         error.prefix = error.prefix)[[1]]
        },
        
        apply.ontology.mapping = function(mapping, 
                                           modify.in.place = F,
                                           error.prefix = error.prefix)
        {
            if (!modify.in.place)
            {
                rv = self$clone(deep=T)
                rv$apply.ontology.mapping(mapping, modify.in.place=T)
            }
            else
            {
                private$i.betas = lapply(private$i.betas, mapping$apply)
                private$i.minimum.dim.names = mapping$apply.to.dim.names(private$i.minimum.dim.names)
            }
        },
        
        # FOR DEBUGGING
        check = function()
        {
            browser()
        }
    ),
    
    active = list(
        
        type = function(value)
        {
            if (missing(value))
                private$i.type
            else
                stop("Cannot modify 'type' for a functional.form object - it is read-only")
        },
        
        is.static = function(value)
        {
            if (missing(value))
                private$i.is.static
            else
                stop("Cannot modify 'is.static' for a functional.form object - it is read-only")
        },
        
        minimum.dim.names = function(value)
        {
            if (missing(value))
                private$i.minimum.dim.names
            else
                stop("Cannot modify 'minimum.dim.names' for a functional.form object - they are read-only")
        },
        
        betas = function(value)
        {
            if (missing(value))
                private$i.betas
            else
                stop("Cannot modify 'betas' for a functional.form object - they are read-only")
        },
        
        link = function(value)
        {
            if (missing(value))
                private$i.link
            else
                stop("Cannot modify 'link' for a functional.form object - it is read-only")
        },
        
        alpha.names = function(value)
        {
            if (missing(value))
                names(private$i.betas)
            else
                stop("Cannot modify 'alpha.names' for a functional.form object - they are read-only")
        },
        
        alpha.links = function(value)
        {
            if (missing(value))
                private$i.alpha.links
            else
                stop("Cannot modify 'alpha.links' for a functional.form object - they are read-only")
        },
        
        alphas.are.additive = function(value)
        {
            if (missing(value))
                private$i.alphas.are.additive
            else
                stop("Cannot modify 'alphas.are.additive' for a functional.form object - they are read-only")
        }
    ),
    
    private = list(
        
        i.type = NULL,
        i.is.static = NULL,
        i.link = NULL,
        i.future.slope.link = NULL,
        
        i.minimum.dim.names = NULL,
        i.betas = NULL,
        
        i.alpha.links = NULL,
        i.alphas.are.additive = NULL,
        

        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            stop(paste0(error.prefix,
                        "This subclass of 'functional.form' ('", private$i.type,
                        "') is incompletely specified. The private 'do.project' method must be implemented at the subclass level"))       
        }
    )
)

STATIC.FUNCTIONAL.FORM = R6::R6Class(
    'static.functional.form',
    inherit = FUNCTIONAL.FORM,
    
    public = list(
        
        #'@description Create an Static (Time-Invariant) Functional Form
        #'
        #'@param value array representing the value
        #'@param link The name of the transformation to the scale on which alphas are added to the value. One of "identity", "log", "logistic"
        #'@param min,max The minimum and maximum values this functional form can create. The default (NA) sets to the min/max for the link specified (eg, 0-Inf for a log link, 0-1 for a logistic link)
        #'@param value.is.on.transformed.scale Logical indicating whether the the value is already on the transformed scale at which alphas apply
        #'@param overwrite.parameters.with.alphas Logical indicating whether, when alphas are applied, they OVERWRITE values. If F, alphas are added to the transformed value (on the transformed scale)
        #'
        #'@export
        initialize = function(value,
                              link=c('identity','log','logistic')[1],
                              min = NA,
                              max = NA,
                              value.name='value',
                              value.is.on.transformed.scale = F,
                              overwrite.parameters.with.alphas = F,
                              error.prefix = 'Cannot create static functional.form: ')
        {
            error.prefix = 'Cannot create static functional.form: '
            
            #-- Set up link and alpha.link --#
            link = get.link(link, min=min, max=max, error.prefix=error.prefix)
            if (overwrite.parameters.with.alphas)
                alpha.link = link
            else
                alpha.link = link$get.coefficient.link()
            
            #-- Transform value if needed --#
            if (!value.is.on.transformed.scale)
            {
                link$check.untransformed.values(value, variable.name.for.error='value', error.prefix=error.prefix)
                value = link$apply(value)
            }
            
            #-- Set up names for beta(s) --#
            betas = list(value)
            names(betas) = value.name
            
            super$initialize(type = "static",
                             betas = betas,
                             link = link,
                             future.slope.link = link$get.coefficient.link(),
                             alphas.are.additive = !overwrite.parameters.with.alphas, #either a single logical value or a named logical vector with the same names as betas
                             alpha.links = alpha.link, #either a single value ('identity','log','logit','custom') or named character vector with the same names as betas
                             is.static = T,
                             error.prefix = error.prefix)
        }
        
    ),
    
    private = list(
        
        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            value = private$i.link$reverse.apply(terms$value)

            lapply(years, function(year){
                value
            })       
        }
    )
)

LINEAR.FUNCTIONAL.FORM = R6::R6Class(
    'linear.functional.form',
    inherit = FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(intercept,
                              slope,
                              anchor.year,
                              link=c('identity','log','logistic')[1],
                              min = NA,
                              max = NA,
                              parameters.are.on.transformed.scale = T,
                              overwrite.parameters.with.alphas = F,
                              error.prefix = 'Cannot create linear functional.form: ')
        {
            
            #-- Set up link --#
            link = get.link(link, min=min, max=max, error.prefix=error.prefix)
            
            #-- Check anchor.year --#
            if (length(anchor.year)!=1 || !is.numeric(anchor.year) || is.na(anchor.year))
                stop(paste0(error.prefix, "'anchor.year' must be a single, non-NA numeric value"))
            
            #-- Check overwrite.parameters.with.alphas --#
            if (!is.logical(overwrite.parameters.with.alphas) || length(overwrite.parameters.with.alphas)!=1 || is.na(overwrite.parameters.with.alphas))
                stop(paste0(error.prefix, "'overwrite.parameters.with.alphas' must be a single, non-NA logical value"))
            
            #-- Set up alpha.links --#
            if (overwrite.parameters.with.alphas)
            {
                alpha.links = list(intercept = link,
                                   slope = link$get.coefficient.link())
            }
            else
            {
                alpha.links = list(intercept = link$get.coefficient.link(),
                                   slope = link$get.coefficient.link())
            }
            
            #-- Check Intercept and Slope --#
            if (!parameters.are.on.transformed.scale)
            {
                link$check.untransformed.values(intercept, variable.name.for.error = 'intercept', error.prefix=error.prefix)
                
                intercept = link$apply(intercept)
                slope = alpha.links$slope$apply(slope)
            }
            
            betas = list(intercept = intercept,
                         slope = slope)
            
            #-- Call the superclass constructor --#
            super$initialize(type = "linear",
                             betas = betas,
                             link = link,
                             future.slope.link = link$get.coefficient.link(),
                             alphas.are.additive = !overwrite.parameters.with.alphas,
                             alpha.links = alpha.links, 
                             is.static = F,
                             error.prefix = error.prefix)
            
            private$i.anchor.year = anchor.year
        }
    ),
    
    private = list(
        i.anchor.year = NULL,
        
        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            if (master.debug)
                browser()
            intercept = terms$intercept
            slope = terms$slope
            
            lapply(years, function(year){
                
                x = intercept + 
                    slope * (year - private$i.anchor.year)
                
                if (!is.null(future.slope) && year > future.slope.after.year)
                    x = x + future.slope * (year - future.slope.after.year)
                
                private$i.link$reverse.apply(x)
            })  
        }
    )
)

LOGISTIC.TAIL.FUNCTIONAL.FORM = R6::R6Class(
    'logistic.tail.functional.form',
    inherit = FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(intercept,
                              slope,
                              anchor.year,
                              logistic.after.frac.of.span=0.5,
                              link = 'logit',
                              min=0,
                              max=1,
                              parameters.are.on.transformed.scale = T,
                              overwrite.parameters.with.alphas = F,
                              future.slope.is.multiplier = T,
                              error.prefix = "Cannot create logistic-tail functional form: ")
        {
            #-- Check min/max and get link --#
            if (is.infinite(min))
                stop(paste0(error.prefix, "'min' must be finite"))
            if (is.infinite(max))
                stop(paste0(error.prefix, "'max' must be finite"))
            
            
            #-- Check logistic.after.frac.of.span --#
            if (!is.numeric(logistic.after.frac.of.span))
                stop("'logistic.after.frac.of.span' must be a single numeric value")
            if (length(logistic.after.frac.of.span)!=1)
                stop("'logistic.after.frac.of.span' must be a *SINGLE* numeric value")
            if (is.na(logistic.after.frac.of.span))
                stop("'logistic.after.frac.of.span' cannot be NA")
            if (logistic.after.frac.of.span<0 || logistic.after.frac.of.span>1)
                stop(paste0("'logistic.after.frac.of.span' (", logistic.after.frac.of.span, ") must be between 0 and 1"))
            
            #-- Check anchor.year --#
            if (length(anchor.year)!=1 || !is.numeric(anchor.year) || is.na(anchor.year))
                stop(paste0(error.prefix, "'anchor.year' must be a single, non-NA numeric value"))
            
            #-- Check future.slope.is.multiplier --#
            if (!is.logical(future.slope.is.multiplier) || length(future.slope.is.multiplier)!=1 || is.na(future.slope.is.multiplier))
                stop("'future.slope.is.multiplier' must be a single, non-NA logical value")
            
            #-- Overwrite.parameters.with.alphas --#
            if (!is.logical(overwrite.parameters.with.alphas) || length(overwrite.parameters.with.alphas)!=1 || is.na(overwrite.parameters.with.alphas))
                stop("'overwrite.parameters.with.alphas' must be a single, non-NA logical value")
            
            #-- Betas --#
            link = get.link(link)
            if (!parameters.are.on.transformed.scale)
            {
                link$check.untransformed.values(intercept,
                                                      variable.name.for.error='intercept',
                                                      error.prefix=error.prefix)
                intercept = link$apply(intercept)
                
                link$check.untransformed.values(slope,
                                                      variable.name.for.error='slope',
                                                      error.prefix=error.prefix)
                slope = link$apply(slope)
            }
            betas = list(intercept = intercept, slope = slope)
            
            
            #-- Set up alpha.links --#
            if (overwrite.parameters.with.alphas)
            {
                alpha.links = list(intercept = link,
                                   slope = link$get.coefficient.link())
            }
            else
            {
                alpha.links = list(intercept = link$get.coefficient.link(),
                                   slope = link$get.coefficient.link())
            }
            
            #-- Call the super-class constructor --#
            
            super$initialize(type = "logistic tail",
                             betas = betas,
                             link = get.link('identity', min=min, max=max, error.prefix = error.prefix),
                             future.slope.link = get.link('identity'), # we just take the future slope as-is
                             alphas.are.additive = !overwrite.parameters.with.alphas,
                             alpha.links = alpha.links, 
                             is.static = F,
                             error.prefix = error.prefix)
            
            private$i.span = max - min
            private$i.logistic.after.value = min + private$i.span * logistic.after.frac.of.span
            private$i.anchor.year = anchor.year
            private$i.future.slope.is.multiplier = future.slope.is.multiplier
        }
        
    ),
    
    private = list(
        
        i.span = NULL,
        i.logistic.after.value = NULL,
        i.anchor.year = NULL,
        i.future.slope.is.multiplier = NULL,
        
        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            # Convert from logit scale
            # intercept = model$min.proportion + model$p.span / (1+exp(-terms$intercept)) 
            # slope = model$min.proportion + model$p.span / (1+exp(-terms$slope))
            
            intercept = private$i.alpha.links$intercept$reverse.apply(terms$intercept)
            slope = pmax(0, private$i.alpha.links$slope$reverse.apply(terms$slope))
            
            if (is.null(future.slope))
                slope.with.future = slope
            else if (private$i.future.slope.is.multiplier)
                slope.with.future = slope * future.slope
            else
                slope.with.future = slope + future.slope
            
            #-- Fold in additional.slope.after.year --#
            logistic.slope.sans.additional = slope * private$i.span / (private$i.logistic.after.value - private$i.link$min) /
                (private$i.link$max - private$i.logistic.after.value)
            
            logistic.slope.with.additional = slope.with.future * private$i.span / 
                (private$i.logistic.after.value - private$i.link$min) /
                (private$i.link$max - private$i.logistic.after.value)
            
            
            val.at.additional.year = intercept + slope * (future.slope.after.year - private$i.anchor.year)
            logistic.after.year = private$i.anchor.year + (private$i.logistic.after.value - intercept) / slope
            mask = val.at.additional.year < private$i.logistic.after.value
            logistic.after.year.after.additional = future.slope.after.year + 
                (private$i.logistic.after.value - val.at.additional.year) / slope.with.future
            logistic.after.year[mask] = logistic.after.year.after.additional[mask]
            
            logistic.intercept = log(private$i.logistic.after.value - private$i.link$min) - 
                log(private$i.link$max - private$i.logistic.after.value) -
                logistic.slope.sans.additional * (pmin(logistic.after.year, future.slope.after.year) - private$i.anchor.year) -
                logistic.slope.with.additional * pmax(0, logistic.after.year - future.slope.after.year)
            
            rv = lapply(years, function(year){
                # Calculate the RV
                # sub in anything past the use logistic tail threshold
                sub.rv = intercept + 
                    slope * min(year - private$i.anchor.year, future.slope.after.year - private$i.anchor.year) +
                    slope.with.future * max(0, year - future.slope.after.year)
                
                mask = sub.rv > private$i.logistic.after.value
                
                # The value based off logistic model
                log.ors = logistic.intercept[mask] +
                    logistic.slope.sans.additional[mask] * (pmin(year, future.slope.after.year)-private$i.anchor.year) +
                    logistic.slope.with.additional[mask] * pmax(0, year-future.slope.after.year)
                
                sub.rv[mask] = private$i.link$min + private$i.span / (1 + exp(-log.ors))
                
                sub.rv[sub.rv<private$i.link$min] = private$i.link$min
                sub.rv
            })
            
            rv
        }
    )
)

SPLINE.FUNCTIONAL.FORM = R6::R6Class(
    'spline.functional.form',
    inherit = FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(type,
                              apply.spline.to.list,
                              can.accomodate.future.slope,
                              
                              knot.times,
                              knot.values,
                              link = c('identity','log','logit')[1],
                              min = NA,
                              max = NA,
                              
                              knots.are.on.transformed.scale=F,
                              overwrite.knot.values.with.alphas=F,
                              
                              before.time=NULL,
                              before.modifier=NULL,
                              before.modifier.application=NULL,
                              overwrite.before.modifier.with.alphas=F,
                              
                              after.time=NULL,
                              after.modifier=NULL,
                              after.modifier.application=NULL,
                              overwrite.after.modifier.with.alphas=F,
                              
                              additional.betas = NULL,
                              additional.alpha.links = NULL,
                              additional.overwrite.alphas=NULL,
                              
                              error.prefix='')
        {
            #-- Set up link (and check min, max) --#
            link = get.link(link, min=min, max=max)
            
            #-- Check knot.times --#
            if (!is.numeric(knot.times))
                stop(paste0(error.prefix, "'knot.times' must be a numeric vector"))
            if (length(knot.times)==0)
                stop(paste0(error.prefix, "'knot.times' must contain at least one value"))
            if (any(is.na(knot.times)))
                stop(paste0(error.prefix, "'knot.times' cannot contain NA values"))
            tabled.knot.times = table(knot.times)
            if (max(tabled.knot.times)>1)
                stop(paste0(error.prefix, "'knot.times' must be unique. ",
                            collapse.with.and(names(tabled.knot.times)[tabled.knot.times>1]),
                            ifelse(sum(tabled.knot.times>1)==1, " is", " are"),
                            " repeated"))

            
            n.knots = length(knot.times)
            
            #-- Check knot.values --#
            if (!is.list(knot.values))
            {
                if (n.knots==1)
                    knot.values = list(knot.values)
                else
                    stop(paste0(error.prefix, "If there is more than one knot, 'knot.values' must be a list"))
            }
            
            if (length(knot.values) != n.knots)
                stop(paste0("'knot.values' (length ", length(knot.values),
                            ") must have the same length as knot.times (", n.knots, ")"))
            
            #-- Check knot names --#
            knot.names = names(knot.times)
            if (is.null(knot.names))
                stop(paste0(error.prefix, "'knot.times' must have names set"))
            if (any(is.na(knot.names)))
                stop(paste0(error.prefix, "The names of 'knot.times' cannot be NA"))
            if (any(nchar(knot.names)==1))
                stop(paste0(error.prefix, "The names of 'knot.times' cannot be empty ('')"))
            tabled.knot.names = table(knot.names)
            if (max(tabled.knot.names)>1)
                stop(paste0(error.prefix, "The names of 'knot.times' must be unique. ",
                            collapse.with.and("'", names(tabled.knot.names)[tabled.knot.names>1], "'"),
                            ifelse(sum(tabled.knot.names>1)==1, " is", " are"),
                            " repeated"))
            
            disallowed.knot.names = c('before.modifier', 'after.modifier')
            invalid.names = intersect(knot.names, disallowed.knot.names)
            if (length(invalid.names)>0)
                stop(paste0(error.prefix, 
                            collapse.with.and("'", invalid.names, "'"),
                            ifelse(length(invalid.names)==1, " is", " are"),
                            " not allowed as names of knot.times"))
            
            if (is.null(names(knot.values)))
                stop(paste0(error.prefix, "'knot.values' must have names set"))
            if (any(is.na(names(knot.values))))
                stop(paste0(error.prefix, "The names of 'knot.values' cannot be NA"))
            
            if (!setequal(knot.names, names(knot.values)))
                stop(paste0(error.prefix, "'knot.values' and 'knot.times' must have the same names"))
            
            knot.values = knot.values[knot.names]
            
            #-- Transform the knot values --#
            if (!is.logical(knots.are.on.transformed.scale) || length(knots.are.on.transformed.scale)!=1 ||
                is.na(knots.are.on.transformed.scale))
                stop(paste0(error.prefix, "'knots.are.on.transformed.scale' must be a single, non-NA logical value"))
            if (!knots.are.on.transformed.scale)
            { 
                knot.values = lapply(knot.names, function(knot.name){
                    link$check.untransformed.values(knot.values[[knot.name]],
                                                              variable.name.for.error = paste0("values for knot '", knot, "'"),
                                                              error.prefix = error.prefix)
                    
                    link$apply(knot.values[[knot.name]])
                })
                names(knot.values) = knot.names
            }
            
            #-- Order knots by time --#
            
            o = order(knot.times)
            knot.times = knot.times[o]
            knot.values = knot.values[o]
            knot.names = knot.names[o]
            
            #-- Check overwrite.knot.values.with.alphas --#
            if (!is.logical(overwrite.knot.values.with.alphas) || length(overwrite.knot.values.with.alphas)!=1 || 
                is.na(overwrite.knot.values.with.alphas))
                stop("'overwrite.knot.values.with.alphas' must be a single, non-NA logical value")
            
            #-- Set up Betas from Knots --#
            betas = knot.values
            
            alphas.are.additive = rep(!overwrite.knot.values.with.alphas, n.knots)
            if (overwrite.knot.values.with.alphas)
                alpha.links = lapply(1:n.knots, function(i){link})
            else
                alpha.links = lapply(1:n.knots, function(i){link$get.coefficient.link()})
            
            names(alphas.are.additive) = names(alpha.links) = names(betas) = knot.names
            
            #-- Check Before Modifier/Application/Time --#
            
            VALID.MODIFIER.APPLICATIONS = c('multiplicative.on.link.scale',
                                            'additive.on.link.scale',
                                            'multiplicative.of.change.on.value.scale')
            
            
            if (is.null(before.modifier))
            {
                if (!is.null(before.time))
                    stop(paste0(error.prefix, "If 'before.time' is non-NULL, 'before.modifier' must be non-NULL as well"))
                if (!is.null(before.modifier.application))
                    stop(paste0(error.prefix, "'before.modifier.application' can only be set (non-NULL) if 'before.modifier' is set"))
            }
            else
            {
                # Check before time
                if (is.null(before.time))
                    stop(paste0(error.prefix, "If 'before.modifier' is non-NULL, 'before.time' must be non-NULL as well"))
                
                if (!is.numeric(before.time) || length(before.time) != 1 || is.na(before.time))
                    stop(paste0(error.prefix, "'before.time' must be a single, non-NA, numeric value"))
                if (before.time >= knot.times[1])
                    stop(paste0(error.prefix, "'before.time' must PRECEDE all the knot.times"))
                
                # Check the modifier value
                if (!is.numeric(before.modifier) || length(before.modifier)==0)
                    stop(paste0(error.prefix, "'before.modifier' must be a non-empty numeric object"))
                
                # overwrite.before.modifier.with.alphas
                if (!is.logical(overwrite.before.modifier.with.alphas) || length(overwrite.before.modifier.with.alphas)!=1 || is.na(overwrite.before.modifier.with.alphas))
                    stop(paste0(error.prefix, "'overwrite.before.modifier.with.alphas' must be a single, non-NA logical value"))
                
                # Check the modifier application
                if (is.null(before.modifier.application))
                    stop(paste0(error.prefix, "If 'before.modifier' is set (ie, not NULL), you must specify 'before.modifier.application"))
                
                if (!is.character(before.modifier.application) || length(before.modifier.application)!=1 || is.na(before.modifier.application))
                    stop(paste0(error.prefix, "'before.modifier.application' must be a single, non-NA character value"))
                if (all(VALID.MODIFIER.APPLICATIONS != before.modifier.application))
                    stop(paste0(error.prefix,
                                "Invalid before.modifier.application '", before.modifier.application,
                                "' - must be one of ",
                                paste0("'", VALID.MODIFIER.APPLICATIONS, "'", collapse=', ')))
                
                if (before.modifier.application == 'multiplicative.of.change.on.value.scale' && n.knots==1)
                    stop(paste0(error.prefix, "In order to use before.modifier.application='multiplicative.of.change.on.value.scale', there must be at least two knots"))
                
                if (before.modifier.application == 'multiplicative.of.change.on.value.scale')
                    modifier.link = get.link('log')
                else if (before.modifier.application == 'additive.on.link.scale')
                    modifier.link = link$get.coefficient.link()
                else
                    modifier.link = get.link('log')
                
                # Apply link
                modifier.link$check.untransformed.values(before.modifier,
                                                         variable.name.for.error = 'before.modifier',
                                                         error.prefix = error.prefix)
                
                # Put it in the vectors
                betas = c(list(modifier.link$apply(before.modifier)), betas)
                alphas.are.additive = c(!overwrite.before.modifier.with.alphas, alphas.are.additive)
                alpha.links = c(list(modifier.link), alpha.links)
                names(betas)[1] = names(alphas.are.additive)[1] = names(alpha.links)[1] = 'before.modifier'
            }
            
            
            #-- Check After Modifier/Application/Time --#
            
            if (is.null(after.modifier))
            {
                if (!is.null(after.time))
                    stop(paste0(error.prefix, "If 'after.time' is non-NULL, 'after.modifier' must be non-NULL as well"))
                if (!is.null(after.modifier.application))
                    stop(paste0(error.prefix, "'after.modifier.application' can only be set (non-NULL) if 'after.modifier' is set"))
            }
            else
            {
                # Check after time
                if (is.null(after.time))
                    stop(paste0(error.prefix, "If 'after.modifier' is non-NULL, 'after.time' must be non-NULL as well"))
                
                if (!is.numeric(after.time) || length(after.time) != 1 || is.na(after.time))
                    stop(paste0(error.prefix, "'after.time' must be a single, non-NA, numeric value"))
                if (after.time <= knot.times[n.knots])
                    stop("'after.time' must be AFTER all the knot.times")
                
                # Check the modifier value
                if (!is.numeric(after.modifier) || length(after.modifier)==0)
                    stop(paste0(error.prefix, "'after.modifier' must be a non-empty numeric object"))
                
                # overwrite.after.modifier.with.alphas
                if (!is.logical(overwrite.after.modifier.with.alphas) || length(overwrite.after.modifier.with.alphas)!=1 || is.na(overwrite.after.modifier.with.alphas))
                    stop(paste0(error.prefix, "'overwrite.after.modifier.with.alphas' must be a single, non-NA logical value"))
                
                # Check the modifier application
                if (is.null(after.modifier.application))
                    stop(paste0(error.prefix, "If 'after.modifier' is set (ie, not NULL), you must specify 'after.modifier.application"))
                
                if (!is.character(after.modifier.application) || length(after.modifier.application)!=1 || is.na(after.modifier.application))
                    stop(paste0(error.prefix, "'after.modifier.application' must be a single, non-NA character value"))
                if (all(VALID.MODIFIER.APPLICATIONS != after.modifier.application))
                    stop(paste0(error.prefix,
                                "Invalid after.modifier.application '", after.modifier.application,
                                "' - must be one of ",
                                paste0("'", VALID.MODIFIER.APPLICATIONS, "'", collapse=', ')))
                
                if (after.modifier.application == 'multiplicative.of.change.on.value.scale' && n.knots==1)
                    stop(paste0(error.prefix, "In order to use after.modifier.application='multiplicative.of.change.on.value.scale', there must be at least two knots"))
                
                if (after.modifier.application == 'multiplicative.of.change.on.value.scale')
                    modifier.link = get.link('log')
                else if (after.modifier.application == 'additive.on.link.scale')
                    modifier.link = link$get.coefficient.link()
                else
                    modifier.link = get.link('log')
                
                # Apply link
                modifier.link$check.untransformed.values(after.modifier,
                                                         variable.name.for.error = 'after.modifier',
                                                         error.prefix = error.prefix)
                
                # Put it in the vectors
                betas = c(betas, list(modifier.link$apply(after.modifier)))
                alphas.are.additive = c(alphas.are.additive, !overwrite.after.modifier.with.alphas)
                alpha.links = c(alpha.links, list(modifier.link))
                names(betas)[length(betas)] = names(alphas.are.additive)[length(betas)] = names(alpha.links)[length(betas)] = 'after.modifier'
            }
            
            #-- Check apply.spline.to.list --#
            if (!is.logical(apply.spline.to.list) || length(apply.spline.to.list)!=1 || is.na(apply.spline.to.list))
                stop(paste0(error.prefix, "'apply.spline.to.list' must be a single, non-NA logical value"))
            
            
            #-- Set up future slope link --#
            if (can.accomodate.future.slope)
                future.slope.link = link$get.coefficient.link()
            else
                future.slope.link = NULL

            #-- Fold in additional betas/alphas --#
            if (length(additional.betas) != length(additional.alpha.links))
                stop(paste0(error.prefix, "'additional.alpha.links' must have the same names as 'additional.betas'"))
            
            if (length(additional.betas) != length(additional.overwrite.alphas))
                stop(paste0(error.prefix, "'additional.overwrite.alphas' must have the same names as 'additional.betas'"))
            
            if (length(additional.betas)>0)
            {
                if (is.null(names(additional.betas)))
                    stop(paste0(error.prefix, "'additional.betas' must have names set"))
                if (is.null(names(additional.alpha.links)))
                    stop(paste0(error.prefix, "'additional.alpha.links' must have names set"))
                if (is.null(names(additional.overwrite.alphas)))
                    stop(paste0(error.prefix, "'additional.overwrite.alphas' must have names set"))
                
                if (!setequal(names(additional.betas), names(additional.alpha.links)))
                    stop(paste0(error.prefix, "'additional.alpha.links' must have the same names as 'additional.betas'"))
                if (!setequal(names(additional.betas), names(additional.overwrite.alphas)))
                    stop(paste0(error.prefix, "'additional.overwrite.alphas' must have the same names as 'additional.betas'"))
            }
            
            
            #-- Call the super-class constructor --#
            betas = c(betas, additional.betas)
            alpha.links = c(alpha.links, additional.alpha.links)
            if (!is.null(additional.overwrite.alphas))
                alphas.are.additive = c(alphas.are.additive, !additional.overwrite.alphas)
            super$initialize(type = type,
                             betas = betas,
                             link = link,
                             future.slope.link = future.slope.link,
                             alphas.are.additive = alphas.are.additive, 
                             alpha.links = alpha.links, 
                             is.static = F,
                             error.prefix = error.prefix)
            
            #-- Store the extra member variables --#
            private$i.knot.times = knot.times
            private$i.knot.names = knot.names

            private$i.before.time = before.time
            private$i.before.modifier.application = before.modifier.application
            
            private$i.after.time = after.time
            private$i.after.modifier.application = after.modifier.application
            
            private$i.apply.spline.to.list = apply.spline.to.list
        }
        
    ),
    
    active = list(
        
        knot.times = function(value)
        {
            if (missing(value))
            {
                c(private$i.before.time,
                  private$i.knot.times,
                  private$i.after.time)
            }
            else
                stop("Cannot modify 'knot.times' for a functional.form object - it is read-only")
        },
        
        n.knots = function(value)
        {
            if (missing(value))
                length(self$knot.times)
            else
                stop("Cannot modify 'n.knots' for a functional.form object - it is read-only")
        }
    ),
    
    private = list(
        
        i.knot.times = NULL,
        i.knot.names = NULL,
        
        i.before.time = NULL,
        i.before.modifier.application = NULL,
        
        i.after.time = NULL,
        i.after.modifier.application = NULL,
        
        i.apply.spline.to.list = NULL,
        
        apply.spline = function(knot.values,
                                knot.times,
                                desired.times)
        {
            stop(paste0(error.prefix,
                        "This subclass of 'functional.form' ('", private$i.type,
                        "') is incompletely specified. The private 'apply.spline' method must be implemented at the subclass level"))       
        },
        
        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            #-- Pull out knot values --#
            knot.values = terms[private$i.knot.names]
            knot.times = self$knot.times
            n.knots = length(knot.times)
            
            
            #-- Set up before multiplier (if present) --#
            if (!is.null(private$i.before.time))
            {
                if (private$i.before.modifier.application == 'multiplicative.on.model.scale')
                    before.knot.value = knot.values[[1]] * private$i.alpha.links$before.modifier$reverse.apply(terms$before.modifier)
                else if (private$i.before.modifier.application == 'additive.on.model.scale')
                    before.knot.value = knot.values[[1]] + terms$before.modifier
                else if (private$i.before.modifier.application == 'multiplicative.of.change.on.value.scale')
                {
                    before.modifier = private$i.alpha.links$before.modifier$reverse.apply(terms$before.modifier)
                    untransformed.knot.value.1 = private$i.link$reverse.apply(knot.values[[1]])
                    change = private$i.link$reverse.apply(knot.values[[2]]) - untransformed.knot.value.1
                    untransformed.before.knot.value = untransformed.knot.value.1 - change * before.modifier
                    if (check.consistency)
                        private$i.link$check.untransformed.values(untransformed.before.knot.value,
                                                                  variable.name.for.error = "untransformed 'before' knot values",
                                                                  error.prefix = error.prefix)
                    before.knot.value = private$i.link$apply(untransformed.before.knot.value)
                }
                else
                    stop(paste0("Invalid before.modifier.application '", private$i.before.modifier.application, "'"))
                
                knot.values = c(list(before.knot.value), knot.values)
            }
            
            
            #-- Set up after multiplier (if present) --#
            if (!is.null(private$i.after.time))
            {
                if (private$i.after.modifier.application == 'multiplicative.on.link.scale')
                    after.knot.value = knot.values[[length(knot.values)]] * private$i.alpha.links$after.modifier$reverse.apply(terms$after.modifier)
                else if (private$i.after.modifier.application == 'additive.on.link.scale')
                    after.knot.value = knot.values[[length(knot.values)]] + terms$after.modifier
                else if (private$i.after.modifier.application == 'multiplicative.of.change.on.value.scale')
                {
                    after.modifier = private$i.alpha.links$after.modifier$reverse.apply(terms$after.modifier)
                    untransformed.last.knot = private$i.link$reverse.apply(knot.values[[length(knot.values)]])
                    change = untransformed.last.knot - private$i.link$reverse.apply(knot.values[[length(knot.values)-1]])
                    untransformed.after.knot.value = untransformed.last.knot + change * after.modifier
                    if (check.consistency)
                        private$i.link$check.untransformed.values(untransformed.after.knot.value,
                                                                  variable.name.for.error = "untransformed 'after' knot values",
                                                                  error.prefix = error.prefix)
                    after.knot.value = private$i.link$apply(untransformed.after.knot.value)
                }
                else
                    stop(paste0("Invalid after.modifier.application '", private$i.after.modifier.application, "'"))
                
                knot.values = c(knot.values, list(after.knot.value))
            }
            
            
            #-- Fit the spline for each value in the array and project --#
            n = length(knot.values[[1]])
            
            future.slope.to.add = future.slope * pmax(0, years-future.slope.after.year)
            
            if (private$i.apply.spline.to.list)
            {
                arr.rv = sapply(1:n, function(i){
                    knot.values.for.i = sapply(knot.values, function(v){v[i]})
                    private$apply.spline(knot.values = knot.values.for.i,
                                         knot.times = knot.times,
                                         desired.times = years)
                })
                
                lapply(1:length(years), function(y){
                    sub.rv = private$i.link$reverse.apply(arr.rv[y,] + 
                                                                  future.slope * max(0, years[y]-future.slope.after.year))
                    
                    
                    if (length(dim.names)>0)
                    {
                        dim(sub.rv) = sapply(dim.names, length)
                        dimnames(sub.rv) = dim.names
                    }
                    else
                        names(sub.rv) = NULL
                    
                    sub.rv
                })
            }
            else
            {
                rv = private$apply.spline(knot.values = knot.values,
                                          knot.times = knot.times,
                                          desired.times = years)
                
                lapply(1:length(years), function(y){
                    private$i.link$reverse.transform(rv[[y]] + 
                                                         future.slope * max(0, years[y]-future.slope.after.year))
                })
            }
        }
    )
)

LINEAR.SPLINE.FUNCTIONAL.FORM = R6::R6Class(
    'linear.spline.functional.form',
    inherit = SPLINE.FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(knot.times,
                              knot.values,
                              link = c('identity','log','logit')[1],
                              min = NA,
                              max = NA,
                              
                              knots.are.on.transformed.scale=F,
                              overwrite.knot.values.with.alphas=F,
                              
                              before.time=NULL,
                              before.modifier=NULL,
                              before.modifier.application=NULL,
                              overwrite.before.modifier.with.alphas=F,
                              
                              after.time=NULL,
                              after.modifier=NULL,
                              after.modifier.application=NULL,
                              overwrite.after.modifier.with.alphas=F,
                              
                              error.prefix='')
        {
            super$initialize(type = 'linear spline',
                             apply.spline.to.list = T,
                             can.accomodate.future.slope = T,
                             
                             knot.times = knot.times,
                             knot.values = knot.values,
                             link = link,
                             min = min,
                             max = max,
                             
                             knots.are.on.transformed.scale = knots.are.on.transformed.scale,
                             overwrite.knot.values.with.alphas = overwrite.knot.values.with.alphas,
                             
                             before.time = before.time,
                             before.modifier = before.modifier,
                             before.modifier.application = before.modifier.application,
                             overwrite.before.modifier.with.alphas = overwrite.before.modifier.with.alphas,
                             
                             after.time = after.time,
                             after.modifier = after.modifier,
                             after.modifier.application = after.modifier.application,
                             overwrite.after.modifier.with.alphas = overwrite.after.modifier.with.alphas,
                             
                             error.prefix = error.prefix)
        }
        
    ),
    
    private = list(
        
        apply.spline = function(knot.values,
                                knot.times,
                                desired.times)
        {
            interpolate(values = knot.values,
                        value.times = knot.times,
                        desired.times = desired.times)
        }
    )
)

NATURAL.SPLINE.FUNCTIONAL.FORM = R6::R6Class(
    'natural.spline.functional.form',
    inherit = SPLINE.FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(knot.times,
                              knot.values,
                              link = c('identity','log','logit')[1],
                              min = NA,
                              max = NA,
                              
                              knots.are.on.transformed.scale=F,
                              overwrite.knot.values.with.alphas=F,
                              
                              before.time=NULL,
                              before.modifier=NULL,
                              before.modifier.application=NULL,
                              overwrite.before.modifier.with.alphas=F,
                              
                              after.time=NULL,
                              after.modifier=NULL,
                              after.modifier.application=NULL,
                              overwrite.after.modifier.with.alphas=F,
                              
                              error.prefix='')
        {
            super$initialize(type = 'natural spline',
                             apply.spline.to.list = T,
                             can.accomodate.future.slope = T,
                             
                             knot.times = knot.times,
                             knot.values = knot.values,
                             link = link,
                             min = min,
                             max = max,
                             
                             knots.are.on.transformed.scale = knots.are.on.transformed.scale,
                             overwrite.knot.values.with.alphas = overwrite.knot.values.with.alphas,
                             
                             before.time = before.time,
                             before.modifier = before.modifier,
                             before.modifier.application = before.modifier.application,
                             overwrite.before.modifier.with.alphas = overwrite.before.modifier.with.alphas,
                             
                             after.time = after.time,
                             after.modifier = after.modifier,
                             after.modifier.application = after.modifier.application,
                             overwrite.after.modifier.with.alphas = overwrite.after.modifier.with.alphas,
                             
                             error.prefix = error.prefix)
        }
        
    ),
    
    private = list(
        
        apply.spline = function(knot.values,
                                knot.times,
                                desired.times)
        {
            stats::spline(x = knot.times, 
                          y = knot.values, 
                          xout = desired.times,
                          method='natural')$y
        }
    )
)

LOGISTIC.SPLINE.FUNCTIONAL.FORM = R6::R6Class(
    'logistic.spline.form',
    inherit = SPLINE.FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(knot.times,
                              knot.values,
                              link = 'log',
                              overwrite.knot.values.with.alphas = F,
                              
                              fraction.of.asymptote.after.end=0.05,
                              fraction.of.asymptote.before.start=0.025,
                              fraction.of.asymptote.for.change.dir=0.02,
                              fraction.asymptote.link = 'log',
                              overwrite.fraction.asymptote.with.alphas = F,
                              
                              error.prefix = '')
        {
            #-- Set up betas for asymptotes --#
            fraction.asymptotes = list(
                fraction.of.asymptote.before.start = fraction.of.asymptote.before.start,
                fraction.of.asymptote.for.change.dir = fraction.of.asymptote.for.change.dir,
                fraction.of.asymptote.after.end = fraction.of.asymptote.after.end
            )[c(T,length(knot.values)>2,T)]
            
            
            #-- Check overwrite.fraction.asymptote.with.alphas --#
            if (!is.logical(overwrite.fraction.asymptote.with.alphas) || length(overwrite.fraction.asymptote.with.alphas)!=1 || is.na(overwrite.fraction.asymptote.with.alphas))
                stop("'overwrite.fraction.asymptote.with.alphas' must be a single, non-NA logical value")
            
            overwrite.fraction.asymptote.with.alphas = rep(overwrite.fraction.asymptote.with.alphas, 
                                                           length(fraction.asymptotes))
            names(overwrite.fraction.asymptote.with.alphas) = names(fraction.asymptotes)


            #-- fraction.asymptote.link --#
            fraction.asymptote.links = lapply(1:length(fraction.asymptotes), function(i){
                get.link(fraction.asymptote.link, min=0, max=1,
                         variable.name.for.error = 'fraction.asymptote.link',
                         error.prefix = error.prefix)
            })
            names(fraction.asymptote.links) = names(fraction.asymptotes)
            
            #-- Apply link scale to fraction.asymptotes --#
            fraction.asymptotes = lapply(names(fraction.asymptote.links), function(name){
                fraction.asymptote.links[[name]]$check.untransformed.values(fraction.asymptotes[[name]],
                                                                            variable.name.for.error = name,
                                                                            error.prefix = error.prefix)
                
                fraction.asymptote.links[[name]]$apply(fraction.asymptotes[[name]])
            })
            names(fraction.asymptotes) = names(fraction.asymptote.links)
            
            #-- Call the super-class (spline) constructor --#
            
            super$initialize(type = 'logistic spline',
                             apply.spline.to.list = F,
                             can.accomodate.future.slope = F,
                             
                             knot.times = knot.times,
                             knot.values = knot.values,
                             link = link,
                             min = NA,
                             max = NA,
                             
                             knots.are.on.transformed.scale = F,
                             overwrite.knot.values.with.alphas = overwrite.knot.values.with.alphas,
                             
                             additional.betas = fraction.asymptotes,
                             additional.alpha.links = fraction.asymptote.links,
                             additional.overwrite.alphas = overwrite.fraction.asymptote.with.alphas,
                             
                             before.time = NULL,
                             before.modifier = NULL,
                             before.modifier.application = NULL,
                             overwrite.before.modifier.with.alphas = NULL,
                             
                             after.time = NULL,
                             after.modifier = NULL,
                             after.modifier.application = NULL,
                             overwrite.after.modifier.with.alphas = NULL,
                             
                             error.prefix = error.prefix)
          
            #-- Check n.knots >2 --#
            if (self$n.knots<2)
                stop(paste0(error.prefix, "A logistic spline must have at least two knots"))
            
            #-- Check fraction.of.asymptote.for.change.dir --#
            if (self$n.knots > 2)
            {
                if (any(fraction.of.asymptote.for.change.dir<0) || any(fraction.of.asymptote.for.change.dir>1))
                    stop(paste0(error.prefix, 
                                "'fraction.of.asymptote.for.change.dir' must have values between 0 and 1"))
            }
            
            #-- Check fraction.of.asymptote.before.start and fraction.of.asymptote.after.end --#
            
            if (any(fraction.of.asymptote.before.start<0) || any(fraction.of.asymptote.before.start>1))
                stop("'fraction.of.asymptote.before.start' must have values between 0 and 1")
            
            if (any(fraction.of.asymptote.after.end<0) || any(fraction.of.asymptote.after.end>1))
                stop("'fraction.of.asymptote.after.end' must have values between 0 and 1")

            #-- One last check (that adjacent fractions don't sum to more than 1)
            fraction.asymptote.before.start = private$i.alpha.links$fraction.of.asymptote.before.start$reverse.apply(private$i.betas$fraction.of.asymptote.before.start)
            
            fraction.of.asymptote.after.end = private$i.alpha.links$fraction.of.asymptote.after.end$reverse.apply(private$i.betas$fraction.of.asymptote.after.end)
            if (self$n.knots>2)
            {
                fraction.of.asymptote.for.change.dir = private$i.alpha.links$fraction.of.asymptote.for.change.dir$reverse.apply(private$i.betas$fraction.of.asymptote.for.change.dir)
                
                if (any((fraction.of.asymptote.before.start + 
                         fraction.of.asymptote.for.change.dir)>=1))
                    stop(paste0(error.prefix,
                                "With >2 knots, the sum of 'fraction.of.asymptote.before.start' + 'fraction.of.asymptote.for.change.dir' must always be less than 1"))
                
                if (any((fraction.of.asymptote.for.change.dir + 
                        fraction.of.asymptote.after.end)>=1))
                    stop(paste0(error.prefix,
                                "With >2 knots, the sum of 'fraction.of.asymptote.for.change.dir' + 'fraction.of.asymptote.after.end' must always be less than 1"))
                
                if (self$n.knots>3)
                { 
                    if (any(fraction.of.asymptote.for.change.dir>=0.5))
                        stop(paste0(error.prefix, "With >3 knots, 'fraction.of.asymptote.for.change.dir' must always be less than 0.5"))
                }
            }
            else
            {
                if (any((fraction.of.asymptote.before.start + 
                         fraction.of.asymptote.after.end)>1))
                    stop(paste0(error.prefix,
                                "With two knots, the sum of fraction.of.asymptote.before.start + fraction.of.asymptote.after.end must always be less than 1"))
            }
        }
        
    ),
    
    private = list(
        
        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            #-- Apply the link (reverse) transformation --#
            for (beta.name in names(terms))
                terms[[beta.name]] = private$i.alpha.links[[beta.name]]$reverse.apply(terms[[beta.name]])
            
            #-- Add Alphas to Knots --#
            knot.values = terms[private$i.knot.names]

            #-- Set fraction asymptote --#
            fraction.of.asymptote.before.start = terms$fraction.of.asymptote.before.start
            fraction.of.asymptote.after.end = terms$fraction.of.asymptote.after.end
            fraction.of.asymptote.for.change.dir = terms$fraction.of.asymptote.for.change.dir
            
            # check asymptotes
            if (length(knot.values)>2)
            {
                if (any((fraction.of.asymptote.before.start + fraction.of.asymptote.for.change.dir)>=1))
                    stop(paste0(error.prefix, "fraction.of.asymptote.before.start and fraction.of.asymptote.for.change.dir cannot sum to one or greater"))
                if (any((fraction.of.asymptote.after.end + fraction.of.asymptote.for.change.dir)>=1))
                    stop(paste0(error.prefix, "fraction.of.asymptote.after.end and fraction.of.asymptote.for.change.dir cannot sum to one or greater"))
                
                if (length(knot.values)>3)
                {
                    if (any(fraction.of.asymptote.for.change.dir>=0.5))
                        stop(paste0(error.prefix, "With >3 knots, fraction.of.asymptote.for.change.dir cannot be 0.5 or greater"))
                }
            }
            else
            {
                if (any((fraction.of.asymptote.before.start + fraction.of.asymptote.after.end)>1))
                    stop(paste0(error.prefix, "fraction.of.asymptote.before.start and fraction.of.asymptote.after.end cannot sum to more than one"))
            }
            
            #-- Fit the spline for each value in the array and project --#
            
            n = length(knot.values[[1]])
            if (length(knot.values)==2)
            {
                arr.rv = sapply(1:n, function(i){
                    knot.values.for.i = sapply(knot.values, function(v){v[i]})
                    
                    calculate.change.ratios.logistic(r0 = knot.values.for.i[1],
                                                     r1 = knot.values.for.i[2],
                                                     times = years,
                                                     t0 = private$i.knot.times[1],
                                                     t1 = private$i.knot.times[2],
                                                     fraction.of.asymptote.after.end = fraction.of.asymptote.after.end[i],
                                                     fraction.of.asymptote.before.start = fraction.of.asymptote.before.start[i])
                })
            }
            else if (length(knot.values)==3)
            {
                arr.rv = sapply(1:n, function(i){
                    knot.values.for.i = sapply(knot.values, function(v){v[i]})
                    
                    calculate.change.ratios.two.logistic(r0 = knot.values.for.i[1],
                                                         r1 = knot.values.for.i[2],
                                                         r2 = knot.values.for.i[3],
                                                         times = years,
                                                         t0 = private$i.knot.times[1],
                                                         t1 = private$i.knot.times[2],
                                                         t2 = private$i.knot.times[3],
                                                         
                                                         fraction.of.asymptote.after.end = fraction.of.asymptote.after.end[i],
                                                         fraction.of.asymptote.before.start = fraction.of.asymptote.before.start[i],
                                                         fraction.of.asymptote.for.change.dir = fraction.of.asymptote.for.change.dir[i])
                })
            }
            else
            {
                n.chunks = length(knot.values)-1
                years.for.chunk = lapply(1:n.chunks, function(chunk){
                    if (chunk==1)
                        years[years<=knot.times[2]]
                    else if (chunk==n.chunks)
                        years[years>knot.times[n.chunks]]
                    else
                        years[years>knot.times[chunk] & years<=knot.times[chunk+1]]
                })
                
                start.asymptote.name = rep('fraction.of.asymptote.for.change.dir', n.chunks)
                start.asymptote.name[1] = 'fraction.of.asymptote.before.start'
                
                end.asymptote.name = rep('fraction.of.asymptote.for.change.dir', n.chunks)
                end.asymptote.name[n.chunks] = 'fraction.of.asymptote.after.end'
                
                chunks.to.do = (1:n.chunks)[sapply(years.for.chunk, length)>0]
                
                arr.rv = sapply(1:n, function(i){
                    knot.values.for.i = sapply(knot.values, function(v){v[i]})
                    
                    unlist(sapply(chunks.to.do, function(chunk){
                        calculate.change.ratios.logistic(r0 = knot.values.for.i[chunk],
                                                         r1 = knot.values.for.i[chunk+1],
                                                         times = years.for.chunk[[chunk]],
                                                         t0 = private$i.knot.times[chunk],
                                                         t1 = private$i.knot.times[chunk+1],
                                                         fraction.of.asymptote.after.end = fraction.asymptotes[[ end.asymptote.name[chunk] ]][i],
                                                         fraction.of.asymptote.before.start = fraction.asymptotes[[ start.asymptote.name[chunk] ]][i])
                    }))
                })
            }
            
            lapply(1:length(years), function(y){
                sub.rv = arr.rv[y,]
                
                if (length(dim.names)>0)
                {
                    dim(sub.rv) = sapply(dim.names, length)
                    dimnames(sub.rv) = dim.names
                }
                else
                    names(sub.rv) = NULL
                
                sub.rv
            })
        }
    )
)


##---------------------------------##
##---------------------------------##
##-- HELPERS for LOGISTIC SPLINE --##
##---------------------------------##
##---------------------------------##

calculate.change.ratios.two.logistic <- function(r1,r2,
                                                 times=0:t2,
                                                 r0=1,
                                                 t0=0,t1=5,t2=10,
                                                 fraction.of.asymptote.after.end=0.05,
                                                 fraction.of.asymptote.before.start=0.025,
                                                 fraction.of.asymptote.for.change.dir=0.02)
{
    r0 = round(r0, 10)
    r1 = round(r1, 10)
    r2 = round(r2, 10)
    
    if ((fraction.of.asymptote.after.end+fraction.of.asymptote.before.start)>=1)
        stop("The sum of fraction.of.asymptote.after.end and fraction.of.asymptote.before.start must be less than 1")
    
    if (r0==0 && r1==0)
        r0.to.1 = 1
    else
        r0.to.1 = r1/r0
    
    if (r0==0 && r2==0)
        r0.to.2 = 1
    else
        r0.to.2 = r2/r0
    
    if (r1==0 && r2==0)
        r1.to.2 = 1
    else if (r0==0)
    {
        if (r1==0 && r2==0)
            r1.to.2 = 0
        else
            r1.to.2 = r2/r1
    }
    else
        r1.to.2 = r0.to.2 / r0.to.1
    
    if (r0.to.1 == 1 && r1.to.2 == 1)
    {
        model.1 = model.2 = get.no.change.logistic.model(r0)
    }
    else if ((r0.to.1 > 1 && r1.to.2 > 1) ||
             (r0.to.1 < 1 && r1.to.2 < 1)) #both in the same directions
    {
        delta.K.A = (r2 - r0) / (1 - fraction.of.asymptote.for.change.dir - fraction.of.asymptote.before.start)
        A.overall = r0 - delta.K.A * fraction.of.asymptote.before.start
        K.overall = r2 + delta.K.A * fraction.of.asymptote.for.change.dir
        
        first.is.smaller = abs(r1-r0) < abs(r2-r1)
        model.1 = fit.logistic.model(r1=r1, t1=t1, A=A.overall, K=K.overall, r0=r0, t0=t0)
        model.2 = fit.logistic.model(r1=r2, t1=t2, A=A.overall, K=K.overall, r0=r1, t0=t1)
        
        if (get.logistic.slope(model.1, t1)==0 || get.logistic.slope(model.2, t1)==0)
        {}
        else if (first.is.smaller)
        {
            A.2 = r1 + (A.overall-r1) * get.logistic.slope(model.1, t1) / get.logistic.slope(model.2, t1)
            model.2 = fit.logistic.model(r1=r2, t1=t2, A=A.2, K=K.overall, r0=r1, t0=t1)
        }
        else
        {
            K.1 = r1 + (K.overall-r1) * get.logistic.slope(model.2, t1) / get.logistic.slope(model.1, t1)
            model.1 = fit.logistic.model(r1=r1, t1=t1, A=A.overall, K=K.1, r0=r0, t0=t0)
        }
    }
    else #change direction
    {
        if (r0.to.1==1)
            model.1 = get.no.change.logistic.model(r0)
        else
        {
            delta.K.A = (r1 - r0) / (1 - fraction.of.asymptote.for.change.dir - fraction.of.asymptote.before.start)
            A = r0 - delta.K.A * fraction.of.asymptote.before.start
            K = r1 + delta.K.A * fraction.of.asymptote.for.change.dir
            
            model.1 = fit.logistic.model(r1=r1, t1=t1, A=A, K=K, r0=r0, t0=t0)
        }
        
        if (r1.to.2==1)
            model.2 = get.no.change.logistic.model(r1)
        else
        {
            delta.K.A = (r2 - r1) / (1 - fraction.of.asymptote.after.end - fraction.of.asymptote.for.change.dir)
            A = r1 - delta.K.A * fraction.of.asymptote.for.change.dir
            K = r2 + delta.K.A * fraction.of.asymptote.after.end
            
            model.2 = fit.logistic.model(r1=r2, t1=t2, A=A, K=K, r0=r1, t0=t1)
        }
    }
    
    times.1 = times[times<t1]
    times.2 = setdiff(times, times.1)
    
    c(get.logistic.points(model.1, times.1), get.logistic.points(model.2, times.2))
}

calculate.change.ratios.logistic <- function(r0=1,r1,
                                             times=t0:t1,
                                             t0=0,t1=5,
                                             fraction.of.asymptote.after.end=0.05,
                                             fraction.of.asymptote.before.start=0.025)
{
    r0 = round(r0, 10)
    r1 = round(r1, 10)
    
    if ((fraction.of.asymptote.after.end+fraction.of.asymptote.before.start)>=1)
        stop("The sum of fraction.of.asymptote.after.end and fraction.of.asymptote.before.start must be less than 1")
    
    r0.to.1 = r1/r0
    
    if (r0.to.1 == 1)
        model.1 = get.no.change.logistic.model(r0)
    else
    {
        delta.K.A = (r1 - r0) / (1 - fraction.of.asymptote.after.end - fraction.of.asymptote.before.start)
        A.overall = r0 - delta.K.A * fraction.of.asymptote.before.start
        K.overall = r1 + delta.K.A * fraction.of.asymptote.after.end
        
        model.1 = fit.logistic.model(r1=r1, t1=t1, A=A.overall, K=K.overall, r0=r0, t0=t0)
    }
    
    get.logistic.points(model.1, times)
}

calculate.change.ratios.logistic.array <- function(r0.arr,
                                                   r1.arr,
                                                   times=t0:t1,
                                                   t0=0,t1=5,
                                                   fraction.of.asymptote.after.end=0.05,
                                                   fraction.of.asymptote.before.start=0.025)
{
    if (any(dim(r0.arr) != dim(r1.arr)))
        stop("r0.arr and r1.arr must have the same dimensions")
    
    if ((fraction.of.asymptote.after.end+fraction.of.asymptote.before.start)>=1)
        stop("The sum of fraction.of.asymptote.after.end and fraction.of.asymptote.before.start must be less than 1")
    
    if (all(r0.arr==r1.arr))
        return (lapply(times, function(i){r0.arr}))
    
    models = lapply(1:length(r0.arr), function(i){
        
        r0 = round(r0.arr[i], 10)
        r1 = round(r1.arr[i], 10)
        
        r0.to.1 = r1/r0
        
        if (r0.to.1 == 1)
            model.1 = get.no.change.logistic.model(r0)
        else
        {
            delta.K.A = (r1 - r0) / (1 - fraction.of.asymptote.after.end - fraction.of.asymptote.before.start)
            A.overall = r0 - delta.K.A * fraction.of.asymptote.before.start
            K.overall = r1 + delta.K.A * fraction.of.asymptote.after.end
            
            model.1 = fit.logistic.model(r1=r1, t1=t1, A=A.overall, K=K.overall, r0=r0, t0=t0)
        }
        
        model.1
    })
    
    lapply(times, function(t){
        arr = sapply(models, get.logistic.points, times=t)
        dim(arr) = dim(r0.arr)
        dimnames(arr) = dimnames(r0.arr)
        arr
    })
}


##-- LOWER-LEVEL LOGISTIC MODELS --##

get.logistic.points <- function(logistic.model, times)
{
    rv = logistic.model$A + (logistic.model$K - logistic.model$A) /
        (1 + logistic.model$Q*exp(-logistic.model$B * (times-logistic.model$t0)))
    names(rv) = times
    rv
}

get.logistic.slope <- function(logistic.model, times)
{
    rv = (logistic.model$K - logistic.model$A) *
        logistic.model$B * logistic.model$Q * exp(-logistic.model$B * times) /
        (1 + logistic.model$Q * exp(-logistic.model$B * (times-logistic.model$t0)))^2
    names(rv) = times
    rv
}

fit.logistic.model <- function(r1, t1, A, K, r0=1, t0=0)
{
    # Solve for Q, B
    Q = (K-r0)/(r0-A)
    B = (log((r1-A)/(K-r1)) + log(Q)) / (t1-t0)
    if (is.na(B))
        stop("NA values generated in fitting logistic model")
    
    
    # Store in an object and return
    list(K=K,
         A=A,
         B=B,
         Q=Q,
         t0=t0)
}

get.no.change.logistic.model <- function(r0=1)
{
    list(K=r0,
         A=0,
         Q=0,
         B=0,
         t0=0)
}


# f(t) = A + (K - A) / (1 + Q*e^(-B*t))
smooth.logistic.one.point <- function(r1, t1, times=t0:t1, r0=1, t0 = 0,
                                      A=NA, K=NA,
                                      r.span.of.total=0.9, min.r=0, max.r=Inf)
{
    if (is.na(A) || is.na(K))
    {
        # Set up upper and lower asymptotes
        r.span = abs(r1-r0)
        r.span.total = r.span / r.span.of.total
        r.extra = r.span.total - r.span
        
        A = min(r1, r0) - r.extra/2
        A = max(min.r, A)
        
        K = max(r1, r0) + r.extra/2
        K = min(max.r, K)
    }
    
    # Plug and chug
}

