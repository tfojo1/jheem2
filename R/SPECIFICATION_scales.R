
##--  The scales on which model elements and quantities operate --##

MODEL.SCALE.INFO = list(
    rate = list(needs.denominator = T,
                aggregate.on.scale = 'rate',
                unbounded.transformation = get.link('log')),
    ratio = list(needs.denominator = NA,
                 unbounded.transformation = get.link('log')),
    proportion = list(needs.denominator = T,
                      aggregate.on.scale = 'proportion',
                      unbounded.transformation = get.link('logit')),
    proportion.leaving = list(needs.denominator = T,
                              aggregate.on.scale = 'proportion.leaving',
                              unbounded.transformation = get.link('logit')),
    proportion.staying = list(needs.denominator = T,
                              aggregate.on.scale = 'proportion.staying',
                              unbounded.transformation = get.link('logit')),
    time = list(needs.denominator = T,
                aggregate.on.scale = 'time',
                unbounded.transformation = get.link('log')),
    number = list(needs.denominator = F,
                  aggregate.on.scale = 'number',
                  unbounded.transformation = get.link('identity')),
    non.negative.number = list(needs.denominator = F,
                               aggregate.on.scale = 'non.negative.number',
                               unbounded.transformation = get.link('log')),
    odds = list(needs.denominator = T,
                aggregate.on.scale = 'proportion',
                unbounded.transformation = get.link('log')),
    odds.leaving = list(needs.denominator = T,
                        aggregate.on.scale = 'proportion.leaving',
                        unbounded.transformation = get.link('log')),
    odds.staying = list(needs.denominator = T,
                        aggregate.on.scale = 'proportion.staying',
                        unbounded.transformation = get.link('log'))
)

MODEL.SCALES = names(MODEL.SCALE.INFO)

##--------------------------##
##-- BASIC INFO on SCALES --##
##--------------------------##

scale.is.aggregatable <- function(scales)
{
    sapply(MODEL.SCALE.INFO[scales], function(info){
        !is.na(info$aggregate.on.scale)
    })
}

scale.needs.denominator <- function(scales)
{
    sapply(MODEL.SCALE.INFO[scales], function(info){
        info$needs.denominator
    })
}

can.convert.scale <- function(convert.from.scale,
                              convert.to.scale,
                              denominator.values=NULL)
{
    tryCatch({
        do.convert.model.scale(values = 0.5,
                               convert.from.scale = convert.from.scale,
                               convert.to.scale = convert.to.scale,
                               denominator.values = denominator.values,
                               error.prefix = '')
        T
    }, error = function(e){
        F
    })
}

##-------------------------------##
##-- CONVERTING between SCALES --##
##-------------------------------##

# values can be a list or a numeric vector
convert.model.scale <- function(values,
                                convert.from.scale,
                                convert.to.scale,
                                denominator.values=NULL,
                                error.prefix='')
{
    if (convert.from.scale==convert.to.scale ||
        (convert.from.scale=='proportion' && convert.to.scale=='proportion.leaving') ||
        (convert.from.scale=='proportion.leaving' && convert.to.scale=='proportion') ||
        (convert.from.scale=='odds' && convert.to.scale=='odds.leaving') ||
        (convert.from.scale=='odds.leaving' && convert.to.scale=='odds') ||
        (convert.from.scale=='non.negative.number' && convert.to.scale=='number'))
    {
        values
    }
    else if (is.list(values))
    {
        lapply(1:length(values), function(i){
            do.convert.model.scale(values = values[[i]],
                                   convert.from.scale = convert.from.scale,
                                   convert.to.scale = convert.to.scale,
                                   denominator.values = denominator.values[[i]],
                                   error.prefix = error.prefix)
        })
    }
    else
    {
        do.convert.model.scale(values=values, convert.from.scale = convert.from.scale,
                               convert.to.scale = convert.to.scale, error.prefix=error.prefix,
                               denominator.values = denominator.values)
    }
}

## !!NOTE!!
## There is a C++ version of this function in engine_helpers.cpp
## Any updates we make here need to be reflected in that function as well
do.convert.model.scale <- function(values,
                                   convert.from.scale,
                                   convert.to.scale,
                                   denominator.values = NULL,
                                   error.prefix='')
{ 
    if (convert.from.scale==convert.to.scale ||
        (convert.from.scale=='proportion' && convert.to.scale=='proportion.leaving') ||
        (convert.from.scale=='proportion.leaving' && convert.to.scale=='proportion') ||
        (convert.from.scale=='odds' && convert.to.scale=='odds.leaving') ||
        (convert.from.scale=='odds.leaving' && convert.to.scale=='odds') ||
        (convert.from.scale=='non.negative.number' && convert.to.scale=='number'))
        values
    else if (convert.from.scale=='ratio' || convert.to.scale=='ratio')
        stop(paste0(error.prefix, "Cannot convert from scale '", convert.from.scale,"' (to '", convert.to.scale, "')"))
    else if (convert.from.scale=='number' || convert.from.scale=='non.negative.number')
    {
        if (convert.to.scale=='number')
            values
        else if (convert.to.scale=='non.negative.number' && convert.from.scale=='number')
        {
            if (any(values<0))
                stop(paste0(error.prefix, "Cannot convert from scale '", 
                            convert.from.scale,"' to '", convert.to.scale, "'",
                            " - some values are less than zero"))
            else
                values
        }
        else
        {
            if (is.null(denominator.values))
                stop(paste0(error.prefix, "In order to convert from scale '", 
                            convert.from.scale,"' to '", convert.to.scale, "'",
                            ", denominator.values must be specified"))
            
            if (any(values<0))
                stop(paste0(error.prefix, "Cannot convert from scale '", 
                            convert.from.scale,"' to '", convert.to.scale, "'",
                            " - some values are less than zero"))
            
            p = values / denominator.values
            do.convert.model.scale(values = p,
                                   convert.from.scale = 'proportion',
                                   convert.to.scale = convert.to.scale,
                                   denominator.values = NULL,
                                   error.prefix='')
        }
    }
    else if (convert.to.scale=='number' || convert.to.scale=='non.negative.number')
    {
        # if convert.from.scale was number or non.negative.number, it would have been caught by the condition above

        p = do.convert.model.scale(values = values,
                                   convert.from.scale = convert.from.scale,
                                   convert.to.scale = 'proportion',
                                   denominator.values = NULL,
                                   error.prefix='')
        
        p * denominator.values
    }
    else if (convert.from.scale=='rate')
    {
        if (convert.to.scale=='proportion' || convert.to.scale=='proportion.leaving')
            1-exp(-values)
        else if (convert.to.scale=='proportion.staying')
            exp(-values)
        else if (convert.to.scale=='time')
            1/values
        else if (convert.to.scale=='odds' || convert.to.scale=='odds.leaving')
            exp(values) - 1
        else if (convert.to.scale=='odds.staying')
            1 / (exp(values)-1)
        else
            stop(paste0(error.prefix, "Invalid scales for conversion: '", convert.from.scale, "' to '", convert.to.scale, "'"))
    }
    else if (convert.from.scale=='proportion' || convert.from.scale=='proportion.leaving')
    {
        if (convert.to.scale=='rate')
            -log(1-values)
        else if (convert.to.scale=='proportion.staying')
            1-values
        else if (convert.to.scale=='time')
            -1/log(1-values)
        else if (convert.to.scale=='odds' || convert.to.scale=='odds.leaving')
            values / (1-values)
        else if (convert.to.scale=='odds.staying')
            (1-values) / values
        else
            stop(paste0(error.prefix, "Invalid scales for conversion: '", convert.from.scale, "' to '", convert.to.scale, "'"))
    }
    else if (convert.from.scale=='proportion.staying')
    {
        if (convert.to.scale=='rate')
            -log(values)
        else if (convert.to.scale=='proportion' || convert.to.scale=='proportion.leaving')
            1-values
        else if (convert.to.scale=='time')
            -1/log(values)
        else if (convert.to.scale=='odds' || convert.to.scale=='odds.leaving')
            (1-values) / values
        else if (convert.to.scale=='odds.staying')
            values / (1-values)
        else
            stop(paste0(error.prefix, "Invalid scales for conversion: '", convert.from.scale, "' to '", convert.to.scale, "'"))
    }
    else if (convert.from.scale=='time')
    {
        if (convert.to.scale=='rate')
            1/values
        else if (convert.to.scale=='proportion' || convert.to.scale=='proportion.leaving')
            1-exp(-1/values)
        else if (convert.to.scale=='proportion.staying')
            exp(-1/values)
        else if (convert.to.scale=='odds' || convert.to.scale=='odds.leaving')
            exp(1/values) - 1
        else if (convert.to.scale=='odds.staying')
            1 / (exp(1/values)-1)
        else
            stop(paste0(error.prefix, "Invalid scales for conversion: '", convert.from.scale, "' to '", convert.to.scale, "'"))
    }
    else if (convert.from.scale=='odds' || convert.from.scale=='odds.leaving')
    {
        if (convert.to.scale=='rate')
            log(values+1)
        else if (convert.to.scale=='proportion' || convert.to.scale=='proportion.leaving')
            values / (1+values)
        else if (convert.to.scale=='proportion.staying')
            1 - values / (1+values)
        else if (convert.to.scale=='time')
            1 / (log(values+1))
        else if (convert.to.scale=='odds.staying')
            1 / values
        else
            stop(paste0(error.prefix, "Invalid scales for conversion: '", convert.from.scale, "' to '", convert.to.scale, "'"))
        
    }
    else if (convert.from.scale=='odds.staying')
    {
        if (convert.to.scale=='rate')
            log(1/values+1)
        else if (convert.to.scale=='proportion' || convert.to.scale=='proportion.leaving')
            1 - values / (1+values)
        else if (convert.to.scale=='proportion.staying')
            values / (1+values)
        else if (convert.to.scale=='time')
            1 / (log(1/values+1))
        else if (convert.to.scale=='odds' || convert.to.scale=='odds.leaving')
            1 / values
        else
            stop(paste0(error.prefix, "Invalid scales for conversion: '", convert.from.scale, "' to '", convert.to.scale, "'"))
    }
    else
        stop(paste0(error.prefix, "Invalid from scale for conversion:'", convert.from.scale, "'"))
}

##----------------------------------##
##-- CHECKING SCALES for validity --##
##----------------------------------##

check.model.scale <- function(scale,
                              varname.for.error='scale',
                              require.scalar=T,
                              error.prefix='')
{
    if (require.scalar)
    {
        if (!is.character(scale) || length(scale)!=1 || is.na(scale))
            stop(paste0(error.prefix, "'", varname.for.error, "' must be a non-NA character scalar"))
        
        if (all(scale != MODEL.SCALES))
            stop(paste0(error.prefix, "'", varname.for.error, "' must be one of the following: ",
                        collapse.with.or("'", MODEL.SCALES, "'")))
    }
    else
    {
        if (!is.character(scale) || length(scale)==0 || any(is.na(scale)))
            stop(paste0(error.prefix, "Invalid ", varname.for.error, ": '", scale, "'. Must be a non empty, non-NA character vector"))
        
        invalid.scales = setdiff(scale, MODEL.SCALES)
        if (length(invalid.scales)>0)
        {
            stop(paste0(error.prefix, "Invalid ", varname.for.error, ": ",
                        collapse.with.and("'", invalid.scales, "'"),
                        ". Must be one of the following: ",
                        collapse.with.or("'", MODEL.SCALES, "'")))
        }
    }
}


check.values.for.model.scale <- function(values, scale, 
                                         variable.name.for.error=NULL,
                                         error.prefix='')
{
    if (is.list(values))
        sapply(values, do.check.values.for.model.scale, scale=scale, 
               variable.name.for.error=variable.name.for.error,
               error.prefix=error.prefix)
    else
        do.check.values.for.model.scale(values=values, scale=scale, 
                                        variable.name.for.error=variable.name.for.error,
                                        error.prefix=error.prefix)
}

# internal to transitions.mapping code
do.check.values.for.model.scale <- function(values, scale,
                                            variable.name.for.error=NULL,
                                            error.prefix='')
{
    if (is.null(variable.name.for.error) || is.na(variable.name.for.error) || nchar(variable.name.for.error)==0)
        variable.name.for.error = 0
    else
        variable.name.for.error = paste0("in '", variable.name.for.error, "' ")
    
    if (scale=='rate' || scale=='ratio' || scale=='time' || scale=='non.negative.number')
    {
        if (any(!is.na(values) & values < 0))
            stop(paste0(error.prefix, 
                        "Invalid value(s) ", variable.name.for.error,
                        "for scale '", scale, "': values must be non-negative"))
    }
    else if (scale=='proportion' || scale=='proportion.leaving' || scale=='proportion.staying')
    {
        if (any(!is.na(values) & (values<0 | values>1)))
            stop(paste0(error.prefix, 
                        "Invalid value(s) ", variable.name.for.error,
                        "for scale '", scale, ": values must be between 0 and 1"))
    }
    else if (scale!='number')
        stop(paste0(error.prefix,
                    "'", scale, "' is not defined as a valid model scale"))
}

##----------------------------##
##-- AGGREGATING FOR SCALES --##
##----------------------------##

aggregate.for.scale <- function(values,
                                scale,
                                keep.dimensions,
                                denominator.values = NULL)
{
    if (MODEL.SCALE.INFO[[scale]]$needs.denominator)
    {
        aggregate.on.scale = MODEL.SCALE.INFO[[scale]]$aggregate.on.scale
        
        if (aggregate.on.scale != scale)
            values = do.convert.model.scale(values, convert.from.scale=scale, convert.to.scale=aggregate.on.scale)
        
        numerators = apply(values * denominator.values, keep.dimensions, sum)
        denominators = apply(denominator.values, keep.dimensions, sum)
        values = numerators / denominators
        
        if (aggregate.on.scale)
            do.convert.model.scale(values, convert.from.scale = aggregate.on.scale, convert.to.scale = scale)
        else
            values
    }
    else
        apply(values, keep.dimensions, sum)
}

##-------------------------##
##-- TRANSFORMING SCALES --##
##-------------------------##

transform.to.unbounded.scale <- function(values,
                                         scales)
{
    if (!is.null(names(values)) && !is.null(names(scales)))
        scales = scales[names(values)]
    
    rv = sapply(1:length(values), function(i){
        MODEL.SCALE.INFO[[ scales[i] ]]$unbounded.transformation$apply(values[i])
    })
    names(rv) = names(values)
    rv
}

transform.from.unbounded.scale <- function(values,
                                           scales)
{
    if (!is.null(names(values)) && !is.null(names(scales)))
        scales = scales[names(values)]
    
    rv = sapply(1:length(values), function(i){
        MODEL.SCALE.INFO[[ scales[i] ]]$unbounded.transformation$reverse.apply(values[i])
    })
    names(rv) = names(values)
    rv
}