
##--  The scales on which model elements and quantities operate --##

MODEL.SCALE.INFO = list(
    rate = list(needs.denominator = T,
                aggregatable = T),
    ratio = list(needs.denominator = F,
                 aggregatable = F),
    proportion = list(needs.denominator = T,
                      aggregatable = T),
    proportion.leaving = list(needs.denominator = T,
                              aggregatable = T),
    proportion.staying = list(needs.denominator = T,
                              aggregatable = T),
    time = list(needs.denominator = T,
                aggregatable = T),
    number = list(needs.denominator = F,
                  aggregatable = T),
    non.negative.number = list(needs.denominator = T,
                               aggregatable = T)
)

MODEL.SCALES = names(MODEL.SCALE.INFO)

##--------------------------##
##-- BASIC INFO on SCALES --##
##--------------------------##

scale.is.aggregatable <- function(scales)
{
    sapply(MODEL.SCALE.INFO[scales], function(info){
        info$aggregatable
    })
}

scale.needs.denominator <- function(scales)
{
    sapply(MODEL.SCALE.INFO[scales], function(info){
        info$needs.denominator
    })
}

##-------------------------------##
##-- CONVERTING between SCALES --##
##-------------------------------##

# values can be a list or a numeric vector
convert.model.scale <- function(values,
                                convert.from.scale,
                                convert.to.scale,
                                error.prefix='')
{
    if (convert.from.scale==convert.to.scale)
        values
    else if (is.list(values))
        lapply(values, do.convert.model.scale,
               convert.from.scale=convert.from.scale, convert.to.scale=convert.to.scale,
               error.prefix=error.prefix)
    else
        do.convert.model.scale(values=values, convert.from.scale = convert.from.scale,
                               convert.to.scale = convert.from.scale, error.prefix=error.prefix)
}


do.convert.model.scale <- function(values,
                                   convert.from.scale,
                                   convert.to.scale,
                                   error.prefix='')
{ 
    if (convert.from.scale==convert.to.scale)
        values
    else if (convert.from.scale=='ratio' || convert.from.scale=='number' || convert.from.scale=='non.negative.number')
        stop(paste0(error.prefix, "Cannot convert from scale '", convert.from.scale,"' (to '", convert.to.scale, "')"))
    else if (convert.to.scale=='ratio'|| convert.to.scale=='number' || convert.to.scale=='non.negative.number')
        stop(paste0(error.prefix, "Cannot convert to scale '", convert.to.scale, "' (from '", convert.from.scale, "')"))
    else if (convert.from.scale=='rate')
    {
        if (convert.to.scale=='proportion' || convert.to.scale=='proportion.leaving')
            1-exp(-values)
        else if (convert.to.scale=='proportion.staying')
            exp(-values)
        else if (convert.to.scale=='time')
            1/values
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
                              error.prefix='')
{
    if (!is.character(scale) || length(scale)!=1 || is.na(scale))
        stop(paste0(error.prefix, "'", varname.for.error, "' must be a non-NA character scalar"))
    
    if (all(scale != MODEL.SCALES))
        stop(paste0(error.prefix, "'", varname.for.error, "' must be one of the following: ",
                    collapse.with.or("'", MODEL.SCALES, "'")))
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
        if (any(!is.na(values) & (values<0) | any(values>1)))
            stop(paste0(error.prefix, 
                        "Invalid value(s) ", variable.name.for.error,
                        "for scale '", scale, ": values must be between 0 and 1"))
    }
    else if (scale!='number')
        stop(paste0(error.prefix,
                    "'", scale, "' is not defined as a valid model scale"))
}


##----------------------------------##
##-- INFERRING and JOINING SCALES --##
##----------------------------------##

infer.two.arg.expr.scale <- function(transition.mapping,
                                     operator,
                                     val1,
                                     val2,
                                     scale1,
                                     scale2)
{
    if (operator=='+')
    {
        if ((scale1=='number' | scale1=='non.negative.number') ||
            (scale2=='number' | scale2=='non.negative.number'))
            'number'
        else
            join.scales(scale1, scale2)
    }
    else if (operator=='-')
    {
        # A rate minus a rate is a rate
        # 1 - proportion or proportion leaving -> proportion staying
        # 1 - proportion.staying -> proportion
        # number - number -> number
        # any combo of number and non-negative.number -> number
        
        if (scale1=='rate' && scale2=='rate')
            'rate'
        else if ((is.numeric(val1) && length(val1)==1 && val1==1))
        {
            if (scale2=='proportion' || scale2=='proportion.leaving')
                'proportion.staying'
            else if (scale2=='proportion.staying')
                'proportion'
        }
        else if ((scale1=='number' | scale1=='non.negative.number') ||
                 (scale2=='number' | scale2=='non.negative.number'))
            'number'
        else
            'unknown'
    }
    else if (operator=='*')
    {
        # rate * anything is a rate
        # time * anything is a time
        # <scale> * ratio is <scale>
        
        if (scale1=='rate')
        {
            if (scale2=='time')
                c('rate','time')
            else
                'rate'
        }
        else if (scale2=='rate')
        {
            if (scale1=='time')
                c('rate','time')
            else
                'rate'
        }
        else if (scale1=='time' || scale2=='time')
            'time'
        else if (scale1=='ratio')
            scale2
        else if (scale2=='ratio')
            scale1
        else if (scale1=='non.negative.number' && scale2=='non.negative.number')
            'non.negative.number'
        else if ((scale1=='number' || scale1=='non.negative.number') &&
                 (scale2=='number' || scale2=='non.negative.number'))
            'number'
        else
            'unknown'
    }
    else if (operator=='/')
    {
        # numeric / rate -> time
        # numeric / time -> rate
        # time / anything -> time
        # rate / anything -> rate
        # two non.negative.numbers --> non.negative.number
        # otherwise, any combo of numbers and non.negative.numbers -> number
        
        if (is.numeric(val1) && scale2=='rate')
            'time'
        else if (is.numeric(val1) && scale2=='time')
            'rate'
        else if (scale1=='rate')
            'rate'
        else if (scale1=='time')
            'time'
        else if (scale1=='non.negative.number' && scale2=='non.negative.number')
            'non.negative.number'
        else if ((scale1=='number' || scale1=='non.negative.number') &&
                 (scale2=='number' || scale2=='non.negative.number'))
            'number'
        else
            'unknown'
    }
    else
        'unknown'
}

infer.one.arg.expr.scale <- function(transition.mapping,
                                     operator, val, scale)
{
    if (operator=='-')
    {
        if (any(scale=='rate'))
            'rate'
        else if (scale=='number' || scale=='non.negative.number')
            'number'
        else
            'unknown'
    }
    else if (operator=='log')
    {
        if (scale=='proportion.staying')
            'rate'
        else if (scale=='non.negative.number' || scale=='number')
            'number'
        else
            'unknown'
    }
    else if (operator=='exp')
    {
        if (any(scale=='rate'))
            'proportion.staying'
        else
            'unknown'
    }
    else
        'unknown'
}

join.scales <- function(scale1, scale2)
{
    scale1[scale1=='proportion.leaving'] = 'proportion'
    scale2[scale2=='proportion.leaving'] = 'proportion'
    
    rv = intersect(scale1, scale2)
    if (length(rv)>1)
        rv = rv[rv!='unknown']
    rv
}
