
# This file defines an 'evaluatable.value' class:
#   A flexible class that allows us to specify how a value will be generated later
#   (via a function, an expression, or substituting a different character value)

EVALUATABLE.VALUE = R6::R6Class(
    'evaluatable.value',
    portable = F,
    
    public = list(
        
        initialize = function(na.replacement,
                              allow.numeric.value,
                              allow.character.value,
                              allow.expression.value,
                              allow.function.value,
                              allowed.expression.functions,
                              function.arguments.to.be.supplied.later = character(), #pass through to FUNCTION.WRAPPER$new()
                              error.prefix)
        {
            #-- Validate na.replacement --#
            if (!is.numeric(na.replacement))
                stop(paste0(error.prefix, "'na.replacement' must be a single NUMERIC value"))
            if (length(na.replacement) != 1)
                stop(paste0(error.prefix, "'na.replacement' must be a SINGLE numeric value"))
            
            #-- Validate logical arguments --#
            if (!is.logical(allow.numeric.value) || length(allow.numeric.value)!=1 || is.na(allow.numeric.value))
                stop(paste0(error.prefix, "'allow.numeric.value' must be a single, non-NA, logical value"))
            if (!is.logical(allow.character.value) || length(allow.character.value)!=1 || is.na(allow.character.value))
                stop(paste0(error.prefix, "'allow.character.value' must be a single, non-NA, logical value"))
            if (!is.logical(allow.expression.value) || length(allow.expression.value)!=1 || is.na(allow.expression.value))
                stop(paste0(error.prefix, "'allow.expression.value' must be a single, non-NA, logical value"))
            if (!is.logical(allow.function.value) || length(allow.function.value)!=1 || is.na(allow.function.value))
                stop(paste0(error.prefix, "'allow.function.value' must be a single, non-NA, logical value"))
            
            #-- Validate allowed.expression.functions --#
            if (allow.expression.value)
            {
                if (length(allowed.expression.functions)==0)
                    allowed.expression.functions = character()
                else if (!is.character(allowed.expression.functions) || any(is.na(allowed.expression.functions)))
                    stop(paste0(error.prefix, "'allowed.expression.functions' must be a character vector without NA values"))
            }
            
            
            #-- Validate function.arguments.to.be.supplied.later --#
            if (!is.character(function.arguments.to.be.supplied.later))
                stop(paste0(error.prefix, "'function.arguments.to.be.supplied.later' passed in creating a new evaluateable.value must be a character vector"))
            if (any(is.na(function.arguments.to.be.supplied.later)))
                stop(paste0(error.prefix, "'function.arguments.to.be.supplied.later' passed in creating a new evaluateable.value cannot contain NA values"))
            
            
            #-- Store Values --#
            private$i.na.replacement = na.replacement
            private$i.allow.numeric.value = allow.numeric.value
            private$i.allow.character.value = allow.character.value
            private$i.allow.expression.value = allow.expression.value
            private$i.allow.function.value = allow.function.value
            private$i.allowed.expression.functions = allowed.expression.functions
            private$i.function.arguments.to.be.supplied.later = function.arguments.to.be.supplied.later
        },
        
        set.value = function(value, ..., value.name='value', error.prefix)
        {
            # Some prelim checking of 'value'
            if (length(value)==0)
                stop(paste0(error.prefix, "'value' cannot be empty"))
            
            if (!is.function(value) && length(list(...))>0)
                stop(paste0(error.prefix,
                            "You can only pass arguments to ... when 'value' is a function"))
            
            # Validate value.name
            if (length(value.name)==0)
                value.name=='value'
            else if (!is.character(value.name) || length(value.name)!=1 || is.na(value.name) || nchar(value.name)==0)
                stop(paste0(error.prefix, "'value.name' passed to set.value() must be a single, non-NA, non-empty character value"))
                
            # Parse 'value'
            if (private$i.allow.character.value && is.character(value))
            {
                if (length(value) != 1 || is.na(value) || value=='')
                    stop(paste0(error.prefix,
                                "if the given 'value' is a character, it must be a single, non-NA, non-empty character value"))
                
                private$i.depends.on = as.character(parse(text=value)[[1]])
                private$i.value.type = 'character'
            }
            else if (private$i.allow.expression.value && 
                     (is.expression(value) || is.call(value) || is.name(value)))
            {
                if (is.expression(value))
                {
                    if (length(expression)!=1)
                        stop(paste0(error.prefix,
                                    "if the given 'value' is an expression, it must be of length 1"))
                    
                    value = value[[1]]
                }
                
                #check for functions
                functions.in.value = get.function.names.in.expr(value)
                
                if (length(functions.in.value)>0)
                {
                    if (any(functions.in.value=='[') && !any(private$i.allowed.expression.functions=='['))
                        stop(paste0(error.prefix, "subsetting ([ ]) is not allowed withing expression for model.quantity.component value"))
                    
                    invalid.functions = setdiff(functions.in.value, private$i.allowed.expression.functions)
                    if (length(invalid.functions)>0)
                        stop(paste0(error.prefix,
                                    "Invalid function(s) in the expression for 'value' for model quantity component: ",
                                    collapse.with.and("'", invalid.functions, "'"),
                                    " - only the following functions are allowed: ",
                                    collapse.with.or("'", private$i.allowed.expression.functions, "'")))
                }
                
                private$i.depends.on = as.character(all.vars(value))
                private$i.value.type = 'expression'
            }
            else if (private$i.allow.numeric.value && is.numeric(value))
            {
                if (length(value) != 1 && is.null(dimnames(value)))
                    stop(paste0(error.prefix,
                                "if the given 'value' is not a scalar, it must have dimenames set"))
                
                if (length(value) != 1 && is.null(names(dimnames(value))))
                    stop(paste0(error.prefix,
                                "if the given 'value' is not a scalar, it must have NAMED dimnames set"))
                
                if (any(is.na(value)))
                    stop(paste0(error.prefix, "'value' cannot contain NA values"))
                
                private$i.depends.on = character()
                private$i.value.type = 'numeric'
            }
            else if (private$i.allow.function.value &&
                     (is.function(value) || is(value, 'specification.function.info')))
            {
                value = FUNCTION.WRAPPER$new(fn = value,
                                             ...,
                                             fn.name = value.name,
                                             require.all.arguments.up.front = F,
                                             throw.error.if.missing.arguments.at.execute = T,
                                             arguments.to.be.supplied.later = private$i.function.arguments.to.be.supplied.later,
                                             error.prefix=error.prefix)

                private$i.depends.on = value$missing.argument.names
                private$i.value.type = 'function'
            }
            else
            {
                allowed.value.types = c('a character value',
                                        'a numeric value',
                                        'a function',
                                        'an expression',
                                        "a 'name'",
                                        "a 'call' generated by expr()")
                allowed.value.types = allowed.value.types[c(private$i.allow.character.value,
                                                            private$i.allow.numeric.value,
                                                            private$i.allow.function.value,
                                                            private$i.allow.expression.value,
                                                            private$i.allow.expression.value,
                                                            private$i.allow.expression.value)]
                
                stop(paste0(error.prefix, "'value' must be ",
                            collapse.with.or(allowed.value.types)))
            }
            
            #-- Store values --#
            private$i.value = value
            private$i.value.name = value.name
            private$i.depends.on.to.binding.name = character()
            
            #-- Return --#
            invisible(self)
        },
        
        evaluate = function(bindings, error.prefix)
        {
            if (private$i.value.type=='numeric')
            {
                rv = private$i.value
            }
            else if (private$i.value.type=='character')
            {
                rv = bindings[[private$i.value]]
                names(rv) = NULL
            }
            else if (private$i.value.type=='expression')
            {
                rv = eval(private$i.value, envir=bindings)
            }
            else if (private$i.value.type=='function')
            {
                if (length(bindings)>0 && !is.null(names(bindings)))
                {
                    names(bindings) = sapply(names(bindings), function(name){
                        if (any(name==names(private$i.depends.on.to.binding.name)))
                            private$private$i.depends.on.to.binding.name[name]
                        else
                            name
                    })
                }
                
                rv = private$i.value$execute(bindings=bindings, error.prefix = error.prefix)
            }
            else
                stop(paste0(error.prefix,
                            "Invalid value.type for an 'evaluatable.value': '", 
                            private$i.value.type, "'"))
            
            if (!is.na(private$i.na.replacement))
                rv[is.na(rv)] = private$i.na.replacement
            
            rv
        },
        
        can.evaluate = function(bindings, error.prefix)
        {
            if (length(bindings)==0)
                length(private$i.depends.on)==0
            else
            {
                if (!is.list(bindings))
                    stop(paste0(error.prefix, "'bindings' must be a list"))
                if (is.null(names(bindings)))
                    stop(paste0(error.prefix, "'bindings' must be a NAMED list"))
                
                is.subset(sub=private$i.depends.on, super=names(bindings)) &&
                    all(sapply(bindings, is.numeric))
            }
        },
        
        rename.depends.on = function(mapping)
        {
            if (private$i.value.type=='character')
            {
                if (any(names(mapping)==private$i.value))
                    self$set.value(mapping[private$i.value])
            }
            else if (private$i.value.type=='expression')
            {
                if (length(intersect(names(mapping), private$i.depends.on))>0)
                    self$set.value(rename.expression.vars(expr = private$i.value,
                                                          new.names = mapping))
            }
            else if (private$i.value.type=='function')
            {
                mapping = mapping[names(mapping)!=mapping]
                
                for (map.name in names(mapping))
                {
                    map.value = mapping[map.name]
                    if (any(names(private$i.depends.on.to.binding.name)==map.value))
                        names(private$i.depends.on.to.binding.name)[names(private$i.depends.on.to.binding.name)==map.value] = map.name
                    else
                        private$i.depends.on.to.binding.name[map.name] = map.value
                }
                private$i.depends.on.to.binding.name = mapping
            }
            
            self
        },
        
        
        simplify = function(bindings, error.prefix)
        {
            
        }
    ),
    
    active = list(
        
        depends.on = function(value)
        {
            if (missing(value))
                private$i.depends.on
            else
                stop("Cannot modify 'depends.on' value - it is read-only")
        },
        
        value.name = function(value)
        {
            if (missing(value))
                private$i.value.name
            else
                stop("Cannot modify 'value.name' - it is read-only")
        },
        
        value.type = function(value)
        {
            if (missing(value))
                private$i.value.type
            else
                stop("Cannot modify 'value.type' - it is read-only")
        }
    ),
    
    private = list(
        
        i.value = NULL,
        i.value.type = NULL,
        i.value.name = NULL,

        i.depends.on = NULL,
        i.depends.on.to.binding.name = NULL, #names are elements of depends.on; values are the names needed by bindings
                
        i.allow.numeric.value = NULL,
        i.allow.character.value = NULL,
        i.allow.expression.value = NULL,
        i.allow.function.value = NULL,
        
        i.allowed.expression.functions = NULL,
        i.function.arguments.to.be.supplied.later = NULL,
        
        i.na.replacement = NULL
    )
)

FUNCTION.WRAPPER = R6::R6Class(
    'function.wrapper',
    
    public = list(
        
        #'@param arguments.to.be.supplied.later Name of arguments that will be passed when execute() is called. These CANNOT be passed to ..., and do not trigger an error if missing
        initialize = function(fn,
                              ...,
                              fn.name,
                              require.all.arguments.up.front=F,
                              throw.error.if.missing.arguments.at.execute=T,
                              arguments.to.be.supplied.later = character(),
                              allowed.arguments.for.later = NULL,
                              error.prefix='')
        {
            #-- Validate error.prefix --#
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop("'error.prefix' for creating a function.wrapper must be a single, non-NA character value")
            
            #-- Make sure fn is a function of the name of a function --#
            if (is.character(fn))
            {
                if (length(fn) != 1 || is.na(fn))
                    stop(paste0(error.prefix, "If 'fn' is the name of a function, it must be a single, non-NA character value"))
                
                fn.name = fn
                tryCatch({
                    fn = get(fn.name)
                },
                error = function(e){
                    stop(paste0(error.prefix, "'", fn.name, "' is not the name of a function"))
                })
                
                if (!is.function(fn))
                    stop(paste0(error.prefix, "'", fn.name, "' is not the name of a FUNCTION"))
            }
            else
            {
                if (!is.character(fn.name) || length(fn.name) != 1 || is.na(fn.name))
                    stop(paste0(error.prefix, "'fn.name' must be a single, non-NA character value giving the name of the function passed to 'fn'"))
                
                if (!is.function(fn))
                    stop(paste0(error.prefix, "The given fn ('", fn.name, "') is not a function"))
            }
            
            
            #-- Validate other arguments --#
            if (!is.logical(require.all.arguments.up.front) || length(require.all.arguments.up.front)!=1 || is.na(require.all.arguments.up.front))
                stop(paste0(error.prefix, "'require.all.arguments.up.front' passed in creating a new function.wrapper must be a single, non-NA logical value"))
            if (!is.logical(throw.error.if.missing.arguments.at.execute) || length(throw.error.if.missing.arguments.at.execute)!=1 || is.na(throw.error.if.missing.arguments.at.execute))
                stop(paste0(error.prefix, "'throw.error.if.missing.arguments.at.execute' passed in creating a new function.wrapper must be a single, non-NA logical value"))
            if (!is.character(arguments.to.be.supplied.later))
                stop(paste0(error.prefix, "'arguments.to.be.supplied.later' passed in creating a new function.wrapper must be a character vector"))
            if (any(is.na(arguments.to.be.supplied.later)))
                stop(paste0(error.prefix, "'arguments.to.be.supplied.later' passed in creating a new function.wrapper cannot contain NA values"))
            if (!is.null(arguments.to.be.supplied.later) && !is.character(arguments.to.be.supplied.later))
                stop(paste0(error.prefix, "'arguments.to.be.supplied.later' passed in creating a new function.wrapper must be either NULL or a character vector"))
            if (any(is.na(arguments.to.be.supplied.later)))
                stop(paste0(error.prefix, "'arguments.to.be.supplied.later' passed in creating a new function.wrapper cannot contain NA values"))
            
            #-- Figure out what arguments are missing --#
            fn.args = formals(args(fn))
            arg.names = names(fn.args)

            arg.names.without.default.value = arg.names[sapply(fn.args, function(val){
                length(val)==1 && !is.list(val) && val==''
                })]
            
            
            #-- Validate arguments supplied in ... --#
            supplied.args = list(...)
            supplied.arg.names = names(supplied.args)
            
            if (length(supplied.arg.names)>0)
            {
                if (is.null(supplied.arg.names) || any(supplied.arg.names==''))
                    stop(paste0(error.prefix,
                                "The extra arguments for '",
                                fn.name, 
                                "' passed to ... must be named"))
                
                disallowed.arguments = intersect(supplied.arg.names, arguments.to.be.supplied.later)
                if (length(disallowed.arguments)>0)
                    stop(paste0(error.prefix,
                                "The extra arguments for '", fn.name,
                                "' pased to ... cannot contain ",
                                ifelse(length(disallowed.arguments)==1, "an argument", "arguments"),
                                " named ",
                                collapse.with.or("'", disallowed.arguments, "'")))
            }
            
            if (all(arg.names!='...'))
            {
                surplus.arg.names = setdiff(supplied.arg.names, arg.names)
                if (length(surplus.arg.names)>0)
                    stop(paste0(error.prefix,
                                "Extra argument(s) - ",
                                paste0("'", surplus.arg.names, "'", collapse=', '),
                                " were supplied, but are not used by '",
                                fn.name, "'"))
            }
            
            #-- Check for missing arguments --#
            missing.arg.names = setdiff(arg.names.without.default.value,
                                        union(supplied.arg.names, 
                                              union('...', arguments.to.be.supplied.later)))
            
            if (require.all.arguments.up.front && length(missing.arg.names)>0)
            {
                stop(paste0(error.prefix,
                            "'",
                            fn.name, 
                            "' requires ",
                            ifelse(length(missing.arg.names)==1, "argument ", "arguments "),
                            collapse.with.and("'", missing.arg.names, "'"),
                            " but ",
                            ifelse(length(missing.arg.names)==1, 
                                   "this argument is", 
                                   "these arguments are"),
                            " not present in '...'"))
            }
            
            #-- Check for extra arguments --#
            if (!is.null(allowed.arguments.for.later))
            {
                disallowed.arguments = setdiff(arg.names.without.default.value,
                                               union(supplied.arg.names, allowed.arguments.for.later))
                
                if (length(disallowed.arguments)>0)
                {
                    if (length(allowed.arguments.for.later)==0)
                        allowed.text = "we are not allowing arguments without default values to be passed later"
                    else
                        allowed.text = paste0("we are only allowing ",
                                              ifelse(length(allowed.arguments.for.later)==1, "argument ", "arguments "),
                                              collapse.with.and("'", allowed.arguments.for.later, "'"),
                                              " to be passed later without a default value")
                    stop(paste0(error.prefix,
                                fn.name, "() takes ",
                                ifelse(length(disallowed.arguments)==1, 'argument ', 'arguments '),
                                collapse.with.and("'", disallowed.arguments, "'"),
                                " which have not been given default values or values in ..., but ",
                                allowed.text))
                }
            }
            
            
            #-- Store Values --#
            
            private$i.fn = fn
            private$i.fn.name = fn.name
            
            private$i.argument.names = arg.names
            private$i.argument.names.without.default = arg.names.without.default.value
            private$i.supplied.argument.values = supplied.args
            private$i.missing.argument.names = missing.arg.names
                  
            private$i.throw.error.if.missing.arguments = throw.error.if.missing.arguments.at.execute
        },
        
        execute = function(bindings, ..., error.prefix='')
        {
            #-- Validate ... (must be named) --#
            dot.dot.dot = list(...)
            if (length(dot.dot.dot)>0 && is.null(names(dot.dot.dot)))
                stop(paste0(error.prefix, "The ... arguments passed to execute() must be named"))
            
            #-- Validate bindings (must be a named list) --#
            if (!missing(bindings) && length(bindings)>0)
            {
                if (!is.list(bindings))
                    stop(paste0(error.prefix, "'bindings' passed to execute() must be a list"))
                if (is.null(names(bindings)))
                    stop(paste0(error.prefix, "'bindings' passed to execute() must be a NAMED list"))
            }
            
            #-- Set up args list --#
            args = private$i.supplied.argument.values
            
            if (any(private$i.argument.names=='...'))
                dot.dot.dot.names.to.include = names(dot.dot.dot)
            else
                dot.dot.dot.names.to.include = intersect(names(dot.dot.dot), private$i.argument.names)
            
            args[dot.dot.dot.names.to.include] = dot.dot.dot[dot.dot.dot.names.to.include]
            
            if (!missing(bindings) && length(bindings)>0)
            {
                if (any(private$i.argument.names=='...'))
                    binding.names.to.include = names(bindings)
                else
                    binding.names.to.include = intersect(names(bindings), private$i.argument.names)
                
                args[binding.names.to.include] = bindings[binding.names.to.include]
            }
            
            #-- Check for missing arguments --#
            if (private$i.throw.error.if.missing.arguments)
            {
                missing.arguments = setdiff(private$i.argument.names.without.default, union('...', names(args)))
                if (length(missing.arguments)>0)
                    stop(paste0(error.prefix,
                                "Cannot execute function '", private$i.fn.name, "' - missing ",
                                ifelse(length(missing.arguments)==1, "argument", "arguments"),
                                " ", collapse.with.and("'", missing.arguments, "'")))
            }
            
            #-- Call the function --#
            do.call(what=private$i.fn,
                    args=args)
        }
    ),
    
    active = list(
        missing.argument.names = function(value)
        {
            if (missing(value))
                private$i.missing.argument.names
            else
                stop("Cannot modify 'missing.argument.names' value - it is read-only")
        },
        
        value.function.name = function(value)
        {
            if (missing(value))
                private$i.fn.name
            else
                stop("Cannot modify 'value.function.name' value - it is read-only")
        }
    ),
    
    private = list(
        
        i.fn = NULL,
        i.fn.name = NULL,
        
        i.argument.names = NULL,
        i.argument.names.without.default = NULL,
        i.supplied.argument.values = NULL,
        i.missing.argument.names = NULL,
        
        i.throw.error.if.missing.arguments = NULL
    )
)

##-------------##
##-- HELPERS --##
##-------------##

get.function.names.in.expr <- function(ex)
{
    setdiff(all.vars(ex, functions = T), all.vars(ex, functions = F))
}

get.function.argument.names <- function(fn, exclude.arguments.with.default.values=F)
{
    fn.args = formals(args(fn))
    arg.names = names(fn.args)
    if (exclude.arguments.with.default.values)
        arg.names[sapply(fn.args, function(val){val==''})]
    else
        arg.names
}