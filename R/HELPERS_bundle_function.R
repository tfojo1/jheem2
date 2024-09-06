

# exporting for now so we can test that this is pulling the right functions from withing the package
#'@export
get.depends.on.functions <- function(fn,
                                     omit.function.names = if (exists('JHEEM2.FUNCTION.NAMES')) JHEEM2.FUNCTION.NAMES else character(),
                                     omit.methods = T,
                                     omit.functions.from.any.package = T,
                                     omit.primitives = T,
                                     omit.explicit.package.calls = T,
                                     recursive = T,
                                     fn.name.for.error = "The function",
                                     only.for.ancestor.environment = .GlobalEnv,
                                     error.prefix = "",
                                     depth = 0 # used for debugging
)
{
    if (!is.null(only.for.ancestor.environment) && !identical(only.for.ancestor.environment, topenv(environment(fn))))
        return (list())
    
    if (omit.primitives && is.primitive(fn))
        return (list())
  
    #-- Prepare a place-holder function name for functions we want to strip out --#
    HOLDER = 'XXXX'
    while (exists(HOLDER))
        HOLDER = paste0(HOLDER, "X")
    HOLDER.PLUS.PAREN = paste0(HOLDER, "(")
    
    #-- Deparse the function body --#
    deparsed = deparse(fn)#deparse(body(fn))

    if (substr(deparsed[1],1,5)=='.Call') # it's a cpp function
        return (list())
    
    deparsed = paste0(deparsed, collapse="\n")

    #-- Sub in the placeholder for calls like <pkg>::<fn>    
    if (omit.explicit.package.calls)
      deparsed = gsub("[a-zA-Z0-9_.]+::[a-zA-Z0-9_.]+\\(", HOLDER.PLUS.PAREN, deparsed)
    
    if (omit.methods)
       deparsed = gsub("[a-zA-Z0-9_.]+\\$[a-zA-Z0-9_.$]+\\(", HOLDER.PLUS.PAREN, deparsed)
    

    #-- Re-parse the text and extract function names --#
    reparsed = parse(text = deparsed)
    
    fn.names = setdiff(all.vars(reparsed, functions=T),
                       all.vars(reparsed, functions = F))
    fn.names = setdiff(fn.names, HOLDER)
    fn.names = setdiff(fn.names, omit.function.names)
    
    #-- Start omitting stuff --#
        
    if (omit.functions.from.any.package)
    {
        from = sapply(fn.names, find)
        from.package.mask = grepl("^package:", from)
        fn.names = fn.names[!from.package.mask]
    }
    
    for (name in fn.names)
    {
        if (!exists(name, where=environment(fn)))
        {
            stop(paste0(error.prefix, 
                        fn.name.for.error,
                        " depends on function ",
                        name, "() - but this function has not been defined"))
        }
    }
    
    fns = lapply(fn.names, get, pos=environment(fn))
    names(fns) = fn.names
    
    if (omit.primitives && !omit.functions.from.any.package)
    {
        primitive.mask = sapply(fns, is.primitive)
        fns = fns[!primitive.mask]
    }
    
    #-- Recurse --#
    
    if (recursive)
    {
        for (i in seq_along(fns))
        {
            sub.fn = fns[[i]]
            #print(paste0("Recurse on ", names(fns)[i]))
            
            if (!is.primitive(sub.fn))
            {
                fns = c(fns,
                        get.depends.on.functions(sub.fn,
                                                 omit.function.names = c(omit.function.names, names(fns)),
                                                 omit.methods = omit.methods,
                                                 omit.functions.from.any.package = omit.functions.from.any.package,
                                                 omit.primitives = omit.primitives,
                                                 omit.explicit.package.calls = omit.explicit.package.calls,
                                                 recursive = T,
                                                 fn.name.for.error = names(fns)[i],
                                                 error.prefix = error.prefix,
                                                 depth = depth+1))
            }
        }
    }
    
    #-- Return --#
    fns
}

bundle.function.and.dependees <- function(fn,
                                          parent.environment,
                                          fn.name.for.error = NULL,
                                          error.prefix = '')
{
    if (is.null(fn.name.for.error))
        fn.name.for.error = deparse(substitute(fn))
    
    new.fn = fn
    dependee.fns = get.depends.on.functions(fn,
                                            fn.name.for.error = fn.name.for.error,
                                            error.prefix = error.prefix)
    
    env = new.env(parent = parent.environment)
    
    environment(new.fn) = env
    for (i in seq_along(dependee.fns))
    {
        one.dependee = dependee.fns[[i]]
        dependee.name = names(dependee.fns)[i]
        
        environment(one.dependee) = env
        env[[dependee.name]] = one.dependee
    }
    
    new.fn
}
