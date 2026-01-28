
## Code to provide some insight (introspection into model compilation) ##

get.jheem.debugger <- function(version=NULL)
{
    if (is.null(version))
    {
        if (is.null(TRACK.LATEST.COMPILED.SPECIFICATION$specification))
            stop("There has been no call to register.model.specification() that would generate a compiler")
        
        JHEEM.DEBUGGER$new(TRACK.LATEST.COMPILED.SPECIFICATION$specification)
    }
    else
    {
        if (!is.character(version) || length(version)!=1 || is.na(version))
            stop("'version' must be a single, non-NA character value")
        
        if (!version.has.been.registered(version))
            stop(paste0("No specification has been registered for version '", version, "'"))
        
        spec = get.compiled.specification.for.version(version)
        JHEEM.DEBUGGER$new(spec)
    }
    
}

TRACK.LATEST.COMPILED.SPECIFICATION = new.env()


JHEEM.DEBUGGER = R6::R6Class(
    classname = 'jheem.debugger',
    portable = F,
    class = F,
    
    public = list(
        
        initialize = function(compiled.specification)
        {
            # if (!is(compiled.specification, 'jheem.compiled.specification'))
            #     stop("Cannot create jheem.compiler - 'compiled.specification' must be an object of class 'jheem.compiled.specification")
            
            private$i.compiled.specification = compiled.specification
        },
        
        get.quantity.dim.names = function(quantity.name)
        {
            quant = private$i.compiled.specification$get.quantity(quantity.name)
            if (is.null(quant))
            {
                stop(paste0("'", quantity.name, 
                            "' is not a valid quantity name for the '", self$version, "' specification"))
            }
            
            quant$max.dim.names
        },
        
        get.outcome.dim.names = function(outcome.name)
        {
            outcome = private$i.compiled.specification$get.outcome(outcome.name)
            if (is.null(outcome))
            {
                stop(paste0("'", outcome.name, 
                            "' is not a valid outcome name for the '", self$version, "' specification"))
            }
            
            outcome$dim.names
        },
        
        get.quantity.ancestors = function(quantity.names)
        {
            if (!is.character(quantity.names) || length(quantity.names)==0 || any(is.na(quantity.names)))
                stop("'quantity.names' must be a non-empty character vector with no NA values")
            
            flat = lapply(quantity.names, function(name){list()})
            names(flat) = quantity.names
            
            to.search = quantity.names
            
            while(length(to.search)>0)
            {
                to.search.child = to.search[1]
                to.search = to.search[-1]
                
                parent.names = self$get.quantity.parents(to.search.child)
                new.parent.names = setdiff(parent.names, names(flat))
                new.to.flat = lapply(new.parent.names, function(name){character()})
                names(new.to.flat) = new.parent.names
                
                flat = c(flat, new.to.flat)
                to.search = union(to.search, parent.names)
                
                for (parent.name in parent.names)
                    flat[[parent.name]] = c(flat[[parent.name]], to.search.child)
            }
            
            top.level.names = setdiff(names(flat), unlist(flat))
            flat.as.list = lapply(flat, function(elem){
                list()
            })
            
            current.leaf.mask = sapply(flat, length)==0
            
            while (length(flat)>0)
            {
                current.leaf.names = names(flat)[current.leaf.mask]
                current.non.leaf.names = setdiff(names(flat.as.list), current.leaf.names)
                
                flat.as.list[current.non.leaf.names] = lapply(current.non.leaf.names, function(node.name){
                    node = flat.as.list[[node.name]]
                    to.overwrite = intersect(current.leaf.names, flat[[node.name]])
                    node[to.overwrite] = flat.as.list[to.overwrite]
                    node
                })
                
                flat = flat[!current.leaf.mask]
                flat = sapply(flat, function(elem){
                    setdiff(elem, current.leaf.names)
                })
                
                current.leaf.mask = sapply(flat, length)==0
            }
            
            tree = flat.as.list[top.level.names]
            tree
        },
        
        get.quantity.descendants = function(quantity.names)
        {
            if (!is.character(quantity.names) || length(quantity.names)==0 || any(is.na(quantity.names)))
                stop("'quantity.names' must be a non-empty character vector with no NA values")
            
            rv = lapply(quantity.names, function(one.name){
                child.names = self$get.quantity.children(one.name)
                if (length(child.names)==0)
                    list()
                else
                    self$get.quantity.descendants(child.names)
            })  
            
            names(rv) = quantity.names
            rv
        },
        
        print.quantity.ancestor.tree = function(quantity.names, enumerate.depth=F)
        {
            if (!is.logical(enumerate.depth) || length(enumerate.depth) != 1 || is.na(enumerate.depth))
                stop("'enumerate.depth' must be a single, non-NA logical value (ie, TRUE or FALSE)")
            
            tree = self$get.quantity.ancestors(quantity.names)
            private$recursive.print.tree(tree, depth=0, enumerate.depth = enumerate.depth)
        },
        
        print.quantity.descendant.tree = function(quantity.names, enumerate.depth=F)
        {
            if (!is.logical(enumerate.depth) || length(enumerate.depth) != 1 || is.na(enumerate.depth))
                stop("'enumerate.depth' must be a single, non-NA logical value (ie, TRUE or FALSE)")
            
            tree = self$get.quantity.descendants(quantity.names)
            private$recursive.print.tree(tree, depth=0, enumerate.depth = enumerate.depth)
        },
        
        get.quantity.parents = function(quantity.name)
        {
            if (!is.character(quantity.name) || length(quantity.name)!=1 || is.na(quantity.name))
                stop("'quantity.name' must be a single, non-NA character value")
            
            quant = private$i.compiled.specification$get.quantity(quantity.name)
            if (is.null(quant))
            {
                stop(paste0("'", quantity.name, 
                            "' is not a valid quantity name for the '", self$version, "' specification"))
            }
            
            private$i.compiled.specification$quantity.names[
                sapply(private$i.compiled.specification$quantity.names, function(possible.dependee.name){
                    possible.dependent.quant = private$i.compiled.specification$get.quantity(possible.dependee.name)
                    any(possible.dependent.quant$depends.on==quantity.name)
                })
            ]
        },
        
        get.outcome.parents = function(outcome.name)
        {
            if (!is.character(outcome.name) || length(outcome.name)!=1 || is.na(outcome.name))
                stop("'outcome.name' must be a single, non-NA character value")
            
            outcome = private$i.compiled.specification$get.outcome(outcome.name)
            if (is.null(outcome))
            {
                stop(paste0("'", outcome.name, 
                            "' is not a valid outcome name for the '", self$version, "' specification"))
            }
            
            outcome$depends.on.outcomes
        },
        
        get.quantity.children = function(quantity.name)
        {
            if (!is.character(quantity.name) || length(quantity.name)!=1 || is.na(quantity.name))
                stop("'quantity.name' must be a single, non-NA character value")
            
            quant = private$i.compiled.specification$get.quantity(quantity.name)
            if (is.null(quant))
            {
                stop(paste0("'", quantity.name, 
                            "' is not a valid quantity name for the '", self$version, "' specification"))
            }
            
            quant$depends.on
        },
        
        
        get.outcome.children = function(outcome.name)
        {
            stop("need to implement")
            private$i.compiled.specification$quantity.names[
                sapply(private$i.compiled.specification$quantity.names, function(possible.dependee.name){
                    possible.dependent.quant = private$i.compiled.specification$get.quantity(possible.dependee.name)
                    any(possible.dependent.quant$depends.on==quantity.name)
                })
            ]
        }
    ),

    active = list(
        
        version = function(value)
        {
            if (missing(value))
                private$i.compiled.specification$version
            else
                stop(paste0("Cannot modify a jheem.compiler's 'version' - it is read-only"))
        }
    ),
    
    private = list(
        
        i.compiled.specification = NULL,
        
        
        recursive.print.tree = function(tree, depth, 
                                        header='- ', 
                                        spacer= ifelse(enumerate.depth, '   ', '  '), 
                                        enumerate.depth=F)
        {
            indent = paste0(rep(spacer, depth), collapse='')
            if (enumerate.depth)
                header = paste0("(", depth, ") ")
            
            for (i in seq_along(tree))
            {
                cat(paste0(indent, header, names(tree)[i], '\n'))
                private$recursive.print.tree(tree[[i]], depth=depth+1, 
                                             header=header, spacer=spacer,
                                             enumerate.depth = enumerate.depth)
            }
        }
        
    )
    
)