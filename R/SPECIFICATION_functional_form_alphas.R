
##-----------------------------##
##-----------------------------##
##-- ALPHAS CLASS DEFINITION --##
##-----------------------------##
##-----------------------------##

# A bit about the internal structure of 'alphas' objects
#
# First, there are some metadata elements (derived from the functional form or set in the constructor):
# $name - a name field
# $maximum.dim.names - the maximum set of dimnames that the functional form value associated with these alphas will be constrained produce.
#                      this is used for checking dimensions and dimension values for values as they are set to the alphas
#                      If this is NULL, then the only constraint on dimension values is that, if they come from a dimension
#                       in the functional form's minimum.dim.names, they must be one of the dimension values in minimum.dim.names for that dimension
# $minimum.dim.names - the minimum set of dimnames that the functional form value associated with these alphas must produce
# $is.additive - whether alphas should ADD to betas in the functional form (if is.additive==T) vs OVERWRITE them (if is.additive==F)
#                If is.additive=F, then there are additional constraints on what dimensions/dimension values can get an alpha value
#                (there cannot be overlapping dimension values which would give two different alpha values)
# $link - the link object for the scale at which alphas should be additive. We assume that alpha values come in at some scale,
#         then use link to transform them before storing them internally (and subsequently adding them)
#
# Then there are three work-horse elements that hold the actual values: all.effect, main.effects, and interaction.effects
# $all.effect is a single numeric value, the value of an alpha that applies to all elements in the values produced by the functional form which uses these alphas
# 
# $main.effects is a named list, where the names correspond to dimensions
#               each element of main effects has two elements
#               $values - is numeric vector containing alpha values 
#               $dim.values - is a list (containing either single character or numeric values) that give the dimension values to which the corresponding values applies
#               A few notes:
#               1) If is.additive==F, main.effects can only have a single element (values for a single dimension), and ONLY if there is no all.effect specified
#               2) If maximum.dim.names is not NULL, then names(main.effects) must be a subset of names(maximum.dim.names)
#                  and names(main.effects[[d]]) must be a subset of maximum.dim.names[[d]]
# 
# $interaction.effects is a named list. The names are a concatenation of the dimensions to which the interaction applies
#                      The elements of interaction.effects are themselves lists (objects) with the following elements:
#   $dimensions - the list of dimensions which this interaction involves (note, the name of the element in interaction.effects is paste0(sort(dimensions), collapse='_') )
#   $n.dim - the number of dimensions involved (ie, length(dimensions))
#   $values - a numeric vector of interaction terms, with one element for each interaction term
#   $dim.values - a list of lists, with one element for each dimension in $dimensions
#                   $dim.values[[d]][[i]] is dimension value for dimension d that corresponds to the ith interaction term in values.
#                   It can either be a single character value or a single numeric value (representing the index into dimension d)
#       (Note, both values and the vectors of dim.values[[d]] are named with a concatenation of the dim.values for the ith term
#       ie names(values)[i] is the same as names(dim.values[[d]])[i], and both are set to paste0(sapply(dimensions, function(d){dim.values[[d]][i]}), collapse='_') )
#
# Lastly, there is an element for caching calculated mappings (to make producing values using the alphas faster):
# $crunched - itself a list with several elements
#   $dim.names - the dim.names of the values to be produced by the functional.form using these alphas based on which mappings were pre-calculated
#                (if we ask to produce a value with different dim.names, we have to redo our mappings)
#   $all.alpha - the alpha value (if there is one) that applies to all dimensions/values
#   $access.indices - indices into the produced value array that should be added to (or overwritten) by alpha values
#   $mapping.indices - indices into a vector array of alpha values such that produced-value[access.indices[i]] should be modified by alpha-values-vector[mapping.indices[i]]
#                      (the order of the corresponding vector of alpha values is main-effect alphas except for the 'all' alpha, 
#                       then interaction alphas in the order in which they appear in $interaction.effects)
#                       
#                       
# NB: We don't make these R6 objects to avoid deep copy issues in jheem engine objects)


##---------------------##
##-- THE CONSTRUCTOR --##
##---------------------##

create.functional.form.alphas <- function(functional.form,
                                          name,
                                          maximum.dim.names,
                                          check.consistency=T,
                                          error.prefix='')
{
    if (check.consistency)
    {
        if (!is(functional.form, 'functional.form'))
            stop(paste0(error.prefix, "'functional.form' must be an object of class 'functional.form'"))
        
        if (!is.character(name) || length(name)!=1 || is.na(name))
            stop(paste0(error.prefix, "'name' must be a single, non-NA character value"))
        
        if (all(functional.form$alpha.names!=name))
            stop(paste0(error.prefix,
                        "'", name, "' is not a valid name for an alpha for the given functional.form (Must be one of ",
                        paste0("'", functional.form$alpha.names, "'", collapse=', '), ")"))
        
        check.dim.names.valid(maximum.dim.names,
                              variable.name.for.error='maximum.dim.names',
                              error.prefix=error.prefix,
                              allow.empty = T,
                              allow.duplicate.values.across.dimensions = T)
        
        if (!is.null(maximum.dim.names))
        {
            dimensions.missing.from.max = setdiff(names(functional.form$minimum.dim.names), names(maximum.dim.names))
            if (length(dimensions.missing.from.max)>0)
                stop(paste0(error.prefix,
                            "max.dim.names is missing the following ",
                            ifelse(length(dimensions.missing.from.max)==1, "dimension, which is", "dimensions, which are"),
                            " built in to the given functional form: ",
                            collapse.with.and("'", dimensions.missing.from.max, "'")))
            
            overlapping.dimensions = intersect(names(maximum.dim.names), names(functional.form$minimum.dim.names))
            sapply(overlapping.dimensions, function(d){
                if (!setequal(maximum.dim.names[[d]], functional.form$minimum.dim.names[[d]]))
                    stop(paste0(error.prefix,
                                "maximum.dim.names[['", d, "']] given to the alphas object (",
                                paste0("'", maximum.dim.names[[d]], "'", collapse=', '),
                                ") does not match minimum.dim.names[['", d,
                                "']] for the functional form associated with the alphas (",
                                paste0("'", functional.form$minimum.dim.names[[d]], "'", collapse=', '), ")"))
            })
        }
    }
    
    rv = list(
        name=name,
        maximum.dim.names=maximum.dim.names,
        minimum.dim.names=functional.form$minimum.dim.names,
        main.effects=list(),
        interaction.effects=list(),
        link = functional.form$alpha.links[[name]],
        is.additive = functional.form$alphas.are.additive[name]
    )
    
    class(rv) = 'functional.form.alphas'
    rv
}


##----------------------------------------##
##-- FUNCTIONS for SETTING ALPHA VALUES --##
##----------------------------------------##

set.alpha.main.effect.values <- function(alphas,
                                         dimensions,
                                         dimension.values,
                                         values,
                                         check.consistency=T,
                                         error.prefix='')
{
    # Prep the values and arguments
    dimensions.and.values = prep.dimensions.and.values.for.alphas(alphas=alphas,
                                                                  dimensions=dimensions,
                                                                  dimension.values=dimension.values,
                                                                  values=values,
                                                                  check.consistency=check.consistency,
                                                                  error.prefix=error.prefix,
                                                                  is.interaction=F)
    
    orig = list(alphas=alphas,
                dimensions=dimensions,
                dimension.values=dimension.values,
                values=values)
    
    dimensions = dimensions.and.values$dimensions
    dimension.values = dimensions.and.values$dimension.values
    values = dimensions.and.values$values
    
    unique.dimensions = unique(dimensions)

    if (check.consistency)
    {   
        if (!is.null(alphas$maximum.dim.names))
        {
            invalid.dimensions = setdiff(unique.dimensions, c('all', names(alphas$maximum.dim.names)))
            if (length(invalid.dimensions)>0)
                stop(paste0(error.prefix, 
                            collapse.with.and("'", invalid.dimensions, "'"),
                            ifelse(length(invalid.dimensions)==1, 
                                   " is not a valid dimension",
                                   " are not valid dimensions"),
                            " for alphas '", alphas$name, "' (must be one of ",
                            collapse.with.or("'", names(alphas$maximum.dim.names), "'")))
        }
        
        if (!alphas$is.additive)
        {
            if (length(unique.dimensions)>1)
                stop(paste0(error.prefix, "For alphas '", alphas$name, 
                            "' that overwrites its betas, only one dimension can be used for main effect values. (",
                            collapse.with.and("'", unique.dimensions, "'"), " were specified)"))
            if (length(alphas$main.effects)>0 && any(names(alphas$main.effects)!=unique.dimensions))
                stop(paste0(error.prefix, "For alphas '",
                            alphas$name, "' that overwrites its betas, only one dimension can be used for main effect values. ",
                            "Attempting to use dimension '", unique.dimensions, "', but have previously used ",
                            paste0("'", names(alphas$main.effects), "'", collapse=', ')))
            if (!is.null(alphas$all.effect) && any(unique.dimensions!='all'))
                stop(paste0(error.prefix, "For alphas '",
                            alphas$name, "' that overwrites its betas, only one dimension can be used for main effect values. ",
                            "Attempting to use dimension '", unique.dimensions, "', but have previously used an alpha for 'all' ",
                            "(Cannot set an all alpha AND other main effect alphas when overwriting values)"))
            
            if (length(alphas$interaction.effects)>0)
            {
                if (any(dimensions=='all'))
                    stop(paste0(error.prefix, "When attempting to set 'all' effect for alphas '",
                                alphas$name, "' that overwrites its betas, interaction alphas were previously set. Cannot set both an effect for 'all' AND interaction effects"))

                for (one.interaction in alphas$interaction.effects)
                {
                    # If we made it past the error checks above, we know that unique.dimensions actually has just a single value
                    if (all(one.interaction$dimensions != unique.dimensions) )
                        stop(paste0(error.prefix, "For alphas '",
                                    alphas$name, "' that overwrites its betas, dimension values for main effects must share a dimension (but not overlap dimension values) with any previously set interaction effects. ",
                                    " Previously set interaction effects did not include dimension '", unique.dimensions, "'"))
                        
                    overlapping.values = intersect(one.interaction$dim.values[[unique.dimensions]], dimension.values)
                    if (length(overlapping.values)>0)
                        stop(paste0(error.prefix, "For alphas '",
                                    alphas$name, "' that overwrites its betas, dimension values for main effects cannot overlap with previously set values for interaction effects. ",
                                    ifelse(length(overlapping.values)==1, "Value ", "Values "),
                                    collapse.with.and("'", overlapping.values, "'"),
                                    ifelse(length(overlapping.values)==1, " was", " were"),
                                    " previously set for an interaction effect for dimension '", unique.dimensions, "'"))
                }
            }
        }
    }
    
    # Sort the dimensions according to the order they appear in max or min.dim.names
    if (is.null(alphas$maximum.dim.names))
        unique.dimensions = intersect(names(alphas$maximum.dim.names), unique.dimensions)
    else
        unique.dimensions = c(intersect(names(alphas$minimum.dim.names), unique.dimensions),
                              sort(setdiff(unique.dimensions, names(alphas$minimum.dim.names))))
    
    # Set the 'all' value
    # (and remove it from our lists of things to add to main effects)
    if (any(unique.dimensions=='all'))
    {
        alphas$all.effect = as.numeric(values[dimensions=='all'][1])
        mask = dimensions != 'all'
        
        dimensions = dimensions[mask]
        dimension.values = dimension.values[mask]
        values = values[mask]
        unique.dimensions = setdiff(unique.dimensions, 'all')
    }
    
    # Set the values (besides 'all')
    for (d in unique.dimensions)
    {
        mask = dimensions == d
        
        # Set up the object if it has not been set up previously
        if (is.null(alphas$main.effects[[d]]))
            alphas$main.effects[[d]] = list(values=numeric(),
                                            dim.values=list())
        
        # Store the length so that we can check below if we have added new values
        length.before.adding = length(alphas$main.effects[[d]]$values)
        
        # Store the values
        value.names = as.character(dimension.values[mask])
        alphas$main.effects[[d]]$values[value.names] = values[mask]
        alphas$main.effects[[d]]$dimension.values[value.names] = dimension.values[mask]
        
        # If we added any value for a new dimension.value, clear the crunched indices
        if (length(alphas$main.effects[[d]]$values) != length.before.adding)
            alphas$crunched = NULL
    }
    
    # Return the updated object
    alphas
}

set.alpha.interaction.value <- function(alphas,
                                         dimensions,
                                         dimension.values,
                                         value,
                                         check.consistency=T,
                                         error.prefix='')
{
    # Prep the values and arguments
    dimensions.and.values = prep.dimensions.and.values.for.alphas(alphas=alphas,
                                                                  dimensions=dimensions,
                                                                  dimension.values=dimension.values,
                                                                  values=value,
                                                                  check.consistency=check.consistency,
                                                                  error.prefix=error.prefix,
                                                                  is.interaction=T)
    dimensions = dimensions.and.values$dimensions
    dimension.values = dimensions.and.values$dimension.values
    value = dimensions.and.values$values
    
    # Identify the dimensions involved here
    unique.dimensions = unique(dimensions)
    if (check.consistency)
    {
        invalid.dimensions = setdiff(dimensions, names(alphas$maximum.dim.names))
        if (length(invalid.dimensions)>0)
            stop(paste0(error.prefix, 
                        collapse.with.and("'", invalid.dimensions, "'"),
                        ifelse(length(invalid.dimensions)==1, 
                               " is not a valid dimension",
                               " are not valid dimensions"),
                        " for alphas '", alphas$name, "'"))
        
        if (length(unique.dimensions)<2)
            stop(paste0(error.prefix,
                        "For interaction alphas, there must be at least two dimensions specified. Tried to set interaction for one dimension ('",
                        unique.dimensions, "') for alphas '", alphas$name, "'"))
        if (length(unique.dimensions)>4)
            stop(paste0(error.prefix,
                        "At the moment, we can only handle two, three, or four dimensions for interaction alphas. Tried to set interaction for dimensions ",
                        collapse.with.and("'", unique.dimensions, "'"),
                        " for alphas '", alphas$name, "'"))
    }
    
    # Sort the dimensions according to the order they appear in max or min.dim.names
    if (is.null(alphas$maximum.dim.names))
        unique.dimensions = intersect(names(alphas$maximum.dim.names), unique.dimensions)
    else
        unique.dimensions = c(intersect(names(alphas$minimum.dim.names), unique.dimensions),
                              sort(setdiff(unique.dimensions, names(alphas$minimum.dim.names))))
    
    
    # Set up the dim values (every combo of the values for each dimension)
    sorted.dim.values = lapply(unique.dimensions, function(dim){
        dimension.values[dimensions==dim]
    })
    
    n.dim = length(unique.dimensions)
    n.values.per.dim = sapply(sorted.dim.values, length)
    
    n.values.before.dim = c(1, cumprod(n.values.per.dim[-n.dim]))
    n.values.after.dim = prod(n.values.per.dim) / n.values.before.dim / n.values.per.dim
    
    iterated.dim.values = lapply(1:n.dim, function(d){
        rep(rep(sorted.dim.values[[d]], n.values.after.dim[d]), each=n.values.before.dim[d])
    })
    
    n.iterated.dim.values = length(iterated.dim.values[[1]])
    
    
    # Get the name for the interaction (based on involved dimensions)
    interaction.name = paste0(unique.dimensions, collapse='_')
    
    # Create the skeleton holder to store info in (if not already present)
    if (is.null(alphas$interaction.effects[[interaction.name]]))
    {
        alphas$interaction.effects[[interaction.name]] = list(
            dimensions = unique.dimensions,
            n.dim = n.dim,
            dim.values = lapply(1:n.dim, function(i){character()}),
            values = numeric()
        )
        names(alphas$interaction.effects[[interaction.name]]$dim.values) = unique.dimensions
    }
    
    # Get the names we will use to identify new values (so we can overwrite if previously set)
    value.names = sapply(1:n.iterated.dim.values, function(i){
        paste0(sapply(iterated.dim.values, function(dv){
            dv[i]
        }), collapse="_") 
    })
    
    # Store the dim.values
    for (i in 1:n.dim)
        alphas$interaction.effects[[interaction.name]]$dim.values[[i]][value.names] = iterated.dim.values[[i]]
    
    
    # Store the length so that we can check below if we have added a new value
    length.before.adding = length(alphas$interaction.effects[[interaction.name]]$values)
    
    # Store the value
    alphas$interaction.effects[[interaction.name]]$values[value.names] = value
    
    # If we added any value for a new dimension.value, clear the crunched indices
    if (length(alphas$interaction.effects[[interaction.name]]$values) != length.before.adding)
        alphas$crunched = NULL
    
    # Check if non-additive
    if (!alphas$is.additive && check.consistency)
    {
        # Check for overlaps with main effects
        if (length(alphas$main.effects)>0)
        {
            if (!is.null(alphas$all.effect))
                stop(paste0(error.prefix, "When attempting to set interaction effects for alphas '",
                            alphas$name, "' that overwrites its betas, an alpha for 'all' was previously set. Cannot set both an effect for 'all' AND interaction effects"))
            
            # If additive, we know that there is at most one main effect
            if (all(names(alphas$main.effects)[1]!=unique.dimensions))
                stop(paste0(error.prefix, "For alphas '",
                            alphas$name, "' that overwrites its betas, dimension values for interaction effects must share a dimension (but not overlap dimension values) with previously set main effects. ",
                            " Previously set main effects did not include dimensions ",
                            collapse.with.or("'", unique.dimensions, "'")))
            
            overlapping.values = intersect(alphas$main.effects[[1]]$dim.values, dimension.values)
            if (length(overlapping.values)>0)
                stop(paste0(error.prefix, "For alphas '",
                            alphas$name, "' that overwrites its betas, dimension values for interaction effects cannot overlap with previously set values for main effects. ",
                            ifelse(length(overlapping.values)==1, "Value ", "Values "),
                            collapse.with.and("'", overlapping.values, "'"),
                            ifelse(length(overlapping.values)==1, " was", " were"),
                            " previously set for a main effect for dimension '", names(alphas$main.effects)[1], "'"))
        }        

        
        one.interaction = alphas$interaction.effects[[interaction.name]]
        for (other.interaction in alphas$interaction.effects[names(alphas$interaction.effects)!=interaction.name])
        {
#            if (length(setdiff(one.interaction$dimensions, other.interaction$dimensions))>0 &&
#                length(setdiff(other.interaction$dimensions, one.interaction$dimensions))>0)
#                stop(paste0(error.prefix, "For alphas '", alphas$name, 
#                            "' that overwrites it betas, interaction effects must all involve the same set of dimensions. Attempted to set an interaction effect on dimensions ",
#                            collapse.with.and("'", one.interaction$dimensions, "'"),
#                            " but previously set interaction effects on ",
#                            collapse.with.and("'", other.interaction$dimensions, "'")))

            overlapping.dimensions = intersect(one.interaction$dimensions, other.interaction$dimensions)
            
            if (length(overlapping.dimensions)==0)
                stop(paste0(error.prefix, "For alphas '",
                            alphas$name, "' that overwrites its betas, dimension values for interaction effects must share a dimension (but not overlap dimension values) with any previously set interaction effects. This interaction effect does not overlap with a previous interaction effect"))
            
            dim.value.sets.1 = sapply(1:length(one.interaction$values), function(i){
                paste0(sapply(one.interaction$dim.values[overlapping.dimensions], function(dv){
                    dv[i]
                }), collapse='_')
            })
            dim.value.sets.2 = sapply(1:length(other.interaction$values), function(i){
                paste0(sapply(other.interaction$dim.values[overlapping.dimensions], function(dv){
                    dv[i]
                }), collapse='_')
            })
            
            if (length(intersect(dim.value.sets.1, dim.value.sets.2))>0)
                stop(paste0(error.prefix, "For alphas '", alphas$name, 
                            "' that overwrites it betas, interaction effects cannot have overlapping dimension values."))
        }
        
    }
    
    # Return the updated object
    alphas
}



prep.dimensions.and.values.for.alphas <- function(alphas,
                                                  dimensions,
                                                  dimension.values,
                                                  values,
                                                  check.consistency=T,
                                                  error.prefix='',
                                                  is.interaction)
{
    if (!is(alphas, 'functional.form.alphas'))
        stop(paste0(error.prefix, "'alphas' must be an object of class 'functional.form.alphas'"))
    
    #-- Make it so that dimension values is a list with each element a single value (numeric or character) --#
    if (is.list(dimension.values))
    {
        if (any(sapply(dimension.values, length)!=1))
        {
            # Repeat the dimensions so that there is one dimension value for each element in dimension.values
            if (length(dimension.values)==length(dimensions))
                dimensions = unlist(sapply(1:length(dimensions), function(i){
                    rep(dimensions[i], length(dimension.values[[i]]))
                }))
            
            # 'unlist' the dimension values
            # (the result still has to be a list, because it can contain either numeric or character values,
            #  but there will be just one value per element in dimension.values)
            orig.dimension.values = dimension.values
            dimension.values = list()
            for (elem in orig.dimension.values)
                dimension.values = c(dimension.values,
                                     as.list(elem))
        }
    }    
    else    
        dimension.values = as.list(dimension.values)
    
    #-- Fill in missing dimensions --#
    if (length(dimensions)==0)
    {   
        if (any(sapply(dimension.values, is.numeric)))
            stop(paste0(error.prefix, 
                        "In setting alphas, if numeric values are passed as dimension.values, 'dimensions' must be explicitly specified for each dimension value"))
        
        if (is.null(alphas$maximum.dim.names))
            dimensions = get.dimension.for.values(alphas$minimum.dim.names, values=dimension.values)
        else
            dimensions = get.dimension.for.values(alphas$maximum.dim.names, values=dimension.values)
        
        dimensions[is.na(dimensions) && sapply(dimension.values, function(dv){dv=='all'})] = 'all'
        
        # If we couldn't figure out a dimension, throw an error
        if (any(is.na(dimensions)))
        {
            if (is.null(alphas$maximum.dim.names))
                stop(paste0(error.prefix,
                            "Cannot identify '",
                            ifelse(sum(is.na(dimensions))==1, "dimension for dimension value ", "dimensions for dimension values "),
                            collapse.with.or("'", dimension.values[is.na(dimensions)], "'"),
                            " for alphas ", alphas$name))
            else
                stop(paste0(error.prefix,
                            "Invalid dimension ",
                            ifelse(sum(is.na(dimensions))==1, "value", "values"),
                            " for alphas '", alphas$name, "' : ",
                            collapse.with.and("'", dimension.values[is.na(dimensions)], "'")))
        }
    }
    
    #-- If there is only a single value for dimensions, it applies to all dimension values --#
    if (length(dimensions)==1)
        dimensions = rep(dimensions, length(dimension.values))
    
    if (check.consistency)
    {
        if (length(dimensions) != length(dimension.values))
            stop(paste0(error.prefix,
                        "'dimensions' must either be a single value, or the same length as 'dimension.values"))
        
        # Check for invalid dimensions (we cannot say dimensions are invalid if alphas$maximum.dim.names is not set)
        if (!is.null(alphas$maximum.dim.names))
        {
            invalid.dimensions = setdiff(dimensions, c('all', names(alphas$maximum.dim.names)))
            
            if (length(invalid.dimensions)>0)
                stop(paste0(error.prefix,
                            "Invalid dimension(s): ",
                            paste0("'", invalid.dimensions, "'", collapse=', '),
                            ". dimensions must be one of: ",
                            paste0("'", names(alphas$maximum.dim.names), "'", collapse=', ')))
        }
        
        # Check for invalid dimension values
        errors = sapply(1:length(dimension.values), function(i){
            if (dimensions[[i]]=='all')
                NA
            else if (is.numeric(dimension.values[[i]]))
            {
                if (is.null(alphas$maximum.dim.names))
                {
                    if (any(names(alphas$minimum.dim.names)==dimensions[i]) && 
                        (dimension.values[[i]]<1 || dimension.values[[i]]>length(alphas$minimum.dim.names[[ dimensions[i] ]])))
                        paste0(error.prefix,
                               "integer dimension.values for dimension '", dimensions[[i]],
                               "' must be between 1 and ", length(alphas$maximum.dim.names[[ dimensions[i] ]]))
                    else if (dimension.values[[i]]<1)
                        paste0(error.prefix,
                               "integer dimension.values for dimension '", dimensions[[i]],
                               "' must be greater than or equal to 1")
                    else
                        NA
                }
                else
                {
                    if (dimension.values[[i]]<1 || dimension.values[[i]]>length(alphas$maximum.dim.names[[ dimensions[i] ]]))
                        paste0(error.prefix,
                               "integer dimension.values for dimension '", dimensions[[i]],
                               "' must be between 1 and ", length(alphas$maximum.dim.names[[ dimensions[i] ]]))
                    else
                        NA
                }
            }
            else
            {
                if (is.null(alphas$maximum.dim.names))
                {
                    if (any(names(alphas$minimum.dim.names)==dimensions[i]))
                        invalid.mask = sapply(dimension.values[[i]], function(val){
                            all(val != alphas$maximum.dim.names[[ dimensions[i] ]])
                        })
                    else
                        invalid.mask = logical()
                }
                else
                {
                    invalid.mask = sapply(dimension.values[[i]], function(val){
                        all(val != alphas$maximum.dim.names[[ dimensions[i] ]])
                    })
                }
                
                if (any(invalid.mask))
                    paste0(error.prefix, collapse.with.and("'", dimension.values[[i]][invalid.mask], "'"),
                           ifelse(sum(invalid.mask)==1, "is not a valid value", "are not valid values"),
                           " for dimension '",
                           dimensions[i])
                else
                    NA
            }
        })
        errors = errors[!is.na(errors)]
        
        if (length(errors)>0)
            stop(paste0(error.prefix,
                        "Invalid dimension values\n",
                        paste0("- ", errors, collapse='\n')))
        
        # Make sure values is numeric and non-NA
        if (!is.numeric(values))
            stop(paste0(error.prefix, "'values' must be a numeric vector"))
        if (any(is.na(values)))
            stop(paste0(error.prefix, "'values' cannot contain NA values"))
        
        if (is.interaction)
        {
            if (length(values) != 1)
                stop(paste0(error.prefix,
                            "For an interaction alpha, 'value' must be of length one"))
        }
    }
    
    # this has to be outside the check.consistency if statement because we need to iterate values if length is < dimension.values length
    if (!is.interaction && length(values) != length(dimension.values))
    {
        if (length(values)==1)
            values = rep(values, length(dimension.values))
        else
            stop(paste0(error.prefix,
                        "'values' must be either a single numeric value or have the same length as 'dimension.values'"))
    }
    
    #-- Make sure we can apply the link function --#
    alphas$link$check.untransformed.values(values, variable.name.for.error='values', error.prefix=paste0(error.prefix, "'values' for alphas do not match expected scale - "))
    
    #-- Package up and return --#
    list(dimensions = dimensions,
         dimension.values = dimension.values,
         values = alphas$link$apply(values))    
}

##------------------------------------------------------------##
##-- SOME GETTERS for SETTING UP DIMENSIONS based on ALPHAS --##
##------------------------------------------------------------##

get.alphas.minimum.dimensions <- function(alphas)
{
    if (is.null(alphas$maximum.dim.names))
    {
        rv = union(names(alphas$main.effects),
                   unlist(sapply(alphas$interaction.effects, function(int.effects){
                       int.effects$dimensions
                   })))
        
        union(intersect(rv, names(alphas$minimum.dim.names)), rv) #put the minimum dimensions first, then any extra dimensions
    }
    else
    {
        mask = sapply(names(alphas$maximum.dim.names), function(d){
            any(names(alphas$main.effects)==d) ||
                any(sapply(alphas$interaction.effects, function(int.effects){
                    any(int.effects$dimensions == d)
                }))
        })
        names(alphas$maximum.dim.names)[mask]
    }
}


get.alphas.minimum.dim.names <- function(alphas)
{
    dimensions = get.alphas.minimum.dimensions(alphas)
    
    if (is.null(alphas$maximum.dim.names))
    {
        rv = lapply(dimensions, function(d){
            if (any(d==names(alphas$minimum.dim.names)))
                alphas$minimum.dim.names[[d]]
            else
                union(alphas$main.effects[[d]]$dim.values,
                      unlist(sapply(alphas$interaction.effects, function(int.effects){
                          int.effects$dim.values[[d]]
                      })))
        })
        names(rv) = dimensions
        
        rv
    }
    else
    {
        alphas$maximum.dim.names[dimensions]
    }
}

##-----------------------------------------------##
##--   HYDRATING the ALPHAS up into an ARRAY   --##
##-- (and a function to pre-crunch to do this) --##
##-----------------------------------------------##

# Function to add alphas to betas
incorporate.alphas <- function(betas,
                               alphas,
                               target.dim.names,
                               error.prefix='')
{
    #-- Crunch (in case the target.dim.names have changed on us) --#
    alphas = crunch.alphas(alphas,
                           betas = betas,
                           target.dim.names = target.dim.names,
                           error.prefix = error.prefix)
    
    #-- Expand up the Betas --#
    arr = betas[ alphas$crunched$expand.beta.indices ]
    
    #-- Fold in the 'all' Alpha (if there is one) --#
    if (!is.null(alphas$all.effect))
    {
        if (alphas$is.additive)
            arr = add_scalar_to_arr(dst = arr, to_add = alphas$all.effect)
        else
            arr = overwrite_arr_with_scalar(dst = arr, overwrite_with = alphas$all.effect)
    }
    
    #-- Fold in the Other Alphas --#
    alpha.values = c(unlist(sapply(alphas$main.effects, function(main.effects){
        main.effects$values
    })),
    unlist(sapply(alphas$interaction.effects, function(int.effects){
        int.effects$values
    })))
    
    if (length(alpha.values)>0)
    {
        if (alphas$is.additive)
            arr = add_to_arr(dst = arr,
                                dst_indices = alphas$crunched$access.indices,
                                src = alpha.values,
                                src_indices = alphas$crunched$mapping.indices)
        # ^The above is equivalent to:
        # arr[alphas$crunched$access.indices] = arr[alphas$crunched$access.indices] +
        #                                           alpha.values[alphas$crunched$mapping.indices]
        #   but with Rcpp enabling modify in place without allocating a new array
        else
            arr = overwrite_arr(dst = arr,
                             dst_indices = alphas$crunched$access.indices,
                             src = alpha.values,
                             src_indices = alphas$crunched$mapping.indices)
        # ^The above is equivalent to:
        # arr[alphas$crunched$access.indices] = alpha.values[alphas$crunched$mapping.indices]
        #   but with Rcpp enabling overwrite in place without allocating a new array
    }

    #-- Set dimnames and return --#
    if (length(target.dim.names)>0)
    {
        dim(arr) = sapply(target.dim.names, length)
        dimnames(arr) = target.dim.names
    }
    
    arr
}

# Will only do crunching if needed
#  (ie, won't re-crunch already crunched elements)
crunch.alphas <- function(alphas,
                          betas,
                          target.dim.names,
                          error.prefix)
{
    if (is.null(alphas$crunched))
        alphas$crunched = list()
    
    # Check if we need to crunch for alphas and betas
    if (is.null(alphas$crunched$dim.names) ||
        !dim.names.equal(target.dim.names, alphas$crunched$dim.names,
                         match.order.of.dimensions = T, match.order.within.dimensions = T))
    {
        if (is.list(betas))
            stop(paste0(error.prefix, "'betas' must be an array or numeric scalar"))
        
        # Calculate indices for the betas
        # (note, we assume here that betas are never going to change dimensions on us)
        alphas$crunched$expand.beta.indices = get.expand.array.indices(to.expand.dim.names = dimnames(betas),
                                                                       target.dim.names = target.dim.names)

        # Do the main effects
        dimension.values = list()
        for (eff in alphas$main.effects)
            dimension.values = c(dimension.values, eff$dimension.values)
        if (length(alphas$main.effects)==0)
        {
            alphas$crunched$access.indices = integer()
            alphas$crunched$mapping.indices = integer()
        }
        else
        {
            indices = calculate_main_effect_indices(target_dim_names = target.dim.names,
                                                        alpha_dimensions = unlist(sapply(names(alphas$main.effects), function(d){
                                                            rep(d, length(alphas$main.effects[[d]]$dimension.values))
                                                        })),
                                                        alpha_dim_values = dimension.values)

            # Check for errors in the indices (will have returned a NULL value)
            if (is.null(indices))
                stop(paste0(error.prefix,
                            "There was an error crunching main-effect alphas for '", alphas$name, "'"))
            
            # Fold into 'crunched'
            alphas$crunched$access.indices = indices$access.indices
            alphas$crunched$mapping.indices = indices$mapping.indices
        }
        
        # Do the interaction effects
        n.alpha.values = length(dimension.values)
        for (int in alphas$interaction.effects)
        {
            if (int$n.dim==2)
            {
                indices = calculate_two_way_interaction_indices(target_dim_names = target.dim.names,
                                                                alpha_dimension1 = int$dimensions[1],
                                                                alpha_dim1_values = int$dim.values[[1]],
                                                                alpha_dimension2 = int$dimensions[2],
                                                                alpha_dim2_values = int$dim.values[[2]])
            }
            else if (int$n.dim==3)
            {
                indices = calculate_three_way_interaction_indices(target_dim_names = target.dim.names,
                                                                  alpha_dimension1 = int$dimensions[1],
                                                                  alpha_dim1_values = int$dim.values[[1]],
                                                                  alpha_dimension2 = int$dimensions[2],
                                                                  alpha_dim2_values = int$dim.values[[2]],
                                                                  alpha_dimension3 = int$dimensions[3],
                                                                  alpha_dim3_values = int$dim.values[[3]])
            }
            else if (int$n.dim==4)
            {
                indices = calculate_four_way_interaction_indices(target_dim_names = target.dim.names,
                                                                 alpha_dimension1 = int$dimensions[1],
                                                                 alpha_dim1_values = int$dim.values[[1]],
                                                                 alpha_dimension2 = int$dimensions[2],
                                                                 alpha_dim2_values = int$dim.values[[2]],
                                                                 alpha_dimension3 = int$dimensions[3],
                                                                 alpha_dim3_values = int$dim.values[[3]],
                                                                 alpha_dimension4 = int$dimensions[4],
                                                                 alpha_dim4_values = int$dim.values[[4]])
            }
            else
                stop(paste0(error.prefix, "At this time, we can only handle interactions with 2, 3, or 4 dimensions"))
               
            
            # Check for errors in the indices (will have returned a NULL value)
            if (is.null(indices))
                stop(paste0(error.prefix,
                            "There was an error crunching interaction-effect alphas for '", alphas$name, "' (dimensions ",
                            collapse.with.and("'", int$dimensions, "'"), ")"))
            
            # Fold into 'crunched'
            alphas$crunched$access.indices = c(alphas$crunched$access.indices, indices$access.indices)
            alphas$crunched$mapping.indices = c(alphas$crunched$mapping.indices, 
                                                n.alpha.values + indices$mapping.indices)
            
            n.alpha.values = n.alpha.values + length(int$values)
        }
        
        # Store the dim.names that we crunched for
        alphas$crunched$dim.names = target.dim.names
    }
    
    # Return
    alphas
}


get.dimension.for.values <- function(dim.names, values)
{
    flattened.dim.names = unlist(dim.names)
    flattened.dimensions = unlist(sapply(names(dim.names), function(d){rep(d, length(dim.names[[d]]))}))
    
    sapply(values, function(val){
        flattened.dimensions[flattened.dim.names==val][1]
    })
}