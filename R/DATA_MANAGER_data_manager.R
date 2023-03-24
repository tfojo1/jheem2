
# Depends on:
# R6 package
# HELPERS_misc_helpers
# HELPERS_dim_name_helpers
# HELPERS_array_helpers

##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##

#'@description Create a JHEEM Data Manager
#'
#'@param name The name of the data manager
#'@param description A short description
#'
#'@export
create.data.manager <- function(name,
                                description)
{
    JHEEM.DATA.MANAGER$new(name, 
                           description=description,
                           dimensions=dimensions)
}

#'@description Register a new data ontology to a data manager before putting data to it
#'
#'@param data.manager A jheem.data.manager object
#'@param name The name of the data group
#'@param ont An ontology object as created by ontology()
#'
#'@export
register.data.ontology <- function(data.manager,
                                   name,
                                   ont)
{
    if (!is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.ontology(name=name,
                                   ont=ont)
}

#'@description Register an outcome to a data manager before putting data for that outcome
#'
#'@param data.manager A jheem.data.manager object
#'@param outcome The name (a single character value) of the outcome
#'@param scale The scale for the outcome. Options are 'rate', 'ratio', 'proportion', 'time', 'number', 'non.negative.number'
#'@param denominator.outcome The denominator outcome type that should be used when aggregating data (taking a weighted average) for this outcome type. Must be a previously registered outcome with scale='non.negative.number'. Only applies if scale is 'rate', 'proportion', or 'time'
#'
#'@export
register.data.outcome <- function(data.manager,
                                  outcome,
                                  scale,
                                  denominator.outcome=NULL)
{
    if (!is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.outcome(outcome=outcome,
                                  scale=scale,
                                  denominator.outcome=denominator.outcome)
}

#'@description Put data into a data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param outcome The outcome type for the data. Must be an outcome previously registered with \code{\link{register.data.outcome}}
#'@param ontology.name The name of the ontology which the data follow. Must be an ontology previously registered with \code{\link{register.data.ontology}}
#'@param dimension.values A named list that indicates what subset of a bigger data element these particular data should be stored into. The names of dimension values. The values of dimension.values can be either (1) character vectors
#'@param data A numeric array or scalar value containing the data to store. If it is an array, it must have named dimnames set
#'@param source The (single) character name of the source from which these data derive. Note: a source is conceived such that one source cannot contain two sets of data for the same set of dimension values
#'@param url,details Either a single character value, or a character array, or a list array (a list with dimnames set) where each element is a character vector, denoting the url (a URL for where to reference the data), or data collection details (sub-categorization of the data source). The dimnames of these arguments must be a subset of the dimnames of data
#'
#'@export
put.data <- function(data.manager,
                     outcome,
                     ontology.name,
                     source,
                     dimension.values,
                     data,
                     url,
                     details)
{
    if (!is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$put(outcome=outcome,
                     data.group=data.group,
                     dimension.values=dimension.values,
                     data=data,
                     source=source,
                     url=url,
                     details=details)
}

#'@description Pull data from a data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param outcome The outcome type for the data. Must be an outcome previously registered to this data manager with \code{\link{register.data.outcome}}
#'@param keep.dimensions The dimensions that should be retained in the returned value
#'@param dimension.values A named list, indicating which values for specific dimensions to pull data for
#'@param sources The data sources from which to pull data (if available). If NULL, will pull from all data sources that have any relevant data
#'@param target.ontology Optional argument, indicating the ontology according to which results are desired. The data manager will apply an ontology mapping (if it can) to align the returned array to the desired ontology
#'@param allow.mapping.from.target.ontology A logical indicator. If TRUE, if target.ontology is specified, but the data manager does not have data that can be mapped to target ontology, it will search for data such that an ontology.mapping can be applied to data in the target.ontology that make those data align with the data pulled
#'@param from.ontology.names The names of the ontologies from which to pull data (if available). If NULL, will pull from all ontologies that have any relevant data and can be mapped to the requested ontology. Must refer to ontologies previously registered to this data manager with \code{\link{register.data.ontology}}
#'@param append.attributes A character vector indicating requested data attributes to include with the result. May be either "details", "url", or both
#'
#'@return A numeric array with named dimnames, with one dimension for each value of 'keep.dimensions'.
#' The return value may have several attributes:
#' (1) If target.ontology is not-NULL and allow.mapping.from.target.ontology==T, 'ontology.mapping' with refer to an ontology.mapping object that can be applied to data in the target.ontology to make them align with the returned array
#' (2) If append.attributes includes "url", a list array (a list with dim and dimnames attributes set), with the same dimensions as the returned array, where each element is a character vector of URLs
#' (3) If append.attributes includes "details", a list array (a list with dim and dimnames attributes set), with the same dimensions as the returned array, where each element is a character vector of strings giving the details about data collection
#'
#'@export
pull.data <- function(data.manager,
                      outcome,
                      keep.dimensions = NULL,
                      dimension.values = NULL,
                      sources = NULL,
                      target.ontology = NULL,
                      allow.mapping.from.target.ontology = T,
                      from.ontology.names = NULL,
                      append.attributes = NULL)
{
    if (!is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get(outcome=outcome,
                     keep.dimensions=keep.dimensions,
                     dimension.values=dimension.values,
                     data.group=data.group,
                     type=type)
}



##----------------------##
##-- CLASS DEFINITION --##
##----------------------##



JHEEM.DATA.MANAGER = R6::R6Class(
    'jheem.data.manager',
    
    public = list(
        initialize = function(name, description)
        {
            # Validate arguments
            # -name, description are single, non-NA character values
            # @NEED TO DO
            
            # Store them
            private$i.name = name
            private$i.description = description
            
            # Create the empty values for other member variables
            private$i.data = list()
            private$i.source = list()
            private$i.url = list()
            private$i.details = list()
            
            private$i.outcomes.info = list()
            private$i.data.groups.info = list()
        },
        
        register.group = function(name, dim.names)
        {
            # Validate arguments
            # - name is a single, non-empty, non-NA character value
            # - dim.names is a named list
            # -- names do not repeat
            # -- elements are either NULL or a non-empty, non-repeating character vector with no empty or NA values
            
            #@need to implement
            
            # If this name has not already been registered, store it
            # If it has, make sure the new dim.names are a 'superset'
            # - all dimensions in the old are still present in the new
            # - dimensions present in the old have the same value as in the new
            # If it is a superset, then store it. Otherwise throw an error
            
            #@need to implement
            
            # Create a list for the data.group in each data.element (ie, i.data, i.source, i.url, i.details)
            for (name in private$i.data.element.names)
                private[[name]] = list()
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        register.outcome = function(outcome,
                                    scale,
                                    denominator.outcome)
        {
            #-- Validate arguments --#
            error.prefix = paste0("Unable to register outcome for data.manager '", private$i.name, "': ")
            
            # - outcome is a single, non-empty, non-NA character value
            if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome) || nchar(outcome)==0)
                stop(paste0(error.prefix, "'outcome' must be a single, non-empty, non-NA character value"))
            error.prefix = paste0("Unable to register outcome '", outcome, 
                                  "' for data.manager '", private$i.name, "': ")
            
            # - scale is a valid model.scale
            check.model.scale(scale, var.name.for.error='scale', error.prefix=error.prefix)
            
            # - If scale is 'rate', 'proportion', or 'time', 
            #   denominator outcome is non-NULL.
            #   Otherwise it is NULL
            # - If non-NULL, denominator outcome:
            # -- is a single, non-empty, non-NA character value
            # -- corresponds to a previously-registered outcome with scale 'non.negative.number'
            
            #@need to implement
            
            # If this outcome has not previously been registered, store it
            # Otherwise, if scale and denominator outcome are the same as before, ignore.
            #   If different, throw an error
            
            #@need to implement
            
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        put = function(outcome,
                       data.group,
                       dimension.values,
                       data,
                       source,
                       url,
                       details)
        {
            #-- Validate arguments --#
            error.prefix = paste0("Unable to put data to data.manager '", private$i.name, "': ")
               
            # *outcome* is a single, non-empty, non-NA character value
            #   Which corresponds to a registered outcome
            if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome) || nchar(outcome)==0)
                stop(paste0(error.prefix, "'outcome' must be a single, non-empty, non-NA character value"))
            
            outcome.info = private$i.outcomes.info[[outcome]]
            if (is.null(outcome.info))
                stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome. Call register.data.outcome() to register it"))
            
            error.prefix = paste0("Unable to put '", outcome, "' data to data.manager '", private$i.name, "': ")
               
            # *data.group* is single, non-empty, non-NA character value
            #   Which a registered data.group
            if (!is.character(data.group) || length(data.group)!=1 || is.na(data.group) || nchar(data.group)==0)
                stop(paste0(error.prefix, "'data.group' must be a single, non-empty, non-NA character value"))
            
            data.group.info = private$i.data.groups.info[[data.group]]
            if (is.null(data.group.info))
                stop(paste0(error.prefix, "'", data.group, "' is not a registered data.group. Call register.data.group() to register it"))
            
            error.prefix = paste0("Unable to put '", outcome, "' data to data.group '",
                                  data.group, "' in data.manager '", private$i.name, "': ")
            
            # *data* is 
            # -- numeric
            # -- appropriate for the scale of the outcome
            # -- either a single scalar value, or an array with named dimnames
            #    where either
            # --- the values of the corresponding dimension in data.group's dim.names is NULL
            # --- the values are a subset of the values of the corresponding dimension in data.group's dim.names
            if (!is.numeric(data))
                stop(paste0(error.prefix,
                            "'data' must be a numeric object"))
            if (length(data)==0)
                stop(paste0(error.prefix,
                            "'data' must have length > 0"))
            
            do.check.values.for.model.scale(data, scale=outcome.info$scale,
                                            variable.name.for.error = 'data',
                                            error.prefix = error.prefix)
            
            if (is.array(data))
            {
                if (is.null(dimnames(data)))
                    stop(paste0(error.prefix,
                                "If 'data' is an array, then it must be an array with named dimnames"))
                
                if (is.null(names(dimnames(data))))
                    stop(paste0(error.prefix,
                                "If 'data' is an array, then it must be an array with NAMED dimnames"))
                
                sapply(names(dimnames(data)), function(d){
                    if (!is.null(data.group.info$dim.names[[d]]))
                    {
                        invalid.values = setdiff(dimnames(data)[[d]], data.group.info$dimnames[[d]])
                        if (length(invalid.values)>0)
                            stop(paste0(error.prefix,
                                        "dimnames(data)[['", d, "']] contains value",
                                        ifelse(length(invalid.values)==0, '', 's'),
                                        collapse.with.and("'", invalid.values, "'"),
                                        " but ",
                                        ifelse(length(invalid.values)==0, 
                                               "this is not a valid value",
                                               "these are not valid values"),
                                        " for the data.group's '", d, "' dimension"))
                    }
                })
                    
            }
            else if (length(data)!=1)
                stop(paste0(error.prefix,
                            "If 'data' is not a single (scalar) numeric value, then it must be an array with named dimnames"))
            
            # dimension.values are valid
            dimension.values = private$resolve.dimension.values(dimension.values,
                                                                data.group = data.group,
                                                                error.prefix = error.prefix)
            
            # *source, url, and details* are:
            # -- either a dimensionless character vector
            #    or lists that are arrays with named dimnames
            # -- if they are arrays with named dimnames, data is an array and each dimension of source/url/details is either:
            # --- not present in the dimnames of data or
            # --- has values exactly equal to the corresponding values of dimnames(data)
            
            #@need to implement this check
            
            
            #-- Set up to receive the data --#
            
            # Figure out what stratification it goes in as the union of:
            # (1) dimensions in the data
            # (2) dimensions in dimension.values
            stratification = private$get.required.stratification(dimension.values=dimension.values,
                                                                 data=data,
                                                                 data.group = data.group,
                                                                 return.as.dimensions = F)
           
            # If there are no data elements for the outcome and data group yet, make empty lists
            if (is.null(private$i.data[[data.group]][[outcome]]))
            {
                for (name in private$i.data.element.names)
                    private[[name]] = list()
            }
            
            # What dim.names do we need to accommodate the new data?
            put.dim.names = private$prepare.put.dim.names(union.dimension.names(dimnames(data),
                                                                                dimension.values))
            
            # If this data element for outcome has not yet been created
            # Or if the previously-created data element does not have all the dimension values we need
            # -> make new data elements
            
            existing.dim.names = dimnames(private$i.data[[data.group]][[outcome]][[stratification]])
            data.already.present = !is.null(existing.dim.names)
            
            if (!data.already.present ||
                !dim.names.are.subset(sub.dim.names=put.dim.names,
                                      super.dim.names = existing.dim.names))
            {
                # Backup old data, if needed
                if (data.already.present)
                {
                    existing.data.and.provenance = lapply(private$i.data.element.names, function(name){
                        private[[name]][[data.group]][[outcome]][[stratification]]
                    })
                }
                
                # Figure out the dimensions for the new data structures
                if (data.already.present)
                    new.dim.names = private$prepare.put.dim.names(union.dimension.names(existing.dim.names, put.dim.names))
                else
                    new.dim.names = put.dim.names
                
                # Make the new (empty) data structures
                private$i.data[[data.group]][[outcome]][[stratification]] = array(NaN, dim=sapply(new.dim.names, length), dimnames = new.dim.names)
                
                for (name in private$i.data.provenance.names)
                {
                    private[[name]][[data.group]][[outcome]][[stratification]] = lapply(1:prod(sapply(new.dim.names, length)), function(i){
                        NULL
                    })
                    
                    dim(private[[name]][[data.group]][[outcome]][[stratification]]) = sapply(new.dim.names, length)
                    dimnames(private[[name]][[data.group]][[outcome]][[stratification]]) = new.dim.names
                }
                    
                # Overwrite the new structure with the old data, if needed
                if (data.already.present)
                {
                    array.access(private$i.data[[data.group]][[outcome]][[stratification]], existing.dim.names) = existing.data.and.provenance$i.data
                    
                    for (name in private$i.data.provenance.names)
                        array.list.access(private[[name]][[data.group]][[outcome]][[stratification]], existing.dim.names) = existing.data.and.provenance[[name]]
                }
            }

            
            #-- Put the data and its provenance --#
            
            # Put the data
            array.access(private$i.data[[data.group]][[outcome]][[stratification]], dimension.values) = data
            
            # Put the data provenance
            data.provenance = list(i.source = source,
                                   i.url = url,
                                   i.details = details)
            
            for (name in private$i.data.provenance.names)
                array.list.access(private[[name]][[data.group]][[outcome]][[stratification]], dimension.values) = data.provenance[[name]]
            
            
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        get = function(outcome,
                       keep.dimensions,
                       dimension.values,
                       data.group,
                       type,
                       na.rm=F)
        {
            #-- Validate arguments --#
            
            # *outcome* is a single, non-NA character value
            #  that has been previously registered as an outcome for this data manager
            
            # *data.group* is either NULL or a single, non-NA character value
            # If NULL, set data.group to the first registered data group (if none have been registered, throw an error)
            # If a character value, it must be a previously registered data.group for this data manager
            
            # *keep.dimensions* is either NULL or a character vector with no NA values or repeats
            # If a vector, all the values of the vector are dimensions for the data.group
            
            # *dimension.values* is valid (resolve.dimension.values does this check)
            
            # *type* is a single, non-NA character value
            # that is one of names(private$i.data.element.names)
            
            
            #-- Figure out what stratification we need to pull the data from --#
            # (the union of the keep dimensions and the dimensions mentioned in dimension.values)
            
            #-- Figure out if we are going to need to aggregate --#
            # We need to aggregate if keep.dimensions is a proper subset of the stratification dimensions
         
            #-- Set up the return value structure (populated with NAs or empty lists) --#
            # A numeric array if type=='data'
            # A list array otherwise
            # And the parallel denominator structure if we need to aggregate and denominator.outcome for the outcome.info is not NULL
            
            #-- Overwrite the data we have into the return value --#
            # Only if we have data
                        
            #-- Aggregate --#
            # (if we need to aggregate)
            # If type=='data'
            #   If we the denominator.outcome is NULL, sum
            #   If the dneominator.outcome is not NULL, sum rv*denominator and divide by sum over denominator
            # If type is one of the others
            #   Union the character vectors in each list
            
            #-- Return --#
        }
    ),
    
    active = list(
        
        name = function(value)
        {
            if (missing(value))
                private$i.name
            else
                stop("Cannot modify 'name' in jheem.data.manager - it is read-only")
        },
        
        description = function(value)
        {
            if (missing(value))
                private$i.description
            else
                stop("Cannot modify 'description' in jheem.data.manager - it is read-only")
        },
        
        outcomes = function(value)
        {
            if (missing(value))
                names(private$i.data)
            else
                stop("Cannot modify 'outcomes' in jheem.data.manager - they are read-only")
        }
    ),
    
    private = list(
        
        #-- About the Data Manager itself --#
        i.name = NULL,
        i.description = NULL,
        
        #-- Storage structures for data and metadata --#
        # These four are lists of lists of lists, indexed
        # [[data.group]][[outcome]][[stratification]]
        i.data = NULL,
        i.source = NULL,
        i.url = NULL,
        i.details = NULL,
        
        i.data.element.names = c(data='i.data', 
                                 source='i.source',
                                 url='i.url',
                                 details='i.details'),
        i.data.provenance.names = c(source='i.source',
                                    url='i.url',
                                    details='i.details'),
        
        #-- Metadata --#
        # These are named lists, with the names being the names of outcomes or data groups
        i.outcomes.info = NULL,
        i.data.groups.info = NULL,
        
        #-- Private Member Functions --#
        
        # Gets the stratification to either
        # - get data with keep.dimensions and dimension.values
        # - put the given 'data' using dimension.values
        get.required.stratification = function(dimension.values,
                                                data=NULL,
                                                keep.dimensions=NULL,
                                               data.group,
                                               return.as.dimensions) #if true, returns the vector of dimensions. If false, collapses to the stratification name
        {
            dimensions = intersect(private$i.data.groups[[data.group]]$dimensions,
                                   union(union(names(dimension.values), 
                                               names(dimnames(data)), keep.dimensions)))
            
            if (return.as.dimensions)
                dimensions
            else
                paste0(dimensions, collapse='__')
        },
        
        prepare.put.dim.names = function(dim.names,
                                         data.group)
        {
            data.group.dim.names = private$i.data.group.info[[data.group]]$dim.names
            rv = lapply(names(dim.names), function(d){
                if (is.null(data.group.dim.names[[d]]))
                    sort(dim.names[[d]])
                else
                    data.group.dim.names[[d]]
            })
            names(rv) = names(dim.names)
            rv
        },
        
        # returns a named list where all elements are character vectors
        # Also checks dimension.values
        resolve.dimension.values = function(dimension.values,
                                             data.group,
                                             error.prefix='')
        {
            if (is.null(dimension.values) ||
                (is.list(dimension.values) && length(dimension.values)==0))
            {
                list()
            }
            else
            {
                dim.names = private$i.data.group.info[[data.group]]$dim.names
                if (is.null(dim.names))
                    stop(paste0(error.prefix,
                                "No data.group with name '", data.group, 
                                "' has been registered to the data.manager"))
                
                if (!is.list(dimension.values))
                    stop(paste0(error.prefix, "'dimension.values' must be a named list"))
                if (is.null(names(dimension.values)))
                    stop(paste0(error.prefix, "'dimension.values' must be a NAMED list"))
                if (any(is.na(names(dimension.values)) | nchar(names(dimension.values))==1))
                    stop(paste0(error.prefix, "The names of 'dimension.values' cannot be NA or the empty string"))
                tabled.dimensions = table(names(dimension.values))
                if (max(tabled.dimensions)>1)
                    stop(paste0(error.prefix,
                                "The names of 'dimension.values' cannot appear more than once. ",
                                collapse.with.and("'", names(tabled.dimensions)[tabled.dimensions>1], "' appears ",
                                                  tabled.dimensions[tabled.dimensions>1], " times") ))
                
                invalid.dimensions = setdiff(names(dimension.values), names(dim.names))
                if (length(invalid.dimensions)>0)
                    stop(paste0(error.prefix,
                                "Invalid dimension",
                                ifelse(length(invalid.dimensions)==1, '', 's'),
                                " for data.group '", data.group, "': ",
                                collapse.with.and("'", invalid.dimensions, "'")))
                
                rv.dimensions = intersect(names(dim.names), names(dimension.values))
                rv = lapply(rv.dimensions, function(d){
                    if (is.null(dim.names[[d]]))
                    {
                        if (!is.character(dimension.values[[d]]))
                            stop(paste0(error.prefix,
                                        "dimension.values for '", d,
                                        "' must be a character vector"))
                        
                        if (length(dimension.values[[d]])==0)
                            stop(paste0(error.prefix,
                                        "dimension.values for '", d,
                                        "' must have at least one value"))
                        
                        if (any(is.na(dimension.values[[d]])) || any(nchar(dimension.values[[d]])==0))
                            stop(paste0(error.prefix,
                                        "dimension.values for '", d, 
                                        "' cannot be NA or empty character values ('')"))
                        
                        dimension.values[[d]]
                    }
                    else
                    {
                        if (is.logical(dimension.values[[d]]))
                        {
                            if (length(dimension.values[[d]])!=length(dim.names[[d]]))
                                stop(paste0(error.prefix,
                                            "if dimension.values[['", d,
                                            "']] is a logical vector, it must have length ",
                                            length(dim.names[[d]])))
                            
                            if (any(is.na(dimension.values[[d]])))
                                stop(paste0(error.prefix,
                                            "dimension.values for '", d,
                                            "' cannot contain NA values"))
                            
                            if (sum(dimension.values[[d]])==0)
                                stop(paste0(error.prefix,
                                            "if dimension.values[['", d,
                                            "']] is a logical vector, it must have at least one value set to TRUE"))
                            
                            dim.names[[d]][ dimension.values[[d]] ]
                        }
                        else if (is.numeric(dimension.values[[d]]) || is.character(dimension.values[[d]]))
                        {
                            if (length(dimension.values[[d]])==0)
                                stop(paste0(error.prefix,
                                            "dimension.values for '", d,
                                            "' must have at least one value"))
                            
                            if (any(is.na(dimension.values[[d]])))
                                stop(paste0(error.prefix,
                                            "dimension.values for '", d,
                                            "' cannot contain NA values"))
                            
                            if (is.numeric(dimension.values[[d]]))
                            {
                                if (any(dimension.values[[d]]<1 || any(dimension.values[[d]]>length(dim.names[[d]]))))
                                    stop(paste0(error.prefix,
                                                "if dimension.values[['", d,
                                                "']] is a numeric vector, its values must fall between 1 and ",
                                                length(dim.names[[d]])))
                                
                                dim.names[[d]][ dimension.values[[d]] ]
                            }
                            else if (is.character(dimension.values[[d]]))
                            {
                                invalid.values = setdiff(dimension.values[[d]], dim.names[[d]])
                                if (length(invalid.values)>0)
                                    stop(paste0(error.prefix,
                                                collapse.with.and("'", invalid.values, "'"),
                                                ifelse(length(invalid.values)==1, " is", " are"),
                                                " not valid dimension.values for dimension '", d, "'"))
                                    
                                dimension.values[[d]]
                            }
                        }
                        else
                            stop(paste0(error.prefix,
                                        "dimension.values for '", d,
                                        "' must be either a character, integer, or logical vector"))
                     }
                })
                
                names(rv) = rv.dimensions
                rv
            }
        }
    )
)
