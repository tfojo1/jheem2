
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
                           description=description)
}

#'@description Register a new data ontology to a data manager before putting data to it
#'
#'@param data.manager A jheem.data.manager object
#'@param name The name of the data group
#'@param ont An ontology object as created by \code{\link{ontology()}}
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
#'@param outcome The name (a single character value) of the outcome. This is the 'internal' name by which the outcome will be referenced in accessing the data manager
#'@param pretty.name A descriptive, fully-formatted and capitalized name to use in generating figures and tables (eg in titles or legends). Should be unique to the outcome (although this is not enforced)
#'@param label A descriptive, one-word or symbol to use in labeling numbers of this outcome (eg "cases") on figures. Need not be unique to the outcome
#'@param description A one-sentence description that gives more details about the outcome (to be used in pop-overs and parentheticals)
#'@param scale The scale for the outcome. Options are 'rate', 'ratio', 'proportion', 'time', 'number', 'non.negative.number'
#'@param denominator.outcome The denominator outcome type that should be used when aggregating data (taking a weighted average) for this outcome type. Must be a previously registered outcome with scale='non.negative.number'. Only applies if scale is 'rate', 'proportion', or 'time'
#'
#'@export
register.data.outcome <- function(data.manager,
                                  outcome,
                                  pretty.name,
                                  label,
                                  description,
                                  scale,
                                  denominator.outcome=NULL)
{
    if (!is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.outcome(outcome=outcome,
                                  pretty.name=pretty.name,
                                  label=label,
                                  description=description,
                                  scale=scale,
                                  denominator.outcome=denominator.outcome)
}

#'@description Put data into a data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param data A numeric array or scalar value containing the data to store. If it is an array, it must have named dimnames set
#'
#'@param outcome The outcome type for the data. Must be an outcome previously registered with \code{\link{register.data.outcome}}
#'@param ontology.name The name of the ontology which the data follow. Must be an ontology previously registered with \code{\link{register.data.ontology}}
#'@param dimension.values A named list that indicates what subset of a bigger data element these particular data should be stored into. The names of dimension values. The values of dimension.values can be either (1) character vectors
#'@param source The (single) character name of the source from which these data derive. Note: a source is conceived such that one source cannot contain two sets of data for the same set of dimension values
#'@param url A character vector with at least one element giving one or more URLs indicating where the data derive from
#'@param details A character vector with at least one element giving one or more URLs giving data collection details. In general, data collected and tabulated using the same approach should have the same details, whereas data with different methods/tabulation should have different details
#'
#'@export
put.data <- function(data.manager,
                     data,
                     outcome,
                     source,
                     ontology.name,
                     dimension.values,
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

#'@description Put long-form data into a data manager
#'
#'@inheritParams put.data
#'@param data A data frame or other 2-dimensional data structure with named columns. Must contain a column named 'value' of numeric values. If the 'source' argument is NULL, then data must also have a column named 'source' that gives the source for each row. May contain additional columns with names matching the specified ontology, which have the dimension values for each row. Any other columns are ignored
#'
#'@export
put.data.long.form <- function(data.manager,
                               data,
                               outcome,
                               source,
                               ontology.name,
                               dimension.values,
                               url,
                               details)
{
    if (!is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$put.long.form(outcome=outcome,
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

#'@description Get pretty names for outcomes
#'
#'@details Gets the pretty.names, labels, or descriptions registered for the outcomes with the data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param outcomes A character vector with the names of outcomes previously registered to this data manager
#'
#'@return A character vector of pretty names, labels, or descriptions
#'
#'@export
get.outcome.pretty.names <- function(data.manager, outcomes)
{
    if (!is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.outcome.pretty.names(outcomes)
}

#'@describeIn get.outcome.pretty.name Get the labels for outcomes
#'@export
get.outcome.labels <- function(data.manager, outcomes)
{
    if (!is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.outcome.labels(outcomes)
}

#'@describeIn get.outcome.pretty.name Get descriptions for outcomes
#'@export
get.outcome.descriptions <- function(data.manager, outcomes)
{
    if (!is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.outcome.descriptions(outcomes)
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
            private$i.url = list()
            private$i.details = list()
            
            private$i.outcome.info = list()
            private$i.ontologies = list()
        },
        
        register.ontology = function(name, ont)
        {
            # Validate arguments
            # - name is a single, non-empty, non-NA character value
            # - ont is an ontology
            
            #@need to implement
            
            # If this name has not already been registered, store it
            # If it has, make sure the new ontology is a 'superset'
            # - all dimensions in the old are still present in the new
            # - dimensions present in the old have the same values as in the new
            # - complete dimensions present in the old are still complete, ditto for incomplete
            # If it is a superset, then store it. Otherwise throw an error
            
            #@need to implement
            
            # Store it
            private$i.ontologies[[name]] = ont
            
            # Create a list for the ontology in each data.element (ie, i.data, i.url, i.details)
            for (name in private$i.data.element.names)
                private[[name]] = list()
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        register.outcome = function(outcome,
                                    pretty.name,
                                    label,
                                    description,
                                    scale,
                                    denominator.outcome=NULL)
        {
            #-- Validate arguments --#
            error.prefix = paste0("Unable to register outcome for data.manager '", private$i.name, "': ")
            
            # - outcome is a single, non-empty, non-NA character value
            if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome) || nchar(outcome)==0)
                stop(paste0(error.prefix, "'outcome' must be a single, non-empty, non-NA character value"))
            error.prefix = paste0("Unable to register outcome '", outcome, 
                                  "' for data.manager '", private$i.name, "': ")
            
            
            # - pretty.name is a single, non-empty, non-NA character value
            if (!is.character(pretty.name) || length(pretty.name)!=1 || is.na(pretty.name) || nchar(pretty.name)==0)
                stop(paste0(error.prefix, "'pretty.name' must be a single, non-empty, non-NA character value"))
            
            # - label is a single, non-empty, non-NA character value
            if (!is.character(label) || length(label)!=1 || is.na(label) || nchar(label)==0)
                stop(paste0(error.prefix, "'label' must be a single, non-empty, non-NA character value"))
            
            # - description is a single, non-empty, non-NA character value
            if (!is.character(description) || length(description)!=1 || is.na(description) || nchar(description)==0)
                stop(paste0(error.prefix, "'description' must be a single, non-empty, non-NA character value"))
            
            
            # - scale is a valid model.scale
            check.model.scale(scale, varname.for.error='scale', error.prefix=error.prefix)
            
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
            
            
            previous.outcome.info = private$i.outcome.info[[outcome]]
            if (!is.null(previous.outcome.info))
            {
                if (scale!=previous.outcome.info$scale ||
                    (is.null(denominator.outcome) && !is.null(previous.outcome.info$denominator.outcome)) ||
                    (!is.null(denominator.outcome) && is.null(previous.outcome.info$denominator.outcome)) ||
                    (!is.null(denominator.outcome) && !is.null(previous.outcome.info$denominator.outcome) &&
                     denominator.outcome != previous.outcome.info$denominator.outcome))
                    stop(paste0(error.prefix, "The outcome '", outcome, "' was previously registered with a different scale and/or denominator outcome"))
            }
            
            
            # Store it
            outcome.info = list(
                outcome = outcome,
                pretty.name = pretty.name,
                label = label,
                description = description,
                scale = scale,
                denominator.outcome = denominator.outcome
            )
            
            private$i.outcome.info[[outcome]] = outcome.info
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        put = function(data,
                       outcome,
                       source,
                       ontology.name,
                       dimension.values,
                       url,
                       details)
        {
            #------------------------#
            #-- Validate arguments --#
            #------------------------#
            
            error.prefix = paste0("Unable to put data to data.manager '", private$i.name, "': ")
               
            # 1) *outcome* is a single, non-empty, non-NA character value
            #     Which corresponds to a registered outcome
            if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome) || nchar(outcome)==0)
                stop(paste0(error.prefix, "'outcome' must be a single, non-empty, non-NA character value"))
            
            outcome.info = private$i.outcome.info[[outcome]]
            if (is.null(outcome.info))
                stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome. Call register.data.outcome() to register it before putting data for that outcome"))
            
            error.prefix = paste0("Unable to put '", outcome, "' data to data.manager '", private$i.name, "': ")
               
            # 2) *ontology.name* is single, non-empty, non-NA character value
            #     Which refers to a registered ontology
            if (!is.character(ontology.name) || length(ontology.name)!=1 || is.na(ontology.name) || nchar(ontology.name)==0)
                stop(paste0(error.prefix, "'ontology.name' must be a single, non-empty, non-NA character value"))
            
            ont = private$i.ontologies[[ontology.name]]
            if (is.null(ont))
                stop(paste0(error.prefix, "'", ontology.name, "' is not a registered ontology. Call register.data.ontology() to register it before putting data for that ontology"))
            
            ont.dimensions.complete = is_complete(ont)
            
            error.prefix = paste0("Unable to put '", outcome, "' data to ontology '",
                                  ontology.name, "' in data.manager '", private$i.name, "': ")
            
            # 3) *data* is 
            #   -- numeric
            #   -- appropriate for the scale of the outcome
            #   -- either a single scalar value, or an array with named dimnames
            #      where either
            #   --- the corresponding dimension in the ontology is incomplete OR
            #   --- the values are a subset of the values of the corresponding dimension in the ontology
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
                
                invalid.dimensions = setdiff(names(dimnames(data)), names(ont))
                if (length(invalid.dimension)>0)
                    stop(paste0(error.prefix,
                                "The dimensions of 'data' include ",
                                collapse.with.and("'", invalid.dimensions, "'"),
                                " which ",
                                ifelse(length(invalid.dimensions)==1, 'is', 'are'),
                                " not present in the '", ontology.name, "' ontology"))
                
                sapply(names(dimnames(data)), function(d){
                    if (ont.dimensions.complete[d])
                    {
                        invalid.values = setdiff(dimnames(data)[[d]], ont[[d]])
                        if (length(invalid.values)>0)
                            stop(paste0(error.prefix,
                                        "dimnames(data)[['", d, "']] contains value",
                                        ifelse(length(invalid.values)==0, '', 's'),
                                        collapse.with.and("'", invalid.values, "'"),
                                        " but ",
                                        ifelse(length(invalid.values)==0, 
                                               "this is not a valid value",
                                               "these are not valid values"),
                                        " for the '", ontology.name, "' ontology's '", d, "' dimension"))
                    }
                })
                    
            }
            else if (length(data)!=1)
                stop(paste0(error.prefix,
                            "If 'data' is not a single (scalar) numeric value, then it must be an array with named dimnames"))
            
            # check dimension.values are valid
            # map them to character values
            dimension.values = resolve.ontology.dimension.values(ont = ont,
                                                                 dimension.values = dimension.values,
                                                                 error.prefix = paste0(error.prefix, "Error resolving 'dimension.values' - "))

            # 4) *source* is
            #    a single, non-NA, non-empty character value
            if (!is.character(source))
                stop(paste0(error.prefix, "'source' must be a single character value"))
            if (length(source) != 1)
                stop(paste0(error.prefix, "'source' must be a SINGLE character value"))
            if (is.na(source))
                stop(paste0(error.prefix, "'source' cannot be NA"))
            if (nchar(source)=='')
                stop(paste0(error.prefix, "'source' cannot be empty ('')"))
                
            
            # 5) *url, and details* are 
            #    -- character vectors
            #    -- with at least one value
            #    -- with no NA values
            #    -- with no empty values
            #    -- with no duplicates
            
            if (!is.character(url))
                stop(paste0(error.prefix, "'url' must be a character vector"))
            if (length(url)==0)
                stop(paste0(error.prefix, "'url' must contain at least one value"))
            if (any(is.na(url)))
                stop(paste0(error.prefix, "'url' cannot contain NA values"))
            if (any(nchar(url)==0))
                stop(paste0(error.prefix, "'url' cannot contain empty values ('')"))
            url = unique(url)
                
            if (!is.character(details))
                stop(paste0(error.prefix, "'details' must be a character vector"))
            if (length(details)==0)
                stop(paste0(error.prefix, "'details' must contain at least one value"))
            if (any(is.na(details)))
                stop(paste0(error.prefix, "'details' cannot contain NA values"))
            if (any(nchar(details)==0))
                stop(paste0(error.prefix, "'details' cannot contain empty values ('')"))
            details = unique(details)
            
            #@need to implement this check
            
            
            #--------------------------------#
            #-- Set up to receive the data --#
            #--------------------------------#
            
            data.element.names = c('i.data','i.url','i.details')
            metadata.element.names = data.element.names[-1]
            
            # Figure out what stratification it goes in as the union of:
            # (1) dimensions in the data
            # (2) dimensions in dimension.values
            stratification = private$get.required.stratification(dimension.values=dimension.values,
                                                                 data=data,
                                                                 data.group = data.group,
                                                                 return.as.dimensions = F)
           
            # If there are no data elements for the outcome and ontology yet, make empty lists
            if (is.null(private$i.data[[ontology.name]][[outcome]]))
            {
                for (name in private$i.data.element.names)
                    private[[name]][[ontology.name]][[outcome]] = list()
            }
            
            if (is.null(private$i.data[[ontology.name]][[outcome]][[stratification]]))
            {
                for (name in private$i.data.element.names)
                    private[[name]][[ontology.name]][[outcome]][[stratification]] = list()
            }
            
            # What dim.names do we need to accommodate the new data?
            put.dim.names = private$prepare.put.dim.names(outer.join.dim.names(dimnames(data),
                                                                                dimension.values))
            
            # If this data element for outcome has not yet been created
            # Or if the previously-created data element does not have all the dimension values we need
            # -> make new data elements
            
            existing.dim.names = dimnames(private$i.data[[data.group]][[outcome]][[stratification]][[source]])
            data.already.present = !is.null(existing.dim.names)
            
            if (!data.already.present ||
                !dim.names.are.subset(sub.dim.names=put.dim.names,
                                      super.dim.names = existing.dim.names))
            {
                # Backup old data, if needed
                if (data.already.present)
                {
                    existing.data.and.metadata = lapply(data.element.names, function(name){
                        private[[name]][[data.group]][[outcome]][[stratification]][[source]]
                    })
                }
                
                # Figure out the dimensions for the new data structures
                if (data.already.present)
                    new.dim.names = private$prepare.put.dim.names(outer.join.dim.names(existing.dim.names, put.dim.names))
                else
                    new.dim.names = put.dim.names
                
                # Make the new (empty) data structures
                private$i.data[[data.group]][[outcome]][[stratification]][[source]] = array(NaN, dim=sapply(new.dim.names, length), dimnames = new.dim.names)
                
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

            
            #-- Put the data and its metadata --#
            
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
        
        put.long.form = function(data,
                                 outcome,
                                 source,
                                 ontology.name,
                                 dimension.values,
                                 url,
                                 details)
        {
            #-- Initial validate arguments --#
            
            # *data* must be a 2d object with named columns
            if (is.null(dim(data)) || length(dim(data))!=2)
                stop("Cannot put long-form data to the data.manager. 'data' must be a 2-dimensional object")
            if (is.null(dimnames(data)[[2]]))
                stop("Cannot put long-form data to the data.manager. 'data' must have named columns")
            
            if (missing(outcome) || is.null(outcome))
            {
                # If 'outcome' is NULL, then data must have a named outcome 
                if (all(names(data)!='outcome'))
                    stop("Cannot put long-form data to the data.manager. Either 'outcome' must be set or it must be a column in the data object")
                
                unique.outcomes = unique(data[,'outcome'])
                for (one.outcome in unique.outcomes)
                {
                    self$put.long.form(outcome = one.outcome,
                                       ontology.name = ontology.name,
                                       source = source,
                                       dimension.values = dimension.values,
                                       data = data[data[,'outcome']==one.outcome,],
                                       url = url,
                                       details = details)
                }
            }
            else
            { 
                #-- Validate Arguments --#
                error.prefix = paste0("Cannot put long-form '", outcome, "' data to the data.manager: ")
                col.names = dimnames(data)[[2]]
                # *data* must have a column named 'value' which is numeric
                if (all(col.names != 'value'))
                    stop(paste0(error.prefix, "'data' must have a column named 'value'"))
                if (!is.numeric(data[,'value']))
                    stop(paste0(error.prefix, "The value column in 'data' must contain numeric values"))
                
                # Ontology name must be a single, non-NA character vector that refers to a registered ontology
                if (!is.character(ontology.name) || length(ontology.name)!=1 || is.na(ontology.name) || nchar(ontology.name)==0)
                    stop(paste0(error.prefix, "'ontology.name' must be a single, non-empty, non-NA character value"))
                
                ont = private$i.ontologies[[ontology.name]]
                if (is.null(ont))
                    stop(paste0(error.prefix, "'", ontology.name, "' is not a registered ontology. Call register.data.ontology() to register it before putting data for that ontology"))
                
                # All the other columns of data which are dimensions in the ontology must be either character or numeric vectors
                # Resolve them to character vectors
                dimensions = intersect(names(ont), setdiff(col.names, c('value','outcome')))
                sapply(dimensions, function(d){
                    if (!is.character(data[,d]) && !is.numeric(data[,d]))
                        stop(paste0(error.prefix, "Column '", d, "' in 'data' must contain character or numeric values"))
                    T
                })
                data[,dimensions] = resolve.ontology.dimension.values(ont=ont, dimension.values=data[,dimensions], 
                                                                      error.prefix=paste0(error.prefix, " Error resolving dimension values - "))
                
                # Hydrate it to an array
                dim.names = lapply(dimensions, function(d){
                    unique(data[,d])
                })
                names(dim.names) = dimensions
                
                arr.data = array(as.numeric(NA), dim=sapply(dim.names, length), dimnames=dim.names)
                for (i in nrow(data))
                {
                    array.access(arr.data, dimension.values = as.list(data[i,dimensions])) = data[i,'value']
                }
                
                #-- Pass through to main put function --#
                
                print("PASS THROUGH")
                self$put(outcome = outcome,
                         ontology.name = ontology.name,
                         source = source,
                         dimension.values = dimension.values,
                         data = arr.data,
                         url = url,
                         details = details)
            }
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
        },
        
        get.outcome.pretty.names = function(outcomes)
        {
            sapply(private$i.outcome.info[outcomes], function(info){
                info$pretty.name
            })
        },
        
        get.outcome.labels = function(outcomes)
        {
            sapply(private$i.outcome.info[outcomes], function(info){
                info$label
            })
        },
        
        get.outcome.descriptions = function(outcomes)
        {
            sapply(private$i.outcome.info[outcomes], function(info){
                info$label
            })
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
                names(private$i.outcome.info)
            else
                stop("Cannot modify 'outcomes' in jheem.data.manager - they are read-only")
        },
        
        ontology.names = function(value)
        {
            if (missing(value))
                names(private$i.ontologies)
            else
                stop("Cannot modify 'ontology.names' in jheem.data.manager - they are read-only")
        }
    ),
    
    private = list(
        
        #-- About the Data Manager itself --#
        i.name = NULL,
        i.description = NULL,
        
        #-- Storage structures for data and metadata --#
        # These three are lists of lists of lists of lists, indexed
        # [[ontology.name]][[outcome]][[stratification]][[source]]
        # for i.data, each element of this depth-4 access is a numeric array
        # for i.url and i.details, each element is a list array (ie a list with dimnames set), each element of which is a character vector
        i.data = NULL,
        i.url = NULL,
        i.details = NULL,
        
        #-- Metadata --#
        # These are named lists, with the names being the names of outcomes or data groups
        i.outcome.info = NULL,
        i.ontologies = NULL,
        
        ##------------------------------##
        ##-- Private Member Functions --##
        ##------------------------------##
        
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
