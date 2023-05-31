
# Depends on:
# R6 package
# HELPERS_misc_helpers
# HELPERS_dim_name_helpers
# HELPERS_array_helpers

##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##

#'@title Create a JHEEM Data Manager
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

#'@title Register a new data ontology to a data manager before putting data to it
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
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.ontology(name=name,
                                   ont=ont)
}

#'@title Register an outcome to a data manager before putting data for that outcome
#'
#'@param data.manager A jheem.data.manager object
#'@param outcome The name (a single character value) of the outcome. This is the 'internal' name by which the outcome will be referenced in accessing the data manager
#'@param metadata An object of class 'outcome.metadata', as created by \code{\link{create.outcome.metadata}} that contains information about how to display the outcome
#'@param denominator.outcome The denominator outcome type that should be used when aggregating data (taking a weighted average) for this outcome type. Must be a previously registered outcome with scale='non.negative.number'. Only applies if scale is 'rate', 'proportion', or 'time'
#'@param overwrite A logical indicating whether the information on this outcome should overwrite previously-registered information about this outcome. However, this registration must include the same metadata$scale and denominator.outcome (ie, can't change the structure of the outcome, only the display 'trappings')
#'
#'@export
register.data.outcome <- function(data.manager,
                                  outcome,
                                  metadata,
                                  denominator.outcome=NULL,
                                  overwrite = F)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.outcome(outcome = outcome,
                                  metadata = metadata,
                                  denominator.outcome = denominator.outcome,
                                  overwrite = overwrite)
}

#'@title Register a data source to a data manager before putting data from that source
#'
#'@param data.manager A jheem.data.manager object
#'@param outcome The name (a single character value) of the outcome. This is the 'internal' name by which the outcome will be referenced in accessing the data manager
#'@param full.name A descriptive, fully-formatted and capitalized name to use in generating figures and tables (eg in popovers). Avoid abbreviations. Should be unique to the data source (although this is not enforced)
#'@param short.name A name for the data source to use in setting where brevity is important. Ideal to use abbreviations, but can be the same as full.name
#'
#'@details Note: a source is conceived such that one source cannot contain two sets of data for the same set of dimension values
#'
#'@export
register.data.source <- function(data.manager,
                                  source,
                                  full.name,
                                  short.name = full.name)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.source(source=source,
                                 full.name=full.name,
                                 short.name=short.name)
}

#'@title Put data into a data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param data A numeric array or scalar value containing the data to store. If it is an array, it must have named dimnames set
#'
#'@param outcome The outcome type for the data. Must be an outcome previously registered with \code{\link{register.data.outcome}}
#'@param ontology.name The name of the ontology which the data follow. Must be an ontology previously registered with \code{\link{register.data.ontology}}
#'@param dimension.values A named list that indicates what subset of a bigger data element these particular data should be stored into. The names of dimension values. The values of dimension.values can be either (1) character vectors
#'@param source The (single) character name of the source from which these data derive. Must be a source previously registered with \code{\link{register.data.source}} Note: a source is conceived such that one source cannot contain two sets of data for the same set of dimension values
#'@param url A character vector with at least one element giving one or more URLs indicating where the data derive from
#'@param details A character vector with at least one element giving one or more URLs giving data collection details. In general, data collected and tabulated using the same approach should have the same details, whereas data with different methods/tabulation should have different details
#'@param allow.na.to.overwrite A logical indicator for whether NA values in data should be allowed to overwrite previous values put to the data manager (if data have been put previously)
#'
#'@export
put.data <- function(data.manager,
                     data,
                     outcome,
                     source,
                     ontology.name,
                     dimension.values,
                     url,
                     details,
                     allow.na.to.overwrite=F)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$put(data=data,
                     outcome=outcome,
                     source=source,
                     ontology.name=ontology.name,
                     dimension.values=dimension.values,
                     url=url,
                     details=details,
                     allow.na.to.overwrite=allow.na.to.overwrite)
}

#'@title Put long-form data into a data manager
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
                               details,
                               allow.na.to.overwrite=F)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$put.long.form(data=data,
                               outcome=outcome,
                               source=source,
                               ontology.name=ontology.name,
                               dimension.values=dimension.values,
                               url=url,
                               details=details,
                               allow.na.to.overwrite=allow.na.to.overwrite) 
}

#'@title Pull data from a data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param outcome The outcome type for the data. Must be an outcome previously registered to this data manager with \code{\link{register.data.outcome}}
#'@param keep.dimensions The dimensions that should be retained in the returned value
#'@param dimension.values A named list, indicating which values for specific dimensions to pull data for
#'@param sources The data sources from which to pull data (if available). If NULL, will pull from all data sources that have any relevant data
#'@param target.ontology Optional argument, indicating the ontology according to which results are desired. The data manager will apply an ontology mapping (if it can) to align its data to the desired ontology
#'@param allow.mapping.from.target.ontology A logical indicator. If TRUE, if target.ontology is specified, but the data manager does not have data that can be mapped to the target ontology, it will search for data such that an ontology.mapping can be applied to data in the target.ontology that make those data align with the data pulled
#'@param from.ontology.names The names of the ontologies from which to pull data (if available). If NULL, will pull from all ontologies that have any relevant data and can be mapped to the requested ontology. Must refer to ontologies previously registered to this data manager with \code{\link{register.data.ontology}}
#'@param append.attributes A character vector indicating requested data attributes to include with the result. May be either "details", "url", or both
#'@param na.rm Whether to disregard NA values when aggregating out dimensions not in keep.dimensions
#'
#'@return A numeric array with named dimnames, with one dimension for each value of 'keep.dimensions', plus an additional dimension, "source" at the beginning, with one value for each source from which data were pulled
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
                      append.attributes = NULL,
                      na.rm = F)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$pull(outcome = outcome,
                     keep.dimensions = keep.dimensions,
                     dimension.values = dimension.values,
                     sources = sources,
                     target.ontology = target.ontology,
                     allow.mapping.from.target.ontology = allow.mapping.from.target.ontology,
                     from.ontology.names = from.ontology.names,
                     append.attributes = append.attributes,
                     na.rm = na.rm)
}

#'@title Get pretty names for outcomes
#'
#'@details Gets the pretty.names, labels, or descriptions registered for the outcomes with the data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param outcomes A character vector with the names of outcomes previously registered to this data manager
#'
#'@return A character vector of pretty names, labels, or descriptions
#'
#'@export
get.data.outcome.pretty.names <- function(data.manager, outcomes)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.outcome.pretty.names(outcomes)
}

#'@describeIn get.data.outcome.pretty.names Get the labels for outcomes
#'@export
get.data.outcome.labels <- function(data.manager, outcomes)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.outcome.labels(outcomes)
}

#'@describeIn get.data.outcome.pretty.names Get descriptions for outcomes
#'@export
get.data.outcome.descriptions <- function(data.manager, outcomes)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.outcome.descriptions(outcomes)
}

#'@title Get full names for data sources
#'
#'@details Gets the full.names or short.names for the data sources in the data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param sources A character vector with the names of sources previously registered to this data manager
#'
#'@return A character vector of full names or short names
#'
#'@export
get.data.source.full.names <- function(data.manager, sources)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.source.full.names(sources)
}

#'@describeIn get.data.source.full.names Get short names for data sources
#'@export
get.data.source.short.names <- function(data.manager, sources)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.source.short.names(sources)
}

#'@export
get.registered.ontology <- function(data.manager, ontology.name)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.registered.ontology(ontology.name)
}

##----------------------##
##-- CLASS DEFINITION --##
##----------------------##



JHEEM.DATA.MANAGER = R6::R6Class(
    'jheem.data.manager',
    
    public = list(
        check = function()
        {
            browser()
        },
      
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
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        register.outcome = function(outcome,
                                    metadata,
                                    denominator.outcome=NULL,
                                    overwrite=F)
        {
            #-- Validate arguments --#
            error.prefix = paste0("Unable to register outcome for data.manager '", private$i.name, "': ")
            
            # - outcome is a single, non-empty, non-NA character value
            if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome) || nchar(outcome)==0)
                stop(paste0(error.prefix, "'outcome' must be a single, non-empty, non-NA character value"))
            error.prefix = paste0("Unable to register outcome '", outcome, 
                                  "' for data.manager '", private$i.name, "': ")
            
            
            # - metadata is a 'outcome.metadata' object
            if (!is(metadata, "outcome.metadata"))
                stop(paste0(error.prefix, "'metadata' must be an object of class 'outcome.metadata' as returned by create.outcome.metadata()"))

            # - If metadata$scale is 'rate', 'proportion', or 'time', 
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
                if (metadata$scale!=previous.outcome.info$metadata$scale ||
                    (is.null(denominator.outcome) && !is.null(previous.outcome.info$denominator.outcome)) ||
                    (!is.null(denominator.outcome) && is.null(previous.outcome.info$denominator.outcome)) ||
                    (!is.null(denominator.outcome) && !is.null(previous.outcome.info$denominator.outcome) &&
                     denominator.outcome != previous.outcome.info$denominator.outcome))
                    stop(paste0(error.prefix, "The outcome '", outcome, "' was previously registered with a different scale and/or denominator outcome"))
            }
            
            
            # Store it
            outcome.info = list(
                outcome = outcome,
                metadata = metadata,
                denominator.outcome = denominator.outcome
            )
            
            private$i.outcome.info[[outcome]] = outcome.info
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        register.source = function(source,
                                   full.name,
                                   short.name,
                                   overwrite=F)
        {
            #-- Validate arguments --#
            error.prefix = paste0("Unable to register source for data.manager '", private$i.name, "': ")
            
            # - source is a single, non-empty, non-NA character value
            if (!is.character(source) || length(source)!=1 || is.na(source) || nchar(source)==0)
                stop(paste0(error.prefix, "'source' must be a single, non-empty, non-NA character value"))
            error.prefix = paste0("Unable to register source '", source, 
                                  "' for data.manager '", private$i.name, "': ")
            
            
            # - full.name is a single, non-empty, non-NA character value
            if (!is.character(full.name) || length(full.name)!=1 || is.na(full.name) || nchar(full.name)==0)
                stop(paste0(error.prefix, "'full.name' must be a single, non-empty, non-NA character value"))
            
            # - short.name is a single, non-empty, non-NA character value
            if (!is.character(short.name) || length(short.name)!=1 || is.na(short.name) || nchar(short.name)==0)
                stop(paste0(error.prefix, "'short.name' must be a single, non-empty, non-NA character value"))
            
            # If this outcome has not previously been registered, store it
            # Otherwise, if overwrite==T, store the new one
            # Otherwise, throw an error if full.name or short.name is different
            #   If different, throw an error
            
            previous.source.info = private$i.source.info[[source]]
            if (overwrite || is.null(previous.source.info))
            {
                source.info = list(
                    source = source,
                    full.name = full.name,
                    short.name = short.name)
                
                private$i.source.info[[source]] = source.info
            }
            else if (previous.source.info$full.name != full.name || previous.source.info$short.name != short.name)
            {
                stop(paste0(error.prefix, "A data source named '", source, "' has already been registered. If you want to overwrite the previously registered source, use overwrite==T"))
            }

            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        put = function(data,
                       outcome,
                       source,
                       ontology.name,
                       dimension.values,
                       url,
                       details,
                       allow.na.to.overwrite=F)
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
            
            do.check.values.for.model.scale(data, scale=outcome.info$metadata$scale,
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
                if (length(invalid.dimensions)>0)
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
            
            #@Andrew Data cannot be missing any incomplete dimensions unless they are present in dimension.values
            missing.dimensions = setdiff(names(ont), c(names(dimnames(data)), names(dimension.values)))
            if (!all(ont.dimensions.complete[missing.dimensions]))
                stop(paste0(error.prefix, "either data or 'dimension.values' must contain any incomplete dimensions in the ontology"))
            
            # check dimension.values are valid
            # map them to character values
            dimension.values = resolve.ontology.dimension.values(ont = ont,
                                                                 dimension.values = dimension.values,
                                                                 error.prefix = paste0(error.prefix, "Error resolving 'dimension.values' - "))

            # 4) *source* is
            #    a single, non-NA, non-empty character value
            #    Which corresponds to a registered outcome
            if (!is.character(source))
                stop(paste0(error.prefix, "'source' must be a single character value"))
            if (length(source) != 1)
                stop(paste0(error.prefix, "'source' must be a SINGLE character value"))
            if (is.na(source))
                stop(paste0(error.prefix, "'source' cannot be NA"))
            if (nchar(source)=='')
                stop(paste0(error.prefix, "'source' cannot be empty ('')"))
            
            source.info = private$i.source.info[[source]]
            if (is.null(source.info))
                stop(paste0(error.prefix, "'", source, "' is not a registered data source. Call register.data.source() to register it before putting data from that source"))
            
            
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
            
            # 6) *allow.na.to.overwrite* is a single, non-NA, logical value
            if (!is.logical(allow.na.to.overwrite) || length(allow.na.to.overwrite)!=1 || is.na(allow.na.to.overwrite))
                stop(paste0(error.prefix, "'allow.na.to.overwrite' must be a single, non-NA logical value"))
            
            #--------------------------------#
            #-- Set up to receive the data --#
            #--------------------------------#

            data.element.names = c('i.data','i.url','i.details')
            metadata.element.names = data.element.names[-1]
            
            # Figure out what stratification it goes in as the union of:
            # (1) dimensions in the data
            # (2) dimensions in dimension.values
            stratification = private$get.required.stratification(dimension.values = dimension.values,
                                                                 data = data,
                                                                 ontology.name = ontology.name,
                                                                 return.as.dimensions = F)
            
            # What dim.names do we need to accommodate the new data?
            put.dim.names = private$prepare.put.dim.names(outer.join.dim.names(if (is.array(data)) dimnames(data) else list(), dimension.values),
                                                          ontology.name = ontology.name)
            
            # -> make new data elements
            
            existing.dim.names = dimnames(private$i.data[[outcome]][[source]][[ontology.name]][[stratification]])
            
            data.already.present = !is.null(existing.dim.names)
            
            if (!data.already.present ||
                !dim.names.are.subset(sub.dim.names = put.dim.names,
                                      super.dim.names = existing.dim.names)
                )
            {
                # Backup old data, if needed
                if (data.already.present)
                {
                    existing.data.and.metadata = lapply(data.element.names, function(name){
                        private[[name]][[outcome]][[source]][[ontology.name]][[stratification]]
                    })
                    names(existing.data.and.metadata) = data.element.names
                }
                
                # Figure out the dimensions for the new data structures
                if (data.already.present)
                    new.dim.names = private$prepare.put.dim.names(outer.join.dim.names(existing.dim.names, put.dim.names),
                                                                  ontology.name = ontology.name)
                else
                    new.dim.names = put.dim.names
                
                # Update ontology
                for (d in names(new.dim.names)) {
                    private$i.ontologies[[ontology.name]][[d]] = new.dim.names[[d]]
                }
                
                # Make the new (empty) data structures
                private$i.data[[outcome]][[source]][[ontology.name]][[stratification]] =
                    array(NaN, dim=sapply(new.dim.names, length), dimnames = new.dim.names)
                
                for (name in metadata.element.names)
                {
                    private[[name]][[outcome]][[source]][[ontology.name]][[stratification]] = 
                        lapply(1:prod(sapply(new.dim.names, length)), function(i){
                            NULL
                        })
                    
                    dim(private[[name]][[outcome]][[source]][[ontology.name]][[stratification]]) = sapply(new.dim.names, length)
                    dimnames(private[[name]][[outcome]][[source]][[ontology.name]][[stratification]]) = new.dim.names
                }
                    
                # Overwrite the new structure with the old data, if needed
                if (data.already.present)
                {
                    for (name in data.element.names)
                        array.access(private[[name]][[outcome]][[source]][[ontology.name]][[stratification]], existing.dim.names) =
                            existing.data.and.metadata[[name]]
                }
            }
            
            #-- Put the data and its metadata --#
            
            # get the indices we're going to write into
            #@Andrew get.array.access.indices makes an array full of 1:prod(sapply(arr.dim.names, length))
            #@ then it calls fast.array.access with that array and the data dimnames and dimension.values
            #@ fast.array.access loops five times if five dimensions (location, year, age, risk, sex...) with lapply
            #@ within fast.array.access, subset.values is a list of length(dims) and each element has all the dim values
            
            overwrite.indices = get.array.access.indices(arr.dim.names = dimnames(private$i.data[[outcome]][[source]][[ontology.name]][[stratification]]),
                                                         dimension.values = c(dimnames(data), dimension.values))
            if (!allow.na.to.overwrite)
            {
                overwrite.indices = overwrite.indices[!is.na(data)]
                data = data[!is.na(data)]
            }
                
            # Put data
            private$i.data[[outcome]][[source]][[ontology.name]][[stratification]][overwrite.indices] = data

            # Put metadata
            private$i.url[[outcome]][[source]][[ontology.name]][[stratification]][overwrite.indices] = 
                lapply(1:length(overwrite.indices), function(i){ url })
            private$i.details[[outcome]][[source]][[ontology.name]][[stratification]][overwrite.indices] = 
                lapply(1:length(overwrite.indices), function(i){ details })
            
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        put.long.form = function(data,
                                 outcome,
                                 source,
                                 ontology.name,
                                 dimension.values,
                                 url,
                                 details,
                                 allow.na.to.overwrite=F)
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

                for (i in 1:nrow(data))
                {
                    array.access(arr.data, dimension.values = as.list(data[i,dimensions])) = data[i,'value']
                }
                
                #-- Pass through to main put function --#
                self$put(outcome = outcome,
                         ontology.name = ontology.name,
                         source = source,
                         dimension.values = dimension.values,
                         data = arr.data,
                         url = url,
                         details = details,
                         allow.na.to.overwrite = allow.na.to.overwrite)
            }
        },
        
        pull = function(data.manager,
                        outcome,
                        keep.dimensions = NULL,
                        dimension.values = NULL,
                        sources = NULL,
                        target.ontology = NULL,
                        allow.mapping.from.target.ontology = T,
                        from.ontology.names = NULL,
                        append.attributes = NULL,
                        na.rm = F)
        {
            #-- Validate arguments --#
            
            error.prefix = paste0("Cannot pull '", outcome, "' data from the data manager: ")
            
            # *outcome* is a single, non-NA character value
            #  that has been previously registered as an outcome for this data manager
            if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome) || nchar(outcome)==0)
                stop(paste0(error.prefix, "'outcome' must be a single, non-empty, non-NA character value"))
            
            outcome.info = private$i.outcome.info[[outcome]]
            if (is.null(outcome.info))
                stop(paste0(error.prefix, "'", outcome, "' is not a registered ontology."))
            
            # *keep.dimensions* is either NULL or a character vector with no NA values or repeats
            if (!is.null(keep.dimensions) && (!is.character(keep.dimensions) || any(duplicated(keep.dimensions)) || anyNA(keep.dimensions)))
                stop(paste0(error.prefix, "'keep.dimensions' must be either NULL or a character vector with no NA values or repeats"))
            
            # *dimension.values* are valid 
            #   - check.dimension.values.valid() doesn't accept NULL because it wants a list
            if (!is.null(dimension.values))
                check.dimension.values.valid(dimension.values, "dimension.values")
            
            # *sources* is either NULL or a character vector with at least one element and no NA or empty values
            #  that have all been registered previously as sources with this data manager
            if (!is.null(sources) && (!is.character(sources) || !length(sources)>0 || anyNA(sources) || any(nchar(sources)==0)))
                stop(paste0(error.prefix, "'sources' must be a character vector with at least one element and no NA or empty values"))
            
            unregistered.sources = sapply(sources, function(x){is.null(private$i.source.info[[x]])})
            if (any(unregistered.sources))
                stop(paste0(error.prefix, "all sources must be registered with this data manager"))

            # *target.ontology* is either NULL or an ontology object
            if (!is.null(target.ontology) && !is.ontology(target.ontology))
                stop(paste0(error.prefix, "'target.ontology' must be either NULL or an ontology object"))
            
            # If there is a target ontology, it needs to contain the dimension.values if any
            resolved.dimension.values = NULL
            if (!is.null(target.ontology) && !is.null(dimension.values)) {
                resolved.dimension.values = resolve.ontology.dimension.values(target.ontology, dimension.values, error.prefix = error.prefix, throw.error.if.unresolvable = FALSE)
                if (is.null(resolved.dimension.values))
                    stop(paste0(error.prefix, "'dimension.values' must be contained in 'target.ontology'"))
            }
            
            # The target ontology also needs to contain the keep dimensions if any
            if (!is.null(target.ontology) && !is.null(keep.dimensions)) {
                if (!any(keep.dimensions %in% names(target.ontology)))
                    stop(paste0(error.prefix, "'keep.dimensions' must be contained in 'target.ontology'"))
            }
            
            # The target ontology cannot have any NULL dimensions (ones that do have never had any data put to them)
            if (any(sapply(target.ontology, is.null)))
                stop(paste0(error.prefix, "'target.ontology' cannot have any NULL dimensions"))
            
            # *allow.mapping.from.target.ontology* is a single, non-NA logical value
            if (!is.logical(allow.mapping.from.target.ontology) || length(allow.mapping.from.target.ontology)!=1 || is.na(allow.mapping.from.target.ontology))
                stop(paste0(error.prefix, "'allow.mapping.from.target.ontology' must be a single, non-NA logical value"))
            
            # *from.ontology.names* is either NULL or a character vector with no NA or empty values
            if (!is.null(from.ontology.names) && (!is.character(from.ontology.names) || anyNA(from.ontology.names) || any(nchar(from.ontology.names)==0)))
                stop(paste0(error.prefix, "from.ontology.names must be either NULL or a character vector with no NA or empty values"))
            
            #  all of which have been previously registered with this data manager (if not NULL)
            unregistered.ontologies = sapply(from.ontology.names, function(x){is.null(private$i.ontologies[[x]])})
            if (is.null(from.ontology.names) && any(unregistered.ontologies))
                stop(paste0(error.prefix, "all ontologies in from.ontology.names must be registered with this data manager"))
            
            # *append.attributes* is either NULL or a character vector with no NA values that 
            #  contains only "details" or "url" or both
            if (!is.null(append.attributes) && (!is.character(append.attributes) || anyNA(append.attributes) || !all(append.attributes %in% c("details", "url"))))
                stop(paste0(error.prefix, "append.attributes' must be either NULL or a character vector with no NA values that contains only 'details' or 'url' or both"))
            
            # *na.rm* is a single, non-NA logical value
            if (!is.logical(na.rm) || length(na.rm)!=1 || is.na(na.rm))
                stop(paste0(error.prefix, "na.rm must be a single, non-NA, logical value"))
            
            #-- The big loop --#
            #
            # In an lapply for source
            
            # If keep.dimensions is NULL, we can overwrite it with the target.ontology dimensions. Otherwise, we will use the dimensions of the first successful pull.
            if (is.null(keep.dimensions) && !is.null(target.ontology))
                keep.dimensions = names(target.ontology)
            
            # These must be saved if applicable
            target.to.common.mapping = NULL
            common.ontology = NULL

            # If sources is NULL, use all the sources from the outcome
            if (is.null(sources))
                sources.used.names = names(private$i.data[[outcome]])
            else
                sources.used.names = sources


            return.data = lapply(sources.used.names, function(x) {
                
                # Helps us return NULL data if we later find we need denominator data for aggregation and can't find it
                source.lacks.denominator.data.flag = FALSE
                
                # Use all ontologies in the source or only those also in from.ontology.names
                source.ontology.names = names(private$i.data[[outcome]][[x]])
                ontologies.used.names = ifelse(
                    is.null(from.ontology.names),
                    source.ontology.names,
                    intersect(source.ontology.names, from.ontology.names))
                
                data.to.return = NULL
                
                for (y in ontologies.used.names) {
                    
                    ont = private$i.ontologies[[y]]
                    
                    # Resolving dimension values gives us character vector version even though input could be logical or numeric
                    # check if any dimensions in dimension.values but not in keep.dimensions include all possible values for that dimension (complete)
                    # if so, drop them from dimension.values
                    # We only need to check this now for the current ontology since we checked it at the beginning for the case with target ontology 
                    if (is.null(target.ontology))
                        resolved.dimension.values = resolve.ontology.dimension.values(ont, dimension.values, error.prefix = error.prefix, throw.error.if.unresolvable = FALSE)
                    
                    if (is.null(resolved.dimension.values) && !is.null(dimension.values)) {
                        next
                    }
                    
                    # Dimension.values cannot contain *incomplete* dimensions that aren't in keep.dimensions because they'd need to be aggregated
                    # We checked this earlier for cases with a target.ontology
                    if (is.null(target.ontology) && !is.null(dimension.values)) {
                        dimensions.eventually.aggregated = setdiff(names(dimension.values), keep.dimensions)
                        if (!all(attr(target.ontology, 'is.complete')[dimensions.eventually.aggregated]))
                            stop(paste0(error.prefix, "'dimension.values' cannot contain incomplete dimensions that are not also in 'keep.dimensions'"))
                    }
                    
                    stratification.names = names(private$i.data[[outcome]][[x]][[y]])
                    
                    for (strat in stratification.names) {

                        strat.data = private$i.data[[outcome]][[x]][[y]][[strat]]
                        strat.dimensions = names(dim(strat.data))
                        strat.dimnames = as.ontology(dimnames(strat.data),incomplete.dimensions = intersect(incomplete.dimensions(ont), strat.dimensions))
                        
                        strat.to.target.mapping = NULL
                        strat.to.common.mapping = NULL
                        target.to.common.mapping.placeholder = NULL
                        
                        if (!is.null(target.ontology)) {
                            if (allow.mapping.from.target.ontology) {
                                if (!is.null(common.ontology)) {
                                    
                                    # we already have a common ontology to use
                                    strat.to.common.mapping = get.ontology.mapping(strat, common.ontology)
                                } else {
                                    
                                    # get an aligning mapping
                                    aligning.mappings.list = get.mappings.to.align.ontologies(strat.dimnames, target.ontology)
                                    if (!is.null(aligning.mappings.list)) {
                                        strat.to.common.mapping = aligning.mappings.list[[1]]
                                        target.to.common.mapping.placeholder = aligning.mappings.list[[2]]
                                    }
                                } 
                            } else {
                                strat.to.target.mapping = get.ontology.mapping(strat.dimnames, target.ontology)
                            }
                        }
                        
                        # Skip this stratification if mappings were needed and couldn't be found
                        if (!is.null(target.ontology) &&
                            (is.null(strat.to.target.mapping) && is.null(strat.to.common.mapping)))
                            next
                        
                        # Figure out the to.dimnames for when we apply a mapping/subset
                        # This will also help us check whether the dimensions for this stratification are correct post-mapping
                        dimnames.for.apply = NULL
                        
                        if (is.null(target.ontology)) {
                            
                            # In this case, check that the stratification has exactly keep.dimensions + dimension.values dimensions
                            if (!setequal(strat.dimensions, union(keep.dimensions, names(dimension.values))))
                                next
                            
                            dimnames.for.apply = strat.dimnames
                            dimnames.for.apply[names(dimension.values)] = dimension.values
                            
                        } else {

                            dimnames.for.apply = target.ontology
                            dimnames.for.apply[names(dimension.values)] = dimension.values
                            
                            # Remove extra dimensions target.ontology may have brought that are not in keep.dimensions or dimension.values
                            # We checked at the beginning that the target.ontology contains both keep.dimensions and the dimension.values dimensions
                            # Only do this if keep.dimensions is NULL, because otherwise we want all of the dimensions of target ontology.
                            if (!is.null(keep.dimensions))
                                dimnames.for.apply = dimnames.for.apply[names(dimnames.for.apply) %in% union(keep.dimensions, names(dimension.values))]
                            
                            if (allow.mapping.from.target.ontology)
                                
                                if (is.null(target.to.common.mapping))
                                    dimnames.for.apply = target.to.common.mapping.placeholder$apply.to.dim.names(dimnames.for.apply)
                                else
                                    dimnames.for.apply = target.to.common.mapping$apply.to.dim.names(dimnames.for.apply)
                            
                            # In this case, check that the stratification has exactly the dimnames.for.apply dimensions
                            if (!setequal(strat.dimensions, names(dimnames.for.apply)))
                                next
                            
                        }

                        # Check that there are data (not all NA) after any ontology is applied
                        pull.data = strat.data
                        if (all(is.na(pull.data))) next
                        
                        ### SUCCESS FOR THIS ONTOLOGY ###
                            
                        # Save the target.to.common.mapping if we discovered one, and the mapped ontology as the common ontology
                        if (!is.null(target.to.common.mapping.placeholder)) {
                            target.to.common.mapping <<- target.to.common.mapping.placeholder
                            common.ontology <<- strat.to.common.mapping$apply.to.ontology(strat.dimnames)
                        }
                        
                        # Save the dimensions of the stratification as keep.dimensions if keep.dimensions was NULL
                        # This is unnecessary if there is a target.ontology
                        if (!is.null(keep.dimensions) && is.null(target.ontology)) {
                            keep.dimensions <<- strat.dimensions
                        }
                        
                        data.elements.accessors = 'data'
                        if ('url' %in% append.attributes)
                            data.elements.accessors = append(data.elements.accessors, 'url')
                        if ('details' %in% append.attributes)
                            data.elements.accessors = append(data.elements.accessors, 'details')

                        # Apply mapping to data and subset in one step
                        data.to.return = lapply(data.elements.accessors, function(a) {
                            
                            # default
                            function.to.apply = 'sum'
                            
                            if (is.null(target.ontology)) {
                                
                                data.to.process = private[[paste0('i.', a)]][[outcome]][[x]][[y]][[strat]]
                                mapping.to.apply = get.identity.ontology.mapping()
                                
                            } else {
                            
                                if (a == 'data') {
                                    
                                    data.to.process = pull.data
                                    
                                } else {
                                    
                                    data.to.process = private[[paste0('i.', a)]][[outcome]][[x]][[y]][[strat]]
                                    function.to.apply = function(b) {list(unique(unlist(b)))}
                                    
                                }
                                
                                if (allow.mapping.from.target.ontology) {
                                    
                                    mapping.to.apply = strat.to.common.mapping
                                    
                                } else {
                                    
                                    mapping.to.apply = strat.to.target.mapping
                                    
                                }
                            }

                            data.temp = mapping.to.apply$apply(
                                data.to.process,
                                na.rm = na.rm,
                                to.dim.names = dimnames.for.apply,
                                fun = function.to.apply
                            )
                            
                            if (!is.null(target.ontology) && a != 'data') {
                                data.temp = lapply(data.temp, function(b) {b[[1]]})
                                dim(data.temp) = sapply(dimnames.for.apply, length)
                                dimnames(data.temp) = dimnames.for.apply
                            }
                            
                            data.temp
                            
                        })
                        
                        names(data.to.return) = data.elements.accessors
                        
                        # Dims and dimnames before the following transformations
                        initial.dim = sapply(dimnames.for.apply, length)
                        initial.dimnames = dimnames.for.apply

                        for (d in seq_along(data.to.return)) {
                            
                            # These will change a couple times in the next steps here
                            current.dim = initial.dim
                            current.dimnames = initial.dimnames
                            
                            # Drop length 1 dimensions that aren't in keep.dimensions
                            dimensions.to.drop = intersect(
                                which(current.dim == 1),
                                which(!(names(current.dim) %in% keep.dimensions)))
                            
                            if (length(dimensions.to.drop) > 0) {
                                current.dim = current.dim[!(names(current.dim) %in% dimensions.to.drop)]
                                current.dimnames = current.dimnames[!(names(current.dimnames) %in% dimensions.to.drop)]
                                
                                data.to.return[[d]] = array(
                                    data.to.return[[d]],
                                    dim = current.dim,
                                    dimnames = current.dimnames)
                                
                            }
                            
                            # check if NEED to aggregate (i.e., any dimensions not in keep.dimensions)
                            if (length(current.dim) > length(keep.dimensions)) {
                                
                                if (names(data.to.return)[[d]] == 'data') {
                                    scale = private$i.outcome.info[[outcome]][['metadata']][['scale']]
                                    
                                    if (scale %in% c('non.negative.number', 'number')) {

                                        data.to.return[[d]] = apply(data.to.return[[d]], keep.dimensions, FUN = sum, na.rm=na.rm)
                                        
                                    } else if (scale %in% c('rate', 'time', 'proportion')) {
                                        
                                        # We'll do weighted averages with a denominator value as weight.
                                        denominator.outcome = private$i.outcome.info[[outcome]][['denominator.outcome']]
                                        
                                        if (is.null(target.ontology))
                                            denominator.ontology = strat.dimnames
                                        else {
                                            if (allow.mapping.from.target.ontology)
                                                denominator.ontology = common.ontology
                                            else
                                                denominator.ontology = target.ontology
                                        }
                                        
                                        # Recursive call to pull -- might not find anything
                                        denominator.array = self$pull(
                                            outcome = denominator.outcome,
                                            keep.dimensions = names(current.dimnames),
                                            dimension.values = dimension.values,
                                            sources = x,
                                            target.ontology = denominator.ontology,
                                            allow.mapping.from.target.ontology = FALSE,
                                            from.ontology.names = NULL,
                                            append.attributes = NULL,
                                            na.rm = na.rm)

                                        # If no denominator data found, break from the loops for data type, stratification, and ontology and return NULL for the whole source
                                        if (is.null(denominator.array)) {
                                            source.lacks.denominator.data.flag = TRUE
                                            data.to.return = NULL
                                            break
                                        }
                                        
                                        # Since the denominator.array came from only one source, we can remove the source so that it will match size of data
                                        denominator.array = array(
                                            denominator.array,
                                            dim = dim(denominator.array)[names(dim(denominator.array)) != 'source'],
                                            dimnames = dimnames(denominator.array)[names(dimnames(denominator.array)) != 'source']
                                        )
                                        
                                        # Catch an otherwise invisible bug if denominator.array somehow doesn't have the same shape/order as the data
                                        if (!identical(dimnames(denominator.array), dimnames(data.to.return[[d]])))
                                            stop(paste0(error.prefix, 'bug in aggregation code: denominator array has incorrect dimensions'))
                                        
                                        # We should find totals by aggregating the denominator.array rather than pulling less stratified data
                                        # because less stratified data might not equal the sum of the more stratified data in denominator.array
                                        denominator.totals.array = apply(denominator.array, keep.dimensions, FUN = sum, na.rm=na.rm)
                                        
                                        # Generate an array that multiplies every cell in data.to.return[[d]] by its weight from denominator.array
                                        # Caution: are the order of the dimensions the same, so that the values align?
                                        weighted.value.array = data.to.return[[d]] * denominator.array
                                        
                                        # Take the sum of the weighted values
                                        data.to.return[[d]] = apply(weighted.value.array, keep.dimensions, FUN = sum, na.rm=na.rm)
                                        
                                        current.dim = dim(data.to.return[[d]])
                                        current.dimnames = dimnames(data.to.return[[d]])
                                        
                                        # Divide by the denominator.totals.array values to finish the weighted average
                                        # fix
                                        data.to.return[[d]] = data.to.return[[d]] / denominator.totals.array
                                        
                                    } else if (scale == 'ratio') {
                                        stop(paste0(error.prefix, scale, ' data cannot be aggregated'))
                                    }
                                    
                                    else {
                                        
                                        stop(paste0(error.prefix, 'aggregating with the ', scale, ' scale is not yet implemented'))
                                        
                                    }
                                    
                                } else {
                                    
                                    data.to.return[[d]] = apply(
                                        data.to.return[[d]],
                                        keep.dimensions,
                                        function(x) {
                                            list(unique(unlist(x)))
                                        })
                                    
                                    current.dim = dim(data.to.return[[d]])
                                    current.dimnames = dimnames(data.to.return[[d]])
                                    
                                    # fix apply's annoying behavior
                                    data.data = lapply(
                                        data.to.return[[d]],
                                        function(x) {x[[1]]})
                                    
                                    data.to.return[[d]] = array(
                                        data.data,
                                        dim = current.dim,
                                        dimnames = current.dimnames) 
                                }
                            }
                                
                        } # end of loop for data types
                        
                        # If we found a match, or lack denominator data, then we won't search any more stratifications for this ontology
                        break
                                
                    } # end of loop for stratification
                    
                    # If we found a match, or lack denominator data, then we won't search any more ontologies for this source
                    if (!is.null(data.to.return) || source.lacks.denominator.data.flag) break
                    
                } # end of loop for ontology
                
                data.to.return
                
            }) # end of lapply for sources
                
            # we have a list (one element per source) of lists (one element per data type, i.e. 'data', 'url', or 'details')
            # repackage this to be a data array with 'url', 'details' and possibly a mapping as attributes
            final.return = NULL


            # Extract data for data, url, and details out of what lapply returned above
            for (data.type in names(return.data[[1]])) {
                
                # make a list of the data from the sources
                pull.return.data.list = lapply(return.data, function(x) {x[[data.type]]})
                names(pull.return.data.list) = sources.used.names
                dim.names.pull = c(dimnames(pull.return.data.list[[1]]), list(source=sources.used.names))
                
                pull.return.data = NULL
                
                if (data.type == 'data') {
                    pull.return.data = sapply(pull.return.data.list, function(x) {x})
                    
                } else {
                    for (src.data in pull.return.data.list) {
                        pull.return.data = append(pull.return.data, src.data)
                    }
                }
                
                dim(pull.return.data) = sapply(dim.names.pull, length)
                dimnames(pull.return.data) = dim.names.pull
                
                # incorporate it into final.return
                if (data.type == 'data') {
                    final.return = pull.return.data
                } else if (data.type == 'url') {
                    attr(final.return, 'url') = pull.return.data
                } else if (data.type == 'details') {
                    attr(final.return, 'details') = pull.return.data
                }
                
            }
            
            # add mapping
            if (!is.null(target.to.common.mapping))
                attr(final.return, 'mapping') = target.to.common.mapping
            
            #-- Return --#
            final.return
        },
        
        get.outcome.pretty.names = function(outcomes)
        {
            sapply(private$i.outcome.info[outcomes], function(info){
                info$pretty.name
            })
        },
        
        get.outcome.units = function(outcomes)
        {
            sapply(private$i.outcome.info[outcomes], function(info){
                info$units
            })
        },
        
        get.outcome.descriptions = function(outcomes)
        {
            sapply(private$i.outcome.info[outcomes], function(info){
                info$description
            })
        },
        
        get.registered.ontology = function(ontology.name)
        {
            # ontology.name must be in the list of registered ontologies
            if (!ontology.name %in% names(private$i.ontologies))
                stop(paste0(ontology.name, " is not a registered ontology"))
            
            rv = private$i.ontologies[[ontology.name]]
            
        },
        
        get.source.full.names = function(sources)
        {
            sapply(private$i.source.info[sources], function(info){
                info$full.name
            })
        },
        
        get.source.short.names = function(sources)
        {
            sapply(private$i.source.info[sources], function(info){
                info$short.name
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
        # [[outcome]][[source]][[ontology.name]][[stratification]]
        # for i.data, each element of this depth-4 access is a numeric array
        # for i.url and i.details, each element is a list array (ie a list with dimnames set), each element of which is a character vector
        i.data = NULL,
        i.url = NULL,
        i.details = NULL,
        
        #-- Metadata --#
        # These are named lists, with the names being the names of outcomes or data groups
        i.outcome.info = NULL,
        i.source.info = NULL,
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
                                               ontology.name,
                                               return.as.dimensions) #if true, returns the vector of dimensions. If false, collapses to the stratification name
        {
            dimensions = intersect(names(private$i.ontologies[[ontology.name]]),
                                   union(names(dimension.values), 
                                         union(names(dimnames(data)), keep.dimensions)))
            
            if (return.as.dimensions)
                dimensions
            else
                paste0(dimensions, collapse='__')
        },
        
        prepare.put.dim.names = function(dim.names,
                                         ontology.name)
        {
            ont = private$i.ontologies[[ontology.name]]
            
            ontology.dimensions.complete = is_complete(ont)
            rv = lapply(names(dim.names), function(d){
                if (ontology.dimensions.complete[d])
                    ont[[d]]
                else
                    sort(dim.names[[d]])
            })
            names(rv) = names(dim.names)
            
            rv
        }
        
    )
)
