
DATA.MANAGER.CODE.ITERATION = 3

# Depends on:
# R6 package
# HELPERS_misc_helpers
# HELPERS_dim_name_helpers
# HELPERS_array_helpers

if (!exists('default.data.manager.holder'))
{
    default.data.manager.holder = new.env()
    default.data.manager.holder$default.data.manager = NULL
}

DATA.MANAGER.ONTOLOGY.ERRORS = new.env()
DATA.MANAGER.ONTOLOGY.ERRORS$did.you.remember.details = list()

#'@title Get Ontology Error Debug Info
#'@description Returns information useful for debugging ontology mapping errors
#'@export
get.ontology.error.debug.info <- function()
{
    DATA.MANAGER.ONTOLOGY.ERRORS$did.you.remember.details # there's not an issue returning an environment, is there?
}

#'@title Clear Ontology Error Debug Info
#'@export
clear.ontology.error.debug.info <- function()
{
    DATA.MANAGER.ONTOLOGY.ERRORS$did.you.remember.details = list()
}

##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##

#'@title Get the default data manager
#'
#'@export
get.default.data.manager <- function()
{
    default.data.manager.holder$default.data.manager
}

#'@title Set the default data manager
#'
#'@export
set.default.data.manager <- function(data.manager)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    default.data.manager.holder$default.data.manager = data.manager
}

#'@title Create a JHEEM Data Manager
#'
#'@param name The name of the data manager
#'@param description A short description
#'
#'@export
create.data.manager <- function(name,
                                description,
                                set.as.default = F)
{
    new.data.manager = JHEEM.DATA.MANAGER$new(name,
                                              description=description)
    if (set.as.default) default.data.manager.holder$default.data.manager = new.data.manager
    invisible(new.data.manager)
}

#'@title Load a cached data manager
#'
#'@param file A file storing a cached data manager object (only)
#'@param name A name for the local data manager. If NULL, the name of the cached data manager.
#'@param description A description for the local data manager. If NULL, the description of the cached data manager.
#'@param set.as.default Should the loaded data manager be set as the default data manager
#'
#'@export
load.data.manager <- function(file,
                              name=NULL,
                              description=NULL,
                              set.as.default=F)
{
    error.prefix = "Error loading cached data manager: "
    imported.object.names = suppressWarnings(load(file))
    if (length(imported.object.names) != 1) {
        stop(paste0(error.prefix, "'file' must store exactly one cached R object"))
    }
    
    loaded.data.manager = get(imported.object.names[[1]])
    
    if (!R6::is.R6(loaded.data.manager) || !is(loaded.data.manager, 'jheem.data.manager'))
        stop(paste0(error.prefix, "'file' must store an R6 object with class 'jheem.data.manager'"))
    
    copy.name = ifelse(is.null(name),
                       loaded.data.manager$name,
                       name)
    copy.description = ifelse(is.null(description),
                              loaded.data.manager$description,
                              description)
    new.data.manager = copy.data.manager(loaded.data.manager,
                                         name=copy.name,
                                         description=copy.description,
                                         preserve.original.creation.date=T,
                                         preserve.original.last.modified.date=T)
    
    if (set.as.default)
        default.data.manager.holder$default.data.manager = new.data.manager
    
    
    invisible(new.data.manager)
}

#'@title Create a copy of a JHEEM Data Manager
#'
#'@param data.manager A jheem.data.manager object to be copied
#'@param name The name of the new data manager
#'@param description A short description of the new data manager. If NULL, the description of the copied data manager.
#'@param set.as.default Should the new data manager be set as the default data manager
#'@param preserve.original.creation.date,preserve.original.last.modified.date Should the new data manager keep the copied data manager's creation date/last modified date (T) or set that date to now (F)?
#'
#'@export
copy.data.manager <- function(data.manager = get.default.data.manager(),
                              name,
                              description,
                              set.as.default=F,
                              preserve.original.creation.date=T,
                              preserve.original.last.modified.date=T)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    new.data.manager = JHEEM.DATA.MANAGER$new(name=name,
                                              description=description,
                                              copy.from.data.manager = data.manager,
                                              preserve.copied.data.manager.creation.date = preserve.original.creation.date,
                                              preserve.copied.data.manager.last.modified.date = preserve.original.last.modified.date)
    
    if (set.as.default) default.data.manager.holder$default.data.manager = new.data.manager
    invisible(new.data.manager)
}

#'@title Import data from another JHEEM Data Manager
#'
#'@param to.data.manager A jheem.data.manager object
#'@param from.data.manager A jheem.data.manager object
#'
#'@export
import.data <- function(to.data.manager = get.default.data.manager(),
                        from.data.manager)
{
    if (!R6::is.R6(to.data.manager) || !is(to.data.manager, 'jheem.data.manager'))
        stop("'to.data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    to.data.manager$import.data(from.data.manager)
}

#'@title Subset a JHEEM Data Manager
#'
#'@param data.manager A jheem.data.manager object
#'@param dimension.values A named list that indicates what subset of the data should be kept. The values of dimension.values must be character vectors unless an 'ontology.name' is specified, in which case logical and numeric vectors may also be used.
#'@param ontology.name The name of a registered ontology for which data should be subset. If NULL, data from all ontologies will be subset.
#'
#'@export
subset.data <- function(data.manager = get.default.data.manager(),
                        dimension.values,
                        ontology.name)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$subset(dimension.values=dimension.values,
                        ontology=ontology)
}

#'@title Register a new data ontology to a data manager before putting data to it
#'
#'@param data.manager A jheem.data.manager object
#'@param name The name of the data group
#'@param ont An ontology object as created by \code{\link{ontology()}}
#'
#'@export
register.data.ontology <- function(data.manager = get.default.data.manager(),
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
#'@param denominator.lags.by.one.year Flag indicating whether the denominator data is shifting one year earlier than this outcome's data. For example, data for this outcome in year 2020 will use the 2019 denominator data if this flag is set to TRUE.
#'@param overwrite A logical indicating whether the information on this outcome should overwrite previously-registered information about this outcome. However, this registration must include the same metadata$scale and denominator.outcome (ie, can't change the structure of the outcome, only the display 'trappings')
#'@param allow.missing.denominator.outcome A logical allowing the user to register an outcome of scale 'rate', 'proportion' or 'time' without supplying a denominator outcome
#'
#'@export
register.data.outcome <- function(data.manager = get.default.data.manager(),
                                  outcome,
                                  metadata,
                                  denominator.outcome=NULL,
                                  denominator.lags.by.one.year=F,
                                  overwrite = F,
                                  allow.missing.denominator.outcome = F)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.outcome(outcome = outcome,
                                  metadata = metadata,
                                  denominator.outcome = denominator.outcome,
                                  denominator.lags.by.one.year = denominator.lags.by.one.year,
                                  overwrite = overwrite,
                                  allow.missing.denominator.outcome = allow.missing.denominator.outcome)
}

#'@title Register a data source to a data manager before putting data from that source
#'
#'@param data.manager A jheem.data.manager object
#'@param source The name (a single character value) of the source. This is the 'internal' name by which the source will be referenced in accessing the data manager
#'@param parent.source A parent source registered with this data manager that reflects the origin of the data this source uses
#'@param full.name A descriptive, fully-formatted and capitalized name to use in generating figures and tables (eg in popovers). Avoid abbreviations. Should be unique to the data source (although this is not enforced)
#'@param short.name A name for the data source to use in setting where brevity is important. Ideal to use abbreviations, but can be the same as full.name
#'
#'@details Note: a source is conceived such that one source cannot contain two sets of data for the same set of dimension values
#'
#'@export
register.data.source <- function(data.manager = get.default.data.manager(),
                                 source,
                                 parent.source,
                                 full.name,
                                 short.name = full.name)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.source(source=source,
                                 parent.source=parent.source,
                                 full.name=full.name,
                                 short.name=short.name)
}

#'@title Register a parent data source to a data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param parent.source The name (a single character value) of the parent source. This is the 'internal' name by which the parent source will be referenced in accessing the data manager
#'@param full.name A descriptive, fully-formatted and capitalized name to use in generating figures and tables (eg in popovers). Avoid abbreviations. Should be unique to the parent data source (although this is not enforced)
#'@param short.name A name for the parent data source to use in setting where brevity is important. Ideal to use abbreviations, but can be the same as full.name
#'
#'@export
register.parent.data.source <- function(data.manager = get.default.data.manager(),
                                        parent.source,
                                        full.name,
                                        short.name = full.name)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.source(parent.source=parent.source,
                                 full.name=full.name,
                                 short.name=short.name)
}

#'@title Put data into a data manager
#'
#'@inheritParams distribute.dimension.values
#'@param data.manager A jheem.data.manager object
#'@param data A numeric array or scalar value containing the data to store. If it is an array, it must have named dimnames set
#'@param outcome The outcome type for the data. Must be an outcome previously registered with \code{\link{register.data.outcome}}
#'@param metric The type of measurement. The default value is "estimate", but other options include "cv", "variance", and "sd".
#'@param ontology.name The name of the ontology which the data follow. Must be an ontology previously registered with \code{\link{register.data.ontology}}
#'@param dimension.values A named list that indicates what subset of a bigger data element these particular data should be stored into. Each element must be a named character, numeric, or logical vector.
#'@param source The (single) character name of the source from which these data derive. Must be a source previously registered with \code{\link{register.data.source}} Note: a source is conceived such that one source cannot contain two sets of data for the same set of dimension values
#'@param url A character vector with at least one element giving one or more URLs indicating where the data derive from
#'@param details A character vector with at least one element giving one or more URLs giving data collection details. In general, data collected and tabulated using the same approach should have the same details, whereas data with different methods/tabulation should have different details
#'@param allow.na.to.overwrite A logical indicator for whether NA values in data should be allowed to overwrite previous values put to the data manager (if data have been put previously)
#'
#'@export
put.data <- function(data.manager = get.default.data.manager(),
                     data,
                     outcome,
                     metric,
                     source,
                     ontology.name,
                     dimension.values=NULL,
                     url,
                     details,
                     allow.na.to.overwrite=F,
                     dimension.values.to.distribute=list(),
                     round.distributed.dimension.values=T,
                     debug=F,
                     printouts=F)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$put(data=data,
                     outcome=outcome,
                     metric=metric,
                     source=source,
                     ontology.name=ontology.name,
                     dimension.values=dimension.values,
                     url=url,
                     details=details,
                     allow.na.to.overwrite=allow.na.to.overwrite,
                     dimension.values.to.distribute = dimension.values.to.distribute,
                     round.distributed.dimension.values = round.distributed.dimension.values,
                     debug=F,
                     printouts=printouts)
}

#'@title Put long-form data into a data manager
#'
#'#'@inheritParams distribute.dimension.values
#'@inheritParams put.data
#'@param data A data frame or other 2-dimensional data structure with named columns. Must contain a column named 'value' of numeric values. If the 'source' argument is NULL, then data must also have a column named 'source' that gives the source for each row. May contain additional columns with names matching the specified ontology, which have the dimension values for each row. Any other columns are ignored
#'
#'@export
put.data.long.form <- function(data.manager = get.default.data.manager(),
                               data,
                               outcome,
                               metric,
                               source,
                               ontology.name,
                               dimension.values=NULL,
                               url,
                               details,
                               allow.na.to.overwrite=F,
                               dimension.values.to.distribute=list(),
                               round.distributed.dimension.values=T,
                               debug=F,
                               printouts=F)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$put.long.form(data=data,
                               outcome=outcome,
                               metric=metric,
                               source=source,
                               ontology.name=ontology.name,
                               dimension.values=dimension.values,
                               url=url,
                               details=details,
                               allow.na.to.overwrite=allow.na.to.overwrite,
                               dimension.values.to.distribute = dimension.values.to.distribute,
                               round.distributed.dimension.values=round.distributed.dimension.values,
                               debug=F,
                               printouts=printouts)
}

#'@title Pull data from a data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param outcome The outcome type for the data. Must be an outcome previously registered to this data manager with \code{\link{register.data.outcome}}
#'@param metric The type of measurement. The default value is "estimate", but other options include "cv", "variance", and "sd".
#'@param keep.dimensions The dimensions that should be retained in the returned value
#'@param dimension.values A named list, indicating which values for specific dimensions to pull data for. Each element must be a named character, numeric, or logical vector.
#'@param sources The data sources from which to pull data (if available). If NULL, will pull from all data sources that have any relevant data
#'@param include.sources.without.data.in.output When 'sources' is not NULL, include all sources in 'sources' in the output array even if no data was found for some, or even all, sources. If 'sources' is NULL, will include all registered sources for the outcome in the output array.
#'@param target.ontology Optional argument, indicating the ontology according to which results are desired. The data manager will apply an ontology mapping (if it can) to align its data to the desired ontology
#'@param allow.mapping.from.target.ontology A logical indicator. If TRUE, if target.ontology is specified, but the data manager does not have data that can be mapped to the target ontology, it will search for data such that an ontology.mapping can be applied to data in the target.ontology that make those data align with the data pulled
#'@param from.ontology.names The names of the ontologies from which to pull data (if available). If NULL, will pull from all ontologies that have any relevant data and can be mapped to the requested ontology. Must refer to ontologies previously registered to this data manager with \code{\link{register.data.ontology}}
#'@param append.attributes A character vector indicating requested data attributes to include with the result. May be either "details", "url", or both
#'
#'@param na.rm Whether to disregard NA values when aggregating out dimensions not in keep.dimensions
#'@param ignore.ontologies.without.requested.locations If 'dimension.values' includes a location dimension, should ontologies that don't have any of those locations for the requested outcome be ignored? See detailed documentation about how ontologies factor into the output. (NOT YET WRITTEN)
#'@param ... Optional alternative way to specify dimension.values. Each element must be a named character, numeric, or logical vector.
#'
#'@return A numeric array with named dimnames, with one dimension for each value of 'keep.dimensions', plus an additional dimension, "source" at the beginning, with one value for each source from which data were pulled
#' The return value may have several attributes:
#' (1) If target.ontology is not-NULL and allow.mapping.from.target.ontology==T, 'ontology.mapping' with refer to an ontology.mapping object that can be applied to data in the target.ontology to make them align with the returned array
#' (2) If append.attributes includes "url", an array with the same dimensions as the returned array, where each element is one or more URLs separated by "__"
#' (3) If append.attributes includes "details", an with the same dimensions as the returned array, where each element is one or more details about data collection separated by "__"
#'
#'@export
pull.data <- function(data.manager = get.default.data.manager(),
                      outcome,
                      metric = 'estimate',
                      keep.dimensions = NULL,
                      dimension.values = NULL,
                      sources = NULL,
                      from.ontology.names = NULL,
                      exclude.ontology.names = NULL,
                      target.ontology = NULL,
                      allow.mapping.from.target.ontology = T,
                      append.attributes = NULL,
                      append.metrics = NULL,
                      allow.other.sources.for.denominator = F,
                      na.rm = F,
                      ignore.ontologies.without.requested.locations = T,
                      check.arguments = T,
                      debug = F,
                      ...)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$pull(outcome = outcome,
                      metric = metric,
                      keep.dimensions = keep.dimensions,
                      dimension.values = dimension.values,
                      sources = sources,
                      from.ontology.names = from.ontology.names,
                      target.ontology = target.ontology,
                      allow.mapping.from.target.ontology = allow.mapping.from.target.ontology,
                      append.attributes = append.attributes,
                      append.metrics = append.metrics,
                      allow.other.sources.for.denominator = allow.other.sources.for.denominator,
                      na.rm = na.rm,
                      ignore.ontologies.without.requested.locations = ignore.ontologies.without.requested.locations,
                      check.arguments = check.arguments,
                      debug = debug,
                      ...)
}

#' @title Remove Data From Data Manager
#' @description Replaces data with NA if data exists
#' @inheritParams put.data
#' @param details.for.removal,url.for.removal Single character values to use for these points' 'details' and 'url'
#' @export
remove.data = function(data.manager,
                       outcome,
                       dimension.values,
                       source,
                       ontology.name,
                       metric = 'estimate',
                       details.for.removal='removed',
                       url.for.removal='url') {
    
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    # Put NAs with overwrite=T
    data.manager$put(as.numeric(NA),
                     outcome=outcome,
                     metric=metric,
                     source=source,
                     ontology.name=ontology.name,
                     dimension.values=dimension.values,
                     details=details.for.removal,
                     url=url.for.removal,
                     allow.na.to.overwrite = T,
                     is.removal = T
    )
    
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
get.data.outcome.pretty.names <- function(data.manager = get.default.data.manager(), outcomes)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.outcome.pretty.names(outcomes)
}

#'@describeIn get.data.outcome.pretty.names Get the labels for outcomes
#'@export
get.data.outcome.labels <- function(data.manager = get.default.data.manager(), outcomes)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.outcome.labels(outcomes)
}

#'@describeIn get.data.outcome.pretty.names Get descriptions for outcomes
#'@export
get.data.outcome.descriptions <- function(data.manager = get.default.data.manager(), outcomes)
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
get.data.source.full.names <- function(data.manager = get.default.data.manager(), sources)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.source.full.names(sources)
}

#'@describeIn get.data.source.full.names Get short names for data sources
#'@export
get.data.source.short.names <- function(data.manager = get.default.data.manager(), sources)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.source.short.names(sources)
}

#'param ontology.name The name of an ontology (must be registered in the data manager)
#'
#'@export
get.registered.ontology <- function(data.manager = get.default.data.manager(), ontology.name)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.registered.ontology(ontology.name)
}

#'@title Get year bounds for outcome
#'
#'@details Gets the earliest and latest years for which a data manager has data for a given outcome and metric. Searches all sources and all ontologies that haven't been specifically excluded
#'
#'@param outcome The outcome for which to find earliest and latest years of data (must be registered in the data manager)
#'@param metric The type of measurement to find earliest and latest years of data within the outcome. The default value is "estimate", but other options include "cv", "variance", and "sd".
#'@param exclude.ontology.names The names of ontologies to exclude from this search. Must be NULL or a character vector with no NA's or empty values.
#'
#'@export
get.year.bounds.for.outcome <- function(data.manager = get.default.data.manager(), outcome, metric='estimate', exclude.ontology.names = NULL)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    data.manager$get.year.bounds.for.outcome(outcome,
                                             metric=metric,
                                             exclude.ontology.names=exclude.ontology.names)
}

#'@title Get locations with data for an outcome
#'
#'@details Get locations that have data for a given outcome, metric and set of years. Searches all sources and all ontologies that haven't been specifcally excluded
#'
#'@inheritParams get.year.bounds.for.outcome
#'@param years The years to include. May be NULL to search all available years, or a numeric vector with no NA's or duplicates.
#'@export
get.locations.with.data <- function(data.manager = get.default.data.manager(), outcome, metric='estimate', years, exclude.ontology.names = NULL)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    data.manager$get.locations.with.data(outcome, metric=metric, years=years)
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
        
        initialize = function(name, description, copy.from.data.manager=NULL, preserve.copied.data.manager.creation.date=T, preserve.copied.data.manager.last.modified.date=T)
        {
            # Validate arguments
            # *name* is a single, non-empty, non-NA character value
            if (!is.character(name) || length(name)!=1 || is.na(name) || nchar(name)==0)
                stop(paste0(error.prefix, "'name' must be a single, non-empty, non-NA character value"))
            
            # *description* must be a single, non-empty, non-NA character value
            if (!is.character(description) || length(description)!=1 || is.na(description) || nchar(description)==0)
                stop(paste0(error.prefix, "'description' must be a single, non-empty, non-NA character value"))
            
            # Store them
            private$i.name = name
            private$i.description = description
            
            
            private$i.code.iteration = DATA.MANAGER.CODE.ITERATION
            
            if (is.null(copy.from.data.manager))
            {
                # Create the empty values for other member variables
                private$i.data = list()
                private$i.url = list()
                private$i.details = list()
                private$i.url.list = character(0)
                private$i.details.list = character(0)
                
                private$i.outcome.info = list()
                private$i.ontologies = list()
                private$i.parent.source.info = list()
                private$i.source.info = list()
            }
            else
            {
                #               if (is.null(copy.from.data.manager$code.iteration) ||
                #                   private$i.code.iteration != copy.from.data.manager$code.iteration)
                #               {
                #                   stop("Cannot copy from data manager: the code iterations are incompatible")
                #               }
                
                private$i.data = copy.from.data.manager$data
                private$i.url = copy.from.data.manager$url
                private$i.details = copy.from.data.manager$details
                private$i.url.list = unlist(copy.from.data.manager$url.list)
                private$i.details.list = unlist(copy.from.data.manager$details.list) # needed during rollout of vector form
                
                private$i.outcome.info = copy.from.data.manager$outcome.info
                private$i.ontologies = copy.from.data.manager$ontologies
                private$i.parent.source.info = copy.from.data.manager$parent.source.info
                private$i.source.info = copy.from.data.manager$source.info
            }
            
            # Set creation date and last modified date to now; make backwards compatible
            if (!is.null(copy.from.data.manager) && preserve.copied.data.manager.creation.date)
                private$i.creation.date = copy.from.data.manager[['creation.date']]
            else
                private$i.creation.date = Sys.time()
            
            if (!is.null(copy.from.data.manager) && preserve.copied.data.manager.last.modified.date)
                private$i.last.modified.date = copy.from.data.manager[['last.modified.date']]
            else
                private$i.last.modified.date = Sys.time()
            
        },
        
        import.data = function(from.data.manager)
        {
            error.prefix = "Can't import data from data manager: "
            if (!R6::is.R6(from.data.manager) || !is(from.data.manager, 'jheem.data.manager'))
                stop("'from.data.manager' must be an R6 object with class 'jheem.data.manager'")
            
            # register ontologies if necessary
            for (ontology.name in from.data.manager$ontology.names) {
                if (!(ontology.name %in% self$ontology.names))
                    self$register.ontology(ontology.name,
                                           from.data.manager$ontologies[[ontology.name]])
                else {
                    from.ont = from.data.manager$ontologies[[ontology.name]]
                    self.ont = self$ontologies[[ontology.name]]
                    # check that these two ontologies really are the same
                    if (!identical(from.ont[setdiff(names(from.ont), incomplete.dimensions(from.ont))],
                                   self.ont[setdiff(names(self.ont), incomplete.dimensions(self.ont))]))
                        stop(paste0(error.prefix, "The corresponding ontologies for '", ontology.name, "' do not have identical complete dimensions"))
                    if (!identical(names(from.ont), names(self.ont)))
                        stop(paste0(error.prefix, "The corresponding ontologies for '", ontology.name, "' do not have the same dimensions (in order) as each other"))
                    # The put() will handle updating the incomplete dimensions
                }
            }
            
            # register outcomes if necessary
            for (outcome.name in names(from.data.manager$outcome.info)) {
                if (!(outcome.name %in% names(self$outcome.info)))
                    self$register.outcome(outcome = from.data.manager$outcome.info[[outcome.name]][['outcome']],
                                          metadata = from.data.manager$outcome.info[[outcome.name]][['metadata']],
                                          denominator.outcome = from.data.manager$outcome.info[[outcome.name]][['denominator.outcome']])
            }
            
            # register parent sources if necessary
            for (parent.source.name in names(from.data.manager$parent.source.info)) {
                self$register.parent.source(parent.source = from.data.manager$parent.source.info[[parent.source.name]][['parent.source']],
                                            full.name = from.data.manager$parent.source.info[[parent.source.name]][['full.name']],
                                            short.name = from.data.manager$parent.source.info[[parent.source.name]][['short.name']])
            }
            
            # register sources if necessary
            for (source.name in names(from.data.manager$source.info)) {
                if (!(source.name %in% names(self$source.info)))
                    self$register.source(source = from.data.manager$source.info[[source.name]][['source']],
                                         parent.source = from.data.manager$source.info[[source.name]][['parent.source']],
                                         full.name = from.data.manager$source.info[[source.name]][['full.name']],
                                         short.name = from.data.manager$source.info[[source.name]][['short.name']])
            }
            
            # import data
            data = from.data.manager$data
            url = from.data.manager$url
            details = from.data.manager$details
            
            for (outcome in names(data)) {
                for (metric in names(data[[outcome]])) {
                    for (source in names(data[[outcome]][[metric]])) {
                        for (ontology in names(data[[outcome]][[metric]][[source]])) {
                            for (stratification in names(data[[outcome]][[metric]][[source]][[ontology]])) {
                                
                                # We must do one put PER unique pair of details/url, because every put() can have only one url and details
                                details.this.strat = details[[outcome]][[metric]][[source]][[ontology]][[stratification]]
                                url.this.strat = url[[outcome]][[metric]][[source]][[ontology]][[stratification]]
                                hashed.details.this.strat = unique(as.vector(details.this.strat))
                                hashed.url.this.strat = unique(as.vector(url.this.strat))
                                
                                for (one.details in hashed.details.this.strat) {
                                    for (one.url in hashed.url.this.strat) {
                                        if (is.na(one.details) || is.na(one.url)) next
                                        # browser()
                                        array.indices = which(details.this.strat==one.details & url.this.strat==one.url)
                                        
                                        array.indices[is.na(array.indices)] = F
                                        # Figure out what the details and url actually are in the from.data.manager
                                        unhashed.one.details = from.data.manager$unhash.details(one.details)
                                        unhashed.one.url = from.data.manager$unhash.url(one.url)
                                        
                                        # Because we have to put arrays, not vectors corresponding to sporadic locations in an array, we fill one with NA everywhere else
                                        one.data = array(NA,
                                                         dim(data[[outcome]][[metric]][[source]][[ontology]][[stratification]]),
                                                         dimnames(data[[outcome]][[metric]][[source]][[ontology]][[stratification]]))
                                        one.data[array.indices] = data[[outcome]][[metric]][[source]][[ontology]][[stratification]][array.indices]
                                        
                                        self$put(data = one.data,
                                                 outcome=outcome,
                                                 metric=metric,
                                                 source=source,
                                                 ontology.name=ontology,
                                                 url=unhashed.one.url,
                                                 details=unhashed.one.details,
                                                 allow.na.to.overwrite = F)
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            # Modified
            private$i.last.modified.date = Sys.time()
            invisible(self)
            
        },
        
        subset.data = function(dimension.values,
                               ontology.name=NULL)
        {
            # --- Validate arguments ---
            error.prefix = paste0("Unable to subset data.manager '", private$i.name, "': ")
            stop(paste0(error.prefix, "This function is not yet implemented"))
            # *ontology* is NULL or a single, non-empty, non-NA character value which refers to a registered ontology
            # *dimension.values* is a named list of character vectors. If an 'ontology.name' is specified, logical and numeric vectors may also be used.
            
            if (!is.null(ontology.name)) {
                if (!is.character(ontology.name) || length(ontology.name)!=1 || is.na(ontology.name) || nchar(ontology.name)==0)
                    stop(paste0, error.prefix, "'ontology.name' must be NULL or a single, non-empty, non-NA character value")
                ont = private$i.ontologies[[ontology.name]]
                if (is.null(ont))
                    stop(paste0(error.prefix, "'", ontology.name, "' is not a registered ontology"))
                dimension.values = resolve.ontology.dimension.values(ont = ont,
                                                                     dimension.values = dimension.values,
                                                                     error.prefix = paste0(error.prefix, "Error resolving 'dimension.values' - "))
            }
            else {
                check.dimension.values.valid(dimension.values,
                                             variable.name.for.error = 'dimension.values')
                for(d in names(dimension.values)) {
                    if(!is.character(dimension.values[[d]]))
                        stop(paste0(error.prefix, "each element of 'dimension.values' must be a character vector if 'ontology.name' is NULL"))
                }
            }
            
            # --- Subset data
            for (data.type in c('i.data', 'i.url', 'i.details')) {
                private[[data.type]] = lapply(private[[data.type]], function(outcome) {
                    lapply(outcome, function(metric) {
                        lapply(metric, function(source) {
                            # To make sure we get a named list for the source
                            ontology.iterator = seq_along(source)
                            names(ontology.iterator) = names(source)
                            
                            lapply(ontology.iterator, function(i) {
                                if (names(source)[[i]] == ontology.name || is.null(ontology.name)) {
                                    lapply(source[[i]], function(stratification) {
                                        dimensions.to.subset = dimension.values[names(dimension.values) %in% names(dim(stratification))]
                                        if (length(dimensions.to.subset) > 0) {
                                            dimnames.for.subset = dimnames(stratification)
                                            for (d in names(dimensions.to.subset))
                                                dimnames.for.subset[[d]] = dimensions.to.subset[[d]][dimensions.to.subset[[d]] %in% dimnames(stratification)[[d]]]
                                            if (all(sapply(dimnames.for.subset, length) > 0))
                                                fast.array.access(stratification, dimnames.for.subset)
                                            else stratification
                                        }
                                        else stratification
                                    })
                                }
                                else source[[i]]
                            })
                        })
                    })
                })
            }
            
            # Modified
            private$i.last.modified.date = Sys.time()
            
        },
        
        register.ontology = function(name, ont)
        {
            error.prefix = "Cannot register.ontology: "
            # Validate arguments
            # - name is a single, non-empty, non-NA character value
            # - ont is an ontology
            if (!is.character(name) || length(name)!=1 || is.na(name))
                stop(paste0(error.prefix, "'name' must be a single, non-NA character value"))
            if (!is.ontology(ont))
                stop(paste0(error.prefix, "'ont' must be an ontology"))
            
            if (name %in% names(private$i.ontologies)) {
                if (identical(ont, private$i.ontologies[[name]]))
                    return(invisible(self))
                else
                    stop(paste0(error.prefix, "an ontology with this name has already been registered in this data manager"))
            }
            
            # If this name has not already been registered, store it
            
            # Todd's old instructions. I'm choosing not to do this because
            # it's too complex to modify the data stored under the old ontology.
            
            # If it has, make sure the new ontology is a 'superset'
            # - all dimensions in the old are still present in the new
            # - dimensions present in the old have the same values as in the new
            # - complete dimensions present in the old are still complete, ditto for incomplete
            # If it is a superset, then store it. Otherwise throw an error
            
            #@need to implement
            
            # Store it in sorted form
            sorted.ont = as.ontology(lapply(ont, function(d) {sort(d)}), incomplete.dimensions=incomplete.dimensions(ont))
            private$i.ontologies[[name]] = sorted.ont
            
            # Modified
            private$i.last.modified.date = Sys.time()
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        register.outcome = function(outcome,
                                    metadata,
                                    denominator.outcome=NULL,
                                    denominator.lags.by.one.year=NULL,
                                    overwrite=F,
                                    allow.missing.denominator.outcome = F)
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
            
            # *denominator.lags.by.one.year* is NULL or a single logical value (if *denominator.outcome* isn't NULL)
            if (!is.null(denominator.outcome) && !is.null(denominator.lags.by.one.year) && (!is.logical(denominator.lags.by.one.year) || length(denominator.lags.by.one.year)!=1 || is.na(denominator.lags.by.one.year)))
                stop(paste0(error.prefix, "if 'denominator.outcome' is supplied, then 'denominator.lags.by.one.year' must be NULL or a single, non-NA logical value"))
            
            if (!is.logical(overwrite) || length(overwrite)!=1 || is.na(overwrite))
                stop(paste0(error.prefix, "'overwrite' must be a single, non-empty, non-NA logical value"))
            if (!is.logical(allow.missing.denominator.outcome) || length(allow.missing.denominator.outcome)!=1 || is.na(allow.missing.denominator.outcome))
                stop(paste0(error.prefix, "'allow.missing.denominator.outcome' must be a single, non-empty, non-NA logical value"))
            
            # - If metadata$scale is 'rate', 'proportion', or 'time',
            #   denominator outcome is non-NULL.
            #   Otherwise it is NULL
            # - If non-NULL, denominator outcome:
            # -- is a single, non-empty, non-NA character value
            # -- corresponds to a previously-registered outcome with scale 'non.negative.number'
            
            if (metadata$scale %in% c('rate', 'proportion', 'time', 'ratio')) {
                if (!is.character(denominator.outcome) || length(denominator.outcome)!=1 || is.na(denominator.outcome) ||
                    is.null(private$i.outcome.info[[denominator.outcome]]) || private$i.outcome.info[[denominator.outcome]]$metadata$scale != 'non.negative.number')
                    stop(paste0(error.prefix, "outcomes with scale 'rate', 'proportion', 'time', or 'ratio' must have a 'denominator.outcome' that corresponds to a previously-registered outcome with scale 'non.negative.number'"))
            }
            else {
                if (!is.null(denominator.outcome))
                    stop(paste0(error.prefix, "'denominator.outcome' must be NULL for outcomes of scale '", metadata$scale, "'"))
            }
            
            # If this outcome has not previously been registered, store it
            # Otherwise, if scale and denominator outcome are the same as before, ignore.
            #   If different, throw an error
            
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
            
            if (metadata$scale %in% c('rate', 'proportion', 'time', 'ratio') && is.null(denominator.outcome) && !allow.missing.denominator.outcome)
                stop(paste0(error.prefix, "'denominator.outcome' must be supplied for scale ", metadata$scale))
            
            # Store it
            outcome.info = list(
                outcome = outcome,
                metadata = metadata,
                denominator.outcome = denominator.outcome,
                denominator.lags.by.one.year = denominator.lags.by.one.year
            )
            
            private$i.outcome.info[[outcome]] = outcome.info
            
            # Modified
            private$i.last.modified.date = Sys.time()
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        register.source = function(source,
                                   parent.source,
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
            
            # - parent.source is a single, non-empty, non-NA character value and must be the name of a previously registered parent.source
            if (!is.character(source) || length(source)!=1 || is.na(source) || nchar(source)==0)
                stop(paste0(error.prefix, "'source' must be a single, non-empty, non-NA character value"))
            if (is.null(private$i.parent.source.info[[parent.source]]))
                stop(paste0(error.prefix, "'parent.source' must be a registered parent source with this data manager"))
            
            # - full.name is a single, non-empty, non-NA character value
            if (!is.character(full.name) || length(full.name)!=1 || is.na(full.name) || nchar(full.name)==0)
                stop(paste0(error.prefix, "'full.name' must be a single, non-empty, non-NA character value"))
            
            # - short.name is a single, non-empty, non-NA character value
            if (!is.character(short.name) || length(short.name)!=1 || is.na(short.name) || nchar(short.name)==0)
                stop(paste0(error.prefix, "'short.name' must be a single, non-empty, non-NA character value"))
            
            # If this source has not previously been registered, store it
            # Otherwise, if overwrite==T, store the new one
            # Otherwise, throw an error if full.name or short.name is different
            #   If different, throw an error
            
            previous.source.info = private$i.source.info[[source]]
            if (overwrite || is.null(previous.source.info))
            {
                source.info = list(
                    source = source,
                    parent.source = parent.source,
                    full.name = full.name,
                    short.name = short.name)
                
                private$i.source.info[[source]] = source.info
            }
            else if (previous.source.info$full.name != full.name || previous.source.info$short.name != short.name)
            {
                stop(paste0(error.prefix, "A data source named '", source, "' has already been registered. If you want to overwrite the previously registered source, use overwrite==T"))
            }
            
            # Modified
            private$i.last.modified.date = Sys.time()
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        register.parent.source = function(parent.source,
                                          full.name,
                                          short.name,
                                          overwrite=F)
        {
            #-- Validate arguments --#
            error.prefix = paste0("Unable to register parent source for data.manager '", private$i.name, "': ")
            
            # - parent.source is a single, non-empty, non-NA character value
            if (!is.character(parent.source) || length(parent.source)!=1 || is.na(parent.source) || nchar(parent.source)==0)
                stop(paste0(error.prefix, "'parent.source' must be a single, non-empty, non-NA character value"))
            error.prefix = paste0("Unable to register parent.source '", parent.source,
                                  "' for data.manager '", private$i.name, "': ")
            
            # - full.name is a single, non-empty, non-NA character value
            if (!is.character(full.name) || length(full.name)!=1 || is.na(full.name) || nchar(full.name)==0)
                stop(paste0(error.prefix, "'full.name' must be a single, non-empty, non-NA character value"))
            
            # - short.name is a single, non-empty, non-NA character value
            if (!is.character(short.name) || length(short.name)!=1 || is.na(short.name) || nchar(short.name)==0)
                stop(paste0(error.prefix, "'short.name' must be a single, non-empty, non-NA character value"))
            
            # If this parent source has not previously been registered, store it
            # Otherwise, if overwrite==T, store the new one
            # Otherwise, throw an error if full.name or short.name is different
            #   If different, throw an error
            
            previous.parent.source.info = private$i.parent.source.info[[parent.source]]
            if (overwrite || is.null(previous.parent.source.info))
            {
                parent.source.info = list(
                    parent.source = parent.source,
                    full.name = full.name,
                    short.name = short.name)
                
                private$i.parent.source.info[[parent.source]] = parent.source.info
            }
            else if (previous.parent.source.info$full.name != full.name || previous.parent.source.info$short.name != short.name)
            {
                stop(paste0(error.prefix, "A data parent source named '", parent.source, "' has already been registered. If you want to overwrite the previously registered parent source, use overwrite==T"))
            }
            
            # Modified
            private$i.last.modified.date = Sys.time()
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        put = function(data,
                       outcome,
                       metric = 'estimate',
                       source,
                       ontology.name,
                       dimension.values=NULL,
                       url,
                       details,
                       allow.na.to.overwrite=F,
                       dimension.values.to.distribute=list(),
                       round.distributed.dimension.values=T,
                       is.removal=F,
                       debug=F,
                       printouts=F)
        {
            if (printouts) {
                dots = "........"
                puttime=Sys.time()
                print(paste0("Beginning put for outcome '", outcome, "', source '", source, "' at ", puttime))
            }
            if (debug) browser()
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
            
            if (!is.logical(is.removal) || length(is.removal)!=1 || is.na(is.removal))
                stop(paste0(error.prefix, "'is.removal' must be T or F"))
            
            # 3) *data* is
            #   -- numeric
            #   -- appropriate for the scale of the outcome
            #   -- either a single scalar value, or an array with named dimnames
            #      where either
            #   --- the corresponding dimension in the ontology is incomplete OR
            #   --- the values are a subset of the values of the corresponding dimension in the ontology
            #   --- the dimension values for any dimension named "location" contains only valid locations
            if (!is.numeric(data))
                stop(paste0(error.prefix,
                            "'data' must be a numeric object"))
            if (length(data)==0)
                stop(paste0(error.prefix,
                            "'data' must have length > 0"))
            
            if (is.removal && (length(data)!=1 || !is.na(data)))
                stop(paste0(error.prefix,
                            "'data' must be 'NA' if 'is.removal' is TRUE"))
            
            ###
            if (metric %in% c('estimate', 'upper.bound', 'lower.bound'))
                do.check.values.for.model.scale(data, scale=outcome.info$metadata$scale,
                                                variable.name.for.error = 'data',
                                                error.prefix = error.prefix)
            else
                do.check.values.for.model.scale(data, scale = 'non.negative.number',
                                                variable.name.for.error = 'data',
                                                error.prefix = error.prefix)
            
            if (is.null(dimension.values)) dimension.values = list()
            
            # *dimension.values.to.distribute* must be a named list with
            # names that are dimensions of the array
            # and values that are in the dimnames of the array
            # and can only be used if the scale is non.negative.number
            # and the input is an array
            if (!is.list(dimension.values.to.distribute))
                stop(paste0(error.prefix, "'dimension.values.to.distribute' must be a named list; defaults to list()"))
            if (length(dimension.values.to.distribute) > 0 && !is.array(data))
                stop(paste0(error.prefix, "'dimension.values.to.distribute' can only be used when the data is an array; should be left as the default, list(), otherwise"))
            if (length(dimension.values.to.distribute) > 0 && outcome.info$metadata$scale != 'non.negative.number')
                stop(paste0(error.prefix, "'dimension.values.to.distribute' can only be used when the data has scale 'non.negative.number'; should be left as the default, list(), otherwise"))
            # We cannot distribute on incomplete dimensions
            if (any(names(dimension.values.to.distribute) %in% incomplete.dimensions(ont)))
                stop(paste0(error.prefix, "'dimension.values.to.distribute' cannot contain any incomplete dimensions"))
            
            # *round.distributed.dimension.values* is logical
            if (!is.logical(round.distributed.dimension.values) || length(round.distributed.dimension.values)!=1 || is.na(round.distributed.dimension.values))
                stop(paste0(error.prefix, "'round.distributed.dimension.values' must be a single, non-NA logical value"))
            
            if (is.array(data))
            {
                if (is.null(dimnames(data)))
                    stop(paste0(error.prefix,
                                "If 'data' is an array, then it must be an array with named dimnames"))
                
                if (is.null(names(dimnames(data))))
                    stop(paste0(error.prefix,
                                "If 'data' is an array, then it must be an array with NAMED dimnames"))
                
                # Distribute dimension values, if applicable, BEFORE checks on the data dimnames. Note that we won't distribute dimensions we don't have, but no error
                if (length(dimension.values.to.distribute) > 0) {
                    dvtd.names = intersect(names(dimension.values.to.distribute), names(dimnames(data)))
                    if (is.null(dvtd.names) || any(is.na(dvtd.names)))
                        stop(paste0(error.prefix, "'dimension.values.to.distribute' must be a named list"))
                    if (any(sapply(dvtd.names, function(d) {
                        length(dimension.values.to.distribute[[d]])==0 ||
                            any(is.na(dimension.values.to.distribute[[d]])) ||
                            any(duplicated(dimension.values.to.distribute[[d]]))
                    })))
                        stop(paste0(error.prefix, "the elements of 'dimension.values.to.distribute' must have positive length and contain no NAs or repeats"))
                    # Use only the values we actually have without throwing error if there are extras, which may leave us with none
                    dimension.values.to.distribute = lapply(dvtd.names, function(d) {
                        intersect(dimension.values.to.distribute[[d]], dimnames(data)[[d]])
                    })
                    names(dimension.values.to.distribute)=dvtd.names
                    dimension.values.to.distribute = dimension.values.to.distribute[sapply(dimension.values.to.distribute, length)>0]
                }
                
                if (length(dimension.values.to.distribute) > 0) {
                    data = distribute.dimension.values(data, dimension.values.to.distribute[names(dimension.values.to.distribute) %in% dvtd.names], round.distributed.dimension.values)
                    # We must now have exactly the dimension values in the ontology for distributed dimensions
                    if (any(sapply(dvtd.names, function(d) {
                        length(setdiff(ont[[d]], dimnames(data)[[d]]))>0
                    })))
                        stop(paste0(error.prefix, "'dimension.values.to.distribute' can only be used when the dataset contains all values in the ontology for relevant dimensions"))
                }
                
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
                
                if ('location' %in% names(dimnames(data))) {
                    if (!all(sapply(dimnames(data)['location'], function(v) {locations::is.location.valid(v)} ))) {
                        stop(paste0(error.prefix, "some or all locations in data are invalid"))
                    }
                }
                
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
            if (printouts) {
                checktime = Sys.time()
                print(paste0(dots, "Performing checks at ", checktime))
            }
            dimension.values = resolve.ontology.dimension.values(ont = ont,
                                                                 dimension.values = dimension.values,
                                                                 error.prefix = paste0(error.prefix, "Error resolving 'dimension.values' - "))
            
            # If dimension.values contains dimension "location", check that every value is a valid location.
            if ("location" %in% names(dimension.values)) {
                if (!all(sapply(dimension.values['location'], function(v) {locations::is.location.valid(v)} ))) {
                    stop(paste0(error.prefix, "some or all locations in 'dimension.values' are invalid"))
                }
            }
            
            # *metric* is one of 'estimate', 'upper.bound', 'lower.bound', or 'coefficient.of.variance' until more options are added
            if (!is.character(metric) || length(metric) != 1 || !(metric %in% c('estimate', 'variance', 'standard.deviation', 'upper.bound', 'lower.bound', 'coefficient.of.variance')))
                stop(paste0(error.prefix, "'metric' must be a character vector with value 'estimate', 'variance', 'standard.deviation', 'upper.bound', 'lower.bound' or 'coefficient.of.variance'"))
            
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
            
            #--------------------------#
            #-- Hash url and details --#
            #--------------------------#
            if (printouts) {
                hashtime = Sys.time()
                print(paste0(dots, "Checktime took ", hashtime-checktime))
            }
            url.hashed = paste0(sort(url), collapse='__')
            details.hashed = paste0(sort(details), collapse='__')
            if (url.hashed %in% private$i.url.list) url = which(private$i.url.list==url.hashed)
            else {
                private$i.url.list = c(private$i.url.list, url.hashed)
                url = length(private$i.url.list)
            }
            if (details.hashed %in% private$i.details.list) details = which(private$i.details.list==details.hashed)
            else {
                private$i.details.list = c(private$i.details.list, details.hashed)
                details = length(private$i.details.list)
            }
            url = as.integer(url)
            details = as.integer(details) # don't know why which() and length() didn't already make it integer...
            if (printouts) {
                phase3time = Sys.time()
                print(paste0(dots, "Hashtime took ", phase3time - hashtime))
            }
            
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
            if (printouts) {
                prepdntime = Sys.time()
                print(paste0(dots, "getting required stratification took ", prepdntime-phase3time))
            }
            # What dim.names do we need to accommodate the new data?
            put.dim.names = private$prepare.put.dim.names(outer.join.dim.names(if (is.array(data)) dimnames(data) else list(), dimension.values),
                                                          ontology.name = ontology.name)
            if (printouts) {
                reqstratdimtime = Sys.time()
                print(paste0(dots, "preparing put dimnames took ", reqstratdimtime-prepdntime))
            }
            
            # Check that our data doesn't have year ranges if our ontology has single years or vice versa.
            
            if (!is.null(ont$year)) {
                if (all(is.year.range(ont$year)) && any(!is.year.range(put.dim.names$year)))
                    stop(paste0(error.prefix, "the data has single years while the existing ontology has year ranges"))
                if (all(!is.year.range(ont$year)) && any(is.year.range(put.dim.names$year)))
                    stop(paste0(error.prefix, "the data has year ranges while the existing ontology has single years"))
            }
            
            ## @AZ re-order the put.dim.names to ensure it has the same order of dimensions as the stratification does.
            stratification.dimensions = private$get.required.stratification(dimension.values = dimension.values,
                                                                            data = data,
                                                                            ontology.name = ontology.name,
                                                                            return.as.dimensions = T)
            put.dim.names = put.dim.names[stratification.dimensions]
            if (printouts) {
                metrictime = Sys.time()
                print(paste0(dots, "getting required strat dimensions took ", metrictime-reqstratdimtime))
                print(paste0(dots, "Phase three stuff took ", metrictime-phase3time))
            }
            # if (debug) browser()
            ## ANDREW'S NEW LOGIC TO ACCOMMODATE MULTIPLE METRICS AND ENSURING ALIGNED DIMNAMES AMONG ALL
            existing.dim.names.this.metric = dimnames(private$i.data[[outcome]][[metric]][[source]][[ontology.name]][[stratification]])
            data.already.present.this.metric = !is.null(existing.dim.names.this.metric)
            
            # Here, if this put is intended for removal, we will limit our put dim names to the intersection with existing dim names.
            if (is.removal) {
                put.dim.names = intersect.shared.dim.names(put.dim.names, existing.dim.names.this.metric)
                dimension.values = put.dim.names
            }
            
            all.metric.names = names(private$i.data[[outcome]])
            existing.dim.names = lapply(names(private$i.data[[outcome]]), function(metr) {
                dimnames(private$i.data[[outcome]][[metr]][[source]][[ontology.name]][[stratification]])
            })
            names(existing.dim.names) = names(private$i.data[[outcome]])
            if (length(existing.dim.names) > 0)
                existing.dim.names = existing.dim.names[sapply(existing.dim.names, function(metr.dimnames) {!is.null(metr.dimnames)})]
            metrics.with.data = names(existing.dim.names)
            
            data.already.present = length(existing.dim.names) > 0
            
            # find aligning dimnames
            dimnames.aligning.all.metrics = put.dim.names
            if (data.already.present) {
                dimnames.aligning.all.metrics = private$prepare.put.dim.names(do.call(outer.join.dim.names, c(existing.dim.names, list(put.dim.names))), ontology.name)
            }
            if (printouts) {
                backuptime = Sys.time()
                print(paste0(dots, "Metric stuff took ", backuptime-metrictime))
            }
            if (data.already.present && !dim.names.equal(existing.dim.names, dimnames.aligning.all.metrics))
            {
                # Backup old data
                existing.data.and.metadata = lapply(data.element.names, function(name) {
                    data.this.element = lapply(metrics.with.data, function(metr) {
                        private[[name]][[outcome]][[metr]][[source]][[ontology.name]][[stratification]]
                    })
                    names(data.this.element) = metrics.with.data
                    return(data.this.element)
                })
                names(existing.data.and.metadata) = data.element.names
                
                # Make the new (empty) data structures... make it for this metric too, even if it's not one of the existing ones
                for (name in data.element.names) {
                    for (metr in union(metrics.with.data, metric))
                        private[[name]][[outcome]][[metr]][[source]][[ontology.name]][[stratification]] =
                            array(NaN, dim=sapply(dimnames.aligning.all.metrics, length), dimnames = dimnames.aligning.all.metrics)
                }
                if (printouts) {
                    print(paste0(dots, "Performing array access..."))
                }
                # Overwrite the new structure with the old data
                for (name in data.element.names) {
                    for (metr in metrics.with.data) {
                        array.access(private[[name]][[outcome]][[metr]][[source]][[ontology.name]][[stratification]], existing.dim.names[[metr]]) =
                            existing.data.and.metadata[[name]][[metr]]
                    }
                }
            }
            else if (!data.already.present)
            {
                # Make the new (empty) data structures
                for (name in data.element.names) {
                    private[[name]][[outcome]][[metric]][[source]][[ontology.name]][[stratification]] =
                        array(NaN, dim=sapply(dimnames.aligning.all.metrics, length), dimnames = dimnames.aligning.all.metrics)
                }
            }
            
            # Update ontology
            for (d in names(dimnames.aligning.all.metrics)) {
                private$i.ontologies[[ontology.name]][[d]] = sort(union(private$i.ontologies[[ontology.name]][[d]], dimnames.aligning.all.metrics[[d]]))
            }
            
            #-- Put the data and its metadata --#
            
            # get the indices we're going to write into
            #@Andrew get.array.access.indices makes an array full of 1:prod(sapply(arr.dim.names, length))
            #@ then it calls fast.array.access with that array and the data dimnames and dimension.values
            #@ fast.array.access loops five times if five dimensions (location, year, age, risk, sex...) with lapply
            #@ within fast.array.access, subset.values is a list of length(dims) and each element has all the dim values
            # browser()
            if (printouts) {
                finalputtime = Sys.time()
                print(paste0(dots, "Backup took ", finalputtime-backuptime))
            }
            
            overwrite.indices = get.array.access.indices(arr.dim.names = dimnames(private$i.data[[outcome]][[metric]][[source]][[ontology.name]][[stratification]]),
                                                         dimension.values = c(dimnames(data), dimension.values))
            if (!allow.na.to.overwrite)
            {
                overwrite.indices = overwrite.indices[!is.na(data)]
                data = data[!is.na(data)]
            }
            
            # Put data
            private$i.data[[outcome]][[metric]][[source]][[ontology.name]][[stratification]][overwrite.indices] = data
            
            # Put metadata
            private$i.url[[outcome]][[metric]][[source]][[ontology.name]][[stratification]][overwrite.indices] = url
            private$i.details[[outcome]][[metric]][[source]][[ontology.name]][[stratification]][overwrite.indices] = details
            if (printouts) {
                totalputtime = Sys.time()
                print(paste0(dots, "Final putting took ", totalputtime-finalputtime))
                print(paste0(dots, "Total put took ", totalputtime - puttime))
            }
            
            # Modified
            private$i.last.modified.date = Sys.time()
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        put.long.form = function(data,
                                 outcome,
                                 metric = 'estimate',
                                 source,
                                 ontology.name,
                                 dimension.values=NULL,
                                 url,
                                 details,
                                 allow.na.to.overwrite=F,
                                 dimension.values.to.distribute=list(),
                                 round.distributed.dimension.values=T,
                                 debug=F,
                                 printouts=F)
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
                
                unique.outcomes = unique(data[['outcome']])
                for (one.outcome in unique.outcomes)
                {
                    self$put.long.form(outcome = one.outcome,
                                       metric = metric,
                                       ontology.name = ontology.name,
                                       source = source,
                                       dimension.values = dimension.values,
                                       data = data[data[['outcome']]==one.outcome,],
                                       url = url,
                                       details = details,
                                       allow.na.to.overwrite=F,
                                       dimension.values.to.distribute = dimension.values.to.distribute,
                                       round.distributed.dimension.values = round.distributed.dimension.values,
                                       debug=debug,
                                       printouts=printouts)
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
                
                if (!is.numeric(data[['value']]))
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
                    if (!is.character(data[[d]]) && !is.numeric(data[[d]]))
                        stop(paste0(error.prefix, "Column '", d, "' in 'data' must contain character or numeric values"))
                    T
                })
                # browser()
                
                # NOTE: We will no longer check the dimension values here, because that interferes with the possibility of redistributing them.
                # Instead, we'll check like we already do in the put function
                # data[dimensions] = resolve.ontology.dimension.values(ont=ont, dimension.values=data[dimensions],
                # error.prefix=paste0(error.prefix, " Error resolving dimension values - "))
                
                # Hydrate it to an array
                dim.names = lapply(dimensions, function(d){
                    unique(data[[d]])
                })
                names(dim.names) = dimensions
                
                arr.data = array(as.numeric(NA), dim=sapply(dim.names, length), dimnames=dim.names)
                indices.arr = array(1:length(arr.data), dim=sapply(dim.names, length), dimnames=dim.names)
                dimensions = names(dim.names)
                indices.for.rows = sapply(1:nrow(data), function(i){
                    do.call('[', args=c(list(indices.arr), as.list(data[i,dimensions])))
                })
                na.mask = is.na(data[,'value'])
                arr.data[indices.for.rows[!na.mask]] = data[!na.mask,'value']
                
                #-- Pass through to main put function --#
                self$put(outcome = outcome,
                         metric = metric,
                         ontology.name = ontology.name,
                         source = source,
                         dimension.values = dimension.values,
                         data = arr.data,
                         url = url,
                         details = details,
                         allow.na.to.overwrite = allow.na.to.overwrite,
                         dimension.values.to.distribute = dimension.values.to.distribute,
                         round.distributed.dimension.values= round.distributed.dimension.values,
                         debug=debug,
                         printouts=printouts)
            }
            
            if (printouts) print('done with put long form')
        },
        
        pull = function(outcome,
                        metric = 'estimate',
                        keep.dimensions = NULL,
                        dimension.values = NULL,
                        sources = NULL,
                        from.ontology.names = NULL,
                        exclude.ontology.names = NULL,
                        target.ontology = NULL,
                        allow.mapping.from.target.ontology = T,
                        append.attributes = NULL,
                        append.metrics = NULL,
                        allow.other.sources.for.denominator = F,
                        na.rm = F,
                        ignore.ontologies.without.requested.locations = T,
                        check.arguments = T,
                        debug = F,
                        ...)
        {
            # if (debug) browser()
            error.prefix = paste0("Cannot pull '", outcome, "' data from the data manager: ")
            # *extra dimensions* are an alternative to 'dimension.values' and must pass the same checks if used
            extra.dimension.values = list(...)
            if (length(extra.dimension.values)>0) {
                # check that either this or dimension.values is len(0)
                if (length(extra.dimension.values)>0 && length(dimension.values)>0)
                    stop(paste0(error.prefix, "'dimension.values' must be specified in either the 'dimension.values' argument or as additional arguments to the function"))
                dimension.values = extra.dimension.values
                check.dimension.values.valid.error.name = "pull function extra arguments"
            }
            # If *keep.dimensions* are NULL and there is a target ontology, set keep.dimensions to empty char vector
            if (!is.null(target.ontology) && is.null(keep.dimensions))
                keep.dimensions = character(0)
            
            outcome.info = private$i.outcome.info[[outcome]]
            
            if (check.arguments) {
                
                # *outcome* is a single, non-NA character value
                #  that has been previously registered as an outcome for this data manager
                if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome) || nchar(outcome)==0)
                    stop(paste0(error.prefix, "'outcome' must be a single, non-empty, non-NA character value"))
                
                if (is.null(outcome.info))
                    stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome."))
                
                # *metric* is one of 'estimate', 'upper.bound', 'lower.bound', or 'coefficient.of.variance' until more options are added #@ add variance and standard deviation
                if (!is.character(metric) || length(metric) != 1 || !(metric %in% c('estimate', 'variance', 'standard.deviation', 'upper.bound', 'lower.bound', 'coefficient.of.variance')))
                    stop(paste0(error.prefix, "'metric' must be a character vector with value 'estimate', 'variance', 'standard.deviation', 'upper.bound', 'lower.bound' or 'coefficient.of.variance'"))
                
                # if (metric %in% c('upper.bound', 'lower.bound'))
                #     stop(paste0(error.prefix, "pulling with metric '", metric, "' is not yet supprted"))
                
                # *keep.dimensions* is either NULL or a character vector with no NA values or repeats
                if (!is.null(keep.dimensions) && (!is.character(keep.dimensions) || any(duplicated(keep.dimensions)) || anyNA(keep.dimensions)))
                    stop(paste0(error.prefix, "'keep.dimensions' must be either NULL or a character vector with no NA values or repeats"))
                
                # make a variable name for error to pass to check.dimension.values.valid
                check.dimension.values.valid.error.name = "dimension.values"
                
                # *dimension.values* are valid
                #   - check.dimension.values.valid() doesn't accept NULL because it wants a list
                if (!is.null(dimension.values))
                    check.dimension.values.valid(dimension.values, "dimension.values")
                
                # *sources* is either NULL or a character vector with at least one element and no NA or empty values
                #  that have all been registered previously as sources for this outcome with this data manager
                if (!is.null(sources) && (!is.character(sources) || !length(sources)>0 || anyNA(sources) || any(nchar(sources)==0)))
                    stop(paste0(error.prefix, "'sources' must be NULL or a character vector with at least one element and no NA or empty values"))
                unregistered.sources = sapply(sources, function(x){
                    !(x %in% unique(unlist(lapply(private$i.data[[outcome]], function(metric.data) {names(metric.data)}))))
                })
                if (any(unregistered.sources))
                    stop(paste0(error.prefix, "all sources must be registered for this outcome with this data manager"))
                
                # *target.ontology* is either NULL or an ontology object
                if (!is.null(target.ontology) && !is.ontology(target.ontology))
                    stop(paste0(error.prefix, "'target.ontology' must be either NULL or an ontology object"))
                
                # The target ontology cannot have any NULL dimensions (ones that do have never had any data put to them)
                if (any(sapply(target.ontology, is.null)))
                    stop(paste0(error.prefix, "'target.ontology' cannot have any NULL dimensions"))
                
                # *allow.mapping.from.target.ontology* is a single, non-NA logical value
                if (!is.logical(allow.mapping.from.target.ontology) || length(allow.mapping.from.target.ontology)!=1 || is.na(allow.mapping.from.target.ontology))
                    stop(paste0(error.prefix, "'allow.mapping.from.target.ontology' must be a single, non-NA logical value"))
                
                # *from.ontology.names* is either NULL or a character vector with no NA or empty values
                if (!is.null(from.ontology.names) && (!is.character(from.ontology.names) || anyNA(from.ontology.names) || any(nchar(from.ontology.names)==0)))
                    stop(paste0(error.prefix, "'from.ontology.names' must be either NULL or a character vector with no NA or empty values"))
                
                #  all of which have been previously registered with this data manager (if not NULL)
                unregistered.ontologies = sapply(from.ontology.names, function(x){is.null(private$i.ontologies[[x]])})
                if (!is.null(from.ontology.names) && any(unregistered.ontologies))
                    stop(paste0(error.prefix, "all ontologies in 'from.ontology.names' must be registered with this data manager"))
                
                # *exclude.ontology.names* is either NULL or a character vector with no NA or empty values
                if (!is.null(exclude.ontology.names) && (!is.character(exclude.ontology.names) || anyNA(exclude.ontology.names) || any(nchar(exclude.ontology.names)==0)))
                    stop(paste0(error.prefix, "'exclude.ontology.names' must be either NULL or a character vector with no NA or empty values"))
                
                # *append.attributes* is either NULL or a character vector with no NA values that
                #  contains only "details" or "url" or both
                if (!is.null(append.attributes) && (!is.character(append.attributes) || anyNA(append.attributes) || !all(append.attributes %in% c("details", "url"))))
                    stop(paste0(error.prefix, "append.attributes' must be either NULL or a character vector with no NA values that contains only 'details' or 'url' or both"))
                
                # *na.rm* is a single, non-NA logical value
                if (!is.logical(na.rm) || length(na.rm)!=1 || is.na(na.rm))
                    stop(paste0(error.prefix, "na.rm must be a single, non-NA, logical value"))
            }
            
            # Check if we have any data at all for this outcome!
            if (is.null(private$i.data[[outcome]][[metric]])) {
                return (NULL)
            }
            
            # Reduce ontology space by ignoring ontologies that don't have our locations if locations are in the dimension values
            # We'll do this first even though we won't *technically* know that 'location' is an incomplete dimension in the ontology we'll end up in.
            
            if (ignore.ontologies.without.requested.locations && 'location' %in% names(dimension.values)) {
                ontologies.with.these.locations = names(private$i.ontologies)[sapply(private$i.ontologies, function(ont) {any(dimension.values$location %in% ont[['location']])})]
                ontologies.in.this.outcome = unique(unlist(sapply(private$i.data[[outcome]][[metric]], function(source) {names(source)})))
                relevant.ontologies = intersect(ontologies.with.these.locations, ontologies.in.this.outcome)
                if (!is.null(from.ontology.names))
                    from.ontology.names = intersect(from.ontology.names, relevant.ontologies)
                else
                    from.ontology.names = relevant.ontologies
                
            }
            
            # Get the universal ontology (replaces 'target.ontology') and the returned mapping, which may be replaced with an identity mapping if keep.dimensions are not in the mapping's 'to' dimensions
            return.mapping.flag = !is.null(target.ontology) && allow.mapping.from.target.ontology
            target.from.arguments = target.ontology
            if (debug) browser()
            if (is.null(target.ontology) || allow.mapping.from.target.ontology) {
                target.ontology = private$get.universal.ontology(outcome = outcome,
                                                                 sources = sources,
                                                                 from.ontology.names = from.ontology.names,
                                                                 exclude.ontology.names = exclude.ontology.names,
                                                                 target.ontology = target.ontology,
                                                                 debug=F)
            }
            # This gets NULL if the ontologies which had the requested locations were not for this outcome
            if (is.null(target.ontology)) return(NULL)
            
            dv.names = names(dimension.values)
            dimension.values = lapply(seq_along(dimension.values), function(d) {
                if (names(dimension.values)[[d]] %in% incomplete.dimensions(target.ontology)) as.character(dimension.values[[d]])
                else dimension.values[[d]]
            })
            names(dimension.values) = dv.names
            resolved.dimension.values = resolve.ontology.dimension.values(target.ontology, dimension.values, error.prefix = error.prefix, throw.error.if.unresolvable = F)
            if ((is.null(resolved.dimension.values) || any(sapply(resolved.dimension.values, is.null))) && !is.null(dimension.values))
                stop(paste0(error.prefix, "'dimension.values' cannot be resolved"))
            
            # need.to.set.keep.dimensions.flag = is.null(keep.dimensions)
            if (is.null(keep.dimensions)) {
                if (is.null(resolved.dimension.values)) {
                    dimension.values.dimensions.longer.than.one = dv.names[sapply(resolved.dimension.values, length)>1]
                    keep.dimensions = setdiff(incomplete.dimensions(target.ontology), dimension.values.dimensions.longer.than.one)
                }
                else {
                    keep.dimensions = incomplete.dimensions(target.ontology)
                }
                if (return.mapping.flag) attr(post.processed.data, 'mapping') = target.to.universal.mapping
            }
            
            # Reduce target to needed dimensions and find a mapping
            if (!is.null(keep.dimensions))
                target.ontology = target.ontology[names(target.ontology) %in% union(keep.dimensions, names(dimension.values))]
            if (return.mapping.flag) target.to.universal.mapping = get.ontology.mapping(target.from.arguments, target.ontology, allow.non.overlapping.incomplete.dimensions = T)
            
            # If sources is NULL, use all the sources from the outcome
            if (is.null(sources))
                sources.used.names = names(private$i.data[[outcome]][[metric]])
            else
                sources.used.names = sources
            sources.successful.names = c()
            
            pre.processed.data = lapply(sources.used.names, function(source.name) {
                source.ontology.names = names(private$i.data[[outcome]][[metric]][[source.name]])
                if (is.null(from.ontology.names))
                    ontologies.used.names = source.ontology.names
                else
                    ontologies.used.names = intersect(source.ontology.names, from.ontology.names)
                ontologies.used.names = setdiff(ontologies.used.names, exclude.ontology.names)
                pulled.source.data = NULL
                
                for (ont.name in ontologies.used.names) {
                    pulled.ont.data = NULL
                    ont = private$i.ontologies[[ont.name]]
                    stratification.names = names(private$i.data[[outcome]][[metric]][[source.name]][[ont.name]])
                    
                    for (strat in stratification.names) {
                        strat.data = private$i.data[[outcome]][[metric]][[source.name]][[ont.name]][[strat]]
                        strat.dimensions = names(dim(strat.data))
                        strat.dimnames = as.ontology(dimnames(strat.data), incomplete.dimensions = intersect(incomplete.dimensions(ont), strat.dimensions))
                        
                        if (all(is.na(strat.data))) next
                        
                        mapping.to.apply = get.ontology.mapping(strat.dimnames, target.ontology, allow.non.overlapping.incomplete.dimensions = T)
                        if (is.null(mapping.to.apply)) next
                        
                        dimnames.for.apply = mapping.to.apply$apply.to.dim.names(strat.dimnames)
                        if (!setequal(names(dimnames.for.apply), union(keep.dimensions, dv.names))) next
                        
                        # Check that the mapped stratification won't aggregate illegally due to missing some values in an incomplete dimension that will be aggregated (since it is in dimension.values but not keep.dimensions)
                        missing.dimension.values.for.aggregated.dimension = F
                        aggregated.dimensions = setdiff(incomplete.dimensions(dimnames.for.apply), keep.dimensions)
                        for (d in aggregated.dimensions) {
                            if (d %in% dv.names) {
                                if (length(setdiff(dimension.values[[d]], dimnames.for.apply[[d]])) > 0) {
                                    missing.dimension.values.for.aggregated.dimension = T
                                    break
                                }
                            }
                        }
                        if (missing.dimension.values.for.aggregated.dimension) next
                        
                        # Insert dimension.values
                        
                        dimension.has.no.intersection = F
                        for (d in dv.names) {
                            if (d == 'year') {
                                replacement = get.range.robust.year.intersect(dimnames.for.apply[[d]], resolved.dimension.values[[d]])
                            }
                            else replacement = intersect(dimnames.for.apply[[d]], resolved.dimension.values[[d]])
                            if (length(replacement) == 0) {
                                dimension.has.no.intersection = T
                                break
                            }
                            dimnames.for.apply[[d]] = replacement
                        }
                        if (dimension.has.no.intersection) {
                            next
                        }
                        # Apply mapping
                        # if (debug) browser()
                        
                        data.types = union('data', append.attributes)
                        # browser()
                        # 
                        # cc <<- list(append.attributes=append.attributes,
                        #             mapping.to.apply=mapping.to.apply,
                        #             strat.dimnames=strat.dimnames,
                        #             dimnames.for.apply=dimnames.for.apply,
                        #             strat.data=strat.data,
                        #             metric=metric,
                        #             outcome=outcome,
                        #             source.name=source.name,
                        #             keep.dimensions=keep.dimensions,
                        #             dv.names=dv.names,
                        #             outcome.info=outcome.info,
                        #             na.rm=na.rm)
                        
                        pulled.ont.data <- do.pre.aggregation.processing(data.manager=self,
                                                                         append.attributes,
                                                                         mapping.to.apply,
                                                                         strat.dimnames,
                                                                         dimnames.for.apply,
                                                                         strat.data,
                                                                         metric=metric,
                                                                         outcome=outcome,
                                                                         source.name=source.name,
                                                                         keep.dimensions=keep.dimensions,
                                                                         dv.names=dv.names,
                                                                         outcome.info=outcome.info,
                                                                         na.rm=na.rm)
                        
                        # Aggregate if needed
                        initial.dimnames = dimnames.for.apply
                        dimensions.to.drop = intersect(which(sapply(initial.dimnames, length) == 1), which(!(names(initial.dimnames) %in% keep.dimensions)))
                        
                        # pulled.ont.data <- lapply(data.types, function(data.type) {
                        #     data.by.data.type <- pulled.ont.data[[data.type]]
                        #     pre.agg.dimnames = initial.dimnames
                        # })
                        
                        pulled.ont.data = lapply(data.types, function(data.type) {
                            if (debug) browser()
                            # if (source.lacks.denominator.data.flag) return (NULL)
                            # if (source.lacks.estimate.data.flag) return (NULL)
                            data.by.data.type = pulled.ont.data[[data.type]]
                            pre.agg.dimnames = initial.dimnames
                            if (length(dimensions.to.drop) > 0) {
                                pre.agg.dimnames = pre.agg.dimnames[-dimensions.to.drop]
                                data.by.data.type = array(data.by.data.type,
                                                          dim = sapply(pre.agg.dimnames, length),
                                                          dimnames = pre.agg.dimnames)
                            }
                            
                            if (length(pre.agg.dimnames) > length(keep.dimensions))
                                data.by.data.type <- do.aggregation(pre.agg.dimnames,
                                                                    data.to.aggregate=data.by.data.type,
                                                                    keep.dimensions,
                                                                    data.type,
                                                                    metric,
                                                                    source.name,
                                                                    outcome,
                                                                    dimension.values=dimension.values,
                                                                    target.ontology=target.ontology)
                            
                            if (length(pre.agg.dimnames) > length(keep.dimensions)) {
                                post.agg.dimnames = pre.agg.dimnames[names(pre.agg.dimnames) %in% keep.dimensions]
                                if (data.type == 'data') {
                                    # if (metric == 'coefficient.of.variance') {
                                    #     # cv = sd / mean, so sd = cv * mean. Need to convert back at the end.
                                    #     # This time we have to actually pull since we're in a different ontology by now
                                    #     
                                    #     # Check if this source has data, otherwise use all available sources and apply mean over result, or give up.
                                    #     if (source.name %in% names(private$i.data[[outcome]][['estimate']]))
                                    #         estimate.source = source.name
                                    #     else if (length(names(private$i.data[[outcome]][['estimate']]))>0)
                                    #         estimate.source = names(private$i.data[[outcome]][['estimate']])
                                    #     else {
                                    #         source.lacks.estimate.data.flag <<- TRUE
                                    #         return (NULL)
                                    #     }
                                    #     
                                    #     estimate.data.for.cv = self$pull(outcome = outcome,
                                    #                                      metric = 'estimate',
                                    #                                      source = estimate.source,
                                    #                                      keep.dimensions = names(pre.agg.dimnames),
                                    #                                      dimension.values = dimension.values[names(dimension.values) %in% names(pre.agg.dimnames)],
                                    #                                      target.ontology = target.ontology,
                                    #                                      allow.mapping.from.target.ontology = F)
                                    #     # TO DO: THE STUFF RELATING TO WHICH SOURCE WE USE FOR THIS PULL (PICK SAME OR USE ALL, THEN TAKE MEAN AFTERWARDS)
                                    #     
                                    #     # If the estimate data came from only one source, we can remove the source so that it will match size of data
                                    #     non.source.dimensions = names(dim(estimate.data.for.cv))[names(dim(estimate.data.for.cv))!='source']
                                    #     if (dim(estimate.data.for.cv)['source'] == 1)
                                    #         estimate.data.for.cv = array(estimate.data.for.cv,
                                    #                                      dim = dim(estimate.data.for.cv)[non.source.dimensions],
                                    #                                      dimnames = dimnames(estimate.data.for.cv)[non.source.dimensions]
                                    #         )
                                    #     
                                    #     # Otherwise, we need to take the mean across source
                                    #     else estimate.data.for.cv = apply.robust(estimate.data.for.cv, non.source.dimensions, mean, na.rm=T)
                                    #     dimnames.in.common = get.dimension.values.overlap(dimnames(data.by.data.type), dimnames(estimate.data.for.cv))
                                    #     estimate.data.for.cv = array.access(estimate.data.for.cv, dimnames.in.common)
                                    #     data.by.data.type = array.access(data.by.data.type, dimnames.in.common)
                                    #     data.by.data.type = data.by.data.type * estimate.data.for.cv
                                    # }
                                    # if (metric %in% c('standard.deviation', 'coefficient.of.variance')) data.by.data.type = data.by.data.type**2
                                    scale = outcome.info[['metadata']][['scale']]
                                    if (scale %in% c('non.negative.number', 'number')) {
                                        data.by.data.type = apply.robust(data.by.data.type, names(post.agg.dimnames), sum, na.rm=na.rm)
                                    } else if (scale %in% c('rate', 'time', 'proportion', 'ratio')) {
                                        denominator.outcome = outcome.info[['denominator.outcome']] ## NOTE: I MUST TELL ZOE TO ADD THIS AFTER I INTRODUCE THE REQUIREMENT TO HAVE IT
                                        denominator.ontology = target.ontology ## NOTE: DO WE NEED TO HAVE THE UNIVERSAL ALIGN TO THE DENOMINATOR ONTOLOGIES TOO, WHEN WE KNOW WE'LL NEED IT?
                                        
                                        # If we have a denominator offset, then the dimension values needs to specify that the years will be
                                        # whatever they are for the main data, but slid back by <offset>, like 2020 -> 2019.
                                        denom.dim.vals = pre.agg.dimnames
                                        if (!is.null(outcome.info$denominator.lags.by.one.year)) {
                                            denom.dim.vals$year = as.character(as.numeric(denom.dim.vals$year) - outcome.info$denominator.lags.by.one.year)
                                        }
                                        
                                        denominator.sources = names(private$i.data[[denominator.outcome]][['estimate']])
                                        # put same source on front
                                        if (source.name %in% denominator.sources) {
                                            denominator.sources = c(source.name, setdiff(denominator.sources, source.name))
                                        }
                                        
                                        denominator.array = NULL
                                        for (denominator.source in denominator.sources) {
                                            denominator.array = self$pull(outcome = denominator.outcome,
                                                                          metric = 'estimate', # I believe we always want "estimates" for aggregating with denominators
                                                                          keep.dimensions = names(pre.agg.dimnames),
                                                                          dimension.values = denom.dim.vals,
                                                                          sources = denominator.source,
                                                                          target.ontology = denominator.ontology,
                                                                          allow.mapping.from.target.ontology = F,
                                                                          from.ontology.names = NULL,
                                                                          na.rm = na.rm,
                                                                          check.arguments = F)
                                            if (!is.null(denominator.array)) break
                                        }
                                        
                                         
                                        if (is.null(denominator.array)) {
                                            source.lacks.denominator.data.flag <<- TRUE
                                            return (NULL)
                                        }
                                        # Since the denominator.array came from only one source, we can remove the source so that it will match size of data
                                        denominator.array = array(denominator.array,
                                                                  dim = dim(denominator.array)[names(dim(denominator.array)) != 'source'],
                                                                  dimnames = dimnames(denominator.array)[names(dimnames(denominator.array)) != 'source']
                                        )
                                        
                                        # If we had an offset, rename the year dimension names to match the main data
                                        if (!is.null(outcome.info$denominator.lags.by.one.year)) {
                                            dimnames(denominator.array)$year = as.character(as.numeric(dimnames(denominator.array)$year) + outcome.info$denominator.lags.by.one.year)
                                        }
                                        
                                        # # Catch an otherwise invisible bug if denominator.array somehow doesn't have the same shape/order as the data
                                        # if (!dim.names.equal(dimnames(denominator.array), dimnames(data.by.data.type)))
                                        #     stop(paste0(error.prefix, 'bug in aggregation code: denominator array has incorrect dimensions'))
                                        
                                        # So apparently it's possible that the ontology we get denominator data from can have the same dimension values but in a different order from those in our main data
                                        denom.ont = as.ontology(dimnames(denominator.array), incomplete.dimensions = intersect(names(dimnames(denominator.array)), c('year', 'location')))
                                        data.ont = as.ontology(dimnames(data.by.data.type), incomplete.dimensions = intersect(names(dimnames(data.by.data.type)), c('year', 'location')))
                                        denom.to.data.mapping = get.ontology.mapping(denom.ont, data.ont)
                                        
                                        if (is.null(denom.to.data.mapping))
                                            # browser()
                                            stop(paste0(error.prefix, 'bug in aggregation code: denominator array dimensions cannot be mapped to main data dimensions'))
                                        
                                        # It's possible that we didn't find as many years or locations in the denominator as we did in the data.by.data.type
                                        if (!setequal(dimnames(denominator.array)$year, dimnames(data.by.data.type)$year) || !setequal(dimnames(denominator.array)$location, dimnames(data.by.data.type)$location))
                                            data.by.data.type = array.access(data.by.data.type, year=dimnames(denominator.array)$year, location=dimnames(denominator.array)$location)
                                        
                                        denominator.array = denom.to.data.mapping$apply(denominator.array, to.dim.names = dimnames(data.by.data.type)) # need this argument to ensure correct order
                                        
                                        if (metric %in% c('variance', 'standard.deviation', 'coefficient.of.variance')) denominator.array = denominator.array**2
                                        
                                        # We should find totals by aggregating the denominator.array rather than pulling less stratified data
                                        # because less stratified data might not equal the sum of the more stratified data in denominator.array
                                        denominator.totals.array = apply.robust(denominator.array, names(post.agg.dimnames), sum, na.rm=na.rm)
                                        weighted.value.array = data.by.data.type * denominator.array
                                        data.by.data.type = apply.robust(weighted.value.array, names(post.agg.dimnames), sum, na.rm=na.rm)
                                        data.by.data.type = data.by.data.type / denominator.totals.array
                                    }
                                    else stop(paste0(error.prefix, 'aggregating ', scale, ' data is not yet implemented'))
                                    if (metric %in% c('standard.deviation', 'coefficient.of.variance')) {
                                        data.by.data.type = sqrt(data.by.data.type)
                                    }
                                    if (metric == 'coefficient.of.variance') {
                                        estimate.data.for.cv = self$pull(outcome = outcome,
                                                                         metric = 'estimate',
                                                                         source = estimate.source,
                                                                         keep.dimensions = names(post.agg.dimnames),
                                                                         dimension.values = dimension.values[names(dimension.values) %in% names(pre.agg.dimnames)],
                                                                         target.ontology = target.ontology,
                                                                         allow.mapping.from.target.ontology = F)
                                        
                                        # If the estimate data came from only one source, we can remove the source so that it will match size of data
                                        non.source.dimensions = names(dim(estimate.data.for.cv))[names(dim(estimate.data.for.cv))!='source']
                                        if (dim(estimate.data.for.cv)['source'] == 1)
                                            estimate.data.for.cv = array(estimate.data.for.cv,
                                                                         dim = dim(estimate.data.for.cv)[non.source.dimensions],
                                                                         dimnames = dimnames(estimate.data.for.cv)[non.source.dimensions]
                                            )
                                        
                                        # Otherwise, we need to take the mean across source
                                        else estimate.data.for.cv = apply.robust(estimate.data.for.cv, non.source.dimensions, mean, na.rm=T)
                                        dimnames.in.common = get.dimension.values.overlap(dimnames(data.by.data.type), dimnames(estimate.data.for.cv))
                                        estimate.data.for.cv = array.access(estimate.data.for.cv, dimnames.in.common)
                                        data.by.data.type = array.access(data.by.data.type, dimnames.in.common)
                                        data.by.data.type = data.by.data.type / estimate.data.for.cv
                                    }
                                } else {
                                    data.by.data.type = apply.robust(data.by.data.type, names(post.agg.dimnames), function(x) {list(unique(unlist(x)))})
                                    # data.by.data.type = lapply(data.by.data.type, function(x) {x[[1]]})
                                    # dim(data.by.data.type) = sapply(post.agg.dimnames, length)
                                    # dimnames(data.by.data.type) = post.agg.dimnames
                                }
                            }
                            dimnames(data.by.data.type) = as.list(dimnames(data.by.data.type))
                            
                            # Now is when we unhash the url and details
                            if (data.type == "url")
                                data.by.data.type = self$unhash.url(data.by.data.type)
                            if (data.type == "details")
                                data.by.data.type = self$unhash.details(data.by.data.type)
                            
                            data.by.data.type
                        }) # end of lapply for data.types
                        
                        if (all(is.na(pulled.ont.data[[1]]))) {
                            pulled.ont.data = NULL
                            next
                        }
                        names(pulled.ont.data) = data.types
                        
                        # Now is when I'll finally unhash what we have for details and url
                        
                        
                        # SUCCESS FOR THIS ONTOLOGY - save info if this is the first ontology we've succeeded on for this source
                        if (is.null(pulled.source.data)) {
                            sources.successful.names <<- c(sources.successful.names, source.name)
                            # keep.dimensions <<- keep.dimensions
                            # need.to.set.keep.dimensions.flag <<- FALSE
                        }
                        break
                        
                    } # end of loop for stratification
                    # if (source.lacks.denominator.data.flag) break
                    
                    if (all(is.na(pulled.ont.data))) next
                    # Integrate ontology's pulled data into the source's pulled data -- they should all be in the same (universal) ontology and therefore compatible
                    new.source.data.dimnames = dimnames(pulled.ont.data[[1]])
                    if (!is.null(pulled.source.data)) {
                        for (d in names(new.source.data.dimnames)) {new.source.data.dimnames[[d]] = sort(union(new.source.data.dimnames[[d]], dimnames(pulled.source.data[[1]])[[d]]))}
                    }
                    pulled.source.data = lapply(data.types, function(data.type) {
                        
                        if (!is.null(pulled.source.data)) {
                            new.pulled.source.data.this.type = array(NA, sapply(new.source.data.dimnames, length), new.source.data.dimnames)
                            new.pulled.source.data.this.type[get.array.access.indices(new.source.data.dimnames, dimnames(pulled.source.data[[data.type]]))] = pulled.source.data[[data.type]]
                            pulled.data.na.mask = !is.na(pulled.ont.data[[data.type]])
                            new.pulled.source.data.this.type[get.array.access.indices(new.source.data.dimnames, dimnames(pulled.ont.data[[data.type]]))[pulled.data.na.mask]]= pulled.ont.data[[data.type]][pulled.data.na.mask]
                            dim(new.pulled.source.data.this.type) = sapply(new.source.data.dimnames, length)
                            dimnames(new.pulled.source.data.this.type) = new.source.data.dimnames
                        }
                        else
                            new.pulled.source.data.this.type = pulled.ont.data[[data.type]]
                        new.pulled.source.data.this.type
                        
                    })
                    names(pulled.source.data) = data.types
                    pulled.source.data <<- pulled.source.data
                    
                }  # end of loop for ontology
                pulled.source.data
            }) # end of lapply for sources
            
            # we have a list (one element per source) of lists (one element per data type, i.e. 'data', 'url', or 'details')
            # repackage this to be a data array with 'url', 'details' and possibly a mapping as attributes
            post.processed.data = NULL
            if (debug) browser()
            # Some sources may have returned NULL above and should be removed.
            pre.processed.data = pre.processed.data[!unlist(lapply(pre.processed.data, is.null))]
            # Extract data for data, url, and details out of what lapply returned above
            if (length(pre.processed.data) > 0) {
                for (data.type in c('data', append.attributes)) {
                    
                    # make a list of the data from the sources
                    data.by.source = lapply(pre.processed.data, function(x) {x[[data.type]]})
                    names(data.by.source) = sources.successful.names
                    
                    return.for.this.data.type = NULL
                    
                    overall.dim.names = c(dimnames(data.by.source[[1]]), list(source=sources.successful.names))
                    
                    # if more than one source, the overall.dim.names will have the union of all incomplete dimension values... sorted!
                    if (length(data.by.source) > 1) {
                        incomplete.dimension.values = lapply(incomplete.dimensions(target.ontology), function(d) {
                            sort(unique(unlist(lapply(data.by.source, function(src) {dimnames(src)[[d]]}))))
                        })
                        incomplete.dimension.values = incomplete.dimension.values[!sapply(incomplete.dimension.values, is.null)]
                        overall.dim.names[names(overall.dim.names) %in% incomplete.dimensions(target.ontology)] = incomplete.dimension.values
                        if (data.type == 'data')
                            return.for.this.data.type = array(NA, sapply(overall.dim.names, length), overall.dim.names)
                        else
                            return.for.this.data.type = array(list(NA), sapply(overall.dim.names, length), overall.dim.names)
                        
                        # for each source, write its data into the new array
                        for (s in seq_along(data.by.source)) {
                            src.dimnames = c(dimnames(data.by.source[[s]]), list(source=names(data.by.source)[[s]]))
                            return.for.this.data.type[get.array.access.indices(overall.dim.names, src.dimnames)] = data.by.source[[s]]
                        }
                    }
                    
                    else {
                        if (data.type == 'data') {
                            return.for.this.data.type = sapply(data.by.source, function(x) {x})
                            
                        } else {
                            for (source.data in data.by.source) {
                                return.for.this.data.type = append(return.for.this.data.type, source.data)
                            }
                        }
                        
                        dim(return.for.this.data.type) = sapply(overall.dim.names, length)
                        dimnames(return.for.this.data.type) = overall.dim.names
                    }
                    
                    # incorporate it into final.return
                    if (data.type == 'data') {
                        post.processed.data = return.for.this.data.type
                    } else if (data.type == 'url') {
                        attr(post.processed.data, 'url') = return.for.this.data.type
                    } else if (data.type == 'details') {
                        attr(post.processed.data, 'details') = return.for.this.data.type
                    }
                    
                }
                if (return.mapping.flag) attr(post.processed.data, 'mapping') = target.to.universal.mapping
            }
            # Rprof(NULL)
            post.processed.data
        },
        
        pull.age.robust = function(outcome,
                                   metric = 'estimate',
                                   keep.dimensions = NULL,
                                   dimension.values = NULL,
                                   sources = NULL,
                                   from.ontology.names = NULL,
                                   exclude.ontology.names = NULL,
                                   target.ontology = NULL,
                                   allow.mapping.from.target.ontology = T,
                                   append.attributes = NULL,
                                   allow.other.sources.for.denominator = F,
                                   na.rm = F,
                                   check.arguments = T,
                                   debug = F,
                                   ...,
                                   restratify.age = F,
                                   desired.age.brackets = NULL,
                                   smooth.infinite.age.to = 100,
                                   allow.extrapolation = F,
                                   method = c('monoH.FC','hyman')[1])
        {
            # validate pull arguments internally
            if (!restratify.age || !('age' %in% union(keep.dimensions, names(dimension.values)))) {
                rv = self$pull(outcome = outcome,
                               metric = metric,
                               keep.dimensions = keep.dimensions,
                               dimension.values = dimension.values,
                               sources = sources,
                               from.ontology.names = from.ontology.names,
                               exclude.ontology.names = exclude.ontology.names,
                               target.ontology = target.ontology,
                               allow.mapping.from.target.ontology = allow.mapping.from.target.ontology,
                               append.attributes = append.attributes,
                               allow.other.sources.for.denominator = allow.other.sources.for.denominator,
                               na.rm = na.rm,
                               check.arguments = check.arguments,
                               debug = debug,
                               ...)
            }
            else {
                rv = tryCatch(
                    {self$pull(outcome = outcome,
                               metric = metric,
                               keep.dimensions = keep.dimensions,
                               dimension.values = dimension.values,
                               sources = sources,
                               from.ontology.names = from.ontology.names,
                               exclude.ontology.names = exclude.ontology.names,
                               target.ontology = target.ontology,
                               allow.mapping.from.target.ontology = allow.mapping.from.target.ontology,
                               append.attributes = append.attributes,
                               allow.other.sources.for.denominator = allow.other.sources.for.denominator,
                               na.rm = na.rm,
                               check.arguments = check.arguments,
                               debug = debug,
                               ...)},
                    error=function(e){ NULL}
                )
                
                if (is.null(rv)) {
                    target.ontology = target.ontology[names(target.ontology)!='age']
                    rv = self$pull(outcome = outcome,
                                   metric = metric,
                                   keep.dimensions = keep.dimensions,
                                   dimension.values = dimension.values,
                                   sources = sources,
                                   from.ontology.names = from.ontology.names,
                                   exclude.ontology.names = exclude.ontology.names,
                                   target.ontology = target.ontology,
                                   allow.mapping.from.target.ontology = allow.mapping.from.target.ontology,
                                   append.attributes = append.attributes,
                                   allow.other.sources.for.denominator = allow.other.sources.for.denominator,
                                   na.rm = na.rm,
                                   check.arguments = check.arguments,
                                   debug = debug,
                                   ...)
                }
                if (is.null(rv)) return (rv)
                
                # check rv scale?
                url = attr(rv, 'url')
                details = attr(rv, 'details')
                pulled.mapping = attr(rv, 'mapping')
                # will need to transform a numerator and denominator to pass in to restratify age counts for proportions
                restratified.data = restratify.age.counts(counts = rv,
                                                          desired.age.brackets = desired.age.brackets,
                                                          smooth.infinite.age.to = smooth.infinite.age.to,
                                                          allow.extrapolation = allow.extrapolation,
                                                          na.rm = na.rm,
                                                          method = method,
                                                          error.prefix = "error restratifying age counts in age robust pull: ") #set
                restratify.mapping = attr(restratified.data, 'mapping')
                # I'll need to use the mapping returned as an attr on the above to map details and url
                attr(restratified.data, 'mapping') = attr(rv, 'mapping')
                if (any(append.attributes)=='url')
                    attr(restratified.data, 'url') = restratify.mapping$apply(url)
                if (any(append.attributes)=='details')
                    attr(restratified.data, 'details') = restratify.mapping$apply(details)
                #  if (!is.null(pulled.mapping)) attr(restratified.data, 'mapping') = pulled.mapping # NOT TRUE ANYMORE... (? what did I write this for?)
                rv = restratified.data
            }
            rv
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
        },
        
        get.year.bounds.for.outcome = function(outcome,
                                               metric='estimate',
                                               exclude.ontology.names = NULL)
        {
            if (is.null(outcome) || !is.character(outcome) || length(outcome) > 1 || is.na(outcome))
                stop("'outcome' must be a single, non-NA character value")
            if (!(outcome %in% names(private$i.outcome.info)))
                stop(paste0("'", outcome, "' is not a registered outcome."))
            if (!(outcome %in% names(private$i.data)))
                stop(paste0("there is no data for outcome '", outcome, "'"))
            if (!is.character(metric) || length(metric) != 1)
                stop("'metric' must be a single character value")
            if (!(metric %in% names(private$i.data[[outcome]])))
                stop(paste0("there is no data for metric '", metric, "' for outcome '", outcome, "'"))
            
            # *exclude.ontology.names* is either NULL or a character vector with no NA or empty values
            if (!is.null(exclude.ontology.names) && (!is.character(exclude.ontology.names) || anyNA(exclude.ontology.names) || any(nchar(exclude.ontology.names)==0)))
                stop(paste0(error.prefix, "'exclude.ontology.names' must be either NULL or a character vector with no NA or empty values"))
            # browser()
            earliest.year = Inf
            latest.year = -Inf
            for (source in private$i.data[[outcome]][[metric]]) {
                ontology.names = setdiff(names(source), exclude.ontology.names)
                for (ontology.name in ontology.names) {
                    for (strat in source[[ontology.name]]) {
                        if ('year' %in% names(dimnames(strat))) {
                            if (all(is.year.range(dimnames(strat)$year))) {
                                parsed.range = parse.year.ranges(dimnames(strat)$year)
                                starts = sort(parsed.range$start)
                                ends = sort(parsed.range$end, decreasing = T)
                                if (starts[[1]] < earliest.year) earliest.year = starts[[1]]
                                if (ends[[1]] > latest.year) latest.year = ends[[1]]
                            }
                            else {
                                years = sort(dimnames(strat)[['year']])
                                if (years[[1]] < earliest.year) earliest.year = years[[1]]
                                if (years[[length(years)]] > latest.year) latest.year = years[[length(years)]]
                            }
                        }
                    }
                }
            }
            if (earliest.year == Inf) {
                earliest.year = NULL
                latest.year = NULL
            }
            list(earliest.year = earliest.year, latest.year = latest.year)
        },
        
        get.locations.with.data = function(outcome,
                                           metric='estimate',
                                           years = NULL,
                                           exclude.ontology.names = NULL)
        {
            if (is.null(outcome) || !is.character(outcome) || length(outcome) > 1 || is.na(outcome))
                stop("'outcome' must be a single, non-NA character value")
            if (!(outcome %in% names(private$i.outcome.info)))
                stop(paste0("'", outcome, "' is not a registered outcome."))
            if (!(outcome %in% names(private$i.data)))
                stop(paste0("there is no data for outcome '", outcome, "'"))
            if (!is.character(metric) || length(metric) != 1)
                stop("'metric' must be a single character value")
            if (!(metric %in% names(private$i.data[[outcome]])))
                stop(paste0("there is no data for metric '", metric, "' for outcome '", outcome, "'"))
            # *years* is NULL or a numeric vector with no NAs or duplicates
            if (!is.null(years) && (!is.numeric(years) || any(is.na(years)) || any(duplicated(years))))
                stop("'years' must be NULL or a numeric vector with no NAs or duplicates")
            # *exclude.ontology.names* is either NULL or a character vector with no NA or empty values
            if (!is.null(exclude.ontology.names) && (!is.character(exclude.ontology.names) || anyNA(exclude.ontology.names) || any(nchar(exclude.ontology.names)==0)))
                stop(paste0(error.prefix, "'exclude.ontology.names' must be either NULL or a character vector with no NA or empty values"))
            
            unique(unlist(lapply(private$i.data[[outcome]][[metric]], function(source.data) {
                ontology.names = setdiff(names(source.data), exclude.ontology.names)
                unique(unlist(lapply(ontology.names, function(ontology.name) {
                    unique(unlist(lapply(source.data[[ontology.name]], function(stratification.data) {
                        if (!is.null(years)) {
                            new.dimnames = dimnames(stratification.data)
                            new.dimnames$year = intersect(years, new.dimnames$year)
                            data.for.years = array(stratification.data[get.array.access.indices(dimnames(stratification.data), dimension.values = list(year = intersect(years, dimnames(stratification.data)$year)))],
                                                   dim = sapply(new.dimnames, length),
                                                   dimnames = new.dimnames)
                            location.has.data = apply(data.for.years, MARGIN='location', FUN=function(x) {!all(is.na(x))})
                        } else
                            location.has.data = apply(stratification.data, MARGIN='location', FUN=function(x) {!all(is.na(x))})
                        names(location.has.data[location.has.data])
                    })))
                })))
            })))
        },
        
        get.ontologies.for.outcome = function(outcome, sources = NULL, exclude.ontology.names = NULL)
        {
            # VALIDATE ARGUMENTS
            error.prefix = "Error getting ontologies for outcome: "
            if (is.null(outcome) || !is.character(outcome) || length(outcome) > 1 || is.na(outcome))
                stop(paste0(error.prefix, "'outcome' must be a single, non-NA character value"))
            if (!(outcome %in% names(private$i.outcome.info)))
                stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome"))
            if (!(outcome %in% names(private$i.data))) return (NULL)
            # *sources* is either NULL or a character vector with at least one element and no NA or empty values
            #  that have all been registered previously as sources for this outcome with this data manager
            if (!is.null(sources) && (!is.character(sources) || !length(sources)>0 || anyNA(sources) || any(nchar(sources)==0)))
                stop(paste0(error.prefix, "'sources' must be NULL or a character vector with at least one element and no NA or empty values"))
            unregistered.sources = sapply(sources, function(x){
                !(x %in% unique(unlist(lapply(private$i.data[[outcome]], function(metric.data) {names(metric.data)}))))
            })
            if (any(unregistered.sources))
                stop(paste0(error.prefix, "all sources must be registered for this outcome with this data manager"))
            # *exclude.ontology.names* is either NULL or a character vector with no NA or empty values
            if (!is.null(exclude.ontology.names) && (!is.character(exclude.ontology.names) || anyNA(exclude.ontology.names) || any(nchar(exclude.ontology.names)==0)))
                stop(paste0(error.prefix, "'exclude.ontology.names' must be either NULL or a character vector with no NA or empty values"))
            
            # GET ONTOLOGIES IN REQUESTED SOURCES
            ont.names = unique(unlist(lapply(names(private$i.data[[outcome]]), function(metric.name) {
                unique(unlist(lapply(names(private$i.data[[outcome]][[metric.name]]), function(source.name) {
                    source.ontologies = names(private$i.data[[outcome]][[metric.name]][[source.name]])
                    if (is.null(sources) || source.name %in% sources) source.ontologies
                    else NULL
                })))
            })))
            ont.names = setdiff(ont.names, exclude.ontology.names)
            onts = lapply(ont.names, function(n) {self$get.registered.ontology(n)})
            names(onts) = ont.names
            onts
        },
        
        get.universal.ontology.for.outcome = function(outcome, exclude.ontology.names = NULL)
        {
            # VALIDATE ARGUMENTS
            error.prefix = "Error getting universal ontology for outcome: "
            if (is.null(outcome) || !is.character(outcome) || length(outcome) > 1 || is.na(outcome))
                stop(paste0(error.prefix, "'outcome' must be a single, non-NA character value"))
            if (!(outcome %in% names(private$i.outcome.info)))
                stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome"))
            # *exclude.ontology.names* is either NULL or a character vector with no NA or empty values
            if (!is.null(exclude.ontology.names) && (!is.character(exclude.ontology.names) || anyNA(exclude.ontology.names) || any(nchar(exclude.ontology.names)==0)))
                stop(paste0(error.prefix, "'exclude.ontology.names' must be either NULL or a character vector with no NA or empty values"))
            if (!(outcome %in% names(private$i.data))) return (NULL)
            private$get.universal.ontology(outcome, exclude.ontology.names=exclude.ontology.names, return.target.to.universal.mapping = F)
        },
        
        unhash.url = function(arr)
        {
            private$do.unhash(arr, private$i.url.list)
        },
        
        unhash.details = function(arr)
        {
            private$do.unhash(arr, private$i.details.list)
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
        
        creation.date = function(value)
        {
            if (missing(value))
                private$i.creation.date
            else
                stop("Cannot modify 'creation.date' in jheem.data.manager - it is read-only")
        },
        
        last.modified.date = function(value)
        {
            if (missing(value))
                private$i.last.modified.date
            else
                stop("Cannot modify 'last.modified.date' in jheem.data.manager - it is read-only")
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
        },
        
        data = function(value)
        {
            if (missing(value))
                private$i.data
            else
                stop("Cannot modify 'data' in jheem.data.manager - it is read-only")
        },
        
        url = function(value)
        {
            if (missing(value))
                private$i.url
            else
                stop("Cannot modify 'url' in jheem.data.manager - it is read-only")
        },
        
        details = function(value)
        {
            if (missing(value))
                private$i.details
            else
                stop("Cannot modify 'details' in jheem.data.manager - it is read-only")
        },
        
        url.list = function(value)
        {
            if (missing(value))
                private$i.url.list
            else
                stop("Cannot modify 'url.list' in jheem.data.manager - it is read-only")
        },
        
        details.list = function(value)
        {
            if (missing(value))
                private$i.details.list
            else
                stop("Cannot modify 'details.list' in jheem.data.manager - it is read-only")
        },
        
        outcome.info = function(value)
        {
            if (missing(value))
                private$i.outcome.info
            else
                stop("Cannot modify 'outcome.info' in jheem.data.manager - it is read-only")
        },
        
        source.info = function(value)
        {
            if (missing(value))
                private$i.source.info
            else
                stop("Cannot modify 'source.info' in jheem.data.manager - it is read-only")
        },
        
        parent.source.info = function(value)
        {
            if (missing(value))
                private$i.parent.source.info
            else
                stop("Cannot modify 'parent.source.info' in jheem.data.manager - it is read-only")
        },
        
        ontologies = function(value)
        {
            if (missing(value))
                private$i.ontologies
            else
                stop("Cannot modify 'ontologies' in jheem.data.manager - it is read-only")
        },
        
        code.iteration = function(value)
        {
            if (missing(value))
                private$i.code.iteration
            else
                stop("Cannot modify 'code.iteration' in jheem.data.manager - it is read-only")
        }
    ),
    
    private = list(
        
        #-- About the Data Manager itself --#
        i.name = NULL,
        i.description = NULL,
        i.code.iteration = NULL,
        i.creation.date = NULL,
        i.last.modified.date = NULL,
        
        #-- Storage structures for data and metadata --#
        # These three are lists of lists of lists of lists, indexed
        # [[outcome]][[source]][[ontology.name]][[stratification]]
        # for i.data, each element of this depth-4 access is a numeric array
        # for i.url and i.details, each element is a list array (ie a list with dimnames set), each element of which is a character vector
        i.data = NULL,
        i.url = NULL,
        i.details = NULL,
        i.url.list = NULL,
        i.details.list = NULL,
        
        #-- Metadata --#
        # These are named lists, with the names being the names of outcomes or data groups
        i.outcome.info = NULL,
        i.source.info = NULL,
        i.parent.source.info = NULL,
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
        },
        
        get.universal.ontology = function(outcome, sources = NULL, from.ontology.names = NULL, exclude.ontology.names = NULL, target.ontology = NULL, return.target.to.universal.mapping = T, debug = F)
        {
            if (debug) browser()
            onts = self$get.ontologies.for.outcome(outcome, sources, exclude.ontology.names = exclude.ontology.names)
            if (!is.null(from.ontology.names)) onts = onts[names(onts) %in% from.ontology.names]
            if (length(onts)==0) return(NULL) # means we have no ontologies in "from.ontology.names" that are for this outcome
            uni = onts[[1]]
            if (length(onts) > 1) {
                for (i in 2:length(onts)) {
                    mps = get.mappings.to.align.ontologies(onts[[i]], uni, allow.non.overlapping.incomplete.dimensions = T)
                    if (is.null(mps))
                    {
                        DATA.MANAGER.ONTOLOGY.ERRORS$did.you.remember.details = list(
                            onts.i = onts[[i]],
                            uni = uni
                        )
                        stop("Error mapping ontologies for outcome '", outcome, "': did you remember to register your mappings for ontology '", names(onts)[[i]],"'?")
                    }
                    uni = mps[[2]]$apply.to.ontology(uni)
                    
                }
            }
            if (!is.null(target.ontology)) {
                mps = get.mappings.to.align.ontologies(target.ontology, uni, allow.non.overlapping.incomplete.dimensions = T)
                if (is.null(mps))
                {
                    DATA.MANAGER.ONTOLOGY.ERRORS$did.you.remember.details = list(
                        target.ontology = target.ontology,
                        uni = uni
                    )
                    stop("Error mapping ontologies to target ontology for outcome '", outcome, "': did you remember to register your mappings?")
                }
                uni = mps[[2]]$apply.to.ontology(uni)
                if (return.target.to.universal.mapping) attr(uni, 'target.to.universal.mapping') = mps[[1]]
            }
            uni
        },
        do.unhash = function(arr, hash.list) {
            # We will take hashed vectors like c(1,3) and unhash each member
            # and also un-collapse components like "first__second" by "__" char.
            # Then we will sort/unique them and check if it matches any. If does,
            # add to our cache
            cache = hash.list # which should now be a vector
            names(cache) = as.character(seq_along(hash.list))
            # returns a vector array
            new.arr = sapply(arr, function(hashed.vector) {
                # ignore any with NA because this implies that the corresponding
                # data point becomes NA in the mapping that was applied
                if (any(is.na(hashed.vector))) NA
                else {
                    # skip a step if we have cached this combo
                    cache.index = paste(hashed.vector, collapse="__")
                    if (cache.index %in% names(cache)) return(cache[[cache.index]])
                    
                    unhashed = paste(sort(unique(unlist(strsplit(sapply(hashed.vector, function(hashed.value) {private$i.details.list[[hashed.value]]}), "__")))), collapse = "__")
                    cache[[cache.index]] = unhashed
                    unhashed
                }
            })
            dim(new.arr) = dim(arr)
            dimnames(new.arr) = dimnames(arr)
            new.arr
        }
        
    )
)
