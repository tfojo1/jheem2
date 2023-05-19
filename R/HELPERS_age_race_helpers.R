
#'@title Make Names for a Set of Age Strata
#'
#'@param endpoints A numeric vector of at least two points. endpoints[1] is the lower bound (inclusive) of the first stratum, endpoints[2] is the upper bound (exclusive) for the first stratum and the lower bound for the second stratum, etc.
#'
#'@return A character vector with length(endpoints)-1 values
#'
#'@export
make.age.strata.names <- function(endpoints=NULL,
                                  lowers=NULL,
                                  uppers=NULL)
{
    if (!is.null(lowers)) # going to (try to) use lowers + uppers
    {
        if (is.null(uppers))
            stop("If 'lowers' is specified (not NULL), 'uppers' must be specified as well")
        if (!is.null(endpoints))
            stop("You must specify EITHER 'endpoints' OR 'lowers'+'uppers' - they cannot all be non-NULL")
        
        if (!is.numeric(lowers))
            stop("'lowers' must be a numeric vector")
        if (length(lowers)==0)
            stop("'lowers' must contain at least one value")
        if (any(is.na(lowers)))
            stop("'lowers' cannot contain NA values")
        
        if (!is.numeric(uppers))
            stop("'uppers' must be a numeric vector")
        if (any(is.na(uppers)))
            stop("'uppers' cannot contain NA values")
        if (length(uppers) != length(lowers))
            stop(paste0("'uppers' (length ", length(uppers), 
                        ") must be the same length as 'lowers' (", length(lowers), ")"))
    }
    else if (is.null(uppers)) # error - lowers but not uppers
    {
        if (is.null(uppers))
            stop("If 'uppers' is specified (not NULL), 'lowers' must be specified as well")
    }
    else # we are using endpoints
    {
        if (!is.numeric(endpoints))
            stop("'endpoints' must be a numeric vector")
        
        if (length(endpoints)<2)
            stop("'endpoints' must contain at least two values")
        
        if (any(is.na(endpoints)))
            stop("'endpoints' cannot contain NA values")
        
        lowers = endpoints[-length(endpoints)]
        uppers = endpoints[-1]
    }
    
    rv = paste0(lowers, "-", uppers-1, " years")
    rv[is.infinite(uppers)] = paste0(lowers[is.infinite(uppers)], "+ years")
    rv[(lowers+1)==uppers] = paste0(lowers, " years")
    rv[lowers==1 & uppers==2] = '1 year'
    
    rv
}



#'@title Convert Age Strata Names into Lower and Upper Bounds for Each Stratum
#'
#'@param strata.names Names of age brackets. The function knows how to parse names generated in the format given by \link{make.age.strata.names}, as well as some other common formats
#'
#'@return A list with two elements, $lowers and $uppers, representing the lower (inclusive) and upper (exclusive) bounds of each age stratum
#'
#'@export
parse.age.strata.names <- function(strata.names)
{
    # Validate
    if (!is.character(strata.names))
        stop("'strata.names' must be a character vector")
    
    if (length(strata.names)==0)
        stop("'strata.names' must contain at least one value")
    
    if (any(is.na(strata.names)))
        stop("'strata.names' cannot contain NA values")
    
    # Massage out text suffixes
    years.mask = grepl(" years$", strata.names) #we'll do this first, since it's the default
    strata.names[years.mask] = substr(strata.names[years.mask], 1, nchar(strata.names[years.mask])-6)
    
    if (!all(years.mask))
    {   
        one.year.mask = grepl(" year$", strata.names) #this next, since it's also used by the default
        strata.names[one.year.mask] = substr(strata.names[one.year.mask], 1, nchar(strata.names[one.year.mask])-5)
        
        if (!all(years.mask | one.year.mask))
        {
            years.old.mask = grepl(" years old$", strata.names) #this next, since it's also used by the default
            strata.names[years.old.mask] = substr(strata.names[years.old.mask], 1, nchar(strata.names[years.old.mask])-10)
        }
    }
    
    # Divide up the three ways to parse
    # <age>+
    # <age>-<age>
    # <age>    
    
    uppers = lowers = numeric(length(strata.names))
    
    dash.position = sapply(strsplit(strata.names, ''), function(chars){
        (1:length(chars))[chars=='-'][1]
    })
    infinite.upper.mask = substr(strata.names, nchar(strata.names), nchar(strata.names)) == "+" | substr(strata.names, 1, 1)=='>'
    zero.lower.mask = substr(strata.names, 1, 1)=='<'
    age.range.mask = !is.na(dash.position)
    single.age.mask = !age.range.mask & !infinite.upper.mask & !zero.lower.mask
    
    # Parse infinite upper
    lowers[infinite.upper.mask] = suppressWarnings(as.numeric(gsub("(>=*|\\+)", '', strata.names[infinite.upper.mask])))
    uppers[infinite.upper.mask] = Inf
    
    # Parse zero lower
    lowers[zero.lower.mask] = 0
    uppers[zero.lower.mask] = suppressWarnings(as.numeric(gsub("<=*", '', strata.names[zero.lower.mask])))
    
    # Parse age range
    lowers[age.range.mask] = suppressWarnings(as.numeric(substr(strata.names[age.range.mask],
                                                                1, dash.position[age.range.mask]-1)))
    uppers[age.range.mask] = 1+suppressWarnings(as.numeric(substr(strata.names[age.range.mask],
                                                                  dash.position[age.range.mask]+1, nchar(strata.names[age.range.mask]))))
    
    
    # Parse single age
    lowers[single.age.mask] = suppressWarnings(as.numeric(strata.names[single.age.mask]))
    uppers[single.age.mask] = lowers[single.age.mask] + 1
    
    # Return
    if (any(is.na(uppers)) || any(is.na(lowers)))
        NULL
    else
        list(lower=lowers, upper=uppers)
}


#'@title Convert Age Strata Names to a Standard Form
#'
#'@param strata.names A character vector of names of age strata
#'
#'@details Calls \code{\link{parse.age.strata.names}} and feeds the output into \code{\link{make.age.strata.names}}
#'
#'@export
standardize.age.strata.names <- function(strata.names)
{
    parsed = parse.age.strata.names(strata.names)
    if (is.null(parsed))
        stop("Unable to parse given strata.names")
    
    make.age.strata.names(lowers=parsed$lower, uppers=parsed$upper)
}


#'@title Convert Race Names to a Standard Form
#'
#'@param race.names A character vector containing race names
#'
#'@export
standardize.race.names <- function(race.names)
{
    # We need to make this match CDC reporting categories
}
