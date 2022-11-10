#load("../../cached/msa.surveillance.Rdata")
load("../cached/msa.surveillance.Rdata")

#' get.outcomes.for.outcomes.vector
#'
#' Process outcomes; return a valid outcome string vector and an invalid entry vector as a list
#'
#' @param outcome_list Some type of input containing various outcomes
#'
#' @return a vector of outcomes
get.outcomes.for.outcomes.vector <- function (outcome_list) {

    #This does a simple check against the list of allowed data types
    ALLOWED.DATA.TYPES = c('prevalence', 'new', 'mortality', 'diagnosed',
                           'diagnosed.ci.lower', 'diagnosed.ci.upper',
                           'suppression', 'suppression.ci.lower', 'suppression.ci.upper',
                           'prevalence.for.continuum', 'suppression.of.engaged',
                           'estimated.prevalence', 'estimated.prevalence.ci.lower',
                           'estimated.prevalence.ci.upper', 'estimated.prevalence.rse',
                           'cumulative.aids.mortality', 'aids.diagnoses',
                           'linkage', 'engagement', 'new.for.continuum', 'retention',
                           'prep', 'testing', 'testing.n')
    
    bad_outcomes = outcome_list [ ! outcome_list %in% ALLOWED.DATA.TYPES ]

    if (length(bad_outcomes) > 0) {
        # We asked for outcomes that we don't have in the results
        # We may want to switch to scanning the dataset in the future
        # Show them to the user calling the function
        warning(paste0("Requested outcomes (",paste(bad_outcomes, sep= ","),") not found, skipping."))
    }

    outcome_list[ outcome_list %in% ALLOWED.DATA.TYPES ]
}

#' return.as.vectors
#'
#' Given some input, attempt to return it as a list of double vectors.
#' I believe this should take a value and convert it into a vector representation
#' So if there is only one entry, we return it as c(entry).
#' This way we have a consistent return value, a vector of values (possibly none, if not
#' a year)
#' 
#'   for example
#'     year.value = 2020 or '2020' or c(2020) -> c(2020)
#'     year.value = "2018-2020" --> c(2018,2019,2020)
#'     year.value = 2018:2020 --> c(2018,2019,2020)
#'     year.value = c("2018","2019") --> c(2018,2019)
#'     year.value = c(2018, 2019, 2020) --> c(2018, 2019, 2020)
#'
#' If this function is unable to determine what years are being
#' asked for it will return NA
#'
#' We only allow consequtive years, but we allow forwards or backwards.
#' We assume that the numbers themselves are valid years, and we allow 
#' both integers and doubles
#'
#' @param input The input to the function; either already doubles/integers or characters
#'
#' @return A vector representing the list of sequential integers represented by the input, or NA
#'
return.as.vectors <- function ( input ) {
    # This is a generic function that we can use to repicate the similiar behaivours in 
    # the year and age check functions being used with the data manager

    cons_check <- function( vec ) {
        # This function checks the vec to see if the values are continuous, either
        # forward or backward
        rv <- NA
        vec_diff <- diff(vec)
        if (all(vec_diff == 1) || all(vec_diff == -1)) {
            rv <- vec
        }
        rv
    }

    entry.length <- length(input)
    type <- typeof(input)

    rv <- NA
    # String?
    if (type == "character") {
        if (entry.length == 1) {
            # One String
            # Split on -
            dash.split <- strsplit(input, "-")[[1]]
            #Convert the resulting 1+ sized vector to numeric
            if (length(dash.split) <= 2) {
                val <- suppressWarnings(as.numeric(dash.split))
                #If all results are not NA
                if (!anyNA(val)) {
                    rv <- val[1]:val[length(val)]
                }
            }
        } else {
            # We have a vector of character years
            # Check if numeric
            val <- suppressWarnings(as.numeric(input))
            if (!anyNA(val)) {
                # We have a valid list of numbers
                rv <- cons_check(val)
            }
        }
    } 
    # Double or Integer?
    else if (type == "double" || type == "integer") {
        rv <- cons_check(input)
    }
    # List?
    else if (type == "list") {
        # Unsure if we want this supported as of now
    }

    #Check if consecutive and return
    rv
    
}

#' get.years.for.year.value
#'
#' Takes input that can be represented as a sequential list of years and returns
#' the years that are represented by this input.
#'
#' @param surveillance.manager A reference to the usable surveillance.manager (needed?)
#' @param year.value Some value representing which years we would like to examine
#'
#' @return A list of numerical years if input is sufficient, NA otherwise
#'
get.years.for.year.value <- function(surveillance.manager,
                                     year.value)
{
    return.as.vectors ( year.value )
}

#' get.age.bounds.for.age.value
#'
#' Takes input that can be represented as a sequential list of ages and returns
#' the ages that are represented by this input.
#'     for example:
#'         age.value = "13-24 years" --> c(13,25)
#'
#' If the value contains the string " years", it is removed.  There is also the special
#' case of a value such as "55+ years", which will need an upper cap.  I will use 80 here.
#'
#' @param surveillance.manager A reference to the usable surveillance.manager (needed?)
#' @param age.value Some value representing which ages we would like to examine
#'
#' @return A list of numerical ages if input is sufficient, NA otherwise
#'
get.age.bounds.for.age.value <- function(surveillance.manager,
                                     age.value)
{
    age.upper.bound = 80
    type <- typeof(age.value)
    if (type == "character") {
        age.value <- gsub("years", "", age.value)
        # We use 'sub' rather than 'gsub' because a second plus sign represents an error
        age.value <- sub ("+", paste("-", age.upper.bound) , age.value, fixed=TRUE)
    }
    return.as.vectors(age.value)
}

get.races.for.race.value <- function()
{
    # black -> black
    # hispanic -> hispanic
    # other -> white, AAPI, american_indian_or_alaska_native, other
}

# Underlying structure
# surveillance.manager$datasets[[outcome]] is a list with one element for every possible stratification
# surveillance.manager$datasets[[outcome]][['total']], surveillance.manager[[outcome]][['age_sex']], surveillance.manager[[outcome]][['sex_race_risk']]
# surveillance.manager$datasets[[outcomes]][[stratification]] is a list with elements
#   $ years
#   $ dimension.values
#   $ data - array indexed [location, year, <other dimensions>]
#   $ source
#   $ url
#   $ details
#
# surveillance.manager$metadata
#
# surveillance.manager$metadata[[outcome]] has elements
#   $ is.proportion
#   $ denominator.outcome
#
# surveillance.manager$source.mapping
# surveillance.manager$url.mapping
# surveillance.manager$details.mapping

#' get.surveillance.data
#'
#' Function to extract specific data from the surveillance.data.manager object
#' 
#'@return An array that is indexed [outcome, location, year, <other stratifying dimension>]
#'
#'@examples To pull age-stratified prevalence from Baltimore MSA: get.surveillance.data(manager, 'prevalence', '12580', keep.dimensions=c('year','age'))
get.surveillance.data <- function(surveillance.data.manager=msa.surveillance,
                                  outcomes,
                                  years,
                                  locations,
                                  dimension.values=list(),
                                  keep.dimensions,
                                  aggregate.locations,
                                  aggregate.years,
                                  na.rm)
{
    
    # How are outcomes going to come in?  Currently it accepts list of characters
    # that match values in the ALLOWED list only.
    #
    # c("new","prevalence", "testing.n", ...)
    # 
    # The data is organized very specifically in msa.surveillance; each outcome is given its own
    # entry:
    #
    # msa.surveillance["new.all"] and so on.  

    requested.outcomes = get.outcomes.for.outcomes.vector(outcomes)

    requested.years = get.years.for.year.value(surveillance.data.manager, years)
    
}


