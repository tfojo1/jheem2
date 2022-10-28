library(stringr)

#'@return An array that is indexed [outcome, location, year, <other stratifying dimension>]
#'
#'@examples To pull age-stratified prevalence from Baltimore MSA: get.surveillance.data(manager, 'prevalence', '12580', keep.dimensions=c('year','age'))
get.surveillance.data <- function(surveillance.data.manager,
                                  outcomes,
                                  years,
                                  locations,
                                  dimension.values=list(),
                                  keep.dimensions,
                                  aggregate.locations,
                                  aggregate.years,
                                  na.rm)
{
    
    
    
}

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
            dash.split <- str_split(input, "-")[[1]]
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

    #Check if consequtive and return
    rv
    
}
get.years.for.year.value <- function(surveillance.manager,
                                     year.value)
{
    # I believe this should take a value and convert it into a vector representation
    # So if there is only one entry, we return it as c(entry).
    # This way we have a consistent return value, a vector of values (possibly none, if not
    # a year)

    # for example
    # year.value = 2020 or '2020' or c(2020) -> c(2020)
    # year.value = "2018-2020" --> c(2018,2019,2020)
    # year.value = 2018:2020 --> c(2018,2019,2020)
    # year.value = c("2018","2019") --> c(2018,2019)
    # year.value = c(2018, 2019, 2020) --> c(2018, 2019, 2020)

    # If this function is unable to determine what years are being
    # asked for it will return NA

    # We only allow consequtive years, but we allow forwards or backwards.
    # We assume that the numbers themselves are valid years, and we allow 
    # both integers and doubles
    
    # This functionality has been reproduced in the more generic function return.as.vectors

    return.as.vectors ( year.value )
}

get.age.bounds.for.age.value <- function(surveillance.manager,
                                     age.value)
{
    # for example
    # age.value = "13-24 years" --> c(13,25)
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
