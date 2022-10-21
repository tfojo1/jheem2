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

    # We only allow consequtive years; we assume that the numbers themselves
    # are valid years, and we allow both integers and doubles

    entry.length <- length(year.value)
    type <- typeof(year.value)

    rv <- NA
    # String?
    if (type == "character") {
        if (entry.length == 1) {
            # One String
            # Split on -
            dash.split = str_split(year.value, "-")[[1]]
            dash.split.length = length(dash.split)
            if (dash.split.length == 1) {
                # At this point we are reasonably sure it's a single year,
                # Let's try to return it.  Will return NA if not valid.
                rv = c(suppressWarnings(as.numeric(year.value)))
            } else if (dash.split.length == 2) {
                # There is only one dash, we have a date range
                years = suppressWarnings(as.numeric(dash.split))
                if (!any(is.na(years))) {
                    # We have valid years for both entries
                    # Check if the first year is less than or equal to the second
                    if (years[1] <= years[2]) {
                        rv = years[1]:years[2]
                    }
                }
            } 
            # If dash split length is greater than 2, we have an 
            # invalid range
        } else {
            # We have a vector of character years
            # Check if numeric
            years = suppressWarnings(as.numeric(year.value))
            if (!any(is.na(years))) {
                # We have a valid list of numbers
                # Check if consequtive:
                if (all(diff(years) == 1)) {
                    # If so, we're done
                    rv = years
                }
            }
        }
    } 
    # Double or Integer?
    else if (type == "double" || type == "integer") {
        if (entry.length == 1) {
            # It's a single number / vector, just return it
            rv = c(year.value)
        } else {
            # It's a multi number vector, check if consequtive     
            if (all(diff(year.value) == 1)) {
                rv = year.value
            }
        }
    }
    # List?
    else if (type == "list") {
        # Unsure if we want this supported as of now
    }

    rv
}

get.age.bounds.for.age.value <- function(surveillance.manager,
                                     age.value)
{
    # for example
    # age.value = "13-24 years" --> c(13,25)
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
