#load("../../cached/msa.surveillance.Rdata")
load("../cached/msa.surveillance.Rdata")

#' get.outcomes.for.outcomes.vector
#'
#' Process outcomes; return a valid outcome string vector and an invalid entry vector as a list
#'
#' @param outcome_list Some type of input containing various outcomes
#' @param valid_outcomes A list of valid outcomes drawn from the data.manager
#'
#' @return a vector of outcomes
get.outcomes.for.outcomes.vector <- function (outcome_list, valid_outcomes) {

    bad_outcomes = outcome_list [ ! outcome_list %in% valid_outcomes ]

    if (length(bad_outcomes) > 0) {
        # We asked for outcomes that we don't have in the results
        # We may want to switch to scanning the dataset in the future
        # Show them to the user calling the function
        warning(paste0("Requested outcomes (",paste(bad_outcomes, sep= ","),") not found, skipping."))
    }

    outcome_list[ outcome_list %in% valid_outcomes ]
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
    
    # Keep dimension
    # retained for final output
    # dimension.values
    # named list : names are dimension, values are the values we're looking for
    # not listed?  get all

    # check dimension.values and keep.dimension for exactly which outcome we are looking for

    # Instead of ALLOWED list, scan the data.manager for viable entries for outcomes

    # 2 dimension in data: year dimension and location dimension
    # keep dimension could eliminate either year or location
    # ALMOST always keep by year


    # The data is organized very specifically in msa.surveillance; each outcome is given its own
    # entry:
    #
    # surveillance.data.manager["new.all"], surveillance.data.manager["prevalence.age.sex"] and so on.  

    # This is a list of the names that are not outcomes from msa.surveillance specifically, 
    # but I imagine that all data managers share the same structure internally.
    data.manager.outcome.exclusion = 
        c("code.map","source","url","details","params","DIMENSION.VALUES")

    # Similar to above; these are the dimenison for msa.surveillance; if there are more then
    # they should be added to this list as well.  They are stripped off the outcomes from the valid.
    # outcomes so we can pass a more concise list to get.outcomes.for.outcome.vector, which checks only
    # that the outcome is valid.
    # This list also functions as the order for which to place these after the . in the naming
    # convention
    dimensions = c("all","sex","age","race","risk")

    # The dimension regex string looks like this:
    #     ".all|.sex|.age|.race|.risk"
    # With all the periods properly escaped so they will match a literal '.' The vertical pipes
    # are the 'or' operator. 
    dimension.string = paste(paste("\\.",dimensions,sep=""), collapse="|")

    # Get a vector of the data manager list member names
    data.manager.members = names(surveillance.data.manager)

    # Remove those listed in data.manager.outcome.exclusion as they are list elements but not outcomes
    raw.valid.outcomes = data.manager.members[! data.manager.members %in% data.manager.outcome.exclusion]
    
    # Outcomes
    # For the remaining full outcomes, strip away all the dimensions and return a list of unique 
    # outcomes.  
    clean.outcomes = unique( gsub(dimension.string, "", raw.valid.outcomes ) )
    # Compare them to the parameter vector, extract the good ones
    requested.outcomes = get.outcomes.for.outcomes.vector(outcomes, clean.outcomes)

    # Figure out what dimension are available in the data.manager for requested.outcomes:
    # We have both new and new.for.continuum as outcomes, we must diffentiate, hence
    # the following regular expression
    avail.dims = lapply(requested.outcomes, function (outcome) {
        raw.valid.outcomes [ grepl(
                                 paste0("^",outcome,"(",dimension.string,")+"), 
                                 raw.valid.outcomes) ]
    })


    only.dims = apply(cbind(requested.outcomes, avail.dims), 1, function (outcome_table) {
        strsplit (
                  sub(
                      paste0(outcome_table$requested.outcomes,"."),
                      "", 
                      outcome_table$avail.dims), 
                  ".", 
                  fixed = T 
        )
    })

    print (avail.dims)
    print (only.dims)

    # Years
    requested.years = get.years.for.year.value(surveillance.data.manager, years)

    
}

# FUTURE CONSIDERATIONS

    # In the future, in order to be more general and support multiple data.managers, I believe we should
    # create a separate list within the list called 'outcomes' where these are listed.  This would
    # prevent mistakes while reading the names from the surveillance.data.manager, and obviate the need
    # for data.manager.outcome.exclusion()

    # We could further subdivide them by dimensions as well - 
    #
    # surveillance.data.manager[['outcomes']][['new']][['all']]
    # surveillance.data.manager[['outcomes']][['prevalence'][['all']]
    #
    # This would allow us to have a correct list of available dimenions for each outcome, rather
    # than relying on the dimension variable above.
    #
    # There are also outcomes (linkage, engagement, suppression, diagnosed) which have both a 
    # full field and a numerator and denominator. For example :

    # linkage.denominator.age.sex
    # linkage.numerator.age.sex
    # linkage.age.sex
    # 
    # In some of these, linkage.age.sex for instance, the value of linkage.age.sex is equal to
    # linkage.denominator.age.sex / linkage.numerator.age.sex.  In others however, such as linkage.age,
    # the numerator and denominator have 6 fewer years and 8 fewer locations than linkage.age.
    #
    # For the cases where the full value is equal to the division of the other two, we could think about
    # keeping only the numerators and denominators if we find we're using too much RAM
