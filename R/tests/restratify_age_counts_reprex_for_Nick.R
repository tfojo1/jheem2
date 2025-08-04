
# For Nick to help test the restratify.age.counts method on some real data.
# The data I'm using here should be small enough to work with while having what
# you need. Let me know if there's more I can provide!

# source the "restratify.age.counts" function however you need
# It's originally located at:
source("../jheem2/R/HELPERS_age_year_helpers.R")

age_data <- get(load(file="../jheem2/R/tests/restratify_age_counts_reprex_for_Nick.Rdata"))

#' @description
#' Pads out the age dimension so that 'restratify.age.counts' acts reasonably.
#' Takes "55+ years" and converts it to "55-x years" where x is the "top.age".
#' Also creates an additionally category that extends beyond the "top.age".
#' @param arr Array with dimensions "year" and "age". The full version I use
#' allows "location" and "sim" as dimensions, but I simplified this one a bit.
#' @param top.age The age where the count will taper to zero.
#' 
do_prepare_for_restratify <- function(arr, top.age=100) {
    original_dimensions <- names(dim(arr))
    reordered_dimensions <- c(original_dimensions[original_dimensions!="age"], "age")
    arr_reordered <- apply(arr, reordered_dimensions, function(x) {x})
    new_dimnames <- dimnames(arr_reordered)
    new_dimnames$age[5] <- paste0("55-", as.character(top.age), " years")
    new_dimnames$age <- c(new_dimnames$age, paste0(as.character(top.age+1), "-", as.character(top.age+10), " years"))
    arr_reordered <- c(arr_reordered, rep(0, dim(arr_reordered)["year"]))
    arr_restored <- array(arr_reordered, sapply(new_dimnames, length), new_dimnames)
    apply(arr_restored, original_dimensions, function(x) {x})
}

#' @description
#' Wrapper for "restratify.age.counts" to work on arrays of many dimensions.
#' Although I simplified here to only take arrays with "year" and "age"...
#' So it might not be necessary. Nromally there are also "location" and "sim"
#' dimensions.
#' @inheritParams do_prepare_for_restratify
#' 
get_restratified_ages <- function(arr, top.age=100) {
    apply(do_prepare_for_restratify(arr, top.age),
          "year", function(one_year) {
              restratify.age.counts(one_year,
                                    desired.age.brackets = 13:top.age)
          })
}


test <- get_restratified_ages(age_data)
