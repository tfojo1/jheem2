#' Package initialization
#' @noRd
.onLoad <- function(libname, pkgname) {
    # Create the NULL intervention after package is loaded
    # This ensures jheem.core is fully loaded first
    NULL.INTERVENTION$new()
}
