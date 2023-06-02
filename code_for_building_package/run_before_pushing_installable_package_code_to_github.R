
##--------------------------------------------------------------------------------------------##
##--                                     OVERVIEW                                           --##
##--------------------------------------------------------------------------------------------##
##
## Source this file prior to pushing an 'installable' package up to GitHub
## It will take care of updating documentation and internally-cached data structures
##
## This depends on packages
## - usethis
## - devtools
## Which do NOT have to be imported by the package itself (just used to set up the package)
##
##--------------------------------------------------------------------------------------------##


##-----------------------------------------------------------##
##-- STEP 1: Update Documentation (updates NAMESPACE file) --##
##-----------------------------------------------------------##

cat("\n-----------------------------\nSETTING UP DOCUMENTATION...\n")
devtools::document(quiet=T)
cat("DONE SETTING UP DOCUMENTATION\n-----------------------------\n")


##-----------------------------------------------------------------##
##-- STEP 2: Indicate Which Files the Package should NOT include --##
##-----------------------------------------------------------------##

cat("\n------------------------------------\nSETTING FILES TO IGNORE IN BUILD...\n")
usethis::use_build_ignore(files=c("^R\\tests$", 
                                  "^data-raw$",
                                  "^\\.github$",
                                  "^.*\\.Rproj$"),
                          escape=F)
cat("DONE SETTING FILES TO IGNORE IN BUILD\n------------------------------------\n")

##---------------------------------------------------------##
##-- STEP 3: Build and Store the Cached Location Manager --##
##---------------------------------------------------------##

cat("\n------------------------------\nBUILDING LOCATION MANAGER...\n")

# Read the location manager
source('code_for_building_package/set_up_cached_location_manager.R')
# Store it to an internal file for the package
usethis::use_data(LOCATION.MANAGER, internal = T, overwrite = T)

cat("DONE BUILDING LOCATION MANAGER\n------------------------------\n")


cat("\n\n ALL DONE PREPARING PACKAGE TO BE INSTALLED\n")