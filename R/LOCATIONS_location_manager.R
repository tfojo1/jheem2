

##-----------------------------##
##-----------------------------##
##-- PUBLIC-FACING INTERFACE --##
##-----------------------------##
##-----------------------------##

##-------------##
##-- Getters --##
##-------------##

#'@description Get the Name of a Location
#'
#'@param locations A character vector of location codes
#'
#'@return A character vector of location names, with length(locations) and names=locations. If location codes are not registered (or if they were NA), the corresponding returned name is NA
#'
#'@export
get.location.name <- function(locations)
{
    # How do we handle NAs?
    # I think throw an error
    # Although we could return NA value
}

#'@description Get an Alias Associated with a Location
#'
#'@param locations A character vector of location codes
#'@param alias.name A single character value representing a previously registered alias
#'@param throw.error.if.unregistered.alias A single logical value indicating whether the function should throw an error if no alias with name alias.name has been registered
#'
#'@return A character vector of aliases, with length(locations) and names=locations. If location codes are not registered (or if they were NA), or if no value for the alias.name has been registered to a location, the corresponding returned alias is NA
#'
#'@export
get.location.alias <- function(locations, alias.name,
                               throw.error.if.unregistered.alias=T)
{
    
}

#'@description Get the Type (Geographic Resolution) of a Location
#'
#'@param locations A character vector of location codes
#'
#'@return A character vector of location types, with length(locations) and names=locations. If location codes are not registered (or if they were NA), the corresponding returned type is NA
#'
#'@export
get.location.type <- function(locations)
{
    
}

#'@description Get Locations that Fall Within a Location
#'
#'@param locations A character vector of location codes
#'@param sub.type The type (geographic resolution) of locations requested for the sub-locations
#'@param limit.to.completely.enclosing A single logical value indicating whether ONLY sub-locations that fall COMPLETELY within the given locations should be returned
#'@param return.list A single logical value indicating whether the return value should be a list with one element for each location, or whether all sub-locations should be 'unlisted' into a vector
#'@param throw.error.if.unregistered.type A single logical value indicating whether the function should throw an error if sub.type has not been registered as a location type
#'
#'@return If return.list==T, a list with length(locations) and names=locations. Each element is itself a character vector with zero or more locations corresponding to sub-locations. If return.list=F, returns a character vector (arbitrary length) containing all sub-locations that fall within ANY of the given locations
#'
#'@export
get.sub.locations <- function(locations, sub.type,
                              limit.to.completely.enclosing,
                              return.list=F,
                              throw.error.if.unregistered.type=T)
{
    
}


#'@description Get Locations that Enclose a Location
#'
#'@param locations A character vector of location codes
#'@param super.type The type (geographic resolution) of locations requested for the super-locations
#'@param limit.to.completely.enclosing A single logical value indicating whether ONLY super-locations that COMPLETELY enclose the given locations should be returned
#'@param return.list A single logical value indicating whether the return value should be a list with one element for each location, or whether all super-locations should be 'unlisted' into a vector
#'@param throw.error.if.unregistered.type A single logical value indicating whether the function should throw an error if super.type has not been registered as a location type
#'
#'@return If return.list==T, a list with length(locations) and names=locations. Each element is itself a character vector with zero or more locations corresponding to super-locations. If return.list=F, returns a character vector (arbitrary length) containing all super-locations that contain ANY of the given locations
#'
#'@export
get.super.locations <- function(locations, super.type,
                                limit.to.completely.enclosing,
                                return.list=F,
                                throw.error.if.unregistered.type=T)
{
    
}

##-------------##
##-- Setters --##
##-------------##

#'@description Register information about locations
#'
#'@param type The geographic resolution at which to register the locations. Can be be either a single character value, or a vector of the same length as locations
#'@param locations A character vector of location codes
#'@param location.names A character vector of the same length as locations with corresponding names
#'@param location.aliases Can be either (1) NULL (in which case it is ignored), (2) A named list where each element is a character vector the same length as locations, where the names of the elements are the names of the aliases, and the values are the alias values for each location
#'@export
register.locations <- function(type,
                               locations,
                               location.names,
                               location.aliases)
{
    
}

#'@description Register sub-super relationships
#'
#'@param sub.locations A character vector of locations codes
#'@param super.locations A character vector of location codes of the same length as sub.locations, with corresponding super.locations
#'@param super.completely.encloses.sub Either a single logical value or a vector the same length as sub.locations and super.locations, indicating whether the location is completely enclosed
#'
#'@details Where super.completely.encloses.sub==T, the function will automatically recognize that locations completely enclosed within the given sub.locations are also completely enclosed within the corresponding super.locations, and, conversely that locations which completely enclose the given super.locations also completely enclose the corresponding sub.locations
#'
#'@export
register.sub.and.super.locations <- function(sub.locations,
                                             super.locations,
                                             super.completely.encloses.sub)
{
    
}


LOCATION.MANAGER = new.env()