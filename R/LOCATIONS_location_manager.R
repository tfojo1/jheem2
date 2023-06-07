
##-----------------------------##
##-----------------------------##
##-- PUBLIC-FACING INTERFACE --##
##-----------------------------##
##-----------------------------##

##-------------##
##-- Getters --##
##-------------##

#'@title get.location.types
#'
#'@description Get a list vector of registered location types, prefixes and longform prefix names.
#'
#'@return A list the same length as the registered number of types, with each list item being the name, the prefix and the longform prefix name.  If there are no registered types it will return an empty list
#'
#'@export
get.location.types <- function()
{
  LOCATION.MANAGER$get.registered.types()
}

#'@title get.location.name
#'
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
  # we could return NA value
  LOCATION.MANAGER$get.names(locations)
}

#'@title get.location.code
#'
#'@description Get the location code for a name and a type
#'
#'@param location.names A list of names to get the location code for
#'@param types A corresponding list of types
#'@param search.aliases A boolean flag indicating whether we want to check the aliases if the name check fails. Default = TRUE
#'
#'@return A list of location.names, with values corresponding to the identified location code.  Multiple results per location.names is possible. If if the location.name is not registered or NA, NA is returned
#'
#'@export
get.location.code <- function(location.names, types, search.aliases = TRUE)
{
  if (length(types) != 1 && length(types) != length(location.names)) {
    stop("get.location.code: Length of types can either be 1 or it must be the length of the location.names")
  }
  if (length(types) == 1) {
    types = rep(types, length(location.names))
  }
  LOCATION.MANAGER$get.codes.from.names(location.names, types, search.aliases)
}

#'@title get.location.name.alias
#'
#'@description Get an Name alias Associated with a Location
#'
#'@param locations A character vector of location codes
#'@param alias.name A single character value representing a previously registered name alias
#'@param throw.error.if.unregistered.alias A single logical value indicating whether the function should throw an error if no alias with name alias.name has been registered
#'
#'@return A character vector of aliases, with length(locations) and names=locations. If location codes are not registered (or if they were NA), or if no value for the alias.name has been registered to a location, the corresponding returned alias is NA
#'
#'@export
get.location.name.alias <- function(locations, alias.name,
                               throw.error.if.unregistered.alias=T)
{
  if (length(alias.name) != 1) {
    stop("get.location.name.alias: alias.name must be a single name")
  }
  LOCATION.MANAGER$get.name.aliases(locations, alias.name, throw.error.if.unregistered.alias)
}

#'@title get.location.type
#'
#'@description Get the Type (Geographic Resolution) of a Location
#'
#'@param locations A character vector of location codes
#'
#'@return A character vector of location types, with length(locations) and names=locations. If location codes are not registered (or if they were NA), the corresponding returned type is NA
#'
#'@export
get.location.type <- function(locations)
{
  #No need to check lengths
  LOCATION.MANAGER$get.types(locations)
}

#'@title get.prefix.for.type
#'
#'@description Get the prefix for a given type
#'
#'@param location.types A character vector of location types
#'
#'@return A character vector of prefixes, with length(location.types) and names=prefixes. If the types are not registered (or if they were NA), the corresponding returned type is NA
#'
#'@export
get.prefix.for.type <- function(location.types)
{
  #No need to check lengths
  LOCATION.MANAGER$get.prefix(location.types)
}

#'@title get.sub.locations
#'
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
   if (length(sub.type) != 1) {
     stop("get.sub.locations: sub.type must be a single character type")
   } 
   if (!is.logical(c(limit.to.completely.enclosing,return.list,throw.error.if.unregistered.type))
       || length(c(limit.to.completely.enclosing,return.list,throw.error.if.unregistered.type)) != 3) {
     stop("get.sub.locations: error in one of the logical types limit.to.completely.enclosing, return.list or throw.error.if.unregistered.type")
   }
   LOCATION.MANAGER$get.sub(locations, sub.type, limit.to.completely.enclosing, return.list, throw.error.if.unregistered.type)
}


#'@title get.super.locations
#'
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
  if (length(super.type) != 1) {
    stop("get.super.locations: sub.type must be a single character type")
  } 
  if (!is.logical(c(limit.to.completely.enclosing,return.list,throw.error.if.unregistered.type))
      || length(c(limit.to.completely.enclosing,return.list,throw.error.if.unregistered.type)) != 3) {
    stop("get.super.locations: error in one of the logical types limit.to.completely.enclosing, return.list or throw.error.if.unregistered.type")
  }
  LOCATION.MANAGER$get.super(locations, super.type, limit.to.completely.enclosing, return.list, throw.error.if.unregistered.type)
}

##--------------##
##-- Checkers --##
##--------------##

#'@title is.location.valid
#'
#'@description Check to see if the passed-in value matches any location code or alias
#'
#'@param location A character vector representing potential locations
#'@param suggest.options A boolean indicating whether to check the aliases for potential matches
#'
#'@return A vector of boolean values whether the passed-in value is a location codes.  If false, this function will display a list of possibilities.
#'
#'@export
is.location.valid <- function(locations, suggest.options = F)
{
  # How do we handle NAs?
  # we could return FALSE
  LOCATION.MANAGER$check.many.locations(locations, suggest.options)
}

##-------------##
##-- Setters --##
##-------------##

#'@title register.types
#'
#'@description Register location type, prefix, and prefix.longform
#'
#'@param type A character vector representing types to be added
#'@param prefix A character vector of unique prefixes for the location codes for types of this kind
#'@param prefix.longform A character vector of longform names for that particular unique prefixes
#'
#'@details The prefix is restricted to letters, numbers, period and '-'.
#'@export
register.types <- function(type,
                          prefix,
                          prefix.longform)
{
  if (length(type) != length(prefix) || length(prefix) != length(prefix.longform)) {
    stop("register.types: Lengths of the 3 parameters must be equal")
  }
  if (any(c(typeof(type),typeof(prefix),typeof(prefix.longform)) != "character")) {
    stop("register.types: All parameters must be characters/strings")
  }
  LOCATION.MANAGER$register.types(type, prefix, prefix.longform)
}

#'@title register.locations
#'
#'@description Register information about locations.  
#'
#'@param type The geographic resolution at which to register the locations. Can be be either a single character value, or a vector of the same length as locations
#'@param locations A character vector of location codes 
#'@param location.names A character vector of the same length as locations with corresponding names
#'
#'@details There is no error checking here; we assume that multiple locations with the same name are possible at different resolutions/types.
#'@export
register.locations <- function(type,
                               locations,
                               location.names)
{
  if (length(type) != 1 && length(type) != length(location.names)) {
    stop("register.locations: Length can either be 1 or it must be the length of the locations")
  }
  if (length(locations) != length(location.names)) {
    stop("register.locations: The length of the codes and the names must be equal")
  }
  
  # Repeat the type as many times as needed
  if (length(type) == 1) {
    type = rep(type, length(locations))
  }
  
  LOCATION.MANAGER$register(type, location.names, locations)
}

#'@title register.name.aliases
#'
#'@description Register name aliases for specific location  
#'
#'@param location A single, previously registered location code or a registered location code alias.
#'@param location.aliases A character vector of aliases for this location name
#'@param location.alias.names A character vector names for the particular alias 'short','no spaces', 'full', etc
#'
#'@details There is no error checking here; we assume that multiple locations with the same name are possible at different resolutions/types.
#'@export
register.name.aliases <- function(location = NA,
                             location.aliases = NA,
                             location.aliases.names = NA)
{
  if (anyNA(c(location,location.aliases))) {
    stop("register.name.aliases: NA values not allowed for location or location.aliases")
  }
  if (length(location.aliases) != length(location.aliases.names)) {
    stop("register.name.aliases: You must provide a name for each alias")
  }
  if (length(location) > 1) {
    stop("register.name.aliases: You can only provide one location code at a time")
  }
  
  LOCATION.MANAGER$register.name.aliases(location, location.aliases, location.aliases.names)
}

#'@title register.code.aliases
#'
#'@description Register location code aliases for specific location  
#'
#'@param location A single, previously registered location code or a registered location code alias.
#'@param location.aliases A character vector of location code aliases for this location name.  Code aliases are unique by type
#'
#'@export
register.code.aliases <- function(location = NA,
                                  location.aliases = NA)
{
  if (anyNA(c(location,location.aliases))) {
    stop("register.code.aliases: NA values not allowed for location or location.aliases")
  }
  if (length(location) > 1) {
    stop("register.code.aliases: You can only provide one location code at a time")
  }
  
  LOCATION.MANAGER$register.code.aliases(location, location.aliases)
}

#'@title register.sub.and.super.locations
#'
#'@description Register hierarchical sub-super relationships
#'
#'@param sub.locations A character vector of locations codes/location code aliases
#'@param super.locations A character vector of location codes/location code aliases of the same length as sub.locations, with corresponding super.locations
#'@param super.completely.encloses.sub Either a single logical value or a vector the same length as sub.locations and super.locations, indicating whether the location is completely enclosed
#'
#'@details Where super.completely.encloses.sub==T, the function will automatically recognize that locations completely enclosed within the given sub.locations are also completely enclosed within the corresponding super.locations, and, conversely that locations which completely enclose the given super.locations also completely enclose the corresponding sub.locations
#'
#'@export
register.sub.and.super.locations <- function(sub.locations,
                                             super.locations,
                                             super.completely.encloses.sub)
{
  if (length(sub.locations) != length(super.locations)) {
    stop("register.sub.and.super.locations: We must have the same number of sub locations and super locations")
  }
  if (length(super.completely.encloses.sub) > 1 && length(super.completely.encloses.sub) != length(sub.locations)) {
    stop("register.sub.and.super.locations: The length of super.completely.encloses.sub must be either 1 or the same length as sub.locations")
  }
  if (length(super.completely.encloses.sub) == 1) {
    super.completely.encloses.sub = rep(super.completely.encloses.sub,length(sub.locations))
  }
  LOCATION.MANAGER$register.hierarchy(sub.locations, super.locations, super.completely.encloses.sub) 
}