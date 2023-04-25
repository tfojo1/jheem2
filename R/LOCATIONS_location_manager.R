
##-----------------------------##
##-----------------------------##
##-- PUBLIC-FACING INTERFACE --##
##-----------------------------##
##-----------------------------##

source ("R/LOCATIONS_impl.R")

##-------------##
##-- Getters --##
##-------------##

#'@description Get the Name of a Location
#'
#'@param locations A character vector of location codes or location code aliases
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

#'@description Get the location code for a name and a type
#'
#'@param location.names A list of names to get the location code for
#'@param types A corresponding list of types
#'
#'@return A character vector of location codes, with length(location.names) and names=location.names. If if the location.name is not registered or NA, NA is returned
#'
#'@export
get.location.code <- function(location.names, types)
{
  if (length(types) != 1 && length(types) != length(location.names)) {
    stop("get.location.code: Length of types can either be 1 or it must be the length of the location.names")
  }
  if (length(types) == 1) {
    types = rep(types, length(location.names))
  }
  LOCATION.MANAGER$get.codes.from.names(location.names, types)
}

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
    stop("get.sub.locations: sub.type must be a single character type")
  } 
  if (!is.logical(c(limit.to.completely.enclosing,return.list,throw.error.if.unregistered.type))
      || length(c(limit.to.completely.enclosing,return.list,throw.error.if.unregistered.type)) != 3) {
    stop("get.sub.locations: error in one of the logical types limit.to.completely.enclosing, return.list or throw.error.if.unregistered.type")
  }
  LOCATION.MANAGER$get.super(locations, super.type, limit.to.completely.enclosing, return.list, throw.error.if.unregistered.type)
}

##-------------##
##-- Setters --##
##-------------##

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

#'@description Register location code aliases for specific location  
#'
#'@param location A single, previously registered location code or a registered location code alias.
#'@param location.aliases A character vector of location code aliases for this location name
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


#'@description Register sub-super relationships
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

#'@description Register a prefix to use for the codes that represent fips values.
#'
#'@param prefix A single character word containing the prefix.  Will be forced uppercase
#'
#'@details This can contain any string but will error if set twice.
#'
#'@export
register.fips.prefix <- function(prefix)
{
  if (!is.character(prefix) || length(prefix) != 1) {
    stop("register.fips.prefix: prefix must be a single string value")
  }
  LOCATION.MANAGER$register.fips.prefix(prefix) 
}

#'@description Register a prefix to use for the codes that represent zipcode values.
#'
#'@param prefix A single character word containing the prefix.  Will be forced uppercase
#'
#'@details This can contain any string but will error if set twice.
#'
#'@export
register.zip.prefix <- function(prefix)
{
  if (!is.character(prefix) || length(prefix) != 1) {
    stop("register.zip.prefix: prefix must be a single string value")
  }
  LOCATION.MANAGER$register.zip.prefix(prefix) 
}

#'@description Register a prefix to use for the codes that represent cbsa values.
#'
#'@param prefix A single character word containing the prefix.  Will be forced uppercase
#'
#'@details This can contain any string but will error if set twice.
#'
#'@export
register.cbsa.prefix <- function(prefix)
{
  if (!is.character(prefix) || length(prefix) != 1) {
    stop("register.cbsa.prefix: prefix must be a single string value")
  }
  LOCATION.MANAGER$register.cbsa.prefix(prefix) 
}

#
# There is an argument to be made here for the removal of the following 
# functions from the API; leave them as LOCATION.MANAGER functions and 
# re-write/add as necessary.
# 
# Leave them in for now.
#

#'@description Register state abbreviations with the location manager as the primary location codes
#'
#'@param filename The name of the file we are trying to read.
#'
#'@details LOCATION.MANAGER will check the existence of the file.
#'
#'@export
register.state.abbrev.file <- function(filename)
{
  LOCATION.MANAGER$register.state.abbrev(filename) 
}

#'@description Register fips state codes as location code aliases to the state abbreviation code
#'
#'@param filename The name of the file we are trying to read.
#'
#'@details LOCATION.MANAGER will check the existence of the file.
#'
#'@export
register.state.fips.code.aliases <- function(filename)
{
  LOCATION.MANAGER$register.state.fips.aliases(filename)
}


#'@description Register fips county file with the location manager
#'
#'@param filename The name of the file we are trying to read.
#'
#'@details LOCATION.MANAGER will check the existence of the file.
#'
#'@export
register.fips.file <- function(filename)
{
  LOCATION.MANAGER$register.fips(filename) 
}

#'@description Register zip code file with the location manager
#'
#'@param filename The name of the file we are trying to read.
#'
#'@details LOCATION.MANAGER will check the existence of the file.
#'
#'@export
register.zipcode.file <- function(filename)
{
  LOCATION.MANAGER$register.zipcodes(filename) 
}

#'@description Register cbsa (Community Based Statistical Area) code file with the location manager
#'
#'@param filename The name of the file we are trying to read.
#'
#'@details LOCATION.MANAGER will check the existence of the file.
#'
#'@export
register.cbsa.file <- function(filename)
{
  LOCATION.MANAGER$register.cbsa(filename) 
}

