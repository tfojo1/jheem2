library(R6)

Location <- R6Class("Location",
  public = list(

    initialize = function(args) {
      private$name <- args[1]
      private$type <- args[2]
      private$contains <- list()
    },
    register.sub.location = function (sub.code, enclose.completely) {
      # Register one sub location at a time
      # we will already have the vector size due to error checking higher up
      private$contains <- append (private$contains, list(c(sub.code,enclose.completely)))
    },
    return.sub.locations = function( contain.completely ) {
      # returns a character vector of contained location codes
      # if contain.completely is true, we only return those entries are completely 
      # contained (second value is true)
      if (completely.contained) {
        sapply(private$contains, "[[", 1)
      } else {
        sapply(private$contains[as.logical(sapply(private$contains,"[[", 2))], "[[", 1)
      }
    }
  ),
  active = list(
    return.type = function() {
      private$type
    },
    contains.list = function() {
      private$contains
    },
    return.name = function() {
      private$name
    }
  ),
  private = list(  type = NULL, # vector of characters
                   name = NULL, # string
                   contains = NULL # list of vector pairs c("token","BOOL"),
                   # where BOOL is a textual repr. of boolean
                   # values, where the value is TRUE if the
                   # current location completely encases
                   # the contained region.  The token field
                   # is the token of the contained region
                 )
)
#Currently an environment but will probably convert to R6 for encapsulation
LOCATION.MANAGER = new.env()

LOCATION.MANAGER$location.list = list()
LOCATION.MANAGER$alias.names = list()
LOCATION.MANAGER$alias.codes = list()

LOCATION.MANAGER$get.names <- function(locations) {
  # return A character vector of location names, with length(locations) and names=locations. If location codes are not registered (or if they were NA), 
  # the corresponding returned name is NA
  
  #Resolve the location codes, preserving NAs and missing codes as NAs
  returned.names = unlist(lapply(locations,function(x){LOCATION.MANAGER$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
  
  #Set those non-na results to the appropriate name
  returned.names[!is.na(returned.names)] = unlist(lapply(returned.names[!is.na(returned.names)], function(x) { LOCATION.MANAGER$location.list[[x]]$return.name }))
  
  #We preserve locations for the names, but we swap new.locations with the names
  names(returned.names) = locations

  returned.names
}

LOCATION.MANAGER$get.name.aliases <- function(locations, alias.name, throw.error.if.unregistered.alias) {
  # return A character vector of aliases, with length(locations) and names=locations. If location codes are not registered 
  # (or if they were NA), or if no value for the alias.name has been registered to a location, the corresponding returned alias is NA
  
  #Resolve the location codes, preserving NAs and missing codes as NAs
  rv = unlist(lapply(locations,function(x){LOCATION.MANAGER$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
  
  #WORKING
  #rv[!is.na(rv)] = 
  
}

LOCATION.MANAGER$resolve.code <- function(code,fail.on.unknown=T) {
  
  #Resolves a single location code from potential alias to actual code
  #If fail.on.unknown is FALSE, the result is NA
  
  #Capitalize
  code <- toupper(code)
  
  #could this function be vectorized?
  
  if (!code %in% names(LOCATION.MANAGER$location.list)) {
    #The code is not in the list, check the code alias list.
    if ( code %in% names(LOCATION.MANAGER$alias.codes) ) {
      #the location code is an alias for another code, use it:
      code <- LOCATION.MANAGER$alias.codes[[ code ]]
    } else {
      if (fail.on.unknown) {
        stop(paste0("LOCATION.MANAGER: The location code used (",code,") cannot be recognized, stopping"))
      } else {
        code <- NA
      }
    }
  }
  code
}

LOCATION.MANAGER$register <- function (types, names, codes) {
  #codes and types are all uppercase; case insensitive
  codes <- toupper(codes)
  types <- toupper(types)
  
  #Check that this code doesn't already exist
  if (any( codes %in% names(LOCATION.MANAGER$location.list) )) {
    stop("LOCATION.MANAGER: Attempting to add a code that already exists in the manager")
  }

  #Check that the code doesn't conflict with a code alias either
  if (any (codes %in% names(LOCATION.MANAGER$alias.codes) )) {
    stop("LOCATION.MANAGER: Attempting to add a code that conflicts with a code alias")
  }

  LOCATION.MANAGER$location.list [ codes ]<- lapply(mapply(c,names,types,SIMPLIFY=F), Location$new)

}

LOCATION.MANAGER$register.name.aliases <- function(code, name.aliases, names.alias.labels) {

  #Sizes have already been checked up one level
  code <- LOCATION.MANAGER$resolve.code(code)  
  
  #Get the type of the location
  location.type = LOCATION.MANAGER$location.list[[code]]$return.type
  
  # If there isn't an existing list, create it
  if (!location.type %in% names(LOCATION.MANAGER$alias.names)) {
    LOCATION.MANAGER$alias.names[[location.type]] = list()
  } else {
    # If the list already exists, if any of the to-assign name aliases are already included in the type, abort
    if ( any (name.aliases %in% names(LOCATION.MANAGER$alias.names[[location.type]]))) {
      stop(paste0("LOCATION.MANAGER: Attempting to add an alias for type ", location.type, " that already exists."))
    }
  }
  LOCATION.MANAGER$alias.names[[location.type]][name.aliases] = mapply(c,rep(code,length(names.alias.labels)),names.alias.labels,SIMPLIFY=F)
}

LOCATION.MANAGER$register.code.aliases <- function(code, code.aliases) {
  
  #Sizes have already been checked up one level
  code <- LOCATION.MANAGER$resolve.code(code)
  code.aliases <- toupper(code.aliases)
  
  #Verify that none of the location code aliases are currently in use
  if ( any( code.aliases %in% names(LOCATION.MANAGER$alias.codes))) {
    stop("LOCATION.MANAGER: One of the location code aliases are already registered")
  }
  
  #Assign the aliases
  LOCATION.MANAGER$alias.codes[code.aliases] = code
}

LOCATION.MANAGER$register.hierarchy <-function(sub, super, fully.contains) {
  
  #Sizes have already been checked up one level
  #We now have three vectors of equal length
  
  #Check the location codes/aliases for both sub and super
  sub = unlist(lapply(sub,LOCATION.MANAGER$resolve.code))
  super = unlist(lapply(super,LOCATION.MANAGER$resolve.code))
  
  
  #Loop here; not ideal!
  #mapply and function { ?
  for (i in seq_along(sub)) {
    LOCATION.MANAGER$location.list[[super[i]]]$register.sub.location(sub[i],fully.contains[i])
  }

}
