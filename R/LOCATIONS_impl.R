library(R6)
library(purrr)

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
      # if it is false, we only return those that are partially contained.
      # contained (second value is true)
      if (contain.completely) {
        sapply(private$contains[as.logical(sapply(private$contains,"[[", 2))], "[[", 1)
      } else {
        sapply(private$contains[!as.logical(sapply(private$contains,"[[", 2))], "[[", 1)
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
LOCATION.MANAGER$types = list()

LOCATION.MANAGER$check.code.validity <- function(code) {
  #We allow only characters, numbers, periods or dashes
  grepl("^[A-Za-z0-9.-]*$", code)
}

LOCATION.MANAGER$get.names <- function(locations) {
  # return A character vector of location names, with length(locations) and names=locations. If location codes are not registered (or if they were NA), 
  # the corresponding returned name is NA
  
  #Resolve the location codes, preserving NAs and missing codes as NAs
  returned.names = unlist(lapply(locations,function(x){LOCATION.MANAGER$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
  
  #Set those non-na results to the appropriate name
  returned.names[!is.na(returned.names)] = unlist(lapply(returned.names[!is.na(returned.names)], function(x) { LOCATION.MANAGER$location.list[[x]]$return.name }))
  
  #We set the names to the original locations
  names(returned.names) = locations

  returned.names
}

LOCATION.MANAGER$get.by.alias <- function(aliases, types) {
  aliases = toupper(aliases)
  types = toupper(types)
  
  #Sizes are checked a level up; either they match or types has a length of 1.
  types = ifelse (length(types) == 1, rep(types,length(aliases)), types)
  rv = rep(NA,length(types))
  results = mapply(function(alias, type) {
    if (alias %in% names(LOCATION.MANAGER$alias.codes[[type]])) {
      return (LOCATION.MANAGER$alias.codes[[type]][alias])
    } else {
      return (NA)
    }
  }, aliases, types, SIMPLIFY=T)
  names(results) = aliases
  results
}

LOCATION.MANAGER$get.types <- function(locations) {
  #return A character vector of location types, with length(locations) and names=locations. If location codes are not registered 
  #(or if they were NA), the corresponding returned type is NA
  
  #Resolve the location codes, preserving NAs and missing codes as NAs
  returned.types = unlist(lapply(locations,function(x){LOCATION.MANAGER$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
  
  #Set those non-na results to the appropriate type
  returned.types[!is.na(returned.types)] = unlist(lapply(returned.types[!is.na(returned.types)], function(x) { LOCATION.MANAGER$location.list[[x]]$return.type }))
  
  #We set the names to the original locations
  names(returned.types) = locations
  
  returned.types
}

LOCATION.MANAGER$get.prefix <- function(location.types) {
  
  location.types <- toupper(location.types)
  #A character vector of prefixes, with length(location.types) and names=prefixes. 
  #If the types are not registered (or if they were NA), the corresponding returned type is NA
  
  rv = lapply(seq_along(location.types), function(index) {
    if (location.types[index] %in% names(LOCATION.MANAGER$types)) {
      return (LOCATION.MANAGER$types[[location.types[index]]][1])
    } else {
      return (NA)
    }
  })
  
  names(rv) = location.types
  rv
}

LOCATION.MANAGER$get.sub <- function(locations, sub.type, limit.to.completely.enclosing, return.list = F, throw.error.if.unregistered.type = T) {
  #return If return.list==T, a list with length(locations) and names=locations. Each element is itself a character vector 
  #with zero or more locations corresponding to sub-locations. If return.list=F, returns a character vector 
  #(arbitrary length) containing all sub-locations that fall within ANY of the given locations
  
  #Capitalize the type
  sub.type = toupper(sub.type)
  
  if (throw.error.if.unregistered.type) {
    #Check the type against the type list;
    if (!sub.type %in% names(LOCATION.MANAGER$types)) {
      stop(paste0("LOCATION.MANAGER$get.sub: Type ", sub.type," not registered, aborting"))
    }
  }
  
  #Resolve the location codes, preserving NAs and missing codes as NAs
  codes = unlist(lapply(locations,function(x){LOCATION.MANAGER$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
  
  #Here I need create sub locations with the following logic:
  #If A completely contains B, and B completely contains C, then A completely contains C
  #If A completely contains B, and B partially contains C, then we can say that A partially contains C (unless we have specified elsewhere that A completely contains C)
  #If A partially contains B, and B completely contains C, we cannot make any inferences about A and C
  #If A partially contains B, and B partially contains C, we cannot make any inferences about A and C
  
  #We need to do the fully enclosed search in both cases
  #Here we ask each location code for a list of the regions it contains completely.  Then we ask each of those location codes for a list of the regions it contains
  #completely, and so on until the list we had previously is equal to our current list.

  #Start with collecting all fully enclosed from each location
  location.contained.collector = function(location, fully.contained) {
    if (is.na(location)) {
      return(NA)
    }
    #TRUE in this case set the boolean contain.completely
    rv = LOCATION.MANAGER$location.list[[location]]$return.sub.locations(fully.contained)
    if (length(rv) == 0 || identical(rv,character(0))) {
      return(NA)
    }
    rv
  }
  
  all.sub.locations = lapply(codes,function(x) {location.contained.collector(x, TRUE)})

  #For each code in each vector, get their list of fully contained regions
  fully.contained.children = function(locations) {
    #locations is a vector of location codes
    unlist(lapply( locations, function(x) {location.contained.collector(x,TRUE)} ))
  }
  
  #TWO LOOPS. Yikes.
  repeat {
    next.sub.locations = lapply(all.sub.locations, fully.contained.children)
    try.again = FALSE

    #LOOP FIXME
    for (i in seq_along(all.sub.locations)) {
      #If the length of the union of the current sub locations and the new sub locations are of different sizes,
      #then the new sub locations has something to add to all.sub.locations
      
      if (length(union(all.sub.locations[[i]], next.sub.locations[[i]])) != length(all.sub.locations[[i]])) {
        diff = setdiff(next.sub.locations[[i]], all.sub.locations[[i]])
        if (length(na.omit(diff)) != 0) {  #There are NA's sneaking in here and I don't know how as of yet. FIXME
          all.sub.locations[[i]] = c(all.sub.locations[[i]], 
                                     na.omit(diff))
          #We still have locations returning 
          try.again= TRUE
        } 
      }
    }
    if (!try.again) {
      break;
    }
  }
  
  #print("Before adding")
  #print(all.sub.locations)

  if (!limit.to.completely.enclosing) {
    #We want fully and partially enclosed lists
    #We ask each location for a list of those places it partially includes, add them on to the list
    partially.contained.children = function(locations) {
      unlist(lapply(locations, function(x) {location.contained.collector(x,FALSE)}))
    }
    # Add the location itself to the locations to check for partially.contained.children
    all.sub.locations = mapply(function(x, code) c(code, x), all.sub.locations, codes, SIMPLIFY = FALSE)
    
    partially.contained = lapply(all.sub.locations, partially.contained.children)
    
    #Now we have to add these lists into the main lists and then unique the whole thing:
    
    #LOOP FIXME
    for (i in seq_along(all.sub.locations)) {
      all.sub.locations[[i]] = unique(c(all.sub.locations[[i]], na.omit(partially.contained[[i]])))
    }
  }
  
  #all.sub.locations is a list, each entry contains a list of things this location contains, from top to bottom
  #this includes locations of all types.  The types are later filtered with the mask.collector algorithm.
  #print(all.sub.locations)
  
  mask.collector = function (locations) {
    if (anyNA(locations)) { 
      return (NA) 
    } 
    unlist(lapply(locations, function(location) { LOCATION.MANAGER$location.list[[location]]$return.type == sub.type} ))
  }
  
  #for each list of contained locations, check to make sure they correspond to the correct type
  sub.types = lapply(all.sub.locations, mask.collector)

  count = length(all.sub.locations)
  
  #LOOP FIXME
  for (i in 1:count) {
    #apply the mask
    if (length(sub.types[[i]]) != 0) {
      all.sub.locations[[i]] = all.sub.locations[[i]][sub.types[[i]]]
      #I confess I don't quite understand why I need this here below, but I do
      if (identical(all.sub.locations[[i]],character(0))) {
        all.sub.locations[[i]] = NA
      }
    }
  }
  
  names(all.sub.locations) = locations
  
  #Now sub.locations is a proper list; if we want a list returned, return it now
  if (return.list) {
    return (all.sub.locations)
  }

  #Return a collapsed vector of valid entries for all locations
  rv = unname(unlist(lapply(all.sub.locations, function (l) {
    l[!is.na(l)]  
  })))
  
  if (length(rv) == 0) {
    return (character())
  }

  rv
}

LOCATION.MANAGER$get.super <- function(locations, super.type, limit.to.completely.enclosing, return.list = F, throw.error.if.unregistered.type = T) {
  super.type = toupper(super.type)
  
  #If required, check to see if the types are valid
  if (throw.error.if.unregistered.type) {
    if (!super.type %in% names(LOCATION.MANAGER$types)) {
      stop(paste0("LOCATION.MANAGER$get.super: Type ", super.type," not registered, aborting"))
    }
  }
  
  #Verify the codes are valid
  codes = unlist(lapply(locations, function(x) { LOCATION.MANAGER$resolve.code(x, F) }))
  
  #Collect all the sub-locations from a certain location
  location.sub.collector = function(location, completely.enclosed) {
    if (is.na(location)) {
      return(NA)
    }
    sub.locations = LOCATION.MANAGER$location.list[[location]]$return.sub.locations(completely.enclosed)
    if (length(sub.locations) == 0 || identical(sub.locations, character(0))) {
      return(NA)
    }
    sub.locations
  }
  
  #Collect all the sub locations for all the locations (!!)
  all_sub_locations = sapply(names(LOCATION.MANAGER$location.list), location.sub.collector, completely.enclosed = limit.to.completely.enclosing, simplify = FALSE)
  names(all_sub_locations) = names(LOCATION.MANAGER$location.list)
  
  # For each location code, check the code of each other registered location.  
  # If the other registered location is of the desired type, and the 'code' is 
  # in the list of sub locations of the other registered location, return it
  # This may not catch skips? TODO
  all_super_locations = lapply(codes, function(code) {
    super_locations = c()
    for (super in names(LOCATION.MANAGER$location.list)) {
      if (LOCATION.MANAGER$location.list[[super]]$return.type == super.type && code %in% all_sub_locations[[super]]) {
        super_locations = c(super_locations, super)
      }
    }
    super_locations
  })
  
  names(all_super_locations) = locations
  
  if (return.list) {
    return (all_super_locations)
  }
  
  rv = unname(unlist(lapply(all_super_locations, function (l) {
    l[!is.na(l)]  
  })))
  
  if (length(rv) == 0) {
    return (character())
  }
  
  rv
}

  
  

# LOCATION.MANAGER$get.super <- function(locations, super.type, limit.to.completely.enclosing, return.list, throw.error.if.unregistered.type) {
#   #return If return.list==T, a list with length(locations) and names=locations. 
#   #Each element is itself a character vector with zero or more locations corresponding to super-locations. 
#   #If return.list=F, returns a character vector (arbitrary length) containing all super-locations that contain ANY of the given locations
#   #Capitalize the type
#   super.type = toupper(super.type)
#   
#   if (throw.error.if.unregistered.type) {
#     #Check the type against the type list;
#     if (!super.type %in% LOCATION.MANAGER$type.list) {
#       stop(paste0("LOCATION.MANAGER$get.sub: Type ", super.type," not registered, aborting"))
#     }
#   }
#   
#   #Resolve the location codes, preserving NAs and missing codes as NAs
#   codes = unlist(lapply(locations,function(x){LOCATION.MANAGER$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
#   
#   #For each location (l), iterate over the locations in LOCATION.MANAGER$location.list to see if they contain l
#   #If they do contain l (either fully or limited, as requested), add them to the list for this location
#   
#   location.count = length(LOCATION.MANAGER$location.list)
#   location.codes = names(LOCATION.MANAGER$location.list)
# 
#   results = lapply(locations, function(location) {
#     #LOOP FIXME
#     loc.list= c()
#     print(paste0("Working on ", location))
#     for (i in seq_along(location.count)) {
#       print(location.codes[i])
#       sub.list = LOCATION.MANAGER$location.list[[location.codes[i]]]$return.sub.locations(limit.to.completely.enclosing)
#       print(sub.list)
#       if (location %in% sub.list) {
#         loc.list = c(location.codes[[i]],loc.list)
#       }
#     }
#     loc.list
#   })
#   print(results)
# }

LOCATION.MANAGER$get.name.aliases <- function(locations, alias.name, throw.error.if.unregistered.alias) {
  # return A character vector of aliases, with length(locations) and names=locations. If location codes are not registered 
  # (or if they were NA), or if no value for the alias.name has been registered to a location, the corresponding returned alias is NA
  
  #Resolve the location codes, preserving NAs and missing codes as NAs
  codes = unlist(lapply(locations,function(x){LOCATION.MANAGER$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
  
  type.collector = function(location) {
    if (is.na(location)) {
      return (NA)
    }
    LOCATION.MANAGER$location.list[[location]]$return.type
  }
  
  #We need to get the type for the location codes
  types = unlist(lapply(codes,type.collector))
  #print(LOCATION.MANAGER$alias.names[types])
  result = unlist(lapply(mapply(c,codes, types,SIMPLIFY=F), function(vars) {
    l = vars[[1]]
    t = vars[[2]]
    n = names(LOCATION.MANAGER$alias.names[[t]])
    #n is a list of all aliases of that type
    #LOOP FIXME
    for (x in seq_along(n)) {
      #For every n
      #If the alias.name matches and the location matches
      if (LOCATION.MANAGER$alias.names[[t]][[n[x]]][[2]] == alias.name 
          && l == LOCATION.MANAGER$alias.names[[t]][[n[x]]][[1]]) {
        return(n[x])
      }
    }
    #We didn't find a match for this alias.name, type and location combination
    if (throw.error.if.unregistered.alias) {
      stop(paste0("LOCATION.MANAGER$get.name.aliases: Unrecognized alias combination :", alias.name, ", ", l, ", ", t))
    } else {
      return(NA)
    }
  }))
  names(result) = locations
  
  result
}

LOCATION.MANAGER$resolve.code <- function(code,fail.on.unknown=T) {
  
  #Resolves a single location code from potential alias to actual code
  #If fail.on.unknown is FALSE, the result is NA
  
  #Capitalize
  code <- toupper(code)
  
  #could this function be vectorized?
  
  if (!code %in% names(LOCATION.MANAGER$location.list)) {
    #The code is not in the list, check the code alias list.
    if (fail.on.unknown) {
      stop(paste0("LOCATION.MANAGER: The location code used (",code,") cannot be recognized, stopping"))
    } else {
      code <- NA
    }
  }
  code
}

LOCATION.MANAGER$register.types <- function (type, prefix, prefix.longform) {
  #Sizes have been checked a step up
  type <- toupper(type)
  prefix <- toupper(prefix)
  
  invisible(Map(function(t, p, p.l) {
    #Check validity of the prefix
    if (!LOCATION.MANAGER$check.code.validity(p)) {
      stop("LOCATION.MANAGER$register.types: We are not allowed characters outside of the letters, numbers, . and -")
    }
    #Add the type into the types list as a list item type = c(prefix,prefix.longform)
    if (!t %in% names(LOCATION.MANAGER$types)) {
      #append(LOCATION.MANAGER$types, list(t = c(p, p.l)), 1)
      LOCATION.MANAGER$types[[t]] = c(p, p.l)
      #Add entry for the aliases to the code alias object
      LOCATION.MANAGER$alias.codes[[t]] = list()
    } else {
      # the type name already exists
      stop(paste0("LOCATION.MANAGER$register.types: Type ",t, " already exists in the system, aborting"))
    }
  }, type, prefix, prefix.longform))
  
}

LOCATION.MANAGER$register <- function (types, location.names, codes) {
  #codes and types are all uppercase; case insensitive
  codes <- toupper(codes)
  types <- toupper(types)
  
  #First we need to check if the type is registered
  if (!all(types %in% names(LOCATION.MANAGER$types))) {
    stop("LOCATION.MANAGER$register: Type not previously registered - ", 
         types[which(is.na(match(types,names(LOCATION.MANAGER$types))))[1]])
  }
  
  #Add Prefixes depending on type 
  #TODO Check if the prefix is already added to the location code
  codes = sprintf("%s%s", sapply(LOCATION.MANAGER$types[types], function(x) x[1]), codes)
  
  #Check that this code doesn't already exist
  if (any( codes %in% names(LOCATION.MANAGER$location.list) )) {
    stop("LOCATION.MANAGER: Attempting to add a code that already exists in the manager")
  }
  
  #Check that this code doesn't contain undesirable characters
  if (!all( LOCATION.MANAGER$check.code.validity(codes) )) {
    stop("LOCATION.MANAGER: Attempting to enter a code with invalid characters (only letters, numbers, . and - allowed")
  }

  #Check that the code doesn't conflict with a code alias either
  # if (any (codes %in% names(LOCATION.MANAGER$alias.codes) )) {
  #   stop("LOCATION.MANAGER: Attempting to add a code that conflicts with a code alias")
  # }
  
  LOCATION.MANAGER$location.list [ codes ]<- lapply(mapply(c,location.names,types,SIMPLIFY=F), Location$new)

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
  
  #Get the name of the type to be aliased
  location.type <- LOCATION.MANAGER$location.list[[code]]$return.type
  
  #Verify that none of the location code aliases are currently in use
  if ( any( code.aliases %in% names(LOCATION.MANAGER$alias.codes[[location.type]]))) {
    stop("LOCATION.MANAGER: One of the location code aliases are already registered")
  }
  
  #Assign the aliases
  LOCATION.MANAGER$alias.codes[[location.type]][[code.aliases]] = code
}

LOCATION.MANAGER$register.hierarchy <-function(sub, super, fully.contains, fail.on.unknown = T) {

  #Sizes have already been checked up one level
  #We now have three vectors of equal length

  if (fail.on.unknown) {
  #Check the location codes/aliases for both sub and super
    sub = unlist(lapply(sub,LOCATION.MANAGER$resolve.code))
    super = unlist(lapply(super,LOCATION.MANAGER$resolve.code))
    #LOOP FIXME
    #mapply and function { ?
    for (i in seq_along(sub)) {
      LOCATION.MANAGER$location.list[[super[i]]]$register.sub.location(sub[i],fully.contains[i])
    }
  } else {
    sub = unlist(lapply(sub, function(x) { LOCATION.MANAGER$resolve.code(x,F)} ))
    super = unlist(lapply(super,function(x) { LOCATION.MANAGER$resolve.code(x,F)} ))
    #LOOP FIXME
    #mapply and function { ?
    for (i in seq_along(sub)) {
      if (!any(is.na(c(super[i],sub[i])))) {
        #Verify that neither address is NA before we attempt to register the sub location
        #Skip those that are not registered yet.
        LOCATION.MANAGER$location.list[[super[i]]]$register.sub.location(sub[i],fully.contains[i])
      }
    }
  }
}
