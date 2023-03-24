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
LOCATION.MANAGER$type.list = c()

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

LOCATION.MANAGER$get.sub <- function(locations, sub.type, limit.to.completely.enclosing, return.list, throw.error.if.unregistered.type) {
  #return If return.list==T, a list with length(locations) and names=locations. Each element is itself a character vector 
  #with zero or more locations corresponding to sub-locations. If return.list=F, returns a character vector 
  #(arbitrary length) containing all sub-locations that fall within ANY of the given locations
  
  #Capitalize the type
  sub.type = toupper(sub.type)
  
  if (throw.error.if.unregistered.type) {
    #Check the type against the type list;
    if (!sub.type %in% LOCATION.MANAGER$type.list) {
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

LOCATION.MANAGER$get.super <- function(locations, super.type, limit.to.completely.enclosing, return.list, throw.error.if.unregistered.type) {
  #return If return.list==T, a list with length(locations) and names=locations. 
  #Each element is itself a character vector with zero or more locations corresponding to super-locations. 
  #If return.list=F, returns a character vector (arbitrary length) containing all super-locations that contain ANY of the given locations
  #Capitalize the type
  super.type = toupper(super.type)
  
  if (throw.error.if.unregistered.type) {
    #Check the type against the type list;
    if (!super.type %in% LOCATION.MANAGER$type.list) {
      stop(paste0("LOCATION.MANAGER$get.sub: Type ", super.type," not registered, aborting"))
    }
  }
  
  #Resolve the location codes, preserving NAs and missing codes as NAs
  codes = unlist(lapply(locations,function(x){LOCATION.MANAGER$resolve.code(x,F)})) #Now contains the fully resolved location codes or NAs
  
  #For each location (l), iterate over the locations in LOCATION.MANAGER$location.list to see if they contain l
  #If they do contain l (either fully or limited, as requested), add them to the list for this location
  
  location.count = length(LOCATION.MANAGER$location.list)
  location.codes = names(LOCATION.MANAGER$location.list)

  results = lapply(locations, function(location) {
    #LOOP FIXME
    loc.list= c()
    print(paste0("Working on ", location))
    for (i in seq_along(location.count)) {
      print(location.codes[i])
      sub.list = LOCATION.MANAGER$location.list[[location.codes[i]]]$return.sub.locations(limit.to.completely.enclosing)
      print(sub.list)
      if (location %in% sub.list) {
        loc.list = c(location.codes[[i]],loc.list)
      }
    }
    loc.list
  })
  print(results)
}

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

LOCATION.MANAGER$get.codes.from.names <- function(location.names, types) {
  #WIP
  c()
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

LOCATION.MANAGER$register <- function (types, location.names, codes) {
  #codes and types are all uppercase; case insensitive
  codes <- toupper(codes)
  types <- toupper(types)
  
  #Check that this code doesn't already exist
  if (any( codes %in% names(LOCATION.MANAGER$location.list) )) {
    stop("LOCATION.MANAGER: Attempting to add a code that already exists in the manager")
  }

  #print(location.names)
  #Check that the code doesn't conflict with a code alias either
  if (any (codes %in% names(LOCATION.MANAGER$alias.codes) )) {
    stop("LOCATION.MANAGER: Attempting to add a code that conflicts with a code alias")
  }
  
  #Some types are missing from the type list, add them
  if (!all(types %in% LOCATION.MANAGER$type.list)) {
    LOCATION.MANAGER$type.list = unique(c(types, LOCATION.MANAGER$type.list))
  }

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
  
  #Verify that none of the location code aliases are currently in use
  if ( any( code.aliases %in% names(LOCATION.MANAGER$alias.codes))) {
    stop("LOCATION.MANAGER: One of the location code aliases are already registered")
  }
  
  #Assign the aliases
  LOCATION.MANAGER$alias.codes[code.aliases] = code
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

LOCATION.MANAGER$register.fips <- function(filename) {
  #Check if the file exists
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the fips file with filename ", filename))
  }
  
  fips.data = read.csv(file = filename)
  
  #States
  states = fips.data[ fips.data[1] == 040, ] #Get only the state data from the fips info
  
  #Column 2 is the state code
  state.codes = states[[2]] * 1000
  
  types = rep("state", length(state.codes))
  
  #Column 7 is the name of the states
  LOCATION.MANAGER$register(types, states[[7]], as.character(state.codes))
  
  #Counties
  counties = fips.data[ fips.data[1] == 050, ] #Get only the county data from the fips info.
  
  #Column 3 is the county code
  county.codes = counties[[2]] * 1000 + counties[[3]]
  
  types = rep("county",length(county.codes))
  
  #Column 7 is the names of the counties
  LOCATION.MANAGER$register(types, counties[[7]], as.character(county.codes))
  
  #There appear to be entries in the county code that don't have a corresponding
  #registered state.  Refrain from trying to create a connect to the non-existent
  #state
  #This list is checked against the state.codes above to make sure the state
  #is registered before we create a hierarchy.
  possible.state.codes = counties[[2]] * 1000
  #Get only the counties with proper states
  counties.of.states = as.character(county.codes [ possible.state.codes %in% state.codes ])
  corresponding.states = as.character(possible.state.codes [ possible.state.codes %in% state.codes ])
  
  #Register the counties as completely contained by the states
  #LOOP FIXME
  #for (i in seq_along(counties.of.states)) {
  #  LOCATION.MANAGER$register.hierarchy(counties.of.states[[i]], corresponding.states[[i]], TRUE)
  #}
  LOCATION.MANAGER$register.hierarchy(counties.of.states, corresponding.states, rep(TRUE,length(counties.of.states)))
  
}

LOCATION.MANAGER$register.zipcodes = function(filename, zipcode.code.format.string = "ZIP%s", #Format for unique zip id (will otherwise conflict with fips)
                                                        zipcode.name.format.string = "ZIP.%s", #Format for Zip name (unique not required)
                                                        zipcode.type.name = "ZIPCODE") { #Name of the type for the zipcodes
  #Check if the file exists
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the zipcode file with filename ", filename))
  } 
  
  zip.data = read.csv(file= filename)
  
  zip.codes = zip.data[['zip']]
  unique.zip.codes = sprintf(zipcode.code.format.string,zip.codes)
  fips.codes = zip.data[['fips']]
  #round(34233,digits=-3) = 34000
  state.codes = as.character(round(as.numeric(fips.codes),digits = -3))
  zip.names = sprintf(zipcode.name.format.string,zip.codes)
  
  #print(length(unique.zip.codes) == length(state.codes))
  
  #Register all the zip codes
  LOCATION.MANAGER$register(rep(zipcode.type.name, length(zip.codes)), zip.names, unique.zip.codes)
  
  #Register the zip code as completely contained by the fips code. If any result is NA, skip
  LOCATION.MANAGER$register.hierarchy(unique.zip.codes,fips.codes,rep(TRUE,length(fips.codes)),F)
  
  #Register the zip code as completely contained by the state.  If any result is NA, skip
  LOCATION.MANAGER$register.hierarchy(unique.zip.codes,state.codes,rep(TRUE,length(state.codes)),F)
  
}

LOCATION.MANAGER$register.state.abbrev = function(filename) {
  #Check if the file exists
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the zipcode file with filename ", filename))
  } 
  
  abbrev.data = read.csv(file= filename, stringsAsFactors = TRUE)
}

