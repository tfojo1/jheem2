#File to load and cache the location data so it doesn't have to be parsed each
#time the model is executed.

#We are currently registering 4 types:

# State; "STATE",  No Prefix
# County: "COUNTY", 'FIPS.' prefix (location code is 2 digit state + 3 digit local fips code)
# CBSA: "CBSA", 'CBSA.' prefix (location code is 5 digit cbsa code)
# zipcode : "ZIPCODE", 'ZIP.' prefix (location code is zipcode)

source("R/LOCATIONS_location_manager.R")


register.state.abbrev = function(LM, filename) {
  #Check if the file exists
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the zipcode file with filename ", filename))
  } 
  
  abbrev.data = read.csv(file= filename, header=FALSE)
  
  types = rep("state", nrow(abbrev.data))
  
  LM$register(types, abbrev.data[[1]], abbrev.data[[2]])
  
  #We need to do this first to register the states with their abbreviations as their 
  #location codes
  LM
}

register.state.fips.aliases <- function(LM, filename, fips.typename = "county") {
  
  fips.typename <- toupper(fips.typename)
  
  #Check if the file exists
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the fips state alias file with filename ", filename))
  }
  fips.state.alias.data = read.csv(file=filename,header=FALSE)
  
  #Column one is state name, mostly for debug purposes; column 2 is the fips code (0padded, 2 chars)
  #Column 3 is the state abbreviation/location code

  # No need to add the prefix as the code alias is based on the type of location
  fips.codes = sprintf("%02d",as.numeric(fips.state.alias.data[[2]]))
  
  #LOOP FIXME
  for ( i in 1:nrow(fips.state.alias.data) ) {
    LM$register.code.aliases (fips.state.alias.data[[3]][i], fips.codes[i])  
  }
  LM
}

register.fips <- function(LM, filename, fips.typename = "county") {
  
  fips.typename <- toupper(fips.typename)
  #Check if the file exists
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the fips file with filename ", filename))
  }
  
  fips.data = read.csv(file = filename)
  
  #States
  states = fips.data[ fips.data[1] == 040, ] #Get only the state data from the fips info
  
  #Column 2 is the state code
  state.codes = states[[2]]
  
  #Counties
  counties = fips.data[ fips.data[1] == 050, ] #Get only the county data from the fips info.
  
  #Column 3 is the county code
  county.codes = counties[[2]] * 1000 + counties[[3]]
  
  types = rep(fips.typename,length(county.codes))
  
  #Convert the county.codes to 0 padded 5 char
  county.codes = sprintf("%05d", county.codes)
  
  #Column 7 is the names of the counties
  LM$register(types, counties[[7]], county.codes)
  #Now register the county codes as aliases:
  #browser()
  walk(county.codes, function(code) {
    LM$register.code.aliases( 
      paste0(LM$get.prefix(fips.typename),code), #These will be the location codes
      code)
  })
  
  #There appear to be entries in the county code that don't have a corresponding
  #registered state.  Refrain from trying to create a connect to the non-existent
  #state
  #This list is checked against the state.codes above to make sure the state
  #is registered before we create a hierarchy.
  possible.state.codes = sprintf("%02d",counties[[2]])
  #Get only the counties with proper states
  counties.of.states = county.codes [ possible.state.codes %in% state.codes ]
  corresponding.states = possible.state.codes [ possible.state.codes %in% state.codes ] 
  
  counties.of.states.with.fips.prefix = sprintf("%s%s",LM$get.prefix(fips.typename), counties.of.states)
  # corresponding.states.with.fips.prefix = sprintf("%s%s",LM$get.prefix(fips.typename), corresponding.states)
  #Where previously I could use the code alias here, I can instead use the fips state number and the type
  corresponding.states.location.code = LM$get.by.alias (corresponding.states, "STATE")
  
  #Register the counties as completely contained by the states
  LM$register.hierarchy(counties.of.states.with.fips.prefix, corresponding.states.location.code, rep(TRUE,length(counties.of.states)))
  LM
}

register.zipcodes = function(LM, filename, fips.typename = "county", zip.typename = "zipcode", 
                                           zipcode.name.format.string = "ZIP_N_%s") { #Format for Zip name (unique not required)
  
  zip.typename <- toupper(zip.typename)
  #Check if the file exists
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the zipcode file with filename ", filename))
  } 
  
  zip.data = read.csv(file= filename)
  
  zip.codes = zip.data[['zip']]
  #Add proper prefix for register.hierarchy
  unique.zip.codes = sprintf("%s%s",LM$get.prefix(zip.typename),zip.codes)
  fips.codes = zip.data[['fips']]
  #round(34233,digits=-3) = 34000
  state.codes = as.character(round(as.numeric(fips.codes),digits = -3))
  zip.names = sprintf(zipcode.name.format.string,zip.codes)
  
  #Register all the zip codes
  #No prefix
  LM$register(rep(zip.typename, length(zip.codes)), zip.names, zip.codes)
  #Now register the raw zipcodes as aliases:
  walk(seq_along(zip.codes), function(i) {
    LM$register.code.aliases(unique.zip.codes[i], zip.codes[i])
  })
  
  #Register the zip code as completely contained by the fips code. If any result is NA, skip
  #With Prefix
  LM$register.hierarchy(unique.zip.codes,paste0(LM$get.prefix(fips.typename),fips.codes),
                        rep(TRUE,length(fips.codes)),F)
  
  #Register the zip code as completely contained by the state.  If any result is NA, skip
  LM$register.hierarchy(unique.zip.codes,paste0(LM$get.by.alias(state.codes, "state"), 
                                                state.codes),
                        rep(TRUE,length(state.codes)),F)
  LM
}

register.cbsa = function(LM, filename, cbsa.typename = "cbsa", fips.typename = "county") {
  
  cbsa.typename <- toupper(cbsa.typename)
  fips.typename <- toupper(fips.typename)
  
  if (!file.exists(filename)) {
    stop(paste0("LOCATION.MANAGER: Cannot find the cbsa file with filename ", filename))
  } 
  cbsa.data = read.csv(file = filename)
  #
  # Type will be CBSA
  #
  # Important Columns : 
  #   'CBSA Code', column 1 - Should be primary key
  #   'CBSA Title', column 4 - Should be name
  #   'FIPS State Code', column 10 - State fips code
  #   'FIPS County Code', column 11 - County fips code
  #
  # So we want to add all the cbsa's, then say that they are contained by
  # the state (entirely in the case where there is only one FIPS State Code for
  # all the entries for the particular CBSA code, partially for the rest) and
  # that they contain their FIPS County Code entirely.
  #
  # Get all the unique CBSA Codes with proper prefixes:
  unique.codes = head(unique(cbsa.data[[1]]), -4) #Remove the last four lines as they contain sources.
  location.codes = sprintf("%s%s",LM$get.prefix(cbsa.typename),unique.codes) 
  #LOOP FIXME
  for (i in seq_along(unique.codes)) {
    # Get cbsa code data:
    code.data = cbsa.data[ cbsa.data$CBSA.Code == unique.codes[i], ]
    
    # register the cbsa:
    # There will always be minimum one entry in the CBSA.Title column
    # Register the unique codes here as the prefixes are added in $register()
    LM$register(cbsa.typename, code.data$CBSA.Title[1], unique.codes[i])
    #Now register the raw cbsa codes as aliases:
    LM$register.code.aliases( location.codes[i], unique.codes[i] )
    
    # Is it fully or only partially contained by the state?
    unique.states <- sprintf("%02d",unique(code.data$FIPS.State.Code))
    #Use the fully prefixed for the hierarchy registration
    if (length(unique.states) == 1) {
      #The majority of cases:
      # Register the location.code value as being a sub location of the prefixed state FIPS code, marking as fully.contains.
      # There will always be at least one value in unique.states
      LM$register.hierarchy ( location.codes[i], LM$get.by.alias(unique.states[1],"STATE"), TRUE)
    } else {
      #Register as being sub of each state in the list
      #LOOP FIXME
      for ( j in seq_along(unique.states) ) {
        #Do the same but mark fully.contains as FALSE
        LM$register.hierarchy ( location.codes[i], LM$get.by.alias(unique.states[j],"STATE"), FALSE)
      }
    }
    # It contains the FIPS.County.Code entirely
    fips.county.data = sprintf("%s%02d%03d",LM$get.prefix(fips.typename),
                                            code.data$FIPS.State.Code, 
                                            code.data$FIPS.County.Code)
    walk(fips.county.data, function(county.code) {
      #We are marking the counties as fully contained by the cbsas
      LM$register.hierarchy ( county.code, location.codes[i], TRUE) 
    })
  }
  
  LM
}
#Prefix and type are auto capitalized

state.type = "state"
state.prefix = ""
state.prefix.longform = "State"

county.type = "county"
county.prefix = "f."
county.prefix.longform = "FIPS code"

cbsa.type = "cbsa"
cbsa.prefix = "c."
cbsa.prefix.longform = "Community Based Statistical Area"

zipcode.type = "zipcode"
zipcode.prefix = "z."
zipcode.prefix.longform = "Zipcode"

register.types(c(county.type,            zipcode.type,            cbsa.type,            state.type), #Typename
               c(county.prefix,          zipcode.prefix,          cbsa.prefix,          state.prefix), #Prefix
               c(county.prefix.longform, zipcode.prefix.longform, cbsa.prefix.longform, state.prefix.longform)) #Longform Name

LOCATION.MANAGER = register.state.abbrev(LOCATION.MANAGER, "locations/us_state_abbreviations.csv")
LOCATION.MANAGER = register.state.fips.aliases(LOCATION.MANAGER, "locations/fips_state_aliases.csv", fips.typename= county.type) #Set the fips typename
LOCATION.MANAGER = register.fips(LOCATION.MANAGER, "locations/fips_codes.csv", fips.typename = county.type) #Set the fips typename
LOCATION.MANAGER = register.cbsa(LOCATION.MANAGER, "locations/cbsas.csv", cbsa.typename = cbsa.type, fips.typename = county.type) #Sets the fips and cbsa typename
LOCATION.MANAGER = register.zipcodes(LOCATION.MANAGER, "locations/zip_codes.csv", fips.typename = county.type, zip.typename = zipcode.type)

register.code.aliases("C.49700","DCBABY")
