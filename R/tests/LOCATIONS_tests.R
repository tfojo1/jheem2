source("R/LOCATIONS_location_manager.R")

register.types(c("county","zipcode","cbsa", "state"),
               c("FIPS.","ZIP.","CBSA.", ""),
               c("FIPS Code", "Zipcodes", "Community Based Statistical Area", "State"))

#register.fips.prefix("fips.")
#register.zip.prefix("zip.")
#register.cbsa.prefix("cbsa.")
#Since we want state abbrev as location codes, we register them
#first, then register the fips codes as location code aliases
start <- proc.time()
LOCATION.MANAGER$register.state.abbrev("locations/us_state_abbreviations.csv")
LOCATION.MANAGER$register.state.fips.aliases("locations/fips_state_aliases.csv", fips.typename= "county") #Set the fips typename
LOCATION.MANAGER$register.fips("locations/fips_codes.csv", fips.typename = "county") #Set the fips typename
LOCATION.MANAGER$register.cbsa("locations/cbsas.csv", cbsa.typename = "cbsa", fips.typename = "county") #Sets the fips and cbsa typename
LOCATION.MANAGER$register.zipcodes("locations/zip_codes.csv", zip.typename = "zipcode")
end <- proc.time()

print("Time elapsed: ", (end - start)['user.self'])

#Barebones testing
# register.locations ("State", c("NY","FL","CA","TX"), c("New York", "Florida", "California", "Texas"))
# register.locations ("City", c("NYC", "MIA", "SFC", "HOU","TMP"), c("New York", "Miami","San Francisco", "Houston", "Tampa"))
# register.locations ("Town", "SMT", "Small Town")
# register.locations ("CITY","BRN","Brooklyn")
# register.name.aliases("SFC","SanFrancisco","no spaces")
# register.name.aliases("NYC","NewYork","no spaces")
# register.code.aliases("SFC", "GoldenState")
# register.locations("County", "TMC", "TAMPACOUNTY")
# register.sub.and.super.locations( c("NYC","MIA"), c("NY","FL"), TRUE)
# register.sub.and.super.locations( "TMP", "FL", TRUE)
# register.sub.and.super.locations( "SMT", "FL", TRUE )
# register.sub.and.super.locations( "TMC", "TMP", TRUE)
# register.sub.and.super.locations( "BRN", "NYC", FALSE)
# #register.code.aliases("TTC","TorontoTransit") #Fails due to TTC being unrecognized
# 
# print(LOCATION.MANAGER$location.list)
# print(LOCATION.MANAGER$alias.names)
# print(LOCATION.MANAGER$alias.codes)
# print(LOCATION.MANAGER$location.list[["NY"]]$contains.list)
# print(LOCATION.MANAGER$location.list[["FL"]]$contains.list)
# 
# get.location.name(c("GoldenState","NY","TOR",NA,"HOU"))
# #     GoldenState              NY             TOR            <NA>             HOU 
# # "San Francisco"      "New York"              NA              NA       "Houston" 
# 
# get.location.name.alias(c("GoldenState","NYC"),"no spaces")
# get.location.name.alias(c("GoldenState", "FL"), "no spaces", F) #Fine; returns NA for FL
# #get.location.name.alias(c("GoldenState", "FL"), "no spaces", T) #Throws an error
# get.location.name.alias(c("GoldenState", NA), "no spaces", F) #Fine; returns NA for NA
# 
# get.location.type(c("GoldenState","NYC","NY","MIA","TOR",NA))
# 
# print(get.sub.locations (c("NY","FL", NA, "TOR"), "CITY", F, T))
# 
# register.fips.file("locations/fips_codes.csv")
# register.zipcode.file("locations/zip_codes.csv")
# register.state.abbrev.file("locations/us_state_abbreviations.csv")
# #get.sub.locations (c("NY","NYC"),"postal", F, F, F)
# #get.sub.locations (c("NY","FL", NA, "TOR"), "city", F, T)
# 
# #get.super.locations(c("MIA","SMT"), "state", F)
#                               #limit.to.completely.enclosing,
#                               #return.list=F,
#                               #throw.error.if.unregistered.type=T)
