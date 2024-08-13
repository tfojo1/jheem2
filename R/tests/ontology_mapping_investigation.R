# investigating ontologies

ont1 = ontology(color = c('orange', 'yellow', 'green'), incomplete.dimensions = 'color')
ont2 = ontology(color = c('blue', 'violet', 'red'), incomplete.dimensions = 'color')

ont3 = ontology(color = c('orange', 'yellow', 'green', 'blue'), incomplete.dimensions = 'color')

ont4 = ontology(color = c('orange', 'yellow', 'green', 'blue'),
                size = c('large', 'small'), incomplete.dimensions = 'color')

get.ontology.mapping(ont1, ont2) # NULL
get.ontology.mapping(ont1, ont3) # identity
get.ontology.mapping(ont3, ont1) # identity

get.mappings.to.align.ontologies(ont1, ont2) # NULL
get.mappings.to.align.ontologies(ont1, ont3) # two identities

get.ontology.mapping(ont1, ont4) # NULL
aligning.mappings = get.mappings.to.align.ontologies(ont1, ont4) # two identities
common.A = aligning.mappings[[1]]$apply.to.ontology(ont1) # ends up the same as ont1
common.B = aligning.mappings[[2]]$apply.to.ontology(ont4) # ends up the same as ont2

# perhaps common.B is what we'd want to use to align subsequent ontologies?
get.ontology.mapping(ont2, common.B) # NULL
get.mappings.to.align.ontologies(ont2, common.B) # two identities


# IF ONTOLOGY MAPPINGS ARE REGISTERED ALREADY
cdc = ontology(year = as.character(2010:2017),
               location = c('C.12580', 'MD', '24510'),
               age = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
               race = c("American Indian/Alaska Native", "Asian", "Black/African American", "Hispanic/Latino", "Multiracial", "Native Hawaiian/Other Pacific Islander", "White"),
               sex = c('male', 'female'),
               risk = c('msm', 'idu', 'msm_idu', 'heterosexual', 'other'),
               incomplete.dimensions = c('year', 'location'))
lhd = ontology(year = as.character(2010:2017),
               location = c('C.12580', 'MD', '24510'),
               age = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
               race = c("black", "hispanic", "other"),
               sex = c('male', 'female'),
               risk = c('msm', 'idu', 'msm_idu', 'heterosexual'),
               incomplete.dimensions = c('year', 'location'))
sim = ontology(year = as.character(2010:2017),
               location = c('C.12580', 'MD', '24510'),
               age = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
               race = c("black", "hispanic", "other"),
               sex = c('heterosexual_male', 'female', 'msm'),
               risk = c('active_IDU', 'never_IDU', 'IDU_in_remission'),
               incomplete.dimensions = c('year', 'location'))

# get a universal ontology that aligns all three ontologies
uni = lhd
cdc.uni.mappings = get.mappings.to.align.ontologies(cdc, uni)
uni = cdc.uni.mappings[[2]]$apply.to.ontology(uni) # propagates whatever locations/years lhd had -- in this case fine because they're all the same
sim.uni.mappings = get.mappings.to.align.ontologies(sim, uni)
uni = sim.uni.mappings[[2]]$apply.to.ontology(uni)

# uni should have the same years and locations, then the same ages, the jheem/lhd race, the cdc/lhd sex, and the lhd risk

lhd.data.dimnames = ontology(year = as.character(2010:2020),
                             location = c('C.12580', 'C.47900'),
                             incomplete.dimensions = c('year', 'location'))
cdc.data.dimnames = ontology(year = as.character(2010:2020),
                             location = c('MD', 'AZ', '24510'),
                             incomplete.dimensions = c('year', 'location'))

# when the first ontology is examined
aligning.mappings = get.mappings.to.align.ontologies(lhd.data.dimnames, uni) # two identities

# make a new "common ontology" for the next data ontology we check
common.A = aligning.mappings[[1]]$apply.to.ontology(lhd.data.dimnames) # overwrites locations

get.ontology.mapping(cdc.data.dimnames, common.A) # NULL
subsequent.aligning.mappings = get.mappings.to.align.ontologies(cdc.data.dimnames, common.A) # NULL!

common.B = aligning.mappings[[2]]$apply.to.ontology(uni) # has all dimensions including those we don't need but correct locations
get.ontology.mapping(cdc.data.dimnames, common.B) # NULL
subsequent.aligning.mappings = get.mappings.to.align.ontologies(cdc.data.dimnames, common.B) # two identities

# THE LAST METHOD SUCCEEDED, BUT WE WOULD WANT TO BE ABLE TO GET A SINGLE MAPPING, NOT NEEDING TO ALIGN
# We could try paring down the target ontology to have only the dimensions 'year' and 'location'...
# that would likely work here, but not if different ontologies needed different dimensions (sex/risk stuff, for example)
common.C = common.B[names(common.B) %in% c('year', 'location')]
get.ontology.mapping(cdc.data.dimnames, common.C) # SUCCESS! An identity mapping.


# --- CASE WHERE THIS STRATEGY WOULDN'T WORK ... actually IT DOES!!--- #
# keep.dimensions: year, location, risk
lhd.data.dimnames = ontology(year = as.character(2010:2020),
                             location = c('C.12580', 'C.47900'),
                             risk = c('msm', 'idu', 'msm_idu', 'heterosexual'),
                             incomplete.dimensions = c('year', 'location'))
sim.if.it.was.data.dimnames = sim[names(sim) %in% c('year', 'location', 'sex', 'risk')]

aligning.mappings = get.mappings.to.align.ontologies(lhd.data.dimnames, uni) # two identities

dimensions.we.think.we.will.need = c('year', 'location', 'risk')
common.B = aligning.mappings[[2]]$apply.to.ontology(uni)
common.C = common.B[names(common.B) %in% dimensions.we.think.we.will.need]

get.ontology.mapping(sim.if.it.was.data.dimnames, common.C) # WORKS!!

dimensions.we.actually.need = c('year', 'location', 'sex', 'risk')
common.D = common.B[names(common.B) %in% dimensions.we.actually.need]

get.ontology.mapping(sim.if.it.was.data.dimnames, common.D) # ALSO WORKS

# CONCLUSION: IF KEEP DIMENSIONS ARE X, CAN WE JUST USE THOSE DIMENSIONS OF A UNIVERSAL ONTOLOGY?


# --- AN EXPERIMENT --- #
ont1 = ontology(sex = c('male', 'female'),
                risk = c('idu', 'heterosexual'))
ont2 = ontology(sexrisk = c('male_idu', 'male_heterosexual', 'female_idu', 'female_heterosexual'))

register.ontology.mapping('imaginary.sex.risk',
                          from.dimensions = c('sex', 'risk'),
                          to.dimensions = c('sexrisk'),
                          mappings = rbind(c('male', 'idu', 'male_idu'),
                                           c('male', 'heterosexual', 'male_heterosexual'),
                                           c('female', 'idu', 'female_idu'),
                                           c('female', 'heterosexual', 'female_heterosexual'))
)
get.ontology.mapping(ont1, ont2) # GETS ERROR in "if (is.ontology(from.dim.names) && is_complete(from.dim.names)[d]) self$to.dim.names[[d]] else unique(resulting.to.values[, ..."
mp = ONTOLOGY.MAPPING.MANAGER$mappings$imaginary.sex.risk
#
