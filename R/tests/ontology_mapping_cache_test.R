source('../jheem_analyses/applications/ehe/ehe_ontology_mappings.R')


from.ont = ontology(race = c('black','hispanic','white','other'),
                    sex = c('msm','heterosexual_male','female'))
to.ont = ontology(race = c('black','hispanic','other'),
                  sex = c('male','female'))


ont1 = ontology(race = c('black','hispanic','white','other'),
                    sex = c('male','female'),
                    risk = c('msm','idu','msm_idu','heterosexual'))
ont2 = ontology(race = c('black','hispanic','other'),
                  sex = c('msm','heterosexual_male','female'),
                  risk = c('active_IDU','IDU_in_remission','never_IDU'))


start1 = Sys.time()
map = get.ontology.mapping(from.ont, to.ont)
#map = get.mappings.to.align.ontologies(ont1, ont2)
end1 = Sys.time()

start2 = Sys.time()
map = get.ontology.mapping(from.ont, to.ont)
#map = get.mappings.to.align.ontologies(ont1, ont2)
end2 = Sys.time()

time1 = as.numeric(end1) - as.numeric(start1)
time2 = as.numeric(end2) - as.numeric(start2)

print(paste0("second time / first = ",
             round(time2/time1, 4)))