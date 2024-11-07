

print("ENGINE TEST: Creating Specification")
source('../jheem_analyses/applications/EHE/ehe_specification.R')

params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.13


print("ENGINE TEST: Setting up Engine")
engine = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10)

print("ENGINE TEST: Running")
sim = engine$run(parameters = params)


#params2 = suppressWarnings(get.quantiles(EHE.PARAMETERS.PRIOR, 0.4))
#params2['global.trate'] = 0.1
#sim2 = engine$run(parameters = params2)

print("ENGINE TEST: ALL DONE!")
