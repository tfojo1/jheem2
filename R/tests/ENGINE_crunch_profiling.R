

N.ITER = 20
DO.RPROF = T

print("COMPILING THE SPECIFICATION")
source('../jheem_analyses/applications/EHE/ehe_specification.R')


params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.1

print("STARTING THE LOOP")

total.start.time = Sys.time()
if (DO.RPROF)
    Rprof()
for (i in 1:N.ITER)
{
    engine = create.jheem.engine('ehe', 'c.12580', start.year=1970, end.year=2025)
    engine$crunch(parameters = params)
    
    if (ceiling(i/10)==(i/10))
        print(paste0("Done running ", i, " / ", N.ITER, " iterations"))
}
if (DO.RPROF)
    Rprof(NULL)
total.end.time = Sys.time()
total.time = as.numeric(total.end.time) - as.numeric(total.start.time)


print("DONE WITH THE LOOP")

print(paste0("Took on average ",
             round(total.time / N.ITER, 2),
             " seconds per crunch"))

if (DO.RPROF)
    summaryRprof()
