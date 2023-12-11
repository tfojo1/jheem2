

source('../jheem_analyses/applications/EHE/ehe_specification.R')
N.ITER = 5
DO.RPROF = T


print("STARTING THE LOOP")

total.start.time = Sys.time()
if (DO.RPROF)
    Rprof()
for (i in 1:n.loop)
{
    print('running one')
    source('../jheem_analyses/applications/EHE/ehe_specification.R')
    
    if (ceiling(i/5)==(i/5))
        print(paste0("Done running ", i, " / ", N.ITER, " iterations"))
}

if (DO.RPROF)
    Rprof(NULL)
total.end.time = Sys.time()
total.time = as.numeric(total.end.time) - as.numeric(total.start.time)


print("DONE WITH THE LOOP")

print(paste0("Took on average ",
             round(total.time / n.loop / 2, 2),
             " seconds per build of the specification"))

if (DO.RPROF)
    summaryRprof()
