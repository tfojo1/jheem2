
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

N.ITER = 2
R.PROF = T

print(paste0("Starting likelihood instantiation profiling for ", N.ITER, " times"))

if (R.PROF)
{
  Rprof()
}

start.time = as.numeric(Sys.time())
for (i in 1:N.ITER)
{
    print(paste0("Instantiating ", i, " of ", N.ITER))
  lik = two.way.transmission.pop.aids.idu.likelihood.instructions$instantiate.likelihood('ehe', "C.12580")
}

end.time = as.numeric(Sys.time())
run.time = end.time - start.time

if (R.PROF)
{
  summ = summaryRprof()
  Rprof(NULL)
}

print(paste0("DONE RUNNING : Took ",
             round(run.time/60, 0), " minutes to instantiate the likelihood ",
             format(N.ITER, big.mark = ","),
             " times (",
             round(run.time / N.ITER, 1), " seconds per instantiation on average)"))