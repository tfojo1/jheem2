

N.SIM = 50
RUN.RPROF = T

source('../jheem2/R/tests/ENGINE_test.R')



print(paste0("Starting ", N.SIM, " runs..."))
start.time = Sys.time()

if (RUN.RPROF)
    Rprof()

for (i in 1:N.SIM)
  engine$run()

if (RUN.RPROF)
{
    summ = summaryRprof()
    Rprof(NULL)
}

end.time = Sys.time()

print(paste0("Done"))

total.time = as.numeric(end.time) - as.numeric(start.time)
time.per = total.time / N.SIM

print(paste0(round(time.per, 1), " seconds per simulation"))