

N.SIM = 50
RUN.RPROF = T


index.time = 0
fold.time = 0
binding.time = 0
eval.time = 0
fn.dim.names.time = 0
in.time = 0
out.time = 0
total.calc.time = 0
preamble.time = 0
loop.top.time = 0
source('../jheem2/R/tests/ENGINE_test.R')

index.time = 0
fold.time = 0
binding.time = 0
eval.time = 0
fn.dim.names.time = 0
in.time = 0
out.time = 0
total.calc.time = 0
preamble.time = 0
loop.top.time = 0


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