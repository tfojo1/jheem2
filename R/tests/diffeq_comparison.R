
N.ITER = 20

load('R/tests/diffeq_comparison.Rdata')
Rcpp::sourceCpp('src/diffeq.cpp')

#-- SETUP for ODEINTR --#
library(odeintr)
fn.odeintr = function(x, t){
  
    compute_dx(state = x,
               time = t,
               settings = args$settings,
               quantities_info = args$quantities_info,
               quantity_scratch_vector = args$quantity_scratch_vector,
               scratch_vector = args$scratch_vector,
               natality_info = args$natality_info,
               mortality_info = args$mortality_info,
               transitions_info = args$transitions_info,
               infections_info = args$infections_info,
               remission_info = args$remission_info,
               fixed_strata_info = args$fixed_strata_info,
               population_trackers = args$population_trackers)
}

#-- SETUP for SUNDIALR --#
#library(sundialr)
fn.sundialr = function(t, y, p){
    
    compute_dx(state = y,
               time = t,
               settings = args$settings,
               quantities_info = args$quantities_info,
               quantity_scratch_vector = args$quantity_scratch_vector,
               scratch_vector = args$scratch_vector,
               natality_info = args$natality_info,
               mortality_info = args$mortality_info,
               transitions_info = args$transitions_info,
               infections_info = args$infections_info,
               remission_info = args$remission_info,
               fixed_strata_info = args$fixed_strata_info,
               population_trackers = args$population_trackers)
}

times = 1970:2021

#-- SETUP for DESOLVE --#
#library(deSolve)
fn.desolve = function(time, y, parms){
    
    list(compute_dx(state = y,
               time = time,
               settings = args$settings,
               quantities_info = args$quantities_info,
               quantity_scratch_vector = args$quantity_scratch_vector,
               scratch_vector = args$scratch_vector,
               natality_info = args$natality_info,
               mortality_info = args$mortality_info,
               transitions_info = args$transitions_info,
               infections_info = args$infections_info,
               remission_info = args$remission_info,
               fixed_strata_info = args$fixed_strata_info,
               population_trackers = args$population_trackers))
}

#-- SETUP for DIFFEQR --#
print("Julia diffeqr setup...")
start.diffeqr.setup = Sys.time()
de = diffeqr::diffeq_setup()
end.diffeqr.setup = Sys.time()
print(paste0("done Julia diffeqr setup: took ", 
             round((as.numeric(end.diffeqr.setup)-as.numeric(start.diffeqr.setup))/60,1),
             " minutes"))

fn.diffeqr = function(u,p,t){
    
    compute_dx(state = u,
                    time = t,
                    settings = args$settings,
                    quantities_info = args$quantities_info,
                    quantity_scratch_vector = args$quantity_scratch_vector,
                    scratch_vector = args$scratch_vector,
                    natality_info = args$natality_info,
                    mortality_info = args$mortality_info,
                    transitions_info = args$transitions_info,
                    infections_info = args$infections_info,
                    remission_info = args$remission_info,
                    fixed_strata_info = args$fixed_strata_info,
                    population_trackers = args$population_trackers)
}

#-- RUN THE COMPARISONS --#
time.odeintr.1e3 = 0
time.odeintr.1e4 = 0
time.odeintr.1e24 = 0
time.odeintr.1e6 = 0
time.sundialr.1e3 = 0
time.desolve = 0
time.desolve.vode = 0
time.desolve.bdf = 0
time.desolve.ode23 = 0
time.desolve.rk4 = 0
time.desolve.daspk = 0
time.diffeqr = 0
time.diffeqr.1e4 = 0

for (i in 1:N.ITER)
{
    start = Sys.time()
    ode.results.odeintr.1e3 = odeintr::integrate_sys(sys = fn.odeintr,
                                         init = args$init,
                                         duration = 51, 
                                         start = 1970,
                                         atol = 1e-3,
                                         rtol = 1e-3)
    end = Sys.time()
    time.odeintr.1e3 = time.odeintr.1e3 + as.numeric(end) - as.numeric(start)
    
    start = Sys.time()
    ode.results.odeintr.1e24 = odeintr::integrate_sys(sys = fn.odeintr,
                                         init = args$init,
                                         duration = 51,
                                         start = 1970,
                                         atol = 1e-2,
                                         rtol = 1e-4)
    end = Sys.time()
    time.odeintr.1e24 = time.odeintr.1e24 + as.numeric(end) - as.numeric(start)
    
    
    start = Sys.time()
    prob = de$ODEProblem(fn.diffeqr,
                         args$init,
                         c(1970,2021))

    ode.results.diffeqr = de$solve(prob,
                                  # de$Tsit5(),
                                   de$BS3(),
                                   # de$AutoTsit5(de$Rosenbrock23()),
                                 #   de$OwrenZen3(),
                                  #de$VCABM(),
                                   #de$DP5(), #1.08x
                                   #de$FBDF(autodiff=F),
                                   #de$QNDF(autodiff=F), slow!
                                   #de$Rosenbrock23(autodiff=F), slow!
                                   saveat=1970:2021,
                                  abstol=1e-02,
                                  reltol=1e-04)
    end = Sys.time()
    time.diffeqr = time.diffeqr + as.numeric(end) - as.numeric(start)
    
    
    # start = Sys.time()
    # prob = de$ODEProblem(fn.diffeqr,
    #                      args$init,
    #                      c(1970,2021))
    # 
    # ode.results.diffeqr.1e4 = de$solve(prob,
    #                                saveat=1970:2021,
    #                                abstol=1e-04,
    #                                reltol=1e-04)
    # end = Sys.time()
    # time.diffeqr.1e4 = time.diffeqr.1e4 + as.numeric(end) - as.numeric(start)
    
    # start = Sys.time()
    # ode.results.odeintr.1e4 = odeintr::integrate_sys(sys = fn.odeintr,
    #                                                  init = args$init,
    #                                                  duration = 51,
    #                                                  start = 1970,
    #                                                  atol = 1e-4,
    #                                                  rtol = 1e-4)
    # end = Sys.time()
    # time.odeintr.1e4 = time.odeintr.1e4 + as.numeric(end) - as.numeric(start)
    
    # start = Sys.time()
    # ode.results = odeintr::integrate_sys(sys = fn.odeintr,
    #                                      init = args$init,
    #                                      duration = 51, 
    #                                      start = 1970,
    #                                      atol = 1e-6,
    #                                      rtol = 1e-6)
    # end = Sys.time()
    # time.odeintr.1e6 = time.odeintr.1e6 + as.numeric(end) - as.numeric(start)
    # 

    # 
    # start = Sys.time()
    # ode.results = deSolve::ode(func = fn.desolve,
    #                            y = args$init,
    #                            times = 1970:2021,
    #                            parms = NULL)
    # end = Sys.time()
    # time.desolve = time.desolve + as.numeric(end) - as.numeric(start)
    # 
    # 
    # start = Sys.time()
    # ode.results = deSolve::ode(func = fn.desolve,
    #                            y = args$init,
    #                            times = 1970:2021,
    #                            parms = NULL,
    #                            method='vode')
    # end = Sys.time()
    # time.desolve.vode = time.desolve.vode + as.numeric(end) - as.numeric(start)
    # 
    # start = Sys.time()
    # ode.results = deSolve::ode(func = fn.desolve,
    #                            y = args$init,
    #                            times = 1970:2021,
    #                            parms = NULL,
    #                            method='bdf')
    # end = Sys.time()
    # time.desolve.bdf= time.desolve.bdf + as.numeric(end) - as.numeric(start)
    # 
    # start = Sys.time()
    # ode.results = deSolve::ode(func = fn.desolve,
    #                            y = args$init,
    #                            times = 1970:2021,
    #                            parms = NULL,
    #                            method='ode23')
    # end = Sys.time()
    # time.desolve.ode23 = time.desolve.ode23 + as.numeric(end) - as.numeric(start)
    # 
    # start = Sys.time()
    # ode.results.rk = deSolve::ode(func = fn.desolve,
    #                            y = args$init,
    #                            times = 1970:2021,
    #                            parms = NULL,
    #                            method='rk4')
    # end = Sys.time()
    # time.desolve.rk4 = time.desolve.rk4 + as.numeric(end) - as.numeric(start)
    
    # start = Sys.time()
    # ode.results.daspk = deSolve::ode(func = fn.desolve,
    #                               y = args$init,
    #                               times = 1970:2021,
    #                               parms = NULL,
    #                               method='daspk')
    # end = Sys.time()
    # time.desolve.daspk = time.desolve.daspk + as.numeric(end) - as.numeric(start)
    
    # start = Sys.time()
    # ode.results = sundialr::cvode(time_vector = times,
    #                               IC = args$init,
    #                               input_function = fn.sundialr, 
    #                               Parameters = numeric(),
    #                               abstolerance = rep(1e-3, length(args$init)),
    #                               reltolerance = 1e-3)
    # end = Sys.time()
    # time.sundialr.1e3 = time.sundialr.1e3 + as.numeric(end) - as.numeric(start)
}

#-- SUMMARIZE --#
print(paste0("For odeintr at tol==1e-03, ODE time was ", round(time.odeintr.1e3/N.ITER,2), " seconds per solve"))
#print(paste0("For odeintr at tol==1e-06, ODE time was ", round(time.odeintr.1e6/N.ITER,2), " seconds per solve"))
#print(paste0("For sundialr at tol==1e-03, ODE time was ", round(time.sundialr.1e3/N.ITER,2), " seconds per solve"))
# print(paste0("For deSolve, ODE time was ", round(time.desolve/N.ITER,2), " seconds per solve"))

# print(paste0("odeintr at tol==1e-04 took ",
#               round(time.odeintr.1e4/time.odeintr.1e3,2), " times as long as odeintr at tol==1e-03"))
# print(paste0("odeintr at tol==1e-06 took ",
#              round(time.odeintr.1e6/time.odeintr.1e3,2), " times as long as odeintr at tol==1e-03"))

print(paste0("odeintr at atol=1e-02/rtol=1e-04 took ",
              round(time.odeintr.1e24/time.odeintr.1e3,2), " times as long as odeintr at tol==1e-03"))


print(paste0("diffeqr took ",
             round(time.diffeqr/time.odeintr.1e3,2), " times as long as odeintr at tol==1e-03"))
# print(paste0("diffeqr at atol/rtol=1e-04 took ",
#              round(time.diffeqr.1e4/time.odeintr.1e3,2), " times as long as odeintr at tol==1e-03"))

# print(paste0("sundialr at tol==1e-03 took ",
#              round(time.sundialr.1e3/time.odeintr.1e3,2), " times as long as odeintr at tol==1e-03"))

# print(paste0("deSolve took ",
# #              round(time.desolve/time.odeintr.1e3,2), " times as long as odeintr at tol==1e-03"))
# print(paste0("deSolve with method=vode took ",
#              round(time.desolve.vode/time.odeintr.1e3,2), " times as long as odeintr at tol==1e-03"))
# # print(paste0("deSolve with method=ode23 took ",
# #              round(time.desolve.ode23/time.odeintr.1e3,2), " times as long as odeintr at tol==1e-03"))
# print(paste0("deSolve with method=rk4 took ",
#              round(time.desolve.rk4/time.odeintr.1e3,2), " times as long as odeintr at tol==1e-03"))
# print(paste0("deSolve with method=bdf took ",
#              round(time.desolve.bdf/time.odeintr.1e3,2), " times as long as odeintr at tol==1e-03"))

# print(paste0("deSolve with method=daspk took ",
#              round(time.desolve.daspk/time.odeintr.1e3,2), " times as long as odeintr at tol==1e-03"))

#-- RESULTS SUMMARIZED (at 10 iterations) --#

# All relatve to odeintr with atol/rtol=1e-03
# 3.05 - for odeinter with atol/rtol=1e-06
# 1.40 - for odeinter with atol/rtol=1e-04
# 0.9 - for odeinter with atol=1e-02/rtol=1e-04
# 2.38 - for deSolve (default)
# 89 - for deSolve with method=vode
# 311 - for deSolve with method=bdf
# 2.31 - for deSolve with method=ode23
# GENERATES NAs - for deSolve with method=rk4 (but 0.19)
# 13.71 - for sundialr with atol/rtol=1e-03

if (1==2)
{
    r1 = ode.results.odeintr.1e3
    r2 = ode.results.odeintr.1e4
    
    range(r1-r2)
    range((r1-r2)/r1, na.rm=T)
    
    qplot(as.numeric)
}

if (1==2)
{
    library(sundialr)
    
    ODE_R <- function(t, y, p){
        # vector containing the right hand side gradients
        ydot = vector(mode = "numeric", length = length(y))
        # R indices start from 1
        ydot[1] = -0.04 * y[1] + 10000 * y[2] * y[3]
        ydot[3] = 30000000 * y[2] * y[2]
        ydot[2] = -ydot[1] - ydot[3]
        ydot
    }
    
    # R code to genrate time vector, IC and solve the equations
    time_vec <- c(0.0, 0.4, 4.0, 40.0, 4E2, 4E3, 4E4, 4E5, 4E6, 4E7, 4E8, 4E9, 4E10)
    IC <- c(1,0,0)
    reltol <- 1e-04
    abstol <- c(1e-8,1e-14,1e-6)
    ## Solving the ODEs using cvode function
    df1 <- cvode(time_vec, IC, ODE_R , numeric(), reltol, abstol)
    
    
    init = args$init[1:945]
    df1 <- cvode(time_vec, init, fn.sundialr , numeric(), reltol, rep(1e-3, length(init))) 
    
    
    x = sapply(ode.results.diffeqr$u, identity)
    dim(x)
    dim(ode.results.odeintr.1e24)
}