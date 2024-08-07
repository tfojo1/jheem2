


# ANDREW'S WAY
outcome.now = .6
param.now = 5

target = .95



rel.step = (logit(target) - logit(outcome.now))/logit(target)

tsfx.step = rel.step * log(param.now)

next.param = exp(log(param.now) + tsfx.step)


# NAIVE DIFFERENTIAL #

param.prev = 5
param.now = 7

outcome.prev = .6
outcome.now = .7

target.outcome = .95

naive.dx = (logit(outcome.now)-logit(outcome.prev)) / (log(param.now)-log(param.prev))

tsfx.step = (logit(target.outcome)-logit(outcome.now)) / naive.dx

next.param = exp(log(param.now) + tsfx.step)




