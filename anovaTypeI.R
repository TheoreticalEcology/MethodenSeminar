x1 = runif(100,-3,3)
x2 = runif(100,-3,3)

y = 2*x1 + 0.3*x2 + 4 + rnorm(100,0,0.5)

data = data.frame(x1, x2, y)

model = lm(y~x1+x2, data)
anova(model)

part = residuals(model, type = "partial")


varX1 = sum((part[,1]^2))

var(y) / (varX1/(1))


varX2 = sum((part[,2]^2))


anova(model)


library(effects)
library(DHARMa)

sim = simulateResiduals(model)
res = DHARMa::recalculateResiduals(sim,group = )

pres=(allEffects(model,partial.residuals = T))


anova(model)

summary(model)