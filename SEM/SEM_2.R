library(lavaan)
library(piecewiseSEM)

vignette('piecewiseSEM')

# DAG: X1 -> Y1 -> Y3 , X1 -> y2 -> y1

dat <- data.frame(x1 = runif(50), y1 = runif(50), y2 = runif(50), y3 = runif(50))
dat = data.frame(scale(dat))

model <- psem(lm(y1 ~ x1, dat), 
              lm(y1 ~ y2, dat), 
              lm(y2 ~ x1, dat), 
              lm(y3 ~ y1, dat))

model <- psem(lm(y1 ~ x1 + y2, dat), 
              lm(y2 ~ x1, dat),
              lm(y3 ~ y1, dat))
basisSet(model)
d = dSep(model)
fisherC(d)

coefs(model, standardize = "scale")



# age -> firesev -> cover


data("keeley")
keeley_psem <- psem(
  lm(firesev ~ age, data = keeley),
  lm(cover ~ firesev, data = keeley),
  data = keeley)

basisSet(keeley_psem)
dSep(keeley_psem, .progressBar = FALSE)

summary(lm(cover ~ firesev + age, data = keeley))$coefficients[3,4]
(C <- -2 * log(summary(lm(cover ~ firesev + age, data = keeley))$coefficients[3, 4]))
