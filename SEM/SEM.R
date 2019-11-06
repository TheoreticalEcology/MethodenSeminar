library(piecewiseSEM)
library(lavaan)

model = "y1 ~ x1 + z2"

dat <- data.frame(x1 = runif(50), y1 = runif(50), y2 = runif(50), y3 = runif(50))

model <- psem(lm(y1 ~ x1, dat), 
              lm(y1 ~ y2, dat), 
              lm(y2 ~ x1, dat), 
              lm(y3 ~ y1, dat))

model <- psem(lm(y1 ~ x1 + y2, dat), 
              lm(y2 ~ x1, dat), 
              lm(y3 ~ y1, dat))
coefs(model, standardize = "scale")





keeley_psem <- psem(
  lm(firesev ~ age, data = keeley),
  lm(cover ~ firesev, data = keeley),
  data = keeley)

keeley_psem
S

basisSet(keeley_psem)

dSep(keeley_psem, .progressBar = FALSE)
summary(lm(cover ~ firesev + age, data = keeley))$coefficients
(C <- -2 * log(summary(lm(cover ~ firesev + age, data = keeley))$coefficients[3, 4]))
1-pchisq(C, 2)
fisherC(keeley_psem)

keeley_psem2 <- psem(
  lm(cover ~ firesev + age, data = keeley),
  lm(firesev ~ age, data = keeley),
  data = keeley
)



library(lavaan)

sem1 <- '
firesev ~ age
cover ~ firesev
'

keeley_sem1 <- sem(sem1, keeley)

summary(keeley_sem1, standardize = T, rsq = T)




# correlation <-> regression coefficient

a = runif(100)
b = 2*a + rnorm(100,0,0.3)
coef(lm(b~a))[2]
cor(a,b)*(sd(b)/sd(a))

#(cov(a, b) / (sd(a) * sd(b))) == cor(a, b)
# -> if z-transformed, correlation == regression coefficient -> standardized coef
# when not standardized we need cov

# If not, values depend on their unit
# we must scale by the variance of the predictor X
cov(a,b)/var(a)


# standardized:
coef(lm(scale(b)~scale(a)))[2]
cor(scale(a), scale(b))


#x1 -> y1 -> y2
x1 = runif(100)
y1 = x1 + runif(100)
y2 = y1 + runif(100)

(pathx1_y1 <- summary(lm(scale(y1) ~ scale(x1)))$coefficients[2, 1])

(pathy1_y2 <- summary(lm(scale(y2) ~ scale(y1)))$coefficients[2, 1])
cor(y2,y1)


# x1 -> y2?
pathx1_y1 * pathy1_y2
cor(x1, y2)


# we must remove the shared variance x1 -> y1
(by2x1 = ( cor(x1, y2) - cor(x1, y1) * cor(y1,y2) ) / (1- cor(x1, y1)^2) * sd(y2)/sd(x1))
summary(lm(y2 ~ x1 + y1))$coefficients[2,1]

(by1y2 = (cor(y1,y2) - cor(x1, y1) * cor(x1, y2) ) / (1 - cor(x1, y1)^2))
summary(lm(scale(y2) ~ scale(x1) + scale(y1)))$coefficients[2:3,1]
