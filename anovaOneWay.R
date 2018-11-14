
aov()

anova()

n = 1
sample = 10

x1 = rnorm(100,1,n)
x2 = rnorm(100,1,n)
x3 = rnorm(100,0,n)


x = sapply(c(rep(1,9), 1.5), function(x) rnorm(sample, x))

means = apply(x, 2, mean)

mean1 = mean(x1)
mean2 = mean(x2)
mean3 = mean(x3)



gesamt = mean(c(x))

# Gesamt varianz
sb1 = sum(sapply(means, function(x) (x - gesamt)^2*sample))/(10-1)

# sb1 = (mean1 - gesamt)^2 * (100) + 
#       (mean2 - gesamt)^2 * (100) +
#       (mean3 - gesamt)^2 * (100) 
# 
# sb1 = sb1/(3-1)

# die durchschnittliche Gruppenvarianz
sb2 = sum(sapply(1:10, function(n, x, mean) (x[,n] - mean[n])^2, x, means))/(10*(sample-1))


# sb2 = sum((x1-mean1)^2 + (x2-mean2)^2 + (x3-mean3)^2) / (3*(100-1))

cat(sb1, " ", sb2)
(r = df(sb1/sb2, 9, 10*(sample-1)))

(p = pf(r, 9, 10*(sample-1)))

qf(0.05,2, 297)











t.test(x2, x3)


var.test(x1,x2,x3)
