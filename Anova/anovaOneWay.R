

n = 1
sample = 10




x = sapply(c(rep(1,9), 1.5), function(x) rnorm(sample, x))

means = apply(x, 2, mean)



gesamt = mean(c(x))

# Gesamt varianz
sb1 = sum(
  sapply(means, function(x) (x - gesamt)^2*sample))/(10-1)



# die durchschnittliche Gruppenvarianz
sb2 = sum(sapply(1:10, function(n, x, mean) (x[,n] - mean[n])^2, x, means))  /  (10*(sample-1))



cat(sb1, " ", sb2)

sb1/sb2

(r = df(sb1/sb2, 9, 10*(sample-1)))

(p = pf(r, 9, 10*(sample-1)))

qf(0.05,2, 297)














