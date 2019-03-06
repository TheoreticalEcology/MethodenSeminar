# Mantel test

x = runif(100,-1,1)
y = 2*x + 1

x = matrix(x,ncol = 1,byrow = T)

mantel = function(x,y, n = 100){
  xd = dist(x = x)
  yd = dist(x = y)
  
  r = sum(xd * yd) 
  r_dist = 
    sapply(1:n, function(i){
      x_sub = x[sample.int(nrow(x),  nrow(x)),]
      xd_sub = dist(x_sub)
      tmp = sum(xd_sub * yd)
      return(tmp)
    })
  
  t_value = (mean(r_dist) - r ) / sd(r_dist)
  
  out = list()
  out$p = pt(t_value, n)
  out$r_dist = r_dist
  out$r = r
  return(out)
}

p_hacking = 
  sapply(1:1000, function(egal){
  
  x1 = runif(100,-1,1)
  x = cbind(x1, x1*0.3, x1*-2, x1*5*x1)
  
  y1 = runif(100, -100, 100)
  y = cbind(y1*0.1, y1*0.3*y1, y1*-2.6*y1, y1*2*y1)
  
  x = apply(x, 2, function(c) return(c+rnorm(100,0,0.01)))
  y = apply(y, 2, function(c) return(c+rnorm(100,0,0.01)))
  
  
  res = mantel(scale(x),y,100)
  cat("\n")
  print(egal)
  #hist(res$r_dist)
  return(res$p)
  })
hist(p_hacking)

table(p_hacking < 0.05)





x1 = runif(100,-5,5)
x = cbind(x1, x1*0.3, x1*-2*x1, x1*5*x1)

y1 = runif(100, -5, 5)
y = cbind(y1*0.1, y1*0.3*y1, y1*-2.6+3, y1*2)

x = apply(x, 2, function(c) return(c+rnorm(100,0,0.01)))
y = apply(y, 2, function(c) return(c+rnorm(100,0,0.01)))


res = mantel(scale(x),scale(y),100)
res$p

cat("\n")
print(egal)