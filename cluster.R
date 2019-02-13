## Unsupervised ##

library(cluster)
data = data(iris)


cluster = kmeans(iris[,1:3], centers = 3)

plot(silhouette(x = cluster$cluster, dist(iris[,1:3])))


plot(Sepal.Length~Sepal.Width, data = iris, 
     col = cluster$cluster, pch = as.integer(iris$Species))



data.frame(iris$Species, cluster$cluster)
data(cars)
library(vegan)
data = data("dune.env")

dD = cluster::daisy(x = dune.env,metric = "gower")
cluster = kmeans(dD,3)

plot(silhouette(x = cluster$cluster, dD))

install.packages("klaR")
library(klaR)
cluster = klaR::kmodes(dune.env, 3)








library(randomForest)
rf = randomForest(dune.env, proximity = TRUE)
cluster = pam(rf$proximity, diss = T, k = 2)

plot(silhouette(x = cluster$cluster, rf$proximity))



plot(dune.env$A1~dune.env$Manure, 
     col = cluster$cluster)

time = system.time({1+2})
write.csv(as.matrix(time), file = "timeINTER.csv")
library(NbClust)

cluster = NbClust::NbClust(data = ,diss = as.matrix(dD) , distance = NULL, 
                 min.nc = 2, max.nc = 5, method = "median")
