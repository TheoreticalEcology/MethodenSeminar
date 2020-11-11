# loading the library and the image 
library(jpeg)
cat <- readJPEG('cat.jpg')

# get dimensions of the image 
ncol(cat)
nrow(cat)

## extracting the color channels 

r <- cat[,,1]
g <- cat[,,2]
b <- cat[,,3]

# perform PCA on each color channel 

cat.r.pca <- prcomp(r, center = FALSE)
cat.g.pca <- prcomp(g, center = FALSE)
cat.b.pca <- prcomp(b, center = FALSE)

# collect the pca results into a list 

rgb.pca <- list(cat.r.pca, cat.g.pca, cat.b.pca)

# plot the outputs 

for (i in seq.int(3, round(nrow(cat) - 10), length.out = 10)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('cat_compressed_', round(i,0), '_components.jpg', sep = ''))
}
