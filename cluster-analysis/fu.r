devtools::install_github("ropenscilabs/umapr")
library(umap)
head(iris)
iris.umap = umap((iris[,1:4]))
head(iris.umap)
fu=as.data.frame(iris.umap$layout)