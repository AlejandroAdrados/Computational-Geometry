# Introducimos los datos
# dpl estructura de los dummy nodes para la teselacion
# rw coordenadas del recuadro de la teselacion
# Se instalan las librer�as deldir, tripack, RColorBrewer
library(deldir)
x <- c(2.3,3.0,7.0,1.0,3.0,8.0)
y <- c(2.3,3.0,2.0,5.0,8.0,9.0)
dxy1 <- deldir(x,y,dpl=NULL, rw=NULL, plotit=TRUE)
# Centroides de la triangulacion
l<-tile.list(dxy1)
g<-tile.centroids(l)
plot(l,close=TRUE)
points(g,pch=20,col="red")

# Ejemplo con Voronoi
set.seed(1)
pts <- cbind(X=rnorm(500,rep(seq(1,9,by=2)/10,100),.022),Y=rnorm(500,.5,.15))
plot(pts)

km1 <- kmeans(pts, centers=5, nstart = 1, algorithm = "Lloyd")
library(tripack)
library(RColorBrewer)
CL5 <- brewer.pal(5, "Pastel1")
V <- voronoi.mosaic(km1$centers[,1],km1$centers[,2])
P <- voronoi.polygons(V)
plot(pts,pch=19,xlim=0:1,ylim=0:1,xlab="",ylab="",col=CL5[km1$cluster])
points(km1$centers[,1],km1$centers[,2],pch=3,cex=1.5,lwd=2)
plot(V,add=TRUE)
