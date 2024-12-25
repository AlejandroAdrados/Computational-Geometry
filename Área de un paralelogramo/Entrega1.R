#Tenemos,v,w
v<-c(1,2)
w<-c(-1,1)
#Hacemos v+w y nos da una diagonal del paralelogramo lo llamaremos d 
d<-v+w
#Hacemos el ángulo entre v2 y d
nd<-sqrt(d[1]^2+d[2]^2)
nw<-sqrt(w[1]^2+w[2]^2)
ang<-acos((w%*%d)/(nd*nw))
#Hallamos la altura del triángulo
h<-sin(ang)*nw
#Entonces tenemos base d y altura h para hacer el área del triángulo 
areaT<-(nd*h)/2
#Multiplicamos por 2, ya que son dos triángulos iguales
area<-areaT*2
area<-as.integer(area)
area
#Si hacemos el determinante podemos ver que da lo mismo 
M<-matrix(c(v[1],w[1],v[2],w[2]),2,2)
area2<-abs(det(M))
area2