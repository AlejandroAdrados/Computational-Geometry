punto1<-c(1,2)
punto2<-c(2,3)
punto3<-c(3,5)
punto4<-c(-1,2)
punto5<-c(-2,4)

puntos<-rbind(punto1,punto2,punto3,punto4,punto5)
colnames(puntos)<-c("x","y")
plot(puntos,xlim=c(-15,15),ylim=c(0,15),type="p",col="green")

#TRASLACIÓN
vectorTraslacion<-c(-5,2)
traslacion<-function(puntos,q){
  p_nuevo<-matrix(nrow = length(puntos[,1]) , ncol = 2)
  i<-1
  for (i in 1:length(puntos[,1])) {
    p<-puntos[i,]
    p_nuevo[i,1]=p[1]+q[1]
    p_nuevo[i,2]=p[2]+q[2]
  }
  p_nuevo
}
puntosTraslacion<-traslacion(puntos,vectorTraslacion)
colnames(puntosTraslacion)<-c("x","y")
points(puntosTraslacion,type="p",col="red")

#ROTACIÓN
alfa<-pi/4
centroRotacion<-c(0,0)

Rotacion<-function(puntos,q,alfa){
  p_nuevo<-matrix(nrow = length(puntos[,1]) , ncol = 2)
  i<-1
  for (i in 1:length(puntos[,1])) {
    p<-puntos[i,]
    p_nuevo[i,1]=cos(alfa)*(p[1]-q[1])-sin(alfa)*(p[2]-q[2])+q[1]
    p_nuevo[i,2]=sin(alfa)*(p[1]-q[1])+cos(alfa)*(p[2]-q[2])+q[2]
  }
  p_nuevo
}

puntosRotacion<-Rotacion(puntos,centroRotacion,alfa)
colnames(puntosRotacion)<-c("x","y")
points(puntosRotacion,type="p",col="blue")

#SIMETRÍA RESPECTO RECTA
recta<-function(x) {3*x+2}

Simetria<-function(puntos,r){
  p_nuevo<-matrix(nrow = length(puntos[,1]) , ncol = 2)
  c=c(0,r(0))
  a=r(1)-r(0)
  b=-1
  aux=1/(a^2+b^2)
  M= matrix(c(b^2-a^2,-2*a*b,-2*a*b, a^2-b^2), nrow=2, ncol=2, byrow=TRUE)
  i<-1
  for (i in 1:length(puntos[,1])) {
    p<-puntos[i,]
    p_nuevo[i,]=aux*M%*%(p-c)+c
    p_nuevo[i,]<-t(p_nuevo[i,])
  }
  p_nuevo
}

puntosSimetria<-Simetria(puntos,recta)
colnames(puntosRotacion)<-c("x","y")
points(puntosSimetria,type="p",col="orange")

#HOMOTECIA
razon<-4
centroHomotecia<-c(1,2)

Homotecia<-function(puntos,q,razon){
  p_nuevo<-matrix(nrow = length(puntos[,1]) , ncol = 2)
  i<-1
  for (i in 1:length(puntos[,1])) {
    p<-puntos[i,]
    p_nuevo[i,1]=q[1]*(1-razon)+razon*p[1]
    p_nuevo[i,2]=q[2]*(1-razon)+razon*p[2]
  }
  p_nuevo
}

puntosHomotecia<-Homotecia(puntos,centroHomotecia,razon)
colnames(puntosHomotecia)<-c("x","y")
points(puntosHomotecia,type="p",col="black")



t<-seq(0,2*pi,0.1)
x<-sin(t)*(exp(1)^cos(t)-2*cos(4*t)-sin(t/12)^5)
y<-cos(t)*(exp(1)^cos(t)-2*cos(4*t)-sin(t/12)^5)


puntosMariposa<-matrix(nrow=63,ncol=2)
for(j in 1:63){
  puntosMariposa[j,1]<-x[j]
  puntosMariposa[j,2]<-y[j]
}
plot(puntosMariposa,xlim=c(-15,15),ylim=c(-15,15),type="p",col="green")

puntosTraslacion<-traslacion(puntosMariposa,vectorTraslacion)
colnames(puntosTraslacion)<-c("x","y")
points(puntosTraslacion,type="p",col="red")

puntosRotacion<-Rotacion(puntosMariposa,centroRotacion,alfa)
colnames(puntosRotacion)<-c("x","y")
points(puntosRotacion,type="p",col="blue")

puntosSimetria<-Simetria(puntosMariposa,recta)
colnames(puntosRotacion)<-c("x","y")
points(puntosSimetria,type="p",col="orange")


puntosHomotecia<-Homotecia(puntosMariposa,centroHomotecia,razon)
colnames(puntosHomotecia)<-c("x","y")
points(puntosHomotecia,type="p",col="black")
