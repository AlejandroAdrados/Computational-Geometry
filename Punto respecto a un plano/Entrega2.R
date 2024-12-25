

Entrega2<-function(punto,plano)
{
  #Si se quiere dibujar el plano descomentar este fragmento de código
  # x1=seq(-3,3,length=100)
  # y1=seq(-3,3,length=100)
  # z1<-outer(x1,y1,plano)
  # persp(x1,y1,z1)
  
  x<-punto[1]
  y<-punto[2]
  z<-plano(x,y)
  if (z[1]<punto[3])
    s<-'El punto está encima del plano'
  if (z[1]>punto[3])
    s<-'El punto está debajo del plano'
  if (z[1]==punto[3])
    s<-'El punto está en el plano'
  s
}

plano <-function(x,y){
  3*x-5*y
}
punto<-c(2,1,1)

Entrega2(punto, plano)


