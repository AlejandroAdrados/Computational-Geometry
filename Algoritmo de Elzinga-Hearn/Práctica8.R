#Algoritmo de Elzinga y Hearn 1-centro

encimaDebajo <- function(A, B, C)
{
  r <- function(x) {
    y = (((B[2] - A[2]) / (B[1] - A[1])) * (x - A[1])) + A[2]
  }
  
  x <- C[1]
  y <- r(C)
  pendiente <- (B[2] - A[2]) / (B[1] - A[1])
  if (A[2] == B[2]) {
    s <- 'Recta horizontal, ni a derecha ni a izquierda'
  }else{
    if (((y[1] < C[2]) & (pendiente > 0)) |
        (y[1] > C[2]) & (pendiente < 0))
      s <- 1
    else if (((y[1] > C[2]) & (pendiente > 0)) |
             (y[1] < C[2]) & (pendiente < 0))
      s <- 2
    if (y[1] == C[2])
      s <- 3
  }
  
  s
}

distancia <- function(punto1, punto2) {
  sqrt((punto2[1] - punto1[1]) ^ 2 + (punto2[2] - punto1[2]) ^ 2)
}

paso2 <- function(puntos,punto1,punto2) {
  dimension <- dim(puntos)
  centro <-
    c((punto1[1] + punto2[1]) / 2, (punto1[2] + punto2[2]) / 2)
  diametro <- distancia(punto1, punto2)
  radio <- diametro / 2
  circulo <- function(x) {
    y = sqrt(radio ^ 2 - (x - centro[1]) ^ 2) + centro[2]
  }
  x <- seq(min(punto1[1], punto2[1]), max(punto1[1], punto2[1]))
  
  #PASO 3
  h <- 1
  cont <- 0
  for (h in 1:dimension[1]) {
    punto3 <- puntos[h, ]
    x<-(punto3[1] - centro[1]) ^ 2 
    y<-(punto3[2] - centro[2]) ^ 2
    r2<-radio^2+0.001
    izq<-x+y
    if ((izq<r2) | (izq==r2)){
      cont <- cont + 1
    } else {
      puntofuera <- punto3
    }
  }
  if (cont == dimension[1]) {
    circMinimo <- c(centro[1], centro[2], radio)
    t<-seq(0,2*pi,0.001)
    x<-sin(t)*radio+centro[1]
    y<-cos(t)*radio+centro[2]
    lines(x,y)
    circMinimo
  } else {
    paso4(punto1, punto2, puntofuera,puntos)
  }
}

paso4 <- function(punto1, punto2, punto3,puntos) {
  #Hcaemos un triángulo con punto1,punto2,punto3
  lado1 <- c(punto2[1] - punto1[1], punto2[2] - punto1[2])
  lado2 <- c(punto3[1] - punto1[1], punto3[2] - punto1[2])
  lado3 <- c(punto3[1] - punto2[1], punto3[2] - punto2[2])
  angulo12 <- acos((lado1 %*% lado2) /
                     (sqrt(lado1[1] ^ 2 + lado1[2] ^ 2) *
                        sqrt(lado2[1] ^ 2 + lado2[2] ^ 2)))
  angulo13 <- acos((-lado1 %*% lado3) /
                     (sqrt(lado1[1] ^ 2 + lado1[2] ^ 2) * 
                        sqrt(lado3[1] ^ 2 + lado3[2] ^ 2)))
  angulo23 <- acos((-lado2 %*% -lado3) /
                     (sqrt(lado2[1] ^ 2 + lado2[2] ^ 2) * 
                        sqrt(lado3[1] ^ 2 + lado3[2] ^ 2)))
  if (angulo12 >= pi / 2) {
    paso2(puntos,punto2, punto3)
  } else if (angulo13 >= pi / 2) {
    paso2(puntos,punto1, punto3)
  } else if (angulo23 >= pi / 2) {
    paso2(puntos,punto1, punto2)
  } else {
    #Hacemos el circulo con punto1, punto2, punto3
    A <- matrix(nrow = 3, ncol = 3)
    A[1,] <- c(punto1[1], punto1[2], 1)
    A[2,] <- c(punto2[1], punto2[2], 1)
    A[3,] <- c(punto3[1], punto3[2], 1)
    b <- matrix(nrow = 3, ncol = 1)
    b[1] <- -(punto1[1] ^ 2 + punto1[2] ^ 2)
    b[2] <- -(punto2[1] ^ 2 + punto2[2] ^ 2)
    b[3] <- -(punto3[1] ^ 2 + punto3[2] ^ 2)
    x <- solve(A, b)
    centro <- c()
    centro[1] <- x[1] / -2
    centro[2] <- x[2] / -2
    radio <- sqrt(centro[1] ^ 2 + centro[2] ^ 2 - x[3])
    #PASO 5
    dimension<-dim(puntos)
    h <- 1
    cont2 <- 0
    for (h in 1:dimension[1]) {
      punto4 <- puntos[h, ]
      x<-(punto4[1] - centro[1]) ^ 2 
      y<-(punto4[2] - centro[2]) ^ 2
      r2<-radio^2+0.001
      izq<-x+y
      if ((izq<r2) | (izq==r2)){
        cont2 <- cont2 + 1
      } else {
        puntofuera <- punto4
      }
    }
    if (cont2 == dimension[1]) {
      circMinimo <- c(centro[1], centro[2], radio)
      t<-seq(0,2*pi,0.001)
      x<-sin(t)*radio+centro[1]
      y<-cos(t)*radio+centro[2]
      lines(x,y)
      circMinimo
    } else{
      #PASO 6
      distancia1 <- distancia(punto1, puntofuera)
      distancia2 <- distancia(punto2, puntofuera)
      distancia3 <- distancia(punto3, puntofuera)
      if ((distancia1 >= distancia2) & (distancia1 >= distancia3)) {
        q <- punto1
      } else if ((distancia2 >= distancia1) &
                 (distancia2 >= distancia3)) {
        q <- punto2
      } else{
        q <- punto3
      }
      #PASO 7
      if (encimaDebajo(q, centro, puntofuera) == 1) {
        if (encimaDebajo(q, centro, punto1) == 2) {
          r <- punto1
        } else if (encimaDebajo(q, centro, punto2) == 2) {
          r <- punto2
        } else if (encimaDebajo(q, centro, punto3) == 2) {
          r <- punto3
        }
      } else if (encimaDebajo(q, centro, puntofuera) == 2) {
        if (encimaDebajo(q, centro, punto1) == 1) {
          r <- punto1
        } else if (encimaDebajo(q, centro, punto2) == 1) {
          r <- punto2
        } else if (encimaDebajo(q, centro, punto3) == 1) {
          r <- punto3
        }
      }
      #PASO 8
      paso4(q, r, puntofuera,puntos)
    }
  }
}

elzingaHearn <- function(puntos) {
  plot(
    puntos[, 1],
    puntos[, 2],
    xlim = c(-limite*2, limite*2),
    ylim = c(-limite*2, limite*2),
    type = "p",
    col = "green"
  )
  vResultado<-paso2(puntos,puntos[1,],puntos[2,])
  resultado<-rbind(vResultado)
  colnames(resultado)<-c("Coordenada x centro","Coordenada y centro", "Radio")
  points(resultado[1,1],resultado[1,2], cex=1, pch=3)
  resultado
}
#Ejemplo con 10 puntos de -5 a 5    
elementos<-10
limite <- 5
vec1<-runif(elementos,-limite,limite)
vec2<-runif(elementos,-limite,limite)
vec<-matrix(nrow=elementos,ncol=2)
vec<-cbind(vec1,vec2)
puntos<-vec

elzingaHearn(puntos)

#Ejemplo con 100 puntos de -50 a 50    
elementos<-100
limite <- 50
vec1<-runif(elementos,-limite,limite)
vec2<-runif(elementos,-limite,limite)
vec<-matrix(nrow=elementos,ncol=2)
vec<-cbind(vec1,vec2)
puntos<-vec

elzingaHearn(puntos)

#Ejemplo con 250 puntos de -100 a 100    
elementos<-250
limite <- 100
vec1<-runif(elementos,-limite,limite)
vec2<-runif(elementos,-limite,limite)
vec<-matrix(nrow=elementos,ncol=2)
vec<-cbind(vec1,vec2)
puntos<-vec

elzingaHearn(puntos)