#EJERCICIO 2
#Implementar en R un algoritmo basado en la ecuación
#de la recta cuya entrada sean tres puntos distintos del
#plano (A, B y C) que determine:
A <- c(31, 14)
B <- c(0, -56)
C <- c(4,18)
x <- c()
y <- c()

#a) Si C está a la derecha o a la izquierda de la recta
#que une A con B orientada por el vector B-A.


apartadoA <- function(A, B, C)
{
  r <- function(x) {
    y = (((B[2] - A[2]) / (B[1] - A[1])) * (x - A[1])) + A[2]
  }
  x <- seq(min(A[1], B[1], C[1])-1, max(A[1], B[1], C[1])+1)
  plot(x, r(x), type = "l", ylim = c(min(A[2], B[2], C[2])-1, max(A[2], B[2], C[2]))+1)
  if (B[1] == A[1]) {
    abline(v = A[1])
  }
  text(A[1]-1,A[2]+1,"A")
  text(B[1]-1,B[2]+1,"B")
  text(C[1]-1,C[2]+1,"C")
  points(A[1], A[2])
  points(B[1], B[2])
  points(C[1], C[2])
  x <- C[1]
  y <- r(C)
  pendiente <- (B[2] - A[2]) / (B[1] - A[1])
  if (A[2] == B[2]) {
    s <- 'Recta horizontal, ni a derecha ni a izquierda'
  }
  else{
    if (((y[1] < C[2]) & (pendiente > 0)) |
        (y[1] > C[2]) & (pendiente < 0))
      s <- 'El punto está a la izquierda de la recta'
    else if (((y[1] > C[2]) & (pendiente > 0)) |
             (y[1] < C[2]) & (pendiente < 0))
      s <- 'El punto está a la derecha de la recta'
    if (y[1] == C[2])
      s <- 'El punto está en la recta'
  }
  
  s
}

#b) Si A, B y C hacen un giro a la derecha o no.
apartadoB <- function(A, B, C)
{
  giro <-
    (B[1] - A[1]) * (C[2] - A[2]) - (B[2] - A[2]) * (C[1] - A[1])
  if (giro < 0)
    s2 <- "Giro a la derecha"
  else if (giro > 0)
    s2 <- "Giro a la izquierda"
  else
    "No hay giro"
  s2
}


apartadoA(A, B, C)
apartadoB(A, B, C)