#Implementar en R el Algoritmo Scan de Graham.de envolventes convexas.

quicksort <- function(x)
{
  #Caso base
  if (length(x) <= 1) {
    return(x)
  }
  #Inicializamos un array que guardará todos los valores menos la mediana
  resto <- c()
  #Elegimos la mediana del vector
  mediana <- length(x) %/% 2
  #Se elige el pivote
  pivote <- x[mediana]
  #Bucle que inserta en resto hasta la posición mediana-1
  for (i in 1:mediana - 1) {
    resto[i] <- x[i]
  }
  longitud <- length(x)
  siguiente_mediana <- mediana + 1
  #Bucle que inserta en resto desde la posición mediana+1
  for (i in siguiente_mediana:longitud) {
    resto[i - 1] <- x[i]
  }
  #Recursividad sobre el vector resto:
  #Parte izquierda del árbol (menores)
  L1 <- quicksort(resto[resto < pivote])
  #Parte derecha del árbol (mayores)
  L3 <- quicksort(resto[resto >= pivote])
  #Se devuelve la concatenación de parte izquierda-pivote-parte derecha
  return(c(L1, pivote, L3))
}

elementos <- 100
limite <- 50
vec1 <- runif(elementos, -limite, limite)
vec2 <- runif(elementos, -limite, limite)
vec <- matrix(nrow = elementos, ncol = 2)
vec <- cbind(vec1, vec2)

puntos <- vec
scanGraham <- function(puntos) {
  colnames(puntos) <- c("x", "y")
  plot(
    puntos[, 1],
    puntos[, 2],
    xlim = c(-limite, limite),
    ylim = c(-limite, limite),
    type = "p",
    col = "green"
  )
  
  i <- 1
  aux <- c(0, 0)
  dimension <- dim(puntos)
  for (i in 1:dimension[1]) {
    if (puntos[i, 2] < puntos[1, 2]) {
      aux <- puntos[i, ]
      puntos[i, ] <- puntos[1, ]
      puntos[1, ] <- aux
    }
  }
  puntos[1, ]
  
  #Recta abscisas
  horizontal <- c(1, 0)
  j <- 2
  matriz <- matrix(nrow = dimension[1], ncol = 1)
  matriz[1] = 0
  for (j in 2:dimension[1]) {
    recta <- puntos[1, ] - puntos[j, ]
    matriz[j] <-
      acos((recta %*% horizontal) / sqrt((recta[1] ^ 2) + (recta[2] ^ 2)) *
             sqrt((horizontal[1] ^ 2) + (horizontal[2] ^ 2)))
  }
  puntos <- cbind(puntos, matriz)
  angOrdenados <- quicksort(puntos[, 3])
  
  h <- 1
  k <- 1
  dimension2 <- dim(t(angOrdenados))
  for (h in 1:dimension2[2]) {
    while (angOrdenados[h] != puntos[k, 3]) {
      k = k + 1
    }
    aux <- puntos[h, ]
    puntos[h, 1] <- puntos[k, 1]
    puntos[h, 2] <- puntos[k, 2]
    puntos[h, 3] <- puntos[k, 3]
    puntos[k, ] <- aux
    k <- 2
  }
  
  puntosOrdenados <- cbind(puntos[, 1], puntos[, 2])
  
  l <- 2
  cont <- 2
  puntosEnvolventeConvexa <- matrix(nrow = dimension[1], ncol = 2)
  puntosEnvolventeConvexa[1, ] <- puntosOrdenados[1, ]
  dimensionActual <- dim(puntosOrdenados)
  while (l <= dimensionActual[1]) {
    dimensionActual <- dim(puntosOrdenados)
    puntosAntes <- puntosOrdenados[l - 1, ]
    puntosActual <- puntosOrdenados[l, ]
    if ((l + 1) > dimensionActual[1]) {
      puntosDespues <- puntosOrdenados[1, ]
    } else {
      puntosDespues <- puntosOrdenados[(l + 1), ]
    }
    giro <-
      (puntosActual[1] - puntosAntes[1]) * (puntosDespues[2] - puntosAntes[2]) -
      (puntosActual[2] - puntosAntes[2]) * (puntosDespues[1] - puntosAntes[1])
    if (giro < 0) {
      puntosEnvolventeConvexa[cont, ] <- puntosActual
      cont = cont + 1
    } else {
      puntosOrdenados <- puntosOrdenados[-l, ]
      l = l - 2
      
      z <- 1
      while (z < cont - 1) {
        if ((puntosActual[1] == puntosEnvolventeConvexa[z, 1]) &
            (puntosActual[2] == puntosEnvolventeConvexa[z, 2])) {
          puntosEnvolventeConvexa <- puntosEnvolventeConvexa[-z, ]
          cont = cont - 1
        } else {
          z = z + 1
        }
      }
      
      if ((puntosActual[1] == puntosEnvolventeConvexa[z, 1]) &
          (puntosActual[2] == puntosEnvolventeConvexa[z, 2])) {
        puntosEnvolventeConvexa <- puntosEnvolventeConvexa[-z, ]
        cont = cont - 1
      }
      
    }
    l = l + 1
    
  }
  
  puntosEnvolventeConvexa <-
    puntosEnvolventeConvexa[!rowSums(!is.finite(puntosEnvolventeConvexa)), ]
  
  n <- 1
  dimension3 <- dim(puntosEnvolventeConvexa)
  for (n in 1:dimension3[1]) {
    if (n + 1 > dimension3[1]) {
      segments(
        x0 = puntosEnvolventeConvexa[n, 1],
        y0 = puntosEnvolventeConvexa[n, 2],
        x1 = puntosEnvolventeConvexa[1, 1],
        y1 = puntosEnvolventeConvexa[1, 2],
        col = "darkgreen"
      )
    } else {
      segments(
        x0 = puntosEnvolventeConvexa[n, 1],
        y0 = puntosEnvolventeConvexa[n, 2],
        x1 = puntosEnvolventeConvexa[n + 1, 1],
        y1 = puntosEnvolventeConvexa[n + 1, 2],
        col = "darkgreen"
      )
    }
  }
}

scanGraham(puntos)
