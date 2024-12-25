quicksort<-function(x)
{
  #Caso base
  if(length(x)<=1){
    return(x)
  }
  #Inicializamos un array que guardará todos los valores menos la mediana
  resto<-c()
  #Elegimos la mediana del vector
  mediana<-length(x)%/%2
  #Se elige el pivote
  pivote<-x[mediana]
  #Bucle que inserta en resto hasta la posición mediana-1
  for(i in 1:mediana-1){
    resto[i]<-x[i]
  }
  longitud<-length(x)
  siguiente_mediana<-mediana+1
  #Bucle que inserta en resto desde la posición mediana+1
  for(i in siguiente_mediana:longitud){
    resto[i-1]<-x[i]
  }
  #Recursividad sobre el vector resto:
    #Parte izquierda del Árbol (menores)
    L1<-quicksort(resto[resto<pivote])
    #Parte derecha del Árbol (mayores)
    L3<-quicksort(resto[resto>=pivote])
  #Se devuelve la concatenación de parte izquierda-pivote-parte derecha
  return(c(L1,pivote,L3))
}

#--------------------------------------------------------------

heapify<-function(vec, n, i)
{
  #Seleccionamos al padre y a los dos hijos
  padre<-i
  hijo_izq<-2*(i-1) + 1
  hijo_der<-2*(i-1) + 2
  if ((hijo_izq < n) & (vec[padre] < vec[hijo_izq]))
  {
    padre<-hijo_izq
  }
  if ((hijo_der < n) & (vec[padre] < vec[hijo_der]))
  {
    padre<-hijo_der
  }
  if (padre != i) {
    vec<-replace(vec, c(i, padre), vec[c(padre, i)])
    vec<-heapify(vec, n, padre)
  }
  vec
}

heapsort<-function(vec)
{
  n<-length(vec)
  #Seleccionamos la mediana
  mediana<-n%/%2
  for (i in mediana:1) {
    vec<-heapify(vec, n, i)
  }
  for (i in n:1) {
    vec<-replace(vec, c(i, 1), vec[c(1, i)])
    vec<-heapify(vec, i, 1)
  }
  vec
}

#--------------------------------------------------------------

merge <- function(izq, der) {
  resultado <- numeric(length(izq) + length(der))
  i <- 1 # Contador de la parte izqierda
  j <- 1 # Contador de la parte derecha
  r <- 1 # Contador del resultado
  for(r in 1 : length(resultado)) {
    if((i <= length(izq) && izq[i] < der[j]) || j > length(der)) {
      resultado[r] <- izq[i]
      i <- i + 1
    } else {
      resultado[r] <- der[j]
      j <- j + 1
    }
  }
  resultado
}

mergesort <- function(l) {
  if(length(l) > 1) {
    mediana <- ceiling(length(l) / 2)
    izq <- mergesort(l[1 : mediana])
    der <- mergesort(l[(mediana + 1) : length(l)])
    merge(izq, der)
  } else {
    l
  }
}

#Vector de números aleatorios y llamada a los algoritmos
#Variable auxiliar
aux<-0
#Para cambiar el número de elementos se debe modificar la siguiente variable
elementos<-500
vec<-runif(elementos,0,1000)

#Cálculo de los tiempos que tarda cada uno de los algoritmos
tiempoQuick<-proc.time()
quicksort(vec)
proc.time()-tiempoQuick

tiempoHeap<-proc.time()
heapsort(vec)
proc.time()-tiempoHeap

tiempoMerge<-proc.time()
mergesort(vec)
proc.time()-tiempoMerge