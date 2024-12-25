#Algoritmo Burbuja
burbuja = function(vec)
{
  vecAux = vec
  for(i in seq(1, length(vecAux)-1))
  {
    #Si un numero es mayor que el de la siguiente posición, se intercambian sus posiciones.
    if(vecAux[i] > vecAux[i+1]) {
      aux = vecAux[i+1]
      vecAux[i+1]=vecAux[i]
      vecAux[i]=aux
    }
  }
  
  if(isTRUE(all.equal(vec, vecAux)))
  {
    return(vecAux)
  }
  else {
    return(burbuja(vecAux))
  }
}

#Algoritmo Inserción
insercion = function(vec)
{
  for (i in seq(1, length(vec)))
  {
    actual = vec[i];
    j = i-1;
    #Desplazamiento de los elementos de la matriz
    while ((j>0) && (vec[j]>actual))
    {
      vec[j+1]=vec[j];
      j = j-1;
    }
    #Insertar el elemento en su lugar
    vec[j+1]=actual;
  }
  return(vec);
}

#Algoritmo Selección
seleccion = function(vec)
{
  for (i in 1:(length(vec)-1))
  {
    minimo = i
    temp = vec[i]
    for(j in (i:length(vec)))
    {
      if(vec[j]<temp) {
        minimo = j
        temp = vec[j]
      }
    }
    vec[minimo] = vec[i]
    vec[i] = temp
  }
  return(vec)
}

#Vector de números aleatorios y llamada a los algoritmos
#Variable auxiliar
aux<-0
#Para cambiar el número de elementos se debe modificar la siguiente variable
elementos<-25000
vec<-runif(elementos,0,1000)

#Cálculo de los tiempos que tarda cada uno de los algoritmos
tiempoBurbuja<-proc.time()
burbuja(vec)
proc.time()-tiempoBurbuja

tiempoInser<-proc.time()
insercion(vec) 
proc.time()-tiempoInser

tiempoSelec<-proc.time()
seleccion(vec)
proc.time()-tiempoSelec

