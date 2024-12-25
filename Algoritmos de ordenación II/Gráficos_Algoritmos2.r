# Datos
Elementos<-c(500,1000,2500,5000,7500,10000,25000,50000)
#Burbuja
Tiempo<-c(0.06,0.19,0.94,2.61,4.19,5.84,16.84,35.3)
#Inserción
y2<-c(0.03,0.04,0.2,0.75,1.68,2.95,18.68,75.32)
#Selección
y3<-c(0.02,0.04,0.14,0.5,1.11,1.96,12.11,48.2)
#Quicksort
y4<-c(0.02,0.02,0.04,0.06,0.08,0.1,0.26,0.5)
#Heapsort
y5<-c(0.03,0.16,0.25,0.72,1.41,2.37,14.69,48.7)
#Mergesort
y6<-c(0.03,0.03,0.05,0.08,0.1,0.13,0.35,0.52)
  
# Gráfico
plot(Elementos, Tiempo, type = "l", main="ALGORITMOS")
legend(x = "topleft",         # Posición
       legend = c("Burbuja", "Inserción","Selección","Quicksort","HeapSort","Mergesort"), # Textos de la leyenda
       lty = c(1),          # Tipo de líneas
       col = c(1, 2, 3, 4, 5, 6),          # Colores de las líneas
       lwd = 2)                # Ancho de las líneas
lines(Elementos,y2, type ="l", col=2)
lines(Elementos,y3, type ="l", col=3)
lines(Elementos,y4, type ="l", col=4)
lines(Elementos,y5, type ="l", col=5)
lines(Elementos,y6, type ="l", col=6)