# Datos
Elementos<-c(200,400,600,800,1000,1200,1400,1600,1800,2000)
#Burbuja
Tiempo<-c(0.03,0.05,0.09,0.12,0.2,0.27,0.36,0.45,0.56,0.67)
#Inserci�n
y2<-c(0.02,0.02,0.03,0.03,0.05,0.06,0.08,0.1,0.11,0.14)
#Selecci�n
y3<-c(0.01,0.01,0.01,0.02,0.03,0.05,0.04,0.06,0.08,0.1)
#Quicksort
y4<-c(0.01,0.01,0.03,0.01,0.04,0.01,0.03,0.03,0.03,0.03)
#Heapsort
y5<-c(0.01,0.03,0.04,0.05,0.06,0.09,0.1,0.14,0.15,0.19)
#Mergesort
y6<-c(0.02,0.04,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03)

# Gr�fico
plot(Elementos, Tiempo, type = "l", main="ALGORITMOS")
legend(x = "topleft",         # Posici�n
       legend = c("Burbuja", "Inserci�n","Selecci�n","Quicksort","HeapSort","Mergesort"), # Textos de la leyenda
       lty = c(1),          # Tipo de l�neas
       col = c(1, 2, 3, 4, 5, 6),          # Colores de las l�neas
       lwd = 2)                # Ancho de las l�neas
lines(Elementos,y2, type ="l", col=2)
lines(Elementos,y3, type ="l", col=3)
lines(Elementos,y4, type ="l", col=4)
lines(Elementos,y5, type ="l", col=5)
lines(Elementos,y6, type ="l", col=6)