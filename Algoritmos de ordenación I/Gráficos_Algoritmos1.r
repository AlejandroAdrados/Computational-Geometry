# Datos
Elementos<-c(500,1000,2500,5000,7500,10000,25000,50000)
#Burbuja
Tiempo<-c(0.06,0.19,0.94,2.61,4.19,5.84,16.84,35.3)
#Inserci�n
y2<-c(0.03,0.04,0.2,0.75,1.68,2.95,18.68,75.32)
#Selecci�n
y3<-c(0.02,0.04,0.14,0.5,1.11,1.96,12.11,48.2)

# Gr�fico
plot(Elementos, Tiempo, type = "l", main="ALGORITMOS")
legend(x = "topleft",         # Posici�n
       legend = c("Burbuja", "Inserci�n","Selecci�n"), # Textos de la leyenda
       lty = c(1),          # Tipo de l�neas
       col = c(1, 2, 3),          # Colores de las l�neas
       lwd = 2)                # Ancho de las l�neas
lines(Elementos,y2, type ="l", col=2)
lines(Elementos,y3, type ="l", col=3)