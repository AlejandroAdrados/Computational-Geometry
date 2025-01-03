library (psych)
library (GPArotation)

M<-read.table("D:/UNI/3�_2do Cuatri/Pr�cticas_GeomComp/Practica6/Asignaturas.txt",header=T,sep="")

# Test de esfericidad de Bartlett
bartlett.test(M)

# Analisis de las *principales componentes sin rotaci�n*

Modelo1 <- princomp (M , cor = TRUE )
# Varianza de cada factor
summary ( Modelo1 )
# Puntuaciones factoriales
loadings ( Modelo1)
# Grafico de sedimentaci�n
plot (Modelo1, type ="barplot")


# An�lisis de las *principales componentes con rotaci�n varimax*

Modelo2 <- principal(M, nfactors=2, rotate="varimax")
# Varianza de cada factor
summary(Modelo2)
# Puntuaciones factoriales
loadings(Modelo2)
# Puntuaciones de los casos
Modelo2 $scores
biplot(Modelo2)


#An�lisis *factanal con rotaci�n varimax*

Modelo3<- factanal(M, 3, rotation="varimax")
#Cargas y Unicidad
print(Modelo3, digits=2, cutoff=.3, sort=TRUE)
# Plot del factor 1 con respecto al factor 2
load <- Modelo3$loadings[,1:2]
plot(load,type="n")
# A�adir el nombre de las componentes
text(load,labels=names(M),cex=.7) 



