##########################################################################
#                                                                        #
# Una aproximacion metodologica al uso de datos de encuestas en hogares  #
#                                                                        #
##########################################################################


# Todas las bases de datos de la ENOE pueden ser obtenidas en la pagina de
# internet del INEGI: www.inegi.org.mx

# Para llevar a cabo los ejercicios se utilizaran los datos del primer
# trimestre de 2016 y la tabla: SDEMT116.dbf

#0. Consideraciones iniciales

    #0.1  Cargar las librerias que se van a utilizar

          library(foreign)
          library(ggplot2)

    #0.2  Cargar la base de datos y filtrar los casos
          SDEMT215<-data.frame(read.dbf("C:/Users/JC/Desktop/D/1/sdemt116.dbf"))
          S<-SDEMT215[ which(SDEMT215$CLASE2==1), ]

    #0.3  Réplicas
          muestra1<-sample(S$HRSOCUP,size=length(S$HRSOCUP)*1000,replace=TRUE)

    #0.4  Cálculo del promedio para cada una de ellas
          
          muestra2 <- apply(matrix(muestra1,1000,length(S$HRSOCUP)),1,mean)

    #0.5  Gráficar los resultados
          
          p = ggplot( data.frame(muestra2),aes(muestra2) ) +
              geom_histogram(aes(y=..density..),binwidth=(diff(range(muestra2))/30)) +
              geom_density(color="red")+
              xlab("Promedio")+ylab("Frecuencia")
          plot(p)


#Referencia
          

http://www.stat.wisc.edu/~larget/stat302/chap3.pdf

