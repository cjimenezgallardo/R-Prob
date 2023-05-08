

dataej <- datos_empleados

# X = Numero de sueldos por sobre los 60.000 por hora en una muestra de 100 trabajadores



# como se desconoce p haremos bootstrap para simular p y luego evaluar su ajuste



library(caret)
library(tidyverse)
library(fitdistrplus)
library(mosaic)
library(rsample)


options(scipen=999) # 1 para notacion cientifica

pe<- data.frame(0) # definimos un vector de numerocp



#creando variable nueva de Exito y fracaso

dataej<-mutate(dataej, corte=ifelse(Sueldo_actual>60000,1,0))


#Estimacion de p para funcion Binomial 

for (i in 1:15){

  datosindex=createDataPartition(dataej$ID,p=0.50)$Resample1 #0.1, porcentaje de elementos para el bootstrap
  d11<-as.data.frame(dataej[datosindex,] )

  t11 <- d11 %>% 
    group_by(corte) %>% 
    summarise(tot=n()) %>% 
    mutate(prop=tot/sum(tot))

  pe[i,1]<-t11[1,3]

}


p<- mean(pe$X0) # estimacion de p binomial
p


#SIMULACION


# definir  un contexto de experimentacion

n<- 100 # escogemos 100 personas

x<-c(0:100)

tablaPb1 <- data.frame(x, PrBx=dbinom(x,n,p),PrABx=pbinom(x,n,p))



mosaic::plotDist(dist="binom",size=100, prob=p,xlab=paste("x ", min(tablaPb1$x),";",max(tablaPb1$x)),kind="histogram")




#POR bootstraps

set.seed(354852)

calculos <- bootstraps(dataej,times=10)

for (j in 1:10) {

  fcb01 <- calculos$splits[[j]]

  ds111 <- as.data.frame(fcb01)

  tS1 <- ds111 %>% 
    group_by(corte) %>% 
    summarise(tot=n()) %>% 
    mutate(prop=tot/sum(tot))
}








