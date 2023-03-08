

library(psych)
library (EnvStats)
library(fitdistrplus)
library(nortest)
library(ggplot2)
library(caret)

#analisis distribucion de probabilidad

#como elemento complementario se recomienda que el dataframe tenga un nombre corto
#entonces copiamos los datos en un nuevo dataframe d1

#d1<-NOMBRE EXTREMADAMENTE LARGO DE UN ARCHIVO.XLSX

d0<-Dry_Bean_Dataset

#d1<-data.frame(d01)
#luego tambien se pueden cambiar los nombres de las variables si no son "comodas"
#names(d1)[2]=c("y")
#el valor [2] depende de la columna que quieres cambiar nombre

names(d0)[2]=c("y")


#  muestrear para data set Grandes.. superior a 5000 datos

datosindex=createDataPartition(d0$y,p=0.20)$Resample1 #0.1, porcentaje de elementos para el bootstrap
d1<-as.data.frame(d0[datosindex,c(1)])

descripY(d1,d1$y)


# lista de Ditribuciones  
# weibull, gamma, logis, lnorm, norm, cauchy, beta, chi, chisq, exp, f, pareto, t, unif
# binom, geom, hyper, nbinom, 


#EVALUACION del comportamiento.
f1<-fitdist(d1$y, "weibull")
f2<-fitdist(d1$y, "gamma")
f3<-fitdist(d1$y, "logis")
f4<-fitdist(d1$y,"lnorm")
par(mfrow=c(2,2))
plot.legend<-c("Weibull","Gamma","Normal","Lnorm")
denscomp(list(f1,f2,f3,f4), legendtext=plot.legend)
qqcomp(list(f1,f2,f3,f4), legendtext=plot.legend)
cdfcomp(list(f1,f2,f3,f4), legendtext=plot.legend)
ppcomp(list(f1,f2,f3,f4), legendtext=plot.legend)
par(mfrow=c(1,1))

#confirmacion, quedese con aquel indicador menor (Akaike)
gofstat(list(f1,f2,f3,f4))

S#de acuerdo a lo anterior se observa un comportamiento ....
#evaluar con la prueba de bondad de ajuste 


ks.test(d1$y,"pweibull",shape=f1[[1]][1],scale=f1[[1]][2])
ks.test(d1$y,"pgamma",shape=f2[[1]][1],rate=f2[[1]][2])
ks.test(d1$y,"plogis",f3[[1]][1],f3[[1]][2])
ks.test(d1$y,"plnorm",f4[[1]][1],f4[[1]][2])

#test para prueba de normalidad

shapiro.test(d1$y)
ad.test(d1$y)
cvm.test(d1$y)
lillie.test(d1$y)
pearson.test(d1$y)
sf.test(d1$y)

