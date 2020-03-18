file.choose()
eph.2.15=read.dbf("D:\\Documents\\SpiderOak Hive\\curso visualizacion\\para taller\\Individual_t215.dbf")
asalariados=subset(eph.2.15, eph.2.15$PP08D1>0 & eph.2.15$PP08D1<20000)
summary(eph.2.15$PP08D1)

summary(asalariados$PP08D1)
asalariados$sexo=as.factor(asalariados$CH04)
levels(asalariados$sexo)=c("varones","mujeres")
table(asalariados$sexo,asalariados$CH04)

(p1=ggplot(asalariados, aes(sexo))+geom_bar())


## sumar los rótulos:
p1 +xlab("Sexo")+ylab("cantidad de casos")
##al producir el gráfico, se generan internamente otras variables
## en este caso, una de ellas es el recuento de casos en las categorías de sexo
## se llama ..count.. (los dos puntos para no confundirla con alguna variable 
## de la base que se podría llamar igual)
## en lugar de cuenta, proporción:
(p2=ggplot(asalariados)+geom_bar(aes(x=sexo, y=..count../sum(..count..)))+
    xlab("Sexo")+ylab("proporción de casos"))

(p3=ggplot(asalariados)+geom_bar(aes(x=sexo, y=100*(..count../sum(..count..))))+
    xlab("Sexo")+ylab("proporción de casos")+
  scale_y_continuous(breaks=c(0, 25, 50)))

##barras rellenas
(p4=ggplot(asalariados, aes(sexo))+
  geom_bar(position="fill",aes(y=..count../sum(..count..)))+xlab("Sexo")+
  ylab("proporción de casos"))

##pintarlas por educacion
ggplot(asalariados, aes(sexo))+
  geom_bar(position="fill",aes(y=..count../sum(..count..),
                               fill=as.factor(NIVEL_ED)))+xlab("Sexo")+
  ylab("proporción de casos")


########## se arregla educacion:
#hacer factor
asalariados$educacion=as.factor(asalariados$NIVEL_ED)
##reordenar niveles
asalariados$educacion = factor(asalariados$educacion,
                               levels(asalariados$educacion)[c(7,1:6)])
##verificar
table(asalariados$educacion, asalariados$NIVEL_ED)
##etiquetar
levels(asalariados$educacion)=c("Sin instrucción","Primaria Incompleta","Primaria Completa","Secundaria Incompleta",
"Secundaria Completa", "Superior Universitaria Incompleta", "Superior Universitaria Completa")
##verificar de nuevo

(P2=ggplot(asalariados, aes(sexo))+
  geom_bar(position="fill", aes(y=..count../sum(..count..), fill=educacion))+
    xlab("SEXO") +ylab("Proporción de casos")+
    ggtitle("Nivel de educación según sexo")+theme_tufte())
  
##buscar "ggplot rellenar barras colores" y sale https://rpubs.com/Rortizdu/140190
##ahi explica
(p3=P2+scale_fill_manual(name="Educación", values = c("blue","skyblue","darkblue",
                                                 "black","green","red","yellow")))

##diversas paletas, por defecto la que termina en rosa
## brewer para escalas continuas
(P3=ggplot(asalariados, aes(sexo))+
  geom_bar(position="fill", aes(y=..count../sum(..count..), fill=educacion))+xlab("SEXO") +
  ylab("Proporción")+ggtitle("Nivel de educación según sexo")+
    scale_fill_brewer(name="Nivel de Educación",type = "seq", palette = 8)+
    theme_tufte())

##para más opciones se carga (o instala si hace falta) Rcolorbrewer
library(RColorBrewer)
display.brewer.all()
(P3=ggplot(asalariados, aes(sexo))+
    geom_bar(position="fill", aes(y=..count../sum(..count..), fill=educacion))+
    xlab("SEXO") +ylab("Proporción")+ggtitle("Nivel de educación según sexo")+
    scale_fill_brewer(name="Nivel de Educación",type = "seq",
                      palette = "Spectral")+theme_tufte())

############## histograma
(p4=ggplot(asalariados, aes(PP08D1))+geom_histogram())

##con proporcion, ajuste cantidad de intervalos
ggplot(asalariados, aes(PP08D1))+
  geom_histogram(bins=20, col="blue", fill="red",aes(y=..count../sum(..count..)))

##ajuste ancho intervalos
ggplot(asalariados, aes(PP08D1))+
  geom_histogram(binwidth=1000, col="blue", fill="red",aes(y=..count../sum(..count..)))


##solo densidad
ggplot(asalariados, aes(PP08D1))+geom_density()

##histograma y densidad
(p5=ggplot(asalariados, aes(PP08D1))+
  geom_histogram(bins=20, col="blue", fill="green",aes(y=..density..))+
  geom_density())

##con lineas para media y mediana
p5+geom_vline(aes(xintercept=mean(PP08D1)),color="red")+
  geom_vline(aes(xintercept=median(PP08D1)),color="blue")

##por sexos y elegir colores
(p6=ggplot(asalariados, aes(PP08D1))+
    geom_histogram(bins=40, aes(y=..count../sum(..count..), fill=sexo))+
    scale_fill_manual(name="Sexo", values = c("skyblue","pink")))

##separa barras
(p7=ggplot(asalariados, aes(PP08D1))+
  geom_histogram(bins=40, aes(y=..count../sum(..count..), fill=sexo),
                              position = "dodge")+
    scale_fill_manual(name="Sexo", values = c("skyblue","pink")))

##titulos
p7+xlab("ingresos salariales")+ylab("proporción de casos")+theme_tufte()

##define ancho de intervalo
(p8=ggplot(asalariados, aes(PP08D1))+
    geom_histogram(binwidth = 2000, aes(y=..count../sum(..count..), fill=sexo),
                   position = "dodge")+
    scale_fill_manual(name="Sexo",values = c("skyblue","pink")))+
  xlab("ingresos salariales")+ylab("proporción de casos")+
  ggtitle("Distribución de ingresos salariales según sexo")+
  theme_tufte()


##para lineas separadas hay que calcularlas por grupo
library(Rmisc)
por.sexos=summarySE(asalariados, groupvars = "sexo", measurevar = "PP08D1")
class(por.sexos)
por.sexos

##con lineas para media y mediana
p8+ geom_vline(data=por.sexos, aes(xintercept=PP08D1, colour=sexo),
               show.legend = FALSE, size=1)

### boxplot
ggplot(asalariados, aes(sexo, PP08D1))+geom_boxplot()

(p9=ggplot(asalariados, aes(sexo, PP08D1))+geom_boxplot(aes(fill=sexo)))

##para que muestre la media, hay que agrear una "estadística" de y (la media) e
##indicar como representarla (con un punto)
p9+stat_summary(fun.y=mean, geom="point")  

##ajustes de aspecto
p9+stat_summary(fun.y=mean, geom="point", size=4, color="green")+
  ylab("ingresos salariales")+theme_tufte()  
