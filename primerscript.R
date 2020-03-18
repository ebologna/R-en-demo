1+2
x<-5
class(x)
t<-"a"
class(t)
x+3
x+t
y<-"1"
class(y)
y<-as.numeric(y)
t<-as.numeric(t)

x<-c(2,5,4,8,7)
y<-c("a", "b", 2)
y
z<-5:9
z
y<-c(2,5,8:23)
z<-seq(5,8,.5)
z
20:255,8,
rep(4,3)
rep("perro",3)
y<-c(2,5,8:23, seq(5,7,.3), rep(2, 3))
y

grupo<-c(rep("experimental",20), rep("control", 20))
puntaje<-c(rnorm(20,25,5), rnorm(20,35,4))
puntaje<-round(puntaje,2)
puntajes.por.grupo<-cbind(grupo, puntaje)
class(puntajes.por.grupo)
puntajes.por.grupo<-as.data.frame(puntajes.por.grupo)
attributes(puntajes.por.grupo)
class(puntajes.por.grupo$grupo)
levels(puntajes.por.grupo$grupo)

write.table(puntajes.por.grupo, "basenueva.csv", sep = ";", row.names = FALSE)

eph.3.18<-read.table("usu_individual_T318.txt", sep = ";", header = TRUE)
class(eph.3.18)
names(eph.3.18)
eph.3.18$sexo<-as.factor(eph.3.18$CH04)
levels(eph.3.18$sexo)<-c("varon", "mujer")
levels(eph.3.18$sexo)
eph.3.18$ESTADO<-as.factor(eph.3.18$ESTADO)
levels(eph.3.18$ESTADO)<-c(NA, "ocupade", "desocupade", "inactive", NA)
levels(eph.3.18$ESTADO)

tabla.sexo<-table(eph.3.18$sexo)
addmargins(table(eph.3.18$sexo))
addmargins(table(eph.3.18$ESTADO))
round(100*prop.table(table(eph.3.18$sexo)), 2)
class(tabla.sexo)
prop.table(tabla.sexo)
attributes(tabla.sexo)
eph.3.18[25,15]
tabla.sexo[3]

100*prop.table(table(eph.3.18$ESTADO, eph.3.18$sexo), 2)
install.packages("questionr")
library(questionr)
?questionr
addmargins(wtd.table(eph.3.18$sexo, weights = eph.3.18$PONDERA))
addmargins(wtd.table(eph.3.18$ESTADO, eph.3.18$sexo, weights = eph.3.18$PONDERA))

tabla.sexo.estado<-table(eph.3.18$ESTADO, eph.3.18$sexo)
tabla.sexo.estado[6]

prueba.ji<-chisq.test(tabla.sexo.estado)
attributes(prueba.ji)

prueba.ji$p.value
eph.3.18$educacion<-as.factor(eph.3.18$NIVEL_ED)
eph.3.18$educacion<-factor(eph.3.18$educacion,
                           levels(eph.3.18$educacion)[c(7, 1:6)])
table(eph.3.18$educacion, eph.3.18$NIVEL_ED)

levels(eph.3.18$educacion)<-c("sin instruccion", "primaria incompleta",
                              "primaria completa", "secundaria incompleta", "secundaria completa",
                              "universitaria incompleta", "universitaria completa")

table(as.numeric(eph.3.18$educacion))

3<4
6<=7
5=4
5==4
x<-c(5,4,2,9,8, NA)
x<5
x==9

summary(eph.3.18$CH06)
eph.3.18$CH06[eph.3.18$CH06==-1]<-0

is.na(x)

summary(eph.3.18$PP08D1)

asalariados.con.ingreso.cordoba<-subset(eph.3.18, eph.3.18$PP08D1>0 &
                                          is.na(eph.3.18$PP08D1)==FALSE & eph.3.18$AGLOMERADO==13)

summary(asalariados.con.ingreso.cordoba$PP08D1)
mean(asalariados.con.ingreso.cordoba$PP08D1)
sd(asalariados.con.ingreso.cordoba$PP08D1)

round(100*sd(asalariados.con.ingreso.cordoba$PP08D1)/mean(asalariados.con.ingreso.cordoba$PP08D1), 2)


quantile(asalariados.con.ingreso.cordoba$PP08D1,c(0.20, .4,.6,.8))

asalariados.con.ingreso.cordoba$ingresos.4.iguales<-cut(asalariados.con.ingreso.cordoba$PP08D1,
                                                        breaks = 4, dig.lab = 10)
table(asalariados.con.ingreso.cordoba$ingresos.4.iguales)
?cut

asalariados.con.ingreso.cordoba$ingresos.4.prop<-cut(asalariados.con.ingreso.cordoba$PP08D1,
                                                     breaks = c(min(asalariados.con.ingreso.cordoba$PP08D1),
                                                                quantile(asalariados.con.ingreso.cordoba$PP08D1,.25),
                                                                median(asalariados.con.ingreso.cordoba$PP08D1),
                                                                quantile(asalariados.con.ingreso.cordoba$PP08D1, .75),
                                                                max(asalariados.con.ingreso.cordoba$PP08D1)),
                                                     dig.lab = 10)
table(asalariados.con.ingreso.cordoba$ingresos.4.prop)

asalariados.con.ingreso.cordoba$ingresos.4.teo<-cut(asalariados.con.ingreso.cordoba$PP08D1,
                                                    breaks = c(800, 10000, 15000, 20000, 70000),
                                                    dig.lab = 10)

100*prop.table(table(asalariados.con.ingreso.cordoba$ingresos.4.teo))
tabla.sexo.estado      
prueba.ji
n<-sum(tabla.sexo.estado)
c<-ncol(tabla.sexo.estado)
f<-nrow(tabla.sexo.estado)

sqrt(prueba.ji$statistic/(n*min(f-1, c-1)))

library(questionr)
cramer.v(tabla.sexo.estado)

cor(asalariados.con.ingreso.cordoba$PP08D1, as.numeric(asalariados.con.ingreso.cordoba$educacion),
    method = "spearman")

summary(asalariados.con.ingreso.cordoba$PP3E_TOT)

asalariados.con.ingreso.cordoba<-subset(asalariados.con.ingreso.cordoba, 
                                        asalariados.con.ingreso.cordoba$PP3E_TOT>0)

cor(asalariados.con.ingreso.cordoba$PP08D1, asalariados.con.ingreso.cordoba$PP3E_TOT)

modelo.1<-lm(asalariados.con.ingreso.cordoba$PP08D1~asalariados.con.ingreso.cordoba$PP3E_TOT)
attributes(modelo.1)
summary(modelo.1)

plot(asalariados.con.ingreso.cordoba$PP3E_TOT, asalariados.con.ingreso.cordoba$PP08D1,
     xlab = "horas trabajadas", ylab = "ingresos salariales",
     main = "Relacion horas trabajadas - ingreso")
abline(5271.47, 265.91, col="red")

install.packages("gmodels")
library(gmodels)
?ci
ci(asalariados.con.ingreso.cordoba$PP08D1, confidence=.99)

desocupado<-subset(eph.3.18, eph.3.18$AGLOMERADO==13)$ESTADO
table(desocupado)
desocupado<-as.numeric(desocupado)
desocupado<-desocupado[desocupado!=3]
desocupado[desocupado==1]<-0
desocupado[desocupado==2]<-1

100*ci.binom(desocupado, confidence = .9)
desocupado<-desocupado[is.na(desocupado)==FALSE]


?t.test
t.test(asalariados.con.ingreso.cordoba$PP08D1, mu=15000, alternative = "less")

t.test(asalariados.con.ingreso.cordoba$PP08D1~asalariados.con.ingreso.cordoba$sexo)
