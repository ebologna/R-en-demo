file.choose()
eph215=read.spss("C:\\Documents and Settings\\Usuario\\Escritorio\\t215_sav\\Individual_t215.sav")
eph215=as.data.frame(eph215)
class(eph215$ch04)
summary(eph215.1$p47t)

eph215.1=subset(eph215,eph215$p47t>0 & p47t<20000)
uno=ggplot(eph215.1, aes(p47t))
uno+geom_histogram()

dos=uno+geom_histogram(aes(fill=ch04))
dos+coord_flip()
dos+scale_x_continuous(limits = c(200,20000))

tres=ggplot(eph215.1, aes(x=ch04, y=p47t))
tres+geom_boxplot()
tres+geom_boxplot(aes(fill=ch04))

eph215.1$sexo=eph215.1$ch04
levels(eph215.1$sexo)=c("varones", "mujeres")
eph215.1$ingreso=eph215.1$p47t

tres=ggplot(eph215.1, aes(x=sexo, y=ingreso))
tres+geom_boxplot()
tres+geom_boxplot(aes(fill=sexo))

###
cuatro=ggplot(eph215.1, aes(nivel_ed,ingreso))
cuatro+stat_summary(fun.y = mean,
                   fun.ymin=function(x) mean(x)-sd(x),
                   fun.ymax=function(x) mean(x)+sd(x),
                   geom="pointrange")

cuatro+stat_summary(fun.y = mean,
                    fun.ymin=function(x) mean(x)-sd(x)/sqrt(length(eph215.1$ingreso)),
                    fun.ymax=function(x) mean(x)+sd(x)/sqrt(length(eph215.1$ingreso)),
                    geom="pointrange")
##falla el orden de las categorias
eph215.1$educacion=as.character(eph215.1$nivel_ed)##la defino como caracter
table(eph215.1$educacion)##ahora el orden es alfabetico
eph215.1$educacion=factor(eph215.1$educacion, levels=c("Sin instrucciÃ³n",
                                                       "Primaria Incompleta (incluye educaciÃ³n especial)",
                                                       "Primaria Completa","Secundaria Incompleta",
                                                       "Secundaria Completa","Superior Universitaria Incompleta",
                                                       "Superior Universitaria Completa"))
##con esto volimos  a factor, en el orden que quiero, a ver
cinco=ggplot(eph215.1, aes(educacion,ingreso))
cinco+stat_summary(fun.y = mean,
                    fun.ymin=function(x) mean(x)-sd(x),
                    fun.ymax=function(x) mean(x)+sd(x),
                    geom="pointrange")##great

seis=cinco+stat_summary(fun.y = mean,
                   fun.ymin=function(x) mean(x),
                   fun.ymax=function(x) mean(x),
                   geom="pointrange")

siete=seis+facet_grid(ch15~.)
siete
ocho=siete+
  theme_tufte()+
  theme(axis.text.x=element_text(angle = 60, vjust=1, size = 7,
                                 hjust =1 ))
ocho
levels(eph215.1$aglomerado)
cba=subset(eph215.1,aglomerado=="Gran CÃ³rdoba")

nueve=ggplot(cba, aes(educacion,ingreso))
nueve+stat_summary(fun.y = mean,
                   fun.ymin=function(x) mean(x)-sd(x),
                   fun.ymax=function(x) mean(x)+sd(x),
                   geom="pointrange")##great

diez=nueve+stat_summary(fun.y = mean,
                        fun.ymin=function(x) mean(x),
                        fun.ymax=function(x) mean(x),
                        geom="pointrange")

diez
once=diez+facet_grid(sexo~.)

once+
  theme_tufte()+
  theme(axis.text.x=element_text(angle = 60, vjust=1, size = 7,
                                 hjust =1 ))
##############3
aggregate(cba$ingreso,list(cba$educacion,cba$sexo),mean)##anda bien con dos criterios


##prueba desde formato dbf:
file.choose()
eph215.desdedbf=read.dbf("E:\\Documents and Settings\\parttimeucc\\Escritorio\\Individual_t215.dbf")
names(eph215.desdedbf)
class(eph215.desdedbf$NIVEL_ED)
summary(eph215.desdedbf$NIVEL_ED)
eph215.desdedbf$educación=factor(eph215.desdedbf$NIVEL_ED,
                                 levels=c(7,1,2,3,4,5,6))
table(eph215.desdedbf$educación)
levels(eph215.desdedbf$educación)=c("no fue", "priinc", "pricom", "secinc",
                                    "seccom", "uniinc","unicomp")
###asi anda, pero es igual que desde spss