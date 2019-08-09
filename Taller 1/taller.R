library(mclust)
library(psych)
library(modeest)
library("rgl")

setwd("G:/Mi unidad/Universidad/Semestre 11/TMDA/Taller I")
DB <- read.table("wpbc.data", header=FALSE, sep=",",
                 col.names=c("id","class","time","radiusMean","textureMean","perimeterMean","areaMean","smoothnessMean","compactnessMean","concavityMean","concaveMean","simetryMean","fractalMean",
                             "radiusSE","textureSE","perimeterSE","areaSE","smoothnessSE","compactnessSE","concavitySE","concaveSE","simetrySE","fractalSE",
                             "radiusWorst","textureWorst","perimeterWorst","areaWorst","smoothnessWorst","compactnessWorst","concavityWorst","concaveWorst","simetryWorst","fractalWorst",
                             "tumorSize","lymph"))

# Preprocesamiento
DB<-DB[,-1] # Eliminacion de columna id

temp<-which(DB[,34]=="?") # Eliminación de las instancias con valores perdidos
DB<-DB[-temp,]

#conversión de caracter a numero
DB[,34]<-as.numeric(as.character(DB[,34]))

# conversion de clase como caracter a numero
DB$class = ifelse(DB$class == "N", 1, DB$class)

######################################
# Análisis estadístico

#Funcion para calcular el coef de variacion
CoefVar<-function(x){
  cv<-0
  mean<-0
  sd<-0
  {
    mean<-sapply(x,mean,na.rm=TRUE)
    sd<-sapply(x,sd,na.rm=TRUE)
    cv<-sd/mean
  }
  return(cv)
}

#Función para obtener la correlacion de Pearson
PearsonCorrelation<-function(data){
  correlationFactor<-0
  {
    correlationFactor<-cor(data,use="complete.obs",method="pearson")
  }
  return(correlationFactor)
}

# Eliminación de la clase

mean<-sapply(DB[,-1],mean,na.rm=TRUE)
sd<-sapply(DB[,-1],sd,na.rm=TRUE)
var<-sapply(DB[,-1],var,na.rm=TRUE)
min<-sapply(DB[,-1],min,na.rm=TRUE)
max<-sapply(DB[,-1],max,na.rm=TRUE)
coefVar<-CoefVar(DB[,-1])

statsb<-matrix(c(mean,sd,var,min,max,coefVar),nrow=33,ncol=6)
dimnames(statsb) = list(c("time","radiusMean","textureMean","perimeterMean","areaMean","smoothnessMean","compactnessMean","concavityMean","concaveMean","simetryMean","fractalMean",
                          "radiusSE","textureSE","perimeterSE","areaSE","smoothnessSE","compactnessSE","concavitySE","concaveSE","simetrySE","fractalSE",
                          "radiusWorst","textureWorst","perimeterWorst","areaWorst","smoothnessWorst","compactnessWorst","concavityWorst","concaveWorst","simetryWorst","fractalWorst",
                          "tumorSize","lymph"),c("Media","Desviacion","Varianza","Minimo","Maximo","Coef. Var."))

#calculo de la correlacion entre las variables
cor<-PearsonCorrelation(DB[,-1])
cor(DB[,-1]) 

#test de normalidad

shapiro_test = data.frame(Variable = character(), p_value=numeric(), normal = numeric())

for (i in 1:33)
{
  p = shapiro.test(DB[2:34][,i])
  shapiro_test = rbind(shapiro_test, data.frame(Variable = names(DB[,2:34][i]), p_value = as.numeric(p[2]), normal = ifelse(as.numeric(p[2])>=0.05,1,0)))
}

# El coeficiente de correlación de Spearman se utiliza cuando los datos no siguen una distribución 
# normal para comprobar si dos variables cuantitativas tienen una relación lineal entre sí, 
# es decir si varían de forma simultánea.

spearman_test = data.frame(Variable = character(), p_value=numeric(), rho_value=numeric(), si1=numeric(), si2=numeric())

for (i in 1:33)
{
  rho = cor.test(DB$class,DB[2:34][,i], method="spearman")
  spearman_test = rbind(spearman_test, data.frame(Variable = names(DB[,2:34][i]), p_value=as.numeric(rho[3]), rho_value = as.numeric(rho[4]), si1=ifelse(as.numeric(rho[4])>0.1 || as.numeric(rho[4])<(-0.1),1,0), si2=ifelse(as.numeric(rho[3])<0.05,1,0)))
}


# Componentes Principales
cp<-princomp(DB[,-1], cor=TRUE)
summary(cp,loadings=TRUE)
# Importance of components:
#                         Comp.1    Comp.2    Comp.3    Comp.4     Comp.5    
#Standard deviation     3.1278724 2.8908708 1.8325520 1.5277373 1.24477347 
#Proportion of Variance 0.2964723 0.2532465 0.1017651 0.0707267 0.04695336
#Cumulative Proportion  0.2964723 0.5497188 0.6514838 0.7222105 0.76916391

plot(cp)

colors <- c("black", "gray", "White")
barplot(t(cp[[2]][,1:3]), beside = TRUE, ylim = c(-1, 1), col = colors, xlab = "Variables", ylab = "Covarianza", main = "Covarianza CP y variables", las = 2)
legend("bottomright", legend = c("CP 1", "CP 2", "CP 3"), fill = colors)

#                   Comp.1       Comp.2      Comp.3        Comp.4       Comp.5
#2  time              0.06384385  0.121551746 -0.06128376  0.2330411748 -0.096310854 * 
#3  radiusMean       -0.21675815 -0.229121608 -0.08808412 -0.0075682138  0.017757121 * +
#4  textureMean      -0.01875639 -0.062615250  0.06517101 -0.5631178461 -0.211958066 *
#5  perimeterMean    -0.23258537 -0.211172026 -0.08828111 -0.0134135632  0.023746786 * +
#6  areaMean         -0.21988602 -0.228713842 -0.09332340 -0.0092458530  0.018466144 * +
#7  smoothnessMean   -0.13531793  0.220123833 -0.01411817  0.0644823355  0.101072357 * +
#8  compactnessMean  -0.23937413  0.194591112 -0.06646116 -0.0161040038  0.010597812 * +
#9  concavityMean    -0.28364520  0.086362027 -0.04197176 -0.0386304844  0.060097389 * +
#10 concaveMean      -0.28969556 -0.002815002 -0.07319797  0.0265022678  0.053601675 * +
#11 simetryMean      -0.14553049  0.194462251 -0.03712895  0.1565756837 -0.178162993
#12 fractalMean      -0.08818317  0.298171300 -0.02228670  0.0206965719  0.023810776 * +
#13 radiusSE         -0.22301348 -0.143635928  0.16376894  0.1165982189 -0.070764242 * +
#14 textureSE        -0.07576367 -0.016750102  0.35835719 -0.2679510917 -0.076148982 * +
#15 perimeterSE      -0.23312948 -0.127408440  0.17616321  0.0905087812 -0.061395717 * +
#16 areaSE           -0.23203486 -0.171924778  0.08398650  0.0909865373 -0.068030297 * +
#17 smoothnessSE     -0.08794471  0.082147515  0.37382073  0.0036248200  0.101164085 * +
#18 compactnessSE    -0.18354156  0.185553425  0.17589456 -0.0958144283  0.022720235
#19 concavitySE      -0.20326534  0.130220693  0.25018646 -0.0686329569  0.075590454 * +
#20 concaveSE        -0.17794582  0.029753159  0.35784629  0.0771588538  0.049031426 * +
#21 simetrySE        -0.12064976  0.105688538  0.22654770  0.1575336378 -0.193180316 * +
#22 fractalSE        -0.15994668  0.203489600  0.20979890 -0.0288846685  0.080402463 * +
#23 radiusWorst      -0.21844878 -0.214632743 -0.17548328  0.0002297485 -0.049759692 * +
#24 textureWorst      0.01711022 -0.004801236 -0.03081011 -0.5986593231 -0.236243212 *
#25 perimeterWorst   -0.23730866 -0.193033281 -0.17073668 -0.0111785173 -0.046926657 * +
#26 areaWorst        -0.21285718 -0.213252052 -0.17819731 -0.0024454747 -0.048961477 * +
#27 smoothnessWorst  -0.03895998  0.240170088 -0.14632141 -0.0492368467  0.037742763 * +
#28 compactnessWorst -0.12519300  0.245715379 -0.19478005 -0.1327614769 -0.008118300 * +
#29 concavityWorst   -0.17318493  0.202533789 -0.18925686 -0.1459134518  0.059826242 * +
#30 concaveWorst     -0.23757279  0.108440952 -0.22261788 -0.0014873063 -0.006352856 * +
#31 simetryWorst     -0.05219327  0.212035133 -0.18900172  0.1087118773 -0.250980948 * +
#32 fractalWorst     -0.05184197  0.288945409 -0.16865415 -0.0852987745  0.036270643 * +
#33 tumorSize        -0.01342308 -0.069397124 -0.04435062 -0.0955930805  0.581691535 * 
#34 lymph            -0.01172245 -0.012807720 -0.02492864 -0.1457399285  0.585604312 *

# c(2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34)
# c(3,5,6,7,8,9,10,12,13,14,15,16,17,19,20,21,22,23,25,26,27,28,29,30,31,32)

#variables a eliminar segun correlacion  
temp<-c(5,6,25,26)
DB1<-DB[,-temp]

#variables considerando spearman
temp<-c(1,2,3,5,6,10,12,13,15,16,23,25,26,33,34)
DB2<-DB[,temp]

#variables combinando spearman con pearson
temp<-c(1,2,3,10,12,13,15,16,23,33,34)
DB3<-DB[,temp]

#variables a tomar con 5 componentes principales
temp<-c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34)
DB4<-DB[,temp]

#variables a tomar con 3 componentes principales
temp<-c(1,3,5,6,7,8,9,10,12,13,14,15,16,17,19,20,21,22,23,25,26,27,28,29,30,31,32)
DB5<-DB[,temp]

#variables combinando 3 componentes principales con correlacion
temp<-c(1,3,7,8,9,10,12,13,14,15,16,17,19,20,21,22,23,27,28,29,30,31,32)
DB6<-DB[,temp]

# datos considerando solo los promedios
temp<-c(13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)
DB7<-DB[,-temp]

# datos considerando solo los promedios y eliminando las variables segun correlacion
temp<-c(5,6,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)
DB8<-DB[,-temp]

#variables combinando 3 componentes principales con correlacion de pearson y spearman
#temp<-c(1,2,3,5,6,10,12,13,15,16,23,25,26,33,34) -(4,7,8,9,11,14,17,18,19,20,21,22,24,27,28,29,30,31,32)
temp<-c(1,3,10,12,13,15,16,23)
DB9<-DB[,temp]

#variables combinando 3 componentes principales con spearman
temp<-c(1,3,5,6,10,12,13,15,16,23,25,26)
DB10<-DB[,temp]

#################################
# Agrupacion por modelos

BIC1 = mclustBIC(DB1[,2:30], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
summary(BIC1)

BIC2 = mclustBIC(DB2[,2:15], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
summary(BIC2)

BIC3 = mclustBIC(DB3[,2:11], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
summary(BIC3)

BIC4 = mclustBIC(DB4[,2:32], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
summary(BIC4)

BIC5 = mclustBIC(DB5[,2:27], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
summary(BIC5)

BIC6 = mclustBIC(DB6[,2:23], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
summary(BIC6)

BIC7 = mclustBIC(DB7[,2:14], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
summary(BIC7)

BIC8 = mclustBIC(DB8[,2:12], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
summary(BIC8)

BIC9 = mclustBIC(DB9[,2:8], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
summary(BIC9)

BIC10 = mclustBIC(DB10[,2:12], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
summary(BIC10)

##################
#mejor BIC -> BIC6
plot(BIC6)

ICL6=mclustICL(DB6[,2:23], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
plot(ICL6)
summary(ICL6)

mod6=Mclust(DB6[,2:23],x=BIC6)
plot(mod6, what = "classification")
summary(mod6)

mod6$parameters[["mean"]] #centroides

classification<-ifelse(mod6$classification==2,1,2)
tabla = table(DB6$class,classification)
print(tabla)
##################

#comparación de mod3 con k-means
#se realiza la comparativa con el algoritmo de las k-means
data_kmeans <- kmeans(DB6[2:23], 2, nstart = 20)
plot(DB6[2:23], col=data_kmeans$cluster)
tabla = table(DB6$class,data_kmeans$cluster)
print(tabla)

data_kmeans$centers #centroides
