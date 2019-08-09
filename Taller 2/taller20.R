require(randomForest)

setwd("G:/Mi unidad/Universidad/Semestre 11/TMDA/Taller 2")
DB <- read.table("wpbc.data", header = FALSE, sep = ",")

colnames(DB) = c("id", "c", "t","r1", "t1", "p1", "a1", "su1",
                      "com1", "con1", "pc1", "si1", "df1",
                      "r2", "t2", "p2", "a2", "su2",
                      "com2", "con2", "pc2", "si2", "df2",
                      "r3", "t3", "p3", "a3", "su3",
                      "com3", "con3", "pc3", "si3", "df3",
                      "tt", "eg")

#se eliminan registros con error
error = DB$eg == '?'
DB = DB[!error,]

#se modifica la ultima variable a numerica
DB$eg = as.numeric(DB$eg)

#eliminar variables que son necesarias
DB$id = NULL
#DB$t = NULL

library(foreach)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

foreach(i=2:34) %dopar% {
  #se seta la semilla
  DB[i] = range01(DB[i])
}


#generar curva ROC
n = 300
n2 = 2
n2_original = 2

curva_roc = data.frame(ntree = numeric() ,ntry = numeric(), especificidad = numeric(), sensibilidad = numeric(), euclidiana = numeric())

foreach(i=1:(171*30)) %dopar% {
  #se seta la semilla
  set.seed(21)
  modelo = randomForest(c~ ., data=DB, ntree = n, importance=TRUE, proximity=TRUE, ntry=n2)
  vp = modelo$confusion[1,1]
  vn = modelo$confusion[1,2]
  fn = modelo$confusion[2,1]
  fp = modelo$confusion[2,2]
  esp = (1-(vn/(fp+vn)))
  sen = (vp/(vp+fn))
  curva_roc = rbind(curva_roc, data.frame(ntree = n, ntry = n2, especificidad = esp, sensibilidad = sen, euclidiana = sqrt(((1-sen)^2) + (esp^2))))
  n2 = n2 + 1
  
  if(n2 == 33){
    n2 = n2_original
    n = n + 10
  }
  print(n)
}


plot(curva_roc$especificidad, curva_roc$sensibilidad, main="Curva ROC", ylab = "sensibilidad", xlab = "1-especificidad")

set.seed(21)
modelo = randomForest(c~ ., data=DB, ntree = 610, importance=TRUE, proximity=TRUE, ntry=2)

tabla = modelo$importance
varImpPlot(modelo)
bc_fn = DB[c("c","eg","df1","tt","su3","r2","t2","t1","con2","t3","p2","r3","si2","si1","a3","pc2","com2","df2","p3","com1","con1","a2","su1","a1","df3","con3","pc3","pc1","su2","r1","si3","p1","com3")]

bc_error = data.frame(n_variables=numeric(), error=numeric())

n = 33

for (i in 1:32)
{
  set.seed(21)
  modelo = randomForest(c~ ., data=bc_fn[,1:n], ntree = 610, importance=TRUE, proximity=TRUE, ntry=2)
  vp = modelo$confusion[1,1]
  vn = modelo$confusion[1,2]
  fn = modelo$confusion[2,1]
  fp = modelo$confusion[2,2]
  e = (vn+fn)/(vp+vn+fn+fp)
  bc_error = rbind(bc_error, data.frame(n_variables = (n-1), error = e))
  n = n - 1
}

#se grafica el error
plot(bc_error$n_variables, bc_error$error,  type="overplotted", main="Reducción de variables", ylab = "Error", xlab = "Cantidad de variables")

#obteniendo que con 14 variables obtiene un mejor resultado
set.seed(21)
modelo = randomForest(c~ ., data=bc_fn[,1:10], ntree = 610, importance=TRUE, proximity=TRUE, ntry=2)


plot(modelo)
legend("bottomright", colnames(modelo$err.rate),col=1:4,cex=0.8,fill=1:4)

#MDS
bc.mds = cmdscale(1 - modelo$proximity, eig=TRUE) # escalamiento clásico

op = par(pty="s") # A character specifying the type of plot region to be used; "s"

pairs(cbind(bc_fn[,2:10], bc.mds$points), cex=0.6, gap=0,
      col=c("red", "green")[as.numeric(bc_fn$c)],
      main="Breast Cancer Pronostic Wisconsin")

par(op)

library("MASS")
parcoord(bc_fn[1:50,2:10],var.label = TRUE,col=c("black", "red")[as. numeric(bc_fn$c)])
legend("bottomright",legend = c("N", "R"),fill=2:3)
