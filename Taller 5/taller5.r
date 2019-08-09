library('e1071')
library('DMwR')
library('pROC')

setwd("G:/Mi unidad/Universidad/Semestre 11/TMDA/Taller 5")
bc_data <- read.table("wpbc.data", header = FALSE, sep = ",")

colnames(bc_data) = c("id", "c", "t","r1", "t1", "p1", "a1", "su1",
                      "com1", "con1", "pc1", "si1", "df1",
                      "r2", "t2", "p2", "a2", "su2",
                      "com2", "con2", "pc2", "si2", "df2",
                      "r3", "t3", "p3", "a3", "su3",
                      "com3", "con3", "pc3", "si3", "df3",
                      "ts", "ly")

#se eliminan registros con error
error = bc_data$ly == '?'
bc_data = bc_data[!error,]

#se modifica la ultima variable a numerica
bc_data$ly = as.numeric(bc_data$ly)

#eliminar variables que son innecesarias
bc_data$id = NULL
bc_data$t = NULL

library('caret')

set.seed(42)
index <- createDataPartition(bc_data$c, p = 0.7, list = FALSE)

train_data <- bc_data[index, ]
test_data  <- bc_data[-index, ]

accuracy <- function(table){
  result <- (table[1,1]+table[2,2])/(table[1,1]+table[1,2]+table[2,1]+table[2,2])
  return(result)
}

model1 <- tune(svm, c~., data = train_data, kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model1)
pred1 <- predict(model1$best.model, test_data[2:33])
table(pred1, test_data$c)
ROC1 <-roc(test_data$c~as.numeric(pred1)) 
ROC1$auc

model2 <- tune(svm, c~., data = train_data, kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model2)
pred2 <- predict(model2$best.model, test_data[2:33])
table(pred2, test_data$c)
ROC2 <-roc(test_data$c~as.numeric(pred2)) 
ROC2$auc

# Smote
bc_smote <- SMOTE(c ~ ., bc_data, perc.over = 100, perc.under=200)
table(bc_smote$c)

set.seed(42)
index <- createDataPartition(bc_smote$c, p = 0.7, list = FALSE)

train_smote <- bc_smote[index, ]
test_smote  <- bc_smote[-index, ]

model3 <- tune(svm, c~., data = train_smote, kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model3)
pred3 <- predict(model3$best.model, test_smote[2:33])
table(pred3, test_smote$c)
ROC3 <-roc(test_smote$c~as.numeric(pred3)) 
ROC3$auc

model4 <- tune(svm, c~., data = train_smote, kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model4)
pred4 <- predict(model4$best.model, test_smote[2:33])
table(pred4, test_smote$c)
ROC4 <-roc(test_smote$c~as.numeric(pred4)) 
ROC4$auc


#### 
#comparación con la eleccion de variables según Mean Decreasse Accuracy

bc_data.rf = bc_smote[c("c","ly","t2","ts","df1","a1","a3","p1","su3","si1","r1","t1","p3","pc2","su2","r2","df3","pc3","r3","con2","con1","com3","pc1","con3","si2","t3","si3","com1","df2","a2","com2","su1","p2")]

set.seed(42)
index <- createDataPartition(bc_data.rf$c, p = 0.7, list = FALSE)

train_smote1 <- bc_data.rf[index, ]
test_smote1  <- bc_data.rf[-index, ]

model5 <- tune(svm, c~., data = train_smote1[1:10], kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model5)
pred5 <- predict(model5$best.model, test_smote1[2:10])
table(pred5, test_smote1$c)
ROC5 <-roc(test_smote1$c~as.numeric(pred5)) 
ROC5$auc

model6 <- tune(svm, c~., data = train_smote1[1:10], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model6)
pred6 <- predict(model6$best.model, test_smote1[2:10])
table(pred6, test_smote1$c)
ROC6 <-roc(test_smote1$c~as.numeric(pred6)) 
ROC6$auc

model7 <- tune(svm, c~., data = train_smote1[1:9], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model7)
pred7 <- predict(model7$best.model, test_smote1[2:9])
table(pred7, test_smote1$c)
ROC7 <-roc(test_smote1$c~as.numeric(pred7)) 
ROC7$auc

model8 <- tune(svm, c~., data = train_smote1[1:8], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model8)
pred8 <- predict(model8$best.model, test_smote1[2:8])
table(pred8, test_smote1$c)
ROC8 <-roc(test_smote1$c~as.numeric(pred8)) 
ROC8$auc

model9 <- tune(svm, c~., data = train_smote1[1:7], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model9)
pred9 <- predict(model9$best.model, test_smote1[2:7])
table(pred9, test_smote1$c)
ROC9 <-roc(test_smote1$c~as.numeric(pred9)) 
ROC9$auc

model10 <- tune(svm, c~., data = train_smote1[1:6], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model10)
pred10 <- predict(model10$best.model, test_smote1[2:6])
table(pred10, test_smote1$c)
ROC10 <-roc(test_smote1$c~as.numeric(pred10)) 
ROC10$auc

model11 <- tune(svm, c~., data = train_smote1[1:5], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model11)
pred11 <- predict(model11$best.model, test_smote1[2:5])
table(pred11, test_smote1$c)
ROC11 <-roc(test_smote1$c~as.numeric(pred11)) 
ROC11$auc

model12 <- tune(svm, c~., data = train_smote1[1:4], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model12)
pred12 <- predict(model12$best.model, test_smote1[2:4])
table(pred12, test_smote1$c)
ROC12 <-roc(test_smote1$c~as.numeric(pred12)) 
ROC12$auc

plot(bc_data.rf[1:4],pch=16)
points(bc_data.rf[1:4]$c, pred12, col = "blue", pch=4)

library("MASS")
parcoord(bc_data.rf[,2:4],var.label = TRUE,col=c("red", "green"))
legend("bottomright",legend = c("N", "R"),col=1:3,cex=0.6,fill=1:4)


model13 <- tune(svm, c~., data = train_smote1[1:9], kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model13)
pred13 <- predict(model13$best.model, test_smote1[2:9])
table(pred13, test_smote1$c)
ROC13 <-roc(test_smote1$c~as.numeric(pred13)) 
ROC13$auc

model14 <- tune(svm, c~., data = train_smote1[1:8], kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model14)
pred14 <- predict(model14$best.model, test_smote1[2:8])
table(pred14, test_smote1$c)
ROC14 <-roc(test_smote1$c~as.numeric(pred14)) 
ROC14$auc

model15 <- tune(svm, c~., data = train_smote1[1:7], kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model15)
pred15 <- predict(model15$best.model, test_smote1[2:7])
table(pred15, test_smote1$c)
ROC15 <-roc(test_smote1$c~as.numeric(pred15)) 
ROC15$auc

model16 <- tune(svm, c~., data = train_smote1[1:6], kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model16)
pred16 <- predict(model16$best.model, test_smote1[2:6])
table(pred16, test_smote1$c)
ROC16 <-roc(test_smote1$c~as.numeric(pred16)) 
ROC16$auc

model17 <- tune(svm, c~., data = train_smote1[1:5], kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model17)
pred17 <- predict(model17$best.model, test_smote1[2:5])
table(pred17, test_smote1$c)
ROC17 <-roc(test_smote1$c~as.numeric(pred17)) 
ROC17$auc

model18 <- tune(svm, c~., data = train_smote1[1:4], kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model18)
pred18 <- predict(model18$best.model, test_smote1[2:4])
table(pred18, test_smote1$c)
ROC18 <-roc(test_smote1$c~as.numeric(pred18)) 
ROC18$auc


require("RWeka")
ranking <-InfoGainAttributeEval(c ~ . , data = bc_smote)

bc_data.gain = bc_smote[c("c","ly","df1","ts","r2","a3","r3","si1","t2","a1","p1","su3","si1","r1","t1","p3","pc2","su2","df3","pc3","r3","con2","con1","com3","pc1","con3","si2","t3","si3","com1","df2","a2","com2","su1","p2")]

set.seed(42)
index <- createDataPartition(bc_data.gain$c, p = 0.7, list = FALSE)

train_smote2 <- bc_data.gain[index, ]
test_smote2 <- bc_data.gain[-index, ]

model19 <- tune(svm, c~., data = train_smote2[1:8], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model19)
pred19 <- predict(model19$best.model, test_smote2[2:8])
table(pred19, test_smote2$c)
ROC19 <-roc(test_smote2$c~as.numeric(pred19)) 
ROC19$auc

model20 <- tune(svm, c~., data = train_smote2[1:7], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model20)
pred20 <- predict(model20$best.model, test_smote2[2:7])
table(pred20, test_smote2$c)
ROC20 <-roc(test_smote2$c~as.numeric(pred20)) 
ROC20$auc

model21 <- tune(svm, c~., data = train_smote2[1:6], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model21)
pred21 <- predict(model21$best.model, test_smote2[2:6])
table(pred21, test_smote2$c)
ROC21 <-roc(test_smote2$c~as.numeric(pred21)) 
ROC21$auc

model22 <- tune(svm, c~., data = train_smote2[1:5], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model22)
pred22 <- predict(model22$best.model, test_smote2[2:5])
table(pred22, test_smote2$c)
ROC22 <-roc(test_smote2$c~as.numeric(pred22)) 
ROC22$auc

model23 <- tune(svm, c~., data = train_smote2[1:4], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model23)
pred23 <- predict(model23$best.model, test_smote2[2:4])
table(pred23, test_smote2$c)
ROC24 <-roc(test_smote2$c~as.numeric(pred23)) 
ROC24$auc


model24 <- tune(svm, c~., data = train_smote2[1:8], kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model24)
pred24 <- predict(model24$best.model, test_smote2[2:8])
table(pred24, test_smote2$c)
ROC24 <-roc(test_smote2$c~as.numeric(pred24)) 
ROC24$auc

model25 <- tune(svm, c~., data = train_smote2[1:7], kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model25)
pred25 <- predict(model25$best.model, test_smote2[2:7])
table(pred25, test_smote2$c)
ROC25 <-roc(test_smote2$c~as.numeric(pred25)) 
ROC25$auc

model26 <- tune(svm, c~., data = train_smote2[1:6], kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model26)
pred26 <- predict(model26$best.model, test_smote2[2:6])
table(pred26, test_smote2$c)
ROC26 <-roc(test_smote2$c~as.numeric(pred26)) 
ROC26$auc

model27 <- tune(svm, c~., data = train_smote2[1:5], kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model27)
pred27 <- predict(model27$best.model, test_smote2[2:5])
table(pred27, test_smote2$c)
ROC27 <-roc(test_smote2$c~as.numeric(pred27)) 
ROC27$auc

model28 <- tune(svm, c~., data = train_smote2[1:4], kernel = "linear", ranges = list(cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model28)
pred28 <- predict(model28$best.model, test_smote2[2:4])
table(pred28, test_smote2$c)
ROC28 <-roc(test_smote2$c~as.numeric(pred28)) 
ROC28$auc

bc_data.MDA = bc_smote[c("c","ly","t2","ts","df1","a1","a3","p1","su3","si1","r1","t1","p3","pc2","su2","r2","df3","pc3","r3","con2","con1","com3","pc1","con3","si2","t3","si3","com1","df2","a2","com2","su1","p2")]

set.seed(42)
index <- createDataPartition(bc_data.MDA$c, p = 0.7, list = FALSE)

train_smote3 <- bc_data.gain[index, ]
test_smote3 <- bc_data.gain[-index, ]

model19 <- tune(svm, c~., data = train_smote3[1:10], kernel = "radial", ranges = list(gamma = 2^(-10:3), cost = 2^(-5:5)), tunecontrol = tune.control(sampling = "cross", cross = 10))
summary(model19)
pred19 <- predict(model19$best.model, test_smote3[2:10])
table(pred19, test_smote3$c)
ROC19 <-roc(test_smote3$c~as.numeric(pred19)) 
ROC19$auc
