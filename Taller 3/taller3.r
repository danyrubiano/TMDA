library("tm")
library("heuristica")
library("SnowballC")
library("wordcloud")
require(maxent)
library("DMwR")
library("caret")


presicion <- function(table){
  vp <- table[1,1] #
  fp <- table[2,2]
  result <- vp/(vp+fp)
  return(result)
}

recall <- function(table){
  vp <- table[1,1]
  fn <- table[2,1]
  result <- vp/(vp+fn)
  return(result)
}

F1 <- function(x,y){
  return(2*x*y/(x+y))
}

getResultsData <- function(data, results){
  
  results.correct_counter = 0
  results.incorrect_counter = 0
  vp = 0
  fp = 0
  fn = 0
  vn = 0
  
  for (i in 1:nrow(data)){
    
    if (i%%500 == 0){ print(i) }
    
    temp.result = as.integer(results[i,1])
    
    temp.test = data[i,]
    
    temp.is_relevant = nrow(subset(temp.test, major == 3 || major ==5 || major = 12 || major ==18 || major == 20 || major == 21 )) == 1
    
    # Está bien clasificado?
    if ( temp.result != 0 && data[i,temp.result] == 1 ){ 
      
      # Es relevante?
      if (temp.is_relevant){
        vp = vp+1
      }else{ # No es relevante
        fp = fp+1
      }
      results.correct_counter = results.correct_counter + 1
      
    }else if ( temp.result == 0 && (data.test[i,]$major.1 + data.test[i,]$major.2 + data.test[i,]$major.4 + data.test[i,]$major.6 + data.test[i,]$major.7 + data.test[i,]$major.8 + data.test[i,]$major.10 + data.test[i,]$major.13 + data.test[i,]$major.14 + data.test[i,]$major.16 + data.test[i,]$major.17 + data.test[i,]$major.19 + data.test[i,]$major.99 == 0)) {
      # Es relevante?
      #cat(i,"\t(0) Bien clasificado\t",data.test[i,]$pid,"\n")
      
      if (temp.is_relevant){
        vp = vp+1
      }else{ # No es relevante
        
        fp = fp+1
      }
      results.correct_counter = results.correct_counter + 1
      
    }else{ # Mal clasificado
      #cat(i,"\t Mal clasificado\t", data.test[i,]$pid,"\n")
      # Es relevante?
      if (temp.is_relevant){
        fn = fn+1
      }else{ # No es relevante
        vn = vn+1
      }
      results.incorrect_counter = results.incorrect_counter + 1
    }
  }
  precision = vp/(vp+fp)
  recall = vp/(vp+fn)
  
  cat("vp: ", vp, "\nvn:", vn, "\nfp", fp, "\nfn", fn ,"\nprecision", precision, "\nrecall", recall)
  
  return(list(vp,vn,fp,fn,precision,recall))
}

data <- read.csv(system.file("data/USCongress.csv.gz",package="maxent"))
data$ID = NULL

wordcloud(words = data$text, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

new_data <- data
new_data$text <- gsub("A bill","",new_data$text) # Eliminar "A bill" frecuente en todas las instancias del senado
new_data$text <- gsub("\\d+","",new_data$text) # Eliminar numeros
new_data$text <- gsub("[[:punct:]]","",new_data$text) # Eliminar puntuación
new_data$text <- tolower(new_data$text) # Convertir en minusculas
new_data$text <- removeWords(new_data$text, stopwords("english")) # elimna stopwords
new_data$text <- stripWhitespace(new_data$text) # elimina doble espacios
new_data$text <- stemDocument(new_data$text,language = "english") # deja las raices de las palabras

wordcloud(words = new_data$text, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#table(new_data$major)
new_data$cong = NULL
new_data$billnum = NULL
new_data$h_or_sen = NULL #as.factor(new_data$h_or_sen)
new_data$major = as.factor(new_data$major)
new_data$text = as.factor(new_data$text)

data_smote <- SMOTE(major~., new_data, perc.over = 800, perc.under = 1000) #, learner = 'rpartXse', se = 0.5)
table(data_smote$major)

wordcloud(words = data_smote$text, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

############################
# Trabajando con data pura

training_index <- sample(nrow(data),(nrow(data)*.7)) 
training_data <- data[training_index,]
test_data <- data[-training_index,]

corpus_training <- Corpus(VectorSource(training_data$text[1:3114]))
matrix_training<-DocumentTermMatrix(corpus_training) #Creación de la matriz
sparse_training<-as.compressed.matrix(matrix_training) #Creación de la matriz dispersa
tune_training<-tune.maxent(sparse_training, training_data$major, nfold=3, verbose=TRUE, showall=TRUE)
model_training<-maxent(sparse_training, training_data$major, l1_regularizer= tune_training[[8,1]],
                       l2_regularizer=tune_training[[8,2]], use_sgd=tune_training[[8,3]], set_heldout=tune_training[[8,4]])
# 8
#
corpus_test <- Corpus(VectorSource(test_data$text[1:1335]))
matrix_test<-DocumentTermMatrix(corpus_test) #Creación de la matriz
sparse_test<-as.compressed.matrix(matrix_test) #Creación de la matriz dispersa

results_ <- predict(model_training,sparse_test)
tab_data <- table(test_data$major, results_[,1])
getResultsData(test_data, results_)
print(tab_data)
###########################
# Trabajando con new_data

training_index <- sample(nrow(new_data),(nrow(new_data)*.7)) 
training_new_data <- new_data[training_index,]
test_new_data <- new_data[-training_index,]

corpus_training_new_data <- Corpus(VectorSource(training_new_data$text[1:3114]))
matrix_training_new_data<-DocumentTermMatrix(corpus_training_new_data) #Creación de la matriz
sparse_training_new_data<-as.compressed.matrix(matrix_training_new_data) #Creación de la matriz dispersa
tune_training_new_data<-tune.maxent(sparse_training_new_data, training_new_data$major, nfold=3, verbose=TRUE, showall=TRUE)
model_training_new_data<-maxent(sparse_training_new_data, training_new_data$major, l1_regularizer= tune_training[[9,1]],
                       l2_regularizer=tune_training[[9,2]], use_sgd=tune_training[[9,3]], set_heldout=tune_training[[9,4]])
# 9
#
corpus_test_new_data <- Corpus(VectorSource(test_new_data$text[1:1335]))
matrix_test_new_data<-DocumentTermMatrix(corpus_test_new_data) #Creación de la matriz
sparse_test_new_data<-as.compressed.matrix(matrix_test_new_data) #Creación de la matriz dispersa

results_new_data <- predict(model_training_new_data,sparse_test_new_data)
tab_new_data <- table(test_new_data$major, results_new_data[,1])
print(tab_new_data)

############################
# Trabajando con data_smote

training_index <- sample(nrow(data_smote),(nrow(data_smote)*.7)) 
training_data_smote <- data_smote[training_index,]
test_data_smote <- data_smote[-training_index,]

corpus_training_smote <- Corpus(VectorSource(training_data_smote$text[1:1868]))
matrix_training_smote<-DocumentTermMatrix(corpus_training_smote) #Creación de la matriz
sparse_training_smote<-as.compressed.matrix(matrix_training_smote) #Creación de la matriz dispersa
tune_training_smote<-tune.maxent(sparse_training_smote, training_data_smote$major, nfold=3, verbose=TRUE, showall=TRUE)
model_training_smote<-maxent(sparse_training_smote, training_data_smote$major, l1_regularizer= tune_training[[8,1]],
                       l2_regularizer=tune_training[[8,2]], use_sgd=tune_training[[8,3]], set_heldout=tune_training[[8,4]])
# 8
#
corpus_test_smote <- Corpus(VectorSource(test_data_smote$text[1:802]))
matrix_test_smote<-DocumentTermMatrix(corpus_test_smote) #Creación de la matriz
sparse_test_smote<-as.compressed.matrix(matrix_test_smote) #Creación de la matriz dispersa

results_smote <- predict(model_training_smote,sparse_test_smote)
tab_data_smote <- table(test_data_smote$major, results_smote[,1])
print(tab_data_smote)
