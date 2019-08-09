# cargar librerias
library(twitteR)
library(tm)
library(wordcloud)

# Recopilar tweets
# Tweets de @Karol_LuceroV
kd_tweets = userTimeline("Karol_LuceroV", 2000)
# Tweets de @camila_vallejo
cv_tweets = userTimeline("camila_vallejo", 2000)
# Tweets de @sebastianpinera
sp_tweets = userTimeline("sebastianpinera", 2000)
# Tweets de @copano
nc_tweets = userTimeline("copano", 2000)

# Extrae el texto de los tweets
kd_txt = sapply(kd_tweets, function(x) x$getText())
cv_txt = sapply(cv_tweets, function(x) x$getText())
sp_txt = sapply(sp_tweets, function(x) x$getText())
nc_txt = sapply(nc_tweets, function(x) x$getText())

# Funcion para limpieza de datos
cleanme = function(x)
{
  # cambia a minúsculas
  x = tolower(x)
  # remueve retweets
  x = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)
  # remueve @
  x = gsub("@\\w+", "", x)
  # remueve links
  x = gsub("(https?:\\/\\/)\\w+(.\\w+)+(.\\w+) ", "", x)
  x = gsub("www.\\w+(.*) ", "", x)
  x = gsub("(https?:\\/\\/)\\w+(.\\w+)+(.\\w+)", "", x)
  x = gsub("www.\\w+(.*)", "", x)
  # caracteres especiales
  x = gsub("<", "", x)
  x = gsub(">", "", x)
  # remueve simbolos de puntuación
  x = gsub("[[:punct:]]", "", x)
  # remove números
  x = gsub("[[:digit:]]", "", x)
  # remueve tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remueve espacio en blanco al inicio
  x = gsub("^ ", "", x)
  # remueve espacio en blanco al final
  x = gsub(" $", "", x)
  return(x)
}

# Aplica la función de limpieza de datos
kd_clean = cleanme(kd_txt)
cv_clean = cleanme(cv_txt)
sp_clean = cleanme(sp_txt)
nc_clean = cleanme(nc_txt)

# Une todos los textos en un sólo vector
kd = paste(kd_clean, collapse=" ")
cv = paste(cv_clean, collapse=" ")
sp = paste(sp_clean, collapse=" ")
nc = paste(nc_clean, collapse=" ")
all = c(kd, cv, sp, nc)

# remueve palabras vacías (stopwords) en español y de una lista personalizada
all = removeWords(all, c(stopwords("spanish"), "Karol_LuceroV", "camila_vallejo", "sebastianpinera", "copano"))
#sw <- readLines("D:/stopwords.es.txt", encoding="UTF-8")
#sw = iconv(sw, to="ASCII//TRANSLIT")
#all = removeWords(all,sw)

# Crea un corpus y una matriz de termino-documento
corpus <- Corpus(VectorSource(all))
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)

# agrega los nombres de las columnas
colnames(m) = c("@Karol_LuceroV",
                "@camila_vallejo",
                "@sebastianpinera",
                "@copano")

# genera la comparación de nube de palabras
comparison.cloud(m, random.order=FALSE,
                 colors = c("#FF0099", "red",
                            "#00B2FF", "#6600CC"),
                 title.size=0.8, max.words=1000)