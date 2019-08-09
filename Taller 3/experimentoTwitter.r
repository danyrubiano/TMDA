# cargar librerias
library(tm)
library(wordcloud)
library(twitteR)

api_key <- "s3BVsXQkZYIvC9zdRbFfVTDEL"
api_secret <- "n2RDYzR8BBM6Q4JU35t7NHGh8FDrlbKu0yKwYqIMmNIkQoSmsw"
access_token <- "321718968-ypop2wR7TK8XGoLS2KwPUfr8PrmTYeMveO33O9Ht"
access_token_secret <- "bl4dNwELdXAmhjKoseHQe08b6MqJqfqWVkrLkbwiB610e"
setup_twitter_oauth(api_key,api_secret,access_token, access_token_secret)

# recolecta tweets de @ahoranoticiasAN
tweets = userTimeline("ahoranoticiasAN", 100)

# vuelca la informacion de los tweets a un data frame
df = twListToDF(tweets)
# obtiene el texto de los tweets
txt = df$text

##### inicio limpieza de datos #####
# remueve retweets
txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt)
# remove @otragente
txtclean = gsub("@\\w+", "", txtclean)
# remueve simbolos de puntuación
txtclean = gsub("[[:punct:]]", "", txtclean)
# remove números
txtclean = gsub("[[:digit:]]", "", txtclean)
# remueve links
txtclean = gsub("http\\w+", "", txtclean)
##### fin limpieza de datos #####

# construye un corpus
corpus = Corpus(VectorSource(txtclean))

# convierte a minúsculas
corpus = tm_map(corpus, tolower)
# remueve palabras vacías (stopwords) en español
corpus = tm_map(corpus, removeWords, c(stopwords("spanish"), "ahoranoticiasAN"))

# remueve palabras vacías personalizada
# En sw podría cargar archivo de palabras que uno indique que no aportan relevancia
# corpus = tm_map(corpus, removeWords, sw)

# remove espacios en blanco extras
corpus = tm_map(corpus, stripWhitespace)

# crea una matriz de términos
tdm <- TermDocumentMatrix(corpus)

# convierte a una matriz
m = as.matrix(tdm)

# conteo de palabras en orden decreciente
wf <- sort(rowSums(m),decreasing=TRUE)

# crea un data frame con las palabras y sus frecuencias
dm <- data.frame(word = names(wf), freq=wf)

# grafica la nube de palabras (wordcloud)
wordcloud(df$word,df$freq,min.freq=6)