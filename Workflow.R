##El proceso se inicia al cargar los paquetes requeridos {tm}, {SnowballC} y {ggplot} 
library(tm, SnowballC)
library(ggplot2)
# La fase de preprocesamiento para Análisis Textual en R requiere una revision 
# inicial de los datos en la fuente. En el caso de los artículos 
# correspondientes a la revisión de literatura, se almacena el listado de
# archivos en una variable:
material <- DirSource("~/Dropbox/test101/")
# Se crea el objeto tipo Corpus a partir del listado de archivos:
articulos <- Corpus(material, readerControl = list(reader = readPDF(), language = "en"))
# En 'articulos' queda almacenado un VCorpus o Corpus Volátil con cada uno de los
# textos. Para verificar el contenido del objeto, se puede usar la funcion
# <inspect> del paquete {tm}:
inspect(articulos)
# El número de artículos debe corresponder con el número de archivos. Se revisan
# los artículos que produjeron un número de caracteres inferior al promedio. Para
# verificar cualquiera de las respuestas se puede llamar la función writeLines,
# teniendo en cuenta forzar el tipo de dato a texto mediante 'as.character':
writeLines(as.character(articulos[43]))
writeLines(as.character(articulos[1]))
writeLines(as.character(articulos[13]))
# Hasta este momento se mantiene el contenido original. Se inicia el proceso de
# transformación de los textos para obtener un material cuantificable. El paquete {tm}
# incluye varias transformaciones:
getTransformations()
#Dado que las transformaciones alteran el contenido, resulta prudente mantener 
#un seguimiento del proceso. Para ello se cambia el nombre del objeto sobre el 
#cual se trabaja a 'limpieza', el cual se genera a partir de 'artículos'. Al
#generar cada transformación subsiguiente se sobreescribe la variable 'limpieza'
#con una nueva versión. Las transformaciones siguientes incluyen pasar todos los
#términos a minúsculas —tolower— y eliminar las palabras sin significación
#relevante, como conjunciones, artículos, etc. utilizando la función removeWords
#con el parámetro 'stopwords' y el idioma de los textos:
limpieza <- tm_map(articulos, content_transformer(tolower))
limpieza <- tm_map(limpieza, removeWords, stopwords("english"))
limpieza <- tm_map(limpieza, removeWords, stopwords("spanish"))
#La siguiente transformación relevante consiste en quitar la puntuación:
limpieza <- tm_map(limpieza, removePunctuation)
#Algunos signos de puntuación asociados a caractéres especiales y que no están 
#separados de las palabras deben ser eliminados directamente, utilizando una 
#función que los reemplace por espacios en blanco. La función genérica que se 
#emplea es la siguiente:
## function toSpace ####
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x, fixed = TRUE))})
# La función se emplea para reemplazar los caracteres indeseados dentro del corpus: 
limpieza <- tm_map(limpieza, toSpace, "\f")
limpieza <- tm_map(limpieza, toSpace, "<")
limpieza <- tm_map(limpieza, toSpace, ">")
limpieza <- tm_map(limpieza, toSpace, "\\")

# Se verifica el resultado del proceso, comparando un elemento inicial:
writeLines(as.character(articulos[43]))
# El cual se diferencia del resultado de depuración:
writeLines(as.character(limpieza[43]))
# La verificación indica que aún subsisten números; se corre ahora la
# transformación correspondiente a la eliminación de dígitos:
limpieza <- tm_map(limpieza, removeNumbers)
#Se verifica de nuevo:
writeLines(as.character(limpieza[43]))

# Se se corre ahora la transformación correspondiente a la eliminación de los
# espacios en blanco:
limpieza <- tm_map(limpieza,stripWhitespace)
#Se verifica de nuevo:
writeLines(as.character(limpieza[13]))
writeLines(as.character(limpieza[43]))
# Para verificar el efecto de transformación, se puede comparar el contenido de
# un mismo elemento en los dos objetos:
writeLines(as.character(limpieza[13]))
writeLines(as.character(articulos[13]))
## A continuación se debe completar un proceso de radicación para reducir las 
## variaciones afijales y condensar los posibles significados. Aunque el proceso
## puede unir palabras con diferente significado, las ventajas del proceso en 
## corpora extensos superan a las desventajas. Se emplea el paquete {SnowballC}.
## Se emplea el algoritmo de radicación para inglés y para español.

limpieza <- tm_map(limpieza, stemDocument, language = "english")
limpieza <- tm_map(limpieza, stemDocument, language = "spanish")



#Un último paso de la fase de preprocesamiento consiste en transformar el 
#listado de respuestas en documentos de texto plano:
tradocs <- tm_map(limpieza, PlainTextDocument)
#Se eliminan tanto el Corpus temporal de limpieza como el índice del directorio.
#Conservamos el corpus inicial de lectura para cotejar la radicación contra los
#términos originales:
rm(limpieza, material)
# Para verificar el efecto de transformación
writeLines(as.character(tradocs[13]))
## Luego de esto se pasa a la fase de montaje de los datos. El resultado será
## una matriz de términos a partir de la cual el paquete {tm} extrae información
## relevante. Para crearla se usa la función correspondiente:
dtm <- DocumentTermMatrix(tradocs)
# Para ver las dimensiones de la matriz:
dim(dtm)
# Se trata de una matriz cuyas filas corresponden a los documentos procesados, 
# y cuyas columnas corresponden a cada uno de los términos encontrados. El valor
# de cada elemento de la matriz es la frecuencia de aparición del término en el
# documento; en este caso, en cada documento
inspect(dtm[13:32,13:35])
# En general resultan de interés los valores totales de frecuencia en la matriz.
# Para obtenerlos se crea una lista con esos totales, la cual se ordena de forma
# descendente por frecuencia:
frecuencias <- sort(colSums(as.matrix(dtm)), decreasing = T)
#Para verificar la frecuencia de frecuencias se genera un resumen de la 
#información obtenida: 
summary(frecuencias)
# Los valores obtenidos informan sobre la distribución de los términos. Para
# mostrar la distribución de las frecuencias de forma explícita, se hace uso de
# la función 'table', la cual crea una tabla de contingencia de las frecuencias:
tabla.frecuencias <- table(frecuencias)
tabla.frecuencias

####Pareto#Frecuencias#acumuladas#####################################################
# Creamos la función pareto que permite calcular la frecuencia acumulada a
# partir de una frecuencia en particular:

pareto <- function(x,matrizn) {
  return(sum(matrizn[matrizn>x]))
  }

# Ahora empleamos la función de Pareto para construir otra que permita hacer un
# análisis exploratorio de factores: se trata de encontrar una frecuencia de
# corte cuya función de Pareto se acerque al porcentaje que establezcamos:

corte <- function(x,matrizc) {
  i <- 1
  while(pareto(i,matrizc)>(pareto(0,matrizc)*x)) {
    i <- i+1
  }
  return(i)
}

# Se calcula el 80% de la frecuencia total acumulada para saber en qué 
# frecuencia podemos cortar el análisis sin perder más del 20% de los
# resultados, es decir, establecemos cuál es la frecuencia mínima para obtener
# términos relevantes, que explican el 80% de la frecuencia acumulada:

frecorte <- corte(.8,frecuencias)

# ¿Cuántos términos son considerados como Factores Principales?  
 
useful<- length(frecuencias) - length(subset(frecuencias, frecuencias<frecorte))
useful

##Unigramas####
# En este punto se procede a extraer información de manera exploratoria. En
# primera instancia, Se extraen los términos más frecuentes que pueden
# considerarse factores principales:

relevant <- head(frecuencias,useful)
relevant

write.table(relevant, "./unigram.csv", sep="\t")

# Verificamos utilizando las funciones internas del paquete {tm}:

findFreqTerms(dtm, frecorte)

# Y mostramos para su inspección los términos menos frecuentes:

tail(frecuencias,200)

#### 
#Transformación#TF-IDF###########################################################################
#
# En este punto se vuelve a ejecutar el proceso utilizando la función de 
# normalización TF-IDF, con la cual se modula el rango estadístico de los términos más frecuentes en
# todos los documentos del corpus. Hay dos maneras de hacerlo: se puede partir 
# del corpus preprocesado y crear la matriz usando la función weightTfIdf: 
# dtm_idf <- DocumentTermMatrix(tradocs, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
# De forma alternativa, se puede aplicar la
# función weightTfIdf a la matriz 'cruda' es decir, la que se procesó antes sin
# ponderar:
dtm_idf <- weightTfIdf(dtm, normalize = TRUE)
# De cualquier modo, es prudente reducir la dispersión de la matriz:
dtm_idf <- removeSparseTerms(dtm_idf, .8)
dim(dtm_idf)
inspect(dtm_idf)
frecuencias_idf <- sort(colSums(as.matrix(dtm_idf)), decreasing = T)
write.table(frecuencias_idf, "./unigram_idf.csv", sep="\t")

##Bigramas#########
# Se hace la tokenización por bigramas usando la capacidad del paquete {NLP}
BigramTokenizer <-
  function(x)
    {
      unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = TRUE)
  }

# Cómputo de la matriz Documento-Término para bigramas, dtm2
dtm2 <- DocumentTermMatrix(tradocs, control = list(tokenize = BigramTokenizer))
# para calcular posteriormente la matriz ponderada con TF-IDF guardamos una copia temporal antes de procesarla
dtm2_raw <- dtm2

dim(dtm2)
inspect(dtm2)

# La matriz dtm2 es un orden de magnitud más grande que la matriz dtm de
# unigramas, por lo cual se va a utilizar el reductor de dispersión incluido en
# el paquete {tm} como 'removeSparseTerms'
dtm2 <-removeSparseTerms(dtm2,.9)
dim(dtm2)
inspect(dtm2)

# En general resultan de interés los valores totales de frecuencia en la matriz.
# Para obtenerlos se crea una lista con esos totales, la cual se ordena de forma
# descendente por frecuencia. 
frecuencias2 <- sort(colSums(as.matrix(dtm2)), decreasing = T)
frecorte2 <- corte(.8,frecuencias2)
useful2<- length(frecuencias2) - length(subset(frecuencias2, frecuencias2<frecorte2))
frecuencias2 <- head(frecuencias2,useful2)
write.table(frecuencias2, "./relevant2.csv", sep = "\t")

## Bigramas TF-IDF ####
#
# dtm2_idf <- DocumentTermMatrix(tradocs, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), tokenize = BigramTokenizer))
# Se repite el análisis de bigramas para la DTM ponderada con TF-IDF:
dtm2_idf <- weightTfIdf(dtm2_raw, normalize = TRUE)
# No necesitamos mantener la matriz cruda en memoria:
rm(dtm2_raw)
dtm2_idf <-removeSparseTerms(dtm2_idf,.9)
inspect(dtm2_idf)
frecuencias2_idf <- sort(colSums(as.matrix(dtm2_idf)), decreasing = T)
write.table(frecuencias2_idf, "./bigramas_idf.csv", sep = "\t")

## Trigramas ######
# Tokenización por trigramas usando la capacidad del paquete {NLP}
TrigramTokenizer <-
  function(x)
  {
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = TRUE)
  }
    
# Cómputo de la matriz DTM para trigramas sin ponderación
 dtm3 <- DocumentTermMatrix(tradocs, control = list(tokenize = TrigramTokenizer))
# para calcular posteriormente la matriz ponderada con TF-IDF guardamos una copia temporal antes de procesarla
 dtm3_raw <- dtm3
 
 dtm3 <-removeSparseTerms(dtm3,.95)
 dim(dtm3)
 inspect(dtm3)
 frecuencias3 <- sort(colSums(as.matrix(dtm3)), decreasing = T)
 frecuencias3
 write.table(frecuencias3, "./relevant3.csv", sep = "\t")
 
 ## Trigramas TF-IDF ####
 #
 # Se repite el análisis de trigramas para la DTM ponderada con TF-IDF:
 # dtm3_idf <- DocumentTermMatrix(tradocs, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), tokenize = TrigramTokenizer))
 dtm3_idf <- weightTfIdf(dtm3_raw, normalize = TRUE)
 rm(dtm3_raw)
 
 dtm3_idf <-removeSparseTerms(dtm3_idf,.9)
 inspect(dtm3_idf)
 frecuencias3_idf <- sort(colSums(as.matrix(dtm3_idf)), decreasing = T)
 write.table(frecuencias3_idf, "./trigramas_idf.csv", sep = "\t")
 
 ## Cuatrigramas ######
 # Tokenización por cuatrigramas usando la capacidad del paquete {NLP}
 FourgramTokenizer <-
   function(x)
   {
     unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = TRUE)
   }
 
 # Cómputo de la matriz Término-Documento, transpuesta de la dtm
 dtm4 <- DocumentTermMatrix(tradocs, control = list(tokenize = FourgramTokenizer))
 dtm4_raw <- dtm4
 
 dtm4 <-removeSparseTerms(dtm4,.95)
 dim(dtm4)
 inspect(dtm4)
 frecuencias4 <- sort(colSums(as.matrix(dtm4)), decreasing = T)
 frecuencias4
 write.table(frecuencias4, "./relevant4.csv", sep = "\t")
 
 ## Tetragramas TF-IDF ####
 #
 # Se repite el análisis de tetragramas para la DTM ponderada con TF-IDF:
 # dtm4_idf <- DocumentTermMatrix(tradocs, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), tokenize = FourgramTokenizer))
 dtm4_idf <- weightTfIdf(dtm4_raw, normalize = TRUE)
 rm(dtm4_raw)
 
 dtm4_idf <-removeSparseTerms(dtm4_idf,.9)
 inspect(dtm4_idf)
 frecuencias4_idf <- sort(colSums(as.matrix(dtm4_idf)), decreasing = T)
 write.table(frecuencias4_idf, "./tetragramas_idf.csv", sep = "\t")

##Plot####
x<-1:length(frecuencias)
qplot(x,frecuencias, log="xy", main = "Logarithmic term frequency",xlab = "Word's Statistical Rank", ylab = "Frequency")

x2<-1:length(frecuencias2)
qplot(x2,frecuencias2, log="xy", main = "Logarithmic bigram frequency",xlab = "Bigram's Statistical Rank", ylab = "Frequency")

x4<-1:length(frecuencias4)
qplot(x4,frecuencias4, log="xy", main = "Logarithmic fourgram frequency",xlab = "Fourgram's Statistical Rank", ylab = "Frequency")

## Plot TF-IDF ####
x_idf<-1:length(frecuencias_idf)
qplot(x_idf,frecuencias_idf, log="xy", main = "Logarithmic term frequency",xlab = "Word's Statistical Rank", ylab = "Normalized Frequency")
qplot(x_idf,frecuencias_idf, main = "Term frequency",xlab = "Word's Statistical Rank", ylab = "Frequency")

# WordCloud ####
require(wordcloud)
wordcloud(names(frecuencias_idf),frecuencias_idf, max.words = 100, ordered.colors = TRUE)
wordcloud(names(frecuencias_idf),frecuencias_idf, max.words = 100, colors = brewer.pal(8, "Dark2"), random.order = F)
# wordcloud(names(frecuencias_idf),frecuencias_idf, max.words = 100, colors = brewer.pal(8, "Set2"), random.order = F)


## Resultados Asociación ####

game <- findAssocs(dtm_idf,"gam",.4)
game
write.table(game, "./gameassocs.csv", sep = "\t")

librari <- findAssocs(dtm_idf,"librari",.4)
librari
write.table(librari, "./librariassocs.csv", sep = "\t")

# Los resultados de 'gam' incluyen 'squ' que puede ser 'squander'. Se crea una dtm temporal a partir de 'articulos' para verificar si aparece:
require(slam)
articulos <- tm_map(articulos, removeWords, stopwords("english"))
dtm_articulos <- DocumentTermMatrix(articulos, control = list(removePunctuation = TRUE, removeNumbers = TRUE, tolower = TRUE, stripWhitespace = TRUE))
squ_score <- tm_term_score(dtm_articulos, "squander", col_sums)
squ_score
# Corriendo las dos líneas de código anteriores con los diferentes términos
# derivados de 'squander' se hace evidente que ninguna de estas palabras está
# presente en el corpus. Se procede a eliminar el Corpus:
rm(articulos)
# buscamos las asociaciones de 'squ':
squ <- findAssocs(dtm_idf,"squ",.4)
squ


# Se revisa la asociación de los bigramas más frecuentes con otros bigramas
# Primer bigrama:
fist_bigram_assoc <- findAssocs(dtm2_idf, names(frecuencias2_idf[1]), .4)
fist_bigram_assoc
write.table(fist_bigram_assoc, "./fist_bigram_assoc.csv", sep = "\t")
# Segundo bigrama:
second_bigram_assoc <- findAssocs(dtm2_idf, names(frecuencias2_idf[2]), .4)
second_bigram_assoc
write.table(second_bigram_assoc, "./second_bigram_assoc.csv", sep = "\t")

findAssocs(dtm2_idf, names(frecuencias2_idf[3]), .7)


## Pierson's Correlations ####

cor_2 <- cor(as.matrix(dtm_idf))
cor_2[names(head(frecuencias_idf, 20)), c("skill", "technolog", "stem", "pedagog", "manag")]
cor_2[names(head(frecuencias_idf, 20)), names(head(frecuencias_idf, 20))]

##Topical Analysis#####
## Antes de instalar el paquete {topicmodels} es importante
## instalar en el sistema operativo la librería GNU Scientific Library (GSL) --
## development package. Sin eso, no corre la instalación.
library(topicmodels)

## LDA parameters ###########################################
#


