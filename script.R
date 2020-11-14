# Librerias requeridas ----------------------------------------------------

if(!require(epubr)) install.packages("epubr")
if(!require(epubr)) install.packages("dplyr")
if(!require(epubr)) install.packages("stringr")
if(!require(epubr)) install.packages("tidyverse")
if(!require(epubr)) install.packages("tidytext")
if(!require(epubr)) install.packages("readr")
if(!require(epubr)) install.packages("ggplot2")
if(!require(epubr)) install.packages("patchwork")


library(epubr)
library(stringr)
library(tidytext)
library(tidyverse)
library(dplyr)
library(readr)
library(tidytext)
library(pdftools)
library(ggplot2)
library(patchwork)
library(topicmodels)



# Funciones personalizadas para trabajo con texto --------------------------

#' limpiatexto: limpa el texto recibido de caracteres extraños.
#' 
#' @param texto el texto a limpiar.
#' @return el texto limpio.
#' limpiatexto(el_hombre_libro)
limpiatexto <- function(texto) {
  resultadotxt <-  texto %>% 
    str_remove_all("\\\\n{1,}") %>% 
    str_remove_all("`C.+=") %>% 
    str_remove_all("c\\(") %>% 
    str_remove_all("\\)") %>% 
    str_replace_all( "\\n", " ") %>% 
    str_replace_all( "«", " ") %>% 
    str_replace_all( "»", " ") %>% 
    str_remove_all("'\'") %>% 
    str_remove_all( "[:punct:]+[:digit:]+[:punct:]") %>%
    str_remove_all( "[:digit:]+[:punct:]") %>% 
    str_remove_all( "[:digit:][:space:]{0,}") %>% 
    str_remove_all( "vol  pp <<")   %>% 
    str_remove_all( "<<") %>% 
    str_remove_all( "<<\n") %>% 
    str_replace_all("[:punct:]", " ") %>% 
    str_replace_all( "[:space:]{2,}", " ")
 
  resultadotxt
}


#' freq_palabras_libros: saca frecuencias de palabras y elimina las stopwords.
#' 
#' @param libro el texto a trabajar.
#' @param stopwords las palabras no deseadas a limpiar.
#' @return tibble con frecuencias.
#' freq_palabras_libros(el_hombre_libro,all_stopwords)
freq_palabras_libros <- function(libro, stopwords) {
  # Se pasa el texto a una tibble con palabras y se cuenta la freq  --------
  libros_por_freq <- tibble(texto = libro) %>% 
    unnest_tokens(input = texto, output = palabra, strip_numeric = TRUE) %>% 
    count(palabra, sort = TRUE)
  
  # Se quitan los stopwords.
  libros_quitando_palabras <- libros_por_freq %>% 
    anti_join(stopwords)
  
  libros_quitando_palabras
}


#' bigramas_libros: Obtiene bigramas del texto entregado.
#' 
#' @param libro_texto el texto a trabajar.
#' @return bigrama de palabras en tibble.
#' bigramas_libros(el_hombre_tibble)
bigramas_libros <- function(libro){
  
  libro_tibble <-  tibble(entrada = libro)
  bigrama_salida <-  libro_tibble %>% 
    unnest_tokens(input = entrada,
                  output = palabra,
                  token = "ngrams",
                  n = 2) %>% 
    filter(!is.na(palabra)) %>% 
    count(palabra, sort = TRUE) %>% 
    separate(col= palabra, 
             into = c("palabra_1","palabra_2"),
             sep = " ") %>% 
    filter(!palabra_1 %in% all_stopwords$palabra) %>% 
    filter(!palabra_2 %in% all_stopwords$palabra)
  
  bigrama_salida
}


# Lectura de los textos formato EPUB --------------------------------------


# LEEMOS el EPUB El Hombre En Busca De Sentido
el_hombre <-  epub("ElHombreEnBuscaDeSentido.epub")

# Verificamos que sea los capitulos correctos del epub los que leeremos
#View(el_hombre$data[[1]])

# tomaremos de la sección 1 a la 5, esto es las filas 2 a la 6
el_hombre_texto <- paste(el_hombre$data[[1]][2:6,2], collapse = " ")



# LEEMOS el EPUB El Elemento
el_elemento <- epub("ElElemento.epub")

# Verificamos que es la info del libro sin agregar cosas demás
#View(el_elemento$data[[1]])

# tomaremos del capitulo 1 al 11, esto es filas 10 a 22
el_elemento_texto <- paste(el_elemento$data[[1]][10:22,2],collapse = " ")



# LEEMOS el EPUB Padre rico, padre pobre
padre_rico <-  epub("PadreRicoPadrePobre.epub")

# Verificamos que sea los capitulos correctos del epub los que leeremos
#View(padre_rico$data[[1]])

# tomaremos del capitulo 1 al 9 (filas 3 a la 11)
padre_rico_texto <- paste(padre_rico$data[[1]][3:11,2], collapse = " ")



# Limpiando los textos --------------------------------------


el_hombre_texto <- limpiatexto(el_hombre_texto)
el_hombre_texto

el_elemento_texto <- limpiatexto(el_elemento_texto)
el_elemento_texto

padre_rico_texto <- limpiatexto(padre_rico_texto)
padre_rico_texto


# Funcion para tomar la frecuencia de palabras y realizar antijoin --------

# 1:
# Se descargan algunas stopwords desde la web.
stopwords_web <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")

# stopword personalizado 
mis_stopwords <- tibble(palabra = c("va", "tenemos","value","hace", "hacer", "ser", "puede", "tiene", "dice")) # colocar mas en caso de...

# unimos
all_stopwords <- bind_rows(stopwords_web, mis_stopwords)

# 2:
# llamamos al método que saca stopwords y les da frecuencia a las palabras del texto.


freq_el_hombre_texto <- freq_palabras_libros(el_hombre_texto,all_stopwords)
freq_el_hombre_texto

freq_el_elemento_texto <- freq_palabras_libros(el_elemento_texto,all_stopwords)
freq_el_elemento_texto

freq_padre_rico_texto <- freq_palabras_libros(padre_rico_texto,all_stopwords)
freq_padre_rico_texto



################ BIGRAMAS ################

el_hombre_bigrama <-  bigramas_libros(el_hombre_texto)

el_elemento_bigrama <-  bigramas_libros(el_elemento_texto)

padre_rico_bigrama <-  bigramas_libros(padre_rico_texto)




## ¿Los resultados son los que esperaban? 
# Si. AL mirar los bigramas y relacionarlos con el autor de cada libro, corresponden con las vivencias y entorno de cada autor.


## ¿Hay algo que les haya llamado la atención?
# Los libros Padre rico y el elemento son parecidos entre si, en la perspectiva de crecimiento personal y económica,
# mientras que "el hombre en busca de sentido" toca temas vividos en un campo de concentración y el sentido a la vida que encontró en ello.




